// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Parses VT sequences into input events.
//!
//! In the future this allows us to take apart the application and
//! support input schemes that aren't VT, such as UEFI, or GUI.

use std::ops::{BitOr, BitOrAssign};
use std::{fmt, mem};

use crate::helpers::{CoordType, Point, Size};
use crate::kbd::*;
use crate::vt;

/// Represents a key/modifier combination.
///
/// TODO: Is this a good idea? I did it to allow typing `MOD_CTRL | VK_A`.
/// The reason it's an awkward u32 and not a struct is to hopefully make ABIs easier later.
/// Of course you could just translate on the ABI boundary, but my hope is that this
/// design lets me realize some restrictions early on that I can't foresee yet.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct InputKey(u32);

impl InputKey {
    pub(crate) const fn new(v: u32) -> Self {
        Self(v)
    }

    pub(crate) const fn from_ascii(ch: char) -> Option<Self> {
        if ch == ' ' || (ch >= '0' && ch <= '9') {
            Some(Self(ch as u32))
        } else if ch >= 'a' && ch <= 'z' {
            Some(Self(ch as u32 & !0x20)) // Shift a-z to A-Z
        } else if ch >= 'A' && ch <= 'Z' {
            Some(Self(MOD_SHIFT.0 | ch as u32))
        } else {
            None
        }
    }

    pub(crate) const fn value(&self) -> u32 {
        self.0
    }

    pub(crate) const fn key(&self) -> Self {
        Self(self.0 & 0x00FFFFFF)
    }

    pub(crate) const fn modifiers(&self) -> InputKeyMod {
        InputKeyMod(self.0 & 0xFF000000)
    }

    pub(crate) const fn modifiers_contains(&self, modifier: InputKeyMod) -> bool {
        (self.0 & modifier.0) != 0
    }

    pub(crate) const fn with_modifiers(&self, modifiers: InputKeyMod) -> Self {
        Self(self.0 | modifiers.0)
    }
}

impl fmt::Debug for InputKey {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self.0 {
            0x00 => "NULL",
            0x08 => "BACK",
            0x09 => "TAB",
            0x0D => "RETURN",
            0x1B => "ESCAPE",
            0x20 => "SPACE",
            0x21 => "PRIOR",
            0x22 => "NEXT",

            0x23 => "END",
            0x24 => "HOME",

            0x25 => "LEFT",
            0x26 => "UP",
            0x27 => "RIGHT",
            0x28 => "DOWN",

            0x2D => "INSERT",
            0x2E => "DELETE",

            0x6A => "MULTIPLY",
            0x6B => "ADD",
            0x6C => "SEPARATOR",
            0x6D => "SUBTRACT",
            0x6E => "DECIMAL",
            0x6F => "DIVIDE",
            _ => {
                return {
                    if matches!(self.0, 0x30..=0x39 | 0x41..=0x5A) {
                        // 0-9, A-Z
                        write!(f, "{}", char::from_u32(self.0).unwrap_or('\0'))
                    } else if matches!(self.0, 0x70..=0x87) {
                        // F1-F24
                        write!(f, "F{}", self.0 - 0x70 + 1)
                    } else if matches!(self.0, 0x60..=0x69) {
                        // NUMPAD0-NUMPAD9
                        write!(f, "NUMPAD{}", self.0 - 0x60)
                    } else {
                        write!(f, "VK_{:02X}", self.0)
                    }
                };
            }
        })
    }
}

/// A keyboard modifier. Ctrl/Alt/Shift.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct InputKeyMod(u32);

impl InputKeyMod {
    pub(crate) const fn new(v: u32) -> Self {
        Self(v)
    }

    pub(crate) const fn contains(&self, modifier: Self) -> bool {
        (self.0 & modifier.0) != 0
    }
}

impl BitOr<InputKeyMod> for InputKeyMod {
    type Output = Self;

    fn bitor(self, rhs: InputKeyMod) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl BitOr<InputKeyMod> for InputKey {
    type Output = Self;

    fn bitor(self, rhs: InputKeyMod) -> Self {
        Self(self.0 | rhs.0)
    }
}

impl BitOr<InputKey> for InputKeyMod {
    type Output = InputKey;

    fn bitor(self, rhs: InputKey) -> InputKey {
        InputKey(self.0 | rhs.0)
    }
}

impl BitOrAssign for InputKeyMod {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl fmt::Debug for InputKeyMod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut sep = "";
        for (modifier, name) in [(MOD_CTRL, "CTRL"), (MOD_ALT, "ALT"), (MOD_SHIFT, "SHIFT")] {
            if self.contains(modifier) {
                write!(f, "{}{}", sep, name)?;
                sep = " | ";
            }
        }
        if sep.is_empty() {
            f.write_str("NONE")?;
        }
        Ok(())
    }
}

/// Mouse input state. Up/Down, Left/Right, etc.
#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum InputMouseState {
    #[default]
    None,

    // These 3 carry their state between frames.
    Left,
    Middle,
    Right,

    // These 2 get reset to None on the next frame.
    Release,
    Scroll,
}

/// Mouse input.
#[derive(Debug, Clone, Copy)]
pub struct InputMouse {
    /// The state of the mouse.Up/Down, Left/Right, etc.
    pub state: InputMouseState,
    /// Any keyboard modifiers that are held down.
    pub modifiers: InputKeyMod,
    /// Position of the mouse in the viewport.
    pub position: Point,
    /// Scroll delta.
    pub scroll: Point,
}

/// Primary result type of the parser.
pub enum Input<'input> {
    /// Window resize event.
    Resize(Size),
    /// Text input.
    /// Note that [`Input::Keyboard`] events can also be text.
    Text(&'input str),
    /// A clipboard paste.
    Paste(Vec<u8>),
    /// Keyboard input.
    Keyboard(InputKey),
    /// Mouse input.
    Mouse(InputMouse),
}

/// Parses VT sequences into input events.
pub struct Parser {
    bracketed_paste: bool,
    bracketed_paste_buf: Vec<u8>,
    x10_mouse_want: bool,
    x10_mouse_buf: [u8; 3],
    x10_mouse_len: usize,
}

impl Parser {
    /// Creates a new parser that turns VT sequences into input events.
    ///
    /// Keep the instance alive for the lifetime of the input stream.
    pub fn new() -> Self {
        Self {
            bracketed_paste: false,
            bracketed_paste_buf: Vec::new(),
            x10_mouse_want: false,
            x10_mouse_buf: [0; 3],
            x10_mouse_len: 0,
        }
    }

    /// Takes an [`vt::Stream`] and returns a [`Stream`]
    /// that turns VT sequences into input events.
    pub fn parse<'parser, 'vt, 'input>(
        &'parser mut self,
        stream: vt::Stream<'vt, 'input>,
    ) -> Stream<'parser, 'vt, 'input> {
        Stream { parser: self, stream }
    }
}

/// An iterator that parses VT sequences into input events.
pub struct Stream<'parser, 'vt, 'input> {
    parser: &'parser mut Parser,
    stream: vt::Stream<'vt, 'input>,
}

impl<'input> Iterator for Stream<'_, '_, 'input> {
    type Item = Input<'input>;

    fn next(&mut self) -> Option<Input<'input>> {
        loop {
            if self.parser.bracketed_paste {
                return self.handle_bracketed_paste();
            }

            if self.parser.x10_mouse_want {
                return self.parse_x10_mouse_coordinates();
            }

            const KEYPAD_LUT: [u8; 8] = [
                VK_UP.value() as u8,    // A
                VK_DOWN.value() as u8,  // B
                VK_RIGHT.value() as u8, // C
                VK_LEFT.value() as u8,  // D
                0,                      // E
                VK_END.value() as u8,   // F
                0,                      // G
                VK_HOME.value() as u8,  // H
            ];

            match self.stream.next()? {
                vt::Token::Text(text) => {
                    return Some(Input::Text(text));
                }
                vt::Token::Ctrl(ch) => match ch {
                    '\0' | '\t' | '\r' => return Some(Input::Keyboard(InputKey::new(ch as u32))),
                    '\n' => return Some(Input::Keyboard(MOD_CTRL | VK_RETURN)),
                    ..='\x1a' => {
                        // Shift control code to A-Z
                        let key = ch as u32 | 0x40;
                        return Some(Input::Keyboard(MOD_CTRL | InputKey::new(key)));
                    }
                    '\x7f' => return Some(Input::Keyboard(VK_BACK)),
                    _ => {}
                },
                vt::Token::Esc(ch) => {
                    match ch {
                        '\0' => return Some(Input::Keyboard(VK_ESCAPE)),
                        '\n' => return Some(Input::Keyboard(MOD_CTRL | MOD_ALT | VK_RETURN)),
                        ' '..='~' => {
                            let ch = ch as u32;
                            let key = ch & !0x20; // Shift a-z to A-Z
                            let modifiers =
                                if (ch & 0x20) != 0 { MOD_ALT } else { MOD_ALT | MOD_SHIFT };
                            return Some(Input::Keyboard(modifiers | InputKey::new(key)));
                        }
                        _ => {}
                    }
                }
                vt::Token::SS3(ch) => match ch {
                    'A'..='H' => {
                        let vk = KEYPAD_LUT[ch as usize - 'A' as usize];
                        if vk != 0 {
                            return Some(Input::Keyboard(InputKey::new(vk as u32)));
                        }
                    }
                    'P'..='S' => {
                        let key = VK_F1.value() + ch as u32 - 'P' as u32;
                        return Some(Input::Keyboard(InputKey::new(key)));
                    }
                    _ => {}
                },
                vt::Token::Csi(csi) => {
                    match csi.final_byte {
                        'A'..='H' => {
                            let vk = KEYPAD_LUT[csi.final_byte as usize - 'A' as usize];
                            if vk != 0 {
                                return Some(Input::Keyboard(
                                    InputKey::new(vk as u32) | Self::parse_modifiers(csi),
                                ));
                            }
                        }
                        'Z' => return Some(Input::Keyboard(MOD_SHIFT | VK_TAB)),
                        '~' => {
                            const LUT: [u8; 35] = [
                                0,
                                VK_HOME.value() as u8,   // 1
                                VK_INSERT.value() as u8, // 2
                                VK_DELETE.value() as u8, // 3
                                VK_END.value() as u8,    // 4
                                VK_PRIOR.value() as u8,  // 5
                                VK_NEXT.value() as u8,   // 6
                                0,
                                0,
                                0,
                                0,
                                0,
                                0,
                                0,
                                0,
                                VK_F5.value() as u8, // 15
                                0,
                                VK_F6.value() as u8,  // 17
                                VK_F7.value() as u8,  // 18
                                VK_F8.value() as u8,  // 19
                                VK_F9.value() as u8,  // 20
                                VK_F10.value() as u8, // 21
                                0,
                                VK_F11.value() as u8, // 23
                                VK_F12.value() as u8, // 24
                                VK_F13.value() as u8, // 25
                                VK_F14.value() as u8, // 26
                                0,
                                VK_F15.value() as u8, // 28
                                VK_F16.value() as u8, // 29
                                0,
                                VK_F17.value() as u8, // 31
                                VK_F18.value() as u8, // 32
                                VK_F19.value() as u8, // 33
                                VK_F20.value() as u8, // 34
                            ];
                            const LUT_LEN: u16 = LUT.len() as u16;

                            match csi.params[0] {
                                0..LUT_LEN => {
                                    let vk = LUT[csi.params[0] as usize];
                                    if vk != 0 {
                                        return Some(Input::Keyboard(
                                            InputKey::new(vk as u32) | Self::parse_modifiers(csi),
                                        ));
                                    }
                                }
                                200 => self.parser.bracketed_paste = true,
                                _ => {}
                            }
                        }
                        'm' | 'M' if csi.private_byte == '<' => {
                            let btn = csi.params[0];
                            let mut mouse = InputMouse {
                                state: InputMouseState::None,
                                modifiers: MOD_NONE,
                                position: Default::default(),
                                scroll: Default::default(),
                            };

                            mouse.state = InputMouseState::None;
                            if (btn & 0x40) != 0 {
                                mouse.state = InputMouseState::Scroll;
                                mouse.scroll.y += if (btn & 0x01) != 0 { 3 } else { -3 };
                            } else if csi.final_byte == 'M' {
                                const STATES: [InputMouseState; 4] = [
                                    InputMouseState::Left,
                                    InputMouseState::Middle,
                                    InputMouseState::Right,
                                    InputMouseState::None,
                                ];
                                mouse.state = STATES[(btn as usize) & 0x03];
                            }

                            mouse.modifiers = MOD_NONE;
                            mouse.modifiers |= if (btn & 0x04) != 0 { MOD_SHIFT } else { MOD_NONE };
                            mouse.modifiers |= if (btn & 0x08) != 0 { MOD_ALT } else { MOD_NONE };
                            mouse.modifiers |= if (btn & 0x10) != 0 { MOD_CTRL } else { MOD_NONE };

                            mouse.position.x = csi.params[1] as CoordType - 1;
                            mouse.position.y = csi.params[2] as CoordType - 1;
                            return Some(Input::Mouse(mouse));
                        }
                        'M' if csi.param_count == 0 => {
                            self.parser.x10_mouse_want = true;
                        }
                        't' if csi.params[0] == 8 => {
                            // Window Size
                            let width = (csi.params[2] as CoordType).clamp(1, 32767);
                            let height = (csi.params[1] as CoordType).clamp(1, 32767);
                            return Some(Input::Resize(Size { width, height }));
                        }
                        _ => {}
                    }
                }
                _ => {}
            }
        }
    }
}

impl<'input> Stream<'_, '_, 'input> {
    /// Once we encounter the start of a bracketed paste
    /// we seek to the end of the paste in this function.
    ///
    /// A bracketed paste is basically:
    /// ```text
    /// <ESC>[201~    lots of text    <ESC>[201~
    /// ```
    ///
    /// That in between text is then expected to be taken literally.
    /// It can be in between anything though, including other escape sequences.
    /// This is the reason why this is a separate method.
    #[cold]
    fn handle_bracketed_paste(&mut self) -> Option<Input<'input>> {
        let beg = self.stream.offset();
        let mut end = beg;

        while let Some(token) = self.stream.next() {
            if let vt::Token::Csi(csi) = token
                && csi.final_byte == '~'
                && csi.params[0] == 201
            {
                self.parser.bracketed_paste = false;
                break;
            }
            end = self.stream.offset();
        }

        if end != beg {
            self.parser
                .bracketed_paste_buf
                .extend_from_slice(&self.stream.input().as_bytes()[beg..end]);
        }

        if !self.parser.bracketed_paste {
            Some(Input::Paste(mem::take(&mut self.parser.bracketed_paste_buf)))
        } else {
            None
        }
    }

    /// Implements the X10 mouse protocol via `CSI M CbCxCy`.
    ///
    /// You want to send numeric mouse coordinates.
    /// You have CSI sequences with numeric parameters.
    /// So, of course you put the coordinates as shifted ASCII characters after
    /// the end of the sequence. Limited coordinate range and complicated parsing!
    /// This is so puzzling to me. The existence of this function makes me unhappy.
    #[cold]
    fn parse_x10_mouse_coordinates(&mut self) -> Option<Input<'input>> {
        self.parser.x10_mouse_len +=
            self.stream.read(&mut self.parser.x10_mouse_buf[self.parser.x10_mouse_len..]);
        if self.parser.x10_mouse_len < 3 {
            return None;
        }

        let button = self.parser.x10_mouse_buf[0] & 0b11;
        let modifier = self.parser.x10_mouse_buf[0] & 0b11100;
        let x = self.parser.x10_mouse_buf[1] as CoordType - 0x21;
        let y = self.parser.x10_mouse_buf[2] as CoordType - 0x21;
        let action = match button {
            0 => InputMouseState::Left,
            1 => InputMouseState::Middle,
            2 => InputMouseState::Right,
            _ => InputMouseState::None,
        };
        let modifiers = match modifier {
            4 => MOD_SHIFT,
            8 => MOD_ALT,
            16 => MOD_CTRL,
            _ => MOD_NONE,
        };

        self.parser.x10_mouse_want = false;
        self.parser.x10_mouse_len = 0;

        Some(Input::Mouse(InputMouse {
            state: action,
            modifiers,
            position: Point { x, y },
            scroll: Default::default(),
        }))
    }

    fn parse_modifiers(csi: &vt::Csi) -> InputKeyMod {
        let mut modifiers = MOD_NONE;
        let p1 = csi.params[1].saturating_sub(1);
        if (p1 & 0x01) != 0 {
            modifiers |= MOD_SHIFT;
        }
        if (p1 & 0x02) != 0 {
            modifiers |= MOD_ALT;
        }
        if (p1 & 0x04) != 0 {
            modifiers |= MOD_CTRL;
        }
        modifiers
    }
}
