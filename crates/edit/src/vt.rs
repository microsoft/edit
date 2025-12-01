// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Our VT parser.

use std::time;

use crate::simd::memchr2;
use crate::unicode::Utf8Chars;

/// The parser produces these tokens.
pub enum Token<'parser, 'input> {
    /// A bunch of text. Doesn't contain any control characters.
    Text(&'input str),
    /// A single control character, like backspace or return.
    Ctrl(char),
    /// We encountered `ESC x` and this contains `x`.
    Esc(char),
    /// We encountered `ESC O x` and this contains `x`.
    SS3(char),
    /// A CSI sequence started with `ESC [`.
    ///
    /// They are the most common escape sequences. See [`Csi`].
    Csi(&'parser Csi),
    /// An OSC sequence started with `ESC ]`.
    ///
    /// The sequence may be split up into multiple tokens if the input
    /// is given in chunks. This is indicated by the `partial` field.
    Osc { data: &'input str, partial: bool },
    /// An DCS sequence started with `ESC P`.
    ///
    /// The sequence may be split up into multiple tokens if the input
    /// is given in chunks. This is indicated by the `partial` field.
    Dcs { data: &'input str, partial: bool },
}

/// Stores the state of the parser.
#[derive(Clone, Copy)]
enum State {
    Ground,
    Esc,
    Ss3,
    Csi,
    Osc,
    Dcs,
    OscEsc,
    DcsEsc,
}

/// A single CSI sequence, parsed for your convenience.
pub struct Csi {
    /// The parameters of the CSI sequence.
    pub params: [u16; 32],
    /// The number of parameters stored in [`Csi::params`].
    pub param_count: usize,
    /// The private byte, if any. `0` if none.
    ///
    /// The private byte is the first character right after the
    /// `ESC [` sequence. It is usually a `?` or `<`.
    pub private_byte: char,
    /// The final byte of the CSI sequence.
    ///
    /// This is the last character of the sequence, e.g. `m` or `H`.
    pub final_byte: char,
}

pub struct Parser {
    state: State,
    // Csi is not part of State, because it allows us
    // to more quickly erase and reuse the struct.
    csi: Csi,
}

impl Parser {
    pub fn new() -> Self {
        Self {
            state: State::Ground,
            csi: Csi { params: [0; 32], param_count: 0, private_byte: '\0', final_byte: '\0' },
        }
    }

    /// Suggests a timeout for the next call to `read()`.
    ///
    /// We need this because of the ambiguity of whether a trailing
    /// escape character in an input is starting another escape sequence or
    /// is just the result of the user literally pressing the Escape key.
    pub fn read_timeout(&mut self) -> std::time::Duration {
        match self.state {
            // 100ms is a upper ceiling for a responsive feel.
            // Realistically though, this could be much lower.
            //
            // However, there seems to be issues with OpenSSH on Windows.
            // See: https://github.com/PowerShell/Win32-OpenSSH/issues/2275
            State::Esc => time::Duration::from_millis(100),
            _ => time::Duration::MAX,
        }
    }

    /// Parses the given input into VT sequences.
    ///
    /// You should call this function even if your `read()`
    /// had a timeout (pass an empty string in that case).
    pub fn parse<'parser, 'input>(
        &'parser mut self,
        input: &'input str,
    ) -> Stream<'parser, 'input> {
        Stream { parser: self, input, off: 0 }
    }
}

/// An iterator that parses VT sequences into [`Token`]s.
///
/// Can't implement [`Iterator`], because this is a "lending iterator".
pub struct Stream<'parser, 'input> {
    parser: &'parser mut Parser,
    input: &'input str,
    off: usize,
}

impl<'input> Stream<'_, 'input> {
    /// Returns the input that is being parsed.
    pub fn input(&self) -> &'input str {
        self.input
    }

    /// Returns the current parser offset.
    pub fn offset(&self) -> usize {
        self.off
    }

    /// Reads and consumes raw bytes from the input.
    pub fn read(&mut self, dst: &mut [u8]) -> usize {
        let bytes = self.input.as_bytes();
        let off = self.off.min(bytes.len());
        let len = dst.len().min(bytes.len() - off);
        dst[..len].copy_from_slice(&bytes[off..off + len]);
        self.off += len;
        len
    }

    fn decode_next(&mut self) -> char {
        let mut iter = Utf8Chars::new(self.input.as_bytes(), self.off);
        let c = iter.next().unwrap_or('\0');
        self.off = iter.offset();
        c
    }

    /// Parses the next VT sequence from the previously given input.
    #[allow(
        clippy::should_implement_trait,
        reason = "can't implement Iterator because this is a lending iterator"
    )]
    pub fn next(&mut self) -> Option<Token<'_, 'input>> {
        let input = self.input;
        let bytes = input.as_bytes();

        // If the previous input ended with an escape character, `read_timeout()`
        // returned `Some(..)` timeout, and if the caller did everything correctly
        // and there was indeed a timeout, we should be called with an empty
        // input. In that case we'll return the escape as its own token.
        if input.is_empty() && matches!(self.parser.state, State::Esc) {
            self.parser.state = State::Ground;
            return Some(Token::Esc('\0'));
        }

        while self.off < bytes.len() {
            // TODO: The state machine can be roughly broken up into two parts:
            // * Wants to parse 1 `char` at a time: Ground, Esc, Ss3
            //   These could all be unified to a single call to `decode_next()`.
            // * Wants to bulk-process bytes: Csi, Osc, Dcs
            // We should do that so the UTF8 handling is a bit more "unified".
            match self.parser.state {
                State::Ground => match bytes[self.off] {
                    0x1b => {
                        self.parser.state = State::Esc;
                        self.off += 1;
                    }
                    c @ (0x00..0x20 | 0x7f) => {
                        self.off += 1;
                        return Some(Token::Ctrl(c as char));
                    }
                    _ => {
                        let beg = self.off;
                        while {
                            self.off += 1;
                            self.off < bytes.len()
                                && bytes[self.off] >= 0x20
                                && bytes[self.off] != 0x7f
                        } {}
                        return Some(Token::Text(&input[beg..self.off]));
                    }
                },
                State::Esc => match self.decode_next() {
                    '[' => {
                        self.parser.state = State::Csi;
                        self.parser.csi.private_byte = '\0';
                        self.parser.csi.final_byte = '\0';
                        while self.parser.csi.param_count > 0 {
                            self.parser.csi.param_count -= 1;
                            self.parser.csi.params[self.parser.csi.param_count] = 0;
                        }
                    }
                    ']' => {
                        self.parser.state = State::Osc;
                    }
                    'O' => {
                        self.parser.state = State::Ss3;
                    }
                    'P' => {
                        self.parser.state = State::Dcs;
                    }
                    c => {
                        self.parser.state = State::Ground;
                        return Some(Token::Esc(c));
                    }
                },
                State::Ss3 => {
                    self.parser.state = State::Ground;
                    return Some(Token::SS3(self.decode_next()));
                }
                State::Csi => {
                    loop {
                        // If we still have slots left, parse the parameter.
                        if self.parser.csi.param_count < self.parser.csi.params.len() {
                            let dst = &mut self.parser.csi.params[self.parser.csi.param_count];
                            while self.off < bytes.len() && bytes[self.off].is_ascii_digit() {
                                let add = bytes[self.off] as u32 - b'0' as u32;
                                let value = *dst as u32 * 10 + add;
                                *dst = value.min(u16::MAX as u32) as u16;
                                self.off += 1;
                            }
                        } else {
                            // ...otherwise, skip the parameters until we find the final byte.
                            while self.off < bytes.len() && bytes[self.off].is_ascii_digit() {
                                self.off += 1;
                            }
                        }

                        // Encountered the end of the input before finding the final byte.
                        if self.off >= bytes.len() {
                            return None;
                        }

                        let c = bytes[self.off];
                        self.off += 1;

                        match c {
                            0x40..=0x7e => {
                                self.parser.state = State::Ground;
                                self.parser.csi.final_byte = c as char;
                                if self.parser.csi.param_count != 0
                                    || self.parser.csi.params[0] != 0
                                {
                                    self.parser.csi.param_count += 1;
                                }
                                return Some(Token::Csi(&self.parser.csi));
                            }
                            b';' => self.parser.csi.param_count += 1,
                            b'<'..=b'?' => self.parser.csi.private_byte = c as char,
                            _ => {}
                        }
                    }
                }
                State::Osc | State::Dcs => {
                    let beg = self.off;
                    let mut data;
                    let mut partial;

                    loop {
                        // Find any indication for the end of the OSC/DCS sequence.
                        self.off = memchr2(b'\x07', b'\x1b', bytes, self.off);

                        data = &input[beg..self.off];
                        partial = self.off >= bytes.len();

                        // Encountered the end of the input before finding the terminator.
                        if partial {
                            break;
                        }

                        let c = bytes[self.off];
                        self.off += 1;

                        if c == 0x1b {
                            // It's only a string terminator if it's followed by \.
                            // We're at the end so we're saving the state and will continue next time.
                            if self.off >= bytes.len() {
                                self.parser.state = match self.parser.state {
                                    State::Osc => State::OscEsc,
                                    _ => State::DcsEsc,
                                };
                                partial = true;
                                break;
                            }

                            // False alarm: Not a string terminator.
                            if bytes[self.off] != b'\\' {
                                continue;
                            }

                            self.off += 1;
                        }

                        break;
                    }

                    let state = self.parser.state;
                    if !partial {
                        self.parser.state = State::Ground;
                    }
                    return match state {
                        State::Osc => Some(Token::Osc { data, partial }),
                        _ => Some(Token::Dcs { data, partial }),
                    };
                }
                State::OscEsc | State::DcsEsc => {
                    // We were processing an OSC/DCS sequence and the last byte was an escape character.
                    // It's only a string terminator if it's followed by \ (= "\x1b\\").
                    if bytes[self.off] == b'\\' {
                        // It was indeed a string terminator and we can now tell the caller about it.
                        let state = self.parser.state;

                        // Consume the terminator (one byte in the previous input and this byte).
                        self.parser.state = State::Ground;
                        self.off += 1;

                        return match state {
                            State::OscEsc => Some(Token::Osc { data: "", partial: false }),
                            _ => Some(Token::Dcs { data: "", partial: false }),
                        };
                    } else {
                        // False alarm: Not a string terminator.
                        // We'll return the escape character as a separate token.
                        // Processing will continue from the current state (`bytes[self.off]`).
                        self.parser.state = match self.parser.state {
                            State::OscEsc => State::Osc,
                            _ => State::Dcs,
                        };
                        return match self.parser.state {
                            State::Osc => Some(Token::Osc { data: "\x1b", partial: true }),
                            _ => Some(Token::Dcs { data: "\x1b", partial: true }),
                        };
                    }
                }
            }
        }

        None
    }
}
