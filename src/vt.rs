use std::{mem, time};

use crate::simd::memchr2;

pub enum Token<'parser, 'input> {
    Text(&'input str),
    Ctrl(char),
    Esc(char),
    SS3(char),
    Csi(&'parser Csi),
    Osc { data: &'input str, partial: bool },
    Dcs { data: &'input str, partial: bool },
}

#[derive(Clone, Copy)]
pub enum State {
    Ground,
    Esc,
    Ss3,
    Csi,
    Osc,
    Dcs,
    OscEsc,
    DcsEsc,
}

pub struct Csi {
    pub params: [i32; 32],
    pub param_count: usize,
    pub private_byte: char,
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
    /// We need this because of the ambiguouity of whether a trailing
    /// escape character in an input is starting another escape sequence or
    /// is just the result of the user literally pressing the Escape key.
    pub fn read_timeout(&mut self) -> std::time::Duration {
        match self.state {
            // 100ms is a upper ceiling for a responsive feel. This uses half that,
            // under the assumption that a really slow terminal needs equal amounts
            // of time for I and O. Realistically though, this could be much lower.
            State::Esc => time::Duration::from_millis(50),
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

pub struct Stream<'parser, 'input> {
    parser: &'parser mut Parser,
    input: &'input str,
    off: usize,
}

impl<'parser, 'input> Stream<'parser, 'input> {
    pub fn input(&self) -> &'input str {
        self.input
    }

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

    /// Parses the next VT sequence from the previously given input.
    ///
    /// Can't implement Iterator, because this is a "lending iterator".
    #[allow(clippy::should_implement_trait)]
    pub fn next(&mut self) -> Option<Token<'parser, 'input>> {
        // I don't know how to tell Rust that `self.parser` and its lifetime
        // `'parser` outlives `self`, and at this point I don't care.
        let parser = unsafe { mem::transmute::<_, &'parser mut Parser>(&mut *self.parser) };
        let input = self.input;
        let bytes = input.as_bytes();

        // If the previous input ended with an escape character, `read_timeout()`
        // returned `Some(..)` timeout, and if the caller did everything correctly
        // and there was indeed a timeout, we should be called with an empty
        // input. In that case we'll return the escape as its own token.
        if input.is_empty() && matches!(parser.state, State::Esc) {
            parser.state = State::Ground;
            return Some(Token::Esc('\0'));
        }

        while self.off < bytes.len() {
            match parser.state {
                State::Ground => match bytes[self.off] {
                    0x1b => {
                        parser.state = State::Esc;
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
                State::Esc => {
                    let c = bytes[self.off];
                    self.off += 1;
                    match c {
                        b'[' => {
                            parser.state = State::Csi;
                            parser.csi.private_byte = '\0';
                            parser.csi.final_byte = '\0';
                            while parser.csi.param_count > 0 {
                                parser.csi.param_count -= 1;
                                parser.csi.params[parser.csi.param_count] = 0;
                            }
                        }
                        b']' => {
                            parser.state = State::Osc;
                        }
                        b'O' => {
                            parser.state = State::Ss3;
                        }
                        b'P' => {
                            parser.state = State::Dcs;
                        }
                        c => {
                            parser.state = State::Ground;
                            return Some(Token::Esc(c as char));
                        }
                    }
                }
                State::Ss3 => {
                    parser.state = State::Ground;
                    let c = bytes[self.off];
                    self.off += 1;
                    return Some(Token::SS3(c as char));
                }
                State::Csi => {
                    loop {
                        // If we still have slots left, parse the parameter.
                        if parser.csi.param_count < parser.csi.params.len() {
                            let dst = &mut parser.csi.params[parser.csi.param_count];
                            while self.off < bytes.len()
                                && bytes[self.off] >= b'0'
                                && bytes[self.off] <= b'9'
                            {
                                let v = *dst * 10 + bytes[self.off] as i32 - b'0' as i32;
                                *dst = v.min(0xffff);
                                self.off += 1;
                            }
                        } else {
                            // ...otherwise, skip the parameters until we find the final byte.
                            while self.off < bytes.len()
                                && bytes[self.off] >= b'0'
                                && bytes[self.off] <= b'9'
                            {
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
                                parser.state = State::Ground;
                                parser.csi.final_byte = c as char;
                                if parser.csi.param_count != 0 || parser.csi.params[0] != 0 {
                                    parser.csi.param_count += 1;
                                }
                                return Some(Token::Csi(&parser.csi as &'parser Csi));
                            }
                            b';' => parser.csi.param_count += 1,
                            b'<'..=b'?' => parser.csi.private_byte = c as char,
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
                                parser.state = match parser.state {
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

                    let state = parser.state;
                    if !partial {
                        parser.state = State::Ground;
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
                        let state = parser.state;

                        // Consume the terminator (one byte in the previous input and this byte).
                        parser.state = State::Ground;
                        self.off += 1;

                        return match state {
                            State::OscEsc => Some(Token::Osc { data: "", partial: false }),
                            _ => Some(Token::Dcs { data: "", partial: false }),
                        };
                    } else {
                        // False alarm: Not a string terminator.
                        // We'll return the escape character as a separate token.
                        // Processing will continue from the current state (`bytes[self.off]`).
                        parser.state = match parser.state {
                            State::OscEsc => State::Osc,
                            _ => State::Dcs,
                        };
                        return match parser.state {
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
