use crate::framebuffer::{Framebuffer, IndexedColor};

pub struct Token {
    pub byte_offset: usize,
    pub length: usize,
    pub foreground: u32,
}

pub struct Highlighter {
    string_color: IndexedColor,
    brace_color: IndexedColor,
}

impl Highlighter {
    pub fn new() -> Self {
        Self {
            string_color: IndexedColor::BrightRed,
            brace_color: IndexedColor::BrightGreen,
        }
    }

    pub fn highlight(&self, src: &str, palette: &Framebuffer) -> Vec<Token> {
        let mut out = Vec::new();
        let mut start = 0;
        let bytes = src.as_bytes();

        let mut in_string = false;
        let mut in_brace = false;

        for (i, &b) in bytes.iter().enumerate() {
            match b {
                b'"' => {
                    if in_string {
                        out.push(Token {
                            byte_offset: start,
                            length: i - start + 1,
                            foreground: palette.indexed(self.string_color),
                        });
                        in_string = false;
                    } else {
                        in_string = true;
                        start = i;
                    }
                }

                b'{' => {
                    if !in_string {
                        in_brace = true;
                        start = i;
                    }
                }

                b'}' => {
                    if in_brace {
                        out.push(Token {
                            byte_offset: start,
                            length: i - start + 1,
                            foreground: palette.indexed(self.brace_color),
                        });
                        in_brace = false;
                    }
                }

                b'\n' => {
                    if in_string {
                        out.push(Token {
                            byte_offset: start,
                            length: i - start,
                            foreground: palette.indexed(self.string_color),
                        });
                        in_string = true;
                        start = i + 1;
                    }

                    if in_brace {
                        out.push(Token {
                            byte_offset: start,
                            length: i - start,
                            foreground: palette.indexed(self.brace_color),
                        });
                        in_brace = true;
                        start = i + 1;
                    }
                }

                _ => {}
            }
        }

        if start < bytes.len() {
            if in_string {
                out.push(Token {
                    byte_offset: start,
                    length: bytes.len() - start,
                    foreground: palette.indexed(self.string_color),
                });
            }
            
            if in_brace {
                out.push(Token {
                    byte_offset: start,
                    length: bytes.len() - start,
                    foreground: palette.indexed(self.brace_color),
                });
            }
        }

        out
    }
}