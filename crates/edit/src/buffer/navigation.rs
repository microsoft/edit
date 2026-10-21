// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ops::Range;

use crate::document::ReadableDocument;

#[derive(Clone, Copy, PartialEq, Eq)]
enum CharClass {
    Whitespace,
    Newline,
    Separator,
    Word,
}

const fn construct_classifier(separators: &[u8]) -> [CharClass; 256] {
    let mut classifier = [CharClass::Word; 256];

    classifier[b' ' as usize] = CharClass::Whitespace;
    classifier[b'\t' as usize] = CharClass::Whitespace;
    classifier[b'\n' as usize] = CharClass::Newline;
    classifier[b'\r' as usize] = CharClass::Newline;

    let mut i = 0;
    let len = separators.len();
    while i < len {
        let ch = separators[i];
        assert!(ch < 128, "Only ASCII separators are supported.");
        classifier[ch as usize] = CharClass::Separator;
        i += 1;
    }

    classifier
}

const WORD_CLASSIFIER: [CharClass; 256] =
    construct_classifier(br#"`~!@#$%^&*()-=+[{]}\|;:'",.<>/?"#);

/// Finds the next word boundary given a document cursor offset.
/// Returns the offset of the next word boundary.
pub fn word_forward(doc: &dyn ReadableDocument, offset: usize) -> usize {
    word_navigation(WordForward { doc, offset, chunk: &[], chunk_off: 0 })
}

/// The backward version of `word_forward`.
pub fn word_backward(doc: &dyn ReadableDocument, offset: usize) -> usize {
    word_navigation(WordBackward { doc, offset, chunk: &[], chunk_off: 0 })
}

/// Word navigation implementation. Matches the behavior of VS Code.
fn word_navigation<T: WordNavigation>(mut nav: T) -> usize {
    // First, read the initial chunk.
    nav.read();

    // Skip one newline, if any.
    nav.skip_newline();

    // Skip any whitespace.
    nav.skip_class(CharClass::Whitespace);

    // Skip one word or separator and take note of the class.
    let class = nav.peek(CharClass::Whitespace);
    if matches!(class, CharClass::Separator | CharClass::Word) {
        nav.next();

        let off = nav.offset();

        // Continue skipping the same class.
        nav.skip_class(class);

        // If the class was a separator and we only moved one character,
        // continue skipping characters of the word class.
        if off == nav.offset() && class == CharClass::Separator {
            nav.skip_class(CharClass::Word);
        }
    }

    nav.offset()
}

trait WordNavigation {
    fn read(&mut self);
    fn skip_newline(&mut self);
    fn skip_class(&mut self, class: CharClass);
    fn peek(&self, default: CharClass) -> CharClass;
    fn next(&mut self);
    fn offset(&self) -> usize;
}

struct WordForward<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    chunk: &'a [u8],
    chunk_off: usize,
}

impl WordNavigation for WordForward<'_> {
    fn read(&mut self) {
        self.chunk = self.doc.read_forward(self.offset);
        self.chunk_off = 0;
    }

    fn skip_newline(&mut self) {
        match self.chunk {
            [b'\n', ..] => self.chunk_off = 1,
            [b'\r', rest @ ..] => {
                let rest =
                    if rest.is_empty() { self.doc.read_forward(self.offset + 1) } else { rest };

                // Only consume CR if followed by LF, even across chunks.
                if rest.first() == Some(&b'\n') {
                    self.offset += 1;
                    self.chunk = rest;
                    self.chunk_off = 1;
                }
            }
            _ => {}
        }
    }

    fn skip_class(&mut self, class: CharClass) {
        while !self.chunk.is_empty() {
            while self.chunk_off < self.chunk.len() {
                if WORD_CLASSIFIER[self.chunk[self.chunk_off] as usize] != class {
                    return;
                }
                self.chunk_off += 1;
            }

            self.offset += self.chunk.len();
            self.read();
        }
    }

    fn peek(&self, default: CharClass) -> CharClass {
        if self.chunk_off < self.chunk.len() {
            WORD_CLASSIFIER[self.chunk[self.chunk_off] as usize]
        } else {
            default
        }
    }

    fn next(&mut self) {
        self.chunk_off += 1;
    }

    fn offset(&self) -> usize {
        self.offset + self.chunk_off
    }
}

struct WordBackward<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    chunk: &'a [u8],
    chunk_off: usize,
}

impl WordNavigation for WordBackward<'_> {
    fn read(&mut self) {
        self.chunk = self.doc.read_backward(self.offset);
        self.chunk_off = self.chunk.len();
    }

    fn skip_newline(&mut self) {
        if self.chunk.get(self.chunk_off.wrapping_sub(1)) == Some(&b'\n') {
            self.chunk_off -= 1;
            if self.chunk_off == 0 {
                self.offset -= self.chunk.len();
                self.read();
            }
        }
        if self.chunk.get(self.chunk_off.wrapping_sub(1)) == Some(&b'\r') {
            self.chunk_off -= 1;
        }
    }

    fn skip_class(&mut self, class: CharClass) {
        while !self.chunk.is_empty() {
            while let Some(&ch) = self.chunk.get(self.chunk_off.wrapping_sub(1)) {
                if WORD_CLASSIFIER[ch as usize] != class {
                    return;
                }
                self.chunk_off -= 1;
            }

            self.offset -= self.chunk.len();
            self.read();
        }
    }

    fn peek(&self, default: CharClass) -> CharClass {
        if let Some(&ch) = self.chunk.get(self.chunk_off.wrapping_sub(1)) {
            WORD_CLASSIFIER[ch as usize]
        } else {
            default
        }
    }

    fn next(&mut self) {
        self.chunk_off -= 1;
    }

    fn offset(&self) -> usize {
        self.offset - self.chunk.len() + self.chunk_off
    }
}

/// Returns the offset range of the "word" at the given offset.
/// Does not cross newlines. Works similar to VS Code.
pub fn word_select(doc: &dyn ReadableDocument, offset: usize) -> Range<usize> {
    let mut beg = offset;
    let mut end = offset;
    let mut class = CharClass::Newline;

    let mut chunk = doc.read_forward(end);
    if !chunk.is_empty() {
        // Not at the end of the document? Great!
        // We default to using the next char as the class, because in terminals
        // the cursor is usually always to the left of the cell you clicked on.
        class = WORD_CLASSIFIER[chunk[0] as usize];

        let mut chunk_off = 0;

        // Select the word, unless we hit a newline.
        if class != CharClass::Newline {
            loop {
                chunk_off += 1;
                end += 1;

                if chunk_off >= chunk.len() {
                    chunk = doc.read_forward(end);
                    chunk_off = 0;
                    if chunk.is_empty() {
                        break;
                    }
                }

                if WORD_CLASSIFIER[chunk[chunk_off] as usize] != class {
                    break;
                }
            }
        }
    }

    let mut chunk = doc.read_backward(beg);
    if !chunk.is_empty() {
        let mut chunk_off = chunk.len();

        // If we failed to determine the class, because we hit the end of the document
        // or a newline, we fall back to using the previous character, of course.
        if class == CharClass::Newline {
            class = WORD_CLASSIFIER[chunk[chunk_off - 1] as usize];
        }

        // Select the word, unless we hit a newline.
        if class != CharClass::Newline {
            loop {
                if WORD_CLASSIFIER[chunk[chunk_off - 1] as usize] != class {
                    break;
                }

                chunk_off -= 1;
                beg -= 1;

                if chunk_off == 0 {
                    chunk = doc.read_backward(beg);
                    chunk_off = chunk.len();
                    if chunk.is_empty() {
                        break;
                    }
                }
            }
        }
    }

    beg..end
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::buffer::gap_buffer::GapBuffer;

    #[test]
    fn test_word_navigation_split_newline() {
        for (text, gap, forward, backward) in [
            ("\r\n", 1, 2, 0),
            ("Hello \r\n World", 7, 14, 0),
            ("\r\n\r\n", 1, 2, 0),
            ("\r\n\r\n", 3, 4, 2),
            ("\rX", 1, 0, 1),
        ] {
            let mut doc = GapBuffer::new(true).unwrap();
            doc.replace(0..0, text.as_bytes());
            doc.allocate_gap(gap, 0, 0);

            assert_eq!(doc.read_forward(0), &text.as_bytes()[..gap]);
            assert_eq!(doc.read_backward(text.len()), &text.as_bytes()[gap..]);
            assert_eq!(
                (word_forward(&doc, gap - 1), word_backward(&doc, gap + 1)),
                (forward, backward),
                "{text:?} with gap at {gap}",
            );
        }
    }

    #[test]
    fn test_word_navigation() {
        for (text, offset, expected) in [
            ("", 0, 0),
            ("Hello World", 2, 5),
            ("Hello,World", 0, 5),
            (" \t Hello", 0, 8),
            (" \t ", 0, 3),
            ("\n\nHello", 0, 1),
            ("\r\n \t Hello", 0, 10),
            ("\r", 0, 0),
            (",Hello", 0, 6),
            (".,!Hello", 0, 3),
        ] {
            assert_eq!(word_forward(&text.as_bytes(), offset), expected, "{text:?} at {offset}");
        }

        for (text, offset, expected) in [
            ("", 0, 0),
            ("Hello World", 11, 6),
            ("Hello,World", 10, 6),
            ("Hello \t ", 8, 0),
            (" \t ", 3, 0),
            ("Hello\n\n", 7, 6),
            ("Hello \t \r\n", 10, 0),
            ("Hello\r", 6, 0),
            ("Hello,", 6, 0),
            ("Hello.,!", 8, 5),
        ] {
            assert_eq!(word_backward(&text.as_bytes(), offset), expected, "{text:?} at {offset}");
        }
    }

    #[test]
    fn test_word_select() {
        for (text, offset, expected) in [
            ("", 0, 0..0),
            ("h\u{e9}llo", 3, 0..6),
            ("Hello World", 6, 6..11),
            ("Hello World", 11, 6..11),
            ("Hello.,!World", 6, 5..8),
            ("Hello \t World", 6, 5..8),
            ("\nHello\n", 3, 1..6),
            ("Hello\r\nWorld", 5, 0..5),
            ("Hello\n\n", 6, 6..6),
            ("Hello\r\n", 7, 7..7),
        ] {
            assert_eq!(word_select(&text.as_bytes(), offset), expected, "{text:?} at {offset}");
        }
    }
}
