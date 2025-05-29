use std::ops::Range;

use crate::document::ReadableDocument;

/// Cache a line/offset pair every CACHE_EVERY lines to speed up line/offset calculations
const CACHE_EVERY: usize = 1024 * 64;

pub struct LineCache {
    cache: Vec<(usize, usize)>, // (index, line)
}

impl LineCache {
    pub fn new() -> Self {
        Self { cache: vec![] }
    }

    pub fn from_document<T: ReadableDocument>(&mut self, document: &T) {
        self.cache.clear();

        let mut offset = 0;
        let mut line = 0;
        let mut last_line = 0;
        loop {
            let text = document.read_forward(offset);
            if text.is_empty() {
                return;
            }

            let len = text.len();
            let mut i = 0;
            while i < len {
                if line != last_line && line % CACHE_EVERY == 0 {
                    self.cache.push((offset + i, line));
                    last_line = line;
                }
                if text[i] == b'\n' {
                    line += 1;
                }
                i += 1;
            }
            offset += i;
        }
    }

    /// Updates the cache after a deletion.
    /// `range` is the deleted byte range, and `text` is the content that was deleted.
    pub fn delete(&mut self, range: Range<usize>, text: &Vec<u8>) {
        let mut newlines = 0;
        for c in text {
            if *c == b'\n' {
                newlines += 1;
            }
        }

        let len = text.len();
        let mut i = self.cache.len() - 1;
        loop {
            let (ref mut off, ref mut line) = self.cache[i];
            if *off > range.start {
                if *off < range.end {
                    self.cache.remove(i); // cache point is within the deleted range
                } else {
                    *off -= len;
                    *line -= newlines;
                }
            }
            if i == 0 {
                break;
            }
            i -= 1;
        }
    }

    /// Updates the cache after an insertion.
    /// `offset` is where the insertion occurs, and `text` is the inserted content.
    pub fn insert(&mut self, offset: usize, text: &[u8]) {
        // Count how many newlines were inserted
        let mut newlines = 0;
        for c in text {
            if *c == b'\n' {
                newlines += 1;
            }
        }

        let len = text.len();
        for &mut (ref mut off, ref mut line) in &mut self.cache {
            if *off > offset {
                *off += len;
                *line += newlines;
            }
        }
    }

    /// Finds the nearest cached line-offset pair relative to a target line.
    /// If `reverse` is false, it returns the closest *before* the target.
    /// If `reverse` is true, it returns the closest *after or at* the target.
    pub fn nearest_offset(&self, target_count: usize, reverse: bool) -> Option<(usize, usize)> {
        let len = self.cache.len();
        let mut i = 0;
        while i < len {
            if self.cache[i].1 >= target_count {
                let ind = if !reverse {
                    if i == 0 {
                        return None;
                    } // No previous line exists
                    i - 1
                } else {
                    if i == len - 1 {
                        return None;
                    } // No next line exists
                    i
                };
                return Some(self.cache[ind]);
            }
            i += 1;
        }

        None
    }
}
