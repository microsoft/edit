use std::ops::Range;

use crate::{document::ReadableDocument, simd::memchr2};

/// Cache a line/offset pair every CACHE_EVERY lines to speed up line/offset calculations
const CACHE_EVERY: usize = 1024 * 64;

#[derive(Clone)]
pub struct CachePoint {
    pub index: usize,
    pub line: usize,
    // pub snapshot: ParserSnapshot
}

pub struct LineCache {
    cache: Vec<CachePoint>,
}

impl LineCache {
    pub fn new() -> Self {
        Self { cache: vec![] }
    }

    pub fn from_document<T: ReadableDocument>(&mut self, document: &T) {
        self.cache.clear();

        let mut offset = 0;
        let mut line = 0;
        loop {
            let text = document.read_forward(offset);
            if text.is_empty() { return; }
            
            let mut off = 0;
            loop {
                off = memchr2(b'\n', b'\n', text, off);
                if off == text.len() { break; }

                if line % CACHE_EVERY == 0 {
                    self.cache.push(CachePoint { index: offset+off, line });
                }
                line += 1;
                off += 1;
            }

            offset += text.len();
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

        let mut beg_del = None;
        let mut end_del = None;
        for (i, point) in self.cache.iter_mut().enumerate() {
            if point.index >= range.start {
                if point.index < range.end {
                    // cache point is within the deleted range
                    if beg_del.is_none() { beg_del = Some(i); }
                    end_del = Some(i + 1);
                }
                else {
                    point.index -= text.len();
                    point.line -= newlines;
                }
            }
        }

        if let (Some(beg), Some(end)) = (beg_del, end_del) {
            self.cache.drain(beg..end);
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
        for point in &mut self.cache {
            if point.index > offset {
                point.index += len;
                point.line += newlines;
            }
        }

        // TODO: This also needs to insert new cache points
    }

    /// Finds the nearest cached line-offset pair relative to a target line.
    /// If `reverse` is false, it returns the closest *before* the target.
    /// If `reverse` is true, it returns the closest *after or at* the target.
    pub fn nearest_offset(&self, target_count: usize, reverse: bool) -> Option<CachePoint> {
        match self.cache.binary_search_by_key(&target_count, |p| p.line) {
            Ok(i) => Some(self.cache[i].clone()),
            Err(i) => {
                if i == 0 || i == self.cache.len() { None }  // target < lowest cache point || target > highest cache point
                else {
                    Some(self.cache[ if reverse {i} else {i-1} ].clone())
                }
            }
        }
    }
}
