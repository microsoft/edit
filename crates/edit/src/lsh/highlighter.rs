// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use lsh::runtime::*;
use stdext::arena::{Arena, scratch_arena};

use crate::document::ReadableDocument;
use crate::helpers::*;
use crate::lsh::definitions::*;
use crate::{simd, unicode};

const MAX_LINE_LEN: usize = 32 * KIBI;

#[derive(Clone)]
pub struct Highlighter<'a> {
    doc: &'a dyn ReadableDocument,
    offset: usize,
    logical_pos_y: CoordType,
    runtime: Runtime<'static, 'static, 'static>,
}

#[derive(Clone)]
pub struct HighlighterState {
    offset: usize,
    logical_pos_y: CoordType,
    state: RuntimeState,
}

impl<'doc> Highlighter<'doc> {
    pub fn new(doc: &'doc dyn ReadableDocument, language: &'static Language) -> Self {
        Self {
            doc,
            offset: 0,
            logical_pos_y: 0,
            runtime: Runtime::new(&ASSEMBLY, &STRINGS, &CHARSETS, language.entrypoint),
        }
    }

    pub fn logical_pos_y(&self) -> CoordType {
        self.logical_pos_y
    }

    /// Create a restorable snapshot of the current highlighter state
    /// so we can resume highlighting from this point later.
    pub fn snapshot(&self) -> HighlighterState {
        HighlighterState {
            offset: self.offset,
            logical_pos_y: self.logical_pos_y,
            state: self.runtime.snapshot(),
        }
    }

    /// Restore the highlighter state from a previously captured snapshot.
    pub fn restore(&mut self, snapshot: &HighlighterState) {
        self.offset = snapshot.offset;
        self.logical_pos_y = snapshot.logical_pos_y;
        self.runtime.restore(&snapshot.state);
    }

    pub fn parse_next_line<'a>(
        &mut self,
        arena: &'a Arena,
    ) -> Vec<Highlight<HighlightKind>, &'a Arena> {
        let scratch = scratch_arena(Some(arena));
        let (line_beg, line) = self.read_next_line(&scratch);

        // Empty lines can be somewhat common.
        //
        // If the line is too long, we don't highlight it.
        // This is to prevent performance issues with very long lines.
        if line.is_empty() || line.len() >= MAX_LINE_LEN {
            return Vec::new_in(arena);
        }

        let line = unicode::strip_newline(line);
        let mut res = self.runtime.parse_next_line(arena, line);

        // Adjust the range to account for the line offset.
        for h in &mut res {
            h.start = line_beg + h.start.min(line.len());
        }

        res
    }

    fn read_next_line<'a>(&mut self, arena: &'a Arena) -> (usize, &'a [u8])
    where
        'doc: 'a,
    {
        self.logical_pos_y += 1;

        let line_beg = self.offset;
        let mut chunk;
        let mut line_buf;

        // Try to read a chunk and see if it contains a newline.
        // In that case we can skip concatenating chunks.
        {
            chunk = self.doc.read_forward(self.offset);
            if chunk.is_empty() {
                return (line_beg, chunk);
            }

            let (off, line) = simd::lines_fwd(chunk, 0, 0, 1);
            self.offset += off;

            if line == 1 {
                return (line_beg, &chunk[..off]);
            }

            let next_chunk = self.doc.read_forward(self.offset);
            if next_chunk.is_empty() {
                return (line_beg, &chunk[..off]);
            }

            line_buf = Vec::new_in(arena);

            // Ensure we don't overflow the heap size with a 1GB long line.
            let end = off.min(MAX_LINE_LEN - line_buf.len());
            let end = end.min(chunk.len());
            line_buf.extend_from_slice(&chunk[..end]);

            chunk = next_chunk;
        }

        // Concatenate chunks until we get a full line.
        while line_buf.len() < MAX_LINE_LEN {
            let (off, line) = simd::lines_fwd(chunk, 0, 0, 1);
            self.offset += off;

            // Ensure we don't overflow the heap size with a 1GB long line.
            let end = off.min(MAX_LINE_LEN - line_buf.len());
            let end = end.min(chunk.len());
            line_buf.extend_from_slice(&chunk[..end]);

            // Start of the next line found.
            if line == 1 {
                break;
            }

            chunk = self.doc.read_forward(self.offset);
            if chunk.is_empty() {
                break;
            }
        }

        (line_beg, line_buf.leak())
    }
}
