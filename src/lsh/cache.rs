use crate::arena::{Arena, scratch_arena};
use crate::helpers::CoordType;
use crate::lsh::Higlight;
use crate::lsh::highlighter::{Highlighter, HighlighterState};

#[cfg(debug_assertions)]
const INTERVAL: CoordType = 16;
#[cfg(not(debug_assertions))]
const INTERVAL: CoordType = 1024;

#[derive(Default)]
pub struct HighlighterCache {
    checkpoints: Vec<HighlighterState>,
}

impl HighlighterCache {
    pub fn new() -> Self {
        Self::default()
    }

    /// Drop any cached states starting at (including) the given logical line.
    pub fn invalidate_from(&mut self, line: CoordType) {
        self.checkpoints.truncate(Self::ceil_line_to_offset(line));
    }

    /// Parse the given logical line. Returns the highlight spans.
    pub fn parse_line<'a>(
        &mut self,
        arena: &'a Arena,
        highlighter: &mut Highlighter,
        line: CoordType,
    ) -> Vec<Higlight, &'a Arena> {
        // Do we need to random seek?
        if line != highlighter.logical_pos_y() {
            // If so, restore the nearest, preceeding checkpoint...
            if !self.checkpoints.is_empty() {
                let n = Self::floor_line_to_offset(line);
                let n = n.min(self.checkpoints.len() - 1);
                highlighter.restore(&self.checkpoints[n]);
            } else {
                // The assumption is that you pass in a default constructed highlighter,
                // and this class handles random seeking for you. As such, there should
                // never be a case where we don't have a checkpoint for line 0,
                // but you have a highlighter for line >0.
                debug_assert!(highlighter.logical_pos_y() == 0);
            }

            // ...and then seek in front of the requested line.
            while highlighter.logical_pos_y() < line {
                // There's a bit of waste here, because we just throw away the results,
                // but that's better than duplicating the logic. The arena is very fast.
                let scratch = scratch_arena(Some(arena));
                _ = self.parse_line_impl(&scratch, highlighter);
            }
        }

        self.parse_line_impl(arena, highlighter)
    }

    fn parse_line_impl<'a>(
        &mut self,
        arena: &'a Arena,
        highlighter: &mut Highlighter,
    ) -> Vec<Higlight, &'a Arena> {
        // If we need to store a checkpoint for the start of the next line, do so now.
        if Self::floor_line_to_offset(highlighter.logical_pos_y()) == self.checkpoints.len() {
            self.checkpoints.push(highlighter.snapshot());
        }

        highlighter.parse_next_line(arena)
    }

    /// Since this line cache is super simplistic (no insertions, only append),
    /// we can directly map from line numbers to offsets in the cache.
    fn floor_line_to_offset(line: CoordType) -> usize {
        (line / INTERVAL).try_into().unwrap_or(0)
    }

    fn ceil_line_to_offset(line: CoordType) -> usize {
        ((line + INTERVAL - 1) / INTERVAL).try_into().unwrap_or(0)
    }
}
