use crate::helpers::CoordType;
use crate::lsh::highlighter::{Highlighter, ParserState};

#[derive(Clone)]
struct Checkpoint {
    line: CoordType, // snapshot corresponds to the start of this logical line
    state: ParserState,
}

pub struct HighlighterCache {
    checkpoints: Vec<Checkpoint>,
    interval: CoordType,
}

impl Default for HighlighterCache {
    fn default() -> Self {
        Self { checkpoints: Vec::new(), interval: 1000 }
    }
}

impl HighlighterCache {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn set_interval(&mut self, interval: CoordType) {
        self.interval = interval.max(1);
    }

    pub fn clear_all(&mut self) {
        self.checkpoints.clear();
    }

    /// Drop any cached state starting at the given logical line.
    pub fn invalidate_from(&mut self, line: CoordType) {
        if self.checkpoints.is_empty() {
            return;
        }
        let idx = match self.checkpoints.binary_search_by_key(&line, |c| c.line) {
            Ok(i) => i,
            Err(i) => i, // first checkpoint with line > given `line` is at position i
        };
        self.checkpoints.truncate(idx);
    }

    /// Prepare the highlighter to start parsing from the last checkpoint before or at `target_line`.
    /// If none exists, do nothing (the caller will parse from the start).
    pub fn prepare(&self, h: &mut Highlighter, target_line: CoordType) {
        if self.checkpoints.is_empty() {
            return;
        }
        let idx = match self.checkpoints.binary_search_by_key(&target_line, |c| c.line) {
            Ok(i) => Some(i),
            Err(0) => None,
            Err(i) => Some(i - 1),
        };
        if let Some(i) = idx {
            h.restore(&self.checkpoints[i].state);
        }
    }

    /// After parsing a line, maybe store a checkpoint. The snapshot at this time
    /// corresponds to the start of the next logical line, which is ideal for resuming.
    pub fn maybe_store_after_parse(&mut self, h: &Highlighter) {
        let next_line = h.logical_pos_y().saturating_add(1);
        if next_line < 0 {
            return;
        }
        if next_line % self.interval != 0 {
            return;
        }
        if self.checkpoints.last().is_some_and(|c| c.line == next_line) {
            return;
        }
        self.checkpoints.push(Checkpoint { line: next_line, state: h.snapshot() });
    }
}
