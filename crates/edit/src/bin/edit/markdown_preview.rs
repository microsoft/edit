// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

mod layout;
mod parser;

use edit::buffer::RcTextBuffer;
use edit::helpers::{CoordType, KIBI, MEBI};

use crate::localization::{LocId, loc};

use self::layout::{render_blocks, render_message};
use self::parser::{Block, parse_blocks};

/// Markdown parsing and layout operate on the whole source, so previews are
/// deliberately bounded to preserve the editor's large-file responsiveness.
const MAX_PREVIEW_BYTES: usize = MEBI;
/// Match LSH's safeguard against pathological single lines.
const MAX_PREVIEW_LINE_BYTES: usize = 32 * KIBI;

#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct InlineStyle(u8);

impl InlineStyle {
    const BOLD: Self = Self(1);
    const ITALIC: Self = Self(2);
    const STRIKETHROUGH: Self = Self(4);
    const CODE: Self = Self(8);
    const LINK: Self = Self(16);

    pub(super) const fn with(self, other: Self) -> Self {
        Self(self.0 | other.0)
    }

    pub const fn is_bold(self) -> bool {
        self.0 & Self::BOLD.0 != 0
    }

    pub const fn is_italic(self) -> bool {
        self.0 & Self::ITALIC.0 != 0
    }

    pub const fn is_strikethrough(self) -> bool {
        self.0 & Self::STRIKETHROUGH.0 != 0
    }

    pub const fn is_code(self) -> bool {
        self.0 & Self::CODE.0 != 0
    }

    pub const fn is_link(self) -> bool {
        self.0 & Self::LINK.0 != 0
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct InlineSpan {
    pub text: String,
    pub style: InlineStyle,
}

pub(super) fn push_span(spans: &mut Vec<InlineSpan>, text: &str, style: InlineStyle) {
    if text.is_empty() {
        return;
    }

    if let Some(last) = spans.last_mut()
        && last.style == style
    {
        last.text.push_str(text);
    } else {
        spans.push(InlineSpan { text: text.to_string(), style });
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum RenderedLineKind {
    Text,
    Heading(u8),
    Quote,
    Rule,
    Code,
    Unavailable,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RenderedLine {
    pub kind: RenderedLineKind,
    pub spans: Vec<InlineSpan>,
}

struct PreviewCache {
    source: Option<Vec<u8>>,
    blocks: Vec<Block>,
    generation: u32,
    unavailable: Option<PreviewUnavailable>,
    width: CoordType,
    lines: Vec<RenderedLine>,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum PreviewUnavailable {
    TotalSize,
    LineLength,
}

struct SourceSnapshot {
    source: Option<Vec<u8>>,
    generation: u32,
    unavailable: Option<PreviewUnavailable>,
}

#[derive(Default)]
pub struct MarkdownPreview {
    enabled: bool,
    cache: Option<PreviewCache>,
    force_validation: bool,
    reset_scroll: bool,
    #[cfg(test)]
    rebuild_count: usize,
    #[cfg(test)]
    parse_count: usize,
    #[cfg(test)]
    layout_count: usize,
    #[cfg(test)]
    source_validation_count: usize,
}

impl MarkdownPreview {
    pub fn is_enabled(&self) -> bool {
        self.enabled
    }

    pub fn set_enabled(&mut self, enabled: bool) {
        if self.enabled == enabled {
            return;
        }

        self.enabled = enabled;
        if enabled {
            self.force_validation = true;
            self.reset_scroll = true;
        }
    }

    pub fn active_document_changed(&mut self) {
        self.reset_scroll = true;
    }

    pub fn prepare(&mut self, buffer: &RcTextBuffer, width: CoordType) {
        let width = width.max(2);
        let generation = buffer.borrow().generation();
        let force_validation = std::mem::take(&mut self.force_validation);

        if self.cache.as_ref().is_some_and(|cache| cache.generation == generation)
            && !force_validation
        {
            self.relayout(width);
            return;
        }

        #[cfg(test)]
        {
            self.source_validation_count += 1;
        }
        let snapshot = read_source(buffer);
        if self.cache.as_ref().is_some_and(|cache| {
            cache
                .source
                .as_ref()
                .zip(snapshot.source.as_ref())
                .is_some_and(|(cached, source)| cached == source)
        }) {
            self.cache.as_mut().unwrap().generation = snapshot.generation;
            self.relayout(width);
            return;
        }

        self.rebuild(snapshot, width);
        self.reset_scroll = true;
    }

    pub fn lines(&self) -> &[RenderedLine] {
        self.cache.as_ref().map_or(&[], |cache| cache.lines.as_slice())
    }

    pub fn take_scroll_reset(&mut self) -> bool {
        std::mem::take(&mut self.reset_scroll)
    }

    fn rebuild(&mut self, snapshot: SourceSnapshot, width: CoordType) {
        let blocks = if snapshot.unavailable.is_none() {
            let source = String::from_utf8_lossy(snapshot.source.as_deref().unwrap_or_default());
            #[cfg(test)]
            {
                self.parse_count += 1;
            }
            parse_blocks(&source)
        } else {
            Vec::new()
        };
        let lines = render(&blocks, snapshot.unavailable, width);
        self.cache = Some(PreviewCache {
            source: snapshot.source,
            blocks,
            generation: snapshot.generation,
            unavailable: snapshot.unavailable,
            width,
            lines,
        });
        #[cfg(test)]
        {
            self.rebuild_count += 1;
            self.layout_count += 1;
        }
    }

    fn relayout(&mut self, width: CoordType) {
        let Some(cache) = &mut self.cache else {
            return;
        };
        if cache.width != width {
            cache.lines = render(&cache.blocks, cache.unavailable, width);
            cache.width = width;
            #[cfg(test)]
            {
                self.layout_count += 1;
            }
        }
    }
}

fn render(
    blocks: &[Block],
    unavailable: Option<PreviewUnavailable>,
    width: CoordType,
) -> Vec<RenderedLine> {
    match unavailable {
        Some(PreviewUnavailable::TotalSize) => {
            render_message(loc(LocId::MarkdownPreviewUnavailableTooLarge), width)
        }
        Some(PreviewUnavailable::LineLength) => {
            render_message(loc(LocId::MarkdownPreviewUnavailableLineTooLong), width)
        }
        None => render_blocks(blocks, width),
    }
}

fn read_source(buffer: &RcTextBuffer) -> SourceSnapshot {
    let tb = buffer.borrow();
    let generation = tb.generation();
    if tb.text_length() > MAX_PREVIEW_BYTES {
        return SourceSnapshot {
            source: None,
            generation,
            unavailable: Some(PreviewUnavailable::TotalSize),
        };
    }

    let mut bytes = Vec::with_capacity(tb.text_length());
    let mut offset = 0;
    let mut line_length = 0;
    let mut line_too_long = false;

    while offset < tb.text_length() {
        let chunk = tb.read_forward(offset);
        if chunk.is_empty() {
            break;
        }
        bytes.extend_from_slice(chunk);
        for &byte in chunk {
            if byte == b'\n' {
                line_length = 0;
            } else {
                line_length += 1;
                line_too_long |= line_length > MAX_PREVIEW_LINE_BYTES;
            }
        }
        offset += chunk.len();
    }

    SourceSnapshot {
        source: Some(bytes),
        generation,
        unavailable: line_too_long.then_some(PreviewUnavailable::LineLength),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use edit::buffer::TextBuffer;

    fn buffer_with_bytes(source: &[u8]) -> RcTextBuffer {
        let buffer = TextBuffer::new_rc(false).unwrap();
        buffer.borrow_mut().write_raw(source);
        buffer
    }

    fn buffer_with(source: &str) -> RcTextBuffer {
        buffer_with_bytes(source.as_bytes())
    }

    fn rendered_text(preview: &MarkdownPreview) -> String {
        preview
            .lines()
            .iter()
            .flat_map(|line| line.spans.iter())
            .map(|span| span.text.as_str())
            .collect()
    }

    #[test]
    fn unchanged_valid_utf8_does_not_rebuild_or_reparse() {
        let buffer = buffer_with("# Before");
        let mut preview = MarkdownPreview::default();

        preview.prepare(&buffer, 80);
        let rebuild_count = preview.rebuild_count;
        let parse_count = preview.parse_count;
        let source_validation_count = preview.source_validation_count;
        preview.prepare(&buffer, 80);

        assert_eq!(preview.rebuild_count, rebuild_count);
        assert_eq!(preview.parse_count, parse_count);
        assert_eq!(preview.source_validation_count, source_validation_count);
        assert_eq!(rendered_text(&preview), "Before");
    }

    #[test]
    fn unchanged_invalid_utf8_does_not_rebuild_or_reparse() {
        let buffer = buffer_with_bytes(b"# invalid \xff");
        let mut preview = MarkdownPreview::default();

        preview.prepare(&buffer, 80);
        let rebuild_count = preview.rebuild_count;
        let parse_count = preview.parse_count;
        let source_validation_count = preview.source_validation_count;
        preview.prepare(&buffer, 80);

        assert_eq!(preview.rebuild_count, rebuild_count);
        assert_eq!(preview.parse_count, parse_count);
        assert_eq!(preview.source_validation_count, source_validation_count);
        assert_eq!(rendered_text(&preview), "invalid \u{fffd}");
    }

    #[test]
    fn content_change_rebuilds_and_requests_scroll_reset() {
        let buffer = buffer_with("# Before");
        let mut preview = MarkdownPreview::default();

        preview.prepare(&buffer, 80);
        assert!(preview.take_scroll_reset());

        buffer.borrow_mut().write_raw(b" and after");
        preview.prepare(&buffer, 80);

        assert_eq!(rendered_text(&preview), "Before and after");
        assert!(preview.take_scroll_reset());
    }

    #[test]
    fn reenable_cannot_reuse_stale_cache_after_generation_reuse() {
        let buffer = buffer_with("# Cached");
        let mut preview = MarkdownPreview::default();
        preview.set_enabled(true);
        preview.prepare(&buffer, 80);
        let cached_generation = buffer.borrow().generation();

        preview.set_enabled(false);
        buffer.borrow_mut().undo();
        buffer.borrow_mut().write_raw(b"# Replacement");
        assert_eq!(buffer.borrow().generation(), cached_generation);

        preview.set_enabled(true);
        preview.prepare(&buffer, 80);

        assert_eq!(rendered_text(&preview), "Replacement");
    }

    #[test]
    fn total_size_limit_renders_unavailable_state() {
        let source = vec![b'a'; MAX_PREVIEW_BYTES + 1];
        let buffer = buffer_with_bytes(&source);
        let mut preview = MarkdownPreview::default();

        preview.prepare(&buffer, 80);

        assert_eq!(rendered_text(&preview), loc(LocId::MarkdownPreviewUnavailableTooLarge));
        assert_eq!(
            preview.cache.as_ref().unwrap().unavailable,
            Some(PreviewUnavailable::TotalSize)
        );
        assert_eq!(preview.parse_count, 0);
    }

    #[test]
    fn line_length_limit_renders_unavailable_state() {
        let source = vec![b'a'; MAX_PREVIEW_LINE_BYTES + 1];
        let buffer = buffer_with_bytes(&source);
        let mut preview = MarkdownPreview::default();

        preview.prepare(&buffer, 80);

        assert_eq!(rendered_text(&preview), loc(LocId::MarkdownPreviewUnavailableLineTooLong));
        assert_eq!(
            preview.cache.as_ref().unwrap().unavailable,
            Some(PreviewUnavailable::LineLength)
        );
        assert_eq!(preview.parse_count, 0);
    }

    #[test]
    fn width_only_relayout_does_not_request_scroll_reset() {
        let buffer = buffer_with("words that wrap across several preview lines");
        let mut preview = MarkdownPreview::default();

        preview.prepare(&buffer, 10);
        assert!(preview.take_scroll_reset());
        let parse_count = preview.parse_count;
        let layout_count = preview.layout_count;

        preview.prepare(&buffer, 20);

        assert!(!preview.take_scroll_reset());
        assert_eq!(preview.parse_count, parse_count);
        assert_eq!(preview.layout_count, layout_count + 1);
    }
}
