// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use super::{InlineSpan, InlineStyle, push_span};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) enum Block {
    Heading { level: u8, spans: Vec<InlineSpan> },
    Paragraph(Vec<InlineSpan>),
    ListItem { marker: String, spans: Vec<InlineSpan> },
    Quote(Vec<InlineSpan>),
    Rule,
    Code(Vec<String>),
}

pub(super) fn parse_blocks(source: &str) -> Vec<Block> {
    let lines: Vec<_> = source.lines().collect();
    let mut blocks = Vec::new();
    let mut paragraph = String::new();
    let mut index = 0;

    while index < lines.len() {
        let line = lines[index];
        let trimmed = line.trim();
        let trimmed_start = line.trim_start();

        if trimmed.is_empty() {
            flush_paragraph(&mut blocks, &mut paragraph);
            index += 1;
            continue;
        }

        if let Some((fence, count)) = fence_start(trimmed_start) {
            flush_paragraph(&mut blocks, &mut paragraph);
            let mut code = Vec::new();
            index += 1;

            while index < lines.len() {
                let line = lines[index];
                if fence_end(line.trim_start(), fence, count) {
                    index += 1;
                    break;
                }
                code.push(line.to_string());
                index += 1;
            }

            blocks.push(Block::Code(code));
            continue;
        }

        if let Some((level, text)) = heading(trimmed_start) {
            flush_paragraph(&mut blocks, &mut paragraph);
            blocks.push(Block::Heading {
                level,
                spans: parse_inline_styled(text, InlineStyle::BOLD),
            });
        } else if horizontal_rule(trimmed_start) {
            flush_paragraph(&mut blocks, &mut paragraph);
            blocks.push(Block::Rule);
        } else if let Some((marker, text)) = list_item(trimmed_start) {
            flush_paragraph(&mut blocks, &mut paragraph);
            blocks.push(Block::ListItem { marker, spans: parse_inline(text) });
        } else if let Some(text) = trimmed_start.strip_prefix('>') {
            flush_paragraph(&mut blocks, &mut paragraph);
            blocks.push(Block::Quote(parse_inline_styled(
                text.strip_prefix(' ').unwrap_or(text),
                InlineStyle::ITALIC,
            )));
        } else {
            if !paragraph.is_empty() {
                paragraph.push(' ');
            }
            paragraph.push_str(trimmed);
        }

        index += 1;
    }

    flush_paragraph(&mut blocks, &mut paragraph);
    blocks
}

fn flush_paragraph(blocks: &mut Vec<Block>, paragraph: &mut String) {
    if paragraph.is_empty() {
        return;
    }

    blocks.push(Block::Paragraph(parse_inline(paragraph)));
    paragraph.clear();
}

fn fence_start(line: &str) -> Option<(char, usize)> {
    let fence = line.chars().next()?;
    if !matches!(fence, '`' | '~') {
        return None;
    }

    let count = line.chars().take_while(|&ch| ch == fence).count();
    (count >= 3).then_some((fence, count))
}

fn fence_end(line: &str, fence: char, count: usize) -> bool {
    let actual = line.chars().take_while(|&ch| ch == fence).count();
    actual >= count && line[actual..].trim().is_empty()
}

fn heading(line: &str) -> Option<(u8, &str)> {
    let level = line.bytes().take_while(|&byte| byte == b'#').count();
    if !(1..=6).contains(&level) {
        return None;
    }

    let rest = &line[level..];
    if !rest.is_empty() && !rest.starts_with(char::is_whitespace) {
        return None;
    }

    let mut text = rest.trim();
    if text.ends_with('#') {
        text = text.trim_end_matches('#').trim_end();
    }
    Some((level as u8, text))
}

fn horizontal_rule(line: &str) -> bool {
    let mut marker = None;
    let mut count = 0;

    for ch in line.chars().filter(|ch| !ch.is_whitespace()) {
        if !matches!(ch, '-' | '_' | '*') {
            return false;
        }
        if marker.is_some_and(|marker| marker != ch) {
            return false;
        }
        marker = Some(ch);
        count += 1;
    }

    count >= 3
}

fn list_item(line: &str) -> Option<(String, &str)> {
    let bytes = line.as_bytes();
    if bytes.len() >= 2 && matches!(bytes[0], b'-' | b'*' | b'+') && bytes[1].is_ascii_whitespace()
    {
        return Some(("•".to_string(), line[2..].trim_start()));
    }

    let digits = bytes.iter().take_while(|byte| byte.is_ascii_digit()).count();
    if digits == 0 || digits + 1 >= bytes.len() {
        return None;
    }
    if !matches!(bytes[digits], b'.' | b')') || !bytes[digits + 1].is_ascii_whitespace() {
        return None;
    }

    Some((line[..=digits].to_string(), line[digits + 2..].trim_start()))
}

pub(super) fn parse_inline(text: &str) -> Vec<InlineSpan> {
    parse_inline_styled(text, InlineStyle::default())
}

fn parse_inline_styled(text: &str, base_style: InlineStyle) -> Vec<InlineSpan> {
    let mut metrics = InlineSearchMetrics::default();
    parse_inline_styled_with_metrics(text, base_style, &mut metrics)
}

struct BacktickSearch {
    max_run_from: Vec<usize>,
}

impl BacktickSearch {
    fn new(text: &str) -> Self {
        let bytes = text.as_bytes();
        let mut max_run_from = vec![0; bytes.len() + 1];
        let mut run = 0;

        for index in (0..bytes.len()).rev() {
            run = if bytes[index] == b'`' { run + 1 } else { 0 };
            max_run_from[index] = max_run_from[index + 1].max(run);
        }

        Self { max_run_from }
    }

    fn has_delimiter(&self, from: usize, count: usize) -> bool {
        self.max_run_from[from] >= count
    }
}

#[derive(Default)]
struct LinkSearch {
    delimiter: Option<usize>,
    delimiter_search_from: usize,
    delimiter_exhausted: bool,
    end: Option<(usize, LinkEnd)>,
}

#[derive(Clone, Copy)]
enum LinkEnd {
    Close(usize),
    NestedOpen,
    Missing,
}

struct LinkMatch {
    label_start: usize,
    label_end: usize,
    url_start: usize,
    url_end: usize,
}

impl LinkSearch {
    fn find(
        &mut self,
        text: &str,
        open: usize,
        metrics: &mut InlineSearchMetrics,
    ) -> Option<LinkMatch> {
        if matches!(self.end, Some((_, LinkEnd::Missing))) {
            return None;
        }

        let delimiter_min = open + 1;
        if self.delimiter.is_some_and(|delimiter| delimiter < delimiter_min) {
            self.delimiter = None;
            self.end = None;
        }

        let delimiter = if let Some(delimiter) = self.delimiter {
            delimiter
        } else {
            if self.delimiter_exhausted {
                return None;
            }

            let search_from = self.delimiter_search_from.max(delimiter_min);
            let remaining = &text[search_from..];
            let relative = remaining.find("](");
            metrics
                .record_delimiter_search(relative.map_or(remaining.len(), |relative| relative + 2));

            let Some(relative) = relative else {
                self.delimiter_exhausted = true;
                return None;
            };

            let delimiter = search_from + relative;
            self.delimiter = Some(delimiter);
            self.delimiter_search_from = delimiter + 2;
            delimiter
        };

        let end = if let Some((cached_delimiter, end)) = self.end
            && cached_delimiter == delimiter
        {
            end
        } else {
            let url_start = delimiter + 2;
            let remaining = &text.as_bytes()[url_start..];
            let relative = remaining.iter().position(|byte| matches!(byte, b')' | b'['));
            metrics
                .record_url_end_search(relative.map_or(remaining.len(), |relative| relative + 1));

            let end = match relative {
                Some(relative) if remaining[relative] == b')' => {
                    LinkEnd::Close(url_start + relative)
                }
                Some(_) => LinkEnd::NestedOpen,
                None => LinkEnd::Missing,
            };
            self.end = Some((delimiter, end));
            end
        };

        let LinkEnd::Close(url_end) = end else {
            return None;
        };

        Some(LinkMatch {
            label_start: open + 1,
            label_end: delimiter,
            url_start: delimiter + 2,
            url_end,
        })
    }
}

#[derive(Default)]
struct InlineSearchMetrics {
    #[cfg(test)]
    inline_iterations: usize,
    #[cfg(test)]
    code_opening_runs: usize,
    #[cfg(test)]
    code_delimiter_searches: usize,
    #[cfg(test)]
    code_delimiter_bytes: usize,
    #[cfg(test)]
    delimiter_searches: usize,
    #[cfg(test)]
    delimiter_bytes: usize,
    #[cfg(test)]
    url_end_searches: usize,
    #[cfg(test)]
    url_end_bytes: usize,
}

impl InlineSearchMetrics {
    #[cfg(test)]
    fn record_inline_iteration(&mut self) {
        self.inline_iterations += 1;
    }

    #[cfg(not(test))]
    fn record_inline_iteration(&mut self) {}

    #[cfg(test)]
    fn record_code_opening_run(&mut self) {
        self.code_opening_runs += 1;
    }

    #[cfg(not(test))]
    fn record_code_opening_run(&mut self) {}

    #[cfg(test)]
    fn record_code_delimiter_search(&mut self, bytes: usize) {
        self.code_delimiter_searches += 1;
        self.code_delimiter_bytes += bytes;
    }

    #[cfg(not(test))]
    fn record_code_delimiter_search(&mut self, _bytes: usize) {}

    #[cfg(test)]
    fn record_delimiter_search(&mut self, bytes: usize) {
        self.delimiter_searches += 1;
        self.delimiter_bytes += bytes;
    }

    #[cfg(not(test))]
    fn record_delimiter_search(&mut self, _bytes: usize) {}

    #[cfg(test)]
    fn record_url_end_search(&mut self, bytes: usize) {
        self.url_end_searches += 1;
        self.url_end_bytes += bytes;
    }

    #[cfg(not(test))]
    fn record_url_end_search(&mut self, _bytes: usize) {}
}

fn parse_inline_styled_with_metrics(
    text: &str,
    base_style: InlineStyle,
    metrics: &mut InlineSearchMetrics,
) -> Vec<InlineSpan> {
    let mut spans = Vec::new();
    let mut backtick_search = None;
    let mut link_search = LinkSearch::default();
    let mut offset = 0;

    while offset < text.len() {
        metrics.record_inline_iteration();
        let remaining = &text[offset..];

        if let Some(escaped) = remaining.strip_prefix('\\')
            && let Some(ch) = escaped.chars().next()
        {
            let len = ch.len_utf8();
            push_span(&mut spans, &escaped[..len], base_style);
            offset += 1 + len;
            continue;
        }

        if remaining.starts_with('`') {
            let count = remaining.bytes().take_while(|&byte| byte == b'`').count();
            let delimiter = &remaining[..count];
            metrics.record_code_opening_run();

            if backtick_search
                .get_or_insert_with(|| BacktickSearch::new(text))
                .has_delimiter(offset + count, count)
            {
                let end = remaining[count..]
                    .find(delimiter)
                    .expect("backtick index must identify an existing delimiter");
                metrics.record_code_delimiter_search(end + count);
                let end = count + end;
                push_span(&mut spans, &remaining[count..end], base_style.with(InlineStyle::CODE));
                offset += end + count;
                continue;
            }

            push_span(&mut spans, delimiter, base_style);
            offset += count;
            continue;
        }

        if let Some((delimiter, style)) = [
            ("**", InlineStyle::BOLD),
            ("__", InlineStyle::BOLD),
            ("~~", InlineStyle::STRIKETHROUGH),
            ("*", InlineStyle::ITALIC),
            ("_", InlineStyle::ITALIC),
        ]
        .into_iter()
        .find(|(delimiter, _)| remaining.starts_with(delimiter))
        {
            let start = delimiter.len();
            if let Some(end) = remaining[start..].find(delimiter)
                && end != 0
            {
                let end = start + end;
                append_spans(
                    &mut spans,
                    parse_inline_styled_with_metrics(
                        &remaining[start..end],
                        base_style.with(style),
                        metrics,
                    ),
                );
                offset += end + start;
                continue;
            }
        }

        if remaining.starts_with('[')
            && let Some(link) = link_search.find(text, offset, metrics)
        {
            append_spans(
                &mut spans,
                parse_inline_styled_with_metrics(
                    &text[link.label_start..link.label_end],
                    base_style.with(InlineStyle::LINK),
                    metrics,
                ),
            );
            push_span(&mut spans, " (", base_style.with(InlineStyle::LINK));
            push_span(
                &mut spans,
                &text[link.url_start..link.url_end],
                base_style.with(InlineStyle::LINK),
            );
            push_span(&mut spans, ")", base_style.with(InlineStyle::LINK));
            offset = link.url_end + 1;
            continue;
        }

        let ch = remaining.chars().next().unwrap();
        push_span(&mut spans, &remaining[..ch.len_utf8()], base_style);
        offset += ch.len_utf8();
    }

    spans
}

#[cfg(test)]
fn parse_inline_with_metrics(text: &str) -> (Vec<InlineSpan>, InlineSearchMetrics) {
    let mut metrics = InlineSearchMetrics::default();
    let spans = parse_inline_styled_with_metrics(text, InlineStyle::default(), &mut metrics);
    (spans, metrics)
}

fn append_spans(destination: &mut Vec<InlineSpan>, source: Vec<InlineSpan>) {
    for span in source {
        push_span(destination, &span.text, span.style);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_supported_block_types() {
        let blocks = parse_blocks(
            "# Heading\n\nParagraph\n\n- bullet\n2. ordered\n> quote\n---\n```rust\nfn main() {}\n```\n",
        );

        assert!(matches!(blocks[0], Block::Heading { level: 1, .. }));
        assert!(matches!(blocks[1], Block::Paragraph(_)));
        assert!(matches!(&blocks[2], Block::ListItem { marker, .. } if marker == "•"));
        assert!(matches!(&blocks[3], Block::ListItem { marker, .. } if marker == "2."));
        assert!(matches!(blocks[4], Block::Quote(_)));
        assert!(matches!(blocks[5], Block::Rule));
        assert!(matches!(&blocks[6], Block::Code(lines) if lines == &["fn main() {}"]));
    }

    #[test]
    fn parses_supported_inline_styles() {
        let spans =
            parse_inline("plain **bold** *italic* ~~strike~~ `code` [link](https://example.com)");

        assert!(spans.iter().any(|span| span.text == "bold" && span.style.is_bold()));
        assert!(spans.iter().any(|span| span.text == "italic" && span.style.is_italic()));
        assert!(spans.iter().any(|span| span.text == "strike" && span.style.is_strikethrough()));
        assert!(spans.iter().any(|span| span.text == "code" && span.style.is_code()));
        assert!(spans.iter().any(|span| span.text.contains("link") && span.style.is_link()));
        assert!(spans.iter().any(|span| span.text.contains("https://example.com")));
    }

    #[test]
    fn malformed_and_incomplete_markup_is_preserved_safely() {
        let blocks = parse_blocks(
            "Text **without a close and [broken](url\n\n> *unterminated\n\n```\nunterminated",
        );

        let Block::Paragraph(spans) = &blocks[0] else {
            panic!("expected paragraph");
        };
        assert_eq!(
            spans.iter().map(|span| span.text.as_str()).collect::<String>(),
            "Text **without a close and [broken](url"
        );
        assert!(matches!(blocks[1], Block::Quote(_)));
        assert!(matches!(&blocks[2], Block::Code(lines) if lines == &["unterminated"]));
    }

    #[test]
    fn bracket_heavy_links_have_bounded_searches_and_recover_after_malformed_input() {
        const BRACKETS: usize = 256 * 1024;

        let no_delimiter = "[".repeat(BRACKETS);
        let (spans, metrics) = parse_inline_with_metrics(&no_delimiter);
        assert_eq!(spans.iter().map(|span| span.text.as_str()).collect::<String>(), no_delimiter);
        assert_eq!(metrics.delimiter_searches, 1);
        assert_eq!(metrics.delimiter_bytes, BRACKETS - 1);
        assert_eq!(metrics.url_end_searches, 0);

        let missing_close = format!("{}](", "[".repeat(BRACKETS));
        let (spans, metrics) = parse_inline_with_metrics(&missing_close);
        assert_eq!(spans.iter().map(|span| span.text.as_str()).collect::<String>(), missing_close);
        assert_eq!(metrics.delimiter_searches, 1);
        assert_eq!(metrics.url_end_searches, 1);
        assert!(metrics.delimiter_bytes + metrics.url_end_bytes <= missing_close.len());

        let recovers = format!("{}](unterminated [valid](ok)", "[".repeat(BRACKETS));
        let (spans, metrics) = parse_inline_with_metrics(&recovers);
        assert!(spans.iter().any(|span| span.text == "valid (ok)" && span.style.is_link()));
        assert_eq!(metrics.delimiter_searches, 2);
        assert_eq!(metrics.url_end_searches, 2);
        assert!(metrics.delimiter_bytes <= recovers.len());
        assert!(metrics.url_end_bytes <= recovers.len());
    }

    #[test]
    fn unmatched_backtick_runs_have_bounded_work_and_recover_for_later_code() {
        const BACKTICKS: usize = 32 * 1024;

        let spans = parse_inline("before ``code ` inside`` after");
        assert!(spans.iter().any(|span| span.text == "code ` inside" && span.style.is_code()));

        let unmatched = "`".repeat(BACKTICKS);
        let (spans, metrics) = parse_inline_with_metrics(&unmatched);
        assert_eq!(spans.iter().map(|span| span.text.as_str()).collect::<String>(), unmatched);
        assert_eq!(metrics.inline_iterations, 1);
        assert_eq!(metrics.code_opening_runs, 1);
        assert_eq!(metrics.code_delimiter_searches, 0);
        assert_eq!(metrics.code_delimiter_bytes, 0);

        let recovers = "``malformed `valid` tail";
        let (spans, metrics) = parse_inline_with_metrics(recovers);
        assert_eq!(
            spans.iter().map(|span| span.text.as_str()).collect::<String>(),
            "``malformed valid tail"
        );
        assert!(spans.iter().any(|span| span.text == "valid" && span.style.is_code()));
        assert_eq!(metrics.code_opening_runs, 2);
        assert_eq!(metrics.code_delimiter_searches, 1);
        assert!(metrics.code_delimiter_bytes <= recovers.len());
    }

    #[test]
    fn highlighting_fixture_exercises_representative_blocks_and_styles() {
        let source = include_str!(concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/../../assets/highlighting-tests/markdown.md"
        ));
        let blocks = parse_blocks(source);

        for level in 1..=6 {
            assert!(
                blocks.iter().any(
                    |block| matches!(block, Block::Heading { level: actual, .. } if *actual == level)
                ),
                "missing heading level {level}"
            );
        }
        assert!(blocks.iter().filter(|block| matches!(block, Block::ListItem { .. })).count() >= 8);
        assert!(blocks.iter().any(|block| matches!(block, Block::Quote(_))));
        assert!(blocks.iter().filter(|block| matches!(block, Block::Code(_))).count() >= 5);
        assert!(blocks.iter().any(
            |block| matches!(block, Block::Code(lines) if lines.iter().any(|line| line.contains("Hello, world")))
        ));

        let spans = blocks.iter().filter_map(|block| match block {
            Block::Paragraph(spans)
            | Block::ListItem { spans, .. }
            | Block::Quote(spans)
            | Block::Heading { spans, .. } => Some(spans.as_slice()),
            Block::Rule | Block::Code(_) => None,
        });
        let styles: Vec<_> = spans.flatten().map(|span| span.style).collect();
        assert!(styles.iter().any(|style| style.is_bold()));
        assert!(styles.iter().any(|style| style.is_italic()));
        assert!(styles.iter().any(|style| style.is_strikethrough()));
        assert!(styles.iter().any(|style| style.is_code()));
        assert!(styles.iter().any(|style| style.is_link()));
    }
}
