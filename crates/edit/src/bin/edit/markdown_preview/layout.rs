// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::helpers::{CoordType, Point};
use edit::unicode::MeasurementConfig;

use super::parser::Block;
use super::{InlineSpan, InlineStyle, RenderedLine, RenderedLineKind, push_span};

pub(super) fn render_blocks(blocks: &[Block], width: CoordType) -> Vec<RenderedLine> {
    let mut lines = Vec::new();

    for (index, block) in blocks.iter().enumerate() {
        if index != 0 {
            lines.push(blank_line());
        }

        match block {
            Block::Heading { level, spans } => {
                wrap_spans(spans, RenderedLineKind::Heading(*level), width, &mut lines);
            }
            Block::Paragraph(spans) => {
                wrap_spans(spans, RenderedLineKind::Text, width, &mut lines);
            }
            Block::ListItem { marker, spans } => {
                let mut prefixed =
                    vec![InlineSpan { text: format!("{marker} "), style: InlineStyle::BOLD }];
                prefixed.extend_from_slice(spans);
                wrap_spans(&prefixed, RenderedLineKind::Text, width, &mut lines);
            }
            Block::Quote(spans) => {
                let mut prefixed =
                    vec![InlineSpan { text: "│ ".to_string(), style: InlineStyle::BOLD }];
                prefixed.extend_from_slice(spans);
                wrap_spans(&prefixed, RenderedLineKind::Quote, width, &mut lines);
            }
            Block::Rule => {
                lines.push(RenderedLine {
                    kind: RenderedLineKind::Rule,
                    spans: vec![InlineSpan {
                        text: "─".repeat(width as usize),
                        style: InlineStyle::BOLD,
                    }],
                });
            }
            Block::Code(code) => {
                if code.is_empty() {
                    lines.push(RenderedLine {
                        kind: RenderedLineKind::Code,
                        spans: vec![InlineSpan {
                            text: "  ".to_string(),
                            style: InlineStyle::CODE,
                        }],
                    });
                } else {
                    for code_line in code {
                        let spans = [InlineSpan {
                            text: format!("  {code_line}"),
                            style: InlineStyle::CODE,
                        }];
                        wrap_spans(&spans, RenderedLineKind::Code, width, &mut lines);
                    }
                }
            }
        }
    }

    if lines.is_empty() {
        lines.push(blank_line());
    }
    lines
}

pub(super) fn render_message(message: &str, width: CoordType) -> Vec<RenderedLine> {
    let mut lines = Vec::new();
    let spans = [InlineSpan { text: message.to_string(), style: InlineStyle::BOLD }];
    wrap_spans(&spans, RenderedLineKind::Unavailable, width, &mut lines);
    lines
}

fn blank_line() -> RenderedLine {
    RenderedLine {
        kind: RenderedLineKind::Text,
        spans: vec![InlineSpan { text: " ".to_string(), style: InlineStyle::default() }],
    }
}

fn wrap_spans(
    spans: &[InlineSpan],
    kind: RenderedLineKind,
    width: CoordType,
    output: &mut Vec<RenderedLine>,
) {
    let text: String = spans.iter().map(|span| span.text.as_str()).collect();
    if text.is_empty() {
        output.push(RenderedLine {
            kind,
            spans: vec![InlineSpan { text: " ".to_string(), style: InlineStyle::default() }],
        });
        return;
    }

    let bytes = text.as_bytes();
    let mut measurement = MeasurementConfig::new(&bytes).with_word_wrap_column(width);
    let mut start = 0;
    let mut row = 0;

    while start < text.len() {
        let mut end = measurement.goto_visual(Point { x: CoordType::MAX, y: row }).offset;
        if end <= start {
            end = text[start..]
                .char_indices()
                .nth(1)
                .map_or(text.len(), |(advance, _)| start + advance);
        }

        output.push(RenderedLine { kind, spans: slice_spans(spans, start, end) });
        start = end;
        row += 1;
    }
}

fn slice_spans(spans: &[InlineSpan], start: usize, end: usize) -> Vec<InlineSpan> {
    let mut result = Vec::new();
    let mut span_start = 0;

    for span in spans {
        let span_end = span_start + span.text.len();
        let overlap_start = start.max(span_start);
        let overlap_end = end.min(span_end);

        if overlap_start < overlap_end {
            push_span(
                &mut result,
                &span.text[overlap_start - span_start..overlap_end - span_start],
                span.style,
            );
        }

        span_start = span_end;
        if span_start >= end {
            break;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::super::parser::parse_inline;
    use super::*;

    #[test]
    fn unicode_wrapping_preserves_text_and_style_boundaries() {
        let spans = parse_inline("你好 **世界🙂 café** τέλος");
        let mut lines = Vec::new();
        wrap_spans(&spans, RenderedLineKind::Text, 7, &mut lines);

        let rendered: String = lines
            .iter()
            .flat_map(|line| line.spans.iter())
            .map(|span| span.text.as_str())
            .collect();
        assert_eq!(rendered, "你好 世界🙂 café τέλος");

        let bold: String = lines
            .iter()
            .flat_map(|line| line.spans.iter())
            .filter(|span| span.style.is_bold())
            .map(|span| span.text.as_str())
            .collect();
        assert_eq!(bold, "世界🙂 café");
        assert!(lines.iter().flat_map(|line| &line.spans).all(|span| !span.text.is_empty()));
    }
}
