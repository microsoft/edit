// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::buffer::TextBuffer;
use edit::helpers::*;
use edit::oklab::StraightRgba;
use edit::tui::*;

use crate::documents::Document;
use crate::localization::*;
use crate::state::*;

const ACTIVE_BACKGROUND: StraightRgba = StraightRgba::from_rgba(0xadd8e6ff);
const ACTIVE_FOREGROUND: StraightRgba = StraightRgba::from_rgba(0x000000ff);

#[derive(Debug, PartialEq, Eq)]
enum NavigationItemKind {
    Heading(String),
    Content,
}

#[derive(Debug, PartialEq, Eq)]
struct NavigationItem {
    level: u8,
    line: CoordType,
    column: CoordType,
    kind: NavigationItemKind,
}

impl NavigationItem {
    fn is_heading(&self) -> bool {
        matches!(self.kind, NavigationItemKind::Heading(_))
    }
}

pub fn document_is_markdown(doc: &Document) -> bool {
    // EN: The navigation command is enabled exclusively for files ending in .md.
    // 中文：導覽功能僅對副檔名為 .md 的文件啟用。
    doc.path
        .as_deref()
        .and_then(|path| path.extension())
        .and_then(|extension| extension.to_str())
        .is_some_and(|extension| extension.eq_ignore_ascii_case("md"))
}

pub fn draw_dialog_navigation(ctx: &mut Context, state: &mut State) {
    // EN: Render a six-level clickable outline with folding and the ESC hint at the bottom.
    // 中文：繪製可點選、可折疊的六層大綱，並將 ESC 提示固定放在視窗下緣。
    let Some(doc) = state.documents.active() else {
        state.wants_navigation = false;
        return;
    };
    if !document_is_markdown(doc) {
        state.wants_navigation = false;
        return;
    }

    let (items, cursor_line) = {
        let tb = doc.buffer.borrow();
        let text = buffer_text(&tb);
        (parse_markdown(&String::from_utf8_lossy(&text)), tb.cursor_logical_pos().y)
    };
    let active = active_display_item(&items, cursor_line, &state.navigation_collapsed);

    let size = ctx.size();
    let width = (size.width - 6).clamp(12, 80);
    let height = (size.height - 9).max(4);
    let mut toggle = None;
    let mut jump = None;

    ctx.modal_begin_centered_title("markdown-navigation", loc(LocId::NavigationDialogTitle));
    {
        ctx.scrollarea_begin("navigation-scrollarea", Size { width, height });
        ctx.inherit_focus();
        {
            if items.is_empty() {
                ctx.label("navigation-empty", loc(LocId::NavigationNoContent));
                ctx.attr_padding(Rect::two(0, 1));
            } else {
                ctx.table_begin("navigation-tree");
                ctx.inherit_focus();
                ctx.table_set_columns(&[1, COORD_TYPE_SAFE_MAX]);
                ctx.table_set_cell_gap(Size { width: 1, height: 0 });
                ctx.attr_padding(Rect::two(0, 1));
                {
                    let mut hidden_below = None;
                    for (idx, item) in items.iter().enumerate() {
                        if let Some(level) = hidden_below {
                            if item.level > level {
                                continue;
                            }
                            hidden_below = None;
                        }

                        let has_children = item.is_heading()
                            && items.get(idx + 1).is_some_and(|next| next.level > item.level);
                        let collapsed = has_children
                            && state.navigation_collapsed.contains(&(item.line as usize));

                        ctx.table_next_row();
                        ctx.next_block_id_mixin(idx as u64 + 1);
                        if has_children {
                            if ctx.button(
                                "navigation-fold",
                                if collapsed { "+" } else { "-" },
                                ButtonStyle::default().bracketed(false),
                            ) {
                                toggle = Some(item.line as usize);
                            }
                        } else {
                            ctx.label("navigation-fold-spacer", " ");
                        }

                        let text = tree_text(&items, idx);
                        ctx.next_block_id_mixin(idx as u64 + 1);
                        match &item.kind {
                            NavigationItemKind::Heading(_) => {
                                if ctx.button(
                                    "navigation-heading",
                                    &text,
                                    ButtonStyle::default().bracketed(false),
                                ) {
                                    jump = Some(Point { x: item.column, y: item.line });
                                }
                                ctx.attr_overflow(Overflow::TruncateTail);
                                if active == Some(idx) {
                                    ctx.attr_background_rgba(ACTIVE_BACKGROUND);
                                    ctx.attr_foreground_rgba(ACTIVE_FOREGROUND);
                                }
                            }
                            NavigationItemKind::Content => {
                                if ctx.button(
                                    "navigation-content",
                                    &text,
                                    ButtonStyle::default().bracketed(false),
                                ) {
                                    jump = Some(Point { x: 0, y: item.line });
                                }
                                ctx.attr_overflow(Overflow::TruncateTail);
                            }
                        }

                        if collapsed {
                            hidden_below = Some(item.level);
                        }
                    }
                }
                ctx.table_end();
            }
        }
        ctx.scrollarea_end();

        ctx.label("navigation-escape-hint", loc(LocId::NavigationEscapeHint));
        ctx.attr_position(Position::Center);
        ctx.attr_padding(Rect::three(1, 0, 0));
    }
    let close = ctx.modal_end();

    if let Some(line) = toggle {
        if !state.navigation_collapsed.remove(&line) {
            state.navigation_collapsed.insert(line);
        }
        ctx.needs_rerender();
    }

    if let Some(pos) = jump
        && let Some(doc) = state.documents.active_mut()
    {
        // EN: A selected tree row moves the editor cursor before the modal closes.
        // 中文：點選樹狀項目後，先移動編輯器游標，再關閉導覽視窗。
        let mut tb = doc.buffer.borrow_mut();
        jump_to_navigation_target(&mut tb, pos);
        state.wants_navigation = false;
        ctx.needs_rerender();
    }

    if close {
        state.wants_navigation = false;
        ctx.needs_rerender();
    }
}

fn jump_to_navigation_target(tb: &mut TextBuffer, pos: Point) {
    tb.cursor_move_to_logical(pos);
    tb.make_cursor_visible();
}

fn buffer_text(tb: &TextBuffer) -> Vec<u8> {
    let mut text = Vec::with_capacity(tb.text_length());
    while text.len() < tb.text_length() {
        let chunk = tb.read_forward(text.len());
        if chunk.is_empty() {
            break;
        }
        text.extend_from_slice(chunk);
    }
    text
}

fn parse_markdown(text: &str) -> Vec<NavigationItem> {
    // EN: Parse ATX headings # through ###### and summarize other non-empty regions.
    // 中文：解析 # 至 ###### 的 ATX 標題，其餘非空白文字區域以概括項目表示。
    let mut items = Vec::new();
    let mut content_start = None;
    let mut last_heading_level = 0;
    let mut fence = None;

    for (line_number, raw_line) in text.lines().enumerate() {
        let line = raw_line.strip_suffix('\r').unwrap_or(raw_line);
        let marker = fence_marker(line);

        if let Some((fence_char, fence_count)) = fence {
            note_content(&mut content_start, line_number, last_heading_level, line);
            if marker.is_some_and(|(ch, count, rest)| {
                ch == fence_char && count >= fence_count && rest.trim().is_empty()
            }) {
                fence = None;
            }
            continue;
        }

        if let Some((fence_char, fence_count, _)) = marker {
            fence = Some((fence_char, fence_count));
            note_content(&mut content_start, line_number, last_heading_level, line);
            continue;
        }

        if let Some((level, column, title)) = parse_heading(line) {
            flush_content(&mut items, &mut content_start);
            items.push(NavigationItem {
                level,
                line: line_number as CoordType,
                column: column as CoordType,
                kind: NavigationItemKind::Heading(title),
            });
            last_heading_level = level;
        } else {
            note_content(&mut content_start, line_number, last_heading_level, line);
        }
    }

    flush_content(&mut items, &mut content_start);
    items
}

fn note_content(
    content_start: &mut Option<(CoordType, u8)>,
    line_number: usize,
    last_heading_level: u8,
    line: &str,
) {
    if content_start.is_none() && !line.trim().is_empty() {
        *content_start = Some((
            line_number as CoordType,
            if last_heading_level == 0 { 1 } else { last_heading_level + 1 },
        ));
    }
}

fn flush_content(items: &mut Vec<NavigationItem>, content_start: &mut Option<(CoordType, u8)>) {
    if let Some((line, level)) = content_start.take() {
        items.push(NavigationItem { level, line, column: 0, kind: NavigationItemKind::Content });
    }
}

fn fence_marker(line: &str) -> Option<(u8, usize, &str)> {
    let bytes = line.as_bytes();
    let mut offset = 0;
    while offset < bytes.len() && offset < 3 && bytes[offset] == b' ' {
        offset += 1;
    }
    let fence_char = *bytes.get(offset)?;
    if !matches!(fence_char, b'`' | b'~') {
        return None;
    }
    let mut end = offset;
    while bytes.get(end) == Some(&fence_char) {
        end += 1;
    }
    let count = end - offset;
    (count >= 3).then_some((fence_char, count, &line[end..]))
}

fn parse_heading(line: &str) -> Option<(u8, usize, String)> {
    let bytes = line.as_bytes();
    let mut offset = 0;
    while bytes.get(offset).is_some_and(|byte| matches!(*byte, b' ' | b'\t')) {
        offset += 1;
    }

    let hashes_start = offset;
    while bytes.get(offset) == Some(&b'#') {
        offset += 1;
    }
    let level = offset - hashes_start;
    if !(1..=6).contains(&level) {
        return None;
    }
    if bytes.get(offset).is_some_and(|byte| !matches!(*byte, b' ' | b'\t')) {
        return None;
    }

    while bytes.get(offset).is_some_and(|byte| matches!(*byte, b' ' | b'\t')) {
        offset += 1;
    }
    let column = offset;
    let mut title_end = bytes.len();
    while title_end > column && matches!(bytes[title_end - 1], b' ' | b'\t') {
        title_end -= 1;
    }

    let mut closing_start = title_end;
    while closing_start > column && bytes[closing_start - 1] == b'#' {
        closing_start -= 1;
    }
    if closing_start < title_end
        && closing_start > 0
        && matches!(bytes[closing_start - 1], b' ' | b'\t')
    {
        title_end = closing_start - 1;
        while title_end > column && matches!(bytes[title_end - 1], b' ' | b'\t') {
            title_end -= 1;
        }
    }

    let title =
        if title_end == column { "#".repeat(level) } else { line[column..title_end].to_string() };
    let logical_column = line[..column].chars().count();
    Some((level as u8, logical_column, title))
}

fn active_display_item(
    items: &[NavigationItem],
    cursor_line: CoordType,
    collapsed: &std::collections::BTreeSet<usize>,
) -> Option<usize> {
    let active = items
        .iter()
        .enumerate()
        .rev()
        .find(|(_, item)| item.is_heading() && item.line <= cursor_line)
        .map(|(idx, _)| idx)?;

    for (idx, item) in items.iter().enumerate().take(active) {
        if item.is_heading()
            && collapsed.contains(&(item.line as usize))
            && is_descendant(items, idx, active)
        {
            return Some(idx);
        }
    }
    Some(active)
}

fn is_descendant(items: &[NavigationItem], parent: usize, child: usize) -> bool {
    if child <= parent || items[child].level <= items[parent].level {
        return false;
    }
    !items[parent + 1..=child].iter().any(|item| item.level <= items[parent].level)
}

fn tree_text(items: &[NavigationItem], idx: usize) -> String {
    let item = &items[idx];
    let has_later_sibling = items[idx + 1..]
        .iter()
        .take_while(|next| next.level >= item.level)
        .any(|next| next.level == item.level);
    let mut text = "│  ".repeat(item.level.saturating_sub(1) as usize);
    text.push_str(if has_later_sibling { "├─ " } else { "└─ " });
    match &item.kind {
        NavigationItemKind::Heading(title) => text.push_str(title),
        NavigationItemKind::Content => text.push_str("[......]"),
    }
    text
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_six_heading_levels_and_content_regions() {
        let items = parse_markdown(
            "intro\n# One\nbody\n## Two\n### Three\n#### Four\n##### Five\n###### Six\ndeep text\n####### Seven\ntail\n# Last\n",
        );
        let headings: Vec<_> = items
            .iter()
            .filter_map(|item| match &item.kind {
                NavigationItemKind::Heading(title) => Some((item.level, title.as_str(), item.line)),
                NavigationItemKind::Content => None,
            })
            .collect();

        assert_eq!(
            headings,
            vec![
                (1, "One", 1),
                (2, "Two", 3),
                (3, "Three", 4),
                (4, "Four", 5),
                (5, "Five", 6),
                (6, "Six", 7),
                (1, "Last", 11),
            ]
        );
        assert!(items.iter().all(|item| {
            !matches!(&item.kind, NavigationItemKind::Heading(title) if title == "Seven")
        }));
    }

    #[test]
    fn ignores_hashes_without_spaces_and_inside_fences() {
        let items =
            parse_markdown("# Valid ###\n#invalid\n```markdown\n## Hidden\n```\n   ### Visible\n");

        let headings: Vec<_> = items
            .iter()
            .filter_map(|item| match &item.kind {
                NavigationItemKind::Heading(title) => Some((item.level, title.as_str())),
                NavigationItemKind::Content => None,
            })
            .collect();
        assert_eq!(headings, vec![(1, "Valid"), (3, "Visible")]);
    }

    #[test]
    fn parses_indented_hash_navigation_levels_and_rejects_asterisks() {
        let items = parse_markdown(
            "這是開頭\n\n# A\n     ## A-1\n          ### A-1-1\n          ### A-1-2\n     ## A-2\n     ## A-3\n# B\n\n這是其他文字\n\n# C\n這是測試\n* 非標題\n** 也非標題\n*** 仍非標題\n",
        );
        let headings: Vec<_> = items
            .iter()
            .filter_map(|item| match &item.kind {
                NavigationItemKind::Heading(title) => Some((item.level, title.as_str())),
                NavigationItemKind::Content => None,
            })
            .collect();
        assert_eq!(
            headings,
            vec![
                (1, "A"),
                (2, "A-1"),
                (3, "A-1-1"),
                (3, "A-1-2"),
                (2, "A-2"),
                (2, "A-3"),
                (1, "B"),
                (1, "C"),
            ]
        );
        assert_eq!(
            items.iter().filter(|item| matches!(item.kind, NavigationItemKind::Content)).count(),
            3
        );

        assert_eq!(
            items
                .iter()
                .filter_map(|item| match &item.kind {
                    NavigationItemKind::Heading(title) => Some((title.as_str(), item.column)),
                    NavigationItemKind::Content => None,
                })
                .collect::<Vec<_>>(),
            vec![
                ("A", 2),
                ("A-1", 8),
                ("A-1-1", 14),
                ("A-1-2", 14),
                ("A-2", 8),
                ("A-3", 8),
                ("B", 2),
                ("C", 2),
            ]
        );
    }

    #[test]
    fn collapsed_parent_represents_the_active_descendant() {
        let items = parse_markdown("# One\n## Two\n### Three\n#### Four\n##### Five\n###### Six\n");
        let collapsed = std::collections::BTreeSet::from([0]);
        assert_eq!(active_display_item(&items, 5, &collapsed), Some(0));
    }

    #[test]
    fn indented_heading_position_moves_the_document_cursor() {
        let text = "開頭\n     ###### 第六層\n尾端\n";
        let items = parse_markdown(text);
        let heading = items.iter().find(|item| item.is_heading()).unwrap();
        let target = Point { x: heading.column, y: heading.line };

        let mut tb = TextBuffer::new(false).unwrap();
        tb.write_raw(text.as_bytes());
        jump_to_navigation_target(&mut tb, target);

        assert_eq!(tb.cursor_logical_pos(), Point { x: 12, y: 1 });
    }
}
