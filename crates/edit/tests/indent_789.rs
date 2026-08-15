// Discriminating tests for issue #789
// (tab/shift-tab with a selection occasionally (un)indents the line above or below it).

use edit::buffer::TextBuffer;
use edit::helpers::Point;

fn contents(buf: &mut TextBuffer) -> String {
    let mut s = String::new();
    buf.save_as_string(&mut s);
    s
}

fn buffer_with(text: &[u8]) -> TextBuffer {
    let mut buf = TextBuffer::new(false).unwrap();
    buf.set_crlf(false);
    buf.set_insert_final_newline(false);
    buf.set_indent_with_tabs(true);
    buf.write_raw(text);
    buf.cursor_move_to_logical(Point { x: 0, y: 0 });
    buf
}

/// Select line 1 only, by holding Shift and pressing Down once from its start.
/// That produces a selection from (0,1) to (0,2): every character of line 1 and
/// zero characters of line 2. Only line 1 should be indented.
#[test]
fn indent_selection_ending_at_column_zero_of_next_line() {
    let mut buf = buffer_with(b"aaa\nbbb\nccc\n");
    buf.cursor_move_to_logical(Point { x: 0, y: 1 });
    buf.selection_update_logical(Point { x: 0, y: 2 });

    buf.indent_change(1);

    assert_eq!(contents(&mut buf), "aaa\n\tbbb\nccc\n", "only the selected line should indent");
}

/// The same selection built backwards (anchor at the start of line 2, cursor moved
/// up to the start of line 1), which is the shape in the second screenshot on #789.
#[test]
fn indent_backwards_selection_ending_at_column_zero() {
    let mut buf = buffer_with(b"aaa\nbbb\nccc\n");
    buf.cursor_move_to_logical(Point { x: 0, y: 2 });
    buf.selection_update_logical(Point { x: 0, y: 1 });

    buf.indent_change(1);

    assert_eq!(contents(&mut buf), "aaa\n\tbbb\nccc\n", "only the selected line should indent");
}

/// Triple-click (`select_line`) selects (0,y)..(0,y+1), the same shape.
#[test]
fn indent_after_select_line() {
    let mut buf = buffer_with(b"aaa\nbbb\nccc\n");
    buf.cursor_move_to_logical(Point { x: 0, y: 1 });
    buf.select_line();

    buf.indent_change(1);

    assert_eq!(contents(&mut buf), "aaa\n\tbbb\nccc\n", "only the clicked line should indent");
}

/// Unindent has the same boundary: the trailing line contributes no selected
/// characters, so its indentation must be left alone.
#[test]
fn unindent_selection_ending_at_column_zero_of_next_line() {
    let mut buf = buffer_with(b"\taaa\n\tbbb\n\tccc\n");
    buf.cursor_move_to_logical(Point { x: 0, y: 1 });
    buf.selection_update_logical(Point { x: 0, y: 2 });

    buf.indent_change(-1);

    assert_eq!(contents(&mut buf), "\taaa\nbbb\n\tccc\n", "only the selected line should unindent");
}

/// Two full lines selected the same way: (0,0)..(0,2) covers lines 0 and 1 only.
#[test]
fn indent_multiline_selection_ending_at_column_zero() {
    let mut buf = buffer_with(b"aaa\nbbb\nccc\n");
    buf.cursor_move_to_logical(Point { x: 0, y: 0 });
    buf.selection_update_logical(Point { x: 0, y: 2 });

    buf.indent_change(1);

    assert_eq!(contents(&mut buf), "\taaa\n\tbbb\nccc\n", "line 2 is not part of the selection");
}

/// CONTROL: a selection that genuinely covers characters on the last line must
/// still indent that line. This is what keeps the fix from over-correcting.
#[test]
fn indent_selection_covering_part_of_last_line() {
    let mut buf = buffer_with(b"aaa\nbbb\nccc\n");
    buf.cursor_move_to_logical(Point { x: 0, y: 1 });
    buf.selection_update_logical(Point { x: 1, y: 2 });

    buf.indent_change(1);

    assert_eq!(contents(&mut buf), "aaa\n\tbbb\n\tccc\n", "both lines carry selected text");
}

/// CONTROL: with no selection at all, Tab inserts a tab at the cursor and Shift+Tab
/// unindents the cursor's line only.
#[test]
fn indent_without_selection_touches_one_line() {
    let mut buf = buffer_with(b"aaa\n\tbbb\nccc\n");
    buf.cursor_move_to_logical(Point { x: 1, y: 1 });

    buf.indent_change(-1);

    assert_eq!(contents(&mut buf), "aaa\nbbb\nccc\n", "only the cursor's line should unindent");
}
