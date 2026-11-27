// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::icu;
use edit::input::Input;

use super::*;

fn outer(layout: &str, classname: &str) -> Rect {
    let node = layout.split_once(&format!("classname:    {classname}\r\n")).unwrap().1;
    let values = node.split_once("outer:        {").unwrap().1.split_once('}').unwrap().0;
    let values: Vec<_> = values.split(", ").map(|value| value.parse().unwrap()).collect();
    let [left, top, right, bottom]: [CoordType; 4] = values.try_into().unwrap();
    Rect { left, top, right, bottom }
}

fn editor(lines: Option<usize>) -> (sys::Deinit, Tui, State) {
    let sys = sys::init().unwrap();
    arena::init(32 * MEBI).unwrap();
    icu::init().unwrap();
    let mut state = State::new().unwrap();
    if let Some(lines) = lines {
        let doc = state.documents.add_untitled().unwrap();
        let mut buffer = doc.buffer.borrow_mut();
        buffer.set_crlf(false);
        buffer.write_raw(&b"line\n".repeat(lines));
    }
    (sys, Tui::new().unwrap(), state)
}

fn frame(tui: &mut Tui, state: &mut State, input: Option<Input<'_>>) {
    crate::draw(tui, input, state);
    for _ in 0..10 {
        if !tui.needs_settling() {
            break;
        }
        crate::draw(tui, None, state);
    }
    assert!(!tui.needs_settling(), "Editor layout did not settle");
    assert_eq!(state.error_log_count, 0);
}

fn resize(tui: &mut Tui, state: &mut State, width: CoordType, height: CoordType) {
    frame(tui, state, Some(Input::Resize(Size { width, height })));
}

#[test]
fn grid_editor_geometry_tracks_search_documents_and_resize() {
    use StateSearchKind::{Disabled, Hidden, Replace, Search};

    for lines in [None, Some(0), Some(1000)] {
        let (_sys, mut tui, mut state) = editor(lines);
        for width in [1, 2, 80] {
            for search in [Hidden, Disabled, Search, Replace] {
                for height in [24, 8, 7, 6, 5, 4, 3, 2, 1, 80, 32767, 24] {
                    state.wants_search.kind = search;
                    resize(&mut tui, &mut state, width, height);
                    let scratch = arena::scratch_arena(None);
                    let layout = tui.debug_layout(&scratch);
                    let name = if lines.is_some() { "textarea" } else { "empty" };
                    let editor = outer(&layout, name);
                    let search_height = match state.wants_search.kind {
                        Search => 2,
                        Replace => 3,
                        _ => 0,
                    };
                    let expected = (height - 2 - search_height).max(0);
                    assert_eq!(editor.height(), expected, "{width}x{height}/{search_height}");
                    assert!(editor.width() >= 0);
                    assert_eq!(outer(&layout, "statusbar").bottom, height);
                }
            }
        }
    }
}

#[test]
fn grid_editor_preserves_focus_through_resize_search_and_menus() {
    for lines in [0, 1000] {
        let (_sys, mut tui, mut state) = editor(Some(lines));
        for height in [24, 1, 24, 3, 24] {
            resize(&mut tui, &mut state, 80, height);
        }
        if lines > 0 {
            frame(&mut tui, &mut state, Some(Input::Keyboard(kbmod::CTRL | vk::HOME)));
            frame(&mut tui, &mut state, Some(Input::Keyboard(vk::NEXT)));
            let doc = state.documents.active().unwrap();
            assert_eq!(doc.buffer.borrow().cursor_logical_pos().y, 21);
            frame(&mut tui, &mut state, Some(Input::Keyboard(kbmod::CTRL | vk::END)));
        }
        let mut expected = "line\n".repeat(lines);
        for phase in ["resize", "search", "menu"] {
            if phase == "search" {
                state.wants_search.kind = StateSearchKind::Search;
                state.wants_search.focus = true;
                frame(&mut tui, &mut state, None);
                frame(&mut tui, &mut state, Some(Input::Text("needle")));
                assert_eq!(state.search_needle, "needle");
                frame(&mut tui, &mut state, Some(Input::Keyboard(vk::ESCAPE)));
                assert!(state.wants_search.kind == StateSearchKind::Hidden);
            }
            if phase == "menu" {
                frame(&mut tui, &mut state, Some(Input::Keyboard(vk::F10)));
                {
                    let scratch = arena::scratch_arena(None);
                    let layout = tui.debug_layout(&scratch);
                    assert!(layout.contains("classname:    flyout\r\n"));
                    assert_eq!(outer(&layout, "textarea").height(), 22);
                    assert_eq!(outer(&layout, "statusbar").bottom, 24);
                }
                frame(&mut tui, &mut state, Some(Input::Keyboard(vk::ESCAPE)));
            }
            frame(&mut tui, &mut state, Some(Input::Text(phase)));
            expected.push_str(phase);
            let mut text = String::new();
            state.documents.active().unwrap().buffer.borrow_mut().save_as_string(&mut text);
            let ending = if cfg!(windows) { "" } else { "\n" };
            assert_eq!(text, format!("{expected}{ending}"));
            let scratch = arena::scratch_arena(None);
            assert!(tui.render(&scratch).contains(phase), "Typed text must be visible");
        }
    }
}
