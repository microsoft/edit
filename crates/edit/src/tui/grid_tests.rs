// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use GridTrack::{Auto, Fixed, Fraction};

use super::*;

fn node<'a>(tui: &'a Tui, name: &str) -> &'a NodeCell<'static> {
    iter::successors(Some(tui.prev_tree.root_first), |node| node.borrow().next)
        .find(|node| node.borrow().classname == name)
        .unwrap()
}

fn rect(tui: &Tui, name: &str) -> Rect {
    node(tui, name).borrow().outer
}

fn pane(ctx: &mut Context<'_, '_>, name: &'static str, width: CoordType, height: CoordType) {
    ctx.block_begin(name);
    ctx.attr_intrinsic_size(Size { width, height });
    ctx.block_end();
}

fn grid(
    tui: &mut Tui,
    columns: &[GridTrack],
    rows: &[GridTrack],
    draw: impl FnOnce(&mut Context<'_, '_>),
) {
    let mut ctx = tui.create_context(None);
    ctx.attr_display(Display::Grid);
    ctx.attr_grid_template_columns(columns);
    ctx.attr_grid_template_rows(rows);
    draw(&mut ctx);
}

#[test]
fn grid_places_flat_children_on_both_axes() {
    let mut tui = Tui::new().unwrap();
    let columns = [Fraction(1), Fraction(2)];
    let rows = [Fixed(1), Fraction(1), Fraction(1), Fixed(1)];
    for height in [9, 10, 2, 1, 0, 3, 9] {
        tui.set_size(Size { width: 11, height });
        grid(&mut tui, &columns, &rows, |ctx| {
            for name in ["h1", "h2", "a", "b", "c", "d", "f1", "f2"] {
                pane(ctx, name, 100, 100);
            }
        });
        let remaining = (height - 2).max(0);
        assert_eq!(rect(&tui, "a").height(), remaining / 2);
        assert_eq!(rect(&tui, "c").height(), remaining - remaining / 2);
        assert_eq!(rect(&tui, "a").bottom, rect(&tui, "c").top);
        if height > 2 {
            assert_eq!(rect(&tui, "a").width(), 3);
            assert_eq!(rect(&tui, "b").left, 3);
            assert_eq!(rect(&tui, "b").right, 11);
        }
        for name in ["h1", "a", "b", "c", "d", "f1"] {
            let node = node(&tui, name).borrow();
            assert!(node.outer.top <= node.outer.bottom && node.outer.bottom <= height);
            assert!(node.inner.top <= node.inner.bottom);
        }
        assert_eq!(rect(&tui, "f1").bottom, height);
    }
}

#[test]
fn grid_preserves_intrinsic_sizing_padding_and_excludes_floats() {
    let mut tui = Tui::new().unwrap();
    tui.set_size(Size { width: 12, height: 10 });
    grid(&mut tui, &[Fraction(1)], &[Fraction(1)], |ctx| {
        ctx.block_begin("nested");
        ctx.attr_display(Display::Grid);
        ctx.attr_grid_template_columns(&[Auto, Fraction(1)]);
        ctx.attr_border();
        ctx.attr_padding(Rect::one(1));
        pane(ctx, "a", 3, 1);
        pane(ctx, "b", 2, 1);
        pane(ctx, "float", 99, 99);
        ctx.attr_float(FloatSpec::default());
        pane(ctx, "c", 1, 2);
        pane(ctx, "d", 1, 1);
        ctx.block_end();
    });
    assert_eq!(node(&tui, "nested").borrow().intrinsic_size, Size { width: 5, height: 3 });
    assert_eq!(rect(&tui, "a"), Rect { left: 2, top: 2, right: 5, bottom: 4 });
    assert_eq!(rect(&tui, "b"), Rect { left: 5, top: 2, right: 10, bottom: 4 });
    assert_eq!(rect(&tui, "d"), Rect { left: 5, top: 4, right: 10, bottom: 8 });
    assert_eq!(tui.prev_tree.iterate_roots().count(), 2);
    assert_eq!(node(&tui, "nested").borrow().child_count, 4);
    assert!(!mem::needs_drop::<Node<'_>>());
}

#[test]
fn grid_allocator_rounds_and_handles_zero_and_extreme_tracks() {
    let arena = Arena::new(MEBI).unwrap();
    let mut tracks =
        GridContent::tracks(&arena, &[Fixed(2), Auto, Fraction(1), Fraction(3), Fraction(0)]);
    tracks[1].intrinsic = 3;
    tracks[2].intrinsic = 999;
    assert_eq!(GridContent::preferred_size(&tracks), 4001);
    for available in 0..40 {
        GridContent::allocate(&mut tracks, available);
        let remaining = (available - 5).max(0);
        let sizes: Vec<_> = tracks.iter().map(|t| t.end - t.start).collect();
        assert_eq!(sizes, [2, 3, remaining / 4, remaining - remaining / 4, 0]);
    }
    let extremes = [Fixed(CoordType::MIN), Fraction(u16::MAX), Fraction(u16::MAX)];
    let mut tracks = GridContent::tracks(&arena, &extremes);
    GridContent::allocate(&mut tracks, CoordType::MAX);
    assert_eq!(tracks[2].end, CoordType::MAX);
    tracks[1].intrinsic = CoordType::MAX;
    assert_eq!(GridContent::preferred_size(&tracks), CoordType::MAX);
}

#[test]
fn grid_bounds_aligned_items_to_narrow_and_zero_columns() {
    let mut tui = Tui::new().unwrap();
    tui.set_size(Size { width: 12, height: 1 });
    for track in [Fixed(4), Fixed(0), Fraction(0)] {
        for width in [2, 8] {
            let column_width = if matches!(track, Fixed(4)) { 4 } else { 0 };
            let spare = (column_width - width).max(0);
            for (position, left) in
                [(Position::Left, 0), (Position::Center, spare / 2), (Position::Right, spare)]
            {
                grid(&mut tui, &[track, Fraction(1)], &[Fraction(1)], |ctx| {
                    pane(ctx, "aligned", width, 1);
                    ctx.attr_position(position);
                    pane(ctx, "next", 1, 1);
                });
                let expected =
                    Rect { left, top: 0, right: (left + width).min(column_width), bottom: 1 };
                assert_eq!(rect(&tui, "aligned"), expected);
                assert_eq!(node(&tui, "aligned").borrow().outer_clipped, expected);
                assert_eq!(rect(&tui, "next").left, column_width);
            }
        }
    }
}

#[test]
fn grid_default_tracks_overrides_and_collapsed_descendants() {
    let mut tui = Tui::new().unwrap();
    for size in [Size { width: 1, height: 1 }, Size { width: 12, height: 10 }] {
        tui.set_size(size);
        grid(&mut tui, &[], &[], |ctx| {
            ctx.block_begin("override");
            ctx.attr_display(Display::Grid);
            ctx.attr_intrinsic_size(Size { width: 7, height: 2 });
            ctx.block_begin("border");
            ctx.attr_border();
            ctx.attr_padding(Rect::one(1));
            pane(ctx, "child", 100, 100);
            ctx.block_end();
            ctx.block_end();
        });
        assert_eq!(node(&tui, "override").borrow().intrinsic_size, Size { width: 7, height: 2 });
        assert_eq!(rect(&tui, "override"), size.as_rect());
        if size.width == 1 {
            for name in ["border", "child"] {
                assert_eq!(node(&tui, name).borrow().inner, Rect::one(1));
            }
        }
    }
}

fn menu(tui: &mut Tui, input: Option<Input<'_>>) -> bool {
    let mut ctx = tui.create_context(input);
    ctx.attr_display(Display::Grid);
    ctx.attr_grid_template_rows(&[Fixed(1), Fraction(1)]);
    let mut clicked = false;
    ctx.menubar_begin();
    if ctx.menubar_menu_begin("File", 'f') {
        clicked |= ctx.menubar_menu_button("Open", 'o', kbmod::CTRL | vk::O);
        clicked |= ctx.menubar_menu_button("Longer entry", 'l', kbmod::CTRL | vk::L);
        ctx.menubar_menu_end();
    }
    ctx.menubar_end();
    pane(&mut ctx, "body", 1, 1);
    clicked
}

fn mouse(state: InputMouseState, position: Point) -> Input<'static> {
    Input::Mouse(input::InputMouse {
        state,
        position,
        modifiers: kbmod::NONE,
        scroll: Point::default(),
        drag: false,
    })
}

#[test]
fn grid_ancestor_preserves_legacy_menu_highlight_and_row_hitbox() {
    let mut tui = Tui::new().unwrap();
    for height in [24, 3, 24] {
        tui.set_size(Size { width: 80, height });
        menu(&mut tui, None);
    }
    for state in [InputMouseState::Left, InputMouseState::None] {
        menu(&mut tui, Some(mouse(state, Point { x: 2, y: 0 })));
    }
    let mut previous = None;
    let mut target = Point::default();
    for index in [NodeChildren::FIRST, NodeChildren::LAST] {
        menu(&mut tui, Some(Input::Keyboard(vk::DOWN)));
        menu(&mut tui, None);
        let flyout = node(&tui, "flyout").borrow();
        let row = flyout.children.get(index).unwrap().borrow();
        assert_eq!(tui.focused_node_path.last(), Some(&row.id));
        assert_eq!(row.attributes.bg, tui.indexed(IndexedColor::Green));
        assert_eq!(row.outer.width(), flyout.inner.width());
        assert_ne!(Some(row.id), previous);
        previous = Some(row.id);
        target = Point { x: row.outer.right - 1, y: row.outer.top };
    }
    assert!(!menu(&mut tui, Some(mouse(InputMouseState::Left, target))));
    assert!(menu(&mut tui, Some(mouse(InputMouseState::None, target))));
}
