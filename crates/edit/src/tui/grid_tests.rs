// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use GridTrack::{Auto, Fixed, Fraction, Intrinsic};

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

fn subgrid_begin(ctx: &mut Context<'_, '_>, name: &'static str) {
    ctx.block_begin(name);
    ctx.attr_display(Display::Grid);
    ctx.attr_grid_column_subgrid();
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
    for gap in [0, 1, 4] {
        assert_eq!(GridContent::preferred_size(&tracks, gap), 4001 + 4 * gap);
        for available in 0..40 {
            GridContent::allocate(&mut tracks, available, gap);
            let remaining = (available - 5 - 4 * gap).max(0);
            let sizes: Vec<_> = tracks.iter().map(|t| t.end - t.start).collect();
            assert_eq!(sizes, [2, 3, remaining / 4, remaining - remaining / 4, 0]);
            assert!(tracks.windows(2).all(|pair| pair[1].start - pair[0].end == gap));
        }
    }
    let extremes = [Fixed(CoordType::MIN), Fraction(u16::MAX), Fraction(u16::MAX)];
    let mut tracks = GridContent::tracks(&arena, &extremes);
    GridContent::allocate(&mut tracks, CoordType::MAX, 0);
    assert_eq!(tracks[2].end, CoordType::MAX);
    tracks[1].intrinsic = CoordType::MAX;
    assert_eq!(GridContent::preferred_size(&tracks, 0), CoordType::MAX);
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

#[test]
fn grid_shared_cells_preserve_minimum_tracks_and_bounded_placement() {
    let mut tui = Tui::new().unwrap();
    for (columns, natural_width) in [
        (&[Intrinsic(3), Intrinsic(20)][..], 26),
        (&[Intrinsic(1), Intrinsic(-1)][..], 10),
        (&[][..], 10),
    ] {
        for (width, height) in [(30, 20), (12, 6), (5, 9), (2, 2), (1, 1), (0, 0), (30, 20)] {
            tui.set_size(Size { width, height });
            {
                let mut ctx = tui.create_context(None);
                ctx.block_begin("grid");
                ctx.attr_display(Display::Grid);
                ctx.attr_grid_template_columns(columns);
                ctx.attr_grid_auto_columns(Intrinsic(0));
                ctx.attr_grid_auto_rows(Intrinsic(0));
                ctx.attr_grid_gap(Size { width: 1, height: 1 });
                subgrid_begin(&mut ctx, "first");
                ctx.attr_grid_align_items(GridAlignment::Start);
                ctx.attr_grid_justify_items(GridAlignment::Stretch);
                ctx.block_begin("a");
                ctx.attr_display(Display::Grid);
                ctx.attr_intrinsic_size(Size { width: 1, height: 1 });
                ctx.attr_border();
                pane(&mut ctx, "child", 8, 8);
                ctx.block_end();
                pane(&mut ctx, "b", 1, 1);
                ctx.attr_position(Position::Right);
                pane(&mut ctx, "float", 99, 99);
                ctx.attr_float(FloatSpec::default());
                ctx.block_end();
                subgrid_begin(&mut ctx, "empty");
                ctx.block_end();
                subgrid_begin(&mut ctx, "padded");
                ctx.attr_grid_align_items(GridAlignment::Start);
                ctx.attr_border();
                ctx.attr_padding(Rect::one(1));
                pane(&mut ctx, "short", 3, 1);
                pane(&mut ctx, "tall", 2, 3);
                ctx.block_end();
                subgrid_begin(&mut ctx, "last");
                pane(&mut ctx, "c", 5, 1);
                ctx.block_end();
                ctx.block_end();
            }
            let grid = node(&tui, "grid").borrow();
            assert_eq!(grid.intrinsic_size, Size { width: natural_width, height: 14 });
            assert_eq!(tui.prev_tree.iterate_roots().count(), 2);
            for row in Tree::iterate_siblings(grid.children.first) {
                assert_eq!(row.borrow().outer.width(), grid.inner.width());
            }
            assert_eq!(rect(&tui, "a").right, width.min(5));
            assert_eq!(rect(&tui, "b").left, width.min(6));
            assert_eq!(rect(&tui, "b").right, width.min(natural_width));
            assert_eq!(rect(&tui, "b").height(), height.min(1));
            assert_eq!(rect(&tui, "c").top, height.min(13));
            for name in ["a", "b", "c", "child", "short", "tall"] {
                let n = node(&tui, name).borrow();
                assert!(n.outer.left <= n.outer.right && n.outer.top <= n.outer.bottom);
                assert!(n.inner.left <= n.inner.right && n.inner.top <= n.inner.bottom);
                assert!(n.outer.right <= width && n.outer.bottom <= height);
            }
            let a = node(&tui, "a").borrow();
            assert_eq!(node(&tui, "child").borrow().outer_clipped, a.inner_clipped);
            let row = node(&tui, "padded").borrow();
            assert_eq!(row.intrinsic_size, Size { width: natural_width - 4, height: 3 });
            for name in ["short", "tall"] {
                let cell = node(&tui, name).borrow();
                assert!(cell.outer.left >= row.inner.left && cell.outer.right <= row.inner.right);
                assert!(cell.outer.top >= row.inner.top && cell.outer.bottom <= row.inner.bottom);
                assert_eq!(cell.outer_clipped, cell.outer.intersect(row.inner_clipped));
            }
            if width == 30 {
                assert_eq!(rect(&tui, "short"), Rect { left: 2, top: 7, right: 5, bottom: 8 });
                let expected = Rect { left: 6, top: 7, right: natural_width - 2, bottom: 10 };
                assert_eq!(rect(&tui, "tall"), expected);
                assert_eq!(rect(&tui, "tall").left, rect(&tui, "b").left);
            }
        }
    }
}

#[test]
fn grid_intrinsic_tracks_gaps_and_alignment_work_without_shared_rows() {
    let mut tui = Tui::new().unwrap();
    tui.set_size(Size { width: 30, height: 12 });
    grid(&mut tui, &[Intrinsic(3), Intrinsic(0)], &[], |ctx| {
        ctx.attr_grid_auto_rows(Intrinsic(0));
        ctx.attr_grid_gap(Size { width: 2, height: 1 });
        ctx.attr_grid_align_items(GridAlignment::Start);
        pane(ctx, "short", 1, 1);
        pane(ctx, "tall", 4, 3);
        pane(ctx, "next", 2, 1);
    });
    assert_eq!(rect(&tui, "short"), Rect { left: 0, top: 0, right: 3, bottom: 1 });
    assert_eq!(rect(&tui, "tall"), Rect { left: 5, top: 0, right: 9, bottom: 3 });
    assert_eq!(rect(&tui, "next"), Rect { left: 0, top: 4, right: 3, bottom: 5 });

    grid(&mut tui, &[Fixed(3), Fraction(1)], &[], |ctx| {
        ctx.attr_grid_auto_rows(Intrinsic(0));
        pane(ctx, "flat", 1, 1);
        subgrid_begin(ctx, "row");
        pane(ctx, "a", 1, 2);
        pane(ctx, "b", 1, 1);
        ctx.block_end();
        pane(ctx, "after", 1, 1);
    });
    assert_eq!(rect(&tui, "flat").top, 0);
    assert_eq!(rect(&tui, "a"), Rect { left: 0, top: 1, right: 3, bottom: 3 });
    assert_eq!(rect(&tui, "b"), Rect { left: 3, top: 1, right: 30, bottom: 3 });
    assert_eq!(rect(&tui, "after").top, 3);
}

fn focus_grid(tui: &mut Tui, display: Display, input: Option<Input<'_>>, consume: bool) {
    let mut ctx = tui.create_context(input);
    ctx.block_begin("container");
    ctx.attr_display(display);
    if matches!(display, Display::Grid) {
        ctx.attr_grid_auto_columns(Intrinsic(0));
        ctx.attr_grid_auto_rows(Intrinsic(0));
    }
    ctx.attr_focus_navigation(FocusNavigation::Vertical);
    ctx.attr_focus_well();
    ctx.focus_on_first_present();
    for (index, names) in [["a", "b"], ["c", "d"]].into_iter().enumerate() {
        ctx.next_block_id_mixin(index as u64);
        ctx.block_begin("row");
        ctx.attr_display(display);
        if matches!(display, Display::Grid) {
            ctx.attr_grid_column_subgrid();
        }
        ctx.attr_focus_navigation(FocusNavigation::Horizontal);
        ctx.inherit_focus();
        for name in names {
            ctx.block_begin(name);
            ctx.attr_intrinsic_size(Size { width: 2, height: 1 });
            ctx.inherit_focus();
            if consume && ctx.is_focused() && ctx.keyboard_input().is_some() {
                ctx.set_input_consumed();
            }
            ctx.block_end();
        }
        ctx.block_end();
    }
    ctx.block_end();
}

#[test]
fn grid_navigation_preserves_horizontal_vertical_tab_and_input_priority() {
    for display in [Display::Grid, Display::Block] {
        let mut tui = Tui::new().unwrap();
        tui.set_size(Size { width: 20, height: 10 });
        focus_grid(&mut tui, display, None, false);
        for (key, name, consume) in [
            (vk::RIGHT, "a", true),
            (vk::RIGHT, "b", false),
            (vk::RIGHT, "a", false),
            (vk::LEFT, "b", false),
            (vk::DOWN, "c", false),
            (vk::UP, "a", false),
            (vk::TAB, "b", false),
            (SHIFT_TAB, "a", false),
        ] {
            focus_grid(&mut tui, display, Some(Input::Keyboard(key)), consume);
            focus_grid(&mut tui, display, None, false);
            assert_eq!(tui.focused_node_path.last(), Some(&node(&tui, name).borrow().id));
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
fn grid_ancestor_preserves_menu_highlight_and_row_hitbox() {
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
