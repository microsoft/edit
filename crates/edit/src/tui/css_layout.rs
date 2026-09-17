// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A frame-local Taffy tree. Arena nodes retain only style indices, never owners.

use super::{CoordType, Node, NodeCell, NodeContent, Point, Rect, Size, Tree, css};

pub(super) fn is_container(node: &Node<'_>) -> bool {
    node.css_style.is_some() && matches!(node.content, NodeContent::None)
}

struct Entry<'a> {
    node: &'a NodeCell<'a>,
    id: css::NodeId,
    parent: css::NodeId,
    container: bool,
}

struct Layout<'a> {
    tree: taffy::TaffyTree<Size>,
    entries: Vec<Entry<'a>>,
}

impl<'a> Layout<'a> {
    fn new() -> Self {
        Self { tree: taffy::TaffyTree::new(), entries: Vec::new() }
    }

    fn insert(&mut self, node: &Node<'a>, styles: &[css::Style]) -> css::NodeId {
        let mut style = node.css_style.map(|index| styles[index].clone()).unwrap_or_default();
        // Taffy owns the box model. Reserve its border for all legacy insets so
        // those insets participate in sizing exactly once, including on leaves.
        let padding = node.attributes.padding;
        let border = node.attributes.bordered as CoordType;
        style.border = css::Rect {
            left: css::length((padding.left + border) as f32),
            right: css::length(
                (padding.right
                    + border.max(matches!(node.content, NodeContent::Scrollarea(..)) as CoordType))
                    as f32,
            ),
            top: css::length((padding.top + border) as f32),
            bottom: css::length((padding.bottom + border) as f32),
        };
        if !is_container(node) {
            return self.tree.new_leaf_with_context(style, node.intrinsic_size).expect("New CSS leaf");
        }

        let id = self.tree.new_with_children(style, &[]).expect("New CSS container");
        for child in Tree::iterate_siblings(node.children.first) {
            let child_node = child.borrow();
            let child_id = self.insert(&child_node, styles);
            self.tree.add_child(id, child_id).expect("Fresh CSS parent and child");
            self.entries.push(Entry {
                node: child,
                id: child_id,
                parent: id,
                container: is_container(&child_node),
            });
        }
        id
    }

    fn compute(&mut self, root: css::NodeId, available: css::Size<css::AvailableSpace>) {
        self.tree
            .compute_layout_with_measure(root, available, |inputs, _, context, style| {
                // Legacy widgets supply current-frame intrinsic metrics, not CSS
                // text reflow. Labels are single-line; textareas own their wrapping
                // and scrolling. Only definite dimensions override these metrics.
                let intrinsic = context.copied().unwrap_or_default();
                taffy::compute_leaf_layout(
                    inputs,
                    style,
                    |_, _| 0.0,
                    |known, _| css::Size {
                        width: known.width.unwrap_or(intrinsic.width.max(0) as f32),
                        height: known.height.unwrap_or(intrinsic.height.max(0) as f32),
                    },
                )
            })
            .expect("CSS tree contains only live, frame-local node IDs");
    }
}

pub(super) fn measure(node: &Node<'_>, styles: &[css::Style]) -> Size {
    let mut layout = Layout::new();
    let root = layout.insert(node, styles);
    layout.compute(
        root,
        css::Size { width: css::AvailableSpace::MaxContent, height: css::AvailableSpace::MaxContent },
    );
    let result = layout.tree.layout(root).expect("Measured CSS root");
    let insets = node.intrinsic_to_outer();
    Size {
        width: (result.size.width as CoordType - insets.width + node.intrinsic_size.width).max(0),
        height: (result.size.height as CoordType - insets.height + node.intrinsic_size.height).max(0),
    }
}

fn apply(node: &mut Node<'_>, result: &taffy::Layout, origin: Point, clip: Rect) {
    node.outer = Rect {
        left: origin.x,
        top: origin.y,
        right: origin.x + result.size.width.max(0.0) as CoordType,
        bottom: origin.y + result.size.height.max(0.0) as CoordType,
    };
    // Legacy widgets use outer for hit-testing and viewport sizing, not only
    // painting. Preserve Taffy's overflowing origins separately during traversal.
    node.outer.left = node.outer.left.clamp(clip.left, clip.right);
    node.outer.right = node.outer.right.clamp(clip.left, clip.right);
    node.outer.top = node.outer.top.clamp(clip.top, clip.bottom);
    node.outer.bottom = node.outer.bottom.clamp(clip.top, clip.bottom);
    node.inner = Rect {
        left: node.outer.left + (result.border.left + result.padding.left) as CoordType,
        top: node.outer.top + (result.border.top + result.padding.top) as CoordType,
        right: node.outer.right - (result.border.right + result.padding.right) as CoordType,
        bottom: node.outer.bottom - (result.border.bottom + result.padding.bottom) as CoordType,
    };
    node.inner.left = node.inner.left.clamp(node.outer.left, node.outer.right);
    node.inner.right = node.inner.right.clamp(node.inner.left, node.outer.right);
    node.inner.top = node.inner.top.clamp(node.outer.top, node.outer.bottom);
    node.inner.bottom = node.inner.bottom.clamp(node.inner.top, node.outer.bottom);
    node.outer_clipped = node.outer.intersect(clip);
    node.inner_clipped = node.inner.intersect(clip);
}

pub(super) fn layout(node: &mut Node<'_>, clip: Rect, styles: &[css::Style]) {
    let mut layout = Layout::new();
    let root = layout.insert(node, styles);
    let mut style = layout.tree.style(root).expect("CSS root style").clone();
    let width = (node.outer.right - node.outer.left).max(0) as f32;
    let height = (node.outer.bottom - node.outer.top).max(0) as f32;
    // The enclosing legacy layout (or terminal viewport) has already allotted
    // this border box. Nested CSS containers are solved in this same Taffy tree.
    style.box_sizing = css::BoxSizing::BorderBox;
    style.size = css::Size { width: css::length(width), height: css::length(height) };
    style.min_size = css::Size { width: css::length(0.0), height: css::length(0.0) };
    style.max_size = css::Size { width: css::length(width), height: css::length(height) };
    layout.tree.set_style(root, style).expect("CSS root style");
    layout.compute(
        root,
        css::Size {
            width: css::AvailableSpace::Definite(width),
            height: css::AvailableSpace::Definite(height),
        },
    );

    let origin = Point { x: node.outer.left, y: node.outer.top };
    apply(node, layout.tree.layout(root).expect("CSS root layout"), origin, clip);
    // Entries are post-order; reverse traversal guarantees parents are placed
    // before children. Keep origins separate from clipped painting rectangles.
    let mut origins = std::collections::HashMap::new();
    origins.insert(root, (origin, node.inner_clipped));
    for entry in layout.entries.iter().rev() {
        let (parent_origin, parent_clip) = origins[&entry.parent];
        let result = layout.tree.layout(entry.id).expect("CSS child layout");
        let origin = Point {
            x: parent_origin.x + result.location.x as CoordType,
            y: parent_origin.y + result.location.y as CoordType,
        };
        let mut child = entry.node.borrow_mut();
        apply(&mut child, result, origin, parent_clip);
        origins.insert(entry.id, (origin, child.inner_clipped));
        if !entry.container {
            let clip = child.inner_clipped;
            child.layout_children(clip, styles);
        }
    }
}

#[cfg(test)]
mod tests {
    use stdext::arena::Arena;

    use super::*;

    fn child<'a>(
        arena: &'a Arena,
        parent: &'a NodeCell<'a>,
        style: Option<usize>,
        width: CoordType,
        height: CoordType,
    ) -> &'a NodeCell<'a> {
        let child = Tree::alloc_node(arena);
        let mut parent_node = parent.borrow_mut();
        {
            let mut node = child.borrow_mut();
            node.parent = Some(parent);
            node.css_style = style;
            node.intrinsic_size = Size { width, height };
            node.intrinsic_size_set = true;
            node.siblings.prev = parent_node.children.last;
        }
        if let Some(last) = parent_node.children.last {
            last.borrow_mut().siblings.next = Some(child);
        } else {
            parent_node.children.first = Some(child);
        }
        parent_node.children.last = Some(child);
        parent_node.child_count += 1;
        child
    }

    fn root(arena: &Arena) -> &NodeCell<'_> {
        let root = Tree::alloc_node(arena);
        root.borrow_mut().css_style = Some(0);
        root
    }

    fn place<'a>(arena: &'a Arena, root: &'a NodeCell<'a>, styles: &[css::Style], size: Size) {
        let mut node = root.borrow_mut();
        node.compute_intrinsic_size(arena, styles);
        node.outer = size.as_rect();
        node.inner = node.outer_to_inner(node.outer);
        node.outer_clipped = node.outer;
        node.inner_clipped = node.inner;
        node.layout_children(size.as_rect(), styles);
    }

    fn grid() -> css::Style {
        css::Style {
            display: css::Display::Grid,
            grid_template_columns: vec![css::flex(1.0)],
            ..Default::default()
        }
    }

    #[test]
    fn nested_grid_and_flex_fill_remaining_rows() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let header = child(&arena, root, None, 20, 1);
        let editor = child(&arena, root, Some(1), 0, 0);
        let text = child(&arena, editor, Some(2), 0, 0);
        let search = child(&arena, editor, Some(3), 0, 0);
        child(&arena, search, None, 10, 2);
        let footer = child(&arena, root, None, 20, 1);
        let styles = [
            css::Style {
                grid_template_rows: vec![css::auto(), css::flex(1.0), css::auto()],
                ..grid()
            },
            css::Style {
                flex_direction: css::FlexDirection::Column,
                min_size: css::Size { width: css::length(0.0), height: css::length(0.0) },
                ..Default::default()
            },
            css::Style {
                flex_grow: 1.0,
                flex_basis: css::length(0.0),
                min_size: css::Size { width: css::length(0.0), height: css::length(0.0) },
                ..Default::default()
            },
            css::Style { flex_shrink: 0.0, ..Default::default() },
        ];
        place(&arena, root, &styles, Size { width: 20, height: 12 });
        assert_eq!(header.borrow().outer, Rect { left: 0, top: 0, right: 20, bottom: 1 });
        assert_eq!(editor.borrow().outer, Rect { left: 0, top: 1, right: 20, bottom: 11 });
        assert_eq!(text.borrow().outer, Rect { left: 0, top: 1, right: 20, bottom: 9 });
        assert_eq!(search.borrow().outer, Rect { left: 0, top: 9, right: 20, bottom: 11 });
        assert_eq!(footer.borrow().outer, Rect { left: 0, top: 11, right: 20, bottom: 12 });
    }

    #[test]
    fn fractional_tracks_round_shared_edges_without_gaps() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let first = child(&arena, root, None, 0, 1);
        let second = child(&arena, root, None, 0, 1);
        let third = child(&arena, root, None, 0, 1);
        let styles = [css::Style {
            grid_template_columns: vec![css::flex(1.0), css::flex(1.0), css::flex(1.0)],
            ..grid()
        }];
        place(&arena, root, &styles, Size { width: 10, height: 1 });
        assert_eq!(first.borrow().outer.right, 3);
        assert_eq!(second.borrow().outer.left, 3);
        assert_eq!(second.borrow().outer.right, 7);
        assert_eq!(third.borrow().outer.left, 7);
        assert_eq!(third.borrow().outer.right, 10);
    }

    #[test]
    fn fixed_auto_and_fractional_rows_use_intrinsic_measurement() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let fixed = child(&arena, root, None, 0, 0);
        let auto = child(&arena, root, None, 0, 3);
        let flexible = child(&arena, root, None, 0, 0);
        let styles = [css::Style {
            grid_template_rows: vec![css::length(2.0), css::auto(), css::flex(1.0)],
            ..grid()
        }];
        place(&arena, root, &styles, Size { width: 10, height: 11 });
        assert_eq!(fixed.borrow().outer.bottom, 2);
        assert_eq!(auto.borrow().outer.bottom, 5);
        assert_eq!(flexible.borrow().outer.bottom, 11);
    }

    #[test]
    fn flex_grow_and_shrink_are_engine_sized() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let first = child(&arena, root, Some(1), 0, 0);
        let second = child(&arena, root, Some(2), 0, 0);
        let mut styles = [
            css::Style::default(),
            css::Style {
                flex_basis: css::length(2.0),
                flex_grow: 1.0,
                min_size: css::Size { width: css::length(0.0), height: css::length(0.0) },
                ..Default::default()
            },
            css::Style {
                flex_basis: css::length(2.0),
                flex_grow: 3.0,
                min_size: css::Size { width: css::length(0.0), height: css::length(0.0) },
                ..Default::default()
            },
        ];
        place(&arena, root, &styles, Size { width: 12, height: 1 });
        assert_eq!(first.borrow().outer.right, 4);
        assert_eq!(second.borrow().outer.left, 4);
        assert_eq!(second.borrow().outer.right, 12);
        styles[1].flex_basis = css::length(8.0);
        styles[2].flex_basis = css::length(8.0);
        place(&arena, root, &styles, Size { width: 10, height: 1 });
        assert_eq!(first.borrow().outer.right, 5);
        assert_eq!(second.borrow().outer.left, 5);
        assert_eq!(second.borrow().outer.right, 10);
    }

    #[test]
    fn bare_fraction_keeps_auto_minimum_but_flex_track_can_shrink() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let item = child(&arena, root, None, 20, 1);
        let mut styles = [css::Style { grid_template_columns: vec![css::fr(1.0)], ..grid() }];
        let mut engine = Layout::new();
        let engine_root = engine.insert(&root.borrow(), &styles);
        engine.compute(
            engine_root,
            css::Size {
                width: css::AvailableSpace::Definite(5.0),
                height: css::AvailableSpace::Definite(1.0),
            },
        );
        assert_eq!(engine.tree.layout(engine.entries[0].id).unwrap().size.width, 20.0);
        place(&arena, root, &styles, Size { width: 5, height: 1 });
        assert_eq!(item.borrow().outer.right, 5);
        assert_eq!(item.borrow().outer_clipped.right, 5);
        styles[0].grid_template_columns = vec![css::flex(1.0)];
        place(&arena, root, &styles, Size { width: 5, height: 1 });
        assert_eq!(item.borrow().outer.right, 5);
    }

    #[test]
    fn legacy_insets_count_once_and_tiny_viewports_stay_nonnegative() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let item = child(&arena, root, None, 3, 1);
        item.borrow_mut().attributes.bordered = true;
        item.borrow_mut().attributes.padding = Rect::two(1, 1);
        let styles = [css::Style { grid_template_rows: vec![css::auto()], ..grid() }];
        place(&arena, root, &styles, Size { width: 7, height: 5 });
        assert_eq!(item.borrow().inner, Rect { left: 2, top: 2, right: 5, bottom: 3 });
        for height in 0..3 {
            place(&arena, root, &styles, Size { width: 0, height });
            for cell in [root, item] {
                let node = cell.borrow();
                for rect in [node.outer, node.inner, node.outer_clipped, node.inner_clipped] {
                    assert!(rect.right >= rect.left && rect.bottom >= rect.top);
                    assert!(rect.right <= 0 && rect.bottom <= height);
                }
            }
        }
    }

    #[test]
    fn unmarked_blocks_keep_legacy_vertical_layout() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = Tree::alloc_node(&arena);
        let first = child(&arena, root, None, 3, 2);
        let second = child(&arena, root, None, 4, 3);
        place(&arena, root, &[], Size { width: 10, height: 10 });
        assert_eq!(first.borrow().outer, Rect { left: 0, top: 0, right: 10, bottom: 2 });
        assert_eq!(second.borrow().outer, Rect { left: 0, top: 2, right: 10, bottom: 5 });
    }

    #[test]
    fn css_row_intrinsic_height_survives_inside_a_legacy_block() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = Tree::alloc_node(&arena);
        let row = child(&arena, root, Some(0), 0, 0);
        row.borrow_mut().intrinsic_size_set = false;
        child(&arena, row, None, 3, 1);
        child(&arena, row, None, 4, 1);
        let footer = child(&arena, root, None, 10, 1);
        place(&arena, root, &[css::Style::default()], Size { width: 10, height: 10 });
        assert_eq!(row.borrow().intrinsic_size, Size { width: 7, height: 1 });
        assert_eq!(row.borrow().outer.height(), 1);
        assert_eq!(footer.borrow().outer.top, 1);
    }

    #[test]
    fn opaque_table_retains_legacy_columns_inside_css_padding() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let table = child(&arena, root, Some(1), 0, 0);
        {
            let mut node = table.borrow_mut();
            node.intrinsic_size_set = false;
            node.content = NodeContent::Table(super::super::TableContent {
                columns: stdext::collections::BVec::empty(),
                cell_gap: Size::default(),
            });
        }
        let row = child(&arena, table, None, 0, 0);
        let first = child(&arena, row, None, 2, 1);
        let second = child(&arena, row, None, 3, 1);
        let styles = [
            css::Style { grid_template_rows: vec![css::auto()], ..grid() },
            css::Style { padding: css::Rect::length(1.0), ..Default::default() },
        ];
        place(&arena, root, &styles, Size { width: 7, height: 3 });
        assert_eq!(table.borrow().inner, Rect { left: 1, top: 1, right: 6, bottom: 2 });
        assert_eq!(first.borrow().outer, Rect { left: 1, top: 1, right: 3, bottom: 2 });
        assert_eq!(second.borrow().outer, Rect { left: 3, top: 1, right: 6, bottom: 2 });
    }

    #[test]
    fn narrow_label_stays_one_line_and_scrollarea_keeps_its_scroll_offset() {
        let arena = Arena::new(1024 * 1024).unwrap();
        let root = root(&arena);
        let label = child(&arena, root, None, 20, 1);
        label.borrow_mut().content = NodeContent::Text(super::super::TextContent {
            text: stdext::collections::BString::empty(),
            chunks: stdext::collections::BVec::empty(),
            overflow: super::super::Overflow::Clip,
        });
        let scrollarea = child(&arena, root, None, 0, 10);
        scrollarea.borrow_mut().content = NodeContent::Scrollarea(super::super::ScrollareaContent {
            scroll_offset: Point { x: 0, y: 2 },
            scroll_offset_y_drag_start: CoordType::MIN,
            thumb_height: 0,
        });
        let content = child(&arena, scrollarea, None, 5, 10);
        let styles = [css::Style {
            grid_template_rows: vec![css::auto(), css::flex(1.0)],
            ..grid()
        }];
        place(&arena, root, &styles, Size { width: 5, height: 4 });
        assert_eq!(label.borrow().outer, Rect { left: 0, top: 0, right: 5, bottom: 1 });
        assert_eq!(scrollarea.borrow().inner, Rect { left: 0, top: 1, right: 4, bottom: 4 });
        assert_eq!(content.borrow().outer, Rect { left: 0, top: -1, right: 4, bottom: 9 });
        assert_eq!(content.borrow().outer_clipped, scrollarea.borrow().inner);
    }

    #[test]
    fn floated_nodes_are_excluded_and_arena_types_do_not_own_styles() {
        assert!(!std::mem::needs_drop::<Node<'_>>());
        assert!(!std::mem::needs_drop::<Tree<'_>>());
        let arena = Arena::new(1024 * 1024).unwrap();
        let mut tree = Tree::new(&arena);
        tree.root_first.borrow_mut().css_style = Some(0);
        let float = Tree::alloc_node(&arena);
        tree.push_child(float);
        tree.move_node_to_root(float, Some(tree.root_first));
        let item = child(&arena, tree.root_first, None, 2, 1);
        float.borrow_mut().intrinsic_size = Size { width: 100, height: 100 };
        float.borrow_mut().intrinsic_size_set = true;
        place(&arena, tree.root_first, &[grid()], Size { width: 8, height: 4 });
        assert_eq!(item.borrow().outer, Rect { left: 0, top: 0, right: 8, bottom: 4 });
        assert_eq!(float.borrow().outer, Rect::default());
    }
}
