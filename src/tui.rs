use crate::buffer::{CursorMovement, RcTextBuffer};
use crate::framebuffer::{Framebuffer, INDEXED_COLORS_COUNT, IndexedColor};
use crate::helpers::{CoordType, Point, Rect, Size, hash, hash_str, wymix};
use crate::input::{InputKeyMod, kbmod, vk};
use crate::ucd::Document;
use crate::{helpers, input, trust_me_bro, ucd};
use std::fmt::Write as _;
use std::iter;
use std::mem;
use std::ptr::{self, null};
use std::time;

const ROOT_ID: u64 = 0x14057B7EF767814F; // Knuth's MMIX constant
const SHIFT_TAB: InputKey = vk::TAB.with_modifiers(kbmod::SHIFT);

type InputText<'input> = input::InputText<'input>;
type InputKey = input::InputKey;
type InputMouseState = input::InputMouseState;

struct CachedTextBuffer {
    node_id: u64,
    editor: RcTextBuffer,
    seen: bool,
}

pub struct Tui {
    framebuffer: Framebuffer,
    read_timeout: time::Duration,

    /// Last known terminal size.
    size: Size,
    /// Last known mouse position.
    mouse_position: Point,
    /// Between mouse down and up, the position where the mouse was pressed.
    /// Otherwise, this contains Point::MIN.
    mouse_down_position: Point,
    /// Last known mouse state.
    mouse_state: InputMouseState,
    mouse_is_drag: bool,
    last_click: std::time::Instant,
    last_click_target: u64,
    last_click_position: Point,

    clipboard: Vec<u8>,
    cached_text_buffers: Vec<CachedTextBuffer>,
    hovered_node_path: Vec<u64>,
    focused_node_path: Vec<u64>,
    focused_node_path_previous_frame: Vec<u64>,
    focused_node_path_for_scrolling: Vec<u64>,

    prev_tree: Tree,
    prev_node_map: Vec<*const Node>,
    prev_node_map_shift: usize,
    prev_node_map_mask: u64,

    settling_have: i32,
    settling_want: i32,
}

impl Tui {
    pub fn new() -> Self {
        let mut tui = Self {
            framebuffer: Framebuffer::new(),
            read_timeout: time::Duration::MAX,

            size: Size {
                width: 0,
                height: 0,
            },
            mouse_position: Point::MIN,
            mouse_down_position: Point::MIN,
            mouse_state: InputMouseState::None,
            mouse_is_drag: false,
            last_click: std::time::Instant::now(),
            last_click_target: 0,
            last_click_position: Point::MIN,

            clipboard: Vec::new(),
            cached_text_buffers: Vec::with_capacity(16),
            hovered_node_path: Vec::with_capacity(16),
            focused_node_path: Vec::with_capacity(16),
            focused_node_path_previous_frame: Vec::with_capacity(16),
            focused_node_path_for_scrolling: Vec::with_capacity(16),

            prev_tree: Tree::new(),
            prev_node_map: vec![null(); 1],
            prev_node_map_shift: 0,
            prev_node_map_mask: 0,

            settling_have: 0,
            settling_want: 0,
        };
        tui.hovered_node_path.push(ROOT_ID);
        tui.focused_node_path.push(ROOT_ID);
        tui.focused_node_path_previous_frame.push(ROOT_ID);
        tui.focused_node_path_for_scrolling.push(ROOT_ID);
        tui
    }

    pub fn size(&self) -> Size {
        self.size
    }

    /// Sets up indexed colors for the TUI context.
    pub fn setup_indexed_colors(&mut self, colors: [u32; INDEXED_COLORS_COUNT]) {
        self.framebuffer.set_indexed_colors(colors);
    }

    pub fn read_timeout(&mut self) -> time::Duration {
        mem::replace(&mut self.read_timeout, time::Duration::MAX)
    }

    pub fn create_context<'tui, 'input>(
        &'tui mut self,
        input: Option<input::Input<'input>>,
    ) -> Context<'tui, 'input> {
        // In the input handler below we transformed a mouse up into a release event.
        // Now, a frame later, we must reset it back to none, to stop it from triggering things.
        // Same for Scroll events.
        if self.mouse_state > InputMouseState::Right {
            self.mouse_down_position = Point::MIN;
            self.mouse_state = InputMouseState::None;
            self.mouse_is_drag = false;
        }

        helpers::vec_replace_all_reuse(
            &mut self.focused_node_path_previous_frame,
            &self.focused_node_path,
        );

        if self.scroll_to_focused() {
            self.needs_more_settling();
        }

        let now = std::time::Instant::now();
        let mut input_text = None;
        let mut input_keyboard = None;
        let mut input_mouse_modifiers = kbmod::NONE;
        let mut input_mouse_is_click = false;
        let mut input_mouse_is_double_click = false;
        let mut input_scroll_delta = Point { x: 0, y: 0 };
        let input_consumed = self.needs_settling();

        match input {
            None => {}
            Some(input::Input::Resize(resize)) => {
                assert!(resize.width > 0 && resize.height > 0);
                assert!(resize.width < 32768 && resize.height < 32768);
                self.size = resize;
            }
            Some(input::Input::Text(text)) => {
                input_text = Some(text);
                // TODO: the .len()==1 check causes us to ignore keyboard inputs that are faster than we process them.
                // For instance, imagine the user presses "A" twice and we happen to read it in a single chunk.
                // This causes us to ignore the keyboard input here. We need a way to inform the caller over
                // how much of the input text we actually processed in a single frame. Or perhaps we could use
                // the needs_settling logic?
                if !text.bracketed && text.text.len() == 1 {
                    let ch = text.text.as_bytes()[0];
                    input_keyboard = InputKey::from_ascii(ch as char)
                }
            }
            Some(input::Input::Keyboard(keyboard)) => {
                input_keyboard = Some(keyboard);
            }
            Some(input::Input::Mouse(mouse)) => {
                let mut next_state = mouse.state;
                let next_position = mouse.position;
                let next_scroll = mouse.scroll;

                let mut hovered_node = null();
                let mut focused_node = null();

                for root in self.prev_tree.iterate_roots() {
                    Tree::visit_all(root, root, 0, true, |_, node| {
                        if !node.outer_clipped.contains(next_position) {
                            // Skip the entire sub-tree, because it doesn't contain the cursor.
                            return VisitControl::SkipChildren;
                        }
                        hovered_node = node;
                        if node.attributes.focusable {
                            focused_node = node;
                        }
                        VisitControl::Continue
                    });
                }

                Self::build_node_path(
                    unsafe { hovered_node.as_ref() },
                    &mut self.hovered_node_path,
                );

                if self.mouse_state != InputMouseState::None && next_state == InputMouseState::None
                {
                    // When the input transitions from some mouse input to no mouse input,
                    // we'll emit 1 InputMouseAction::Release event.
                    next_state = InputMouseState::Release;
                } else if self.mouse_state == InputMouseState::None
                    && next_state == InputMouseState::Left
                {
                    // On left-mouse-down we change focus.
                    Self::build_node_path(
                        unsafe { focused_node.as_ref() },
                        &mut self.focused_node_path,
                    );
                    self.mouse_down_position = next_position;
                    self.needs_more_settling(); // See `needs_more_settling()`.
                }

                if next_state == InputMouseState::Release && next_position == self.mouse_position {
                    let click_target = unsafe { focused_node.as_ref() }.map(|n| n.id).unwrap_or(0);
                    input_mouse_is_click = true;
                    input_mouse_is_double_click = (now - self.last_click)
                        <= std::time::Duration::from_millis(500)
                        && self.last_click_target == click_target
                        && self.last_click_position == next_position;
                    self.last_click = now;
                    self.last_click_target = click_target;
                    self.last_click_position = next_position;
                } else if self.mouse_state == InputMouseState::Left
                    && next_state == InputMouseState::Left
                    && next_position != self.mouse_position
                {
                    self.mouse_is_drag = true;
                }

                input_mouse_modifiers = mouse.modifiers;
                input_scroll_delta = next_scroll;
                self.mouse_position = next_position;
                self.mouse_state = next_state;
            }
        }

        if !input_consumed {
            // Every time there's input, we naturally need to re-render at least once.
            self.settling_have = 0;
            self.settling_want = 1;
        }

        Context {
            tui: self,

            input_text,
            input_keyboard,
            input_mouse_modifiers,
            input_mouse_is_click,
            input_mouse_is_double_click,
            input_scroll_delta,
            input_consumed,

            tree: Tree::new(),
            last_modal: null(),
            next_block_id_mixin: 0,
            needs_settling: false,
        }
    }

    fn report_context_completion(&mut self, ctx: &mut Context) {
        // If this hits, you forgot to block_end() somewhere. The best way to figure
        // out where is to do a binary search of commenting out code in main.rs.
        debug_assert!(
            Tree::node_ref(ctx.tree.current_node)
                .map(|r| r.stack_parent)
                .unwrap_or(null())
                .is_null()
        );

        if let Some(node) = Tree::node_ref(ctx.last_modal) {
            if !self.is_subtree_focused(node.id) {
                ctx.steal_focus_for(node);
            }
        }

        // If nodes have appeared or disappeared, we need to re-render.
        // Same, if the focus has changed (= changes the highlight color, etc.).
        let mut needs_settling = ctx.needs_settling;
        needs_settling |= self.prev_tree.checksum != ctx.tree.checksum;

        // Adopt the new tree and recalculate the node hashmap.
        self.prev_tree = mem::take(&mut ctx.tree);
        {
            let width = (4 * self.prev_tree.count + 1).ilog2().max(1) as usize;
            let shift = std::mem::size_of::<usize>() * 8 - width;
            let slots = 1 << width;
            let mask = slots - 1;
            let node_map = &mut self.prev_node_map;

            if slots != node_map.len() {
                *node_map = vec![null(); slots];
            } else {
                node_map.fill(null());
            }

            let mut node = self.prev_tree.root_first;
            while !node.is_null() {
                let n = unsafe { &*node };
                let mut slot = (n.id >> shift) as usize;
                loop {
                    if node_map[slot].is_null() {
                        node_map[slot] = n;
                        break;
                    }
                    slot = (slot + 1) & mask;
                }
                node = n.next;
            }

            self.prev_node_map_shift = shift;
            self.prev_node_map_mask = mask as u64;
        }

        let mut focus_path_pop_min = 0;
        // If the user pressed Escape, we move the focus to a parent node.
        if !ctx.input_consumed && ctx.consume_shortcut(vk::ESCAPE) {
            focus_path_pop_min = 1;
        }

        // Remove any unknown nodes from the focus path.
        // It's important that we do this after the tree has been swapped out,
        // so that pop_focusable_node() has access to the newest version of the tree.
        let focus_path_changed = self.pop_focusable_node(focus_path_pop_min);
        needs_settling |= focus_path_changed;

        // If some elements went away and the focus path changed above, we ignore tab presses.
        // It may otherwise lead to weird situations where focus moves unexpectedly.
        if !focus_path_changed && !ctx.input_consumed && ctx.input_keyboard.is_some() {
            needs_settling |= self.move_focus(ctx.input_keyboard.unwrap());
        }

        if needs_settling {
            self.needs_more_settling();
        }

        self.settling_have += 1;

        // Remove cached text editors that are no longer in use.
        self.cached_text_buffers.retain(|c| c.seen);

        for root in Tree::iterate_siblings(self.prev_tree.root_first) {
            root.compute_intrinsic_size();
        }

        let viewport = self.size.as_rect();

        for root in Tree::iterate_siblings(self.prev_tree.root_first) {
            if let Some(float) = &root.attributes.float {
                let (mut x, mut y) = Tree::node_ref(root.parent)
                    .map(|parent| (parent.outer.left, parent.outer.top))
                    .unwrap_or((0, 0));
                let size = root.intrinsic_to_outer();

                x += float.offset.x;
                y += float.offset.y;
                x -= (float.gravity_x * size.width as f32) as CoordType;
                y -= (float.gravity_y * size.height as f32) as CoordType;

                root.outer.left = x;
                root.outer.top = y;
                root.outer.right = x + size.width;
                root.outer.bottom = y + size.height;
                root.outer = root.outer.intersect(viewport);
            } else {
                root.outer = viewport;
            }

            root.inner = root.outer_to_inner(root.outer);
            root.outer_clipped = root.outer;
            root.inner_clipped = root.inner;
            root.layout_children(root.outer);
        }
    }

    fn build_node_path(node: Option<&Node>, path: &mut Vec<u64>) {
        path.clear();
        iter::successors(node, |&node| Tree::node_ref(node.parent)).for_each(|node| {
            path.push(node.id);
        });
        if path.is_empty() {
            path.push(ROOT_ID);
        }
    }

    /// After you finished processing all input, continue redrawing your UI until this returns false.
    pub fn needs_settling(&mut self) -> bool {
        self.settling_have <= self.settling_want
    }

    fn needs_more_settling(&mut self) {
        // If the focus has changed, the new node may need to be re-rendered.
        // Same, every time we encounter a previously unknown node via `get_prev_node`,
        // because that means it likely failed to get crucial information such as the layout size.
        debug_assert!(self.settling_have < 20);
        self.settling_want = (self.settling_have + 1).min(20);
    }

    /// Renders all nodes into a string-frame representation.
    pub fn render(&mut self) -> String {
        self.framebuffer.reset(self.size);
        for child in self.prev_tree.iterate_roots() {
            self.render_node(child);
        }
        self.framebuffer.render()
    }

    /// Recursively renders each node and its children.
    #[allow(clippy::only_used_in_recursion)]
    fn render_node(&mut self, node: &mut Node) {
        let outer_clipped = node.outer_clipped;
        if outer_clipped.is_empty() {
            return;
        }

        if node.attributes.bordered {
            // ┌────┐
            {
                let mut fill = String::new();
                fill.push('┌');
                helpers::string_append_repeat(
                    &mut fill,
                    '─',
                    (outer_clipped.right - outer_clipped.left - 2) as usize,
                );
                fill.push('┐');
                self.framebuffer.replace_text(
                    outer_clipped.top,
                    outer_clipped.left,
                    outer_clipped.right,
                    &fill,
                );
            }

            // │    │
            {
                let mut fill = String::new();
                fill.push('│');
                helpers::string_append_repeat(
                    &mut fill,
                    ' ',
                    (outer_clipped.right - outer_clipped.left - 2) as usize,
                );
                fill.push('│');

                for y in outer_clipped.top + 1..outer_clipped.bottom - 1 {
                    self.framebuffer.replace_text(
                        y,
                        outer_clipped.left,
                        outer_clipped.right,
                        &fill,
                    );
                }
            }

            // └────┘
            {
                let mut fill = String::new();
                fill.push('└');
                helpers::string_append_repeat(
                    &mut fill,
                    '─',
                    (outer_clipped.right - outer_clipped.left - 2) as usize,
                );
                fill.push('┘');
                self.framebuffer.replace_text(
                    outer_clipped.bottom - 1,
                    outer_clipped.left,
                    outer_clipped.right,
                    &fill,
                );
            }
        } else if node.attributes.float.is_some() {
            let mut fill = String::new();
            helpers::string_append_repeat(
                &mut fill,
                ' ',
                (outer_clipped.right - outer_clipped.left) as usize,
            );

            for y in outer_clipped.top..outer_clipped.bottom {
                self.framebuffer
                    .replace_text(y, outer_clipped.left, outer_clipped.right, &fill);
            }
        }

        if node.attributes.focus_brackets {
            let has_focus = self.is_node_focused(node.id);
            let center_y = (node.inner_clipped.top + node.inner_clipped.bottom) / 2;
            self.framebuffer.replace_text(
                center_y,
                node.inner.left - 1,
                node.inner.left,
                if has_focus { ">" } else { "[" },
            );
            self.framebuffer.replace_text(
                center_y,
                node.inner.right,
                node.inner.right + 1,
                if has_focus { "<" } else { "]" },
            );
        }

        {
            let mut rect = outer_clipped;
            if node.attributes.focus_brackets {
                rect.left += 1;
                rect.right -= 1;
            }
            self.framebuffer.blend_bg(rect, node.attributes.bg);
            self.framebuffer.blend_fg(rect, node.attributes.fg);
        }

        let inner = node.inner;
        let inner_clipped = node.inner_clipped;
        if inner_clipped.is_empty() {
            return;
        }

        match &mut node.content {
            NodeContent::Modal(title) => {
                self.framebuffer.replace_text(
                    node.outer.top,
                    node.outer.left + 2,
                    node.outer.right - 1,
                    title,
                );
            }
            NodeContent::Text(content) => {
                if !inner_clipped.is_empty() {
                    if content.overflow != Overflow::Clip
                        && node.intrinsic_size.width > inner.width()
                        // TODO: Implement ellipsis support for text with multiple chunks.
                        && content.chunks.len() == 1
                    {
                        let actual_width = node.intrinsic_size.width;
                        let restricted_width = inner.width();
                        let chunk = &content.chunks[0];
                        let text = &chunk.text[..];
                        let bytes = text.as_bytes();
                        let mut modified = String::with_capacity(text.len());
                        let mut cfg = ucd::MeasurementConfig::new(&bytes);

                        match content.overflow {
                            Overflow::Clip => unreachable!(),
                            Overflow::TruncateHead => {
                                modified.push('…');
                                let beg = cfg.goto_visual(Point {
                                    x: actual_width - restricted_width + 1,
                                    y: 0,
                                });
                                modified.push_str(&text[beg.offset..]);
                            }
                            Overflow::TruncateMiddle => {
                                let mid_beg_x = (restricted_width - 1) / 2;
                                let mid_end_x = actual_width - restricted_width / 2;
                                let beg = cfg.goto_visual(Point { x: mid_beg_x, y: 0 });
                                let mut end = cfg.goto_visual(Point { x: mid_end_x, y: 0 });
                                if end.visual_pos.x < mid_end_x {
                                    // If we intersected a wide glyph, we need to move past that.
                                    end = cfg.goto_logical(Point {
                                        x: end.logical_pos.x + 1,
                                        y: 0,
                                    });
                                }
                                modified.push_str(&text[..beg.offset]);
                                modified.push('…');
                                modified.push_str(&text[end.offset..]);
                            }
                            Overflow::TruncateTail => {
                                let end = cfg.goto_visual(Point {
                                    x: restricted_width - 1,
                                    y: 0,
                                });
                                modified.push_str(&text[..end.offset]);
                                modified.push('…');
                            }
                        }

                        let rect = self.framebuffer.replace_text(
                            inner_clipped.top,
                            inner_clipped.left,
                            inner_clipped.right,
                            &modified,
                        );
                        self.framebuffer.blend_fg(rect, chunk.fg);
                    } else {
                        let mut beg_x = inner.left;
                        for chunk in &content.chunks {
                            let rect = self.framebuffer.replace_text(
                                inner_clipped.top,
                                beg_x,
                                inner_clipped.right,
                                &chunk.text,
                            );
                            self.framebuffer.blend_fg(rect, chunk.fg);
                            beg_x = rect.right;
                        }
                    }
                }
            }
            NodeContent::Textarea(tc) => {
                let tb = &mut *tc.buffer;
                let mut destination = Rect {
                    left: inner_clipped.left,
                    top: inner_clipped.top,
                    right: inner_clipped.right,
                    bottom: inner_clipped.bottom,
                };

                if !tc.single_line {
                    // Account for the scrollbar.
                    destination.right -= 1;
                }

                tb.render(
                    tc.scroll_offset,
                    destination,
                    tc.has_focus,
                    &mut self.framebuffer,
                );

                if !tc.single_line {
                    // Render the scrollbar.
                    let track = Rect {
                        left: inner_clipped.right - 1,
                        top: inner_clipped.top,
                        right: inner_clipped.right,
                        bottom: inner_clipped.bottom,
                    };
                    tc.thumb_height = self.framebuffer.draw_scrollbar(
                        inner_clipped,
                        track,
                        tc.scroll_offset.y,
                        tb.get_visual_line_count() + inner.height() - 1,
                    );
                }
            }
            NodeContent::Scrollarea(sc) => {
                let content = Tree::node_ref(node.children.first).unwrap();
                let track = Rect {
                    left: inner.right,
                    top: inner.top,
                    right: inner.right + 1,
                    bottom: inner.bottom,
                };
                sc.thumb_height = self.framebuffer.draw_scrollbar(
                    outer_clipped,
                    track,
                    sc.scroll_offset.y,
                    content.intrinsic_size.height,
                );
            }
            _ => {}
        }

        for child in Tree::iterate_siblings(node.children.first) {
            self.render_node(child);
        }
    }

    /// Outputs a debug string of the layout and focus tree.
    pub fn debug_layout(&mut self) -> String {
        let mut result = String::new();
        result.push_str("general:\r\n- focus_path:\r\n");

        for &id in self.focused_node_path.iter().rev() {
            _ = write!(result, "  - {:016x}\r\n", id);
        }

        result.push_str("\r\ntree:\r\n");

        for root in self.prev_tree.iterate_roots() {
            Tree::visit_all(root, root, 0, true, |depth, node| {
                helpers::string_append_repeat(&mut result, ' ', depth * 2);
                _ = write!(result, "- id: {:016x}\r\n", node.id);

                helpers::string_append_repeat(&mut result, ' ', depth * 2);
                _ = write!(result, "  classname:    {}\r\n", node.classname);

                if depth == 0 {
                    if let Some(parent) = Tree::node_ref(node.parent) {
                        helpers::string_append_repeat(&mut result, ' ', depth * 2);
                        _ = write!(result, "  parent:       {:016x}\r\n", parent.id);
                    }
                }

                helpers::string_append_repeat(&mut result, ' ', depth * 2);
                _ = write!(
                    result,
                    "  intrinsic:    {{{}, {}}}\r\n",
                    node.intrinsic_size.width, node.intrinsic_size.height
                );

                helpers::string_append_repeat(&mut result, ' ', depth * 2);
                _ = write!(
                    result,
                    "  outer:        {{{}, {}, {}, {}}}\r\n",
                    node.outer.left, node.outer.top, node.outer.right, node.outer.bottom
                );

                helpers::string_append_repeat(&mut result, ' ', depth * 2);
                _ = write!(
                    result,
                    "  inner:        {{{}, {}, {}, {}}}\r\n",
                    node.inner.left, node.inner.top, node.inner.right, node.inner.bottom
                );

                if node.attributes.bordered {
                    helpers::string_append_repeat(&mut result, ' ', depth * 2);
                    result.push_str("  bordered:     true\r\n");
                }

                if node.attributes.bg != 0 {
                    helpers::string_append_repeat(&mut result, ' ', depth * 2);
                    _ = write!(result, "  bg:           #{:08x}\r\n", node.attributes.bg);
                }

                if node.attributes.fg != 0 {
                    helpers::string_append_repeat(&mut result, ' ', depth * 2);
                    _ = write!(result, "  fg:           #{:08x}\r\n", node.attributes.fg);
                }

                if self.is_node_focused(node.id) {
                    helpers::string_append_repeat(&mut result, ' ', depth * 2);
                    result.push_str("  focused:      true\r\n");
                }

                match &node.content {
                    NodeContent::Text(content) => {
                        helpers::string_append_repeat(&mut result, ' ', depth * 2);
                        _ = write!(
                            result,
                            "  text:         \"{}\"\r\n",
                            content
                                .chunks
                                .iter()
                                .map(|c| c.text.as_str())
                                .collect::<String>()
                        );
                    }
                    NodeContent::Textarea(content) => {
                        let tb = &*content.buffer;
                        helpers::string_append_repeat(&mut result, ' ', depth * 2);
                        _ = write!(result, "  textarea:     {tb:p}\r\n");
                    }
                    NodeContent::Scrollarea(..) => {
                        helpers::string_append_repeat(&mut result, ' ', depth * 2);
                        result.push_str("  scrollable:   true\r\n");
                    }
                    _ => {}
                }

                VisitControl::Continue
            });
        }

        result
    }

    /// Checks if the pointer is on the current node's boundary (hover).
    pub fn is_node_hovered(&mut self, id: u64) -> bool {
        // We construct the hovered_node_path always with at least 1 element (the root id).
        unsafe { *self.hovered_node_path.get_unchecked(0) == id }
    }

    /// Checks if a node's subtree contains the hover path.
    pub fn is_subtree_hovered(&mut self, id: u64) -> bool {
        self.hovered_node_path.contains(&id)
    }

    /// Checks if a node with the given ID has input focus.
    fn is_node_focused(&self, id: u64) -> bool {
        // We construct the focused_node_path always with at least 1 element (the root id).
        unsafe { *self.focused_node_path.get_unchecked(0) == id }
    }

    /// Checks if a node's subtree contains the focus path.
    fn is_subtree_focused(&self, id: u64) -> bool {
        self.focused_node_path.contains(&id)
    }

    fn get_prev_node<'b>(&mut self, id: u64) -> Option<&'b Node> {
        let node_map = &self.prev_node_map[..];
        let shift = self.prev_node_map_shift;
        let mask = self.prev_node_map_mask;
        let mut slot = (id >> shift) & mask;

        loop {
            let node = node_map[slot as usize];
            if node.is_null() {
                return None;
            }
            if unsafe { &*node }.id == id {
                return Some(unsafe { &*(node as *const _) });
            }
            slot = (slot + 1) & mask;
        }
    }

    fn pop_focusable_node(&mut self, pop_minimum: usize) -> bool {
        let pop_minimum = pop_minimum.min(self.focused_node_path.len());
        let path = &self.focused_node_path[pop_minimum..self.focused_node_path.len()];

        // TODO: This is only needed because get_prev_node doesn't live on a property of `self`.
        // That would fix the borrow checker issue.
        let path = trust_me_bro::this_lifetime_change_is_totally_safe(path);

        // Find the next focusable node upwards in the hierarchy.
        let focusable_idx = path
            .iter()
            .skip(pop_minimum)
            .position(|&id| {
                self.get_prev_node(id)
                    .is_some_and(|node| node.attributes.focusable)
            })
            .map(|idx| idx + pop_minimum)
            .unwrap_or(path.len());

        if focusable_idx == 0 {
            // Nothing to remove.
            false
        } else {
            // Remove all the nodes between us and the next focusable node.
            self.focused_node_path.drain(..focusable_idx);
            if self.focused_node_path.is_empty() {
                self.focused_node_path.push(ROOT_ID);
            }
            true
        }
    }

    fn move_focus(&mut self, input: InputKey) -> bool {
        let Some(focused) = self.get_prev_node(self.focused_node_path[0]) else {
            debug_assert!(false); // The caller should've cleaned up the focus path.
            return false;
        };

        if input == vk::LEFT || input == vk::RIGHT {
            if let Some(row) = Tree::node_ref(focused.parent) {
                if let Some(table) = Tree::node_ref(row.parent) {
                    if matches!(table.content, NodeContent::Table(..)) {
                        let mut next = if input == vk::LEFT {
                            focused.siblings.prev
                        } else {
                            focused.siblings.next
                        };
                        if next.is_null() {
                            next = if input == vk::LEFT {
                                row.children.last
                            } else {
                                row.children.first
                            };
                        }
                        let next = Tree::node_ref(next).unwrap();
                        if !ptr::eq(next, focused) {
                            Tui::build_node_path(Some(next), &mut self.focused_node_path);
                            return true;
                        }
                    }
                }
            }
        }

        let forward = match input {
            SHIFT_TAB => false,
            vk::TAB => true,
            _ => return false,
        };

        let mut focused_start = focused;
        let mut focused_next = focused;
        let mut root = focused;

        // Figure out if we're inside a focus void (a container that doesn't
        // allow tabbing inside), and in that case, toss the focus to it.
        //
        // Also, figure out the container within which the focuse must be contained.
        // This way, tab/shift-tab only moves within the same window.
        // The ROOT_ID node has no parent, and the others have a float attribute.
        // If the root is the focused node, it should of course not move upward.
        while !root.attributes.focus_well {
            if root.attributes.focus_void {
                focused_start = root;
            }
            if root.parent.is_null() {
                break;
            }
            root = Tree::node_ref(root.parent).unwrap();
        }

        // If the window doesn't contain any nodes, there's nothing to focus.
        // This also protects against infinite loops below.
        if root.children.first.is_null() {
            return false;
        }

        Tree::visit_all(root, focused_start, usize::MAX / 2, forward, |_, node| {
            if node.attributes.focusable && !ptr::eq(node, root) && !ptr::eq(node, focused_start) {
                focused_next = trust_me_bro::this_lifetime_change_is_totally_safe(node);
                VisitControl::Stop
            } else if node.attributes.focus_void {
                VisitControl::SkipChildren
            } else {
                VisitControl::Continue
            }
        });

        if ptr::eq(focused_next, focused_start) {
            false
        } else {
            Tui::build_node_path(Some(focused_next), &mut self.focused_node_path);
            true
        }
    }

    // Scroll the focused node(s) into view inside scrollviews
    fn scroll_to_focused(&mut self) -> bool {
        if self.focused_node_path_for_scrolling == self.focused_node_path {
            return false;
        }

        let Some(focused) = self.get_prev_node(self.focused_node_path[0]) else {
            // Node not found because we're using the old layout tree.
            // Retry in the next rendering loop.
            return true;
        };

        let mut scrollarea = Tree::node_mut(focused).unwrap();
        let mut scroll_to = focused.outer;

        while !scrollarea.parent.is_null() && scrollarea.attributes.float.is_none() {
            if let NodeContent::Scrollarea(sc) = &mut scrollarea.content {
                let off_y = sc.scroll_offset.y.max(0);
                let mut y = off_y;
                y = y.min(scroll_to.top - scrollarea.inner.top + off_y);
                y = y.max(scroll_to.bottom - scrollarea.inner.bottom + off_y);
                sc.scroll_offset.y = y;
                scroll_to = scrollarea.outer;
            }
            scrollarea = Tree::node_mut(scrollarea.parent).unwrap();
        }

        self.focused_node_path_for_scrolling = self.focused_node_path.clone();
        true
    }
}

pub struct Context<'tui, 'input> {
    tui: &'tui mut Tui,

    /// Current text input, if any.
    input_text: Option<InputText<'input>>,
    /// Current keyboard input, if any.
    input_keyboard: Option<InputKey>,
    input_mouse_modifiers: InputKeyMod,
    input_mouse_is_click: bool,
    input_mouse_is_double_click: bool,
    /// By how much the mouse wheel was scrolled since the last frame.
    input_scroll_delta: Point,
    input_consumed: bool,

    tree: Tree,
    last_modal: *const Node,
    next_block_id_mixin: u64,
    needs_settling: bool,
}

impl Drop for Context<'_, '_> {
    fn drop(&mut self) {
        let this = self as *mut _;
        self.tui.report_context_completion(unsafe { &mut *this });
    }
}

impl Context<'_, '_> {
    /// Returns the current terminal size.
    pub fn size(&self) -> Size {
        self.tui.size
    }

    pub fn indexed(&self, index: IndexedColor) -> u32 {
        self.tui.framebuffer.indexed(index)
    }

    pub fn set_clipboard(&mut self, data: Vec<u8>) {
        self.tui.clipboard = data;
        self.needs_rerender();
    }

    pub fn get_clipboard(&self) -> &[u8] {
        &self.tui.clipboard
    }

    pub fn needs_rerender(&mut self) {
        // If this hits, the call stack is responsible is trying to deadlock you.
        debug_assert!(self.tui.settling_have < 100);
        self.needs_settling = true;
    }

    /// Begins a new UI block (container) with a unique ID.
    pub fn block_begin(&mut self, classname: &'static str) {
        let parent = self.tree.current_node_mut();

        let mut id = hash_str(parent.id, classname);
        if self.next_block_id_mixin != 0 {
            id = hash(id, &self.next_block_id_mixin.to_ne_bytes());
            self.next_block_id_mixin = 0;
        }

        // If this hits, you have tried to create a block with the same ID as a previous one
        // somewhere up this call stack. Change the classname, or use next_block_id_mixin().
        #[cfg(debug_assertions)]
        for child in Tree::iterate_siblings(parent.children.first) {
            debug_assert_ne!(child.id, id);
        }

        self.tree.append_child(Node {
            stack_parent: parent,
            id,
            classname,
            parent,
            ..Default::default()
        });
    }

    /// Ends the current UI block, returning to its parent container.
    pub fn block_end(&mut self) {
        self.tree.pop_stack();
    }

    /// Mixes in an extra value to the next UI block's ID for uniqueness.
    pub fn next_block_id_mixin(&mut self, id: u64) {
        self.next_block_id_mixin = id;
    }

    fn attr_focusable(&mut self) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.focusable = true;
    }

    pub fn focus_on_first_present(&mut self) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.focusable = true;
        if self.tui.get_prev_node(last_node.id).is_none() {
            self.steal_focus();
        }
    }

    pub fn steal_focus(&mut self) {
        self.steal_focus_for(self.tree.last_node_ref());
    }

    fn steal_focus_for(&mut self, node: &Node) {
        if !self.tui.is_node_focused(node.id) {
            Tui::build_node_path(Some(node), &mut self.tui.focused_node_path);
            self.needs_rerender();
        }
    }

    pub fn toss_focus_up(&mut self) {
        let current_node = self.tree.current_node_ref();
        // Check the path length to avoid popping the root node and scheduling a rerender for no reason.
        if current_node.attributes.focusable
            && self.tui.focused_node_path.len() >= 2
            && self.tui.is_node_focused(current_node.id)
        {
            self.tui.pop_focusable_node(1);
            self.needs_rerender();
        }
    }

    pub fn inherit_focus(&mut self) {
        let last_node = self.tree.last_node_mut();
        let Some(parent) = Tree::node_mut(last_node.parent) else {
            return;
        };

        last_node.attributes.focusable = true;
        // Mark the parent as focusable, so that if the user presses Escape,
        // and `block_end` bubbles the focus up the tree, it'll stop on our parent,
        // which will then focus us on the next iteration.
        parent.attributes.focusable = true;

        if self.tui.is_node_focused(parent.id) {
            self.needs_rerender();
            self.tui.focused_node_path.insert(0, last_node.id);
        }
    }

    pub fn attr_focus_well(&mut self) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.focus_well = true;
    }

    pub fn attr_intrinsic_size(&mut self, size: Size) {
        let last_node = self.tree.last_node_mut();
        last_node.intrinsic_size = size;
        last_node.intrinsic_size_set = true;
    }

    pub fn attr_float(&mut self, spec: FloatSpec) {
        let last_node = self.tree.last_node_mut();
        let anchor = match spec.anchor {
            Anchor::Last if !last_node.siblings.prev.is_null() => last_node.siblings.prev,
            Anchor::Last | Anchor::Parent => last_node.parent,
            // By not giving such floats a parent, they get the same origin as the original root node,
            // but they also gain their own "root id" in the tree. That way, their focus path is totally unique,
            // which means that we can easily check if a modal is open by calling `is_focused()` on the original root.
            Anchor::Root => null(),
        };

        // Remove the node from the UI tree and insert it into the floater list.
        last_node.remove_from_parent();
        last_node.siblings.prev = self.tree.root_last;
        if let Some(root) = Tree::node_mut(self.tree.root_last) {
            root.siblings.next = last_node;
            self.tree.root_last = last_node;
        }

        last_node.parent = anchor;
        last_node.attributes.focus_well = true;
        last_node.attributes.float = Some(FloatAttributes {
            gravity_x: spec.gravity_x.clamp(0.0, 1.0),
            gravity_y: spec.gravity_y.clamp(0.0, 1.0),
            offset: Point {
                x: spec.offset_x,
                y: spec.offset_y,
            },
        });
    }

    pub fn attr_border(&mut self) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.bordered = true;
    }

    pub fn attr_position(&mut self, align: Position) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.position = align;
    }

    pub fn attr_padding(&mut self, padding: Rect) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.padding = Self::normalize_rect(padding);
    }

    fn normalize_rect(rect: Rect) -> Rect {
        Rect {
            left: rect.left.max(0),
            top: rect.top.max(0),
            right: rect.right.max(0),
            bottom: rect.bottom.max(0),
        }
    }

    pub fn attr_background_rgba(&mut self, bg: u32) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.bg = bg;
    }

    pub fn attr_foreground_rgba(&mut self, fg: u32) {
        let last_node = self.tree.last_node_mut();
        last_node.attributes.fg = fg;
    }

    pub fn consume_shortcut(&mut self, shortcut: InputKey) -> bool {
        if !self.input_consumed && self.input_keyboard == Some(shortcut) {
            self.set_input_consumed();
            true
        } else {
            false
        }
    }

    fn set_input_consumed(&mut self) {
        debug_assert!(!self.input_consumed);
        self.input_consumed = true;
    }

    pub fn is_hovering(&mut self) -> bool {
        let last_node = self.tree.last_node_ref();
        self.tui.is_node_hovered(last_node.id)
    }

    pub fn contains_hover(&mut self) -> bool {
        let last_node = self.tree.last_node_ref();
        self.tui.is_subtree_hovered(last_node.id)
    }

    pub fn is_focused(&mut self) -> bool {
        let last_node = self.tree.last_node_ref();
        self.tui.is_node_focused(last_node.id)
    }

    pub fn contains_focus(&mut self) -> bool {
        let last_node = self.tree.last_node_ref();
        self.tui.is_subtree_focused(last_node.id)
    }

    pub fn modal_begin(&mut self, classname: &'static str, title: &str) {
        self.block_begin(classname);
        self.focus_on_first_present();
        self.attr_border();
        self.attr_background_rgba(self.indexed(IndexedColor::White));
        self.attr_foreground_rgba(self.indexed(IndexedColor::Black));
        self.attr_float(FloatSpec {
            anchor: Anchor::Root,
            gravity_x: 0.5,
            gravity_y: 0.5,
            offset_x: self.tui.size.width / 2,
            offset_y: self.tui.size.height / 2,
        });

        let last_node = self.tree.last_node_mut();
        last_node.content = NodeContent::Modal(format!(" {} ", title));
        self.last_modal = last_node;
    }

    pub fn modal_end(&mut self) -> bool {
        self.block_end();
        self.consume_shortcut(vk::ESCAPE)
    }

    pub fn table_begin(&mut self, classname: &'static str) {
        self.block_begin(classname);

        let last_node = self.tree.last_node_mut();
        last_node.content = NodeContent::Table(TableContent {
            columns: Vec::new(),
            cell_gap: Size::default(),
        });
    }

    pub fn table_set_columns(&mut self, columns: &[i32]) {
        let last_node = self.tree.last_node_mut();
        if let NodeContent::Table(spec) = &mut last_node.content {
            spec.columns = columns.to_vec();
        } else {
            debug_assert!(false);
        }
    }

    pub fn table_set_cell_gap(&mut self, cell_gap: Size) {
        let last_node = self.tree.last_node_mut();
        if let NodeContent::Table(spec) = &mut last_node.content {
            spec.cell_gap = cell_gap;
        } else {
            debug_assert!(false);
        }
    }

    pub fn table_next_row(&mut self) {
        let current_node = self.tree.current_node_ref();

        // If this is the first call to table_next_row() inside a new table, the
        // current_node will refer to the table. Otherwise, it'll refer to the current row.
        if !matches!(current_node.content, NodeContent::Table(_)) {
            let Some(parent) = Tree::node_ref(current_node.parent) else {
                return;
            };

            // Neither the current nor its parent nodes are a table?
            // You definitely called this outside of a table block.
            debug_assert!(matches!(parent.content, NodeContent::Table(_)));

            self.block_end();
            self.next_block_id_mixin(parent.child_count as u64);
        }

        self.block_begin("row");
    }

    pub fn table_end(&mut self) {
        let current_node = self.tree.current_node_ref();

        // If this is the first call to table_next_row() inside a new table, the
        // current_node will refer to the table. Otherwise, it'll refer to the current row.
        if !matches!(current_node.content, NodeContent::Table(_)) {
            self.block_end();
        }

        self.block_end(); // table
    }

    pub fn label(&mut self, classname: &'static str, overflow: Overflow, text: &str) {
        self.styled_label_begin(classname, overflow);
        self.styled_label_add_text(text);
        self.styled_label_end();
    }

    pub fn styled_label_begin(&mut self, classname: &'static str, overflow: Overflow) {
        self.block_begin(classname);
        self.tree.last_node_mut().content = NodeContent::Text(TextContent {
            chunks: Vec::new(),
            overflow,
        });
    }

    pub fn styled_label_set_foreground_indexed(&mut self, index: Option<IndexedColor>) {
        let fg = index.map(|i| self.indexed(i)).unwrap_or(0);
        if let Some(chunk) = self.styled_label_get_last_chunk(true) {
            chunk.fg = fg;
        }
    }

    pub fn styled_label_add_text(&mut self, text: &str) {
        if let Some(chunk) = self.styled_label_get_last_chunk(false) {
            chunk.text.push_str(text);
        }
    }

    fn styled_label_get_last_chunk(&mut self, flush: bool) -> Option<&mut StyledTextChunk> {
        let last_node = self.tree.last_node_mut();
        let NodeContent::Text(content) = &mut last_node.content else {
            // You called styled_label_*() outside an styled_label_*() block.
            debug_assert!(false);
            return None;
        };

        if content.chunks.is_empty() || (flush && !content.chunks.last().unwrap().text.is_empty()) {
            content.chunks.push(StyledTextChunk {
                text: String::new(),
                fg: 0,
            });
        }

        content.chunks.last_mut()
    }

    pub fn styled_label_end(&mut self) {
        let last_node = self.tree.last_node_mut();
        let NodeContent::Text(content) = &last_node.content else {
            return;
        };

        let cursor = ucd::MeasurementConfig::new(&content.chunks).goto_visual(Point {
            x: CoordType::MAX,
            y: 0,
        });
        last_node.intrinsic_size.width = cursor.visual_pos.x;
        last_node.intrinsic_size.height = 1;
        last_node.intrinsic_size_set = true;

        self.block_end();
    }

    pub fn button(&mut self, classname: &'static str, overflow: Overflow, text: &str) -> bool {
        self.label(classname, overflow, text);

        let last_node = self.tree.last_node_mut();
        last_node.attributes.focusable = true;
        last_node.attributes.focus_brackets = true;

        self.button_activated()
    }

    pub fn checkbox(
        &mut self,
        classname: &'static str,
        overflow: Overflow,
        text: &str,
        checked: &mut bool,
    ) -> bool {
        self.styled_label_begin(classname, overflow);
        self.styled_label_add_text(if *checked { "▣ " } else { "☐ " });
        self.styled_label_add_text(text);
        self.styled_label_end();

        let last_node = self.tree.last_node_mut();
        last_node.attributes.focusable = true;
        last_node.attributes.focus_brackets = true;

        let activated = self.button_activated();
        if activated {
            *checked = !*checked;
        }
        activated
    }

    fn button_activated(&mut self) -> bool {
        if !self.input_consumed
            && ((self.input_mouse_is_click && self.contains_hover())
                || self.input_keyboard == Some(vk::RETURN)
                || self.input_keyboard == Some(vk::SPACE))
            && self.is_focused()
        {
            self.set_input_consumed();
            true
        } else {
            false
        }
    }

    pub fn editline<'a, 'b: 'a>(
        &'a mut self,
        classname: &'static str,
        text: &'b mut String,
    ) -> bool {
        self.block_begin(classname);

        let node = self.tree.current_node_mut();
        let cached;
        if let Some(buffer) = self
            .tui
            .cached_text_buffers
            .iter_mut()
            .find(|t| t.node_id == node.id)
        {
            cached = buffer;
            cached.seen = true;
        } else {
            self.tui.cached_text_buffers.push(CachedTextBuffer {
                node_id: node.id,
                editor: RcTextBuffer::new(true).unwrap(),
                seen: true,
            });
            cached = self.tui.cached_text_buffers.last_mut().unwrap();
        }

        let mut buffer = cached.editor.clone();
        buffer.copy_from_str(text);

        self.textarea_internal(buffer.clone(), true);

        let changed = buffer.is_dirty();
        if changed {
            buffer.save_as_string(text);
        }

        self.block_end();
        changed
    }

    pub fn textarea(&mut self, classname: &'static str, tb: RcTextBuffer) {
        self.block_begin(classname);
        self.textarea_internal(tb, false);
        self.block_end();
    }

    fn textarea_internal(&mut self, buffer: RcTextBuffer, single_line: bool) {
        self.attr_focusable();

        let node = self.tree.last_node_mut();
        node.attributes.bg = self.indexed(IndexedColor::DefaultBackground);
        node.attributes.fg = self.indexed(IndexedColor::DefaultForeground);
        node.attributes.focus_brackets = single_line;

        let mut content = TextareaContent {
            buffer,
            scroll_offset: Point::default(),
            scroll_offset_y_drag_start: CoordType::MIN,
            thumb_height: 0,
            preferred_column: 0,
            single_line,
            has_focus: self.tui.is_node_focused(node.id),
        };

        if let Some(node_prev) = self.tui.get_prev_node(node.id) {
            if let NodeContent::Textarea(content_prev) = &node_prev.content {
                content.scroll_offset = content_prev.scroll_offset;
                content.scroll_offset_y_drag_start = content_prev.scroll_offset_y_drag_start;
                content.thumb_height = content_prev.thumb_height;
                content.preferred_column = content_prev.preferred_column;

                let mut text_width = node_prev.inner.width();
                if !single_line {
                    // Subtract -1 to account for the scrollbar.
                    text_width -= 1;
                }

                let mut make_cursor_visible = content.buffer.take_cursor_visibility_request();
                make_cursor_visible |= content.buffer.set_width(text_width);
                make_cursor_visible |=
                    self.textarea_handle_input(&mut content, node_prev, single_line);

                if make_cursor_visible {
                    self.textarea_make_cursor_visible(&mut content, node_prev);
                }
            } else {
                debug_assert!(false);
            }
        }

        self.textarea_adjust_scroll_offset(&mut content);

        node.intrinsic_size.height = content.buffer.get_visual_line_count();
        node.intrinsic_size_set = true;
        node.content = NodeContent::Textarea(content);
    }

    fn textarea_handle_input(
        &mut self,
        tc: &mut TextareaContent,
        node_prev: &Node,
        single_line: bool,
    ) -> bool {
        if self.input_consumed {
            return false;
        }

        let tb = &mut *tc.buffer;

        if self.tui.mouse_state == InputMouseState::Scroll && self.tui.is_node_hovered(node_prev.id)
        {
            tc.scroll_offset.x += self.input_scroll_delta.x;
            tc.scroll_offset.y += self.input_scroll_delta.y;
            self.set_input_consumed();
            return false;
        }

        if self.tui.mouse_state != InputMouseState::None && self.tui.is_node_focused(node_prev.id) {
            let mut make_cursor_visible = false;
            let mouse = self.tui.mouse_position;
            let inner = node_prev.inner;
            let text = Rect {
                left: inner.left + tb.get_margin_width(),
                top: inner.top,
                right: inner.right - 1,
                bottom: inner.bottom,
            };
            let pos = Point {
                x: mouse.x - inner.left - tb.get_margin_width() + tc.scroll_offset.x,
                y: mouse.y - inner.top + tc.scroll_offset.y,
            };

            if self.input_mouse_is_double_click {
                tb.select_word();
            } else if self.tui.mouse_is_drag {
                let track_rect = Rect {
                    left: inner.right - 1,
                    top: inner.top,
                    right: inner.right,
                    bottom: inner.bottom,
                };
                if track_rect.contains(self.tui.mouse_down_position) {
                    if tc.scroll_offset_y_drag_start == CoordType::MIN {
                        tc.scroll_offset_y_drag_start = tc.scroll_offset.y;
                    }

                    // The textarea supports 1 height worth of "scrolling beyond the end".
                    // `track_height` is the same as the viewport height.
                    let scrollable_height = tb.get_visual_line_count() - 1;

                    if scrollable_height > 0 {
                        let trackable = track_rect.height() - tc.thumb_height;
                        let delta_y = mouse.y - self.tui.mouse_down_position.y;
                        tc.scroll_offset.y = tc.scroll_offset_y_drag_start
                            + ((delta_y * scrollable_height) / trackable);
                    }
                } else {
                    tb.selection_update_visual(pos);
                    tc.preferred_column = tb.get_cursor_visual_pos().x;

                    let height = inner.height();

                    // If the editor is only 1 line tall we can't possibly scroll up or down.
                    if height >= 2 {
                        fn calc(min: CoordType, max: CoordType, mouse: CoordType) -> CoordType {
                            // Otherwise, the scroll zone is up to 3 lines at the top/bottom.
                            let zone_height = ((max - min) / 2).min(3);

                            // The .y positions where the scroll zones begin:
                            // Mouse coordinates above top and below bottom respectively.
                            let scroll_min = min + zone_height;
                            let scroll_max = max - zone_height - 1;

                            // Calculate the delta for scrolling up or down.
                            let delta_min = (mouse - scroll_min).clamp(-zone_height, 0);
                            let delta_max = (mouse - scroll_max).clamp(0, zone_height);

                            // If I didn't mess up my logic here, only one of the two values can possibly be !=0.
                            let idx = 3 + delta_min + delta_max;

                            const SPEEDS: [CoordType; 7] = [-9, -3, -1, 0, 1, 3, 9];
                            let idx = idx.clamp(0, SPEEDS.len() as CoordType) as usize;
                            SPEEDS[idx]
                        }

                        let delta_x = calc(text.left, text.right, mouse.x);
                        let delta_y = calc(text.top, text.bottom, mouse.y);

                        tc.scroll_offset.x += delta_x;
                        tc.scroll_offset.y += delta_y;

                        if delta_x != 0 || delta_y != 0 {
                            self.tui.read_timeout = time::Duration::from_millis(25);
                        }
                    }
                }
            } else {
                match self.tui.mouse_state {
                    InputMouseState::Left => {
                        if self.input_mouse_modifiers.contains(kbmod::SHIFT) {
                            // TODO: Untested because Windows Terminal surprisingly doesn't support Shift+Click.
                            tb.selection_update_visual(pos);
                        } else {
                            tb.cursor_move_to_visual(pos);
                        }
                        tc.preferred_column = tb.get_cursor_visual_pos().x;
                        make_cursor_visible = true;
                    }
                    InputMouseState::Release => {
                        tc.scroll_offset_y_drag_start = CoordType::MIN;
                        tb.selection_finalize();
                    }
                    _ => return false,
                }
            }

            self.set_input_consumed();
            return make_cursor_visible;
        }

        if !tc.has_focus {
            return false;
        }

        if let Some(input) = &self.input_text {
            let mut text = input.text.as_bytes();
            if single_line {
                let (end, _) = ucd::newlines_forward(text, 0, 0, 1);
                text = ucd::strip_newline(&text[..end]);
            }

            tb.write(text, input.bracketed);

            tc.preferred_column = tb.get_cursor_visual_pos().x;
            self.set_input_consumed();
            return true;
        }

        if let Some(input) = &self.input_keyboard {
            let key = input.key();
            let modifiers = input.modifiers();
            let mut make_cursor_visible = true;

            match key {
                vk::BACK => {
                    let granularity = if modifiers == kbmod::CTRL {
                        CursorMovement::Word
                    } else {
                        CursorMovement::Grapheme
                    };
                    tb.delete(granularity, -1);
                }
                vk::TAB => {
                    if single_line {
                        // If this is just a simple input field, don't consume Tab (= early return).
                        return false;
                    }
                    tb.write(b"\t", false);
                }
                vk::RETURN => {
                    if single_line {
                        // If this is just a simple input field, don't consume Enter (= early return).
                        return false;
                    }
                    tb.write(b"\n", false);
                }
                vk::ESCAPE => {
                    // If there was a selection, clear it and show the cursor (= fallthrough).
                    if !tb.clear_selection() {
                        if single_line {
                            // If this is just a simple input field, don't consume the escape key
                            // (early return) and don't show the cursor (= return false).
                            return false;
                        }

                        // If this is a textarea, don't show the cursor if
                        // the escape key was pressed and nothing happened.
                        make_cursor_visible = false;
                    }
                }
                vk::PRIOR => {
                    let height = node_prev.inner.height();

                    // If the cursor was already on the first line,
                    // move it to the start of the buffer.
                    if tb.get_cursor_visual_pos().y == 0 {
                        tc.preferred_column = 0;
                    }

                    if modifiers == kbmod::SHIFT {
                        tb.selection_update_visual(Point {
                            x: tc.preferred_column,
                            y: tb.get_cursor_visual_pos().y - height,
                        });
                    } else {
                        tb.cursor_move_to_visual(Point {
                            x: tc.preferred_column,
                            y: tb.get_cursor_visual_pos().y - height,
                        });
                    }

                    tc.scroll_offset.y -= height;
                }
                vk::NEXT => {
                    let height = node_prev.inner.height();

                    // If the cursor was already on the last line,
                    // move it to the end of the buffer.
                    if tb.get_cursor_visual_pos().y >= tb.get_visual_line_count() - 1 {
                        tc.preferred_column = CoordType::MAX;
                    }

                    if modifiers == kbmod::SHIFT {
                        tb.selection_update_visual(Point {
                            x: tc.preferred_column,
                            y: tb.get_cursor_visual_pos().y + height,
                        });
                    } else {
                        tb.cursor_move_to_visual(Point {
                            x: tc.preferred_column,
                            y: tb.get_cursor_visual_pos().y + height,
                        });
                    }

                    if tc.preferred_column == CoordType::MAX {
                        tc.preferred_column = tb.get_cursor_visual_pos().x;
                    }

                    tc.scroll_offset.y += height;
                }
                vk::END => match modifiers {
                    kbmod::CTRL => tb.cursor_move_to_logical(Point::MAX),
                    kbmod::SHIFT => tb.selection_update_visual(Point {
                        x: CoordType::MAX,
                        y: tb.get_cursor_visual_pos().y,
                    }),
                    _ => tb.cursor_move_to_visual(Point {
                        x: CoordType::MAX,
                        y: tb.get_cursor_visual_pos().y,
                    }),
                },
                vk::HOME => match modifiers {
                    kbmod::CTRL => tb.cursor_move_to_logical(Point::default()),
                    kbmod::SHIFT => tb.selection_update_visual(Point {
                        x: 0,
                        y: tb.get_cursor_visual_pos().y,
                    }),
                    _ => tb.cursor_move_to_visual(Point {
                        x: 0,
                        y: tb.get_cursor_visual_pos().y,
                    }),
                },
                vk::LEFT => {
                    let granularity = if modifiers.contains(kbmod::CTRL) {
                        CursorMovement::Word
                    } else {
                        CursorMovement::Grapheme
                    };
                    if modifiers.contains(kbmod::SHIFT) {
                        tb.selection_update_delta(granularity, -1);
                    } else {
                        tb.cursor_move_delta(granularity, -1);
                    }
                }
                vk::UP => {
                    match modifiers {
                        kbmod::NONE => {
                            // If the cursor was already on the first line,
                            // move it to the start of the buffer.
                            if tb.get_cursor_visual_pos().y == 0 {
                                tc.preferred_column = 0;
                            }

                            tb.cursor_move_to_visual(Point {
                                x: tc.preferred_column,
                                y: tb.get_cursor_visual_pos().y - 1,
                            });
                        }
                        kbmod::CTRL => {
                            tc.scroll_offset.y -= 1;
                            make_cursor_visible = false;
                        }
                        kbmod::SHIFT => {
                            // If the cursor was already on the first line,
                            // move it to the start of the buffer.
                            if tb.get_cursor_visual_pos().y == 0 {
                                tc.preferred_column = 0;
                            }

                            tb.selection_update_visual(Point {
                                x: tc.preferred_column,
                                y: tb.get_cursor_visual_pos().y - 1,
                            });
                        }
                        kbmod::CTRL_ALT => {
                            // TODO: Add cursor above
                        }
                        _ => return false,
                    }
                }
                vk::RIGHT => {
                    let granularity = if modifiers.contains(kbmod::CTRL) {
                        CursorMovement::Word
                    } else {
                        CursorMovement::Grapheme
                    };
                    if modifiers.contains(kbmod::SHIFT) {
                        tb.selection_update_delta(granularity, 1);
                    } else {
                        tb.cursor_move_delta(granularity, 1);
                    }
                }
                vk::DOWN => match modifiers {
                    kbmod::NONE => {
                        // If the cursor was already on the last line,
                        // move it to the end of the buffer.
                        if tb.get_cursor_visual_pos().y >= tb.get_visual_line_count() - 1 {
                            tc.preferred_column = CoordType::MAX;
                        }

                        tb.cursor_move_to_visual(Point {
                            x: tc.preferred_column,
                            y: tb.get_cursor_visual_pos().y + 1,
                        });

                        if tc.preferred_column == CoordType::MAX {
                            tc.preferred_column = tb.get_cursor_visual_pos().x;
                        }
                    }
                    kbmod::CTRL => {
                        tc.scroll_offset.y += 1;
                        make_cursor_visible = false;
                    }
                    kbmod::SHIFT => {
                        // If the cursor was already on the last line,
                        // move it to the end of the buffer.
                        if tb.get_cursor_visual_pos().y >= tb.get_visual_line_count() - 1 {
                            tc.preferred_column = CoordType::MAX;
                        }

                        tb.selection_update_visual(Point {
                            x: tc.preferred_column,
                            y: tb.get_cursor_visual_pos().y + 1,
                        });

                        if tc.preferred_column == CoordType::MAX {
                            tc.preferred_column = tb.get_cursor_visual_pos().x;
                        }
                    }
                    kbmod::CTRL_ALT => {
                        // TODO: Add cursor above
                    }
                    _ => return false,
                },
                vk::INSERT => match modifiers {
                    kbmod::SHIFT => tb.write(&self.tui.clipboard, true),
                    kbmod::CTRL => self.tui.clipboard = tb.extract_selection(false),
                    _ => tb.set_overtype(!tb.is_overtype()),
                },
                vk::DELETE => match modifiers {
                    kbmod::SHIFT => self.tui.clipboard = tb.extract_selection(true),
                    kbmod::CTRL => tb.delete(CursorMovement::Word, 1),
                    _ => tb.delete(CursorMovement::Grapheme, 1),
                },
                vk::A => match modifiers {
                    kbmod::CTRL => tb.select_all(),
                    _ => return false,
                },
                vk::X => match modifiers {
                    kbmod::CTRL => self.tui.clipboard = tb.extract_selection(true),
                    _ => return false,
                },
                vk::C => match modifiers {
                    kbmod::CTRL => self.tui.clipboard = tb.extract_selection(false),
                    _ => return false,
                },
                vk::V => match modifiers {
                    kbmod::CTRL => tb.write(&self.tui.clipboard, true),
                    _ => return false,
                },
                vk::Y => match modifiers {
                    kbmod::CTRL => tb.redo(),
                    _ => return false,
                },
                vk::Z => match modifiers {
                    kbmod::CTRL => tb.undo(),
                    kbmod::CTRL_SHIFT => tb.redo(),
                    kbmod::ALT => tb.toggle_word_wrap(),
                    _ => return false,
                },
                _ => return false,
            }

            if !matches!(key, vk::PRIOR | vk::NEXT | vk::UP | vk::DOWN) {
                tc.preferred_column = tb.get_cursor_visual_pos().x;
            }

            self.set_input_consumed();
            return make_cursor_visible;
        }

        false
    }

    fn textarea_make_cursor_visible(&self, content: &mut TextareaContent, node_prev: &Node) {
        let tb = &mut *content.buffer;
        let mut scroll_x = content.scroll_offset.x;
        let mut scroll_y = content.scroll_offset.y;

        let text_width = tb.get_text_width();
        let cursor_x = tb.get_cursor_visual_pos().x;
        scroll_x = scroll_x.min(cursor_x);
        scroll_x = scroll_x.max(cursor_x - text_width + 1);

        let viewport_height = node_prev.inner.height();
        let cursor_y = tb.get_cursor_visual_pos().y;
        // Scroll up if the cursor is above the visible area.
        scroll_y = scroll_y.min(cursor_y);
        // Scroll down if the cursor is below the visible area.
        scroll_y = scroll_y.max(cursor_y - viewport_height + 1);

        content.scroll_offset.x = scroll_x;
        content.scroll_offset.y = scroll_y;
    }

    fn textarea_adjust_scroll_offset(&self, content: &mut TextareaContent) {
        let tb = &mut *content.buffer;
        let mut scroll_x = content.scroll_offset.x;
        let mut scroll_y = content.scroll_offset.y;

        if tb.is_word_wrap_enabled() {
            scroll_x = 0;
        }

        scroll_x = scroll_x.max(0);
        scroll_y = scroll_y.clamp(0, tb.get_visual_line_count() - 1);

        content.scroll_offset.x = scroll_x;
        content.scroll_offset.y = scroll_y;
    }

    pub fn scrollarea_begin(&mut self, classname: &'static str, intrinsic_size: Size) {
        self.block_begin(classname);

        let container = self.tree.last_node_mut();
        container.content = NodeContent::Scrollarea(ScrollareaContent {
            scroll_offset: Point::MIN,
            scroll_offset_y_drag_start: CoordType::MIN,
            thumb_height: 0,
        });

        if intrinsic_size.width > 0 || intrinsic_size.height > 0 {
            container.intrinsic_size.width = intrinsic_size.width.max(0);
            container.intrinsic_size.height = intrinsic_size.height.max(0);
            container.intrinsic_size_set = true;
        }

        self.block_begin("content");
        self.inherit_focus();

        // Ensure that attribute modifications apply to the outer container.
        self.tree.last_node = container;
    }

    pub fn scrollarea_scroll_to(&mut self, pos: Point) {
        let container = self.tree.last_node_mut();
        if let NodeContent::Scrollarea(sc) = &mut container.content {
            sc.scroll_offset = pos;
        } else {
            debug_assert!(false);
        }
    }

    pub fn scrollarea_end(&mut self) {
        self.block_end(); // content block

        let container = self.tree.current_node_mut();
        let NodeContent::Scrollarea(sc) = &mut container.content else {
            panic!();
        };

        if let Some(prev_container) = self.tui.get_prev_node(container.id) {
            if sc.scroll_offset == Point::MIN {
                if let NodeContent::Scrollarea(sc_prev) = &prev_container.content {
                    *sc = sc_prev.clone();
                }
            }

            if !self.input_consumed && self.tui.mouse_state != InputMouseState::None {
                let container_rect = prev_container.inner;

                if self.tui.mouse_is_drag {
                    // We don't need to look up the previous track node,
                    // since it has a fixed size based on the container size.
                    let track_rect = Rect {
                        left: container_rect.right,
                        top: container_rect.top,
                        right: container_rect.right + 1,
                        bottom: container_rect.bottom,
                    };
                    if track_rect.contains(self.tui.mouse_down_position) {
                        if sc.scroll_offset_y_drag_start == CoordType::MIN {
                            sc.scroll_offset_y_drag_start = sc.scroll_offset.y;
                        }

                        let content = Tree::node_ref(prev_container.children.first).unwrap();
                        let content_rect = content.inner;
                        let content_height = content_rect.height();
                        let track_height = track_rect.height();

                        if content_height > track_height {
                            let trackable = track_height - sc.thumb_height;
                            let scrollable_height = content_height - track_height;
                            let delta_y =
                                self.tui.mouse_position.y - self.tui.mouse_down_position.y;
                            sc.scroll_offset.y = sc.scroll_offset_y_drag_start
                                + ((delta_y * scrollable_height) / trackable);
                        }

                        self.set_input_consumed();
                    }
                } else {
                    match self.tui.mouse_state {
                        InputMouseState::Release => {
                            sc.scroll_offset_y_drag_start = CoordType::MIN;
                        }
                        InputMouseState::Scroll => {
                            if container_rect.contains(self.tui.mouse_position) {
                                sc.scroll_offset.x += self.input_scroll_delta.x;
                                sc.scroll_offset.y += self.input_scroll_delta.y;
                                self.set_input_consumed();
                            }
                        }
                        _ => {}
                    }
                }
            }
        }

        self.block_end(); // outer container
    }

    pub fn list_begin(&mut self, classname: &'static str) {
        self.block_begin(classname);
        self.attr_focusable();

        let last_node = self.tree.last_node_mut();
        let content = self
            .tui
            .get_prev_node(last_node.id)
            .and_then(|n| match &n.content {
                NodeContent::List(content) => Some(ListContent {
                    selected: content.selected,
                    selected_node: ptr::null(),
                }),
                _ => None,
            })
            .unwrap_or(ListContent {
                selected: 0,
                selected_node: ptr::null(),
            });

        last_node.attributes.focus_void = true;
        last_node.content = NodeContent::List(content);
    }

    pub fn list_item(&mut self, select: bool, overflow: Overflow, text: &str) -> ListSelection {
        let list = self.tree.current_node_mut();
        let content = match &mut list.content {
            NodeContent::List(content) => content,
            _ => panic!(),
        };
        let idx = list.child_count;

        self.next_block_id_mixin(hash_str(idx as u64, text));
        self.styled_label_begin("item", overflow);
        self.attr_focusable();

        let item = self.tree.last_node_ref();
        let item_id = item.id;
        let selected_before = content.selected == item_id;
        let focused = self.is_focused();

        // Inherit the default selection & Click changes selection
        let selected_now = selected_before || (select && content.selected == 0) || focused;

        // Note down the selected node for keyboard navigation.
        if selected_now {
            content.selected_node = item;
            if !selected_before {
                content.selected = item_id;
                self.needs_rerender();
            }
        }

        self.styled_label_add_text("  ");
        self.styled_label_add_text(text);
        self.styled_label_end();

        // Clicking an item activates it
        let clicked =
            !self.input_consumed && (self.input_mouse_is_double_click && self.is_hovering());
        // Pressing Enter on a selected item activates it as well
        let entered = focused
            && selected_before
            && !self.input_consumed
            && matches!(self.input_keyboard, Some(vk::RETURN));
        let activated = clicked || entered;
        if activated {
            self.set_input_consumed();
        }

        if selected_before && activated {
            ListSelection::Activated
        } else if selected_now && !selected_before {
            ListSelection::Selected
        } else {
            ListSelection::Unchanged
        }
    }

    pub fn list_end(&mut self) {
        self.block_end();

        let list = self.tree.last_node_mut();
        let content = match &mut list.content {
            NodeContent::List(content) => content,
            _ => panic!(),
        };

        let mut selected_next = content.selected_node;

        if content.selected_node.is_null() && !list.children.first.is_null() {
            selected_next = list.children.first;
        } else if !content.selected_node.is_null()
            && !self.input_consumed
            && (self.input_keyboard == Some(vk::UP) || self.input_keyboard == Some(vk::DOWN))
            && self.contains_focus()
        {
            let selected = Tree::node_ref(content.selected_node).unwrap();
            let forward = self.input_keyboard == Some(vk::DOWN);
            let mut node = if forward {
                selected.siblings.next
            } else {
                selected.siblings.prev
            };
            if node.is_null() {
                node = if forward {
                    list.children.first
                } else {
                    list.children.last
                };
            }
            selected_next = node;
        }

        let has_focus = self.tui.is_subtree_focused(list.id);
        let Some(node) = Tree::node_mut(selected_next) else {
            return;
        };

        // Now that we know which item is selected we can mark it as such.
        if let NodeContent::Text(content) = &mut node.content {
            unsafe {
                content.chunks[0].text.as_bytes_mut()[0] = b'>';
            }
        }

        // If the list has focus, we also delegate focus to the selected item and colorize it.
        if has_focus {
            node.attributes.bg = self.indexed(IndexedColor::Green);
            self.steal_focus_for(node);
        }
    }

    pub fn menubar_begin(&mut self) {
        self.table_begin("menubar");
        self.table_next_row();
    }

    pub fn menubar_menu_begin(&mut self, text: &str, accelerator: char) -> bool {
        let row = self.tree.current_node_ref();

        self.next_block_id_mixin(row.child_count as u64);
        self.menubar_label(text, accelerator);
        self.attr_focusable();
        self.attr_padding(Rect::two(0, 1));

        if self.consume_shortcut(kbmod::ALT | InputKey::new(accelerator as u32)) {
            self.steal_focus();
        }

        if self.contains_focus() {
            if self.consume_shortcut(vk::ESCAPE) {
                // TODO: This should reassign the previous focused path.
                self.needs_rerender();
                self.tui.focused_node_path.clear();
                self.tui.focused_node_path.push(ROOT_ID);
                return false;
            }

            self.attr_background_rgba(self.indexed(IndexedColor::Green));

            self.table_begin("flyout");
            self.attr_float(FloatSpec {
                anchor: Anchor::Last,
                gravity_x: 0.0,
                gravity_y: 0.0,
                offset_x: 0,
                offset_y: 1,
            });
            self.attr_border();
            self.attr_background_rgba(self.indexed(IndexedColor::White));
            self.attr_foreground_rgba(self.indexed(IndexedColor::Black));
            return true;
        }

        false
    }

    pub fn menubar_menu_item(&mut self, text: &str, accelerator: char, shortcut: InputKey) -> bool {
        self.table_next_row();
        self.attr_focusable();
        if self.is_focused() {
            self.attr_background_rgba(self.indexed(IndexedColor::Green));
        }

        let clicked =
            self.button_activated() || self.consume_shortcut(InputKey::new(accelerator as u32));

        self.menubar_label(text, accelerator);
        self.menubar_shortcut(shortcut);

        if clicked {
            // TODO: This should reassign the previous focused path.
            self.needs_rerender();
            self.tui.focused_node_path.clear();
            self.tui.focused_node_path.push(ROOT_ID);
        }

        clicked
    }

    pub fn menubar_menu_end(&mut self) {
        self.table_end();
    }

    pub fn menubar_end(&mut self) {
        let menu_row = self.tree.current_node_ref();
        self.table_end();

        let focus_path = &self.tui.focused_node_path[..];
        if self.input_consumed
            || focus_path.len() < 4
            || focus_path[focus_path.len() - 3] != menu_row.id
        {
            return;
        }
        let Some(input) = self.input_keyboard else {
            return;
        };
        if !matches!(input, vk::LEFT | vk::RIGHT | vk::UP | vk::DOWN) {
            return;
        }

        let container;
        let element_id;

        if input == vk::LEFT || input == vk::RIGHT {
            container = menu_row;
            element_id = focus_path[focus_path.len() - 4];
        } else {
            let flyout = unsafe { self.tree.root_last.as_ref().unwrap() };
            container = flyout;
            element_id = if focus_path.len() == 6 && focus_path[focus_path.len() - 5] == flyout.id {
                focus_path[focus_path.len() - 6]
            } else {
                0
            };
        }

        // In an unnested menu like ours, going up/left and down/right respectively is the same.
        // The only thing that changes is the layout direction, which we don't care about.
        let focused_node = Tree::iterate_siblings(container.children.first)
            .find(|node| node.id == element_id)
            .and_then(|node| {
                if input == vk::LEFT || input == vk::UP {
                    Tree::node_ref(node.siblings.prev)
                } else {
                    Tree::node_ref(node.siblings.next)
                }
            })
            .or_else(|| {
                if input == vk::LEFT || input == vk::UP {
                    Tree::node_ref(container.children.last)
                } else {
                    Tree::node_ref(container.children.first)
                }
            })
            .unwrap();

        Tui::build_node_path(Some(focused_node), &mut self.tui.focused_node_path);
        self.needs_rerender();

        self.set_input_consumed();
    }

    fn menubar_label(&mut self, text: &str, accelerator: char) {
        if !accelerator.is_ascii_uppercase() {
            self.label("label", Overflow::Clip, text);
            return;
        }

        let mut off = text.len();

        for (i, c) in text.bytes().enumerate() {
            // Perfect match (uppercase character) --> stop
            if c as char == accelerator {
                off = i;
                break;
            }
            // Inexact match (lowercase character) --> use first hit
            if (c & !0x20) as char == accelerator && off == text.len() {
                off = i;
            }
        }

        self.styled_label_begin("label", Overflow::Clip);

        if off < text.len() {
            // Highlight the accelerator in red.
            self.styled_label_add_text(&text[..off]);
            self.styled_label_set_foreground_indexed(Some(IndexedColor::BrightRed));
            self.styled_label_add_text(&text[off..off + 1]);
            self.styled_label_set_foreground_indexed(None);
            self.styled_label_add_text(&text[off + 1..]);
        } else {
            // Add the accelerator in parentheses (still in red).
            let ch = accelerator as u8;
            self.styled_label_add_text(text);
            self.styled_label_add_text("(");
            self.styled_label_set_foreground_indexed(Some(IndexedColor::BrightRed));
            self.styled_label_add_text(unsafe { helpers::str_from_raw_parts(&ch, 1) });
            self.styled_label_set_foreground_indexed(None);
            self.styled_label_add_text(")");
        }

        self.styled_label_end();
        self.attr_padding(Rect::two(0, 1));
    }

    fn menubar_shortcut(&mut self, shortcut: InputKey) {
        let shortcut_letter = shortcut.value() as u8 as char;
        if shortcut_letter.is_ascii_uppercase() {
            let mut shortcut_text = String::new();
            if shortcut.modifiers_contains(kbmod::CTRL) {
                shortcut_text.push_str("Ctrl+");
            }
            if shortcut.modifiers_contains(kbmod::ALT) {
                shortcut_text.push_str("Alt+");
            }
            if shortcut.modifiers_contains(kbmod::SHIFT) {
                shortcut_text.push_str("Shift+");
            }
            shortcut_text.push(shortcut_letter);

            self.label("shortcut", Overflow::Clip, &shortcut_text);
            self.attr_padding(Rect::two(0, 1));
        } else {
            self.block_begin("shortcut");
            self.block_end();
        }
    }
}

enum VisitControl {
    Continue,
    SkipChildren,
    Stop,
}

struct Tree {
    tail: *const Node,
    root_first: *const Node,
    root_last: *const Node,
    last_node: *const Node,
    current_node: *const Node,
    count: usize,
    checksum: u64,
}

impl Tree {
    fn new() -> Self {
        let mut tree = Self::default();
        tree.append_child(Node {
            id: ROOT_ID,
            classname: "root",
            attributes: Attributes {
                focus_well: true,
                ..Default::default()
            },
            ..Default::default()
        });
        tree.root_first = tree.tail;
        tree.root_last = tree.tail;
        tree.last_node = tree.tail;
        tree.current_node = tree.tail;
        tree
    }

    fn append_child(&mut self, node: Node) {
        let node = Box::leak(Box::new(node));

        if let Some(parent) = Tree::node_mut(self.current_node) {
            parent.append_child(node);
        }

        node.prev = self.tail;
        if let Some(tail) = Tree::node_mut(self.tail) {
            tail.next = node;
        }
        self.tail = node;

        self.last_node = node;
        self.current_node = node;
        self.count += 1;
        // wymix is weak, but both checksum and node.id are proper random, so... it's not *that* bad.
        self.checksum = wymix(self.checksum, node.id);
    }

    fn pop_stack(&mut self) {
        let current_node = self.current_node_ref();
        self.last_node = current_node;
        self.current_node = current_node.stack_parent;
    }

    fn last_node_ref<'a>(&self) -> &'a Node {
        debug_assert!(!self.last_node.is_null());
        unsafe { &*(self.last_node as *const _) }
    }

    fn last_node_mut<'a>(&self) -> &'a mut Node {
        debug_assert!(!self.last_node.is_null());
        unsafe { &mut *(self.last_node as *mut _) }
    }

    fn current_node_ref<'a>(&self) -> &'a Node {
        debug_assert!(!self.current_node.is_null());
        unsafe { &*(self.current_node as *const _) }
    }

    fn current_node_mut<'a>(&self) -> &'a mut Node {
        debug_assert!(!self.current_node.is_null());
        unsafe { &mut *(self.current_node as *mut _) }
    }

    fn node_ref<'a>(node: *const Node) -> Option<&'a Node> {
        unsafe { node.as_ref() }
    }

    // This (and node_ref) are unsafe, unsound, and whatever else you want to call it.
    // But there was major time crunch and Rust is such a pain in the ass when it comes to building trees.
    // I used RefCell first and that was just absolutely awful.
    fn node_mut<'a>(node: *const Node) -> Option<&'a mut Node> {
        unsafe { (node as *mut Node).as_mut() }
    }

    fn iterate_siblings<'a>(mut node: *const Node) -> impl Iterator<Item = &'a mut Node> {
        iter::from_fn(move || {
            if node.is_null() {
                None
            } else {
                let n = unsafe { &mut *(node as *mut Node) };
                node = n.siblings.next;
                Some(n)
            }
        })
    }

    fn iterate_roots<'a>(&self) -> impl Iterator<Item = &'a mut Node> + use<'a> {
        Self::iterate_siblings(self.root_first)
    }

    /// Visits all nodes under and including `root` in depth order.
    /// Starts with node `start`.
    ///
    /// WARNING: Breaks in hilarious ways if `start` is not within `root`.
    #[inline]
    fn visit_all<T: FnMut(usize, &Node) -> VisitControl>(
        root: *const Node,
        start: *const Node,
        mut depth: usize,
        forward: bool,
        mut cb: T,
    ) {
        if root.is_null() || start.is_null() {
            return;
        }

        let mut node = unsafe { &*start };
        let children_idx = if forward {
            NodeChildren::FIRST
        } else {
            NodeChildren::LAST
        };
        let siblings_idx = if forward {
            NodeSiblings::NEXT
        } else {
            NodeSiblings::PREV
        };

        while {
            'traverse: {
                match cb(depth, node) {
                    VisitControl::Continue => {
                        // Depth first search: It has a child? Go there.
                        if !node.children.get(children_idx).is_null() {
                            node = unsafe { &*node.children.get(children_idx) };
                            depth += 1;
                            break 'traverse;
                        }
                    }
                    VisitControl::SkipChildren => {}
                    VisitControl::Stop => return,
                }

                // Out of children? Go back to the parent.
                while node.siblings.get(siblings_idx).is_null() && !ptr::eq(node, root) {
                    node = unsafe { &*node.parent };
                    depth -= 1;
                }

                // If `start != root`, this ensures we restart the traversal at `root` until we hit `start` again.
                // Otherwise, this will continue above, hit the if condition, and break out of the loop.
                if ptr::eq(node, root) {
                    break 'traverse;
                }

                // Go to the parent's next sibling. --> Next subtree.
                node = unsafe { &*node.siblings.get(siblings_idx) };
            }

            // We're done once we wrapped around to the `start`.
            !ptr::eq(node, start)
        } {}
    }
}

impl Drop for Tree {
    fn drop(&mut self) {
        let mut node = self.root_first;
        while !node.is_null() {
            let next = unsafe { (*node).next };
            unsafe {
                let _ = Box::from_raw(node as *mut Node);
            }
            node = next;
        }
    }
}

impl Default for Tree {
    fn default() -> Self {
        Self {
            tail: null(),
            root_first: null(),
            root_last: null(),
            last_node: null(),
            current_node: null(),
            count: 0,
            checksum: 0,
        }
    }
}

#[derive(Default)]
pub enum Anchor {
    #[default]
    Last,
    Parent,
    Root,
}

#[derive(Default)]
pub struct FloatSpec {
    pub anchor: Anchor,
    // Specifies the origin of the container relative to the container size. [0, 1]
    pub gravity_x: f32,
    pub gravity_y: f32,
    // Specifies an offset from the origin in cells.
    pub offset_x: CoordType,
    pub offset_y: CoordType,
}

struct FloatAttributes {
    // Specifies the origin of the container relative to the container size. [0, 1]
    gravity_x: f32,
    gravity_y: f32,
    // Specifies an offset from the origin in cells.
    offset: Point,
}

#[derive(PartialEq, Eq)]
pub enum ListSelection {
    Unchanged,
    Selected,
    Activated,
}

#[derive(Default)]
pub enum Position {
    #[default]
    Stretch,
    Left,
    Center,
    Right,
}

#[derive(PartialEq, Eq, Default)]
pub enum Overflow {
    #[default]
    Clip,
    TruncateHead,
    TruncateMiddle,
    TruncateTail,
}

#[derive(Default)]
struct Attributes {
    float: Option<FloatAttributes>,
    position: Position,
    padding: Rect,
    bg: u32,
    fg: u32,
    bordered: bool,
    focusable: bool,
    focus_well: bool, // Prevents focus from leaving via Tab
    focus_void: bool, // Prevents focus from entering via Tab
    focus_brackets: bool,
}

struct ListContent {
    selected: u64,
    selected_node: *const Node, // Points to the Node that holds this ListContent instance, if any.
}

struct TableContent {
    columns: Vec<CoordType>,
    cell_gap: Size,
}

struct StyledTextChunk {
    text: String,
    fg: u32,
}

impl Document for Vec<StyledTextChunk> {
    fn read_backward(&self, mut off: usize) -> &[u8] {
        for chunk in self.iter().rev() {
            if off < chunk.text.len() {
                return &chunk.text.as_bytes()[chunk.text.len() - off..];
            }
            off -= chunk.text.len();
        }
        &[]
    }

    fn read_forward(&self, mut off: usize) -> &[u8] {
        for chunk in self.iter() {
            if off < chunk.text.len() {
                return &chunk.text.as_bytes()[off..];
            }
            off -= chunk.text.len();
        }
        &[]
    }
}

struct TextContent {
    chunks: Vec<StyledTextChunk>,
    overflow: Overflow,
}

struct TextareaContent {
    buffer: RcTextBuffer,
    scroll_offset: Point,
    scroll_offset_y_drag_start: CoordType,
    thumb_height: CoordType,
    preferred_column: CoordType,
    single_line: bool,
    has_focus: bool,
}

#[derive(Clone)]
struct ScrollareaContent {
    scroll_offset: Point,
    scroll_offset_y_drag_start: CoordType,
    thumb_height: CoordType,
}

#[derive(Default)]
enum NodeContent {
    #[default]
    None,
    List(ListContent),
    Modal(String), // title
    Table(TableContent),
    Text(TextContent),
    Textarea(TextareaContent),
    Scrollarea(ScrollareaContent),
}

struct NodeSiblings {
    prev: *const Node,
    next: *const Node,
}

impl NodeSiblings {
    const PREV: usize = 0;
    const NEXT: usize = 1;

    fn get(&self, off: usize) -> *const Node {
        match off & 1 {
            0 => self.prev,
            1 => self.next,
            _ => unreachable!(),
        }
    }
}

struct NodeChildren {
    first: *const Node,
    last: *const Node,
}

impl NodeChildren {
    const FIRST: usize = 0;
    const LAST: usize = 1;

    fn get(&self, off: usize) -> *const Node {
        match off & 1 {
            0 => self.first,
            1 => self.last,
            _ => unreachable!(),
        }
    }
}

pub struct Node {
    prev: *const Node,
    next: *const Node,
    stack_parent: *const Node,

    id: u64,
    classname: &'static str,
    parent: *const Node,
    siblings: NodeSiblings,
    children: NodeChildren,
    child_count: usize,

    attributes: Attributes,
    content: NodeContent,

    intrinsic_size: Size,
    intrinsic_size_set: bool,
    outer: Rect,         // in screen-space, calculated during layout
    inner: Rect,         // in screen-space, calculated during layout
    outer_clipped: Rect, // in screen-space, calculated during layout, restricted to the viewport
    inner_clipped: Rect, // in screen-space, calculated during layout, restricted to the viewport
}

impl Default for Node {
    fn default() -> Self {
        Node {
            prev: null(),
            next: null(),
            stack_parent: null(),

            id: 0,
            classname: "",
            parent: null(),
            siblings: NodeSiblings {
                prev: null(),
                next: null(),
            },
            children: NodeChildren {
                first: null(),
                last: null(),
            },
            child_count: 0,

            attributes: Default::default(),
            content: Default::default(),

            intrinsic_size: Default::default(),
            intrinsic_size_set: false,
            outer: Default::default(),
            inner: Default::default(),
            outer_clipped: Default::default(),
            inner_clipped: Default::default(),
        }
    }
}

impl Node {
    fn outer_to_inner(&self, mut outer: Rect) -> Rect {
        let l = self.attributes.bordered || self.attributes.focus_brackets;
        let t = self.attributes.bordered;
        let r = self.attributes.bordered
            || self.attributes.focus_brackets
            || matches!(self.content, NodeContent::Scrollarea(..));
        let b = self.attributes.bordered;

        outer.left += self.attributes.padding.left + l as CoordType;
        outer.top += self.attributes.padding.top + t as CoordType;
        outer.right -= self.attributes.padding.right + r as CoordType;
        outer.bottom -= self.attributes.padding.bottom + b as CoordType;
        outer
    }

    fn intrinsic_to_outer(&self) -> Size {
        let l = self.attributes.bordered || self.attributes.focus_brackets;
        let t = self.attributes.bordered;
        let r = self.attributes.bordered
            || self.attributes.focus_brackets
            || matches!(self.content, NodeContent::Scrollarea(..));
        let b = self.attributes.bordered;

        let mut size = self.intrinsic_size;
        size.width += self.attributes.padding.left
            + self.attributes.padding.right
            + l as CoordType
            + r as CoordType;
        size.height += self.attributes.padding.top
            + self.attributes.padding.bottom
            + t as CoordType
            + b as CoordType;
        size
    }

    fn compute_intrinsic_size(&mut self) {
        match &mut self.content {
            NodeContent::Table(spec) => {
                // Calculate each row's height and the maximum width of each of its columns.
                for row in Tree::iterate_siblings(self.children.first) {
                    let mut row_height = 0;

                    for (column, cell) in Tree::iterate_siblings(row.children.first).enumerate() {
                        cell.compute_intrinsic_size();

                        let size = cell.intrinsic_to_outer();

                        // If the spec.columns[] value is positive, it's an absolute width.
                        // Otherwise, it's a fraction of the remaining space.
                        //
                        // TODO: The latter is computed incorrectly.
                        // Example: If the items are "a","b","c" then the intrinsic widths are [1,1,1].
                        // If the column spec is [0,-3,-1], then this code assigns an intrinsic row
                        // width of 3, but it should be 5 (1+1+3), because the spec says that the
                        // last column (flexible 1/1) must be 3 times as wide as the 2nd one (1/3rd).
                        // It's not a big deal yet, because such functionality isn't needed just yet.
                        if column >= spec.columns.len() {
                            spec.columns.push(0);
                        }
                        spec.columns[column] = spec.columns[column].max(size.width);

                        row_height = row_height.max(size.height);
                    }

                    row.intrinsic_size.height = row_height;
                }

                // Assuming each column has the width of the widest cell in that column,
                // calculate the total width of the table.
                let total_gap_width =
                    spec.cell_gap.width * spec.columns.len().saturating_sub(1) as CoordType;
                let total_inner_width = spec.columns.iter().sum::<CoordType>() + total_gap_width;
                let mut total_width = 0;
                let mut total_height = 0;

                // Assign the total width to each row.
                for row in Tree::iterate_siblings(self.children.first) {
                    row.intrinsic_size.width = total_inner_width;
                    row.intrinsic_size_set = true;

                    let size = row.intrinsic_to_outer();
                    total_width = total_width.max(size.width);
                    total_height += size.height;
                }

                let total_gap_height =
                    spec.cell_gap.height * self.child_count.saturating_sub(1) as CoordType;
                total_height += total_gap_height;

                // Assign the total width/height to the table.
                self.intrinsic_size.width = total_width;
                self.intrinsic_size.height = total_height;
                self.intrinsic_size_set = true;
            }
            _ => {
                let mut max_width = 0;
                let mut total_height = 0;

                for child in Tree::iterate_siblings(self.children.first) {
                    child.compute_intrinsic_size();

                    let size = child.intrinsic_to_outer();
                    max_width = max_width.max(size.width);
                    total_height += size.height;
                }

                if !self.intrinsic_size_set {
                    self.intrinsic_size.width = max_width;
                    self.intrinsic_size.height = total_height;
                    self.intrinsic_size_set = true;
                }

                /*

                let mut row_size = Size {
                    width: 0,
                    height: 0,
                };
                let mut total_size = Size {
                    width: 0,
                    height: 0,
                };
                let columns = self.attributes.grid_columns.len().max(1);
                let mut column = 0;

                for child in Tree::iterate_siblings(self.children.first) {
                    child.compute_intrinsic_size();

                    let size = child.intrinsic_to_outer();
                    row_size.width += size.width;
                    row_size.height = row_size.height.max(size.height);

                    column += 1;
                    if column >= columns {
                        total_size.width = total_size.width.max(row_size.width);
                        total_size.height += row_size.height;
                        row_size = Size {
                            width: 0,
                            height: 0,
                        };
                        column = 0;
                    }
                }

                total_size.width = total_size.width.max(row_size.width);
                total_size.height += row_size.height;

                if !self.intrinsic_size_set {
                    self.intrinsic_size = total_size;
                    self.intrinsic_size_set = true;
                }
                */
            }
        }
    }

    fn layout_children(&mut self, clip: Rect) {
        if self.children.first.is_null() || self.inner.is_empty() {
            return;
        }

        match &mut self.content {
            NodeContent::Table(spec) => {
                let width = self.inner.right - self.inner.left;
                let mut x = self.inner.left;
                let mut y = self.inner.top;

                for row in Tree::iterate_siblings(self.children.first) {
                    let mut size = row.intrinsic_to_outer();
                    size.width = width;
                    row.outer.left = x;
                    row.outer.top = y;
                    row.outer.right = x + size.width;
                    row.outer.bottom = y + size.height;
                    row.outer = row.outer.intersect(self.inner);
                    row.inner = row.outer_to_inner(row.outer);
                    row.outer_clipped = row.outer.intersect(clip);
                    row.inner_clipped = row.inner.intersect(clip);

                    let mut row_height = 0;

                    for (column, cell) in Tree::iterate_siblings(row.children.first).enumerate() {
                        let mut size = cell.intrinsic_to_outer();
                        size.width = spec.columns[column];
                        cell.outer.left = x;
                        cell.outer.top = y;
                        cell.outer.right = x + size.width;
                        cell.outer.bottom = y + size.height;
                        cell.outer = cell.outer.intersect(self.inner);
                        cell.inner = cell.outer_to_inner(cell.outer);
                        cell.outer_clipped = cell.outer.intersect(clip);
                        cell.inner_clipped = cell.inner.intersect(clip);

                        x += size.width + spec.cell_gap.width;
                        row_height = row_height.max(size.height);

                        cell.layout_children(clip);
                    }

                    x = self.inner.left;
                    y += row_height + spec.cell_gap.height;
                }
            }
            NodeContent::Scrollarea(sc) => {
                let Some(content) = Tree::node_mut(self.children.first) else {
                    unreachable!();
                };

                // content available viewport size (-1 for the track)
                let sx = self.inner.right - self.inner.left;
                let sy = self.inner.bottom - self.inner.top;
                // actual content size
                let cx = sx;
                let cy = content.intrinsic_size.height.max(sy);
                // scroll offset
                let ox = 0;
                let oy = sc.scroll_offset.y.clamp(0, cy - sy);

                sc.scroll_offset.x = ox;
                sc.scroll_offset.y = oy;

                content.outer.left = self.inner.left - ox;
                content.outer.top = self.inner.top - oy;
                content.outer.right = content.outer.left + cx;
                content.outer.bottom = content.outer.top + cy;
                content.inner = content.outer_to_inner(content.outer);
                content.outer_clipped = content.outer.intersect(self.inner_clipped);
                content.inner_clipped = content.inner.intersect(self.inner_clipped);

                content.layout_children(content.inner_clipped);
            }
            _ => {
                let width = self.inner.right - self.inner.left;
                let x = self.inner.left;
                let mut y = self.inner.top;

                for child in Tree::iterate_siblings(self.children.first) {
                    let size = child.intrinsic_to_outer();
                    let remaining = (width - size.width).max(0);

                    child.outer.left = x + match child.attributes.position {
                        Position::Stretch | Position::Left => 0,
                        Position::Center => remaining / 2,
                        Position::Right => remaining,
                    };
                    child.outer.right = child.outer.left
                        + match child.attributes.position {
                            Position::Stretch => width,
                            _ => size.width,
                        };
                    child.outer.top = y;
                    child.outer.bottom = y + size.height;

                    child.outer = child.outer.intersect(self.inner);
                    child.inner = child.outer_to_inner(child.outer);
                    child.outer_clipped = child.outer.intersect(clip);
                    child.inner_clipped = child.inner.intersect(clip);

                    y += size.height;
                }

                for child in Tree::iterate_siblings(self.children.first) {
                    child.layout_children(clip);
                }

                /*

                let mut columns = &mut self.attributes.grid_columns[..];
                let mut default_width = 1;
                if columns.is_empty() {
                    columns = slice::from_mut(&mut default_width);
                }

                // TODO: We can skip this for nodes without a grid layout.
                let mut intrinsic_column_width = vec![0; columns.len()];
                let mut column = 0;

                for child in Tree::iterate_siblings(self.children.first) {
                    let size = child.intrinsic_to_outer();
                    intrinsic_column_width[column] = intrinsic_column_width[column].max(size.width);

                    column += 1;
                    if column >= columns.len() {
                        column = 0;
                    }
                }

                {
                    let mut total_abs_widths = 0;
                    let mut total_fr_widths = 0;

                    for i in 0..columns.len() {
                        if columns[i] > 0 {
                            total_fr_widths += columns[i];
                        } else {
                            total_abs_widths += intrinsic_column_width[i];
                        }
                    }

                    let mut fr_scale = 0.0;
                    if total_fr_widths > 0 {
                        let remaining = (self.inner.right - self.inner.left) - total_abs_widths;
                        let remaining = remaining.max(0);
                        // `unit` will be negative and invert the `grid_widths` each to a positive value.
                        fr_scale = remaining as f64 / total_fr_widths as f64;
                    }

                    for i in 0..columns.len() {
                        if columns[i] > 0 {
                            columns[i] = (columns[i] as f64 * fr_scale + 0.5) as CoordType;
                        } else {
                            columns[i] = intrinsic_column_width[i];
                        }
                    }
                }

                let mut x = self.inner.left;
                let mut y = self.inner.top;
                let mut row_height = 0;
                let mut column = 0;

                for child in Tree::iterate_siblings(self.children.first) {
                    let mut size = child.intrinsic_to_outer();
                    size.width = columns[column];

                    child.outer.left = x;
                    child.outer.top = y;
                    child.outer.right = x + size.width;
                    child.outer.bottom = y + size.height;
                    child.outer = child.outer.intersect(self.inner);
                    child.inner = child.outer_to_inner(child.outer);
                    child.outer_clipped = child.outer.intersect(clip);
                    child.inner_clipped = child.inner.intersect(clip);

                    x += size.width;
                    row_height = row_height.max(size.height);
                    column += 1;

                    if column >= columns.len() {
                        x = self.inner.left;
                        y += row_height;
                        row_height = 0;
                        column = 0;
                    }
                }

                for child in Tree::iterate_siblings(self.children.first) {
                    child.layout_children(clip);
                }
                */
            }
        }
    }

    fn append_child(&mut self, child: &mut Self) {
        // The child node is supposed to not be part of any tree.
        assert!(child.siblings.prev.is_null() && child.siblings.next.is_null());

        child.parent = self;
        child.siblings.prev = self.children.last;

        if let Some(child_last) = Tree::node_mut(self.children.last) {
            child_last.siblings.next = child;
        }
        if self.children.first.is_null() {
            self.children.first = child;
        }
        self.children.last = child;
        self.child_count += 1;
    }

    fn remove_from_parent(&mut self) {
        let Some(parent) = Tree::node_mut(self.parent) else {
            return;
        };

        if let Some(sibling_prev) = Tree::node_mut(self.siblings.prev) {
            sibling_prev.siblings.next = self.siblings.next;
        }
        if let Some(sibling_next) = Tree::node_mut(self.siblings.next) {
            sibling_next.siblings.prev = self.siblings.prev;
        }
        if ptr::eq(parent.children.first, self) {
            parent.children.first = self.siblings.next;
        }
        if ptr::eq(parent.children.last, self) {
            parent.children.last = self.siblings.prev;
        }
        parent.child_count -= 1;

        self.parent = null();
        self.siblings.prev = null();
        self.siblings.next = null();
    }
}
