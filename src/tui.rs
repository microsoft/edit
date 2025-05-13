use std::arch::breakpoint;
#[cfg(debug_assertions)]
use std::collections::HashSet;
use std::fmt::Write as _;
use std::mem::MaybeUninit;
use std::{iter, mem, ptr, time};

use crate::arena::{Arena, ArenaString, scratch_arena};
use crate::buffer::{CursorMovement, RcTextBuffer, TextBuffer, TextBufferCell};
use crate::cell::*;
use crate::framebuffer::{Attributes, Framebuffer, INDEXED_COLORS_COUNT, IndexedColor};
use crate::helpers::*;
use crate::input::{InputKeyMod, kbmod, vk};
use crate::ucd::WriteableDocument;
use crate::{apperr, arena_format, input, ucd};

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

enum TextBufferPayload<'a> {
    Editline(&'a mut dyn WriteableDocument),
    Textarea(RcTextBuffer),
}

pub struct ModifierTranslations {
    pub ctrl: &'static str,
    pub alt: &'static str,
    pub shift: &'static str,
}

pub struct Tui {
    framebuffer: Framebuffer,
    read_timeout: time::Duration,

    modifier_translations: ModifierTranslations,
    floater_default_bg: u32,
    floater_default_fg: u32,
    modal_default_bg: u32,
    modal_default_fg: u32,

    /// Last known terminal size.
    size: Size,
    /// Last known mouse position.
    mouse_position: Point,
    /// Between mouse down and up, the position where the mouse was pressed.
    /// Otherwise, this contains Point::MIN.
    mouse_down_position: Point,
    left_mouse_down_target: u64,
    mouse_up_timestamp: std::time::Instant,
    mouse_state: InputMouseState,
    mouse_is_drag: bool,
    mouse_click_counter: CoordType,
    first_click_position: Point,
    first_click_target: u64,

    clipboard: Vec<u8>,
    clipboard_generation: u32,
    cached_text_buffers: Vec<CachedTextBuffer>,
    mouse_down_node_path: Vec<u64>,
    focused_node_path: Vec<u64>,
    focused_node_for_scrolling: u64,

    arena_prev: Arena,
    arena_next: Arena,

    prev_tree: Tree<'static>,
    prev_node_map: NodeMap<'static>,

    settling_have: i32,
    settling_want: i32,
}

impl Tui {
    pub fn new() -> apperr::Result<Self> {
        let arena_prev = Arena::new(128 * MEBI)?;
        let arena_next = Arena::new(128 * MEBI)?;
        // SAFETY: Since `prev_tree` refers to `arena_prev`/`arena_next`, from its POV the lifetime
        // is `'static`, requiring us to use `transmute` to circumvent the borrow checker.
        let prev_tree = Tree::new(unsafe { mem::transmute::<&Arena, &Arena>(&arena_next) });

        let mut tui = Self {
            framebuffer: Framebuffer::new(),
            read_timeout: time::Duration::MAX,

            modifier_translations: ModifierTranslations {
                ctrl: "Ctrl",
                alt: "Alt",
                shift: "Shift",
            },
            floater_default_bg: 0,
            floater_default_fg: 0,
            modal_default_bg: 0,
            modal_default_fg: 0,

            size: Size { width: 0, height: 0 },
            mouse_position: Point::MIN,
            mouse_down_position: Point::MIN,
            left_mouse_down_target: 0,
            mouse_up_timestamp: std::time::Instant::now(),
            mouse_state: InputMouseState::None,
            mouse_is_drag: false,
            mouse_click_counter: 0,
            first_click_position: Point::MIN,
            first_click_target: 0,

            clipboard: Vec::new(),
            clipboard_generation: 0,
            cached_text_buffers: Vec::with_capacity(16),
            mouse_down_node_path: Vec::with_capacity(16),
            focused_node_path: Vec::with_capacity(16),
            focused_node_for_scrolling: ROOT_ID,

            arena_prev,
            arena_next,

            prev_tree,
            prev_node_map: Default::default(),

            settling_have: 0,
            settling_want: 0,
        };
        tui.mouse_down_node_path.push(ROOT_ID);
        tui.focused_node_path.push(ROOT_ID);
        Ok(tui)
    }

    pub fn size(&self) -> Size {
        self.size
    }

    pub fn setup_indexed_colors(&mut self, colors: [u32; INDEXED_COLORS_COUNT]) {
        self.framebuffer.set_indexed_colors(colors);
    }

    pub fn setup_modifier_translations(&mut self, translations: ModifierTranslations) {
        self.modifier_translations = translations;
    }

    pub fn set_floater_default_bg(&mut self, color: u32) {
        self.floater_default_bg = color;
    }

    pub fn set_floater_default_fg(&mut self, color: u32) {
        self.floater_default_fg = color;
    }

    pub fn set_modal_default_bg(&mut self, color: u32) {
        self.modal_default_bg = color;
    }

    pub fn set_modal_default_fg(&mut self, color: u32) {
        self.modal_default_fg = color;
    }

    pub fn read_timeout(&mut self) -> time::Duration {
        mem::replace(&mut self.read_timeout, time::Duration::MAX)
    }

    #[inline]
    pub fn indexed(&self, index: IndexedColor) -> u32 {
        self.framebuffer.indexed(index)
    }

    #[inline]
    pub fn indexed_alpha(&self, index: IndexedColor, alpha: u8) -> u32 {
        self.framebuffer.indexed_alpha(index, alpha)
    }

    pub fn contrasted(&self, color: u32) -> u32 {
        self.framebuffer.contrasted(color)
    }

    pub fn get_clipboard(&self) -> &[u8] {
        &self.clipboard
    }

    pub fn get_clipboard_generation(&self) -> u32 {
        self.clipboard_generation
    }

    pub fn create_context<'a, 'input>(
        &'a mut self,
        input: Option<input::Input<'input>>,
    ) -> Context<'a, 'input> {
        // SAFETY: Since we have a unique `&mut self`, nothing is holding onto `arena_prev`,
        // which will become `arena_next` and get reset. It's safe to reset and reuse its memory.
        mem::swap(&mut self.arena_prev, &mut self.arena_next);
        unsafe { self.arena_next.reset(0) };

        // In the input handler below we transformed a mouse up into a release event.
        // Now, a frame later, we must reset it back to none, to stop it from triggering things.
        // Same for Scroll events.
        if self.mouse_state > InputMouseState::Right {
            self.mouse_down_position = Point::MIN;
            self.left_mouse_down_target = 0;
            self.mouse_state = InputMouseState::None;
            self.mouse_is_drag = false;
        }

        if self.scroll_to_focused() {
            self.needs_more_settling();
        }

        let now = std::time::Instant::now();
        let mut input_text = None;
        let mut input_keyboard = None;
        let mut input_mouse_modifiers = kbmod::NONE;
        let mut input_mouse_click = 0;
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
                let mouse_down = self.mouse_state == InputMouseState::None
                    && next_state != InputMouseState::None;
                let mouse_up = self.mouse_state != InputMouseState::None
                    && next_state == InputMouseState::None;
                let is_drag = self.mouse_state == InputMouseState::Left
                    && next_state == InputMouseState::Left
                    && next_position != self.mouse_position;

                let mut hovered_node = None; // Needed for `mouse_down`
                let mut focused_node = None; // Needed for `mouse_down` and `is_click`
                if mouse_down || mouse_up {
                    for root in self.prev_tree.iterate_roots() {
                        Tree::visit_all(root, root, true, |node| {
                            let n = node.borrow();
                            if !n.outer_clipped.contains(next_position) {
                                // Skip the entire sub-tree, because it doesn't contain the cursor.
                                return VisitControl::SkipChildren;
                            }
                            hovered_node = Some(node);
                            if n.attributes.focusable {
                                focused_node = Some(node);
                            }
                            VisitControl::Continue
                        });
                    }
                }

                if mouse_down {
                    // Transition from no mouse input to some mouse input --> Record the mouse down position.
                    Self::build_node_path(hovered_node, &mut self.mouse_down_node_path);

                    // On left-mouse-down we change focus.
                    let mut target = 0;
                    if next_state == InputMouseState::Left {
                        target = focused_node.map_or(0, |n| n.borrow().id);
                        Self::build_node_path(focused_node, &mut self.focused_node_path);
                        self.needs_more_settling(); // See `needs_more_settling()`.
                    }

                    // Double-/Triple-/Etc.-clicks are triggered on mouse-down,
                    // unlike the first initial click, which is triggered on mouse-up.
                    if self.mouse_click_counter != 0 {
                        if self.first_click_target != target
                            || self.first_click_position != next_position
                            || (now - self.mouse_up_timestamp)
                                > std::time::Duration::from_millis(500)
                        {
                            // If the cursor moved / the focus changed in between, or if the user did a slow click,
                            // we reset the click counter. On mouse-up it'll transition to a regular click.
                            self.mouse_click_counter = 0;
                            self.first_click_position = Point::MIN;
                            self.first_click_target = 0;
                        } else {
                            self.mouse_click_counter += 1;
                            input_mouse_click = self.mouse_click_counter;
                        };
                    }

                    // Gets reset at the start of this function.
                    self.left_mouse_down_target = target;
                    self.mouse_down_position = next_position;
                } else if mouse_up {
                    // Transition from some mouse input to no mouse input --> The mouse button was released.
                    next_state = InputMouseState::Release;

                    let target = focused_node.map_or(0, |n| n.borrow().id);

                    if self.left_mouse_down_target == 0 || self.left_mouse_down_target != target {
                        // If `left_mouse_down_target == 0`, then it wasn't a left-click, in which case
                        // the target gets reset. Same, if the focus changed in between any clicks.
                        self.mouse_click_counter = 0;
                        self.first_click_position = Point::MIN;
                        self.first_click_target = 0;
                    } else if self.mouse_click_counter == 0 {
                        // No focus change, and no previous clicks? This is an initial, regular click.
                        self.mouse_click_counter = 1;
                        self.first_click_position = self.mouse_down_position;
                        self.first_click_target = target;
                        input_mouse_click = 1;
                    }

                    self.mouse_up_timestamp = now;
                } else if is_drag {
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

        // TODO: There should be a way to do this without unsafe.
        // Allocating from the arena borrows the arena, and so allocating the tree here borrows self.
        // This conflicts with us passing a mutable reference to `self` into the struct below.
        let tree = Tree::new(unsafe { mem::transmute::<&Arena, &Arena>(&self.arena_next) });

        Context {
            tui: self,

            input_text,
            input_keyboard,
            input_mouse_modifiers,
            input_mouse_click,
            input_scroll_delta,
            input_consumed,

            tree,
            last_modal: None,
            next_block_id_mixin: 0,
            needs_settling: false,

            #[cfg(debug_assertions)]
            seen_ids: HashSet::new(),
        }
    }

    fn report_context_completion<'a>(&'a mut self, ctx: &mut Context<'a, '_>) {
        // If this hits, you forgot to block_end() somewhere. The best way to figure
        // out where is to do a binary search of commenting out code in main.rs.
        debug_assert!(ctx.tree.current_node.borrow().stack_parent.is_none());

        if let Some(node) = ctx.last_modal
            && !self.is_subtree_focused(&node.borrow())
        {
            ctx.steal_focus_for(node);
        }

        // If nodes have appeared or disappeared, we need to re-render.
        // Same, if the focus has changed (= changes the highlight color, etc.).
        let mut needs_settling = ctx.needs_settling;
        needs_settling |= self.prev_tree.checksum != ctx.tree.checksum;

        // Adopt the new tree and recalculate the node hashmap.
        //
        // SAFETY: The memory used by the tree is owned by the `self.arena_next` right now.
        // Stealing the tree here thus doesn't need to copy any memory unless someone resets the arena.
        // (The arena is reset in `reset()` above.)
        unsafe {
            self.prev_tree = mem::transmute_copy(&ctx.tree);
            self.prev_node_map = NodeMap::new(mem::transmute(&self.arena_next), &self.prev_tree);
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
        if !focus_path_changed
            && !ctx.input_consumed
            && let Some(input) = ctx.input_keyboard
        {
            needs_settling |= self.move_focus(input);
        }

        if needs_settling {
            self.needs_more_settling();
        }

        self.settling_have += 1;

        // Remove cached text editors that are no longer in use.
        self.cached_text_buffers.retain(|c| c.seen);

        for root in Tree::iterate_siblings(Some(self.prev_tree.root_first)) {
            let mut root = root.borrow_mut();
            root.compute_intrinsic_size();
        }

        let viewport = self.size.as_rect();

        for root in Tree::iterate_siblings(Some(self.prev_tree.root_first)) {
            let mut root = root.borrow_mut();
            let root = &mut *root;

            if let Some(float) = &root.attributes.float {
                let mut x = 0;
                let mut y = 0;

                if let Some(node) = root.parent {
                    let node = node.borrow();
                    x = node.outer.left;
                    y = node.outer.top;
                }

                let size = root.intrinsic_to_outer();

                x += (float.offset_x - float.gravity_x * size.width as f32) as CoordType;
                y += (float.offset_y - float.gravity_y * size.height as f32) as CoordType;

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

            let outer = root.outer;
            root.layout_children(outer);
        }
    }

    fn build_node_path(node: Option<&NodeCell>, path: &mut Vec<u64>) {
        path.clear();
        if let Some(mut node) = node {
            loop {
                let n = node.borrow();
                path.push(n.id);
                node = match n.parent {
                    Some(parent) => parent,
                    None => break,
                };
            }
            path.reverse();
        } else {
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
        if cfg!(debug_assertions) && self.settling_have == 15 {
            breakpoint();
        }
        self.settling_want = (self.settling_have + 1).min(20);
    }

    /// Renders all nodes into a string-frame representation.
    pub fn render<'a>(&mut self, arena: &'a Arena) -> ArenaString<'a> {
        self.framebuffer.reset(self.size);
        for child in self.prev_tree.iterate_roots() {
            let mut child = child.borrow_mut();
            self.render_node(&mut child);
        }
        self.framebuffer.render(arena)
    }

    /// Recursively renders each node and its children.
    #[allow(clippy::only_used_in_recursion)]
    fn render_node(&mut self, node: &mut Node) {
        let outer_clipped = node.outer_clipped;
        if outer_clipped.is_empty() {
            return;
        }

        let scratch = scratch_arena(None);

        if node.attributes.bordered {
            // ┌────┐
            {
                let mut fill = ArenaString::new_in(&scratch);
                fill.push('┌');
                fill.push_repeat('─', (outer_clipped.right - outer_clipped.left - 2) as usize);
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
                let mut fill = ArenaString::new_in(&scratch);
                fill.push('│');
                fill.push_repeat(' ', (outer_clipped.right - outer_clipped.left - 2) as usize);
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
                let mut fill = ArenaString::new_in(&scratch);
                fill.push('└');
                fill.push_repeat('─', (outer_clipped.right - outer_clipped.left - 2) as usize);
                fill.push('┘');
                self.framebuffer.replace_text(
                    outer_clipped.bottom - 1,
                    outer_clipped.left,
                    outer_clipped.right,
                    &fill,
                );
            }
        }

        if node.attributes.float.is_some() && node.attributes.bg & 0xff000000 == 0xff000000 {
            if !node.attributes.bordered {
                let mut fill = ArenaString::new_in(&scratch);
                fill.push_repeat(' ', (outer_clipped.right - outer_clipped.left) as usize);

                for y in outer_clipped.top..outer_clipped.bottom {
                    self.framebuffer.replace_text(
                        y,
                        outer_clipped.left,
                        outer_clipped.right,
                        &fill,
                    );
                }
            }

            self.framebuffer.replace_attr(outer_clipped, Attributes::All, Attributes::None);
        }

        self.framebuffer.blend_bg(outer_clipped, node.attributes.bg);
        self.framebuffer.blend_fg(outer_clipped, node.attributes.fg);

        if node.attributes.reverse {
            self.framebuffer.reverse(outer_clipped);
        }

        let inner = node.inner;
        let inner_clipped = node.inner_clipped;
        if inner_clipped.is_empty() {
            return;
        }

        match &mut node.content {
            NodeContent::Modal(title) => {
                if !title.is_empty() {
                    self.framebuffer.replace_text(
                        node.outer.top,
                        node.outer.left + 2,
                        node.outer.right - 1,
                        title,
                    );
                }
            }
            NodeContent::Text(content) => self.render_styled_text(
                inner,
                node.intrinsic_size.width,
                &content.text,
                &content.chunks,
                content.overflow,
            ),
            NodeContent::Textarea(tc) => {
                let mut tb = tc.buffer.borrow_mut();
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

                if let Some(res) =
                    tb.render(tc.scroll_offset, destination, tc.has_focus, &mut self.framebuffer)
                {
                    tc.scroll_offset_x_max = res.visual_pos_x_max;
                }

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
                let content = node.children.first.unwrap().borrow();
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
            let mut child = child.borrow_mut();
            self.render_node(&mut child);
        }
    }

    fn render_styled_text(
        &mut self,
        target: Rect,
        actual_width: CoordType,
        text: &str,
        chunks: &[StyledTextChunk],
        overflow: Overflow,
    ) {
        let target_width = target.width();
        // The section of `text` that is skipped by the ellipsis.
        let mut skipped = 0..0;
        // The number of columns skipped by the ellipsis.
        let mut skipped_cols = 0;

        if overflow == Overflow::Clip || target_width >= actual_width {
            self.framebuffer.replace_text(target.top, target.left, target.right, text);
        } else {
            let bytes = text.as_bytes();
            let mut cfg = ucd::MeasurementConfig::new(&bytes);

            match overflow {
                Overflow::Clip => unreachable!(),
                Overflow::TruncateHead => {
                    let beg = cfg.goto_visual(Point { x: actual_width - target_width + 1, y: 0 });
                    skipped = 0..beg.offset;
                    skipped_cols = beg.visual_pos.x - 1;
                }
                Overflow::TruncateMiddle => {
                    let mid_beg_x = (target_width - 1) / 2;
                    let mid_end_x = actual_width - target_width / 2;
                    let beg = cfg.goto_visual(Point { x: mid_beg_x, y: 0 });
                    let end = cfg.goto_visual(Point { x: mid_end_x, y: 0 });
                    skipped = beg.offset..end.offset;
                    skipped_cols = end.visual_pos.x - beg.visual_pos.x - 1;
                }
                Overflow::TruncateTail => {
                    let end = cfg.goto_visual(Point { x: target_width - 1, y: 0 });
                    skipped_cols = actual_width - end.visual_pos.x - 1;
                    skipped = end.offset..text.len();
                }
            }

            let scratch = scratch_arena(None);

            let mut modified = ArenaString::new_in(&scratch);
            modified.reserve(text.len() + 3);
            modified.push_str(&text[..skipped.start]);
            modified.push('…');
            modified.push_str(&text[skipped.end..]);

            self.framebuffer.replace_text(target.top, target.left, target.right, &modified);
        }

        if !chunks.is_empty() {
            let bytes = text.as_bytes();
            let mut cfg = ucd::MeasurementConfig::new(&bytes).with_cursor(ucd::Cursor {
                visual_pos: Point { x: target.left, y: 0 },
                ..Default::default()
            });

            let mut iter = chunks.iter().peekable();

            while let Some(chunk) = iter.next() {
                let beg = chunk.offset;
                let end = iter.peek().map_or(text.len(), |c| c.offset);

                if beg >= skipped.start && end <= skipped.end {
                    // Chunk is fully inside the text skipped by the ellipsis.
                    // We don't need to render it at all.
                    continue;
                }

                if beg < skipped.start {
                    let beg = cfg.goto_offset(beg).visual_pos.x;
                    let end = cfg.goto_offset(end.min(skipped.start)).visual_pos.x;
                    let rect =
                        Rect { left: beg, top: target.top, right: end, bottom: target.bottom };
                    self.framebuffer.blend_fg(rect, chunk.fg);
                    self.framebuffer.replace_attr(rect, chunk.attr, chunk.attr);
                }

                if end > skipped.end {
                    let beg = cfg.goto_offset(beg.max(skipped.end)).visual_pos.x - skipped_cols;
                    let end = cfg.goto_offset(end).visual_pos.x - skipped_cols;
                    let rect =
                        Rect { left: beg, top: target.top, right: end, bottom: target.bottom };
                    self.framebuffer.blend_fg(rect, chunk.fg);
                    self.framebuffer.replace_attr(rect, chunk.attr, chunk.attr);
                }
            }
        }
    }

    /// Outputs a debug string of the layout and focus tree.
    pub fn debug_layout<'a>(&mut self, arena: &'a Arena) -> ArenaString<'a> {
        let mut result = ArenaString::new_in(arena);
        result.push_str("general:\r\n- focus_path:\r\n");

        for &id in &self.focused_node_path {
            _ = write!(result, "  - {id:016x}\r\n");
        }

        result.push_str("\r\ntree:\r\n");

        for root in self.prev_tree.iterate_roots() {
            Tree::visit_all(root, root, true, |node| {
                let node = node.borrow();
                let depth = node.depth;
                result.push_repeat(' ', depth * 2);
                _ = write!(result, "- id: {:016x}\r\n", node.id);

                result.push_repeat(' ', depth * 2);
                _ = write!(result, "  classname:    {}\r\n", node.classname);

                if depth == 0
                    && let Some(parent) = node.parent
                {
                    let parent = parent.borrow();
                    result.push_repeat(' ', depth * 2);
                    _ = write!(result, "  parent:       {:016x}\r\n", parent.id);
                }

                result.push_repeat(' ', depth * 2);
                _ = write!(
                    result,
                    "  intrinsic:    {{{}, {}}}\r\n",
                    node.intrinsic_size.width, node.intrinsic_size.height
                );

                result.push_repeat(' ', depth * 2);
                _ = write!(
                    result,
                    "  outer:        {{{}, {}, {}, {}}}\r\n",
                    node.outer.left, node.outer.top, node.outer.right, node.outer.bottom
                );

                result.push_repeat(' ', depth * 2);
                _ = write!(
                    result,
                    "  inner:        {{{}, {}, {}, {}}}\r\n",
                    node.inner.left, node.inner.top, node.inner.right, node.inner.bottom
                );

                if node.attributes.bordered {
                    result.push_repeat(' ', depth * 2);
                    result.push_str("  bordered:     true\r\n");
                }

                if node.attributes.bg != 0 {
                    result.push_repeat(' ', depth * 2);
                    _ = write!(result, "  bg:           #{:08x}\r\n", node.attributes.bg);
                }

                if node.attributes.fg != 0 {
                    result.push_repeat(' ', depth * 2);
                    _ = write!(result, "  fg:           #{:08x}\r\n", node.attributes.fg);
                }

                if self.is_node_focused(node.id) {
                    result.push_repeat(' ', depth * 2);
                    result.push_str("  focused:      true\r\n");
                }

                match &node.content {
                    NodeContent::Text(content) => {
                        result.push_repeat(' ', depth * 2);
                        _ = write!(result, "  text:         \"{}\"\r\n", &content.text);
                    }
                    NodeContent::Textarea(content) => {
                        let tb = content.buffer.borrow();
                        let tb = &*tb;
                        result.push_repeat(' ', depth * 2);
                        _ = write!(result, "  textarea:     {tb:p}\r\n");
                    }
                    NodeContent::Scrollarea(..) => {
                        result.push_repeat(' ', depth * 2);
                        result.push_str("  scrollable:   true\r\n");
                    }
                    _ => {}
                }

                VisitControl::Continue
            });
        }

        result
    }

    fn was_mouse_down_on_node(&self, id: u64) -> bool {
        // We construct the hovered_node_path always with at least 1 element (the root id).
        unsafe { *self.mouse_down_node_path.last().unwrap_unchecked() == id }
    }

    fn was_mouse_down_on_subtree(&self, node: &Node) -> bool {
        self.mouse_down_node_path.get(node.depth) == Some(&node.id)
    }

    fn is_node_focused(&self, id: u64) -> bool {
        // We construct the focused_node_path always with at least 1 element (the root id).
        unsafe { *self.focused_node_path.last().unwrap_unchecked() == id }
    }

    fn is_subtree_focused(&self, node: &Node) -> bool {
        self.focused_node_path.get(node.depth) == Some(&node.id)
    }

    fn is_subtree_focused_alt(&self, id: u64, depth: usize) -> bool {
        self.focused_node_path.get(depth) == Some(&id)
    }

    fn pop_focusable_node(&mut self, pop_minimum: usize) -> bool {
        let last_before = self.focused_node_path.last().cloned().unwrap_or(0);

        // Remove `pop_minimum`-many nodes from the end of the focus path.
        let path = &self.focused_node_path[..];
        let path = &path[..path.len().saturating_sub(pop_minimum)];
        let mut len = 0;

        for (i, &id) in path.iter().enumerate() {
            // Truncate the path so that it only contains nodes that still exist.
            let Some(node) = self.prev_node_map.get(id) else {
                break;
            };

            let n = node.borrow();
            // If the caller requested upward movement, pop out of the current focus void, if any.
            // This is kind of janky, to be fair.
            if pop_minimum != 0 && n.attributes.focus_void {
                break;
            }

            // Skip over those that aren't focusable.
            if n.attributes.focusable {
                // At this point `n.depth == i` should be true,
                // but I kind of don't want to rely on that.
                len = i + 1;
            }
        }

        self.focused_node_path.truncate(len);

        // If it's empty now, push `ROOT_ID` because there must always be >=1 element.
        if self.focused_node_path.is_empty() {
            self.focused_node_path.push(ROOT_ID);
        }

        // Return true if the focus path changed.
        let last_after = self.focused_node_path.last().cloned().unwrap_or(0);
        last_before != last_after
    }

    // TODO: Move this into `block_end()` and run it whenever the block is a `focus_well`.
    // It makes no sense otherwise that all input handling occurs in the controls, except for this.
    fn move_focus(&mut self, input: InputKey) -> bool {
        if !matches!(input, vk::TAB | SHIFT_TAB | vk::UP | vk::DOWN | vk::LEFT | vk::RIGHT) {
            return false;
        }

        let focused_id = self.focused_node_path.last().cloned().unwrap_or(0);
        let Some(focused) = self.prev_node_map.get(focused_id) else {
            debug_assert!(false); // The caller should've cleaned up the focus path.
            return false;
        };

        let mut focused_start = focused;
        let mut root = focused;

        // Figure out if we're inside a focus void (a container that doesn't
        // allow tabbing inside), and in that case, toss the focus to it.
        //
        // Also, figure out the container within which the focuse must be contained.
        // This way, tab/shift-tab only moves within the same window.
        // The ROOT_ID node has no parent, and the others have a float attribute.
        // If the root is the focused node, it should of course not move upward.
        loop {
            let root_node = root.borrow();
            if root_node.attributes.focus_well {
                break;
            }
            if root_node.attributes.focus_void {
                focused_start = root;
            }
            root = match root_node.parent {
                Some(parent) => parent,
                None => break,
            }
        }

        let forward;
        let min_depth;
        match input {
            SHIFT_TAB | vk::TAB => {
                forward = input == vk::TAB;
                min_depth = usize::MAX;
            }
            vk::UP | vk::DOWN => {
                forward = input == vk::DOWN;
                min_depth = usize::MAX;
            }
            vk::LEFT | vk::RIGHT => {
                // Find the cell within a row within a table that we're in.
                // To do so we'll use a circular buffer of the last 3 nodes while we travel up.
                let mut buf = [None; 3];
                let mut idx = buf.len() - 1;
                let mut node = focused_start;

                loop {
                    idx = (idx + 1) % buf.len();
                    buf[idx] = Some(node);
                    if let NodeContent::Table(..) = &node.borrow().content {
                        break;
                    }
                    if ptr::eq(node, root) {
                        return false;
                    }
                    node = match node.borrow().parent {
                        Some(parent) => parent,
                        None => return false,
                    }
                }

                // The current `idx` points to the table.
                // The last item is the row.
                // The 2nd to last item is the cell.
                let Some(row) = buf[(idx + 3 - 1) % buf.len()] else {
                    return false;
                };
                let Some(cell) = buf[(idx + 3 - 2) % buf.len()] else {
                    return false;
                };

                root = row;
                focused_start = cell;
                forward = input == vk::RIGHT;
                min_depth = root.borrow().depth;
            }
            _ => return false,
        }

        let mut focused_next = focused_start;
        Tree::visit_all(root, focused_start, forward, |node| {
            let n = node.borrow();
            if ptr::eq(node, root) {
                VisitControl::Continue
            } else if n.attributes.focusable && !ptr::eq(node, focused_start) {
                focused_next = node;
                VisitControl::Stop
            } else if n.attributes.focus_void || n.depth >= min_depth {
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
        let focused_id = self.focused_node_path.last().cloned().unwrap_or(0);
        if self.focused_node_for_scrolling == focused_id {
            return false;
        }

        let Some(node) = self.prev_node_map.get(focused_id) else {
            // Node not found because we're using the old layout tree.
            // Retry in the next rendering loop.
            return true;
        };

        let mut node = node.borrow_mut();
        let mut scroll_to = node.outer;

        while node.parent.is_some() && node.attributes.float.is_none() {
            let n = &mut *node;
            if let NodeContent::Scrollarea(sc) = &mut n.content {
                let off_y = sc.scroll_offset.y.max(0);
                let mut y = off_y;
                y = y.min(scroll_to.top - n.inner.top + off_y);
                y = y.max(scroll_to.bottom - n.inner.bottom + off_y);
                sc.scroll_offset.y = y;
                scroll_to = n.outer;
            }
            node = node.parent.unwrap().borrow_mut();
        }

        self.focused_node_for_scrolling = focused_id;
        true
    }
}

pub struct Context<'a, 'input> {
    tui: &'a mut Tui,

    /// Current text input, if any.
    input_text: Option<InputText<'input>>,
    /// Current keyboard input, if any.
    input_keyboard: Option<InputKey>,
    input_mouse_modifiers: InputKeyMod,
    input_mouse_click: CoordType,
    /// By how much the mouse wheel was scrolled since the last frame.
    input_scroll_delta: Point,
    input_consumed: bool,

    tree: Tree<'a>,
    last_modal: Option<&'a NodeCell<'a>>,
    next_block_id_mixin: u64,
    needs_settling: bool,

    #[cfg(debug_assertions)]
    seen_ids: HashSet<u64>,
}

impl<'a> Drop for Context<'a, '_> {
    fn drop(&mut self) {
        let tui: &'a mut Tui = unsafe { mem::transmute(&mut *self.tui) };
        tui.report_context_completion(self);
    }
}

impl<'a> Context<'a, '_> {
    pub fn arena(&self) -> &'a Arena {
        // TODO:
        // `Context` borrows `Tui` for lifetime 'a, so `self.tui` should be `&'a Tui`, right?
        // And if I do `&self.tui.arena` then that should be 'a too, right?
        // Searching for and failing to find a workaround for this was _very_ annoying.
        //
        // SAFETY: Both the returned reference and its allocations outlive &self.
        unsafe { mem::transmute::<&'_ Arena, &'a Arena>(&self.tui.arena_next) }
    }

    pub fn size(&self) -> Size {
        self.tui.size
    }

    #[inline]
    pub fn indexed(&self, index: IndexedColor) -> u32 {
        self.tui.indexed(index)
    }

    #[inline]
    pub fn indexed_alpha(&self, index: IndexedColor, alpha: u8) -> u32 {
        self.tui.indexed_alpha(index, alpha)
    }

    pub fn contrasted(&self, color: u32) -> u32 {
        self.tui.contrasted(color)
    }

    pub fn set_clipboard(&mut self, data: Vec<u8>) {
        if !data.is_empty() {
            self.tui.clipboard = data;
            self.tui.clipboard_generation = self.tui.clipboard_generation.wrapping_add(1);
            self.needs_rerender();
        }
    }

    pub fn get_clipboard(&self) -> &[u8] {
        self.tui.get_clipboard()
    }

    pub fn get_clipboard_generation(&self) -> u32 {
        self.tui.get_clipboard_generation()
    }

    pub fn needs_rerender(&mut self) {
        // If this hits, the call stack is responsible is trying to deadlock you.
        debug_assert!(self.tui.settling_have < 15);
        self.needs_settling = true;
    }

    /// Begins a new UI block (container) with a unique ID.
    pub fn block_begin(&mut self, classname: &'static str) {
        let parent = self.tree.current_node;

        let mut id = hash_str(parent.borrow().id, classname);
        if self.next_block_id_mixin != 0 {
            id = hash(id, &self.next_block_id_mixin.to_ne_bytes());
            self.next_block_id_mixin = 0;
        }

        // If this hits, you have tried to create a block with the same ID as a previous one
        // somewhere up this call stack. Change the classname, or use next_block_id_mixin().
        // TODO: HashMap
        #[cfg(debug_assertions)]
        if !self.seen_ids.insert(id) {
            panic!("Duplicate node ID: {id:x}");
        }

        let node: &mut NodeCell = self.arena().alloc_default();
        {
            let mut n = node.borrow_mut();
            n.id = id;
            n.classname = classname;
        }

        self.tree.push_child(node);
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
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.focusable = true;
    }

    pub fn focus_on_first_present(&mut self) {
        let steal = {
            let mut last_node = self.tree.last_node.borrow_mut();
            last_node.attributes.focusable = true;
            self.tui.prev_node_map.get(last_node.id).is_none()
        };
        if steal {
            self.steal_focus();
        }
    }

    pub fn steal_focus(&mut self) {
        self.steal_focus_for(self.tree.last_node);
    }

    fn steal_focus_for(&mut self, node: &NodeCell<'a>) {
        if !self.tui.is_node_focused(node.borrow().id) {
            Tui::build_node_path(Some(node), &mut self.tui.focused_node_path);
            self.needs_rerender();
        }
    }

    pub fn toss_focus_up(&mut self) {
        if self.tui.pop_focusable_node(1) {
            self.needs_rerender();
        }
    }

    pub fn inherit_focus(&mut self) {
        let mut last_node = self.tree.last_node.borrow_mut();
        let Some(parent) = last_node.parent else {
            return;
        };

        last_node.attributes.focusable = true;

        // Mark the parent as focusable, so that if the user presses Escape,
        // and `block_end` bubbles the focus up the tree, it'll stop on our parent,
        // which will then focus us on the next iteration.
        let mut parent = parent.borrow_mut();
        parent.attributes.focusable = true;

        if self.tui.is_node_focused(parent.id) {
            self.needs_rerender();
            self.tui.focused_node_path.push(last_node.id);
        }
    }

    pub fn attr_focus_well(&mut self) {
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.focus_well = true;
    }

    pub fn attr_intrinsic_size(&mut self, size: Size) {
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.intrinsic_size = size;
        last_node.intrinsic_size_set = true;
    }

    pub fn attr_float(&mut self, spec: FloatSpec) {
        let last_node = self.tree.last_node;
        let anchor = {
            let ln = last_node.borrow();
            match spec.anchor {
                Anchor::Last if ln.siblings.prev.is_some() => ln.siblings.prev,
                Anchor::Last | Anchor::Parent => ln.parent,
                // By not giving such floats a parent, they get the same origin as the original root node,
                // but they also gain their own "root id" in the tree. That way, their focus path is totally unique,
                // which means that we can easily check if a modal is open by calling `is_focused()` on the original root.
                Anchor::Root => None,
            }
        };

        self.tree.move_node_to_root(last_node, anchor);

        let mut ln = last_node.borrow_mut();
        ln.attributes.focus_well = true;
        ln.attributes.float = Some(FloatAttributes {
            gravity_x: spec.gravity_x.clamp(0.0, 1.0),
            gravity_y: spec.gravity_y.clamp(0.0, 1.0),
            offset_x: spec.offset_x,
            offset_y: spec.offset_y,
        });
        ln.attributes.bg = self.tui.floater_default_bg;
        ln.attributes.fg = self.tui.floater_default_fg;
    }

    pub fn attr_border(&mut self) {
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.bordered = true;
    }

    pub fn attr_position(&mut self, align: Position) {
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.position = align;
    }

    pub fn attr_padding(&mut self, padding: Rect) {
        let mut last_node = self.tree.last_node.borrow_mut();
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
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.bg = bg;
    }

    pub fn attr_foreground_rgba(&mut self, fg: u32) {
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.fg = fg;
    }

    pub fn attr_reverse(&mut self) {
        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.attributes.reverse = true;
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

    pub fn was_mouse_down(&mut self) -> bool {
        let last_node = self.tree.last_node.borrow();
        self.tui.was_mouse_down_on_node(last_node.id)
    }

    pub fn contains_mouse_down(&mut self) -> bool {
        let last_node = self.tree.last_node.borrow();
        self.tui.was_mouse_down_on_subtree(&last_node)
    }

    pub fn is_focused(&mut self) -> bool {
        let last_node = self.tree.last_node.borrow();
        self.tui.is_node_focused(last_node.id)
    }

    pub fn contains_focus(&mut self) -> bool {
        let last_node = self.tree.last_node.borrow();
        self.tui.is_subtree_focused(&last_node)
    }

    pub fn modal_begin(&mut self, classname: &'static str, title: &str) {
        self.block_begin(classname);
        self.attr_float(FloatSpec { anchor: Anchor::Root, ..Default::default() });
        self.attr_intrinsic_size(Size { width: self.tui.size.width, height: self.tui.size.height });
        self.attr_background_rgba(self.indexed_alpha(IndexedColor::Background, 0xd6));
        self.attr_foreground_rgba(self.indexed_alpha(IndexedColor::Background, 0xd6));
        self.attr_focus_well();

        self.block_begin("window");
        self.attr_float(FloatSpec {
            anchor: Anchor::Last,
            gravity_x: 0.5,
            gravity_y: 0.5,
            offset_x: self.tui.size.width as f32 * 0.5,
            offset_y: self.tui.size.height as f32 * 0.5,
        });
        self.attr_border();
        self.attr_background_rgba(self.tui.modal_default_bg);
        self.attr_foreground_rgba(self.tui.modal_default_fg);
        self.inherit_focus();
        self.focus_on_first_present();

        let mut last_node = self.tree.last_node.borrow_mut();
        let title = if title.is_empty() {
            ArenaString::new_in(self.arena())
        } else {
            arena_format!(self.arena(), " {} ", title)
        };
        last_node.content = NodeContent::Modal(title);
        self.last_modal = Some(self.tree.last_node);
    }

    pub fn modal_end(&mut self) -> bool {
        self.block_end();
        self.block_end();
        self.contains_focus() && self.consume_shortcut(vk::ESCAPE)
    }

    pub fn table_begin(&mut self, classname: &'static str) {
        self.block_begin(classname);

        let mut last_node = self.tree.last_node.borrow_mut();
        last_node.content = NodeContent::Table(TableContent {
            columns: Vec::new_in(self.arena()),
            cell_gap: Default::default(),
        });
    }

    pub fn table_set_columns(&mut self, columns: &[i32]) {
        let mut last_node = self.tree.last_node.borrow_mut();
        if let NodeContent::Table(spec) = &mut last_node.content {
            spec.columns.clear();
            spec.columns.extend_from_slice(columns);
        } else {
            debug_assert!(false);
        }
    }

    pub fn table_set_cell_gap(&mut self, cell_gap: Size) {
        let mut last_node = self.tree.last_node.borrow_mut();
        if let NodeContent::Table(spec) = &mut last_node.content {
            spec.cell_gap = cell_gap;
        } else {
            debug_assert!(false);
        }
    }

    pub fn table_next_row(&mut self) {
        {
            let current_node = self.tree.current_node.borrow();

            // If this is the first call to table_next_row() inside a new table, the
            // current_node will refer to the table. Otherwise, it'll refer to the current row.
            if !matches!(current_node.content, NodeContent::Table(_)) {
                let Some(parent) = current_node.parent else {
                    return;
                };

                let parent = parent.borrow();
                // Neither the current nor its parent nodes are a table?
                // You definitely called this outside of a table block.
                debug_assert!(matches!(parent.content, NodeContent::Table(_)));

                self.block_end();
                self.next_block_id_mixin(parent.child_count as u64);
            }
        }

        self.block_begin("row");
    }

    pub fn table_end(&mut self) {
        let current_node = self.tree.current_node.borrow();

        // If this is the first call to table_next_row() inside a new table, the
        // current_node will refer to the table. Otherwise, it'll refer to the current row.
        if !matches!(current_node.content, NodeContent::Table(_)) {
            self.block_end();
        }

        self.block_end(); // table
    }

    pub fn label(&mut self, classname: &'static str, text: &str) {
        self.styled_label_begin(classname);
        self.styled_label_add_text(text);
        self.styled_label_end();
    }

    pub fn styled_label_begin(&mut self, classname: &'static str) {
        self.block_begin(classname);
        self.tree.last_node.borrow_mut().content = NodeContent::Text(TextContent {
            text: ArenaString::new_in(self.arena()),
            chunks: Vec::with_capacity_in(4, self.arena()),
            overflow: Overflow::Clip,
        });
    }

    pub fn styled_label_set_foreground(&mut self, fg: u32) {
        let mut node = self.tree.last_node.borrow_mut();
        let NodeContent::Text(content) = &mut node.content else {
            unreachable!();
        };

        let last = content.chunks.last().unwrap_or(&INVALID_STYLED_TEXT_CHUNK);
        if last.offset != content.text.len() && last.fg != fg {
            content.chunks.push(StyledTextChunk {
                offset: content.text.len(),
                fg,
                attr: last.attr,
            });
        }
    }

    pub fn styled_label_set_attributes(&mut self, attr: Attributes) {
        let mut node = self.tree.last_node.borrow_mut();
        let NodeContent::Text(content) = &mut node.content else {
            unreachable!();
        };

        let last = content.chunks.last().unwrap_or(&INVALID_STYLED_TEXT_CHUNK);
        if last.offset != content.text.len() && last.attr != attr {
            content.chunks.push(StyledTextChunk { offset: content.text.len(), fg: last.fg, attr });
        }
    }

    pub fn styled_label_add_text(&mut self, text: &str) {
        let mut node = self.tree.last_node.borrow_mut();
        let NodeContent::Text(content) = &mut node.content else {
            unreachable!();
        };

        content.text.push_str(text);
    }

    pub fn styled_label_end(&mut self) {
        {
            let mut last_node = self.tree.last_node.borrow_mut();
            let NodeContent::Text(content) = &last_node.content else {
                return;
            };

            let cursor = ucd::MeasurementConfig::new(&content.text.as_bytes())
                .goto_visual(Point { x: CoordType::MAX, y: 0 });
            last_node.intrinsic_size.width = cursor.visual_pos.x;
            last_node.intrinsic_size.height = 1;
            last_node.intrinsic_size_set = true;
        }

        self.block_end();
    }

    pub fn attr_overflow(&mut self, overflow: Overflow) {
        let mut last_node = self.tree.last_node.borrow_mut();
        let NodeContent::Text(content) = &mut last_node.content else {
            return;
        };

        content.overflow = overflow;
    }

    pub fn button(&mut self, classname: &'static str, text: &str) -> bool {
        self.styled_label_begin(classname);
        self.attr_focusable();
        if self.is_focused() {
            self.attr_reverse();
        }
        self.styled_label_add_text("[");
        self.styled_label_add_text(text);
        self.styled_label_add_text("]");
        self.styled_label_end();

        self.button_activated()
    }

    pub fn checkbox(&mut self, classname: &'static str, text: &str, checked: &mut bool) -> bool {
        self.styled_label_begin(classname);
        self.attr_focusable();
        if self.is_focused() {
            self.attr_reverse();
        }
        self.styled_label_add_text(if *checked { "[▣ " } else { "[☐ " });
        self.styled_label_add_text(text);
        self.styled_label_add_text("]");
        self.styled_label_end();

        let activated = self.button_activated();
        if activated {
            *checked = !*checked;
        }
        activated
    }

    fn button_activated(&mut self) -> bool {
        if !self.input_consumed
            && ((self.input_mouse_click != 0 && self.contains_mouse_down())
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

    pub fn editline<'s, 'b: 's>(
        &'s mut self,
        classname: &'static str,
        text: &'b mut dyn WriteableDocument,
    ) -> bool {
        self.textarea_internal(classname, TextBufferPayload::Editline(text))
    }

    pub fn textarea(&mut self, classname: &'static str, tb: RcTextBuffer) {
        self.textarea_internal(classname, TextBufferPayload::Textarea(tb));
    }

    fn textarea_internal(&mut self, classname: &'static str, payload: TextBufferPayload) -> bool {
        self.block_begin(classname);
        self.block_end();

        let mut node = self.tree.last_node.borrow_mut();
        let node = &mut *node;
        let single_line = match &payload {
            TextBufferPayload::Editline(_) => true,
            TextBufferPayload::Textarea(_) => false,
        };

        let buffer = {
            let buffers = &mut self.tui.cached_text_buffers;

            let cached = match buffers.iter_mut().find(|t| t.node_id == node.id) {
                Some(cached) => {
                    if let TextBufferPayload::Textarea(tb) = &payload {
                        cached.editor = tb.clone();
                    };
                    cached.seen = true;
                    cached
                }
                None => {
                    // If the node is not in the cache, we need to create a new one.
                    buffers.push(CachedTextBuffer {
                        node_id: node.id,
                        editor: match &payload {
                            TextBufferPayload::Editline(_) => TextBuffer::new_rc(true).unwrap(),
                            TextBufferPayload::Textarea(tb) => tb.clone(),
                        },
                        seen: true,
                    });
                    buffers.last_mut().unwrap()
                }
            };

            // SAFETY: *Assuming* that there are no duplicate node IDs in the tree that
            // would cause this cache slot to be overwritten, then this operation is safe.
            // The text buffer cache will keep the buffer alive for us long enough.
            unsafe { mem::transmute(&*cached.editor) }
        };

        node.content = NodeContent::Textarea(TextareaContent {
            buffer,
            scroll_offset: Default::default(),
            scroll_offset_y_drag_start: CoordType::MIN,
            scroll_offset_x_max: 0,
            thumb_height: 0,
            preferred_column: 0,
            single_line,
            has_focus: self.tui.is_node_focused(node.id),
        });

        let content = match node.content {
            NodeContent::Textarea(ref mut content) => content,
            _ => unreachable!(),
        };

        if let TextBufferPayload::Editline(text) = &payload {
            content.buffer.borrow_mut().copy_from_str(*text);
        }

        if let Some(node_prev) = self.tui.prev_node_map.get(node.id) {
            let node_prev = node_prev.borrow();
            if let NodeContent::Textarea(content_prev) = &node_prev.content {
                content.scroll_offset = content_prev.scroll_offset;
                content.scroll_offset_y_drag_start = content_prev.scroll_offset_y_drag_start;
                content.scroll_offset_x_max = content_prev.scroll_offset_x_max;
                content.thumb_height = content_prev.thumb_height;
                content.preferred_column = content_prev.preferred_column;

                let mut text_width = node_prev.inner.width();
                if !single_line {
                    // Subtract -1 to account for the scrollbar.
                    text_width -= 1;
                }

                let mut make_cursor_visible;
                {
                    let mut tb = content.buffer.borrow_mut();
                    make_cursor_visible = tb.take_cursor_visibility_request();
                    make_cursor_visible |= tb.set_width(text_width);
                }

                make_cursor_visible |= self.textarea_handle_input(content, &node_prev, single_line);

                if make_cursor_visible {
                    self.textarea_make_cursor_visible(content, &node_prev);
                }
            } else {
                debug_assert!(false);
            }
        }

        let dirty;
        {
            let mut tb = content.buffer.borrow_mut();
            dirty = tb.is_dirty();
            if dirty && let TextBufferPayload::Editline(text) = payload {
                tb.save_as_string(text);
            }
        }

        self.textarea_adjust_scroll_offset(content);

        node.attributes.bg = self.indexed(IndexedColor::Background);
        node.attributes.fg = self.indexed(IndexedColor::Foreground);
        if single_line && !content.has_focus {
            node.attributes.bg &= 0x7fffffff;
            node.attributes.fg = self.contrasted(node.attributes.bg);
        }

        node.attributes.focusable = true;
        node.intrinsic_size.height = content.buffer.borrow().get_visual_line_count();
        node.intrinsic_size_set = true;

        dirty
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

        let mut tb = tc.buffer.borrow_mut();
        let tb = &mut *tb;
        let mut make_cursor_visible = false;

        if self.tui.mouse_state != InputMouseState::None
            && self.tui.was_mouse_down_on_node(node_prev.id)
        {
            // Scrolling works even if the node isn't focused.
            if self.tui.mouse_state == InputMouseState::Scroll {
                tc.scroll_offset.x += self.input_scroll_delta.x;
                tc.scroll_offset.y += self.input_scroll_delta.y;
                self.set_input_consumed();
            } else if self.tui.is_node_focused(node_prev.id) {
                let mouse = self.tui.mouse_position;
                let inner = node_prev.inner;
                let text_rect = Rect {
                    left: inner.left + tb.get_margin_width(),
                    top: inner.top,
                    right: inner.right - !single_line as CoordType,
                    bottom: inner.bottom,
                };
                let track_rect = Rect {
                    left: text_rect.right,
                    top: inner.top,
                    right: inner.right,
                    bottom: inner.bottom,
                };
                let pos = Point {
                    x: mouse.x - inner.left - tb.get_margin_width() + tc.scroll_offset.x,
                    y: mouse.y - inner.top + tc.scroll_offset.y,
                };

                if text_rect.contains(self.tui.mouse_down_position) {
                    if self.tui.mouse_is_drag {
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

                            let delta_x = calc(text_rect.left, text_rect.right, mouse.x);
                            let delta_y = calc(text_rect.top, text_rect.bottom, mouse.y);

                            tc.scroll_offset.x += delta_x;
                            tc.scroll_offset.y += delta_y;

                            if delta_x != 0 || delta_y != 0 {
                                self.tui.read_timeout = time::Duration::from_millis(25);
                            }
                        }
                    } else {
                        match self.input_mouse_click {
                            5.. => {}
                            4 => tb.select_all(),
                            3 => tb.select_line(),
                            2 => tb.select_word(),
                            _ => match self.tui.mouse_state {
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
                                    tb.selection_finalize();
                                }
                                _ => return false,
                            },
                        }
                    }
                } else if track_rect.contains(self.tui.mouse_down_position) {
                    if self.tui.mouse_state == InputMouseState::Release {
                        tc.scroll_offset_y_drag_start = CoordType::MIN;
                    } else if self.tui.mouse_is_drag {
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
                    }
                }

                self.set_input_consumed();
            }

            return make_cursor_visible;
        }

        if !tc.has_focus {
            return false;
        }

        let mut write: &[u8] = b"";
        let mut write_raw = false;

        if let Some(input) = &self.input_text {
            write = input.text.as_bytes();
            write_raw = input.bracketed;
            tc.preferred_column = tb.get_cursor_visual_pos().x;
            make_cursor_visible = true;
        } else if let Some(input) = &self.input_keyboard {
            let key = input.key();
            let modifiers = input.modifiers();

            make_cursor_visible = true;

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
                    if modifiers == kbmod::SHIFT {
                        tb.unindent();
                    } else {
                        write = b"\t";
                    }
                }
                vk::RETURN => {
                    if single_line {
                        // If this is just a simple input field, don't consume Enter (= early return).
                        return false;
                    }
                    write = b"\n";
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
                    kbmod::CTRL => tb.cursor_move_to_logical(Default::default()),
                    kbmod::SHIFT => {
                        tb.selection_update_visual(Point { x: 0, y: tb.get_cursor_visual_pos().y })
                    }
                    _ => tb.cursor_move_to_visual(Point { x: 0, y: tb.get_cursor_visual_pos().y }),
                },
                vk::LEFT => {
                    let granularity = if modifiers.contains(kbmod::CTRL) {
                        CursorMovement::Word
                    } else {
                        CursorMovement::Grapheme
                    };
                    if modifiers.contains(kbmod::SHIFT) {
                        tb.selection_update_delta(granularity, -1);
                    } else if let Some((beg, _)) = tb.selection_range() {
                        unsafe { tb.set_cursor(beg) };
                    } else {
                        tb.cursor_move_delta(granularity, -1);
                    }
                }
                vk::UP => {
                    match modifiers {
                        kbmod::NONE => {
                            let mut x = tc.preferred_column;
                            let mut y = tb.get_cursor_visual_pos().y - 1;

                            // If there's a selection we the cursor above the start of the selection.
                            if let Some((beg, _)) = tb.selection_range() {
                                x = beg.visual_pos.x;
                                y = beg.visual_pos.y - 1;
                                tc.preferred_column = x;
                            }

                            // If the cursor was already on the first line,
                            // move it to the start of the buffer.
                            if y < 0 {
                                x = 0;
                                tc.preferred_column = 0;
                            }

                            tb.cursor_move_to_visual(Point { x, y });
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
                    } else if let Some((_, end)) = tb.selection_range() {
                        unsafe { tb.set_cursor(end) };
                    } else {
                        tb.cursor_move_delta(granularity, 1);
                    }
                }
                vk::DOWN => match modifiers {
                    kbmod::NONE => {
                        let mut x = tc.preferred_column;
                        let mut y = tb.get_cursor_visual_pos().y + 1;

                        // If there's a selection we the cursor below the end of the selection.
                        if let Some((_, end)) = tb.selection_range() {
                            x = end.visual_pos.x;
                            y = end.visual_pos.y + 1;
                            tc.preferred_column = x;
                        }

                        // If the cursor was already on the last line,
                        // move it to the end of the buffer.
                        if y >= tb.get_visual_line_count() {
                            x = CoordType::MAX;
                        }

                        tb.cursor_move_to_visual(Point { x, y });

                        // If we fell into the `if y >= tb.get_visual_line_count()` above, we wanted to
                        // update the `preferred_column` but didn't know yet what it was. Now we know!
                        if x == CoordType::MAX {
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
                    kbmod::SHIFT => {
                        write = &self.tui.clipboard;
                        write_raw = true;
                    }
                    kbmod::CTRL => self.set_clipboard(tb.extract_selection(false)),
                    _ => tb.set_overtype(!tb.is_overtype()),
                },
                vk::DELETE => match modifiers {
                    kbmod::SHIFT => self.set_clipboard(tb.extract_selection(true)),
                    kbmod::CTRL => tb.delete(CursorMovement::Word, 1),
                    _ => tb.delete(CursorMovement::Grapheme, 1),
                },
                vk::A => match modifiers {
                    kbmod::CTRL => tb.select_all(),
                    _ => return false,
                },
                vk::H => match modifiers {
                    kbmod::CTRL => tb.delete(CursorMovement::Word, -1),
                    _ => return false,
                },
                vk::X => match modifiers {
                    kbmod::CTRL => self.set_clipboard(tb.extract_selection(true)),
                    _ => return false,
                },
                vk::C => match modifiers {
                    kbmod::CTRL => self.set_clipboard(tb.extract_selection(false)),
                    _ => return false,
                },
                vk::V => match modifiers {
                    kbmod::CTRL => {
                        write = &self.tui.clipboard;
                        write_raw = true;
                    }
                    _ => return false,
                },
                vk::Y => match modifiers {
                    kbmod::CTRL => tb.redo(),
                    _ => return false,
                },
                vk::Z => match modifiers {
                    kbmod::CTRL => tb.undo(),
                    kbmod::CTRL_SHIFT => tb.redo(),
                    kbmod::ALT => tb.set_word_wrap(!tb.is_word_wrap_enabled()),
                    _ => return false,
                },
                _ => return false,
            }

            if !matches!(key, vk::PRIOR | vk::NEXT | vk::UP | vk::DOWN) {
                tc.preferred_column = tb.get_cursor_visual_pos().x;
            }
        } else {
            return false;
        }

        if single_line && !write.is_empty() {
            let (end, _) = ucd::newlines_forward(write, 0, 0, 1);
            write = ucd::strip_newline(&write[..end]);
        }
        if !write.is_empty() {
            tb.write(write, write_raw);
        }

        self.set_input_consumed();
        make_cursor_visible
    }

    fn textarea_make_cursor_visible(&self, tc: &mut TextareaContent, node_prev: &Node) {
        let tb = tc.buffer.borrow();
        let mut scroll_x = tc.scroll_offset.x;
        let mut scroll_y = tc.scroll_offset.y;

        let text_width = tb.get_text_width();
        let cursor_x = tb.get_cursor_visual_pos().x;
        scroll_x = scroll_x.min(cursor_x - 10);
        scroll_x = scroll_x.max(cursor_x - text_width + 10);

        let viewport_height = node_prev.inner.height();
        let cursor_y = tb.get_cursor_visual_pos().y;
        // Scroll up if the cursor is above the visible area.
        scroll_y = scroll_y.min(cursor_y);
        // Scroll down if the cursor is below the visible area.
        scroll_y = scroll_y.max(cursor_y - viewport_height + 1);

        tc.scroll_offset.x = scroll_x;
        tc.scroll_offset.y = scroll_y;
    }

    fn textarea_adjust_scroll_offset(&self, tc: &mut TextareaContent) {
        let tb = tc.buffer.borrow();
        let mut scroll_x = tc.scroll_offset.x;
        let mut scroll_y = tc.scroll_offset.y;

        scroll_x = scroll_x.min(tc.scroll_offset_x_max.max(tb.get_cursor_visual_pos().x) - 10);
        scroll_x = scroll_x.max(0);
        scroll_y = scroll_y.clamp(0, tb.get_visual_line_count() - 1);

        if tb.is_word_wrap_enabled() {
            scroll_x = 0;
        }

        tc.scroll_offset.x = scroll_x;
        tc.scroll_offset.y = scroll_y;
    }

    pub fn scrollarea_begin(&mut self, classname: &'static str, intrinsic_size: Size) {
        self.block_begin(classname);

        let container_node = self.tree.last_node;
        {
            let mut container = self.tree.last_node.borrow_mut();
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
        }

        self.block_begin("content");
        self.inherit_focus();

        // Ensure that attribute modifications apply to the outer container.
        self.tree.last_node = container_node;
    }

    pub fn scrollarea_scroll_to(&mut self, pos: Point) {
        let mut container = self.tree.last_node.borrow_mut();
        if let NodeContent::Scrollarea(sc) = &mut container.content {
            sc.scroll_offset = pos;
        } else {
            debug_assert!(false);
        }
    }

    pub fn scrollarea_end(&mut self) {
        self.block_end(); // content block
        self.block_end(); // outer container

        let mut container = self.tree.last_node.borrow_mut();
        let container_id = container.id;
        let container_depth = container.depth;
        let Some(prev_container) = self.tui.prev_node_map.get(container_id) else {
            return;
        };

        let prev_container = prev_container.borrow();
        let NodeContent::Scrollarea(sc) = &mut container.content else {
            unreachable!();
        };

        if sc.scroll_offset == Point::MIN
            && let NodeContent::Scrollarea(sc_prev) = &prev_container.content
        {
            *sc = sc_prev.clone();
        }

        if !self.input_consumed {
            if self.tui.mouse_state != InputMouseState::None {
                let container_rect = prev_container.inner;

                match self.tui.mouse_state {
                    InputMouseState::Left => {
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

                                let content = prev_container.children.first.unwrap().borrow();
                                let content_rect = content.inner;
                                let content_height = content_rect.height();
                                let track_height = track_rect.height();
                                let scrollable_height = content_height - track_height;

                                if scrollable_height > 0 {
                                    let trackable = track_height - sc.thumb_height;
                                    let delta_y =
                                        self.tui.mouse_position.y - self.tui.mouse_down_position.y;
                                    sc.scroll_offset.y = sc.scroll_offset_y_drag_start
                                        + ((delta_y * scrollable_height) / trackable);
                                }

                                self.set_input_consumed();
                            }
                        }
                    }
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
            } else if self.tui.is_subtree_focused_alt(container_id, container_depth)
                && let Some(key) = self.input_keyboard
            {
                match key {
                    vk::PRIOR => sc.scroll_offset.y -= prev_container.inner_clipped.height(),
                    vk::NEXT => sc.scroll_offset.y += prev_container.inner_clipped.height(),
                    vk::END => sc.scroll_offset.y = CoordType::MAX,
                    vk::HOME => sc.scroll_offset.y = 0,
                    _ => return,
                }
                self.set_input_consumed();
            }
        }
    }

    pub fn list_begin(&mut self, classname: &'static str) {
        self.block_begin(classname);
        self.attr_focusable();

        let mut last_node = self.tree.last_node.borrow_mut();
        let content = self
            .tui
            .prev_node_map
            .get(last_node.id)
            .and_then(|node| match &node.borrow().content {
                NodeContent::List(content) => {
                    Some(ListContent { selected: content.selected, selected_node: None })
                }
                _ => None,
            })
            .unwrap_or(ListContent { selected: 0, selected_node: None });

        last_node.attributes.focus_void = true;
        last_node.content = NodeContent::List(content);
    }

    pub fn list_item(&mut self, select: bool, text: &str) -> ListSelection {
        self.styled_list_item_begin();
        self.styled_label_add_text(text);
        self.styled_list_item_end(select)
    }

    pub fn styled_list_item_begin(&mut self) {
        let list = self.tree.current_node;
        let idx = list.borrow().child_count;

        self.next_block_id_mixin(idx as u64);
        self.styled_label_begin("item");
        self.styled_label_add_text("  ");
        self.attr_focusable();
    }

    pub fn styled_list_item_end(&mut self, select: bool) -> ListSelection {
        self.styled_label_end();

        let list = self.tree.current_node;

        let selected_before;
        let selected_now;
        let focused;
        {
            let mut list = list.borrow_mut();
            let content = match &mut list.content {
                NodeContent::List(content) => content,
                _ => unreachable!(),
            };

            let item = self.tree.last_node.borrow();
            let item_id = item.id;
            selected_before = content.selected == item_id;
            focused = self.is_focused();

            // Inherit the default selection & Click changes selection
            selected_now = selected_before || (select && content.selected == 0) || focused;

            // Note down the selected node for keyboard navigation.
            if selected_now {
                content.selected_node = Some(self.tree.last_node);
                if !selected_before {
                    content.selected = item_id;
                    self.needs_rerender();
                }
            }
        }

        // Clicking an item activates it
        let clicked =
            !self.input_consumed && (self.input_mouse_click == 2 && self.was_mouse_down());
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

        let contains_focus;
        let selected_now;
        let mut selected_next;
        {
            let list = self.tree.last_node.borrow();

            contains_focus = self.tui.is_subtree_focused(&list);
            selected_now = match &list.content {
                NodeContent::List(content) => content.selected_node,
                _ => unreachable!(),
            };
            selected_next = match selected_now.or(list.children.first) {
                Some(node) => node,
                None => return,
            };
        }

        if contains_focus
            && !self.input_consumed
            && let Some(key) = self.input_keyboard
            && let Some(selected_now) = selected_now
        {
            let list = self.tree.last_node.borrow();

            if let Some(prev_container) = self.tui.prev_node_map.get(list.id) {
                let mut consumed = true;

                match key {
                    vk::PRIOR => {
                        selected_next = selected_now;
                        for _ in 0..prev_container.borrow().inner_clipped.height() - 1 {
                            let node = selected_next.borrow();
                            selected_next = match node.siblings.prev {
                                Some(node) => node,
                                None => break,
                            };
                        }
                    }
                    vk::NEXT => {
                        selected_next = selected_now;
                        for _ in 0..prev_container.borrow().inner_clipped.height() - 1 {
                            let node = selected_next.borrow();
                            selected_next = match node.siblings.next {
                                Some(node) => node,
                                None => break,
                            };
                        }
                    }
                    vk::END => {
                        selected_next = list.children.last.unwrap_or(selected_next);
                    }
                    vk::HOME => {
                        selected_next = list.children.first.unwrap_or(selected_next);
                    }
                    vk::UP => {
                        selected_next = selected_now
                            .borrow()
                            .siblings
                            .prev
                            .or(list.children.last)
                            .unwrap_or(selected_next);
                    }
                    vk::DOWN => {
                        selected_next = selected_now
                            .borrow()
                            .siblings
                            .next
                            .or(list.children.first)
                            .unwrap_or(selected_next);
                    }
                    _ => consumed = false,
                }

                if consumed {
                    self.set_input_consumed();
                }
            }
        }

        // Now that we know which item is selected we can mark it as such.
        if !opt_ptr_eq(selected_now, Some(selected_next))
            && let NodeContent::List(content) = &mut self.tree.last_node.borrow_mut().content
        {
            content.selected_node = Some(selected_next);
        }

        // Now that we know which item is selected we can mark it as such.
        if let NodeContent::Text(content) = &mut selected_next.borrow_mut().content {
            unsafe {
                content.text.as_bytes_mut()[0] = b'>';
            }
        }

        // If the list has focus, we also delegate focus to the selected item and colorize it.
        if contains_focus {
            {
                let mut node = selected_next.borrow_mut();
                node.attributes.bg = self.indexed(IndexedColor::Green);
                node.attributes.fg = self.contrasted(self.indexed(IndexedColor::Green));
            }
            self.steal_focus_for(selected_next);
        }
    }

    pub fn menubar_begin(&mut self) {
        self.table_begin("menubar");
        self.attr_focus_well();
        self.table_next_row();
    }

    pub fn menubar_menu_begin(&mut self, text: &str, accelerator: char) -> bool {
        let mixin = self.tree.current_node.borrow().child_count as u64;
        self.next_block_id_mixin(mixin);

        self.menubar_label(text, accelerator, None);
        self.attr_focusable();
        self.attr_padding(Rect::two(0, 1));

        let contains_focus = self.contains_focus();
        let keyboard_focus = !contains_focus
            && self.consume_shortcut(kbmod::ALT | InputKey::new(accelerator as u32));

        if contains_focus || keyboard_focus {
            self.attr_background_rgba(self.tui.floater_default_bg);
            self.attr_foreground_rgba(self.tui.floater_default_fg);

            if self.is_focused() {
                self.attr_background_rgba(self.indexed(IndexedColor::Green));
                self.attr_foreground_rgba(self.contrasted(self.indexed(IndexedColor::Green)));
            }

            self.next_block_id_mixin(mixin);
            self.table_begin("flyout");
            self.attr_float(FloatSpec {
                anchor: Anchor::Last,
                gravity_x: 0.0,
                gravity_y: 0.0,
                offset_x: 0.0,
                offset_y: 1.0,
            });
            self.attr_border();
            self.attr_focus_well();

            if keyboard_focus {
                self.steal_focus();
            }

            true
        } else {
            false
        }
    }

    pub fn menubar_menu_button(
        &mut self,
        text: &str,
        accelerator: char,
        shortcut: InputKey,
    ) -> bool {
        self.menubar_menu_checkbox(text, accelerator, shortcut, false)
    }

    pub fn menubar_menu_checkbox(
        &mut self,
        text: &str,
        accelerator: char,
        shortcut: InputKey,
        checked: bool,
    ) -> bool {
        self.table_next_row();
        self.attr_focusable();

        // First menu item? Steal focus.
        if self.tree.current_node.borrow_mut().siblings.prev.is_none() {
            self.inherit_focus();
        }

        if self.is_focused() {
            self.attr_background_rgba(self.indexed(IndexedColor::Green));
            self.attr_foreground_rgba(self.contrasted(self.indexed(IndexedColor::Green)));
        }

        let clicked =
            self.button_activated() || self.consume_shortcut(InputKey::new(accelerator as u32));

        self.menubar_label(text, accelerator, Some(checked));
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

        if !self.input_consumed
            && let Some(key) = self.input_keyboard
            && matches!(key, vk::ESCAPE | vk::UP | vk::DOWN | vk::LEFT | vk::RIGHT)
        {
            if matches!(key, vk::UP | vk::DOWN) {
                let ln = self.tree.last_node.borrow();
                if self.tui.is_node_focused(ln.parent.map_or(0, |n| n.borrow().id)) {
                    let selected_next =
                        if key == vk::UP { ln.children.last } else { ln.children.first };
                    if let Some(selected_next) = selected_next {
                        self.steal_focus_for(selected_next);
                        self.set_input_consumed();
                    }
                }
            } else if self.contains_focus() {
                if key == vk::ESCAPE {
                    // TODO: This should reassign the previous focused path.
                    self.needs_rerender();
                    self.set_input_consumed();
                    self.tui.focused_node_path.clear();
                    self.tui.focused_node_path.push(ROOT_ID);
                } else if !self.is_focused() {
                    self.tui.pop_focusable_node(2);
                }
            }
        }
    }

    pub fn menubar_end(&mut self) {
        self.table_end();
    }

    fn menubar_label(&mut self, text: &str, accelerator: char, checked: Option<bool>) {
        if !accelerator.is_ascii_uppercase() {
            self.label("label", text);
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

        self.styled_label_begin("label");
        if let Some(checked) = checked {
            self.styled_label_add_text(if checked { "▣ " } else { "  " });
        }

        if off < text.len() {
            // Highlight the accelerator in red.
            self.styled_label_add_text(&text[..off]);
            self.styled_label_set_attributes(Attributes::Underlined);
            self.styled_label_add_text(&text[off..off + 1]);
            self.styled_label_set_attributes(Attributes::None);
            self.styled_label_add_text(&text[off + 1..]);
        } else {
            // Add the accelerator in parentheses (still in red).
            let ch = accelerator as u8;
            self.styled_label_add_text(text);
            self.styled_label_add_text("(");
            self.styled_label_set_attributes(Attributes::Underlined);
            self.styled_label_add_text(unsafe { str_from_raw_parts(&ch, 1) });
            self.styled_label_set_attributes(Attributes::None);
            self.styled_label_add_text(")");
        }

        self.styled_label_end();
        self.attr_padding(Rect { left: 0, top: 0, right: 2, bottom: 0 });
    }

    fn menubar_shortcut(&mut self, shortcut: InputKey) {
        let shortcut_letter = shortcut.value() as u8 as char;
        if shortcut_letter.is_ascii_uppercase() {
            let mut shortcut_text = ArenaString::new_in(self.arena());
            if shortcut.modifiers_contains(kbmod::CTRL) {
                shortcut_text.push_str(self.tui.modifier_translations.ctrl);
                shortcut_text.push('+');
            }
            if shortcut.modifiers_contains(kbmod::ALT) {
                shortcut_text.push_str(self.tui.modifier_translations.alt);
                shortcut_text.push('+');
            }
            if shortcut.modifiers_contains(kbmod::SHIFT) {
                shortcut_text.push_str(self.tui.modifier_translations.shift);
                shortcut_text.push('+');
            }
            shortcut_text.push(shortcut_letter);

            self.label("shortcut", &shortcut_text);
        } else {
            self.block_begin("shortcut");
            self.block_end();
        }
        self.attr_padding(Rect { left: 0, top: 0, right: 2, bottom: 0 });
    }
}

#[derive(Clone, Copy)]
enum VisitControl {
    Continue,
    SkipChildren,
    Stop,
}

struct Tree<'a> {
    tail: &'a NodeCell<'a>,
    root_first: &'a NodeCell<'a>,
    root_last: &'a NodeCell<'a>,
    last_node: &'a NodeCell<'a>,
    current_node: &'a NodeCell<'a>,

    count: usize,
    checksum: u64,
}

impl<'a> Tree<'a> {
    fn new(arena: &'a Arena) -> Self {
        let root: &mut NodeCell = arena.alloc_default();
        {
            let mut r = root.borrow_mut();
            r.id = ROOT_ID;
            r.classname = "root";
            r.attributes.focusable = true;
            r.attributes.focus_well = true;
        }
        Self {
            tail: root,
            root_first: root,
            root_last: root,
            last_node: root,
            current_node: root,
            count: 1,
            checksum: ROOT_ID,
        }
    }

    fn push_child(&mut self, node: &'a NodeCell<'a>) {
        let mut n = node.borrow_mut();
        n.parent = Some(self.current_node);
        n.stack_parent = Some(self.current_node);

        {
            let mut p = self.current_node.borrow_mut();
            n.siblings.prev = p.children.last;
            n.depth = p.depth + 1;

            if let Some(child_last) = p.children.last {
                let mut child_last = child_last.borrow_mut();
                child_last.siblings.next = Some(node);
            }
            if p.children.first.is_none() {
                p.children.first = Some(node);
            }
            p.children.last = Some(node);
            p.child_count += 1;
        }

        n.prev = Some(self.tail);
        {
            let mut tail = self.tail.borrow_mut();
            tail.next = Some(node);
        }
        self.tail = node;

        self.last_node = node;
        self.current_node = node;
        self.count += 1;
        // wymix is weak, but both checksum and node.id are proper random, so... it's not *that* bad.
        self.checksum = wymix(self.checksum, n.id);
    }

    fn move_node_to_root(&mut self, node: &'a NodeCell<'a>, anchor: Option<&'a NodeCell<'a>>) {
        let mut n = node.borrow_mut();
        let Some(parent) = n.parent else {
            return;
        };

        if let Some(sibling_prev) = n.siblings.prev {
            let mut sibling_prev = sibling_prev.borrow_mut();
            sibling_prev.siblings.next = n.siblings.next;
        }
        if let Some(sibling_next) = n.siblings.next {
            let mut sibling_next = sibling_next.borrow_mut();
            sibling_next.siblings.prev = n.siblings.prev;
        }

        {
            let mut p = parent.borrow_mut();
            if opt_ptr_eq(p.children.first, Some(node)) {
                p.children.first = n.siblings.next;
            }
            if opt_ptr_eq(p.children.last, Some(node)) {
                p.children.last = n.siblings.prev;
            }
            p.child_count -= 1;
        }

        n.parent = anchor;
        n.depth = anchor.map_or(0, |n| n.borrow().depth + 1);
        n.siblings.prev = Some(self.root_last);
        n.siblings.next = None;

        self.root_last.borrow_mut().siblings.next = Some(node);
        self.root_last = node;
    }

    fn pop_stack(&mut self) {
        let current_node = self.current_node.borrow();
        let stack_parent = current_node.stack_parent.unwrap();
        self.last_node = self.current_node;
        self.current_node = stack_parent;
    }

    fn iterate_siblings(
        mut node: Option<&'a NodeCell<'a>>,
    ) -> impl Iterator<Item = &'a NodeCell<'a>> + use<'a> {
        iter::from_fn(move || {
            let n = node?;
            node = n.borrow().siblings.next;
            Some(n)
        })
    }

    fn iterate_roots(&self) -> impl Iterator<Item = &'a NodeCell<'a>> + use<'a> {
        Self::iterate_siblings(Some(self.root_first))
    }

    /// Visits all nodes under and including `root` in depth order.
    /// Starts with node `start`.
    ///
    /// WARNING: Breaks in hilarious ways if `start` is not within `root`.
    fn visit_all<T: FnMut(&'a NodeCell<'a>) -> VisitControl>(
        root: &'a NodeCell<'a>,
        start: &'a NodeCell<'a>,
        forward: bool,
        mut cb: T,
    ) {
        let root_depth = root.borrow().depth;
        let mut node = start;
        let children_idx = if forward { NodeChildren::FIRST } else { NodeChildren::LAST };
        let siblings_idx = if forward { NodeSiblings::NEXT } else { NodeSiblings::PREV };

        while {
            'traverse: {
                match cb(node) {
                    VisitControl::Continue => {
                        // Depth first search: It has a child? Go there.
                        if let Some(child) = node.borrow().children.get(children_idx) {
                            node = child;
                            break 'traverse;
                        }
                    }
                    VisitControl::SkipChildren => {}
                    VisitControl::Stop => return,
                }

                loop {
                    // If we hit the root while going up, we restart the traversal at
                    // `root` going down again until we hit `start` again.
                    let n = node.borrow();
                    if n.depth <= root_depth {
                        break 'traverse;
                    }

                    // Go to the parent's next sibling. --> Next subtree.
                    if let Some(sibling) = n.siblings.get(siblings_idx) {
                        node = sibling;
                        break;
                    }

                    // Out of children? Go back to the parent.
                    node = n.parent.unwrap();
                }
            }

            // We're done once we wrapped around to the `start`.
            !ptr::eq(node, start)
        } {}
    }
}

struct NodeMap<'a> {
    slots: &'a [Option<&'a NodeCell<'a>>],
    shift: usize,
    mask: u64,
}

impl Default for NodeMap<'static> {
    fn default() -> Self {
        Self { slots: &[None], shift: 0, mask: 0 }
    }
}

impl<'a> NodeMap<'a> {
    fn new(arena: &'a Arena, tree: &Tree<'a>) -> Self {
        let width = (4 * tree.count + 1).ilog2().max(1) as usize;
        let slots = 1 << width;
        let shift = 64 - width;
        let mask = (slots - 1) as u64;

        let slots = arena.alloc_uninit_slice(slots);
        slots.fill(MaybeUninit::new(None));
        let slots = unsafe { slice_assume_init_mut(slots) };

        for node in iter::successors(Some(tree.root_first), |&node| node.borrow().next) {
            let mut slot = node.borrow().id >> shift;
            loop {
                if slots[slot as usize].is_none() {
                    slots[slot as usize] = Some(node);
                    break;
                }
                slot = (slot + 1) & mask;
            }
        }

        Self { slots, shift, mask }
    }

    fn get(&mut self, id: u64) -> Option<&'a NodeCell<'a>> {
        let shift = self.shift;
        let mask = self.mask;
        let mut slot = (id >> shift) & mask;

        loop {
            let node = self.slots[slot as usize]?;
            if node.borrow().id == id {
                return Some(node);
            }
            slot = (slot + 1) & mask;
        }
    }
}

#[derive(Default, Clone, Copy, PartialEq, Eq)]
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
    pub offset_x: f32,
    pub offset_y: f32,
}

struct FloatAttributes {
    // Specifies the origin of the container relative to the container size. [0, 1]
    gravity_x: f32,
    gravity_y: f32,
    // Specifies an offset from the origin in cells.
    offset_x: f32,
    offset_y: f32,
}

#[derive(Clone, Copy, PartialEq, Eq)]
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

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum Overflow {
    #[default]
    Clip,
    TruncateHead,
    TruncateMiddle,
    TruncateTail,
}

// NOTE: Must not contain items that require drop().
#[derive(Default)]
struct NodeAttributes {
    float: Option<FloatAttributes>,
    position: Position,
    padding: Rect,
    bg: u32,
    fg: u32,
    reverse: bool,
    bordered: bool,
    focusable: bool,
    focus_well: bool, // Prevents focus from leaving via Tab
    focus_void: bool, // Prevents focus from entering via Tab
}

// NOTE: Must not contain items that require drop().
struct ListContent<'a> {
    selected: u64,
    // Points to the Node that holds this ListContent instance, if any>.
    selected_node: Option<&'a NodeCell<'a>>,
}

// NOTE: Must not contain items that require drop().
struct TableContent<'a> {
    columns: Vec<CoordType, &'a Arena>,
    cell_gap: Size,
}

// NOTE: Must not contain items that require drop().
struct StyledTextChunk {
    offset: usize,
    fg: u32,
    attr: Attributes,
}

const INVALID_STYLED_TEXT_CHUNK: StyledTextChunk =
    StyledTextChunk { offset: usize::MAX, fg: 0, attr: Attributes::None };

// NOTE: Must not contain items that require drop().
struct TextContent<'a> {
    text: ArenaString<'a>,
    chunks: Vec<StyledTextChunk, &'a Arena>,
    overflow: Overflow,
}

// NOTE: Must not contain items that require drop().
struct TextareaContent<'a> {
    buffer: &'a TextBufferCell,

    // Carries over between frames.
    scroll_offset: Point,
    scroll_offset_y_drag_start: CoordType,
    scroll_offset_x_max: CoordType,
    thumb_height: CoordType,
    preferred_column: CoordType,

    single_line: bool,
    has_focus: bool,
}

// NOTE: Must not contain items that require drop().
#[derive(Clone)]
struct ScrollareaContent {
    scroll_offset: Point,
    scroll_offset_y_drag_start: CoordType,
    thumb_height: CoordType,
}

// NOTE: Must not contain items that require drop().
#[derive(Default)]
enum NodeContent<'a> {
    #[default]
    None,
    List(ListContent<'a>),
    Modal(ArenaString<'a>), // title
    Table(TableContent<'a>),
    Text(TextContent<'a>),
    Textarea(TextareaContent<'a>),
    Scrollarea(ScrollareaContent),
}

// NOTE: Must not contain items that require drop().
#[derive(Default)]
struct NodeSiblings<'a> {
    prev: Option<&'a NodeCell<'a>>,
    next: Option<&'a NodeCell<'a>>,
}

impl<'a> NodeSiblings<'a> {
    const PREV: usize = 0;
    const NEXT: usize = 1;

    fn get(&self, off: usize) -> Option<&'a NodeCell<'a>> {
        match off & 1 {
            0 => self.prev,
            1 => self.next,
            _ => unreachable!(),
        }
    }
}

// NOTE: Must not contain items that require drop().
#[derive(Default)]
struct NodeChildren<'a> {
    first: Option<&'a NodeCell<'a>>,
    last: Option<&'a NodeCell<'a>>,
}

impl<'a> NodeChildren<'a> {
    const FIRST: usize = 0;
    const LAST: usize = 1;

    fn get(&self, off: usize) -> Option<&'a NodeCell<'a>> {
        match off & 1 {
            0 => self.first,
            1 => self.last,
            _ => unreachable!(),
        }
    }
}

type NodeCell<'a> = SemiRefCell<Node<'a>>;

// NOTE: Must not contain items that require drop().
#[derive(Default)]
struct Node<'a> {
    prev: Option<&'a NodeCell<'a>>,
    next: Option<&'a NodeCell<'a>>,
    stack_parent: Option<&'a NodeCell<'a>>,

    id: u64,
    classname: &'static str,
    parent: Option<&'a NodeCell<'a>>,
    depth: usize,
    siblings: NodeSiblings<'a>,
    children: NodeChildren<'a>,
    child_count: usize,

    attributes: NodeAttributes,
    content: NodeContent<'a>,

    intrinsic_size: Size,
    intrinsic_size_set: bool,
    outer: Rect,         // in screen-space, calculated during layout
    inner: Rect,         // in screen-space, calculated during layout
    outer_clipped: Rect, // in screen-space, calculated during layout, restricted to the viewport
    inner_clipped: Rect, // in screen-space, calculated during layout, restricted to the viewport
}

impl Node<'_> {
    fn outer_to_inner(&self, mut outer: Rect) -> Rect {
        let l = self.attributes.bordered;
        let t = self.attributes.bordered;
        let r = self.attributes.bordered || matches!(self.content, NodeContent::Scrollarea(..));
        let b = self.attributes.bordered;

        outer.left += self.attributes.padding.left + l as CoordType;
        outer.top += self.attributes.padding.top + t as CoordType;
        outer.right -= self.attributes.padding.right + r as CoordType;
        outer.bottom -= self.attributes.padding.bottom + b as CoordType;
        outer
    }

    fn intrinsic_to_outer(&self) -> Size {
        let l = self.attributes.bordered;
        let t = self.attributes.bordered;
        let r = self.attributes.bordered || matches!(self.content, NodeContent::Scrollarea(..));
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
                    let mut row = row.borrow_mut();
                    let mut row_height = 0;

                    for (column, cell) in Tree::iterate_siblings(row.children.first).enumerate() {
                        let mut cell = cell.borrow_mut();
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
                    let mut row = row.borrow_mut();
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
                if !self.intrinsic_size_set {
                    self.intrinsic_size.width = total_width;
                    self.intrinsic_size.height = total_height;
                    self.intrinsic_size_set = true;
                }
            }
            _ => {
                let mut max_width = 0;
                let mut total_height = 0;

                for child in Tree::iterate_siblings(self.children.first) {
                    let mut child = child.borrow_mut();
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
        if self.children.first.is_none() || self.inner.is_empty() {
            return;
        }

        match &mut self.content {
            NodeContent::Table(spec) => {
                let width = self.inner.right - self.inner.left;
                let mut x = self.inner.left;
                let mut y = self.inner.top;

                for row in Tree::iterate_siblings(self.children.first) {
                    let mut row = row.borrow_mut();
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
                        let mut cell = cell.borrow_mut();
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
                let mut content = self.children.first.unwrap().borrow_mut();

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

                let clip = content.inner_clipped;
                content.layout_children(clip);
            }
            _ => {
                let width = self.inner.right - self.inner.left;
                let x = self.inner.left;
                let mut y = self.inner.top;

                for child in Tree::iterate_siblings(self.children.first) {
                    let mut child = child.borrow_mut();
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
                    let mut child = child.borrow_mut();
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
}
