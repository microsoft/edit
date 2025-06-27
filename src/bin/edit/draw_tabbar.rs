// Copyright (c) Pavel Sich.
// Licensed under the MIT License.

use edit::framebuffer::IndexedColor;
use edit::helpers::*;
use edit::tui::*;

use crate::state::*;

/// Store information about a tab for rendering
struct TabInfo {
    index: usize,
    filename: String,
    is_dirty: bool,
    is_active: bool,
}

/// Draw the tab bar showing all open documents
pub fn draw_tabbar(ctx: &mut Context, state: &mut State) {
    if state.documents.len() <= 1 {
        // Don't show tab bar if there's only one document or no documents
        return;
    }

    // Collect tab information first to avoid borrowing conflicts
    let active_index = state.documents.active_index();
    let tab_infos: Vec<TabInfo> = state.documents.iter().enumerate().map(|(index, doc)| {
        TabInfo {
            index,
            filename: doc.filename.clone(),
            is_dirty: doc.buffer.borrow().is_dirty(),
            is_active: index == active_index,
        }
    }).collect();

    ctx.block_begin("tabbar");
    ctx.attr_background_rgba(ctx.indexed(IndexedColor::Background));
    ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Foreground));
    ctx.attr_intrinsic_size(Size { width: 0, height: 1 });
    {
        ctx.table_begin("tabs");
        ctx.table_set_cell_gap(Size { width: 0, height: 0 });
        ctx.table_next_row();
        
        for tab_info in &tab_infos {
            // Create tab text with filename and dirty indicator
            let mut tab_text = String::new();
            tab_text.push_str(&tab_info.filename);
            if tab_info.is_dirty {
                tab_text.push('*');
            }
            
            // [TODO] Include close button in tab text if there are multiple tabs
            if tab_infos.len() > 1 {
                tab_text = format!(" {} ", tab_text);
            } else {
                tab_text = format!(" {} ", tab_text);
            }
            
            // Create button ID by mixing the index into the class name
            ctx.next_block_id_mixin(tab_info.index as u64);
            
            if ctx.button("tab", &tab_text, ButtonStyle::default().bracketed(false)) {
                // Switch to this document
                state.documents.set_active_index(tab_info.index);
                ctx.needs_rerender();
            }
            
            // Style the active tab differently
            if tab_info.is_active {
                ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlue));
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::BrightWhite));
            } else {
                ctx.attr_background_rgba(ctx.indexed(IndexedColor::BrightBlack));
                ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::White));
            }
        }
        
        ctx.table_end();
    }
    ctx.block_end();
}
