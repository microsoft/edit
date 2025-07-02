// Copyright (c) Pavel Sich.
// Licensed under the MIT License.

//! AI Dock interface for agentic AI commands

use edit::helpers::*;
use edit::input::{kbmod, vk};
use edit::tui::*;

use crate::state::*;

pub fn draw_ai_dock(ctx: &mut Context, state: &mut State) {
    if !state.ai_dock_visible {
        return;
    }

    // Calculate height based on dock size
    let dock_height = match state.ai_dock_size {
        AiDockSize::Minimized => 3,  // Just header and border
        AiDockSize::Default => 8,    // Normal size
        AiDockSize::Expanded => ctx.size().height / 2,  // 50% of screen
    };

    // Create AI dock area
    ctx.block_begin("ai_dock");
    ctx.attr_intrinsic_size(Size { width: ctx.size().width, height: dock_height });
    ctx.attr_position(Position::Right); // Position it to not interfere with status bar
    ctx.attr_background_rgba(0xFF2d2d2d); // Dark background
    ctx.attr_foreground_rgba(0xFFe0e0e0); // Light text
    {
        // Header with title and resize buttons
        // Draw border-integrated header
        ctx.block_begin("ai_header");
        ctx.attr_padding(Rect::three(1, 0, 0));
        {
            ctx.table_begin("ai_header_table");
            ctx.table_set_cell_gap(Size { width: 0, height: 0 });
            {
                ctx.table_next_row();
                
                // Title with dash prefix
                ctx.label("title_prefix1", "──");
                ctx.label("title_prefix2", " AI Assistant ");
                ctx.attr_foreground_rgba(0xFFe0e0e0); // Light text
                
                // Control buttons
                match state.ai_dock_size {
                    AiDockSize::Minimized => {
                        if ctx.button("expand_up", "▲", ButtonStyle::default()) {
                            state.ai_dock_size = AiDockSize::Default;
                            ctx.needs_rerender();
                        }
                        if ctx.button("close", "x", ButtonStyle::default()) {
                            state.ai_dock_visible = false;
                            state.ai_dock_focused = false;
                            ctx.needs_rerender();
                        }
                    },
                    AiDockSize::Default => {
                        if ctx.button("minimize_down", "▼", ButtonStyle::default()) {
                            state.ai_dock_size = AiDockSize::Minimized;
                            ctx.needs_rerender();
                        }
                        if ctx.button("expand_up", "▲", ButtonStyle::default()) {
                            state.ai_dock_size = AiDockSize::Expanded;
                            ctx.needs_rerender();
                        }
                        if ctx.button("close", "x", ButtonStyle::default()) {
                            state.ai_dock_visible = false;
                            state.ai_dock_focused = false;
                            ctx.needs_rerender();
                        }
                    },
                    AiDockSize::Expanded => {
                        if ctx.button("minimize_down", "▼", ButtonStyle::default()) {
                            state.ai_dock_size = AiDockSize::Default;
                            ctx.needs_rerender();
                        }
                        if ctx.button("close", "x", ButtonStyle::default()) {
                            state.ai_dock_visible = false;
                            state.ai_dock_focused = false;
                            ctx.needs_rerender();
                        }
                    },
                }
            }
            ctx.table_end();
        }
        ctx.block_end();

        // Show contents based on size state
        if state.ai_dock_size != AiDockSize::Minimized {
            // Prompt input area
            ctx.block_begin("ai_prompt_section");
            ctx.attr_padding(Rect::three(1, 1, 0));
            {
                ctx.label("prompt_label", "Prompt:");
                ctx.attr_overflow(Overflow::TruncateTail);
                ctx.attr_foreground_rgba(0xFFBBBBBB); // Gray label

                // Text input for AI prompt using editline
                if ctx.editline("ai_prompt_input", &mut state.ai_prompt) {
                    // Handle input changes if needed
                }
                
                if state.ai_dock_focused {
                    ctx.inherit_focus();
                }
            }
            ctx.block_end();

            // Action buttons in a single row
            ctx.block_begin("ai_buttons");
            ctx.attr_padding(Rect::three(1, 1, 0));
            {
                ctx.table_begin("button_table");
                ctx.table_set_cell_gap(Size { width: 1, height: 0 });
                {
                    ctx.table_next_row();
                    
                    if ctx.button("send", "Send (Ctrl+Enter)", ButtonStyle::default()) {
                        execute_ai_prompt(ctx, state);
                    }

                    if ctx.button("clear", "Clear", ButtonStyle::default()) {
                        state.ai_prompt.clear();
                        ctx.needs_rerender();
                    }
                }
                ctx.table_end();
            }
            ctx.block_end();

            // Output area (if there's output)
            if !state.ai_output.is_empty() {
                ctx.block_begin("ai_output_section");
                ctx.attr_padding(Rect::three(1, 1, 0));
                {
                    ctx.label("output_label", "Output:");
                    ctx.attr_overflow(Overflow::TruncateTail);
                    ctx.attr_foreground_rgba(0xFFBBBBBB); // Gray label

                    ctx.label("ai_output", &state.ai_output);
                    ctx.attr_overflow(Overflow::TruncateTail);
                    ctx.attr_foreground_rgba(0xFF90EE90); // Light green for output
                }
                ctx.block_end();
            }
        }
    }
    ctx.block_end();

    // Handle keyboard input when AI dock is focused
    if state.ai_dock_focused {
        if let Some(key) = ctx.keyboard_input() {
            if key == vk::ESCAPE {
                state.ai_dock_visible = false;
                state.ai_dock_focused = false;
                ctx.needs_rerender();
                ctx.set_input_consumed();
            } else if key == kbmod::CTRL | vk::RETURN {
                execute_ai_prompt(ctx, state);
                ctx.needs_rerender();
                ctx.set_input_consumed();
            }
        }
    }
}

fn execute_ai_prompt(ctx: &mut Context, state: &mut State) {
    if state.ai_prompt.trim().is_empty() {
        return;
    }

    // For now, just simulate AI output with a simple response
    // TODO: Integrate with actual AI API
    state.ai_output = format!("AI Response to: '{}'\n[This is a placeholder - AI integration coming soon!]", state.ai_prompt);
    
    // Optional: Insert output into current document
    if let Some(doc) = state.documents.active() {
        let mut tb = doc.buffer.borrow_mut();
        let output_text = format!("\n// AI Generated:\n// {}\n", state.ai_output);
        tb.write_canon(output_text.as_bytes());
    }

    ctx.needs_rerender();
}
