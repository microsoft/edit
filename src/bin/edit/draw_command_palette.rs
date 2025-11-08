// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use edit::arena::scratch_arena;
use edit::arena_format;
use edit::framebuffer::IndexedColor;
use edit::fuzzy::score_fuzzy;
use edit::helpers::*;
use edit::input::vk;
use edit::tui::*;

use crate::localization::*;
use crate::state::*;

const MAX_COMMAND_RESULTS: usize = 12;

#[derive(Clone, Copy)]
enum StaticCommand {
    NewFile,
    OpenFile,
    OpenRecentList,
    SaveFile,
    SaveFileAs,
    CloseFile,
    ExitApp,
    Find,
    Replace,
    GoToFile,
    GoToLine,
    Preferences,
    About,
    ToggleWordWrap,
}

#[derive(Clone, Copy)]
enum CommandAction {
    Static(StaticCommand),
    RecentFile(usize),
}

struct StaticCommandDefinition {
    label: LocId,
    shortcut: Option<&'static str>,
    keywords: &'static [&'static str],
    action: StaticCommand,
    requires_document: bool,
    requires_search: bool,
    requires_recent: bool,
}

struct CommandEntry<'a> {
    label: &'a str,
    shortcut: Option<&'static str>,
    keywords: &'static [&'static str],
    action: CommandAction,
    enabled: bool,
}

const STATIC_COMMANDS: &[StaticCommandDefinition] = &[
    StaticCommandDefinition {
        label: LocId::FileNew,
        shortcut: Some("Ctrl+N"),
        keywords: &["new"],
        action: StaticCommand::NewFile,
        requires_document: false,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::FileOpen,
        shortcut: Some("Ctrl+O"),
        keywords: &["open", "file"],
        action: StaticCommand::OpenFile,
        requires_document: false,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::FileOpenRecent,
        shortcut: None,
        keywords: &["recent"],
        action: StaticCommand::OpenRecentList,
        requires_document: false,
        requires_search: false,
        requires_recent: true,
    },
    StaticCommandDefinition {
        label: LocId::FileSave,
        shortcut: Some("Ctrl+S"),
        keywords: &["save"],
        action: StaticCommand::SaveFile,
        requires_document: true,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::FileSaveAs,
        shortcut: Some("Ctrl+Shift+S"),
        keywords: &["save as"],
        action: StaticCommand::SaveFileAs,
        requires_document: true,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::FileClose,
        shortcut: Some("Ctrl+W"),
        keywords: &["close"],
        action: StaticCommand::CloseFile,
        requires_document: true,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::FileExit,
        shortcut: Some("Ctrl+Q"),
        keywords: &["quit", "exit"],
        action: StaticCommand::ExitApp,
        requires_document: false,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::EditFind,
        shortcut: Some("Ctrl+F"),
        keywords: &["find", "search"],
        action: StaticCommand::Find,
        requires_document: true,
        requires_search: true,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::EditReplace,
        shortcut: Some("Ctrl+R"),
        keywords: &["replace"],
        action: StaticCommand::Replace,
        requires_document: true,
        requires_search: true,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::ViewGoToFile,
        shortcut: Some("Ctrl+P"),
        keywords: &["switch file"],
        action: StaticCommand::GoToFile,
        requires_document: true,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::FileGoto,
        shortcut: Some("Ctrl+G"),
        keywords: &["goto line"],
        action: StaticCommand::GoToLine,
        requires_document: true,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::EditPreferences,
        shortcut: None,
        keywords: &["prefs", "settings"],
        action: StaticCommand::Preferences,
        requires_document: false,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::HelpAbout,
        shortcut: None,
        keywords: &["about"],
        action: StaticCommand::About,
        requires_document: false,
        requires_search: false,
        requires_recent: false,
    },
    StaticCommandDefinition {
        label: LocId::ViewWordWrap,
        shortcut: Some("Alt+Z"),
        keywords: &["wrap"],
        action: StaticCommand::ToggleWordWrap,
        requires_document: true,
        requires_search: false,
        requires_recent: false,
    },
];

pub fn draw_command_palette(ctx: &mut Context, state: &mut State) {
    if state.command_palette_reset_selection {
        state.command_palette_selection = 0;
        state.command_palette_reset_selection = false;
    }

    ctx.modal_begin("command-palette", loc(LocId::CommandPaletteTitle));
    ctx.attr_focus_well();
    ctx.attr_padding(Rect::three(1, 2, 1));

    let mut close = false;
    let mut activate: Option<CommandAction> = None;

    if ctx.contains_focus() && ctx.consume_shortcut(vk::ESCAPE) {
        close = true;
    }

    ctx.block_begin("filter");
    ctx.attr_padding(Rect::three(0, 0, 1));
    ctx.attr_intrinsic_size(Size { width: COORD_TYPE_SAFE_MAX, height: 1 });
    if ctx.editline("command-filter", &mut state.command_palette_filter) {
        state.command_palette_selection = 0;
    }
    ctx.block_end();

    let entries = build_command_entries(state);
    let filtered = filter_commands(entries, state.command_palette_filter.trim());
    let mut selection = state.command_palette_selection;

    let visible_len = filtered.len().min(MAX_COMMAND_RESULTS);
    if visible_len == 0 {
        selection = 0;
    } else {
        selection = selection.min(visible_len.saturating_sub(1));
    }

    ctx.block_begin("results");
    ctx.attr_padding(Rect::three(0, 0, 1));

    if visible_len == 0 {
        ctx.attr_foreground_rgba(ctx.indexed_alpha(IndexedColor::BrightBlack, 3, 4));
        ctx.label("no-results", loc(LocId::CommandPaletteNoResults));
        ctx.attr_foreground_rgba(ctx.indexed(IndexedColor::Foreground));
    } else {
        ctx.list_begin("command-list");
        ctx.inherit_focus();
        {
            for (idx, entry) in filtered.iter().take(visible_len).enumerate() {
                let mut label_owned = None;
                let label_text: &str = if let Some(shortcut) = entry.shortcut {
                    label_owned =
                        Some(arena_format!(ctx.arena(), "{}    {}", entry.label, shortcut));
                    label_owned.as_deref().unwrap()
                } else {
                    entry.label
                };

                match ctx.list_item(selection == idx, label_text) {
                    ListSelection::Activated => {
                        selection = idx;
                        if entry.enabled {
                            activate = Some(entry.action);
                        }
                    }
                    ListSelection::Selected => selection = idx,
                    ListSelection::Unchanged => {}
                }
            }
        }
        ctx.list_end();
    }
    ctx.block_end();

    drop(filtered);
    state.command_palette_selection = selection;

    if ctx.modal_end() {
        close = true;
    }

    if let Some(action) = activate {
        execute_command(ctx, state, action);
        close = true;
    }

    if close {
        state.wants_command_palette = false;
        state.command_palette_filter.clear();
        state.command_palette_selection = 0;
    }
}

fn build_command_entries<'a>(state: &'a State) -> Vec<CommandEntry<'a>> {
    let mut entries = Vec::new();

    for def in STATIC_COMMANDS {
        let has_doc = state.documents.active().is_some();
        let search_available = !matches!(state.wants_search.kind, StateSearchKind::Disabled);
        let recent_available = !state.recent_files.is_empty();
        let enabled = (!def.requires_document || has_doc)
            && (!def.requires_search || search_available)
            && (!def.requires_recent || recent_available);

        entries.push(CommandEntry {
            label: loc(def.label),
            shortcut: def.shortcut,
            keywords: def.keywords,
            action: CommandAction::Static(def.action),
            enabled,
        });
    }

    for (idx, entry) in state.recent_files.iter().enumerate() {
        entries.push(CommandEntry {
            label: entry.as_str(),
            shortcut: None,
            keywords: &["recent"],
            action: CommandAction::RecentFile(idx),
            enabled: true,
        });
    }

    entries
}

fn filter_commands<'a>(entries: Vec<CommandEntry<'a>>, needle: &str) -> Vec<CommandEntry<'a>> {
    if needle.is_empty() {
        return entries;
    }

    let scratch = scratch_arena(None);
    let mut matches = Vec::new();
    for entry in entries {
        let mut best = score_fuzzy(&scratch, entry.label, needle, true).0;
        if best == 0 {
            for keyword in entry.keywords {
                let (score, _) = score_fuzzy(&scratch, keyword, needle, true);
                best = best.max(score);
            }
        }
        if best > 0 {
            matches.push((best, entry));
        }
    }

    matches.sort_by(|a, b| b.0.cmp(&a.0));
    matches.into_iter().map(|(_, entry)| entry).collect()
}

fn execute_command(ctx: &mut Context, state: &mut State, action: CommandAction) {
    match action {
        CommandAction::Static(cmd) => match cmd {
            StaticCommand::NewFile => {
                draw_add_untitled_document(ctx, state);
                ctx.needs_rerender();
            }
            StaticCommand::OpenFile => {
                state.wants_file_picker = StateFilePicker::Open;
                ctx.needs_rerender();
            }
            StaticCommand::OpenRecentList => {
                state.wants_recent_files = true;
                ctx.needs_rerender();
            }
            StaticCommand::SaveFile => {
                state.wants_save = true;
                ctx.needs_rerender();
            }
            StaticCommand::SaveFileAs => {
                state.wants_file_picker = StateFilePicker::SaveAs;
                ctx.needs_rerender();
            }
            StaticCommand::CloseFile => {
                state.wants_close = true;
                ctx.needs_rerender();
            }
            StaticCommand::ExitApp => {
                state.wants_exit = true;
                ctx.needs_rerender();
            }
            StaticCommand::Find => {
                if state.wants_search.kind != StateSearchKind::Disabled {
                    state.wants_search.kind = StateSearchKind::Search;
                    state.wants_search.focus = true;
                    ctx.needs_rerender();
                }
            }
            StaticCommand::Replace => {
                if state.wants_search.kind != StateSearchKind::Disabled {
                    state.wants_search.kind = StateSearchKind::Replace;
                    state.wants_search.focus = true;
                    ctx.needs_rerender();
                }
            }
            StaticCommand::GoToFile => {
                state.wants_go_to_file = true;
                ctx.needs_rerender();
            }
            StaticCommand::GoToLine => {
                state.wants_goto = true;
                ctx.needs_rerender();
            }
            StaticCommand::Preferences => {
                state.wants_preferences = true;
                ctx.needs_rerender();
            }
            StaticCommand::About => {
                state.wants_about = true;
                ctx.needs_rerender();
            }
            StaticCommand::ToggleWordWrap => {
                if let Some(doc) = state.documents.active_mut() {
                    let mut tb = doc.buffer.borrow_mut();
                    let wrap_enabled = tb.is_word_wrap_enabled();
                    tb.set_word_wrap(!wrap_enabled);
                    tb.make_cursor_visible();
                    ctx.needs_rerender();
                }
            }
        },
        CommandAction::RecentFile(idx) => {
            if let Some(entry) = state.recent_files.get(idx) {
                let path = entry.as_path().to_path_buf();
                let prefs = state.preferences.clone();
                match state.documents.add_file_path(&path) {
                    Ok(doc) => {
                        prefs.apply_to_document(doc);
                        state.mark_file_recent_path(&path);
                        ctx.needs_rerender();
                    }
                    Err(err) => error_log_add(ctx, state, err),
                }
            }
        }
    }
}
