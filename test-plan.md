# Accessibility VT Sequence (OSC 200) — Manual Test Plan

Test with a screen reader active (Narrator, NVDA, or JAWS) and the Terminal build
from `dev/cazamor/a11y/vt-seq-prototype`.

## Prerequisites

- Build Edit in debug mode: `cargo build`
- Have at least two text files available for testing (e.g., `test1.txt`, `test2.txt`)
- Have the screen reader running before launching Edit

---

## 1. Lifecycle: Per-Frame Suppress / Restore

### 1a. Launch announces editor context
1. Open a terminal with the screen reader active
2. Run `edit test1.txt`
3. **Expected announcement**: "test1.txt - Edit"
4. The terminal should NOT announce raw screen repaint output (OSC 200;0
   is sent before each frame, and OSC 200;1 re-enables announcing after).

### 1a-ii. Launch with untitled file
1. Run `edit` (no file argument)
2. **Expected announcement**: "New File - Edit"

### 1b. Exit restores terminal announcing
1. Press `Ctrl+Q` to exit Edit
2. Run a normal command like `echo hello`
3. **Expected**: The terminal resumes announcing output normally.
   Because OSC 200;1 is emitted at the end of every frame, the terminal
   is already in the "announcing" state when Edit exits.

### 1c. Panic/crash restores terminal announcing
1. If possible, force a crash (debug build panic)
2. **Expected**: Terminal announcing is restored. Because OSC 200 suppress/restore
   is per-frame (not startup/exit), the terminal is in the "announcing" state
   between frames. A crash between frames leaves announcing enabled.

---

## 2. Search & Replace

### 2a. Search — match found
1. Open a file with known content (e.g., a file containing "hello")
2. Press `Ctrl+F` → **Expected announcement**: "Find"
3. Type `hello` and press `Enter`
4. **Expected announcement**: "Match found, Line N" (where N is the line number of the match)

### 2b. Search — no matches
1. In the search bar, type a string that doesn't exist (e.g., `zzzzxyzzy`)
2. Press `Enter`
3. **Expected announcement**: "No matches found"
4. Verify the search input has a red background (visual confirmation)

### 2c. Find and Replace
1. Press `Ctrl+R` → **Expected announcement**: "Replace"
2. Type a needle that exists, tab to the replacement field, type replacement
3. Press `Enter` (single replace)
4. **Expected announcement**: "Replaced, Match found, Line N" (replacement done, next match highlighted)

### 2c-ii. Replace — last match
1. Replace until no more matches remain
2. **Expected announcement**: "Replaced, No matches found"

### 2d. Replace All
1. In the Replace bar, press `Ctrl+Alt+Enter`
2. **Expected announcement**: "All replaced"

### 2e. F3 — Find Next
1. Close the search bar with `Escape`
2. Press `F3`
3. **Expected announcement**: "Match found, Line N" or "No matches found"

### 2f. Search options — toggle announcements
1. Open Find with `Ctrl+F`
2. Tab to the "Match Case" checkbox and press `Space`
3. **Expected announcement**: "Match Case: On"
4. Press `Space` again
5. **Expected announcement**: "Match Case: Off"
6. Tab to "Whole Word" and press `Space`
7. **Expected announcement**: "Whole Word: On"
8. Tab to "Use Regex" and press `Space`
9. **Expected announcement**: "Use Regex: On"

---

## 3. File Operations

### 3a. Save existing file
1. Open `test1.txt`, make an edit (type something)
2. Press `Ctrl+S`
3. **Expected announcement**: "Saved: test1.txt"

### 3b. Save untitled file (triggers Save As)
1. Press `Ctrl+N` to create a new untitled document
2. Type some text, then press `Ctrl+S`
3. **Expected announcement**: "Save As…" followed by
   "Save As…, Folder: <path>, File name: Untitled.txt" (file picker context)

### 3c. Save As via shortcut
1. Press `Ctrl+Shift+S`
2. **Expected announcement**: "Save As…" followed by file picker context announcement

### 3d. Open file via shortcut
1. Press `Ctrl+O`
2. **Expected announcement**: "Open File…" followed by
   "Open File…, Folder: <path>, File name: " (file picker context)
3. Navigate to a file using arrow keys
4. **Expected**: Each item announces "<name>, selected" as you navigate
5. Press `Enter`
6. **Expected announcement**: "Open File…: <filename>" (file opened)

### 3e. Open file via menu
1. Press `F10` to focus the menubar
2. Navigate to File → Open File…
3. **Expected announcement**: "Open File…" followed by file picker context

### 3e-ii. File picker — folder navigation
1. In the file picker, select a directory and press `Enter`
2. **Expected announcement**: "Folder: <new path>" (folder change announced)
3. Press `Backspace` or `Alt+Up` to go up
4. **Expected announcement**: "Folder: <parent path>"

### 3f. Save As completion
1. From the Save As file picker, type a name and press `Enter`
2. **Expected announcement**: "Saved: <filename>"

### 3g. File overwrite warning
1. In Save As, choose a filename that already exists
2. **Expected announcement**: "Confirm Save As: File already exists. Do you want to overwrite it?"
3. Press `Y` or `N` to dismiss

---

## 4. Dialogs

### 4a. Unsaved Changes dialog
1. Open a file, make an edit (so the `*` dirty indicator appears)
2. Press `Ctrl+W` to close
3. **Expected announcement**: "Unsaved Changes: Do you want to save the changes you made?"
4. Press `N` (Don't Save)
5. Reopen the file, make an edit, press `Ctrl+W` again
6. **Expected**: The announcement fires again (not suppressed from previous)

### 4b. Unsaved Changes during exit
1. Open a file, make an edit
2. Press `Ctrl+Q`
3. **Expected announcement**: "Unsaved Changes: Do you want to save the changes you made?"
4. Press `Cancel`
5. **Expected**: Editor stays open, no further announcements

### 4c. Close clean file — no dialog
1. Open a file, make NO edits
2. Press `Ctrl+W`
3. **Expected**: File closes silently. NO "Unsaved Changes" announcement.

### 4d. Go to Line dialog
1. Press `Ctrl+G`
2. **Expected announcement**: "Go to Line:Column…"
3. Type `10:5` and press `Enter`
4. **Expected announcement**: "10:5" (the position navigated to)

### 4e. Go to Line — invalid input
1. Press `Ctrl+G`
2. Type `abc` and press `Enter`
3. **Expected**: Red background on input (visual), no position announcement
   (the goto_invalid flag is set, but we don't announce "invalid" — the visual indicator suffices)

### 4f. About dialog
1. Press `F10`, navigate to Help → About
2. **Expected announcement**: "Microsoft Edit Version X.Y.Z"

### 4g. Go to File dialog
1. Open multiple files: `edit test1.txt test2.txt`
2. Press `Ctrl+P`
3. **Expected announcement**: "Go to File"
4. Use arrow keys to navigate the file list
5. **Expected**: Each file announces "<filename>, selected" as you navigate
6. Press `Enter` to switch to a file
7. **Expected announcement**: "<filename>" of the newly active document

### 4h. Go to File from statusbar
1. Click the filename in the bottom-right status bar
2. **Expected announcement**: "Go to File"

---

## 5. Errors

### 5a. File open error
1. Try to open a nonexistent file path from the file picker
   (or a file with no read permissions)
2. **Expected announcement**: "Error: <error message>"
3. An error dialog should also appear visually

### 5b. ICU missing (if applicable)
1. If ICU is not installed, try `Ctrl+F` (Find)
2. **Expected announcement**: "Error: <ICU missing message>"
3. Search is disabled

---

## 6. Status Bar Toggles

### 6a. Line ending toggle
1. Click the `CRLF` / `LF` button in the status bar
2. **Expected announcement**: The NEW mode — e.g., if it was "CRLF", announcement should be "LF"
3. Click again
4. **Expected announcement**: "CRLF"

### 6b. Overtype mode off
1. Press `Insert` key to enter overtype mode (the "OVR" indicator appears in the status bar)
2. Click the `OVR` button in the status bar
3. **Expected announcement**: "Insert"

### 6c. Word wrap toggle via menu
1. Press `F10`, navigate to View → Word Wrap
2. **Expected announcement**: "Word Wrap: On" (if it was off) or "Word Wrap: Off" (if it was on)

### 6d. Encoding change — Convert
1. Click the encoding button in the status bar (e.g., "UTF-8")
2. Click "Convert"
3. Select a different encoding (e.g., "ISO-8859-1")
4. **Expected announcement**: "ISO-8859-1" (the canonical encoding name)

### 6e. Encoding change — Reopen
1. Click the encoding button, click "Reopen"
2. Select a different encoding
3. **Expected announcement**: The encoding name (or an error if it fails)

---

## 7. Clipboard Warning (Large Copy)

### 7a. Large clipboard warning
1. Open a large file (>128 KiB of text)
2. Select all (`Ctrl+A`), copy (`Ctrl+C`)
3. **Expected announcement**: "Warning: Text you copy is shared with the terminal clipboard."
4. Dismiss the dialog (Yes/No/Always)
5. **Expected**: Announcement does not repeat until the next large copy

### 7b. Small clipboard — no warning
1. Select a small amount of text, copy
2. **Expected**: No clipboard warning announcement

---

## 8. Menu-Triggered Shortcuts (verify menu path matches keyboard path)

For each of these, confirm the announcement matches what the keyboard shortcut produces:

| Menu Path                 | Keyboard     | Expected Announcement |
|---------------------------|--------------|-----------------------|
| File → Open File…         | Ctrl+O       | "Open File…"          |
| File → Save As            | (menu only)  | "Save As…"            |
| Edit → Find               | Ctrl+F       | "Find"                |
| Edit → Replace            | Ctrl+R       | "Replace"             |
| View → Go to File         | Ctrl+P       | "Go to File"          |
| View → Go to Line:Column… | Ctrl+G      | "Go to Line:Column…"  |
| View → Word Wrap          | Alt+Z        | "Word Wrap: On/Off"   |
| Help → About              | (menu only)  | "Microsoft Edit ..."  |

---

## 9. Edge Cases

### 9a. Rapid repeated searches
1. Open Find, type a term, press `Enter` repeatedly
2. **Expected**: Each press produces a "Match found" or "No matches found" announcement.
   Announcements should not queue up excessively (each is emitted per frame).

### 9b. Multiple file close during exit
1. Open 3 files, edit all 3
2. Press `Ctrl+Q`
3. **Expected**: "Unsaved Changes" dialog appears for each dirty file in turn,
   with an announcement each time.

### 9c. Terminal without OSC 200 support
1. Run Edit in a terminal that does NOT support OSC 200 (e.g., older terminal)
2. **Expected**: Edit works normally. OSC 200 sequences are silently ignored
   by terminals that don't understand them. No crashes or visual artifacts.
