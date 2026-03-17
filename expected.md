# Accessibility VT Sequence (OSC 200) - Expected Behaviors

## 1. Lifecycle: Per-Frame Suppress / Restore

### 1a. Launch announces editor context

### 1a-ii. Launch with untitled file
✅

### 1b. Exit restores terminal announcing
✅

### 1c. Panic/crash restores terminal announcing
✅
---

## 2. Search & Replace

### 2a. Search — match found

### 2b. Search — no matches

### 2c. Find and Replace

### 2c-ii. Replace — last match

### 2d. Replace All

### 2e. F3 — Find Next

### 2f. Search options — toggle announcements


---

## 3. File Operations

### 3a. Save existing file

### 3b. Save untitled file (triggers Save As)

### 3c. Save As via shortcut

### 3d. Open file via shortcut

### 3e. Open file via menu

### 3e-ii. File picker — folder navigation

### 3f. Save As completion

### 3g. File overwrite warning

---

## 4. Dialogs

### 4a. Unsaved Changes dialog

### 4b. Unsaved Changes during exit

### 4c. Close clean file — no dialog

### 4d. Go to Line dialog

### 4e. Go to Line — invalid input

### 4f. About dialog

### 4g. Go to File dialog

### 4h. Go to File from statusbar

---

## 5. Errors

### 5a. File open error

### 5b. ICU missing (if applicable)

---

## 6. Status Bar Toggles

### 6a. Line ending toggle

### 6b. Overtype mode off

### 6c. Word wrap toggle via menu

### 6d. Encoding change — Convert

### 6e. Encoding change — Reopen

---

## 7. Clipboard Warning (Large Copy)

### 7a. Large clipboard warning

### 7b. Small clipboard — no warning

---

## 8. Menu-Triggered Shortcuts (verify menu path matches keyboard path)

---

## 9. Edge Cases

### 9a. Rapid repeated searches

### 9b. Multiple file close during exit

### 9c. Terminal without OSC 200 support

V=1

