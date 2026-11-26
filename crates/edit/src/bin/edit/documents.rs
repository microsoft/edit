// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

use std::ffi::OsStr;
use std::fs::File;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use std::{fs, io};

use edit::buffer::{RcTextBuffer, TextBuffer};
use edit::helpers::{CoordType, Point};
use edit::lsh::{FILE_ASSOCIATIONS, Language, process_file_associations};
use edit::{path, sys};

use crate::apperr;
use crate::settings::Settings;
use crate::state::DisplayablePathBuf;

#[derive(PartialEq, Eq)]
struct FileVersion {
    id: sys::FileId,
    len: u64,
    modified: Option<SystemTime>,
}

fn file_version(file: &File, path: &Path) -> io::Result<FileVersion> {
    let metadata = file.metadata()?;
    Ok(FileVersion {
        id: sys::file_id(Some(file), path)?,
        len: metadata.len(),
        modified: metadata.modified().ok(),
    })
}

fn version_at_path(path: &Path) -> io::Result<Option<FileVersion>> {
    match File::open(path) {
        Ok(file) => file_version(&file, path).map(Some),
        Err(err) if err.kind() == io::ErrorKind::NotFound => Ok(None),
        Err(err) => Err(err),
    }
}

#[derive(PartialEq, Eq, Debug)]
pub enum DiskChange {
    None,
    Reloaded,
    Conflict,
}

pub struct Document {
    pub buffer: RcTextBuffer,
    pub path: Option<PathBuf>,
    pub dir: Option<DisplayablePathBuf>,
    pub filename: String,
    pub file_id: Option<sys::FileId>,
    disk_version: Option<FileVersion>,
    conflict_reported: bool,
    pending_version: Option<FileVersion>,
    pub new_file_counter: usize,
    pub language_override: Option<Option<&'static Language>>,
}

impl Document {
    pub fn disk_differs(&self) -> io::Result<bool> {
        let Some(path) = self.path.as_deref() else { return Ok(false) };
        Ok(version_at_path(path)? != self.disk_version)
    }

    pub fn check_for_changes(&mut self) -> apperr::Result<DiskChange> {
        let Some(path) = self.path.as_deref() else { return Ok(DiskChange::None) };
        let version = match version_at_path(path) {
            Ok(version) => version,
            Err(_) if self.conflict_reported => return Ok(DiskChange::None),
            Err(err) => {
                self.conflict_reported = true;
                return Err(err.into());
            }
        };
        if version == self.disk_version {
            self.conflict_reported = false;
            self.pending_version = None;
            return Ok(DiskChange::None);
        }
        if self.buffer.borrow().is_dirty() {
            if !self.conflict_reported {
                self.conflict_reported = true;
                return Ok(DiskChange::Conflict);
            }
            return Ok(DiskChange::None);
        }
        // A missing file may be in the middle of an atomic replacement. Retry later.
        if version.is_none() {
            return Ok(DiskChange::None);
        }
        // Wait for the same version on two polls, so an in-place formatter's
        // truncate/write sequence is less likely to be mistaken for a complete file.
        if self.pending_version.as_ref() != version.as_ref() {
            self.pending_version = version;
            return Ok(DiskChange::None);
        }
        match self.reread(None) {
            Ok(()) => Ok(DiskChange::Reloaded),
            Err(_) if self.conflict_reported => Ok(DiskChange::None),
            Err(err) => {
                self.conflict_reported = true;
                Err(err)
            }
        }
    }

    pub fn save(&mut self, new_path: Option<PathBuf>) -> apperr::Result<()> {
        self.save_inner(new_path, false)
    }

    pub fn save_overwriting(&mut self) -> apperr::Result<()> {
        self.save_inner(None, true)
    }

    fn save_inner(&mut self, new_path: Option<PathBuf>, overwrite: bool) -> apperr::Result<()> {
        let path = new_path.as_deref().unwrap_or_else(|| self.path.as_ref().unwrap().as_path());
        if new_path.is_none() && !overwrite && self.disk_differs()? {
            return Err(io::Error::new(io::ErrorKind::AlreadyExists, "file changed on disk").into());
        }
        let mut file = DocumentManager::open_for_writing(path)?;
        self.buffer.borrow_mut().write_file(&mut file)?;
        let version = file_version(&file, path)?;
        self.file_id = Some(sys::file_id(Some(&file), path)?);
        self.disk_version = Some(version);
        self.conflict_reported = false;
        self.pending_version = None;

        if let Some(path) = new_path {
            self.set_path(path);
        }
        Ok(())
    }

    pub fn reread(&mut self, encoding: Option<&'static str>) -> apperr::Result<()> {
        let path = self.path.as_ref().unwrap().as_path();
        let mut file = DocumentManager::open_for_reading(path)?;
        let version = file_version(&file, path)?;
        self.buffer.borrow_mut().reload_file(&mut file, encoding, || {
            if version_at_path(path)?.as_ref() == Some(&version) {
                Ok(())
            } else {
                Err(io::Error::other("file changed while reading").into())
            }
        })?;
        self.file_id = Some(sys::file_id(Some(&file), path)?);
        self.disk_version = Some(version);
        self.conflict_reported = false;
        self.pending_version = None;
        Ok(())
    }

    fn set_path(&mut self, path: PathBuf) {
        let filename = path.file_name().unwrap_or_default().to_string_lossy().into_owned();
        let dir = path.parent().map(ToOwned::to_owned).unwrap_or_default();

        self.filename = filename;
        self.dir = Some(DisplayablePathBuf::from_path(dir));
        self.path = Some(path);

        self.buffer.borrow_mut().set_ruler(if self.filename == "COMMIT_EDITMSG" { 72 } else { 0 });
        self.update_language();
    }

    pub fn auto_detect_language(&mut self) {
        self.language_override = None;
        self.update_language();
    }

    pub fn override_language(&mut self, lang: Option<&'static Language>) {
        self.language_override = Some(lang);
        self.update_language();
    }

    fn update_language(&mut self) {
        self.buffer.borrow_mut().set_language(self.get_language());
    }

    fn get_language(&self) -> Option<&'static Language> {
        if let Some(lang) = self.language_override {
            return lang;
        }

        if let Some(path) = &self.path {
            let settings = Settings::borrow();
            if let Some(lang) = process_file_associations(&settings.file_associations, path) {
                return Some(lang);
            }
            if let Some(lang) = process_file_associations(FILE_ASSOCIATIONS, path) {
                return Some(lang);
            }
        }

        None
    }

    /// Moves the cursor to a 1-based line/char position.
    /// A negative line counts backwards from the end
    /// of the document, e.g. -1 is the last line.
    pub fn cursor_move_to_goto(&self, goto: Point) {
        let mut tb = self.buffer.borrow_mut();
        let x = goto.x.saturating_sub(1);
        let y = if goto.y < 0 {
            tb.logical_line_count().saturating_add(goto.y)
        } else {
            goto.y.saturating_sub(1)
        };
        tb.cursor_move_to_logical(Point { x, y });
    }
}

#[derive(Default)]
pub struct DocumentManager {
    list: Vec<Document>,
}

impl DocumentManager {
    #[inline]
    pub fn len(&self) -> usize {
        self.list.len()
    }

    pub fn has_files(&self) -> bool {
        self.list.iter().any(|doc| doc.path.is_some())
    }

    #[inline]
    pub fn active(&self) -> Option<&Document> {
        self.list.last()
    }

    #[inline]
    pub fn active_mut(&mut self) -> Option<&mut Document> {
        self.list.last_mut()
    }

    pub fn update_active<F: FnMut(&Document) -> bool>(&mut self, mut func: F) -> bool {
        let Some(idx) = self.list.iter().rposition(&mut func) else {
            return false;
        };

        // Already active (= last) document matched? Nothing to do.
        if idx == self.list.len() - 1 {
            return false;
        }

        // Otherwise, move the matched document to the end of the list so it becomes active.
        // Uses unsafe, because `rotate_left()` is horrendously bad with -Copt-level=s
        // (it's really almost comical) and I just don't tolerate that.
        // If I'm dead and you're looking to rewrite this use `list.push(list.remove(idx))`.
        unsafe {
            let beg = self.list.as_mut_ptr();
            let doc = beg.add(idx);
            let last = beg.add(self.list.len() - 1);
            let amount = self.list.len() - idx - 1;
            let mut temp = std::mem::MaybeUninit::<Document>::uninit();

            // Make a backup of the document
            std::ptr::copy_nonoverlapping(doc, temp.as_mut_ptr(), 1);
            // Shift the rest to the front
            std::ptr::copy(doc.add(1), doc, amount);
            // Move the backup to the end
            std::ptr::copy_nonoverlapping(temp.as_ptr(), last, 1);
        }

        true
    }

    pub fn remove_active(&mut self) {
        self.list.pop();
    }

    pub fn add_untitled(&mut self) -> apperr::Result<&mut Document> {
        let buffer = Self::create_buffer()?;
        let mut doc = Document {
            buffer,
            path: None,
            dir: Default::default(),
            filename: Default::default(),
            file_id: None,
            disk_version: None,
            conflict_reported: false,
            pending_version: None,
            new_file_counter: 0,
            language_override: None,
        };
        self.gen_untitled_name(&mut doc);

        // In the future this could use push_mut, but it's unstable right now. As usual.
        self.list.push(doc);
        Ok(self.list.last_mut().unwrap())
    }

    pub fn gen_untitled_name(&self, doc: &mut Document) {
        let mut new_file_counter = 0;
        for doc in &self.list {
            new_file_counter = new_file_counter.max(doc.new_file_counter);
        }
        new_file_counter += 1;

        doc.filename = format!("Untitled-{new_file_counter}.txt");
        doc.new_file_counter = new_file_counter;
    }

    pub fn add_file_path(&mut self, path: &Path) -> apperr::Result<&mut Document> {
        let path = path::normalize(path);

        let mut file = match File::open(&path) {
            Ok(file) => Some(file),
            Err(err) if err.kind() == io::ErrorKind::NotFound => None,
            Err(err) => return Err(err.into()),
        };

        let disk_version = file.as_ref().map(|file| file_version(file, &path)).transpose()?;
        let file_id = file.as_ref().map(|file| sys::file_id(Some(file), &path)).transpose()?;

        // Check if the file is already open.
        if file_id.is_some() && self.update_active(|doc| doc.file_id == file_id) {
            let doc = self.active_mut().unwrap();
            return Ok(doc);
        }

        let buffer = Self::create_buffer()?;
        {
            if let Some(file) = &mut file {
                let mut tb = buffer.borrow_mut();
                tb.read_file(file, None)?;
            }
        }

        let mut doc = Document {
            buffer,
            path: None,
            dir: None,
            filename: Default::default(),
            file_id,
            disk_version,
            conflict_reported: false,
            pending_version: None,
            new_file_counter: 0,
            language_override: None,
        };
        doc.set_path(path);

        if let Some(active) = self.active()
            && active.path.is_none()
            && active.file_id.is_none()
            && !active.buffer.borrow().is_dirty()
        {
            // If the current document is a pristine Untitled document with no
            // name and no ID, replace it with the new document.
            self.remove_active();
        }

        self.list.push(doc);
        Ok(self.list.last_mut().unwrap())
    }

    /// Reopens the document named by a pending external-change warning, if still open.
    pub fn reopen_path(&mut self, path: &Path) -> apperr::Result<()> {
        if let Some(doc) = self.list.iter_mut().find(|doc| doc.path.as_deref() == Some(path)) {
            doc.reread(None)?;
        }
        Ok(())
    }

    pub fn check_for_changes(&mut self) -> Vec<(PathBuf, apperr::Result<DiskChange>)> {
        self.list
            .iter_mut()
            .filter_map(|doc| {
                let result = doc.check_for_changes();
                if matches!(result, Ok(DiskChange::None)) {
                    None
                } else {
                    Some((doc.path.clone()?, result))
                }
            })
            .collect()
    }

    pub fn reflow_all(&self) {
        for doc in &self.list {
            let mut tb = doc.buffer.borrow_mut();
            tb.reflow();
        }
    }

    pub fn open_for_reading(path: &Path) -> apperr::Result<File> {
        File::open(path).map_err(apperr::Error::from)
    }

    pub fn open_for_writing(path: &Path) -> apperr::Result<File> {
        // Error handling for directory creation and file writing

        // It is worth doing an existence check because it is significantly
        // faster than calling mkdir() and letting it fail (at least on Windows).
        if let Some(parent) = path.parent()
            && !parent.exists()
        {
            fs::create_dir_all(parent)?;
        }

        File::create(path).map_err(apperr::Error::from)
    }

    fn create_buffer() -> apperr::Result<RcTextBuffer> {
        let buffer = TextBuffer::new_rc(false)?;
        {
            let mut tb = buffer.borrow_mut();
            tb.set_insert_final_newline(!cfg!(windows)); // As mandated by POSIX.
            tb.set_margin_enabled(true);
            tb.set_line_highlight_enabled(true);
        }
        Ok(buffer)
    }
}

/// Parse a filename in the form of "filename:line:char".
/// Returns the filename and the [`Document::cursor_move_to_goto`] coordinates.
pub fn parse_filename_goto(path: &Path) -> (&Path, Option<Point>) {
    fn parse(s: &[u8]) -> Option<CoordType> {
        let (negative, digits) = match s {
            [b'-', rest @ ..] => (true, rest),
            _ => (false, s),
        };
        if digits.is_empty() {
            return None;
        }

        let mut num: CoordType = 0;
        for &b in digits {
            if !b.is_ascii_digit() {
                return None;
            }
            let digit = (b - b'0') as CoordType;
            num = num.checked_mul(10)?.checked_add(digit)?;
        }
        Some(if negative { -num } else { num })
    }

    fn find_colon_rev(bytes: &[u8], offset: usize) -> Option<usize> {
        (0..offset.min(bytes.len())).rev().find(|&i| bytes[i] == b':')
    }

    let bytes = path.as_os_str().as_encoded_bytes();
    let colend = match find_colon_rev(bytes, bytes.len()) {
        // Reject filenames that would result in an empty filename after stripping off the :line:char suffix.
        // For instance, a filename like ":123:456" will not be processed by this function.
        Some(colend) if colend > 0 => colend,
        _ => return (path, None),
    };

    let last = match parse(&bytes[colend + 1..]) {
        Some(last) => last,
        None => return (path, None),
    };
    let mut len = colend;
    let mut goto = Point { x: 1, y: last };

    // Counting backwards is only supported for lines,
    // so a negative `last` rules out a char position.
    if last >= 0
        && let Some(colbeg) = find_colon_rev(bytes, colend)
        // Same here: Don't allow empty filenames.
        && colbeg != 0
        && let Some(first) = parse(&bytes[colbeg + 1..colend])
    {
        len = colbeg;
        goto = Point { x: last, y: first };
    }

    // Strip off the :line:char suffix.
    let path = &bytes[..len];
    let path = unsafe { OsStr::from_encoded_bytes_unchecked(path) };
    let path = Path::new(path);
    (path, Some(goto))
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::{AtomicUsize, Ordering};

    static NEXT_FILE: AtomicUsize = AtomicUsize::new(0);

    struct TestFile(PathBuf);

    impl TestFile {
        fn new(contents: &str) -> Self {
            let path = std::env::temp_dir().join(format!(
                "edit-219-{}-{}",
                std::process::id(),
                NEXT_FILE.fetch_add(1, Ordering::Relaxed)
            ));
            fs::write(&path, contents).unwrap();
            Self(path)
        }
    }

    impl Drop for TestFile {
        fn drop(&mut self) {
            let _ = fs::remove_file(&self.0);
        }
    }

    #[test]
    fn reload_clean_file_and_keep_dirty_file() {
        let file = TestFile::new("before");
        let mut manager = DocumentManager::default();
        manager.add_file_path(&file.0).unwrap();
        assert_eq!(manager.active_mut().unwrap().check_for_changes().unwrap(), DiskChange::None);

        fs::write(&file.0, "after external formatting").unwrap();
        let doc = manager.active_mut().unwrap();
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::Reloaded);
        assert_eq!(doc.buffer.borrow().read_forward(0), b"after external formatting");
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);

        doc.buffer.borrow_mut().mark_as_dirty();
        fs::write(&file.0, "external edit").unwrap();
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::Conflict);
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
        assert_eq!(doc.buffer.borrow().read_forward(0), b"after external formatting");
        assert!(doc.save(None).is_err());
        assert_eq!(fs::read(&file.0).unwrap(), b"external edit");
        manager.reopen_path(&file.0).unwrap();
        assert_eq!(manager.active().unwrap().buffer.borrow().read_forward(0), b"external edit");
        assert!(!manager.active().unwrap().buffer.borrow().is_dirty());
        let doc = manager.active_mut().unwrap();
        fs::write(&file.0, "another external edit").unwrap();
        assert!(doc.save(None).is_err());
        doc.save_overwriting().unwrap();
        assert_eq!(fs::read(&file.0).unwrap(), b"external edit");
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
    }

    #[test]
    fn atomic_replace_and_self_save() {
        let file = TestFile::new("old");
        let replacement = TestFile::new("new text");
        let mut manager = DocumentManager::default();
        manager.add_file_path(&file.0).unwrap();

        fs::remove_file(&file.0).unwrap();
        assert_eq!(manager.active_mut().unwrap().check_for_changes().unwrap(), DiskChange::None);
        fs::rename(&replacement.0, &file.0).unwrap();
        let doc = manager.active_mut().unwrap();
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::Reloaded);
        doc.save(None).unwrap();
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
    }

    #[test]
    fn checks_inactive_files_but_ignores_untitled() {
        let file1 = TestFile::new("first");
        let file2 = TestFile::new("second");
        let mut manager = DocumentManager::default();
        manager.add_file_path(&file1.0).unwrap();
        manager.add_file_path(&file2.0).unwrap();
        manager.add_untitled().unwrap();
        fs::write(&file1.0, "first changed and longer").unwrap();
        assert!(manager.check_for_changes().is_empty());
        let results = manager.check_for_changes();
        assert_eq!(results.len(), 1);
        assert_eq!(results[0].0, file1.0);
        assert_eq!(results[0].1.as_ref().unwrap(), &DiskChange::Reloaded);
    }

    #[test]
    fn keeping_changes_does_not_reload_and_save_stays_guarded() {
        let file = TestFile::new("saved text");
        let mut manager = DocumentManager::default();
        let doc = manager.add_file_path(&file.0).unwrap();
        doc.buffer.borrow_mut().mark_as_dirty();
        fs::write(&file.0, "external edit").unwrap();

        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::Conflict);
        // "Keep Changes" dismisses the warning without reopening the document.
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
        assert_eq!(doc.buffer.borrow().read_forward(0), b"saved text");
        assert!(doc.buffer.borrow().is_dirty());
        assert!(doc.save(None).is_err());
        assert_eq!(fs::read(&file.0).unwrap(), b"external edit");
    }

    #[test]
    fn failed_reopen_preserves_dirty_contents() {
        let file = TestFile::new("unsaved text");
        let mut manager = DocumentManager::default();
        let doc = manager.add_file_path(&file.0).unwrap();
        doc.buffer.borrow_mut().mark_as_dirty();
        fs::remove_file(&file.0).unwrap();

        // "Reopen" cannot discard edits if the file is temporarily missing.
        assert!(manager.reopen_path(&file.0).is_err());
        let doc = manager.active().unwrap();
        assert!(doc.buffer.borrow().is_dirty());
        assert_eq!(doc.buffer.borrow().read_forward(0), b"unsaved text");
    }

    #[test]
    fn newly_created_file_reloads_clean_missing_document() {
        let file = TestFile::new("placeholder");
        fs::remove_file(&file.0).unwrap();
        let mut manager = DocumentManager::default();
        let doc = manager.add_file_path(&file.0).unwrap();
        fs::write(&file.0, "new file").unwrap();

        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::None);
        assert_eq!(doc.check_for_changes().unwrap(), DiskChange::Reloaded);
        assert_eq!(doc.buffer.borrow().read_forward(0), b"new file");
    }

    #[test]
    fn failed_reload_preserves_contents_and_cursor() {
        let file = TestFile::new("first line\nsecond line");
        let mut manager = DocumentManager::default();
        let doc = manager.add_file_path(&file.0).unwrap();
        doc.buffer.borrow_mut().cursor_move_to_logical(Point { x: 3, y: 1 });
        let cursor = doc.buffer.borrow().cursor_logical_pos();
        let mut open = File::open(&file.0).unwrap();
        assert!(
            doc.buffer
                .borrow_mut()
                .reload_file(&mut open, None, || {
                    Err(io::Error::other("interrupted read").into())
                })
                .is_err()
        );
        assert_eq!(doc.buffer.borrow().read_forward(0), b"first line\nsecond line");
        assert_eq!(doc.buffer.borrow().cursor_logical_pos(), cursor);
    }

    #[test]
    fn test_parse_last_numbers() {
        fn parse(s: &str) -> (&str, Option<Point>) {
            let (p, g) = parse_filename_goto(Path::new(s));
            (p.to_str().unwrap(), g)
        }

        assert_eq!(parse("123"), ("123", None));
        assert_eq!(parse("abc"), ("abc", None));
        assert_eq!(parse(":123"), (":123", None));
        assert_eq!(parse("abc:123"), ("abc", Some(Point { x: 1, y: 123 })));
        assert_eq!(parse("45:123"), ("45", Some(Point { x: 1, y: 123 })));
        assert_eq!(parse(":45:123"), (":45", Some(Point { x: 1, y: 123 })));
        assert_eq!(parse("abc:45:123"), ("abc", Some(Point { x: 123, y: 45 })));
        assert_eq!(parse("abc:def:123"), ("abc:def", Some(Point { x: 1, y: 123 })));
        assert_eq!(parse("1:2:3"), ("1", Some(Point { x: 3, y: 2 })));
        assert_eq!(parse("::3"), (":", Some(Point { x: 1, y: 3 })));
        assert_eq!(parse("1::3"), ("1:", Some(Point { x: 1, y: 3 })));
        assert_eq!(parse(""), ("", None));
        assert_eq!(parse(":"), (":", None));
        assert_eq!(parse("::"), ("::", None));
        assert_eq!(parse("a:1"), ("a", Some(Point { x: 1, y: 1 })));
        assert_eq!(parse("1:a"), ("1:a", None));
        assert_eq!(parse("file.txt:10"), ("file.txt", Some(Point { x: 1, y: 10 })));
        assert_eq!(parse("file.txt:10:5"), ("file.txt", Some(Point { x: 5, y: 10 })));
        assert_eq!(parse("file.txt:-1"), ("file.txt", Some(Point { x: 1, y: -1 })));
        assert_eq!(parse("file.txt:-10:5"), ("file.txt", Some(Point { x: 5, y: -10 })));
        assert_eq!(parse("file.txt:10:-5"), ("file.txt:10", Some(Point { x: 1, y: -5 })));
        assert_eq!(parse("file.txt:-"), ("file.txt:-", None));
    }
}
