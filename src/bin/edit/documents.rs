use std::collections::LinkedList;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};

use edit::buffer::{RcTextBuffer, TextBuffer};
use edit::helpers::{CoordType, Point};
use edit::simd::memrchr2;
use edit::{apperr, sys};

pub enum DocumentPath {
    None,
    Preliminary(PathBuf),
    Canonical(PathBuf),
}

impl DocumentPath {
    pub fn as_path(&self) -> Option<&Path> {
        match self {
            DocumentPath::None => None,
            DocumentPath::Preliminary(p) | DocumentPath::Canonical(p) => Some(p),
        }
    }

    pub fn eq_canonical(&self, path: &Path) -> bool {
        match self {
            DocumentPath::Canonical(p) => p == path,
            _ => false,
        }
    }
}

pub struct Document {
    pub buffer: RcTextBuffer,
    pub path: DocumentPath,
    pub filename: String,
    pub new_file_counter: usize,
}

impl Document {
    fn update_file_mode(&mut self) {
        let mut tb = self.buffer.borrow_mut();
        tb.set_ruler(if self.filename == "COMMIT_EDITMSG" { 72 } else { 0 });
    }
}

#[derive(Default)]
pub struct DocumentManager {
    list: LinkedList<Document>,
}

impl DocumentManager {
    #[inline]
    pub fn len(&self) -> usize {
        self.list.len()
    }

    #[inline]
    pub fn active(&self) -> Option<&Document> {
        self.list.front()
    }

    #[inline]
    pub fn active_mut(&mut self) -> Option<&mut Document> {
        self.list.front_mut()
    }

    #[inline]
    pub fn update_active<F: FnMut(&Document) -> bool>(&mut self, mut func: F) -> bool {
        let mut cursor = self.list.cursor_front_mut();
        while let Some(doc) = cursor.current() {
            if func(doc) {
                let list = cursor.remove_current_as_list().unwrap();
                self.list.cursor_front_mut().splice_before(list);
                return true;
            }
            cursor.move_next();
        }
        false
    }

    pub fn remove_active(&mut self) {
        self.list.pop_front();
    }

    pub fn add_untitled(&mut self) -> apperr::Result<&mut Document> {
        let buffer = TextBuffer::new_rc(false)?;
        {
            let mut tb = buffer.borrow_mut();
            tb.set_margin_enabled(true);
            tb.set_line_highlight_enabled(true);
        }

        let mut doc = Document {
            buffer,
            path: DocumentPath::None,
            filename: Default::default(),
            new_file_counter: 0,
        };
        self.gen_untitled_name(&mut doc);

        self.list.push_front(doc);
        Ok(self.list.front_mut().unwrap())
    }

    pub fn gen_untitled_name(&self, doc: &mut Document) {
        let mut new_file_counter = 0;
        for doc in &self.list {
            new_file_counter = new_file_counter.max(doc.new_file_counter);
        }
        new_file_counter += 1;

        doc.filename = format!("Untitled-{new_file_counter}");
        doc.new_file_counter = new_file_counter;
    }

    pub fn add_file_path(&mut self, path: &Path) -> apperr::Result<&mut Document> {
        let (path, goto) = Self::parse_filename_goto(path);

        let canonical = match sys::canonicalize(path).map_err(apperr::Error::from) {
            Ok(path) => Some(path),
            Err(err) if sys::apperr_is_not_found(err) => None,
            Err(err) => return Err(err),
        };
        let canonical_ref = canonical.as_deref();
        let canonical_is_file = canonical_ref.is_some_and(|p| p.is_file());

        // Check if the file is already open.
        if let Some(canon) = canonical_ref {
            if self.update_active(|doc| doc.path.eq_canonical(canon)) {
                let doc = self.active_mut().unwrap();
                if let Some(goto) = goto {
                    doc.buffer.borrow_mut().cursor_move_to_logical(goto);
                }
                return Ok(doc);
            }
        }

        let buffer = TextBuffer::new_rc(false)?;
        {
            let mut tb = buffer.borrow_mut();
            tb.set_margin_enabled(true);
            tb.set_line_highlight_enabled(true);

            if canonical_is_file && let Some(canon) = canonical_ref {
                tb.read_file_path(canon, None)?;

                if let Some(goto) = goto
                    && goto != Point::default()
                {
                    tb.cursor_move_to_logical(goto);
                }
            }
        }

        let path = match canonical {
            // Path exists and is a file.
            Some(path) if canonical_is_file => DocumentPath::Canonical(path),
            // Path doesn't exist at all.
            None => DocumentPath::Preliminary(path.to_path_buf()),
            // Path exists but is not a file (a directory?).
            _ => DocumentPath::None,
        };
        let filename = path.as_path().map_or(Default::default(), Self::get_filename_from_path);
        let mut doc = Document { buffer, path, filename, new_file_counter: 0 };

        if doc.filename.is_empty() {
            self.gen_untitled_name(&mut doc);
        }
        doc.update_file_mode();

        self.list.push_front(doc);
        Ok(self.list.front_mut().unwrap())
    }

    pub fn save_active(&mut self, new_path: Option<&Path>) -> apperr::Result<()> {
        let Some(doc) = self.active_mut() else {
            return Ok(());
        };

        {
            let path = new_path.or_else(|| doc.path.as_path()).unwrap();
            let mut tb = doc.buffer.borrow_mut();
            tb.write_file(path)?;
        }

        // Turn the new_path or existing preliminary path into a canonical path.
        // Now that the file exists, that should theoretically work.
        if let Some(path) = new_path.or_else(|| match &doc.path {
            DocumentPath::Preliminary(path) => Some(path),
            _ => None,
        }) {
            let path = sys::canonicalize(path)?;
            doc.filename = Self::get_filename_from_path(&path);
            doc.path = DocumentPath::Canonical(path);
            doc.update_file_mode();
        }

        Ok(())
    }

    pub fn get_filename_from_path(path: &Path) -> String {
        path.file_name().unwrap_or_default().to_string_lossy().into_owned()
    }

    // Parse a filename in the form of "filename:line:char".
    // Returns the position of the first colon and the line/char coordinates.
    fn parse_filename_goto(path: &Path) -> (&Path, Option<Point>) {
        fn parse(s: &[u8]) -> Option<CoordType> {
            if s.is_empty() {
                return None;
            }

            let mut num: CoordType = 0;
            for &b in s {
                if !b.is_ascii_digit() {
                    return None;
                }
                let digit = (b - b'0') as CoordType;
                num = num.checked_mul(10)?.checked_add(digit)?;
            }
            Some(num)
        }

        let bytes = path.as_os_str().as_encoded_bytes();
        let colend = match memrchr2(b':', b':', bytes, bytes.len()) {
            // Reject filenames that would result in an empty filename after stripping off the :line:char suffix.
            // For instance, a filename like ":123:456" will not be processed by this function.
            Some(colend) if colend > 0 => colend,
            _ => return (path, None),
        };

        let last = match parse(&bytes[colend + 1..]) {
            Some(last) => last,
            None => return (path, None),
        };
        let last = (last - 1).max(0);
        let mut len = colend;
        let mut goto = Point { x: 0, y: last };

        if let Some(colbeg) = memrchr2(b':', b':', bytes, colend) {
            // Same here: Don't allow empty filenames.
            if colbeg != 0 {
                if let Some(first) = parse(&bytes[colbeg + 1..colend]) {
                    let first = (first - 1).max(0);
                    len = colbeg;
                    goto = Point { x: last, y: first };
                }
            }
        }

        // Strip off the :line:char suffix.
        let path = &bytes[..len];
        let path = unsafe { OsStr::from_encoded_bytes_unchecked(path) };
        let path = Path::new(path);
        (path, Some(goto))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_last_numbers() {
        fn parse(s: &str) -> (&str, Option<Point>) {
            let (p, g) = DocumentManager::parse_filename_goto(Path::new(s));
            (p.to_str().unwrap(), g)
        }

        assert_eq!(parse("123"), ("123", None));
        assert_eq!(parse("abc"), ("abc", None));
        assert_eq!(parse(":123"), (":123", None));
        assert_eq!(parse("abc:123"), ("abc", Some(Point { x: 0, y: 122 })));
        assert_eq!(parse("45:123"), ("45", Some(Point { x: 0, y: 122 })));
        assert_eq!(parse(":45:123"), (":45", Some(Point { x: 0, y: 122 })));
        assert_eq!(parse("abc:45:123"), ("abc", Some(Point { x: 122, y: 44 })));
        assert_eq!(parse("abc:def:123"), ("abc:def", Some(Point { x: 0, y: 122 })));
        assert_eq!(parse("1:2:3"), ("1", Some(Point { x: 2, y: 1 })));
        assert_eq!(parse("::3"), (":", Some(Point { x: 0, y: 2 })));
        assert_eq!(parse("1::3"), ("1:", Some(Point { x: 0, y: 2 })));
        assert_eq!(parse(""), ("", None));
        assert_eq!(parse(":"), (":", None));
        assert_eq!(parse("::"), ("::", None));
        assert_eq!(parse("a:1"), ("a", Some(Point { x: 0, y: 0 })));
        assert_eq!(parse("1:a"), ("1:a", None));
        assert_eq!(parse("file.txt:10"), ("file.txt", Some(Point { x: 0, y: 9 })));
        assert_eq!(parse("file.txt:10:5"), ("file.txt", Some(Point { x: 4, y: 9 })));
    }
}
