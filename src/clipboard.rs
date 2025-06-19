//! Clipboard facilities for the editor.

/// The builtin, internal clipboard of the editor.
///
/// This is useful particularly when the terminal doesn't support
/// OSC 52 or when the clipboard contents are huge (e.g. 1GiB).
#[derive(Default)]
pub struct Clipboard {
    data: Vec<u8>,
    line_copy: bool,
    wants_host_sync: bool,
}

impl Clipboard {
    /// If true, we should emit a OSC 52 sequence to sync the clipboard
    /// with the hosting terminal.
    pub fn wants_host_sync(&self) -> bool {
        self.wants_host_sync
    }

    /// Call this once the clipboard has been synchronized with the host.
    pub fn mark_as_synchronized(&mut self) {
        self.wants_host_sync = false;
    }

    /// The editor has a special behavior when you have no selection and press
    /// Ctrl+C: It copies the current line to the clipboard. Then, when you
    /// paste it, it inserts the line at *the start* of the current line.
    /// This effectively prepends the current line with the copied line.
    /// `clipboard_line_start` is true in that case.
    pub fn is_line_copy(&self) -> bool {
        self.line_copy
    }

    /// Returns the current contents of the clipboard.
    pub fn read(&self) -> &[u8] {
        &self.data
    }

    /// Fill the clipboard with the given data.
    pub fn write(&mut self, data: Vec<u8>) {
        if !data.is_empty() {
            self.data = data;
            self.line_copy = false;
            self.wants_host_sync = true;
        }
    }

    /// See [`Clipboard::is_line_copy`].
    pub fn write_was_line_copy(&mut self, line_copy: bool) {
        self.line_copy = line_copy;
    }
}
