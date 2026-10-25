use std::ffi::c_int;

use super::registrable::Token;

/// A set of signals with associated tokens.
/// Supports flagging signals to coalesce them.
///
/// It would be trivial to make this faster with bitsets, etc.,
/// but what for? Are you intending to register 64 signals in your app?
#[derive(Default)]
pub struct SignalSet {
    entries: Vec<SignalEntry>,
}

struct SignalEntry {
    token: Token,
    signum: c_int,
    flagged: bool,
}

impl SignalSet {
    /// Returns `true` if the token was newly registered.
    pub fn add(&mut self, signum: c_int, token: Token) -> bool {
        if let Some(entry) = self.find(signum) {
            entry.token = token;
            false
        } else {
            self.entries.push(SignalEntry { token, signum, flagged: false });
            true
        }
    }

    /// Flags the given `signum` as ready.
    pub fn flag(&mut self, signum: c_int) {
        if let Some(entry) = self.find(signum) {
            entry.flagged = true;
        }
    }

    /// Iterates over all tokens whose signal was flagged.
    /// Resets the list of flagged signals.
    #[inline]
    pub fn fetch_flagged(&mut self) -> impl Iterator<Item = Token> {
        self.entries.iter_mut().filter_map(|entry| {
            if entry.flagged {
                entry.flagged = false;
                Some(entry.token)
            } else {
                None
            }
        })
    }

    fn find(&mut self, signum: c_int) -> Option<&mut SignalEntry> {
        self.entries.iter_mut().find(|e| e.signum == signum)
    }
}
