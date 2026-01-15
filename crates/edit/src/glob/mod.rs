// Copyright (c) 2023 Devon Govett
// Licensed under the MIT License.

//! A fork of [glob-match](https://crates.io/crates/glob-match) with case-insensitive matching.
//! Capture groups were removed to reduce size.

use std::path::is_separator;

#[derive(Clone, Copy, Debug, Default)]
struct State {
    // These store character indices into the glob and path strings.
    path_index: usize,
    glob_index: usize,

    // When we hit a * or **, we store the state for backtracking.
    wildcard: Wildcard,
    globstar: Wildcard,
}

#[derive(Clone, Copy, Debug, Default)]
struct Wildcard {
    // Using u32 rather than usize for these results in 10% faster performance.
    glob_index: u32,
    path_index: u32,
    capture_index: u32,
}

pub fn glob_match(glob: &[u8], path: &[u8]) -> bool {
    // This algorithm is based on https://research.swtch.com/glob
    let mut state = State::default();

    // Store the state when we see an opening '{' brace in a stack.
    // Up to 10 nested braces are supported.
    let mut brace_stack = BraceStack::default();

    // First, check if the pattern is negated with a leading '!' character.
    // Multiple negations can occur.
    let mut negated = false;
    while state.glob_index < glob.len() && glob[state.glob_index] == b'!' {
        negated = !negated;
        state.glob_index += 1;
    }

    while state.glob_index < glob.len() || state.path_index < path.len() {
        if state.glob_index < glob.len() {
            match glob[state.glob_index] {
                b'*' => {
                    let is_globstar =
                        state.glob_index + 1 < glob.len() && glob[state.glob_index + 1] == b'*';
                    if is_globstar {
                        // Coalesce multiple ** segments into one.
                        state.glob_index = skip_globstars(glob, state.glob_index + 2) - 2;
                    }

                    state.wildcard.glob_index = state.glob_index as u32;
                    state.wildcard.path_index = state.path_index as u32 + 1;

                    // ** allows path separators, whereas * does not.
                    // However, ** must be a full path component, i.e. a/**/b not a**b.
                    if is_globstar {
                        state.glob_index += 2;

                        if glob.len() == state.glob_index {
                            // A trailing ** segment without a following separator.
                            state.globstar = state.wildcard;
                        } else if (state.glob_index < 3 || glob[state.glob_index - 3] == b'/')
                            && glob[state.glob_index] == b'/'
                        {
                            // Matched a full /**/ segment. If the last character in the path was a separator,
                            // skip the separator in the glob so we search for the next character.
                            // In effect, this makes the whole segment optional so that a/**/b matches a/b.
                            if state.path_index == 0
                                || (state.path_index < path.len()
                                    && is_separator(path[state.path_index - 1] as char))
                            {
                                state.glob_index += 1;
                            }

                            // The allows_sep flag allows separator characters in ** matches.
                            // one is a '/', which prevents a/**/b from matching a/bb.
                            state.globstar = state.wildcard;
                        }
                    } else {
                        state.glob_index += 1;
                    }

                    // If we are in a * segment and hit a separator,
                    // either jump back to a previous ** or end the wildcard.
                    if state.globstar.path_index != state.wildcard.path_index
                        && state.path_index < path.len()
                        && is_separator(path[state.path_index] as char)
                    {
                        // Special case: don't jump back for a / at the end of the glob.
                        if state.globstar.path_index > 0 && state.path_index + 1 < path.len() {
                            state.glob_index = state.globstar.glob_index as usize;
                            state.wildcard.glob_index = state.globstar.glob_index;
                            state.wildcard.capture_index = state.globstar.capture_index;
                        } else {
                            state.wildcard.path_index = 0;
                        }
                    }

                    // If the next char is a special brace separator,
                    // skip to the end of the braces so we don't try to match it.
                    if brace_stack.length > 0
                        && state.glob_index < glob.len()
                        && matches!(glob[state.glob_index], b',' | b'}')
                        && state.skip_braces(glob, false) == BraceState::Invalid
                    {
                        // invalid pattern!
                        return false;
                    }

                    continue;
                }
                b'?' if state.path_index < path.len() => {
                    if !is_separator(path[state.path_index] as char) {
                        state.glob_index += 1;
                        state.path_index += 1;
                        continue;
                    }
                }
                b'[' if state.path_index < path.len() => {
                    state.glob_index += 1;
                    let c = path[state.path_index];

                    // Check if the character class is negated.
                    let mut negated = false;
                    if state.glob_index < glob.len()
                        && matches!(glob[state.glob_index], b'^' | b'!')
                    {
                        negated = true;
                        state.glob_index += 1;
                    }

                    // Try each range.
                    let mut first = true;
                    let mut is_match = false;
                    while state.glob_index < glob.len() && (first || glob[state.glob_index] != b']')
                    {
                        let mut low = glob[state.glob_index];
                        if !unescape(&mut low, glob, &mut state.glob_index) {
                            // Invalid pattern!
                            return false;
                        }
                        state.glob_index += 1;

                        // If there is a - and the following character is not ], read the range end character.
                        let high = if state.glob_index + 1 < glob.len()
                            && glob[state.glob_index] == b'-'
                            && glob[state.glob_index + 1] != b']'
                        {
                            state.glob_index += 1;
                            let mut high = glob[state.glob_index];
                            if !unescape(&mut high, glob, &mut state.glob_index) {
                                // Invalid pattern!
                                return false;
                            }
                            state.glob_index += 1;
                            high
                        } else {
                            low
                        };

                        if low <= c && c <= high {
                            is_match = true;
                        }
                        first = false;
                    }
                    if state.glob_index >= glob.len() {
                        // invalid pattern!
                        return false;
                    }
                    state.glob_index += 1;
                    if is_match != negated {
                        state.path_index += 1;
                        continue;
                    }
                }
                b'{' if state.path_index < path.len() => {
                    if brace_stack.length as usize >= brace_stack.stack.len() {
                        // Invalid pattern! Too many nested braces.
                        return false;
                    }

                    // Push old state to the stack, and reset current state.
                    state = brace_stack.push(&state);
                    continue;
                }
                b'}' if brace_stack.length > 0 => {
                    // If we hit the end of the braces, we matched the last option.
                    brace_stack.longest_brace_match =
                        brace_stack.longest_brace_match.max(state.path_index as u32);
                    state.glob_index += 1;
                    state = brace_stack.pop(&state);
                    continue;
                }
                b',' if brace_stack.length > 0 => {
                    // If we hit a comma, we matched one of the options!
                    // But we still need to check the others in case there is a longer match.
                    brace_stack.longest_brace_match =
                        brace_stack.longest_brace_match.max(state.path_index as u32);
                    state.path_index = brace_stack.last().path_index;
                    state.glob_index += 1;
                    state.wildcard = Wildcard::default();
                    state.globstar = Wildcard::default();
                    continue;
                }
                mut c if state.path_index < path.len() => {
                    // Match escaped characters as literals.
                    if !unescape(&mut c, glob, &mut state.glob_index) {
                        // Invalid pattern!
                        return false;
                    }

                    let is_match = if c == b'/' {
                        is_separator(path[state.path_index] as char)
                    } else {
                        // NOTE: Here's where this fork differs and does case-insensitive matching.
                        path[state.path_index].eq_ignore_ascii_case(&c)
                    };

                    if is_match {
                        if brace_stack.length > 0
                            && state.glob_index > 0
                            && glob[state.glob_index - 1] == b'}'
                        {
                            brace_stack.longest_brace_match = state.path_index as u32;
                            state = brace_stack.pop(&state);
                        }
                        state.glob_index += 1;
                        state.path_index += 1;

                        // If this is not a separator, lock in the previous globstar.
                        if c != b'/' {
                            state.globstar.path_index = 0;
                        }
                        continue;
                    }
                }
                _ => {}
            }
        }

        // If we didn't match, restore state to the previous star pattern.
        if state.wildcard.path_index > 0 && state.wildcard.path_index as usize <= path.len() {
            state.backtrack();
            continue;
        }

        if brace_stack.length > 0 {
            // If in braces, find next option and reset path to index where we saw the '{'
            match state.skip_braces(glob, true) {
                BraceState::Invalid => return false,
                BraceState::Comma => {
                    state.path_index = brace_stack.last().path_index;
                    continue;
                }
                BraceState::EndBrace => {}
            }

            // Hit the end. Pop the stack.
            // If we matched a previous option, use that.
            if brace_stack.longest_brace_match > 0 {
                state = brace_stack.pop(&state);
                continue;
            } else {
                // Didn't match. Restore state, and check if we need to jump back to a star pattern.
                state = *brace_stack.last();
                brace_stack.length -= 1;
                if state.wildcard.path_index > 0 && state.wildcard.path_index as usize <= path.len()
                {
                    state.backtrack();
                    continue;
                }
            }
        }

        return negated;
    }

    if brace_stack.length > 0 && state.glob_index > 0 && glob[state.glob_index - 1] == b'}' {
        brace_stack.longest_brace_match = state.path_index as u32;
        brace_stack.pop(&state);
    }

    !negated
}

#[inline(always)]
fn unescape(c: &mut u8, glob: &[u8], glob_index: &mut usize) -> bool {
    if *c == b'\\' {
        *glob_index += 1;
        if *glob_index >= glob.len() {
            // Invalid pattern!
            return false;
        }
        *c = match glob[*glob_index] {
            b'a' => b'\x61',
            b'b' => b'\x08',
            b'n' => b'\n',
            b'r' => b'\r',
            b't' => b'\t',
            c => c,
        }
    }
    true
}

#[derive(PartialEq)]
enum BraceState {
    Invalid,
    Comma,
    EndBrace,
}

impl State {
    #[inline(always)]
    fn backtrack(&mut self) {
        self.glob_index = self.wildcard.glob_index as usize;
        self.path_index = self.wildcard.path_index as usize;
    }

    fn skip_braces(&mut self, glob: &[u8], stop_on_comma: bool) -> BraceState {
        let mut braces = 1;
        let mut in_brackets = false;
        while self.glob_index < glob.len() && braces > 0 {
            match glob[self.glob_index] {
                // Skip nested braces.
                b'{' if !in_brackets => braces += 1,
                b'}' if !in_brackets => braces -= 1,
                b',' if stop_on_comma && braces == 1 && !in_brackets => {
                    self.glob_index += 1;
                    return BraceState::Comma;
                }
                c @ (b'*' | b'?' | b'[') if !in_brackets => {
                    if c == b'[' {
                        in_brackets = true;
                    }
                    if c == b'*'
                        && self.glob_index + 1 < glob.len()
                        && glob[self.glob_index + 1] == b'*'
                    {
                        self.glob_index = skip_globstars(glob, self.glob_index + 2) - 2;
                        self.glob_index += 1;
                    }
                }
                b']' => in_brackets = false,
                b'\\' => {
                    self.glob_index += 1;
                }
                _ => {}
            }
            self.glob_index += 1;
        }

        if braces != 0 {
            return BraceState::Invalid;
        }

        BraceState::EndBrace
    }
}

#[inline(always)]
fn skip_globstars(glob: &[u8], mut glob_index: usize) -> usize {
    // Coalesce multiple ** segments into one.
    while glob_index + 3 <= glob.len()
        && unsafe { glob.get_unchecked(glob_index..glob_index + 3) } == b"/**"
    {
        glob_index += 3;
    }
    glob_index
}

struct BraceStack {
    stack: [State; 10],
    length: u32,
    longest_brace_match: u32,
}

impl Default for BraceStack {
    #[inline]
    fn default() -> Self {
        // Manual implementation is faster than the automatically derived one.
        BraceStack { stack: [State::default(); 10], length: 0, longest_brace_match: 0 }
    }
}

impl BraceStack {
    #[inline(always)]
    fn push(&mut self, state: &State) -> State {
        // Push old state to the stack, and reset current state.
        self.stack[self.length as usize] = *state;
        self.length += 1;
        State { path_index: state.path_index, glob_index: state.glob_index + 1, ..State::default() }
    }

    #[inline(always)]
    fn pop(&mut self, state: &State) -> State {
        self.length -= 1;
        let state = State {
            path_index: self.longest_brace_match as usize,
            glob_index: state.glob_index,
            // But restore star state if needed later.
            wildcard: self.stack[self.length as usize].wildcard,
            globstar: self.stack[self.length as usize].globstar,
        };
        if self.length == 0 {
            self.longest_brace_match = 0;
        }

        state
    }

    #[inline(always)]
    fn last(&self) -> &State {
        &self.stack[self.length as usize - 1]
    }
}
