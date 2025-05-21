// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! Fuzzy search algorithm based on the one used in VS Code (`/src/vs/base/common/fuzzyScorer.ts`).
//! Other algorithms exist, such as Sublime Text's, or the one used in `fzf`,
//! but I figured that this one is what lots of people may be familiar with.

use std::vec;

use crate::arena::{Arena, scratch_arena};
use crate::icu;

const NO_MATCH: i32 = 0;

pub fn score_fuzzy<'a>(
    arena: &'a Arena,
    haystack: &str,
    needle: &str,
    allow_non_contiguous_matches: bool,
) -> (i32, Vec<usize, &'a Arena>) {
    if haystack.is_empty() || needle.is_empty() {
        // return early if target or query are empty
        return (NO_MATCH, Vec::new_in(arena));
    }

    let scratch = scratch_arena(Some(arena));
    let target = map_chars(&scratch, haystack);
    let query = map_chars(&scratch, needle);

    if target.len() < query.len() {
        // impossible for query to be contained in target
        return (NO_MATCH, Vec::new_in(arena));
    }

    let target_lower = icu::fold_case(&scratch, haystack);
    let query_lower = icu::fold_case(&scratch, needle);
    let target_lower = map_chars(&scratch, &target_lower);
    let query_lower = map_chars(&scratch, &query_lower);

    let area = query.len() * target.len();
    let mut scores = vec::from_elem_in(0, area, &*scratch);
    let mut matches = vec::from_elem_in(0, area, &*scratch);

    //
    // Build Scorer Matrix:
    //
    // The matrix is composed of query q and target t. For each index we score
    // q[i] with t[i] and compare that with the previous score. If the score is
    // equal or larger, we keep the match. In addition to the score, we also keep
    // the length of the consecutive matches to use as boost for the score.
    //
    //      t   a   r   g   e   t
    //  q
    //  u
    //  e
    //  r
    //  y
    //
    for query_index in 0..query.len() {
        let query_index_offset = query_index * target.len();
        let query_index_previous_offset =
            if query_index > 0 { (query_index - 1) * target.len() } else { 0 };

        for target_index in 0..target.len() {
            let current_index = query_index_offset + target_index;
            let diag_index = if query_index > 0 && target_index > 0 {
                query_index_previous_offset + target_index - 1
            } else {
                0
            };
            let left_score = if target_index > 0 { scores[current_index - 1] } else { 0 };
            let diag_score =
                if query_index > 0 && target_index > 0 { scores[diag_index] } else { 0 };
            let matches_sequence_len =
                if query_index > 0 && target_index > 0 { matches[diag_index] } else { 0 };

            // If we are not matching on the first query character any more, we only produce a
            // score if we had a score previously for the last query index (by looking at the diagScore).
            // This makes sure that the query always matches in sequence on the target. For example
            // given a target of "ede" and a query of "de", we would otherwise produce a wrong high score
            // for query[1] ("e") matching on target[0] ("e") because of the "beginning of word" boost.
            let score = if diag_score == 0 && query_index != 0 {
                0
            } else {
                compute_char_score(
                    query[query_index],
                    query_lower[query_index],
                    if target_index != 0 { Some(target[target_index - 1]) } else { None },
                    target[target_index],
                    target_lower[target_index],
                    matches_sequence_len,
                )
            };

            // We have a score and its equal or larger than the left score
            // Match: sequence continues growing from previous diag value
            // Score: increases by diag score value
            let is_valid_score = score != 0 && diag_score + score >= left_score;
            if is_valid_score
                && (
                    // We don't need to check if it's contiguous if we allow non-contiguous matches
                    allow_non_contiguous_matches ||
                        // We must be looking for a contiguous match.
                        // Looking at an index above 0 in the query means we must have already
                        // found out this is contiguous otherwise there wouldn't have been a score
                        query_index > 0 ||
                        // lastly check if the query is completely contiguous at this index in the target
                        target_lower[target_index..].starts_with(&query_lower)
                )
            {
                matches[current_index] = matches_sequence_len + 1;
                scores[current_index] = diag_score + score;
            } else {
                // We either have no score or the score is lower than the left score
                // Match: reset to 0
                // Score: pick up from left hand side
                matches[current_index] = NO_MATCH;
                scores[current_index] = left_score;
            }
        }
    }

    // Restore Positions (starting from bottom right of matrix)
    let mut positions = Vec::new_in(arena);

    if !query.is_empty() && !target.is_empty() {
        let mut query_index = query.len() - 1;
        let mut target_index = target.len() - 1;

        loop {
            let current_index = query_index * target.len() + target_index;
            if matches[current_index] == NO_MATCH {
                if target_index == 0 {
                    break;
                }
                target_index -= 1; // go left
            } else {
                positions.push(target_index);

                // go up and left
                if query_index == 0 || target_index == 0 {
                    break;
                }
                query_index -= 1;
                target_index -= 1;
            }
        }

        positions.reverse();
    }

    (scores[area - 1], positions)
}

fn compute_char_score(
    query: char,
    query_lower: char,
    target_prev: Option<char>,
    target_curr: char,
    target_curr_lower: char,
    matches_sequence_len: i32,
) -> i32 {
    let mut score = 0;

    if !consider_as_equal(query_lower, target_curr_lower) {
        return score; // no match of characters
    }

    // Character match bonus
    score += 1;

    // Consecutive match bonus
    if matches_sequence_len > 0 {
        score += matches_sequence_len * 5;
    }

    // Same case bonus
    if query == target_curr {
        score += 1;
    }

    if let Some(target_prev) = target_prev {
        // After separator bonus
        let separator_bonus = score_separator_at_pos(target_prev);
        if separator_bonus > 0 {
            score += separator_bonus;
        }
        // Inside word upper case bonus (camel case). We only give this bonus if we're not in a contiguous sequence.
        // For example:
        // NPE => NullPointerException = boost
        // HTTP => HTTP = not boost
        else if target_curr != target_curr_lower && matches_sequence_len == 0 {
            score += 2;
        }
    } else {
        // Start of word bonus
        score += 8;
    }

    score
}

fn consider_as_equal(a: char, b: char) -> bool {
    // Special case path separators: ignore platform differences
    a == b || (a == '/' && b == '\\') || (a == '\\' && b == '/')
}

fn score_separator_at_pos(ch: char) -> i32 {
    match ch {
        '/' | '\\' => 5,                               // prefer path separators...
        '_' | '-' | '.' | ' ' | '\'' | '"' | ':' => 4, // ...over other separators
        _ => 0,
    }
}

fn map_chars<'a>(arena: &'a Arena, s: &str) -> Vec<char, &'a Arena> {
    let mut chars = Vec::with_capacity_in(s.len(), arena);
    chars.extend(s.chars());
    chars.shrink_to_fit();
    chars
}
