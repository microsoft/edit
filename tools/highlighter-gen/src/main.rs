//! This file takes a [`LanguageDefinition`] which describes syntax highlighting rules
//! for a language via a list of regular expressions that result in
//! * a highlight kind (comment, string, number, etc.)
//! * a push/pop action of another state (allows for nesting languages, such as in Markdown)
//!
//! It then transforms the definition into a list of [`WipState`], which are directions
//! to our custom DFA engine. The engine is very simple to reduce binary size.
//! Each defined state represents a root. Each additional state represents one step in
//! the regular expression. The difference between the two is that the root states will
//! seek to the next possible occurrence of any of the defined regular expressions,
//! whereas the additional states will try to match the next character without seeking.
//! If it doesn't match, it will fall back to the next possible defined regular expression.

mod languages;
mod transformer;
mod types;

use crate::transformer::parse_language_definition;

/**
---
config:
  layout: elk
---
flowchart TD
    0["ground"]
    1["comment"]
    2["string"]
    3["string_escape"]
    0 -->|"Prefix(//)"| 4
    4 -->|"Chars(Line)"| pop262144[/"Pop"/]
    4 -->|"Chars(0)"| pop262144[/"Pop"/]
    0 -->|"Prefix(/*)"| push1[/"Push(comment)"/]
    0 -->|"Prefix(&quot;)"| push2[/"Push(string)"/]
    0 -->|"Prefix(-)"| 5
    5 -->|"Charset(['0'-'9'])"| 6
    5 -->|"Chars(0)"| pop327680[/"Pop"/]
    6 -->|"Prefix(.)"| 7
    6 -->|"Chars(0)"| 8
    6 -->|"Chars(0)"| pop393216[/"Pop"/]
    7 -->|"Charset(['0'-'9'])"| 8
    7 -->|"Chars(0)"| pop458752[/"Pop"/]
    8 -->|"PrefixInsensitive(e)"| 9
    8 -->|"Chars(0)"| pop524288[/"Pop"/]
    9 -->|"Prefix(+)"| 10
    9 -->|"Prefix(-)"| 10
    9 -->|"Chars(0)"| 10
    10 -->|"Charset(['0'-'9'])"| pop655360[/"Pop"/]
    0 -->|"Charset(['0'-'9'])"| 6
    0 -->|"Prefix(true)"| pop0[/"Pop"/]
    0 -->|"Prefix(false)"| pop0[/"Pop"/]
    0 -->|"Prefix(null)"| pop0[/"Pop"/]
    1 -->|"Prefix(*/)"| pop65536[/"Pop"/]
    2 -->|"Prefix(\\)"| push131075[/"Push(string_escape)"/]
    2 -->|"Prefix(&quot;)"| pop131072[/"Pop"/]
    3 -->|"Chars(1)"| pop196608[/"Pop"/]
**/
fn main() {
    parse_language_definition(&languages::JSON);
}
