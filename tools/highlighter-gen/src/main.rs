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

mod transformer;
mod types;

use ActionDefinition::*;
use HighlightKind::*;

use crate::transformer::GraphBuilder;
use crate::types::*;

pub const LANGUAGES: &[Language] = &[
    Language {
        name: "JSON",
        extensions: &["json", "jsonc"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"//.*"#, Comment, Pop),
                    (r#"/\*"#, Comment, Push("comment")),
                    (r#"""#, String, Push("string")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop),
                    (r#"(?:true|false|null)"#, Keyword, Pop),
                ],
            },
            State { name: "comment", rules: &[(r#"\*/"#, Comment, Pop)] },
            State {
                name: "string",
                rules: &[(r#"\\"#, String, Push("string_escape")), (r#"""#, String, Pop)],
            },
            State { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
        ],
    },
    Language {
        name: "PowerShell",
        extensions: &["ps1", "psm1", "psd1"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"#.*"#, Comment, Pop),
                    (r#"<#"#, Comment, Push("comment")),
                    (r#"'"#, String, Push("string_single")),
                    (r#"\""#, String, Push("string_double")),
                    (r#"\$"#, Variable, Push("variable")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop),
                    (r#"-\w+"#, Operator, Pop),
                    (r#"[!*/%+<=>|]"#, Operator, Pop),
                    // TODO: Add [\w-]+ suffix which results in kind=Other
                    (
                        r#"(?i:break|catch|continue|do|else|finally|foreach|function|if|return|switch|throw|try|using|while)"#,
                        Keyword,
                        Pop,
                    ),
                    (r#"[\w-]+"#, Method, Pop),
                ],
            },
            State { name: "comment", rules: &[(r#"#>"#, Comment, Pop)] },
            State {
                name: "string_single",
                rules: &[(r#"'"#, String, Pop), (r#"`"#, String, Push("string_escape"))],
            },
            State {
                name: "string_double",
                rules: &[
                    (r#"""#, String, Pop),
                    (r#"`"#, String, Push("string_escape")),
                    (r#"\$"#, Other, Push("variable")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
            State {
                name: "variable",
                rules: &[
                    (r#"\("#, Other, Pop), // TODO: subexpression
                    (r#"\w+"#, Variable, Pop),
                    (r#""#, Other, Pop),
                ],
            },
        ],
    },
];

fn main() {
    for lang in LANGUAGES {
        let mut builder = GraphBuilder::new();

        for s in lang.states {
            builder.add_root(s.name);
        }

        for (root, state) in lang.states.iter().enumerate() {
            for rule in state.rules {
                builder.parse(root, rule);
            }
        }

        builder.compute_required_charsets();
        builder.connect_fallbacks();
        println!("{}", builder.format_as_mermaid());
    }
}
