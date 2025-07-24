use ActionDefinition::*;
use HighlightKind::*;

use crate::types::*;

pub const JSON: LanguageDefinition = LanguageDefinition {
    name: "JSON",
    extensions: &["json", "jsonc"],
    states: &[
        StateDefinition {
            name: "ground",
            rules: &[
                (r#"//.*"#, Comment, Pop),
                (r#"/\*"#, Comment, Push("comment")),
                (r#"""#, String, Push("string")),
                (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Pop),
                (r#"(?:true|false|null)"#, Keyword, Pop),
            ],
        },
        StateDefinition { name: "comment", rules: &[(r#"\*/"#, Comment, Pop)] },
        StateDefinition {
            name: "string",
            rules: &[(r#"\\"#, String, Push("string_escape")), (r#"""#, String, Pop)],
        },
        StateDefinition { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
    ],
};
