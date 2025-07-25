use ActionDefinition::*;
use HighlightKind::*;

#[derive(Default, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    #[default]
    Other,
    Comment,
    Number,
    String,
    Variable,
    Operator,
    Keyword,
    Method,
}

pub struct Language {
    #[allow(dead_code)]
    pub name: &'static str,
    pub extensions: &'static [&'static str],
    pub states: &'static [State],
}

pub struct State {
    pub name: &'static str,
    pub rules: &'static [Rule],
}

pub type Rule = (&'static str, HighlightKind, ActionDefinition);

#[derive(Debug, Clone, Copy)]
pub enum ActionDefinition {
    Push(&'static str), // state name to push
    Pop,
}

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
        name: "YAML",
        extensions: &["yaml", "yml"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"#.*"#, Comment, Pop),
                    (r#"""#, String, Push("string_double")),
                    (r#"'"#, String, Push("string_single")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Number, Push("string_resolve")),
                    (r#"(?:true|false|null)"#, Keyword, Push("string_resolve")),
                ],
            },
            State {
                name: "string_double",
                rules: &[(r#"""#, String, Pop), (r#"\\"#, String, Push("string_escape"))],
            },
            State {
                name: "string_single",
                rules: &[(r#"'"#, String, Pop), (r#"\\"#, String, Push("string_escape"))],
            },
            State { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
            State { name: "string_resolve", rules: &[(r#"\s*\S+[^#]+"#, String, Pop)] },
        ],
    },
    Language {
        name: "Bash",
        extensions: &["sh", "zsh"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"#.*"#, Comment, Pop),
                    (r#"'"#, String, Push("string_single")),
                    (r#"""#, String, Push("string_double")),
                    (r#"\$"#, Variable, Push("variable")),
                    (r#"[!*/%+<=>|]"#, Operator, Pop),
                    (
                        r"(?i:break|case|continue|done|do|elif|else|esac|fi|for|function|if|in|return|select|then|until|while)",
                        Keyword,
                        Pop,
                    ),
                    (r#"\d+"#, Number, Pop),
                    (r"\w+", Method, Pop),
                ],
            },
            State {
                name: "string_single",
                rules: &[(r#"'"#, String, Pop), (r#"\\"#, String, Push("string_escape"))],
            },
            State {
                name: "string_double",
                rules: &[
                    (r#"""#, String, Pop),
                    (r#"\\"#, String, Push("string_escape")),
                    (r#"\$"#, Other, Push("variable")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
            State {
                name: "variable",
                rules: &[
                    (r#"[#?]"#, Variable, Pop),
                    (r#"\{[^}]*\}"#, Variable, Pop),
                    (r#"\w+"#, Variable, Pop),
                ],
            },
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
                    (r#"[$^?]"#, Variable, Pop),
                    (r#"\{[^}]*\}"#, Variable, Pop),
                    (r#"\w+"#, Variable, Pop),
                ],
            },
        ],
    },
    Language {
        name: "Batch",
        extensions: &["bat", "cmd"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"(?::: |::\t).*"#, Comment, Pop),
                    (r#"(?i:rem |rem\t).*"#, Comment, Pop),
                    (r#"""#, String, Push("string")),
                    (r#"[!*/%+<=>|]"#, Operator, Pop),
                    (
                        r"(?i:break|call|cd|chdir|cls|copy|del|dir|echo|exit|for|goto|if|md|mkdir|move|pause|ren|set)",
                        Keyword,
                        Pop,
                    ),
                    (r#"\d+"#, Number, Pop),
                    (r#"\w+"#, Method, Pop),
                ],
            },
            State {
                name: "string",
                rules: &[(r#"""#, String, Pop), (r#"\\"#, String, Push("string_escape"))],
            },
            State { name: "string_escape", rules: &[(r#"."#, String, Pop)] },
        ],
    },
];
