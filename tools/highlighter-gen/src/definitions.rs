use ActionDefinition::*;
use HighlightKind::*;

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
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

pub type Rule = (&'static str, Option<HighlightKind>, ActionDefinition);

#[derive(Debug, Clone, Copy)]
pub enum ActionDefinition {
    Change(&'static str),
    Push(&'static str),
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
                    (r#"//.*"#, Some(Comment), Pop),
                    (r#"/\*"#, Some(Comment), Push("comment")),
                    (r#"""#, Some(String), Push("string")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?\w+"#, Some(Other), Pop),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Some(Number), Pop),
                    (r#"(?:true|false|null)\w+"#, Some(Other), Pop),
                    (r#"(?:true|false|null)"#, Some(Keyword), Pop),
                ],
            },
            State { name: "comment", rules: &[(r#"\*/"#, Some(Comment), Pop)] },
            State {
                name: "string",
                rules: &[
                    (r#"\\"#, Some(String), Push("string_escape")),
                    (r#"""#, Some(String), Pop),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, Some(String), Pop)] },
        ],
    },
    Language {
        name: "YAML",
        extensions: &["yaml", "yml"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"#.*"#, Some(Comment), Pop),
                    (r#"""#, Some(String), Push("string_double")),
                    (r#"'"#, Some(String), Push("string_single")),
                    (
                        r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#,
                        Some(Number),
                        Change("string_resolve"),
                    ),
                    (r#"(?:true|false|null)"#, Some(Keyword), Change("string_resolve")),
                ],
            },
            State {
                name: "string_double",
                rules: &[
                    (r#"""#, Some(String), Pop),
                    (r#"\\"#, Some(String), Push("string_escape")),
                ],
            },
            State {
                name: "string_single",
                rules: &[
                    (r#"'"#, Some(String), Pop),
                    (r#"\\"#, Some(String), Push("string_escape")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, Some(String), Pop)] },
            State {
                name: "string_resolve",
                rules: &[(r#"\s*\S+"#, None, Change("string_resolve_change")), (r#""#, None, Pop)],
            },
            State {
                name: "string_resolve_change",
                rules: &[(r#"[^#]*"#, Some(String), Pop), (r#""#, None, Pop)],
            },
        ],
    },
    Language {
        name: "Bash",
        extensions: &["sh", "zsh"],
        states: &[
            State {
                name: "ground",
                rules: &[
                    (r#"#.*"#, Some(Comment), Pop),
                    (r#"'"#, Some(String), Push("string_single")),
                    (r#"""#, Some(String), Push("string_double")),
                    (r#"\$"#, Some(Variable), Push("variable")),
                    (r#"[!*/%+<=>|]"#, Some(Operator), Pop),
                    (
                        r"(?i:break|case|continue|done|do|elif|else|esac|fi|for|function|if|in|return|select|then|until|while)",
                        Some(Keyword),
                        Pop,
                    ),
                    (r#"\d+"#, Some(Number), Pop),
                    (r"\w+", Some(Method), Pop),
                ],
            },
            State {
                name: "string_single",
                rules: &[
                    (r#"'"#, Some(String), Pop),
                    (r#"\\"#, Some(String), Push("string_escape")),
                ],
            },
            State {
                name: "string_double",
                rules: &[
                    (r#"""#, Some(String), Pop),
                    (r#"\\"#, Some(String), Push("string_escape")),
                    (r#"\$"#, Some(Other), Push("variable")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, Some(String), Pop)] },
            State {
                name: "variable",
                rules: &[
                    (r#"[#?]"#, Some(Variable), Pop),
                    (r#"\{[^}]*\}"#, Some(Variable), Pop),
                    (r#"\w+"#, Some(Variable), Pop),
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
                    (r#"#.*"#, Some(Comment), Pop),
                    (r#"<#"#, Some(Comment), Push("comment")),
                    (r#"'"#, Some(String), Push("string_single")),
                    (r#"\""#, Some(String), Push("string_double")),
                    (r#"\$"#, Some(Variable), Push("variable")),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Some(Number), Pop),
                    (r#"-\w+"#, Some(Operator), Pop),
                    (r#"[!*/%+<=>|]"#, Some(Operator), Pop),
                    (
                        r#"(?i:break|catch|continue|do|elseif|else|finally|foreach|function|if|return|switch|throw|try|using|while)[\w-]+"#,
                        Some(Method),
                        Pop,
                    ),
                    (
                        r#"(?i:break|catch|continue|do|elseif|else|finally|foreach|function|if|return|switch|throw|try|using|while)"#,
                        Some(Keyword),
                        Pop,
                    ),
                    (r#"[\w-]+"#, Some(Method), Pop),
                ],
            },
            State { name: "comment", rules: &[(r#"#>"#, Some(Comment), Pop)] },
            State {
                name: "string_single",
                rules: &[
                    (r#"'"#, Some(String), Pop),
                    (r#"`"#, Some(String), Push("string_escape")),
                ],
            },
            State {
                name: "string_double",
                rules: &[
                    (r#"""#, Some(String), Pop),
                    (r#"`"#, Some(String), Push("string_escape")),
                    (r#"\$"#, Some(Other), Push("variable")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, Some(String), Pop)] },
            State {
                name: "variable",
                rules: &[
                    (r#"\("#, Some(Other), Pop), // TODO: subexpression
                    (r#"[$^?]"#, Some(Variable), Pop),
                    (r#"\{[^}]*\}"#, Some(Variable), Pop),
                    (r#"\w+"#, Some(Variable), Pop),
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
                    (r#"(?i:rem)\S+"#, Some(Other), Pop),
                    (r#"(?i:rem).*"#, Some(Comment), Pop),
                    (r#"::.*"#, Some(Comment), Pop),
                    (r#"""#, Some(String), Push("string")),
                    (r#"%%"#, Some(Other), Pop),
                    (r#"%"#, Some(Variable), Push("variable")),
                    (r#"[!*/+<=>|]"#, Some(Operator), Pop),
                    (
                        r"(?i:break|call|cd|chdir|cls|copy|del|dir|echo|exit|for|goto|if|md|mkdir|move|pause|ren|set)\w+",
                        Some(Other),
                        Pop,
                    ),
                    (
                        r"(?i:break|call|cd|chdir|cls|copy|del|dir|echo|exit|for|goto|if|md|mkdir|move|pause|ren|set)",
                        Some(Keyword),
                        Pop,
                    ),
                    (r#"\d+"#, Some(Number), Pop),
                ],
            },
            State {
                name: "string",
                rules: &[
                    (r#"""#, Some(String), Pop),
                    (r#"\\"#, Some(String), Push("string_escape")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, Some(String), Pop)] },
            State { name: "variable", rules: &[(r#"%"#, Some(Variable), Pop)] },
        ],
    },
];
