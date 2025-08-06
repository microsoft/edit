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
                        Change("type_resolve"),
                    ),
                    (r#"(?:true|false|null)"#, Some(Keyword), Change("type_resolve")),
                    (r#"\w+"#, Some(String), Change("type_resolve")),
                ],
            },
            State {
                name: "type_resolve",
                rules: &[
                    (r#"\s*[^\s#:]+:"#, Some(Keyword), Change("type_resolve_maybe_keyword")),
                    (r#"\s*[^\s#:]+"#, Some(String), Pop),
                    (r#"\s*:"#, Some(Keyword), Change("type_resolve_maybe_keyword")),
                    (r#""#, None, Pop),
                ],
            },
            State {
                name: "type_resolve_maybe_keyword",
                rules: &[(r#"[^\s#:]+[^#]*"#, Some(String), Pop), (r#""#, None, Pop)],
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
                    (r#"\)"#, Some(Other), Pop),
                    (r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#, Some(Number), Pop),
                    (r#"-\w+"#, Some(Operator), Pop),
                    (r#"[!*/%+<=>|]"#, Some(Operator), Pop),
                    (
                        r#"(?i:break|catch|continue|do|elseif|else|finally|foreach|for|function|if|return|switch|throw|try|using|while)[\w-]+"#,
                        Some(Method),
                        Pop,
                    ),
                    (
                        r#"(?i:break|catch|continue|do|elseif|else|finally|foreach|for|function|if|return|switch|throw|try|using|while)"#,
                        Some(Keyword),
                        Pop,
                    ),
                    (r#"[\w-]+"#, Some(Method), Pop),
                ],
            },
            State { name: "comment", rules: &[(r#"#>"#, Some(Comment), Pop)] },
            State {
                name: "string_single",
                rules: &[(r#"'"#, None, Pop), (r#"`"#, None, Push("string_escape"))],
            },
            State {
                name: "string_double",
                rules: &[
                    (r#"""#, None, Pop),
                    (r#"`"#, None, Push("string_escape")),
                    (r#"\$\("#, Some(Other), Push("ground")),
                    (r#"\$"#, Some(Variable), Push("variable")),
                ],
            },
            State { name: "string_escape", rules: &[(r#"."#, None, Pop)] },
            State {
                name: "variable",
                rules: &[
                    (r#"[$^?]"#, None, Pop),
                    (r#"\{[^}]*\}"#, None, Pop),
                    (r#"\w+"#, None, Pop),
                    (r#""#, Some(Other), Pop),
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
