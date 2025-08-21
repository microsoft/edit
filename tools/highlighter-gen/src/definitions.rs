#![allow(dead_code)]

use HighlightKind::*;
use IndexedColor::*;

pub const LANGUAGES: &[&Language] = &[&LANG_GIT_COMMIT, &LANG_GIT_REBASE];

const LANG_DIFF: Language = Language {
    name: "Diff",
    filenames: &["*.diff", "*.patch"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"diff"#).is(Direct(BrightBlue)).then_goto("ignore"),
                re(r#"---"#).is(Direct(BrightBlue)).then_goto("ignore"),
                re(r#"\+\+\+"#).is(Direct(BrightBlue)).then_goto("ignore"),
                re(r#"-"#).is(Direct(BrightRed)).then_goto("ignore"),
                re(r#"\+"#).is(Direct(BrightGreen)).then_goto("ignore"),
                re(r#""#).then_goto("ignore"),
            ],
        },
        State {
            name: "ignore",
            rules: &[re(r#".*"#)],
        },
    ],
};

const LANG_GIT_COMMIT: Language = Language {
    name: "Git Commit Message",
    filenames: &["COMMIT_EDITMSG", "MERGE_MSG"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"#"#).is(Comment).then_call("comment"),
                re(r#"diff \-\-git.*"#)
                    .is(Direct(BrightBlue))
                    .then_call("diff_transition"),
                re(r#""#).then_goto("ignore"),
            ],
        },
        State {
            name: "comment",
            rules: &[
                re(r#"\tdeleted:.*"#).is(Direct(BrightRed)).then_return(),
                re(r#"\tmodified:.*"#).is(Direct(BrightBlue)).then_return(),
                re(r#"\tnew file:.*"#).is(Direct(BrightGreen)).then_return(),
                re(r#"\trenamed:.*"#).is(Direct(BrightBlue)).then_return(),
                re(r#".*"#).then_return(),
            ],
        },
        State {
            name: "diff_transition",
            rules: &[re(r#""#).is(Other).then_call("diff")],
        },
        // TODO: The ability to invoke another language (here: LANG_DIFF). :)
        State {
            name: "diff",
            rules: &[
                re(r#"diff"#).is(Direct(BrightBlue)).then_goto("ignore"),
                re(r#"---"#).is(Direct(BrightBlue)).then_goto("ignore"),
                re(r#"\+\+\+"#).is(Direct(BrightBlue)).then_goto("ignore"),
                re(r#"-"#).is(Direct(BrightRed)).then_goto("ignore"),
                re(r#"\+"#).is(Direct(BrightGreen)).then_goto("ignore"),
                re(r#""#).then_goto("ignore"),
            ],
        },
        State {
            name: "ignore",
            rules: &[re(r#".*"#)],
        },
    ],
};

const LANG_GIT_REBASE: Language = Language {
    name: "Git Rebase Message",
    filenames: &["git-rebase-todo"], // TODO: https://github.com/microsoft/vscode/issues/156954
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"(?:break|exec|b|x)\b{end-half}"#)
                    .is(Keyword)
                    .then_call("comment"),
                re(r#"(?:drop|edit|fixup|pick|reword|squash|d|e|f|p|r|s)\b{end-half}"#)
                    .is(Keyword)
                    .then_call("hash"),
                re(r#"#.*"#).is(Comment),
            ],
        },
        State {
            name: "hash",
            rules: &[
                re(r#"\S+"#).is(Variable).then_call("comment"),
                re(r#"\s+"#),
                re(r#".*"#).then_return(),
            ],
        },
        State {
            name: "comment",
            rules: &[re(r#".*"#).is(Comment).then_return()],
        },
    ],
};

const LANG_JSON: Language = Language {
    name: "JSON",
    filenames: &["*.json", "*.jsonc"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"//.*"#).is(Comment),
                re(r#"/\*"#).is(Comment).then_call("comment"),
                re(r#"""#).is(String).then_goto("string_double"),
                re(r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#)
                    .is(Number)
                    .then_goto("resolve_type"),
                re(r#"(?i:false)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:null)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:true)"#).is(Keyword).then_goto("resolve_type"),
            ],
        },
        State {
            name: "resolve_type",
            rules: &[re(r#"\w+"#).is(Other), re(r#""#)],
        },
        State {
            name: "comment",
            rules: &[re(r#"\*/"#).then_return()],
        },
        State {
            name: "string_double",
            rules: &[re(r#"""#), re(r#"\\."#).then_goto("string_double")],
        },
    ],
};

const LANG_YAML: Language = Language {
    name: "YAML",
    filenames: &["*.yaml", "*.yml"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"#.*"#).is(Comment),
                re(r#"""#).is(String).then_goto("string_double"),
                re(r#"'"#).is(String).then_goto("string_single"),
                re(r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#)
                    .is(Number)
                    .then_goto("resolve_type"),
                re(r#"(?i:false)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:null)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:true)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"\w+"#).is(String).then_goto("resolve_type"),
            ],
        },
        State {
            name: "resolve_type",
            rules: &[
                re(r#"\s*[^\s#:]+:"#)
                    .is(Keyword)
                    .then_goto("resolve_type_maybe_keyword"),
                re(r#"\s*[^\s#:]+"#).is(String),
                re(r#"\s*:"#)
                    .is(Keyword)
                    .then_goto("resolve_type_maybe_keyword"),
                re(r#""#),
            ],
        },
        State {
            name: "resolve_type_maybe_keyword",
            rules: &[re(r#"[^\s#:]+[^#]*"#).is(String), re(r#""#)],
        },
        State {
            name: "string_double",
            rules: &[re(r#"""#), re(r#"\\."#).then_goto("string_double")],
        },
        State {
            name: "string_single",
            rules: &[re(r#"'"#), re(r#"\\."#).then_goto("string_single")],
        },
    ],
};

const LANG_BASH: Language = Language {
    name: "Bash",
    filenames: &["*.sh", "*.zsh"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"#.*"#).is(Comment),
                re(r#"'"#).is(String).then_call("string_single"),
                re(r#"""#).is(String).then_call("string_double"),
                re(r#"\$"#).is(Variable).then_call("variable"),
                re(r#"[!*/%+<=>|]"#).is(Operator),
                re(r"(?i:break)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:case)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:continue)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:done)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:do)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:elif)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:else)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:esac)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:fi)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:for)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:function)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:if)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:in)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:return)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:select)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:then)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:until)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:while)").is(Keyword).then_goto("resolve_type"),
                re(r#"\d+"#).is(Number),
                re(r"\w+").is(Method),
            ],
        },
        State {
            name: "string_single",
            rules: &[
                re(r#"'"#).then_return(),
                re(r#"\\."#).then_goto("string_single"),
            ],
        },
        State {
            name: "string_double",
            rules: &[
                re(r#"""#).then_return(),
                re(r#"\\."#).then_goto("string_double"),
                re(r#"\$"#).is(Other).then_call("variable"),
            ],
        },
        State {
            name: "variable",
            rules: &[
                re(r#"[#?]"#).is(Variable).then_return(),
                re(r#"\{[^}]*\}"#).is(Variable).then_return(),
                re(r#"\w+"#).is(Variable).then_return(),
                re(r#""#).is(Other).then_return(),
            ],
        },
        State {
            name: "resolve_type",
            rules: &[re(r#"\w+"#).is(Other), re(r#""#)],
        },
    ],
};

const LANG_POWERSHELL: Language = Language {
    name: "PowerShell",
    filenames: &["*.ps1", "*.psm1", "*.psd1"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"#.*"#).is(Comment),
                re(r#"<#"#).is(Comment).then_call("comment"),
                re(r#"'"#).is(String).then_call("string_single"),
                re(r#"\""#).is(String).then_call("string_double"),
                re(r#"\$\("#).is(Other).then_call("ground"),
                re(r#"\$"#).is(Variable).then_call("variable"),
                re(r#"\("#).is(Other).then_call("ground"),
                re(r#"\)"#).is(Other).then_return(),
                re(r#"(?:-\d+|\d+)(?:\.\d+)?(?:[eE][+-]?\d+)?"#).is(Number),
                re(r#"-\w+"#).is(Operator),
                re(r#"[!*/%+<=>|]"#).is(Operator),
                re(r#"(?i:break)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:catch)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:continue)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:do)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:elseif)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:else)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:finally)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:foreach)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:for)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:function)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:if)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:return)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:switch)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:throw)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:try)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:using)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"(?i:while)"#).is(Keyword).then_goto("resolve_type"),
                re(r#"[\w-]+"#).is(Method),
            ],
        },
        State {
            name: "comment",
            rules: &[re(r#"#>"#).is(Comment).then_return()],
        },
        State {
            name: "string_single",
            rules: &[
                re(r#"'"#).then_return(),
                re(r#"`."#).then_goto("string_single"),
            ],
        },
        State {
            name: "string_double",
            rules: &[
                re(r#"""#).then_return(),
                re(r#"`."#).then_goto("string_double"),
                re(r#"\$\("#).is(Other).then_call("ground"),
                re(r#"\$"#).is(Variable).then_call("variable"),
            ],
        },
        State {
            name: "variable",
            rules: &[
                re(r#"[$^?]"#).then_return(),
                re(r#"\{[^}]*\}"#).then_return(),
                re(r#"\w+"#).then_return(),
                re(r#""#).is(Other).then_return(),
            ],
        },
        State {
            name: "resolve_type",
            rules: &[re(r#"[\w-]+"#).is(Other), re(r#""#)],
        },
    ],
};

const LANG_BATCH: Language = Language {
    name: "Batch",
    filenames: &["*.bat", "*.cmd"],
    states: &[
        State {
            name: "ground",
            rules: &[
                re(r#"(?i:rem)\S+"#).is(Other),
                re(r#"(?i:rem).*"#).is(Comment),
                re(r#"::.*"#).is(Comment),
                re(r#"""#).is(String).then_call("string_double"),
                re(r#"%%"#).is(Other),
                re(r#"%"#).is(Variable).then_call("variable"),
                re(r#"[!*/+<=>|]"#).is(Operator),
                re(r"(?i:break)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:call)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:cd)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:chdir)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:cls)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:copy)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:del)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:dir)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:echo)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:exit)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:for)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:goto)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:if)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:md)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:mkdir)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:move)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:pause)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:ren)").is(Keyword).then_goto("resolve_type"),
                re(r"(?i:set)").is(Keyword).then_goto("resolve_type"),
                re(r#"\d+"#).is(Number),
            ],
        },
        State {
            name: "string_double",
            rules: &[
                re(r#"""#).then_return(),
                re(r#"\\."#).then_goto("string_double"),
            ],
        },
        State {
            name: "variable",
            rules: &[re(r#"%"#).then_return()],
        },
        State {
            name: "resolve_type",
            rules: &[re(r#"\w+"#).is(Other), re(r#""#)],
        },
    ],
};

#[allow(dead_code)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IndexedColor {
    Black,
    Red,
    Green,
    Yellow,
    Blue,
    Magenta,
    Cyan,
    White,
    BrightBlack,
    BrightRed,
    BrightGreen,
    BrightYellow,
    BrightBlue,
    BrightMagenta,
    BrightCyan,
    BrightWhite,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum HighlightKind {
    Other,
    Direct(IndexedColor),
    Comment,
    Number,
    String,
    Variable,
    Operator,
    Keyword,
    Method,
}

pub struct Language {
    pub name: &'static str,
    pub filenames: &'static [&'static str],
    pub states: &'static [State],
}

pub struct State {
    pub name: &'static str,
    pub rules: &'static [Rule],
}

pub enum Instruction {
    Continue,
    Change(&'static str),
    Push(&'static str),
    Pop,
}

pub struct Rule {
    pub pattern: &'static str,
    pub kind: Option<HighlightKind>,
    pub action: ActionDefinition,
}

const fn re(s: &'static str) -> Rule {
    Rule {
        pattern: s,
        kind: None,
        action: ActionDefinition::Continue,
    }
}

impl Rule {
    const fn is(mut self, kind: HighlightKind) -> Self {
        self.kind = Some(kind);
        self
    }

    const fn then_goto(mut self, target: &'static str) -> Self {
        self.action = ActionDefinition::Change(target);
        self
    }

    const fn then_call(mut self, target: &'static str) -> Self {
        self.action = ActionDefinition::Push(target);
        self
    }

    const fn then_return(mut self) -> Self {
        self.action = ActionDefinition::Pop;
        self
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ActionDefinition {
    Continue,
    Change(&'static str),
    Push(&'static str),
    Pop,
}
