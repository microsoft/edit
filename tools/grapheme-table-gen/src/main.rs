mod rules;

use crate::rules::{JOIN_RULES_GRAPHEME_CLUSTER, JOIN_RULES_LINE_BREAK};
use anyhow::{bail, Context};
use indoc::writedoc;
use rayon::prelude::*;
use std::collections::HashMap;
use std::fmt::Write as FmtWrite;
use std::io::Write as IoWrite;
use std::ops::RangeInclusive;
use std::path::PathBuf;

type TrieType = u32;

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum CharacterWidth {
    ZeroWidth,
    Narrow,
    Wide,
    Ambiguous,
}

// NOTE: The order of these items must match JOIN_RULES_GRAPHEME_CLUSTER.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum ClusterBreak {
    Other,         // GB999
    CR,            // GB3, GB4, GB5
    LF,            // GB3, GB4, GB5
    Control,       // GB4, GB5
    Extend,        // GB9, GB9a -- includes SpacingMark
    RI,            // GB12, GB13
    Prepend,       // GB9b
    HangulL,       // GB6, GB7, GB8
    HangulV,       // GB6, GB7, GB8
    HangulT,       // GB6, GB7, GB8
    HangulLV,      // GB6, GB7, GB8
    HangulLVT,     // GB6, GB7, GB8
    InCBLinker,    // GB9c
    InCBConsonant, // GB9c
    ExtPic,        // GB11
    ZWJ,           // GB9, GB11
}

// NOTE: The order of these items must match JOIN_RULES_LINE_BREAK.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(non_camel_case_types)]
enum LineBreak {
    Other, // Anything else

    // Non-tailorable Line Breaking Classes
    WordJoiner,     // WJ
    ZeroWidthSpace, // ZW
    Glue,           // GL
    Space,          // SP

    // Break Opportunities
    BreakAfter,  // BA
    BreakBefore, // BB
    Hyphen,      // HY

    // Characters Prohibiting Certain Breaks
    ClosePunctuation,       // CL
    CloseParenthesis_EA,    // CP, East Asian
    CloseParenthesis_NotEA, // CP, not East Asian
    Exclamation,            // EX
    Inseparable,            // IN
    Nonstarter,             // NS
    OpenPunctuation_EA,     // OP, East Asian
    OpenPunctuation_NotEA,  // OP, not East Asian
    Quotation,              // QU

    // Numeric Context
    InfixNumericSeparator,     // IS
    Numeric,                   // NU
    PostfixNumeric,            // PO
    PrefixNumeric,             // PR
    SymbolsAllowingBreakAfter, // SY

    // Other Characters
    Alphabetic,  // AL & HL
    Ideographic, // ID & EB & EM
}

#[derive(Clone, Default)]
struct Ucd {
    description: String,
    values: Vec<u32>,
}

#[derive(Clone, Default)]
struct Stage {
    values: Vec<u32>,
    index: usize,
    shift: usize,
    mask: usize,
    bits: usize,
}

#[derive(Clone, Default)]
struct Trie {
    stages: Vec<Stage>,
    total_size: usize,
}

#[derive(Clone, Copy, Default)]
enum Language {
    #[default]
    C,
    Rust,
}

#[derive(Default)]
struct Output {
    arg_lang: Language,
    arg_no_ambiguous: bool,
    arg_line_breaks: bool,

    ucd: Ucd,
    trie: Trie,
    rules_gc: [Vec<u32>; 2],
    rules_lb: Vec<u32>,
    total_size: usize,
}

const HELP: &str = "\
Usage: grapheme-table-gen [options...] <ucd.nounihan.grouped.xml>
  -h, --help            Prints help information
  --lang=<c|rust>       Output language (default: c)
  --no-ambiguous        Treat all ambiguous characters as narrow
  --line-breaks         Store and expose line break information
";

fn main() -> anyhow::Result<()> {
    let mut args = pico_args::Arguments::from_env();
    if args.contains(["-h", "--help"]) {
        eprint!("{}", HELP);
        return Ok(());
    }

    let mut out = Output {
        arg_lang: args.value_from_fn("--lang", |arg| match arg {
            "c" => Ok(Language::C),
            "rust" => Ok(Language::Rust),
            l => bail!("invalid language: \"{}\"", l),
        })?,
        arg_no_ambiguous: args.contains("--no-ambiguous"),
        arg_line_breaks: args.contains("--line-breaks"),
        ..Default::default()
    };
    let arg_input = args.free_from_os_str(|s| -> Result<PathBuf, &'static str> { Ok(s.into()) })?;
    let arg_remaining = args.finish();
    if !arg_remaining.is_empty() {
        bail!("unrecognized arguments: {:?}", arg_remaining);
    }

    let input = std::fs::read_to_string(arg_input)?;
    let doc = roxmltree::Document::parse(&input)?;
    out.ucd = extract_values_from_ucd(&doc, &out)?;

    // Find the best trie configuration over the given block sizes (2^2 - 2^8) and stages (4).
    // More stages = Less size. The trajectory roughly follows a+b*c^stages, where c < 1.
    // 4 still gives ~30% savings over 3 stages and going beyond 5 gives diminishing returns (<10%).
    out.trie = build_best_trie(&out.ucd.values, 2, 8, 4);
    // The joinRules above has 2 bits per value. This packs it into 32-bit integers to save space.
    out.rules_gc = JOIN_RULES_GRAPHEME_CLUSTER
        .map(|t| t.iter().map(|row| prepare_rules_row(row, 2, 3)).collect());
    out.rules_lb = JOIN_RULES_LINE_BREAK
        .iter()
        .map(|row| prepare_rules_row(row, 1, 0))
        .collect();

    // Each rules item has the same length. Each item is 32 bits = 4 bytes.
    out.total_size = out.trie.total_size + out.rules_gc.len() * out.rules_gc[0].len() * 4;
    if out.arg_line_breaks {
        out.total_size += out.rules_lb.len() * 4;
    }

    // Run a quick sanity check to ensure that the trie works as expected.
    for (cp, &expected) in out.ucd.values.iter().enumerate() {
        let mut actual = 0;
        for s in &out.trie.stages {
            actual = s.values[actual as usize + ((cp >> s.shift) & s.mask)];
        }
        assert_eq!(
            expected, actual,
            "trie sanity check failed for U+{:04X}",
            cp
        );
    }
    for (cp, &expected) in out.ucd.values[..0x80].iter().enumerate() {
        let last = out.trie.stages.last().unwrap();
        let actual = last.values[cp];
        assert_eq!(
            expected, actual,
            "trie sanity check failed for direct ASCII mapping of U+{:04X}",
            cp
        );
    }

    let buf = match out.arg_lang {
        Language::C => generate_c(out),
        Language::Rust => generate_rust(out),
    };

    std::io::stdout().write_all(buf.as_bytes())?;
    Ok(())
}

impl Output {
    fn args(&self) -> String {
        let mut buf = String::new();
        match self.arg_lang {
            Language::C => buf.push_str("--lang=c"),
            Language::Rust => buf.push_str("--lang=rust"),
        }
        if self.arg_no_ambiguous {
            buf.push_str(" --no-ambiguous")
        }
        if self.arg_line_breaks {
            buf.push_str(" --line-breaks")
        }
        buf
    }
}

fn generate_c(out: Output) -> String {
    let mut buf = String::new();

    _ = writedoc!(
        buf,
        "
        // BEGIN: Generated by grapheme-table-gen on {}, from {}, with {}, {} bytes
        // clang-format off
        ",
        chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        out.ucd.description,
        out.args(),
        out.total_size,
    );

    for stage in &out.trie.stages {
        let mut width = 16;
        if stage.index != 0 {
            width = stage.mask + 1;
        }

        _ = write!(
            buf,
            "static const uint{}_t s_stage{}[] = {{",
            stage.bits, stage.index
        );
        for (j, &value) in stage.values.iter().enumerate() {
            if j % width == 0 {
                buf.push_str("\n   ");
            }
            _ = write!(buf, " 0x{:01$x},", value, stage.bits / 4);
        }
        buf.push_str("\n};\n");
    }

    _ = writeln!(
        buf,
        "static const uint32_t s_grapheme_cluster_join_rules[{}][{}] = {{",
        out.rules_gc.len(),
        out.rules_gc[0].len()
    );
    for table in &out.rules_gc {
        buf.push_str("    {\n");
        for &r in table {
            _ = writeln!(buf, "        0b{:032b},", r);
        }
        buf.push_str("    },\n");
    }
    buf.push_str("};\n");

    if out.arg_line_breaks {
        _ = writeln!(
            buf,
            "static const uint32_t s_line_break_join_rules[{}] = {{",
            out.rules_lb.len()
        );
        for r in &out.rules_lb {
            _ = writeln!(buf, "    0b{r:032b},");
        }
        buf.push_str("};\n");
    }

    _ = writedoc!(
        buf,
        "
        inline int ucd_grapheme_cluster_lookup(const uint32_t cp)
        {{
            if (cp < 0x80) {{
                return s_stage{}[cp];
            }}
        ",
        out.trie.stages.len() - 1,
    );
    for stage in &out.trie.stages {
        if stage.index == 0 {
            _ = writeln!(
                buf,
                "    const uint{}_t s0 = s_stage0[cp >> {}];",
                stage.bits, stage.shift,
            );
        } else {
            _ = writeln!(
                buf,
                "    const uint{}_t s{} = s_stage{}[s{} + ((cp >> {}) & {})];",
                stage.bits,
                stage.index,
                stage.index,
                stage.index - 1,
                stage.shift,
                stage.mask,
            );
        }
    }
    _ = writedoc!(
        buf,
        "
                return s{};
        }}
        ",
        out.trie.stages.len() - 1,
    );

    _ = writedoc!(
        buf,
        "
        inline int ucd_grapheme_cluster_joins(const int state, const int lead, const int trail)
        {{
            const int l = lead & 15;
            const int t = trail & 15;
            return (s_grapheme_cluster_join_rules[state][l] >> (t * 2)) & 3;
        }}
        inline bool ucd_grapheme_cluster_joins_done(const int state)
        {{
            return state == 3;
        }}
        inline int ucd_grapheme_cluster_character_width(const int val)
        {{
            return (val >> 4) & 3;
        }}
        ",
    );

    if out.arg_line_breaks {
        _ = writedoc!(
            buf,
            "
            inline bool ucd_line_break_joins(const int lead, const int trail)
            {{
                const int l = lead >> 6;
                const int t = trail >> 6;
                return (s_line_break_join_rules[l] >> t) & 1;
            }}
            ",
        );
    }

    buf.push_str("// clang-format on\n// END: Generated by grapheme-table-gen\n");
    buf
}

fn generate_rust(out: Output) -> String {
    let mut buf = String::new();

    _ = writeln!(
        buf,
        "// BEGIN: Generated by grapheme-table-gen on {}, from {}, with {}, {} bytes",
        chrono::Utc::now().to_rfc3339_opts(chrono::SecondsFormat::Secs, true),
        out.ucd.description,
        out.args(),
        out.total_size,
    );

    for stage in &out.trie.stages {
        let mut width = 16;
        if stage.index != 0 {
            width = stage.mask + 1;
        }

        _ = write!(
            buf,
            "#[rustfmt::skip]\nconst STAGE{}: [u{}; {}] = [",
            stage.index,
            stage.bits,
            stage.values.len(),
        );
        for (j, &value) in stage.values.iter().enumerate() {
            if j % width == 0 {
                buf.push_str("\n   ");
            }
            _ = write!(buf, " 0x{:01$x},", value, stage.bits / 4);
        }
        buf.push_str("\n];\n");
    }

    _ = writeln!(
        buf,
        "#[rustfmt::skip]\nconst GRAPHEME_JOIN_RULES: [[u32; {}]; {}] = [",
        out.rules_gc[0].len(),
        out.rules_gc.len(),
    );
    for table in &out.rules_gc {
        buf.push_str("    [\n");
        for &r in table {
            _ = writeln!(buf, "        0b{:032b},", r);
        }
        buf.push_str("    ],\n");
    }
    buf.push_str("];\n");

    if out.arg_line_breaks {
        _ = writeln!(
            buf,
            "#[rustfmt::skip]\nconst LINE_BREAK_JOIN_RULES: [u32; {}] = [",
            out.rules_lb.len(),
        );
        for r in &out.rules_lb {
            _ = writeln!(buf, "    0b{r:032b},");
        }
        buf.push_str("];\n");
    }

    _ = writedoc!(
        buf,
        "
        #[inline(always)]
        pub fn ucd_grapheme_cluster_lookup(cp: char) -> usize {{
            unsafe {{
                let cp = cp as usize;
                if cp < 0x80 {{
                    return STAGE{}[cp] as usize;
                }}
        ",
        out.trie.stages.len() - 1,
    );
    for stage in &out.trie.stages {
        if stage.index == 0 {
            _ = writeln!(
                buf,
                "        let s = *STAGE{}.get_unchecked(cp >> {}) as usize;",
                stage.index, stage.shift,
            );
        } else if stage.index != out.trie.stages.len() - 1 {
            _ = writeln!(
                buf,
                "        let s = *STAGE{}.get_unchecked(s + ((cp >> {}) & {})) as usize;",
                stage.index, stage.shift, stage.mask,
            );
        } else {
            _ = writeln!(
                buf,
                "        let s = *STAGE{}.get_unchecked(s + (cp & {})) as usize;",
                stage.index, stage.mask,
            );
        }
    }
    _ = writedoc!(
        buf,
        "
                s
            }}
        }}
        ",
    );

    _ = writedoc!(
        buf,
        "
        #[inline(always)]
        pub fn ucd_grapheme_cluster_joins(state: u32, lead: usize, trail: usize) -> u32 {{
            unsafe {{
                let l = lead & 15;
                let t = trail & 15;
                let s = GRAPHEME_JOIN_RULES.get_unchecked(state as usize);
                (s[l] >> (t * 2)) & 3
            }}
        }}
        #[inline(always)]
        pub fn ucd_grapheme_cluster_joins_done(state: u32) -> bool {{
            state == 3
        }}
        #[inline(always)]
        pub fn ucd_grapheme_cluster_character_width(val: usize) -> usize {{
            (val >> 4) & 3
        }}
        ",
    );

    if out.arg_line_breaks {
        _ = writedoc!(
            buf,
            "
            #[inline(always)]
            pub fn ucd_line_break_joins(lead: usize, trail: usize) -> bool {{
                unsafe {{
                    let l = lead >> 6;
                    let t = trail >> 6;
                    let s = *LINE_BREAK_JOIN_RULES.get_unchecked(l as usize);
                    ((s >> t) & 1) != 0
                }}
            }}
            ",
        );
    }

    buf.push_str("// END: Generated by grapheme-table-gen\n");
    buf
}

fn extract_values_from_ucd(doc: &roxmltree::Document, out: &Output) -> anyhow::Result<Ucd> {
    let ambiguous_value = if out.arg_no_ambiguous {
        CharacterWidth::Narrow
    } else {
        CharacterWidth::Ambiguous
    };
    let mut values = vec![
        trie_value(
            ClusterBreak::Other,
            CharacterWidth::Narrow,
            LineBreak::Other
        );
        1114112
    ];

    let ns = "http://www.unicode.org/ns/2003/ucd/1.0";
    let root = doc.root_element();
    let description = root
        .children()
        .find(|n| n.has_tag_name((ns, "description")))
        .context("missing ucd description")?;
    let repertoire = root
        .children()
        .find(|n| n.has_tag_name((ns, "repertoire")))
        .context("missing ucd repertoire")?;
    let description = description.text().unwrap_or_default().to_string();

    for group in repertoire.children().filter(|n| n.is_element()) {
        const DEFAULT_ATTRIBUTES: UcdAttributes = UcdAttributes {
            general_category: "",
            line_break: "",
            grapheme_cluster_break: "",
            indic_conjunct_break: "",
            extended_pictographic: "",
            east_asian: "",
        };
        let group_attributes = extract_attributes(&group, &DEFAULT_ATTRIBUTES);

        for char in group.children().filter(|n| n.is_element()) {
            let char_attributes = extract_attributes(&char, &group_attributes);
            let range = extract_range(&char);

            let mut cb = match char_attributes.grapheme_cluster_break {
                "XX" => ClusterBreak::Other, // Anything else
                // We ignore GB3 which demands that CR × LF do not break apart, because
                // * these control characters won't normally reach our text storage
                // * otherwise we're in a raw write mode and historically conhost stores them in separate cells
                "CR" => ClusterBreak::CR,            // Carriage Return
                "LF" => ClusterBreak::LF,            // Line Feed
                "CN" => ClusterBreak::Control,       // Control
                "EX" | "SM" => ClusterBreak::Extend, // Extend, SpacingMark
                "PP" => ClusterBreak::Prepend,       // Prepend
                "ZWJ" => ClusterBreak::ZWJ,          // Zero Width Joiner
                "RI" => ClusterBreak::RI,            // Regional Indicator
                "L" => ClusterBreak::HangulL,        // Hangul Syllable Type L
                "V" => ClusterBreak::HangulV,        // Hangul Syllable Type V
                "T" => ClusterBreak::HangulT,        // Hangul Syllable Type T
                "LV" => ClusterBreak::HangulLV,      // Hangul Syllable Type LV
                "LVT" => ClusterBreak::HangulLVT,    // Hangul Syllable Type LVT
                _ => bail!(
                    "Unrecognized GCB {:?} for U+{:04X} to U+{:04X}",
                    char_attributes.grapheme_cluster_break,
                    range.start(),
                    range.end()
                ),
            };

            if char_attributes.extended_pictographic == "Y" {
                // Currently every single Extended_Pictographic codepoint happens to be GCB=XX.
                // This is fantastic for us because it means we can stuff it into the ClusterBreak enum
                // and treat it as an alias of EXTEND, but with the special GB11 properties.
                if cb != ClusterBreak::Other {
                    bail!(
                        "Unexpected GCB {:?} with ExtPict=Y for U+{:04X} to U+{:04X}",
                        char_attributes.grapheme_cluster_break,
                        range.start(),
                        range.end()
                    );
                }

                cb = ClusterBreak::ExtPic;
            }

            cb = match char_attributes.indic_conjunct_break {
                "None" | "Extend" => cb,
                "Linker" => ClusterBreak::InCBLinker,
                "Consonant" => ClusterBreak::InCBConsonant,
                _ => bail!(
                    "Unrecognized InCB {:?} for U+{:04X} to U+{:04X}",
                    char_attributes.indic_conjunct_break,
                    range.start(),
                    range.end()
                ),
            };

            let mut width = match char_attributes.east_asian {
                "N" | "Na" | "H" => CharacterWidth::Narrow, // Half-width, Narrow, Neutral
                "F" | "W" => CharacterWidth::Wide,          // Wide, Full-width
                "A" => ambiguous_value,                     // Ambiguous
                _ => bail!(
                    "Unrecognized ea {:?} for U+{:04X} to U+{:04X}",
                    char_attributes.east_asian,
                    range.start(),
                    range.end()
                ),
            };

            // There's no "ea" attribute for "zero width" so we need to do that ourselves. This matches:
            //   Me: Mark, enclosing
            //   Mn: Mark, non-spacing
            //   Cf: Control, format
            match char_attributes.general_category {
                "Cf" if cb == ClusterBreak::Control => {
                    // A significant portion of Cf characters are not just gc=Cf (= commonly considered zero-width),
                    // but also GCB=CN (= does not join). This is a bit of a problem for terminals,
                    // because they don't support zero-width graphemes, as zero-width columns can't exist.
                    // So, we turn all of them into Extend, which is roughly how wcswidth() would treat them.
                    cb = ClusterBreak::Extend;
                    width = CharacterWidth::ZeroWidth;
                }
                "Me" | "Mn" | "Cf" => {
                    width = CharacterWidth::ZeroWidth;
                }
                _ => {}
            };

            let lb = if out.arg_line_breaks {
                let lb_ea = matches!(char_attributes.east_asian, "F" | "W" | "H");
                match char_attributes.line_break {
                    "WJ" => LineBreak::WordJoiner,
                    "ZW" => LineBreak::ZeroWidthSpace,
                    "GL" => LineBreak::Glue,
                    "SP" => LineBreak::Space,

                    "BA" => LineBreak::BreakAfter,
                    "BB" => LineBreak::BreakBefore,
                    "HY" => LineBreak::Hyphen,

                    "CL" => LineBreak::ClosePunctuation,
                    "CP" if lb_ea => LineBreak::CloseParenthesis_EA,
                    "CP" => LineBreak::CloseParenthesis_NotEA,
                    "EX" => LineBreak::Exclamation,
                    "IN" => LineBreak::Inseparable,
                    "NS" => LineBreak::Nonstarter,
                    "OP" if lb_ea => LineBreak::OpenPunctuation_EA,
                    "OP" => LineBreak::OpenPunctuation_NotEA,
                    "QU" => LineBreak::Quotation,

                    "IS" => LineBreak::InfixNumericSeparator,
                    "NU" => LineBreak::Numeric,
                    "PO" => LineBreak::PostfixNumeric,
                    "PR" => LineBreak::PrefixNumeric,
                    "SY" => LineBreak::SymbolsAllowingBreakAfter,

                    "AL" | "HL" => LineBreak::Alphabetic,
                    "ID" | "EB" | "EM" => LineBreak::Ideographic,

                    _ => LineBreak::Other,
                }
            } else {
                LineBreak::Other
            };

            values[range].fill(trie_value(cb, width, lb));
        }
    }

    // U+00AD: Soft Hyphen
    // A soft hyphen is a hint that a word break is allowed at that position.
    // By default, the glyph is supposed to be invisible, and only if
    // a word break occurs, the text renderer should display a hyphen.
    // A terminal does not support computerized typesetting, but unlike the other
    // gc=Cf cases we give it a Narrow width, because that matches wcswidth().
    values[0x00AD] = trie_value_mod_width(values[0x00AD], CharacterWidth::Narrow);

    // U+2500 to U+257F: Box Drawing block
    // U+2580 to U+259F: Block Elements block
    // By default, CharacterWidth.Ambiguous, but by convention .Narrow in terminals.
    //
    // Most of these characters are LineBreak.Other, but some are actually LineBreak.Alphabetic.
    // But to us this doesn't really matter much, because it doesn't make much sense anyway that
    // a light double dash is "alphabetic" while a light triple dash is not.
    values[0x2500..=0x259F].fill(trie_value(
        ClusterBreak::Other,
        CharacterWidth::Narrow,
        LineBreak::Other,
    ));

    // U+FE0F Variation Selector-16 is used to turn unqualified Emojis into qualified ones.
    // By convention, this turns them from being ambiguous width (= narrow) into wide ones.
    // We achieve this here by explicitly giving this codepoint a wide width.
    // Later down below we'll clamp width back to <= 2.
    //
    // U+FE0F actually has a LineBreak property of CM (Combining Mark),
    // but for us that's equivalent to Other.
    values[0xFE0F] = trie_value_mod_width(values[0xFE0F], CharacterWidth::Wide);

    Ok(Ucd {
        description,
        values,
    })
}

struct UcdAttributes<'a> {
    general_category: &'a str,
    line_break: &'a str,
    grapheme_cluster_break: &'a str,
    indic_conjunct_break: &'a str,
    extended_pictographic: &'a str,
    east_asian: &'a str,
}

fn extract_attributes<'a>(
    node: &'a roxmltree::Node,
    default: &'a UcdAttributes,
) -> UcdAttributes<'a> {
    UcdAttributes {
        general_category: node.attribute("gc").unwrap_or(default.general_category),
        line_break: node.attribute("lb").unwrap_or(default.line_break),
        grapheme_cluster_break: node
            .attribute("GCB")
            .unwrap_or(default.grapheme_cluster_break),
        indic_conjunct_break: node
            .attribute("InCB")
            .unwrap_or(default.indic_conjunct_break),
        extended_pictographic: node
            .attribute("ExtPict")
            .unwrap_or(default.extended_pictographic),
        east_asian: node.attribute("ea").unwrap_or(default.east_asian),
    }
}

fn extract_range(node: &roxmltree::Node) -> RangeInclusive<usize> {
    let (first, last) = match node.attribute("cp") {
        Some(val) => {
            let cp = usize::from_str_radix(val, 16).unwrap();
            (cp, cp)
        }
        None => (
            usize::from_str_radix(node.attribute("first-cp").unwrap_or("0"), 16).unwrap(),
            usize::from_str_radix(node.attribute("last-cp").unwrap_or("0"), 16).unwrap(),
        ),
    };
    first..=last
}

fn trie_value(cb: ClusterBreak, width: CharacterWidth, lb: LineBreak) -> TrieType {
    let cb = cb as TrieType;
    let width = (width as TrieType) << 4;
    let lb = (lb as TrieType) << 6;
    cb | width | lb
}

fn trie_value_mod_width(value: TrieType, width: CharacterWidth) -> TrieType {
    let value = value & !(3 << 4); // mask out the width bits
    let width = (width as TrieType) << 4;
    value | width
}

fn build_best_trie(
    uncompressed: &[TrieType],
    min_shift: usize,
    max_shift: usize,
    stages: usize,
) -> Trie {
    let depth = stages - 1;
    let delta = max_shift - min_shift + 1;
    let total = delta.pow(depth as u32);

    let mut tasks = Vec::new();
    for i in 0..total {
        let mut shifts = vec![0; depth];
        let mut index = i;
        for s in &mut shifts {
            *s = min_shift + (index % delta);
            index /= delta;
        }
        tasks.push(shifts);
    }

    tasks
        .par_iter()
        .map(|shifts| build_trie(uncompressed.to_vec(), shifts))
        .min_by_key(|t| t.total_size)
        .unwrap()
}

fn build_trie(mut uncompressed: Vec<TrieType>, shifts: &[usize]) -> Trie {
    let mut cumulative_shift = 0;
    let mut stages = Vec::new();

    for (stage, &shift) in shifts.iter().enumerate() {
        let chunk_size = 1 << shift;
        let mut cache = HashMap::new();
        let mut compressed = Vec::new();
        let mut offsets = Vec::new();
        let mut off = 0;

        while off < uncompressed.len() {
            let chunk = &uncompressed[off..off + chunk_size.min(uncompressed.len() - off)];

            let offset = if stage == 0 && off < 0x80 {
                // The first stage (well, really the last stage - the one which contains the values instead of indices)
                // contains a direct 1:1 mapping for all ASCII codepoints as they're most common in IT environments.
                compressed.extend_from_slice(chunk);
                (compressed.len() - chunk.len()) as TrieType
            } else {
                *cache.entry(chunk).or_insert_with(|| {
                    if let Some(existing) = find_existing(&compressed, chunk) {
                        existing as TrieType
                    } else {
                        let overlap = measure_overlap(&compressed, chunk);
                        compressed.extend_from_slice(&chunk[overlap..]);
                        (compressed.len() - chunk.len()) as TrieType
                    }
                })
            };

            offsets.push(offset);
            off += chunk.len();
        }

        stages.push(Stage {
            values: compressed,
            index: shifts.len() - stages.len(),
            shift: cumulative_shift,
            mask: chunk_size - 1,
            bits: 0,
        });

        uncompressed = offsets;
        cumulative_shift += shift;
    }

    stages.push(Stage {
        values: uncompressed,
        index: 0,
        shift: cumulative_shift,
        mask: usize::MAX,
        bits: 0,
    });

    stages.reverse();

    for stage in stages.iter_mut() {
        let max_val = stage.values.iter().max().cloned().unwrap_or(0);
        stage.bits = match max_val {
            0..0x100 => 8,
            0x100..0x10000 => 16,
            _ => 32,
        };
    }

    let total_size: usize = stages
        .iter()
        .map(|stage| (stage.bits / 8) * stage.values.len())
        .sum();

    Trie { stages, total_size }
}

fn find_existing(haystack: &[TrieType], needle: &[TrieType]) -> Option<usize> {
    haystack
        .windows(needle.len())
        .position(|window| window == needle)
}

fn measure_overlap(prev: &[TrieType], next: &[TrieType]) -> usize {
    (0..prev.len().min(next.len()))
        .rev()
        .find(|&i| prev[prev.len() - i..] == next[..i])
        .unwrap_or(0)
}

fn prepare_rules_row(row: &[i32], bit_width: usize, non_joiner_value: i32) -> u32 {
    row.iter().enumerate().fold(0u32, |acc, (trail, &value)| {
        let value = if value < 0 { non_joiner_value } else { value };
        acc | ((value as u32) << (trail * bit_width))
    })
}
