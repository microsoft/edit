// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

// Used as an indicator in our rules for ÷ ("does not join").
// Underscore is one of the few characters that are permitted as an identifier,
// are monospace in most fonts and also visually distinct from the digits.
const X: i32 = -1;

// The following rules are based on the Grapheme Cluster Boundaries section of Unicode Standard Annex #29,
// but slightly modified to allow for use with a plain MxN lookup table.
//
// Break at the start and end of text, unless the text is empty.
// GB1:   ~ sot ÷ Any
// GB2:   ~ Any ÷ eot
//        Handled by our ucd_* functions.
//
// Do not break between a CR and LF. Otherwise, break before and after controls.
// GB3:   ✓ CR × LF
// GB4:   ✓ (Control | CR | LF) ÷
// GB5:   ✓ ÷ (Control | CR | LF)
//
// Do not break Hangul syllable or other conjoining sequences.
// GB6:   ✓ L × (L | V | LV | LVT)
// GB7:   ✓ (LV | V) × (V | T)
// GB8:   ✓ (LVT | T) × T
//
// Do not break before extending characters or ZWJ.
// GB9:   ✓ × (Extend | ZWJ)
//
// Do not break before SpacingMarks, or after Prepend characters.
// GB9a:  ✓ × SpacingMark
// GB9b:  ✓ Prepend ×
//
// Do not break within certain combinations with Indic_Conjunct_Break (InCB)=Linker.
// GB9c:  ~ \p{InCB=Linker} × \p{InCB=Consonant}
//                          × \p{InCB=Linker}
//        modified from
//          \p{InCB=Consonant} [ \p{InCB=Extend} \p{InCB=Linker} ]* \p{InCB=Linker} [ \p{InCB=Extend} \p{InCB=Linker} ]* × \p{InCB=Consonant}
//        because this has almost the same effect from what I can tell for most text, and greatly simplifies our design.
//
// Do not break within emoji modifier sequences or emoji zwj sequences.
// GB11:  ~ ZWJ × \p{Extended_Pictographic}    modified from    \p{Extended_Pictographic} Extend* ZWJ × \p{Extended_Pictographic}
//        because this allows us to use LUTs, while working for most valid text.
//
// Do not break within emoji flag sequences. That is, do not break between regional indicator (RI) symbols if there is an odd number of RI characters before the break point.
// GB12:  ~ sot (RI RI)* RI × RI
// GB13:  ~ [^RI] (RI RI)* RI × RI
//        the lookup table we generate supports RIs via something akin to RI ÷ RI × RI ÷ RI, but the corresponding
//        grapheme cluster algorithm doesn't count them. It would need to be updated to recognize and special-case RIs.
//
// Otherwise, break everywhere.
// GB999: ✓ Any ÷ Any
//
// This is a great reference for the resulting table:
// https://www.unicode.org/Public/UCD/latest/ucd/auxiliary/GraphemeBreakTest.html
#[rustfmt::skip]
pub const JOIN_RULES_GRAPHEME_CLUSTER: [[[i32; 16]; 16]; 2] = [
    // Base table
    [
        /* ↓ leading        → trailing codepoint                                                                                                                                                                   */
        /*               |   Other  |    CR    |    LF    |  Control |  Extend  |    RI    | Prepend  |  HangulL |  HangulV |  HangulT | HangulLV | HangulLVT | InCBLinker | InCBConsonant |  ExtPic  |    ZWJ   | */
        /* Other         | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* CR            | */ [X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, X /*  |   */, X /*    | */, X /* | */, X /* | */],
        /* LF            | */ [X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, X /*  |   */, X /*    | */, X /* | */, X /* | */],
        /* Control       | */ [X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, X /*  |   */, X /*    | */, X /* | */, X /* | */],
        /* Extend        | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* RI            | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 1 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* Prepend       | */ [0 /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /*  |  */, 0 /*  |   */, 0 /*    | */, 0 /* | */, 0 /* | */],
        /* HangulL       | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, X /* | */, 0 /* | */, 0 /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulV       | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulT       | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulLV      | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulLVT     | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* InCBLinker    | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, 0 /*    | */, X /* | */, 0 /* | */],
        /* InCBConsonant | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* ExtPic        | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* ZWJ           | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, 0 /* | */, 0 /* | */],
    ],
    // Once we have encountered a Regional Indicator pair we'll enter this table.
    // It's a copy of the base table, but instead of RI × RI, we're RI ÷ RI.
    [
        /* ↓ leading        → trailing codepoint                                                                                                                                                                   */
        /*               |   Other  |    CR    |    LF    |  Control |  Extend  |    RI    | Prepend  |  HangulL |  HangulV |  HangulT | HangulLV | HangulLVT | InCBLinker | InCBConsonant |  ExtPic  |    ZWJ   | */
        /* Other         | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* CR            | */ [X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, X /*  |   */, X /*    | */, X /* | */, X /* | */],
        /* LF            | */ [X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, X /*  |   */, X /*    | */, X /* | */, X /* | */],
        /* Control       | */ [X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, X /*  |   */, X /*    | */, X /* | */, X /* | */],
        /* Extend        | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* RI            | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* Prepend       | */ [0 /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /* | */, 0 /*  |  */, 0 /*  |   */, 0 /*    | */, 0 /* | */, 0 /* | */],
        /* HangulL       | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, X /* | */, 0 /* | */, 0 /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulV       | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulT       | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulLV      | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* HangulLVT     | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* InCBLinker    | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, 0 /*    | */, X /* | */, 0 /* | */],
        /* InCBConsonant | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* ExtPic        | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, X /* | */, 0 /* | */],
        /* ZWJ           | */ [X /* | */, X /* | */, X /* | */, X /* | */, 0 /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /* | */, X /*  |  */, 0 /*  |   */, X /*    | */, 0 /* | */, 0 /* | */],
    ],
];

// The following rules are based on Unicode Standard Annex #14: Line Breaking Properties,
// but heavily modified to allow for use with lookup tables.
//
// TODO: I should go through this and cross check: https://www.unicode.org/Public/draft/ucd/auxiliary/LineBreakTest.html
//
// NOTE: If you convert these rules into a lookup table, you must apply them in reverse order.
//       This is because the rules are ordered from most to least important (e.g. LB8 overrides LB18).
//
// Resolve line breaking classes:
// LB1:   Assign a line breaking class [...].
//        ✗ Unicode does that for us via the "lb" attribute.
//
// Start and end of text:
// LB2:   Never break at the start of text.
//        ~ Functionality not needed.
// LB3:   Always break at the end of text.
//        ~ Functionality not needed.
//
// Mandatory breaks:
// LB4:   Always break after hard line breaks.
//        ~ Handled by our ucd_* functions.
// LB5:   Treat CR followed by LF, as well as CR, LF, and NL as hard line breaks.
//        ~ Handled by our ucd_* functions.
// LB6:   Do not break before hard line breaks.
//        ~ Handled by our ucd_* functions.
//
// Explicit breaks and non-breaks:
// LB7:   Do not break before spaces or zero width space.
//        ✓ × SP
//        ✓ × ZW
// LB8:   Break before any character following a zero-width space, even if one or more spaces intervene.
//        ~ ZW ÷    modified from    ZW SP* ÷    because it's not worth being this anal about accuracy here.
// LB8a:  Do not break after a zero width joiner.
//        ~ Our ucd_* functions never break within grapheme clusters.
//
// Combining marks:
// LB9:   Do not break a combining character sequence; treat it as if it has the line breaking class of the base character in all of the following rules. Treat ZWJ as if it were CM.
//        ~ Our ucd_* functions never break within grapheme clusters.
// LB10:  Treat any remaining combining mark or ZWJ as AL.
//        ✗ To be honest, I'm not entirely sure, I understand the implications of this rule.
//
// Word joiner:
// LB11:  Do not break before or after Word joiner and related characters.
//        ✓ × WJ
//        ✓ WJ ×
//
// Non-breaking characters:
// LB12:  Do not break after NBSP and related characters.
//        ✓ GL ×
// LB12a: Do not break before NBSP and related characters, except after spaces and hyphens.
//        ✓ [^SP BA HY] × GL
//
// Opening and closing:
// LB13:  Do not break before ']' or '!' or '/', even after spaces.
//        ✓ × CL
//        ✓ × CP
//        ✓ × EX
//        ✓ × SY
// LB14:  Do not break after '[', even after spaces.
//        ~ OP ×    modified from    OP SP* ×    just because it's simpler. It would be nice to address this.
// LB15a: Do not break after an unresolved initial punctuation that lies at the start of the line, after a space, after opening punctuation, or after an unresolved quotation mark, even after spaces.
//        ✗ Not implemented. Seemed too complex for little gain?
// LB15b: Do not break before an unresolved final punctuation that lies at the end of the line, before a space, before a prohibited break, or before an unresolved quotation mark, even after spaces.
//        ✗ Not implemented. Seemed too complex for little gain?
// LB15c: Break before a decimal mark that follows a space, for instance, in 'subtract .5'.
//        ~ SP ÷ IS    modified from    SP ÷ IS NU    because this fits neatly with LB15d.
// LB15d: Otherwise, do not break before ';', ',', or '.', even after spaces.
//        ✓ × IS
// LB16:  Do not break between closing punctuation and a nonstarter (lb=NS), even with intervening spaces.
//        ✗ Not implemented. Could be useful in the future, but its usefulness seemed limited to me.
// LB17:  Do not break within '——', even with intervening spaces.
//        ✗ Not implemented. Terminal applications nor code use em-dashes much anyway.
//
// Spaces:
// LB18:  Break after spaces.
//        ✓ SP ÷
//
// Special case rules:
// LB19:  Do not break before non-initial unresolved quotation marks, such as ' ” ' or ' " ', nor after non-final unresolved quotation marks, such as ' “ ' or ' " '.
//        ~ × QU    modified from    × [ QU - \p{Pi} ]
//        ~ QU ×    modified from    [ QU - \p{Pf} ] ×
//        We implement the Unicode 16.0 instead of 16.1 rules, because it's simpler and allows us to use a LUT.
// LB19a: Unless surrounded by East Asian characters, do not break either side of any unresolved quotation marks.
//        ✗ [^$EastAsian] × QU
//        ✗ × QU ( [^$EastAsian] | eot )
//        ✗ QU × [^$EastAsian]
//        ✗ ( sot | [^$EastAsian] ) QU ×
//        Same as LB19.
// LB20:  Break before and after unresolved CB.
//        ✗ We break by default. Unicode inline objects are super irrelevant in a terminal in either case.
// LB20a: Do not break after a word-initial hyphen.
//        ✗ Not implemented. Seemed not worth the hassle as the window will almost always be >1 char wide.
// LB21:  Do not break before hyphen-minus, other hyphens, fixed-width spaces, small kana, and other non-starters, or after acute accents.
//        ✓ × BA
//        ✓ × HY
//        ✓ × NS
//        ✓ BB ×
//        ✗ Added HY ÷ HY, because of the following note in TR14:
//        > If used as hyphen, it acts like U+2010 HYPHEN, which has line break class BA.
// LB21a: Do not break after the hyphen in Hebrew + Hyphen + non-Hebrew.
//        ✗ Not implemented. Perhaps in the future.
// LB21b: Do not break between Solidus and Hebrew letters.
//        ✗ Not implemented. Perhaps in the future.
// LB22:  Do not break before ellipses.
//        ✓ × IN
//
// Numbers:
// LB23:  Do not break between digits and letters.
//        ✓ (AL | HL) × NU
//        ✓ NU × (AL | HL)
// LB23a: Do not break between numeric prefixes and ideographs, or between ideographs and numeric postfixes.
//        ✓ PR × (ID | EB | EM)
//        ✓ (ID | EB | EM) × PO
// LB24:  Do not break between numeric prefix/postfix and letters, or between letters and prefix/postfix.
//        ✓ (PR | PO) × (AL | HL)
//        ✓ (AL | HL) × (PR | PO)
// LB25:  Do not break numbers:
//        ~ CL × PO                  modified from    NU ( SY | IS )* CL × PO
//        ~ CP × PO                  modified from    NU ( SY | IS )* CP × PO
//        ~ CL × PR                  modified from    NU ( SY | IS )* CL × PR
//        ~ CP × PR                  modified from    NU ( SY | IS )* CP × PR
//        ~ ( NU | SY | IS ) × PO    modified from    NU ( SY | IS )* × PO
//        ~ ( NU | SY | IS ) × PR    modified from    NU ( SY | IS )* × PR
//        ~ PO × OP                  modified from    PO × OP NU
//        ~ PO × OP                  modified from    PO × OP IS NU
//        ✓ PO × NU
//        ~ PR × OP                  modified from    PR × OP NU
//        ~ PR × OP                  modified from    PR × OP IS NU
//        ✓ PR × NU
//        ✓ HY × NU
//        ✓ IS × NU
//        ~ ( NU | SY | IS ) × NU    modified from    NU ( SY | IS )* × NU
//        Most were simplified because the cases this additionally allows don't matter much here.
//
// Korean syllable blocks
// LB26:  Do not break a Korean syllable.
//        ✗ Our ucd_* functions never break within grapheme clusters.
// LB27:  Treat a Korean Syllable Block the same as ID.
//        ✗ Our ucd_* functions never break within grapheme clusters.
//
// Finally, join alphabetic letters into words and break everything else.
// LB28:  Do not break between alphabetics ("at").
//        ✓ (AL | HL) × (AL | HL)
// LB28a: Do not break inside the orthographic syllables of Brahmic scripts.
//        ✗ Our ucd_* functions never break within grapheme clusters.
// LB29:  Do not break between numeric punctuation and alphabetics ("e.g.").
//        ✓ IS × (AL | HL)
// LB30:  Do not break between letters, numbers, or ordinary symbols and opening or closing parentheses.
//        ✓ (AL | HL | NU) × [OP-$EastAsian]
//        ✓ [CP-$EastAsian] × (AL | HL | NU)
// LB30a: Break between two regional indicator symbols if and only if there are an even number of regional indicators preceding the position of the break.
//        ✗ Our ucd_* functions never break within grapheme clusters.
// LB30b: Do not break between an emoji base (or potential emoji) and an emoji modifier.
//        ✗ Our ucd_* functions never break within grapheme clusters.
// LB31:  Break everywhere else.
//        ✗ Our default behavior.
#[rustfmt::skip]
pub const JOIN_RULES_LINE_BREAK: [[i32; 24]; 25] = [
    /* ↓ leading                    → trailing codepoint                                                                                                                                                                                                                                                                                                                                                                               */
    /*                           |   Other  | WordJoiner | ZeroWidthSpace |   Glue   |   Space  | BreakAfter | BreakBefore | Hyphen   | ClosePunctuation | CloseParenthesis_EA | CloseParenthesis_NotEA | Exclamation | Inseparable | Nonstarter | OpenPunctuation_EA | OpenPunctuation_NotEA | Quotation | InfixNumericSeparator | Numeric  | PostfixNumeric | PrefixNumeric | SymbolsAllowingBreakAfter | Alphabetic | Ideographic | */
    /* Other                     | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* WordJoiner                | */ [1 /* |  */, 1 /*  |    */, 1 /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, 1 /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* ZeroWidthSpace            | */ [X /* |  */, X /*  |    */, X /*    | */, X /* | */, 1 /* |  */, X /*  |  */, X /*   | */, X /* |    */, X /*      |      */, X /*       |        */, X /*        |  */, X /*   |  */, X /*   |  */, X /*  |      */, X /*      |        */, X /*       | */, X /*  |       */, X /*        | */, X /* |   */, X /*     |   */, X /*    |         */, X /*          | */, X /*   |  */, X /*   | */],
    /* Glue                      | */ [1 /* |  */, 1 /*  |    */, 1 /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, 1 /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* Space                     | */ [X /* |  */, 1 /*  |    */, 1 /*    | */, X /* | */, 1 /* |  */, X /*  |  */, X /*   | */, X /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, X /*   |  */, X /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* BreakAfter                | */ [X /* |  */, 1 /*  |    */, X /*    | */, X /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* BreakBefore               | */ [1 /* |  */, 1 /*  |    */, 1 /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, 1 /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* Hyphen                    | */ [X /* |  */, 1 /*  |    */, X /*    | */, X /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, X /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* ClosePunctuation          | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* CloseParenthesis_EA       | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* CloseParenthesis_NotEA    | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, X /*   | */],
    /* Exclamation               | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* Inseparable               | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* Nonstarter                | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* OpenPunctuation_EA        | */ [1 /* |  */, 1 /*  |    */, 1 /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, 1 /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* OpenPunctuation_NotEA     | */ [1 /* |  */, 1 /*  |    */, 1 /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, 1 /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* Quotation                 | */ [1 /* |  */, 1 /*  |    */, 1 /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, 1 /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* InfixNumericSeparator     | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, X /*   | */],
    /* Numeric                   | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, X /*   | */],
    /* PostfixNumeric            | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, 1 /*   |  */, X /*   | */],
    /* PrefixNumeric             | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, 1 /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, X /*     |   */, X /*    |         */, 1 /*          | */, 1 /*   |  */, 1 /*   | */],
    /* SymbolsAllowingBreakAfter | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* Alphabetic                | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, 1 /*       | */, 1 /*  |       */, 1 /*        | */, 1 /* |   */, 1 /*     |   */, 1 /*    |         */, 1 /*          | */, 1 /*   |  */, X /*   | */],
    /* Ideographic               | */ [X /* |  */, 1 /*  |    */, X /*    | */, 1 /* | */, 1 /* |  */, 1 /*  |  */, X /*   | */, 1 /* |    */, 1 /*      |      */, 1 /*       |        */, 1 /*        |  */, 1 /*   |  */, 1 /*   |  */, 1 /*  |      */, X /*      |        */, X /*       | */, 1 /*  |       */, 1 /*        | */, X /* |   */, 1 /*     |   */, X /*    |         */, 1 /*          | */, X /*   |  */, X /*   | */],
    /* StartOfText               | */ [X /* |  */, X /*  |    */, X /*    | */, X /* | */, X /* |  */, X /*  |  */, X /*   | */, X /* |    */, X /*      |      */, X /*       |        */, X /*        |  */, X /*   |  */, X /*   |  */, X /*  |      */, X /*      |        */, X /*       | */, X /*  |       */, X /*        | */, X /* |   */, X /*     |   */, X /*    |         */, X /*          | */, X /*   |  */, X /*   | */],
];
