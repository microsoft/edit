// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

//! A simple, tiny, approximate (!) float parser.
//! It's a good fit when you're fine with a few ULP of error.
//!
//! It implements the same grammar accepted by `f64::from_str`.

/// Parse an ASCII byte string into an `f64`.
///
/// Accepts the same grammar as `f64::from_str`.
/// The result may differ from `f64::from_str` by a few ULP.
pub fn parse_f64_approx(input: &[u8]) -> Option<f64> {
    if input.is_empty() {
        return None;
    }

    let mut pos = 0;

    // Sign
    let negative = match input[pos] {
        b'+' => {
            pos += 1;
            false
        }
        b'-' => {
            pos += 1;
            true
        }
        _ => false,
    };

    if pos >= input.len() {
        return None;
    }

    // Special values
    let remaining = &input[pos..];
    if remaining.eq_ignore_ascii_case(b"inf") || remaining.eq_ignore_ascii_case(b"infinity") {
        return Some(if negative { f64::NEG_INFINITY } else { f64::INFINITY });
    }
    if remaining.eq_ignore_ascii_case(b"nan") {
        return Some(f64::NAN);
    }

    let mut mantissa: u64 = 0;
    let mut exponent: i32 = 0;
    let mut has_digits = false;

    // Integer part
    while pos < input.len() && input[pos].is_ascii_digit() {
        has_digits = true;
        let d = (input[pos] - b'0') as u64;
        if mantissa < 1_000_000_000_000_000_000 {
            mantissa = mantissa * 10 + d;
        } else {
            exponent += 1;
        }
        pos += 1;
    }

    // Fractional part
    if pos < input.len() && input[pos] == b'.' {
        pos += 1;
        while pos < input.len() && input[pos].is_ascii_digit() {
            has_digits = true;
            let d = (input[pos] - b'0') as u64;
            if mantissa < 1_000_000_000_000_000_000 {
                mantissa = mantissa * 10 + d;
                exponent -= 1;
            }
            pos += 1;
        }
    }

    // Must have had at least one digit
    if !has_digits {
        return None;
    }

    // Explicit exponent
    if pos < input.len() && (input[pos] == b'e' || input[pos] == b'E') {
        pos += 1;

        let exp_negative = match input.get(pos) {
            Some(b'+') => {
                pos += 1;
                false
            }
            Some(b'-') => {
                pos += 1;
                true
            }
            _ => false,
        };

        // Must have at least one exponent digit
        if pos >= input.len() || !input[pos].is_ascii_digit() {
            return None;
        }

        let mut exp_val: i32 = 0;
        while pos < input.len() && input[pos].is_ascii_digit() {
            exp_val = exp_val.saturating_mul(10).saturating_add((input[pos] - b'0') as i32);
            pos += 1;
        }

        if exp_negative {
            exponent = exponent.saturating_sub(exp_val);
        } else {
            exponent = exponent.saturating_add(exp_val);
        }
    }

    // Must have consumed the entire input
    if pos != input.len() {
        return None;
    }

    let mut value = mantissa as f64;

    if exponent != 0 {
        const TABLE: [f64; 8] = [1e-3, 1e-2, 1e-1, 1e0, 1e1, 1e2, 1e3, 1e4];
        value *= match TABLE.get((exponent + 3) as usize) {
            Some(&v) => v,
            None => 10f64.powi(exponent),
        };
    }

    if negative {
        value = -value;
    }

    Some(value)
}

#[cfg(test)]
#[allow(clippy::approx_constant)]
mod tests {
    use super::*;

    /// Helper: parse and unwrap.
    fn p(s: &str) -> f64 {
        parse_f64_approx(s.as_bytes())
            .unwrap_or_else(|| panic!("parse_f64 returned None for {:?}", s))
    }

    /// Helper: assert parse fails.
    fn fail(s: &str) {
        assert!(parse_f64_approx(s.as_bytes()).is_none(), "expected None for {:?}", s);
    }

    /// Helper: assert result is within 1 ULP of expected.
    fn approx(s: &str, expected: f64) {
        let got = p(s);
        let diff = (got.to_bits() as i64).wrapping_sub(expected.to_bits() as i64).unsigned_abs();
        assert!(diff <= 1, "more than 1 ULP off for {:?}: got={}, expected={}", s, got, expected);
    }

    // ---- Integers ----
    #[test]
    fn integers() {
        assert_eq!(p("0"), 0.0);
        assert_eq!(p("1"), 1.0);
        assert_eq!(p("123"), 123.0);
        assert_eq!(p("007"), 7.0);
        assert_eq!(p("-456"), -456.0);
        assert_eq!(p("+42"), 42.0);
    }

    // ---- Decimals ----
    #[test]
    fn decimals() {
        assert_eq!(p("3.14"), 3.14);
        assert_eq!(p("0.5"), 0.5);
        assert_eq!(p(".5"), 0.5);
        assert_eq!(p("5."), 5.0);
        assert_eq!(p("-3.14"), -3.14);
        assert_eq!(p("0.0"), 0.0);
    }

    // ---- Scientific notation ----
    #[test]
    fn scientific() {
        assert_eq!(p("1e3"), 1e3);
        assert_eq!(p("2.5E10"), 2.5e10);
        approx("2.5e-10", 2.5e-10);
        approx("1.5e-3", 0.0015);
        assert_eq!(p("1e0"), 1.0);
        assert_eq!(p("1e+2"), 100.0);
    }

    // ---- Special values ----
    #[test]
    fn special_values() {
        assert_eq!(p("inf"), f64::INFINITY);
        assert_eq!(p("-inf"), f64::NEG_INFINITY);
        assert_eq!(p("+infinity"), f64::INFINITY);
        assert_eq!(p("Inf"), f64::INFINITY);
        assert_eq!(p("INFINITY"), f64::INFINITY);
        assert!(p("NaN").is_nan());
        assert!(p("nan").is_nan());
        assert!(p("NAN").is_nan());
    }

    // ---- Edge: many digits ----
    #[test]
    fn many_digits() {
        // 19+ digit integer — truncation kicks in but result is close
        let v = p("12345678901234567890");
        assert!((v - 12345678901234567890.0f64).abs() / v < 1e-15);
    }

    // ---- Errors ----
    #[test]
    fn errors() {
        fail("");
        fail("+");
        fail("-");
        fail(".");
        fail("e5");
        fail("1e");
        fail("1e+");
        fail("1e-");
        fail("abc");
        fail(" 1");
        fail("1 ");
        fail("1.2.3");
        fail("--1");
        fail("1e2e3");
    }

    // ---- Cross-check against stdlib for common config values ----
    #[test]
    fn cross_check_stdlib() {
        let cases = [
            "0", "1", "-1", "0.5", "123.456", "1e10", "1e-10", "3.14", "2.5E10", "2.5e-10",
            "0.0015", "1000000", "99.99", "0.001", "1e22", "-0.0", "0.1", "0.2", "0.3",
        ];
        for s in cases {
            approx(s, s.parse().unwrap());
        }
    }
}
