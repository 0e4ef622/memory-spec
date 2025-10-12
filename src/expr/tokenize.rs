use super::checked_int::Checked;
use unicode_ident::{is_xid_continue, is_xid_start};

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token<'a> {
    NumericLiteral(i64),
    Ident(&'a str),
    Operator(Operator),
    LParen,
    RParen,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Dot,
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum TokenizeError {
    Eof,
    InvalidToken,
    Overflow,
}

impl std::fmt::Display for TokenizeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Eof => f.write_str("unexpected eof"),
            Self::InvalidToken => f.write_str("invalid token"),
            Self::Overflow => f.write_str("overflow evaluating literal"),
        }
    }
}

impl std::error::Error for TokenizeError {}

pub struct Tokenizer<'a> {
    errored: bool,
    input: &'a str,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self { errored: false, input }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<Token<'a>, TokenizeError>;
    fn next(&mut self) -> Option<Self::Item> {
        if self.errored {
            return None;
        }
        match tokenize(self.input) {
            Ok((token, rest)) => {
                self.input = rest;
                Some(Ok(token))
            }
            Err(TokenizeError::Eof) => None,
            Err(e) => {
                self.errored = true;
                Some(Err(e))
            }
        }
    }
}

fn tokenize(mut s: &str) -> Result<(Token<'_>, &str), TokenizeError> {
    s = s.trim_start();

    match s.as_bytes().get(0).ok_or(TokenizeError::Eof)? {
        b'+' => Ok((Token::Operator(Operator::Add), &s[1..])),
        b'*' => Ok((Token::Operator(Operator::Multiply), &s[1..])),
        b'/' => Ok((Token::Operator(Operator::Divide), &s[1..])),
        b'-' => Ok((Token::Operator(Operator::Subtract), &s[1..])),
        b'.' => Ok((Token::Operator(Operator::Dot), &s[1..])),
        b'(' => Ok((Token::LParen, &s[1..])),
        b')' => Ok((Token::RParen, &s[1..])),
        b'0'..=b'9' => return tokenize_numeric_literal(s),
        _ => return tokenize_ident(s),
    }
}

// returns None on overflow
fn tokenize_numeric_literal(s: &str) -> Result<(Token<'_>, &str), TokenizeError> {
    if s.starts_with("0x") {
        let s = &s[2..];
        let mut r = Checked(Some(0i64));
        for (i, c) in s.bytes().enumerate() {
            match c {
                b'0'..=b'9' => r = r * 16 + i64::from(c - b'0'),
                b'a'..=b'f' => r = r * 16 + i64::from(c - b'a') + 10,
                b'A'..=b'F' => r = r * 16 + i64::from(c - b'A') + 10,
                b'_' => (),
                _ => {
                    let r = r.0.ok_or(TokenizeError::Overflow)?;
                    return Ok((Token::NumericLiteral(r), &s[i..]));
                }
            }
        }
        let r = r.0.ok_or(TokenizeError::Overflow)?;
        return Ok((Token::NumericLiteral(r), ""));
    } else if s.starts_with("0o") {
        let s = &s[2..];
        let mut r = Checked(Some(0i64));
        for (i, c) in s.bytes().enumerate() {
            match c {
                b'0'..=b'7' => r = r * 8 + i64::from(c - b'0'),
                b'_' => (),
                _ => {
                    let r = r.0.ok_or(TokenizeError::Overflow)?;
                    return Ok((Token::NumericLiteral(r), &s[i..]));
                }
            }
        }
        let r = r.0.ok_or(TokenizeError::Overflow)?;
        return Ok((Token::NumericLiteral(r), ""));
    } else if s.starts_with("0b") {
        let s = &s[2..];
        let mut r = Checked(Some(0i64));
        for (i, c) in s.bytes().enumerate() {
            match c {
                b'0'..=b'1' => r = r * 2 + i64::from(c - b'0'),
                b'_' => (),
                _ => {
                    let r = r.0.ok_or(TokenizeError::Overflow)?;
                    return Ok((Token::NumericLiteral(r), &s[i..]));
                }
            }
        }
        let r = r.0.ok_or(TokenizeError::Overflow)?;
        return Ok((Token::NumericLiteral(r), ""));
    } else {
        let mut r = Checked(Some(0i64));
        for (i, c) in s.bytes().enumerate() {
            match c {
                b'0'..=b'9' => r = r * 10 + i64::from(c - b'0'),
                b'_' => (),
                _ => {
                    let r = r.0.ok_or(TokenizeError::Overflow)?;
                    return Ok((Token::NumericLiteral(r), &s[i..]));
                }
            }
        }
        let r = r.0.ok_or(TokenizeError::Overflow)?;
        return Ok((Token::NumericLiteral(r), ""));
    }
}

pub fn tokenize_ident(s: &str) -> Result<(Token<'_>, &str), TokenizeError> {
    let mut first = true;
    for (i, ch) in s.char_indices() {
        if first && !is_xid_start(ch) && ch != '_' {
            return Err(TokenizeError::InvalidToken);
        } else {
            if !is_xid_continue(ch) {
                return Ok((Token::Ident(&s[..i]), &s[i..]));
            }
        }
        first = false;
    }
    Ok((Token::Ident(s), ""))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn numeric_literal_success() {
        let cases = [
            ("0", 0),
            ("0x0", 0),
            ("0o0", 0),
            ("0b0", 0),
            ("1", 1),
            ("0x1", 1),
            ("0o1", 1),
            ("0b1", 1),
            ("0xf", 15),
            ("0xF", 15),
            ("9223372036854775807", 9223372036854775807),
            ("0x7fff_ffff_ffff_ffff", 9223372036854775807),
            ("0o777777777777777777777", 9223372036854775807),
            (
                "0b01111111_11111111_11111111_11111111_11111111_11111111_11111111_11111111",
                9223372036854775807,
            ),
            ("1_______3__", 13),
        ];
        for (input, expected) in cases {
            assert_eq!(
                tokenize(input),
                Ok((Token::NumericLiteral(expected), "")),
                "input = {input:?}, expected = {expected}",
            );
        }
    }

    #[test]
    fn numeric_literal_overflow() {
        let cases = [
            "9223372036854775808",
            "0x8000_0000_0000_0000",
            "0o1000000000000000000000",
            "0b10000000_00000000_00000000_00000000_00000000_00000000_00000000_00000000",
        ];
        for input in cases {
            assert_eq!(
                tokenize(input),
                Err(TokenizeError::Overflow),
                "input = {input}"
            );
        }
    }

    #[test]
    fn eof() {
        assert_eq!(tokenize(""), Err(TokenizeError::Eof));
        assert_eq!(tokenize(" "), Err(TokenizeError::Eof));
        assert_eq!(tokenize("        "), Err(TokenizeError::Eof));
        assert_eq!(tokenize("\n\t\r\n\t\r"), Err(TokenizeError::Eof));
    }

    #[test]
    fn ident_success() {
        let cases = ["test", "_", "テスト", "hunter2"];
        for case in cases {
            assert_eq!(tokenize(case), Ok((Token::Ident(case), "")));
        }
    }

    #[test]
    fn ident_invalid() {
        let cases = ["$var"];
        for case in cases {
            assert_eq!(tokenize(case), Err(TokenizeError::InvalidToken));
        }
    }

    #[test]
    fn token_iter() {
        #[rustfmt::skip]
        let cases = [
            ("1 + 1", vec![Token::NumericLiteral(1), Token::Operator(Operator::Add), Token::NumericLiteral(1)]),
            ("a + b", vec![Token::Ident("a"), Token::Operator(Operator::Add), Token::Ident("b")]),
            ("256K", vec![Token::NumericLiteral(256), Token::Ident("K")]),
            ("x.y.z - 3 * (4 - 1)", vec![
                Token::Ident("x"),
                Token::Operator(Operator::Dot),
                Token::Ident("y"),
                Token::Operator(Operator::Dot),
                Token::Ident("z"),
                Token::Operator(Operator::Subtract),
                Token::NumericLiteral(3),
                Token::Operator(Operator::Multiply),
                Token::LParen,
                Token::NumericLiteral(4),
                Token::Operator(Operator::Subtract),
                Token::NumericLiteral(1),
                Token::RParen,
            ]),
        ];
        for (input, expected) in cases {
            let expected = expected.iter().cloned().map(Ok).collect::<Vec<_>>();
            let tokens = Tokenizer::new(input).collect::<Vec<_>>();
            assert_eq!(tokens, expected, "input = {input:?}, expected = {expected:?}");
        }
    }
}
