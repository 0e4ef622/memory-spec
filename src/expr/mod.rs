use std::{borrow::Cow, collections::HashMap, iter::Peekable};

use crate::expr::tokenize::{Operator as OperatorToken, Token, TokenizeError, Tokenizer};

mod checked_int;
pub mod tokenize;


pub fn eval(expr: &str, vars: &Namespace<'_>) -> Result<i64, EvalError> {
    let r = _eval(&mut Tokenizer::new(expr).peekable(), &vars, 0)?;
    let r = resolve(r, &vars)?;
    match &*r {
        Value::N(n) => Ok(*n),
        _ => Err(EvalError::TypeMismatch),
    }
}

fn _eval<'a, 'b>(tokens: &mut Peekable<Tokenizer<'a>>, vars: &'b Namespace<'a>, min_bp: u8) -> Result<Cow<'b, Value<'a>>, EvalError> {
    let first_token = tokens.next().ok_or(EvalError::Eof)??;

    let mut lhs;
    if first_token == Token::LParen {
        lhs = _eval(tokens, vars, 0)?;
        let rparen = tokens.next().ok_or(EvalError::Eof)??;
        if rparen != Token::RParen {
            return Err(EvalError::ParenMismatch);
        }
    } else if first_token == Token::RParen {
        return Err(EvalError::ParenMismatch);
    } else if let Ok(op) = PrefixOperator::from_token(first_token.clone()) {
        lhs = _eval(tokens, vars, op.binding_power())?;
        lhs = op.apply(lhs, vars)?;
    } else {
        lhs = Cow::Owned(Value::from_token(first_token)?);
    }

    loop {
        let Some(t) = tokens.peek() else { return Ok(lhs) };
        let op_token = t.as_ref().map_err(|e| *e)?;
        let infix_op;
        if let Ok(op) = PostfixOperator::from_token(op_token.clone()) {
            if op.binding_power() < min_bp {
                return Ok(lhs);
            } else {
                lhs = op.apply(lhs, vars)?;
                tokens.next();
                continue;
            }
        } else if let Ok(op) = Operator::from_token(op_token.clone()) {
            if op.binding_power().0 < min_bp {
                return Ok(lhs);
            } else {
                infix_op = op;
                tokens.next();
            }
        } else if *op_token == Token::RParen {
            return Ok(lhs);
        } else {
            return Err(EvalError::UnexpectedInput);
        }

        let rhs = _eval(tokens, vars, infix_op.binding_power().1)?;
        lhs = infix_op.apply(lhs, &rhs, vars)?;
    }
}

pub type Namespace<'a> = HashMap<String, Value<'a>>;
#[derive(Clone, Debug)]
pub enum Value<'a> {
    N(i64),
    Namespace(Namespace<'a>),
    Unresolved(&'a str),
}

impl<'a> Value<'a> {
    fn from_token(t: Token<'a>) -> Result<Self, EvalError> {
        match t {
            Token::NumericLiteral(n) => Ok(Value::N(n)),
            Token::Ident(i) => Ok(Value::Unresolved(i)),
            _ => Err(EvalError::UnexpectedInput),
        }
    }

    pub fn namespace_mut(&mut self) -> Option<&mut Namespace<'a>> {
        match self {
            Self::Namespace(ns) => Some(ns),
            _ => None,
        }
    }
}

fn resolve<'a, 'b>(value: Cow<'b, Value<'a>>, vars: &'b Namespace<'a>) -> Result<Cow<'b, Value<'a>>, EvalError> {
    if let Value::Unresolved(s) = &*value {
        Ok(Cow::Borrowed(vars.get(*s).ok_or(EvalError::UnknownName)?))
    } else {
        Ok(value)
    }
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum EvalError {
    TokenizeError(TokenizeError),
    UnexpectedInput,
    Eof,
    TypeMismatch,
    UnknownName,
    Overflow,
    ParenMismatch,
}

impl std::fmt::Display for EvalError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            EvalError::TokenizeError(e) => e.fmt(f),
            EvalError::UnexpectedInput => f.write_str("syntax error"),
            EvalError::Eof => f.write_str("unexpected eof"),
            EvalError::TypeMismatch => f.write_str("type mismatch"),
            EvalError::UnknownName => f.write_str("unknown name"),
            EvalError::Overflow => f.write_str("overflow"),
            EvalError::ParenMismatch => f.write_str("mismatched parentheses"),
        }
    }
}

impl std::error::Error for EvalError {}

impl From<TokenizeError> for EvalError {
    fn from(t: TokenizeError) -> Self {
        EvalError::TokenizeError(t)
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum Operator {
    Add,
    Subtract,
    Multiply,
    Divide,
    Dot,
}

impl Operator {
    fn from_token(t: Token<'_>) -> Result<Self, EvalError> {
        match t {
            Token::Operator(OperatorToken::Add) => Ok(Self::Add),
            Token::Operator(OperatorToken::Subtract) => Ok(Self::Subtract),
            Token::Operator(OperatorToken::Multiply) => Ok(Self::Multiply),
            Token::Operator(OperatorToken::Divide) => Ok(Self::Divide),
            Token::Operator(OperatorToken::Dot) => Ok(Self::Dot),
            _ => Err(EvalError::UnexpectedInput),
        }
    }

    fn binding_power(self) -> (u8, u8) {
        match self {
            Self::Add => (1, 2),
            Self::Subtract => (1, 2),
            Self::Multiply => (3, 4),
            Self::Divide => (3, 4),
            Self::Dot => (7, 8),
        }
    }

    fn apply<'a, 'b>(self, lhs: Cow<'b, Value<'a>>, rhs: &Value<'a>, vars: &'b Namespace<'a>) -> Result<Cow<'b, Value<'a>>, EvalError> {
        let mut lhs = resolve(lhs, &vars)?;
        let rhs = if self != Operator::Dot {
            &*resolve(Cow::Borrowed(rhs), &vars)?
        } else { rhs };
        match (self, lhs, rhs) {
            (Operator::Dot, Cow::Borrowed(Value::Namespace(ns)), Value::Unresolved(key)) => {
                return Ok(Cow::Borrowed(ns.get(*key).ok_or(EvalError::UnknownName)?));
            }
            (_, l, _) => lhs = l, // this is so dumb lol
        }
        match (self, &*lhs, rhs) {
            (Operator::Add, Value::N(a), Value::N(b)) => Ok(Cow::Owned(Value::N(a.checked_add(*b).ok_or(EvalError::Overflow)?))),
            (Operator::Subtract, Value::N(a), Value::N(b)) => Ok(Cow::Owned(Value::N(a.checked_sub(*b).ok_or(EvalError::Overflow)?))),
            (Operator::Multiply, Value::N(a), Value::N(b)) => Ok(Cow::Owned(Value::N(a.checked_mul(*b).ok_or(EvalError::Overflow)?))),
            (Operator::Divide, Value::N(a), Value::N(b)) => Ok(Cow::Owned(Value::N(a.checked_div(*b).ok_or(EvalError::Overflow)?))),
            _ => Err(EvalError::TypeMismatch),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum PrefixOperator {
    Negate,
}

impl PrefixOperator {
    fn from_token(t: Token<'_>) -> Result<Self, EvalError> {
        match t {
            Token::Operator(OperatorToken::Subtract) => Ok(Self::Negate),
            _ => Err(EvalError::UnexpectedInput),
        }
    }

    fn binding_power(self) -> u8 {
        match self {
            Self::Negate => 5,
        }
    }
    fn apply<'a, 'b>(self, value: Cow<'b, Value<'a>>, vars: &'b Namespace<'a>) -> Result<Cow<'b, Value<'a>>, EvalError> {
        let value = resolve(value, vars)?;
        match (self, &*value) {
            (Self::Negate, Value::N(n)) => Ok(Cow::Owned(Value::N(n.checked_neg().ok_or(EvalError::Overflow)?))),
            _ => Err(EvalError::TypeMismatch),
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq)]
enum PostfixOperator {
    K,
    M,
}

impl PostfixOperator {
    fn from_token(t: Token<'_>) -> Result<Self, EvalError> {
        match t {
            Token::Ident("K") => Ok(Self::K),
            Token::Ident("M") => Ok(Self::M),
            _ => Err(EvalError::UnexpectedInput),
        }
    }
    fn binding_power(self) -> u8 {
        match self {
            Self::K => 5,
            Self::M => 5,
        }
    }
    fn apply<'a, 'b>(self, value: Cow<'b, Value<'a>>, vars: &'b Namespace<'a>) -> Result<Cow<'b, Value<'a>>, EvalError> {
        let value = resolve(value, vars)?;
        match (self, &*value) {
            (Self::K, Value::N(n)) => Ok(Cow::Owned(Value::N(n.checked_mul(1024).ok_or(EvalError::Overflow)?))),
            (Self::M, Value::N(n)) => Ok(Cow::Owned(Value::N(n.checked_mul(1024*1024).ok_or(EvalError::Overflow)?))),
            _ => Err(EvalError::TypeMismatch),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{eval, Namespace, Value};

    macro_rules! namespace {
        ($($key:literal: $value:tt),* $(,)?) => {{
            let mut ns = Namespace::default();
            $(ns.insert(($key).into(), namespace!(@nest $value));)*
            ns
        }};

        (@nest { $($inner:tt)* }) => {
            Value::Namespace(namespace!($($inner)*))
        };
        (@nest $n:literal) => {
            Value::N($n)
        }
    }

    #[test]
    fn infix_test() {
        let cases = [
            ("1 + 1", 2),
            ("72 + 7", 79),
            ("72 - 7", 65),
            ("72 * 7", 504),
            ("72 / 7", 10),
            ("3 + 2 * 5", 13),
        ];
        for (input, expected) in cases {
            let result = eval(input, &Namespace::default());
            assert_eq!(result, Ok(expected), "{input:?}");
        }
    }

    #[test]
    fn prefix_test() {
        let cases = [
            ("-1", -1),
            ("-1 + 1", 0),
            ("-(1 + 1)", -2),
        ];
        for (input, expected) in cases {
            let result = eval(input, &Namespace::default());
            assert_eq!(result, Ok(expected), "{input:?}");
        }
    }

    #[test]
    fn postfix_test() {
        let cases = [
            ("2K", 2048),
            ("2M", 2*1024*1024),
            ("3 + 1K", 1027),
            ("(3 + 1)K", 4096),
        ];
        for (input, expected) in cases {
            let result = eval(input, &Namespace::default());
            assert_eq!(result, Ok(expected), "{input:?}");
        }
    }

    #[test]
    fn vars_test() {
        let vars = namespace! {
            "one": 1,
            "two": 2,
            "rest": {
                "three": 3,
                "more": {
                    "four": 4,
                },
            }
        };

        let cases = [
            ("one", 1),
            ("rest.three", 3),
            ("rest.more.four", 4),
            ("rest.(three)", 3), // lol
            ("one + two * rest.three K", 1 + 2*3*1024),
        ];
        for (input, expected) in cases {
            let result = eval(input, &vars);
            assert_eq!(result, Ok(expected), "{input:?}");
        }
    }
}
