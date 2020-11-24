use super::{Expr, Hand, Op, Val};
use serde_derive::Serialize;
use std::convert::TryFrom;
use std::str::FromStr;

#[derive(Debug, Eq, PartialEq, Serialize)]
#[serde(tag = "error", rename_all = "snake_case")]
pub enum ParseError {
    UnexpectedToken { index: usize, token: char },
    BadDie { index: usize },
    IllegalExpression { index: usize },
    UnmatchedParen { index: usize },
    EmptyExpression { index: usize },
}

#[derive(Debug, Eq, PartialEq)]
pub(super) struct Tokens(Vec<IndexedToken>);

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
struct IndexedToken {
    index: usize,
    token: Token,
}

impl IndexedToken {
    fn new(index: usize, token: Token) -> Self {
        IndexedToken { index, token }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Token {
    Num(u32),
    Die(u32),
    Add,
    Sub,
    Mul,
    OpenParen,
    CloseParen,
}

impl FromStr for Tokens {
    type Err = ParseError;

    fn from_str(expr: &str) -> Result<Self, Self::Err> {
        use Token::*;
        let mut tokens = Vec::new();
        let mut chars = expr.chars().enumerate().peekable();
        while let Some((idx, c)) = chars.next() {
            let token = match c {
                // Digits
                '0'..='9' => {
                    let mut num = c as u32 - '0' as u32;
                    while let Some((_, c @ '0'..='9')) = chars.peek() {
                        // Advance the iterator
                        let (_, c) = chars.next().unwrap();
                        num = num * 10 + c as u32 - '0' as u32
                    }
                    IndexedToken::new(idx, Num(num))
                }
                // Die
                'd' => {
                    let mut num = 0;
                    while let Some((_, c @ '0'..='9')) = chars.peek() {
                        // Advance the iterator
                        let (_, c) = chars.next().unwrap();
                        num = num * 10 + c as u32 - '0' as u32
                    }
                    if num > 0 {
                        IndexedToken::new(idx, Token::Die(num))
                    } else {
                        Err(ParseError::BadDie { index: idx })?
                    }
                }
                // Skip whitespace
                ' ' | '\t' | '\n' => continue,
                '+' => IndexedToken::new(idx, Add),
                '-' => IndexedToken::new(idx, Sub),
                '*' => IndexedToken::new(idx, Mul),
                '(' | '[' | '{' => IndexedToken::new(idx, Token::OpenParen),
                ')' | ']' | '}' => IndexedToken::new(idx, Token::CloseParen),
                token => Err(ParseError::UnexpectedToken {
                    index: idx,
                    token: c,
                })?,
            };
            tokens.push(token);
        }
        Ok(Tokens(tokens))
    }
}

impl Tokens {
    pub(super) fn normalize(self) -> Result<Normalized, ParseError> {
        use Token::*;
        // Check matching parens
        {
            let parens = self.0.iter().try_fold(Vec::new(), |mut acc, it| {
                match it.token {
                    OpenParen => acc.push(it),
                    CloseParen => {
                        acc.pop()
                            .ok_or_else(|| ParseError::UnmatchedParen { index: it.index })?;
                    }
                    _ => {}
                }
                Ok(acc)
            })?;
            if let Some(it) = parens.last() {
                Err(ParseError::UnmatchedParen { index: it.index })?
            };
        }

        let mut normalized = Vec::new();
        let mut tokens = self.0.into_iter();
        let mut left = tokens
            .next()
            .ok_or_else(|| ParseError::EmptyExpression { index: 0 })?;

        // Normalize first token
        // normalize_pair will not push left token, ok to use as a marker of new expression
        normalize_pair(&mut normalized, OpenParen, left)?;
        while let Some(right) = tokens.next() {
            normalize_pair(&mut normalized, left.token, right)?;
            left = right;
        }
        // Normalize last token
        match left.token {
            Num(_) | Die(_) | CloseParen => {} // Already pushed
            // Deny trailing operands
            Add | Sub | Mul | OpenParen => {
                Err(ParseError::IllegalExpression { index: left.index })?
            }
        }
        Ok(Normalized(normalized))
    }
}

fn normalize_pair(
    res: &mut Vec<Token>,
    left: Token,
    right: IndexedToken,
) -> Result<(), ParseError> {
    use Token::*;

    // Assume left already pushed
    // Assume all parens balanced
    match left {
        // Expression
        Num(_) | Die(_) | CloseParen => match right.token {
            Num(_) | Die(_) | OpenParen => {
                res.push(Mul);
                res.push(right.token);
            }
            Add | Sub | Mul | CloseParen => res.push(right.token),
        },
        // Operands
        Add | Sub | Mul => match right.token {
            Num(_) | Die(_) | OpenParen => {
                res.push(right.token);
            }
            Add | Sub | Mul | CloseParen => {
                Err(ParseError::IllegalExpression { index: right.index })?
            }
        },
        // Expression start
        OpenParen => match right.token {
            Num(_) | Die(_) | OpenParen => res.push(right.token),
            // Prepend '+' or '-' with Num(0) token
            Add | Sub => {
                res.push(Num(0));
                res.push(right.token)
            }
            Mul | CloseParen => Err(ParseError::IllegalExpression { index: right.index })?,
        },
    }
    Ok(())
}

pub(super) struct Normalized(Vec<Token>);

impl Normalized {
    pub(super) fn to_expr(self) -> Expr {
        Normalized::parse_recursive(ParsedExpr::None, self.0.as_slice()).expr
    }

    fn parse_recursive(e: ParsedExpr, tokens: &[Token]) -> Parsed {
        let prio = |op: Op| match op {
            Op::Add => 1,
            Op::Sub => 1,
            Op::Mul => 2,
        };
        let get_op = |t: Token| match t {
            Token::Add => Op::Add,
            Token::Sub => Op::Sub,
            Token::Mul => Op::Mul,
            _ => unreachable!(),
        };
        let get_val = |tokens: &[Token]| match tokens[0] {
            Token::Die(n) => Parsed::new(1, Expr::Value(Val::Die(n))),
            Token::Num(n) => Parsed::new(1, Expr::Value(Val::Num(n))),
            Token::OpenParen => {
                Normalized::parse_recursive(ParsedExpr::None, &tokens[1..]).shift(1)
            }
            _ => unreachable!(),
        };
        let next_op = |tokens: &[Token]| {
            let mut depth = 0;
            tokens.iter().find_map(|t| match t {
                Token::Add if depth == 0 => Some(Op::Add),
                Token::Sub if depth == 0 => Some(Op::Sub),
                Token::Mul if depth == 0 => Some(Op::Mul),
                Token::OpenParen => {
                    depth += 1;
                    None
                }
                Token::CloseParen => {
                    depth -= 1;
                    None
                }
                _ => None,
            })
        };

        match e {
            ParsedExpr::None => {
                if let Some(token) = tokens.get(0) {
                    let p = get_val(tokens);
                    Normalized::parse_recursive(ParsedExpr::Expr(p.expr), &tokens[p.shift..])
                        .shift(p.shift)
                } else {
                    unreachable!()
                }
            }
            ParsedExpr::Expr(e) => {
                let token = tokens.get(0);
                if token == None || token == Some(&Token::CloseParen) {
                    Parsed::new(1, e)
                } else {
                    let op = get_op(tokens[0]);
                    Normalized::parse_recursive(ParsedExpr::Half { op, left: e }, &tokens[1..])
                        .shift(1)
                }
            }
            ParsedExpr::Half { op, left } => {
                if next_op(tokens).map(|next_op| prio(op) < prio(next_op)) == Some(true) {
                    let p = Normalized::parse_recursive(ParsedExpr::None, &tokens);
                    Parsed::new(
                        p.shift,
                        Expr::Expr {
                            op,
                            left: Box::new(left),
                            right: Box::new(p.expr),
                        },
                    )
                } else {
                    let p = get_val(tokens);
                    let expr = Expr::Expr {
                        op,
                        left: Box::new(left),
                        right: Box::new(p.expr),
                    };
                    Normalized::parse_recursive(ParsedExpr::Expr(expr), &tokens[p.shift..])
                        .shift(p.shift)
                }
            }
        }
    }
}

enum ParsedExpr {
    None,
    Expr(Expr),
    Half { op: Op, left: Expr },
}

struct Parsed {
    shift: usize,
    expr: Expr,
}

impl Parsed {
    fn new(shift: usize, expr: Expr) -> Self {
        Parsed { shift, expr }
    }
    fn shift(self, s: usize) -> Self {
        Self {
            shift: self.shift + s,
            ..self
        }
    }
}

#[cfg(all(test, target_arch = "wasm32"))]
mod test {
    use wasm_bindgen_test::*;

    use super::*;

    #[wasm_bindgen_test]
    fn tokenize_expr_ok() {
        use Token::*;

        let expr = "d20 - 3 + 2(d6 * 2)";
        let tokens = Tokens::from_str(expr).expect("Unable to tokenize valid expr");
        assert_eq!(
            tokens.0,
            vec![
                IndexedToken::new(0, Die(20)),
                IndexedToken::new(4, Sub),
                IndexedToken::new(6, Num(3)),
                IndexedToken::new(8, Add),
                IndexedToken::new(10, Num(2)),
                IndexedToken::new(11, OpenParen),
                IndexedToken::new(12, Die(6)),
                IndexedToken::new(15, Mul),
                IndexedToken::new(17, Num(2)),
                IndexedToken::new(18, CloseParen),
            ]
        );
    }

    #[wasm_bindgen_test]
    fn tokenize_expr_unexpected_token() {
        let expr = "d20 * 200%";
        let tokens = Tokens::from_str(expr);
        assert_eq!(
            tokens,
            Err(ParseError::UnexpectedToken {
                index: 9,
                token: '%'
            })
        )
    }

    #[wasm_bindgen_test]
    fn tokenize_expr_bad_die() {
        let expr = "d20 + d0 * 3";
        let tokens = Tokens::from_str(expr);
        assert_eq!(tokens, Err(ParseError::BadDie { index: 6 }))
    }
}
