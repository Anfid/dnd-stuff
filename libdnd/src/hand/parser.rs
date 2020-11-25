use super::{Die, Expr, Op, Val};
use serde_derive::Serialize;
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
    fn begin(index: usize) -> Self {
        IndexedToken {
            index,
            token: Token::Begin,
        }
    }

    fn end(index: usize) -> Self {
        IndexedToken {
            index,
            token: Token::End,
        }
    }

    fn operation(index: usize, op: Op) -> Self {
        IndexedToken {
            index,
            token: Token::Op(op),
        }
    }

    fn value(index: usize, val: Val) -> Self {
        IndexedToken {
            index,
            token: Token::Val(val),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
enum Token {
    Begin,
    End,
    Op(Op),
    Val(Val),
}

impl FromStr for Tokens {
    type Err = ParseError;

    fn from_str(expr: &str) -> Result<Self, Self::Err> {
        let mut tokens = Vec::new();
        let mut chars = expr.chars().enumerate().peekable();
        tokens.push(IndexedToken::begin(0));
        while let Some((index, c)) = chars.next() {
            let token = match c {
                // Digits
                '0'..='9' => {
                    let mut num = c as u32 - '0' as u32;
                    while let Some((_, '0'..='9')) = chars.peek() {
                        // Advance the iterator
                        let (_, c) = chars.next().unwrap();
                        num = num * 10 + c as u32 - '0' as u32
                    }
                    IndexedToken::value(index, Val::Num(num))
                }
                // Die
                'd' => {
                    let mut num = 0;
                    while let Some((_, '0'..='9')) = chars.peek() {
                        // Advance the iterator
                        let (_, c) = chars.next().unwrap();
                        num = num * 10 + c as u32 - '0' as u32
                    }
                    if num > 0 {
                        IndexedToken::value(index, Val::Die(Die::new(num)))
                    } else {
                        Err(ParseError::BadDie { index })?
                    }
                }
                // Skip whitespace
                ' ' | '\t' | '\n' => continue,
                '+' => IndexedToken::operation(index, Op::Add),
                '-' => IndexedToken::operation(index, Op::Sub),
                '*' => IndexedToken::operation(index, Op::Mul),
                '(' | '[' | '{' => IndexedToken::begin(index),
                ')' | ']' | '}' => IndexedToken::end(index),
                token => Err(ParseError::UnexpectedToken { index, token })?,
            };
            tokens.push(token);
        }
        tokens.push(IndexedToken::end(
            tokens.last().map(|t| t.index).unwrap_or(0),
        ));
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
                    Begin => acc.push(it),
                    End => {
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

        Tokens::normalize_recursive(&self.0).map(|(res, _)| res)
    }

    fn normalize_recursive(
        tokens: &[IndexedToken],
    ) -> Result<(Normalized, &[IndexedToken]), ParseError> {
        use super::Op::*;
        use super::Val::*;
        use Token::*;

        let mut left = tokens[0];
        let mut tokens = &tokens[1..];
        let mut normalized = Vec::new();

        loop {
            // Guranteed to have at least 2 elements: Begin and End
            let right = tokens[0];
            match (left.token, right.token) {
                // Expression start
                (Begin, Begin) | (Val(_), Begin) | (Op(_), Begin) => {
                    if let Val(_) = left.token {
                        normalized.push(NormToken::Op(Mul))
                    }
                    let (expr, remaining) = Tokens::normalize_recursive(tokens)?;
                    normalized.push(NormToken::Expr(expr));
                    tokens = remaining;
                    // Treat expression on next iteration as regular value
                    left = IndexedToken::value(remaining[0].index, Num(0));
                    continue;
                }
                (Begin, End) => Err(ParseError::EmptyExpression { index: left.index })?,
                (Begin, Val(v)) => normalized.push(NormToken::Val(v)),
                (Begin, Op(r @ Sub)) | (Begin, Op(r @ Add)) => {
                    normalized.push(NormToken::Val(Num(0)));
                    normalized.push(NormToken::Op(r))
                }
                (Begin, Op(Mul)) | (Op(_), Op(_)) => {
                    Err(ParseError::IllegalExpression { index: right.index })?
                }
                // Values
                (Val(_), Val(v)) => {
                    normalized.push(NormToken::Op(Mul));
                    normalized.push(NormToken::Val(v));
                }
                (Val(_), Op(o)) => normalized.push(NormToken::Op(o)),
                (Val(_), End) => return Ok((Normalized(normalized), &tokens[1..])),
                // Operators
                (Op(_), Val(v)) => normalized.push(NormToken::Val(v)),
                (Op(_), End) => Err(ParseError::IllegalExpression { index: left.index })?,
                // left can't be End
                (End, _) => unreachable!(),
            }
            left = right;
            tokens = &tokens[1..];
        }
    }
}

#[derive(Debug, Clone)]
pub(super) struct Normalized(Vec<NormToken>);

#[derive(Debug, Clone)]
enum NormToken {
    Op(Op),
    Val(Val),
    Expr(Normalized),
}

impl Normalized {
    pub(super) fn to_expr(self) -> Expr {
        Normalized::to_expr_recursive(ParsedExpr::None, self.0.as_slice())
    }

    fn to_expr_recursive(e: ParsedExpr, tokens: &[NormToken]) -> Expr {
        let get_expr = |token: &NormToken| match token {
            NormToken::Val(v) => Expr::Value(*v),
            NormToken::Expr(e) => Normalized::to_expr_recursive(ParsedExpr::None, &e.0),
            NormToken::Op(_) => unreachable!(),
        };
        let prio = |op: Op| match op {
            Op::Add => 1,
            Op::Sub => 1,
            Op::Mul => 2,
        };

        match e {
            ParsedExpr::None => {
                let e = get_expr(&tokens[0]);
                Normalized::to_expr_recursive(ParsedExpr::Expr(e), &tokens[1..])
            }
            ParsedExpr::Expr(e) => match tokens.get(0) {
                None => e,
                Some(NormToken::Op(op)) => Normalized::to_expr_recursive(
                    ParsedExpr::Half { op: *op, left: e },
                    &tokens[1..],
                ),
                Some(NormToken::Expr(_)) | Some(NormToken::Val(_)) => unreachable!(),
            },
            ParsedExpr::Half { op, left } => {
                let next_op = tokens.iter().find_map(|t| match t {
                    NormToken::Op(o) => Some(o),
                    _ => None,
                });
                if next_op.map(|next_op| prio(op) < prio(*next_op)) == Some(true) {
                    let right = Normalized::to_expr_recursive(ParsedExpr::None, &tokens);
                    Expr::Expr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    }
                } else {
                    let right = get_expr(&tokens[0]);
                    let expr = Expr::Expr {
                        op,
                        left: Box::new(left),
                        right: Box::new(right),
                    };
                    Normalized::to_expr_recursive(ParsedExpr::Expr(expr), &tokens[1..])
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

#[cfg(test)]
mod test {
    use wasm_bindgen_test::*;

    use super::*;

    #[test]
    #[wasm_bindgen_test]
    fn tokenize_expr_ok() {
        let expr = "d20 - 3 + 2(d6 * 2)";
        let tokens = Tokens::from_str(expr).expect("Unable to tokenize valid expr");
        assert_eq!(
            tokens.0,
            vec![
                IndexedToken::begin(0),
                IndexedToken::value(0, Val::Die(Die::new(20))),
                IndexedToken::operation(4, Op::Sub),
                IndexedToken::value(6, Val::Num(3)),
                IndexedToken::operation(8, Op::Add),
                IndexedToken::value(10, Val::Num(2)),
                IndexedToken::begin(11),
                IndexedToken::value(12, Val::Die(Die::new(6))),
                IndexedToken::operation(15, Op::Mul),
                IndexedToken::value(17, Val::Num(2)),
                IndexedToken::end(18),
                IndexedToken::end(18),
            ]
        );
    }

    #[test]
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

    #[test]
    #[wasm_bindgen_test]
    fn tokenize_expr_bad_die() {
        let expr = "d20 + d0 * 3";
        let tokens = Tokens::from_str(expr);
        assert_eq!(tokens, Err(ParseError::BadDie { index: 6 }))
    }
}
