#![allow(unused)]
use std::str::FromStr;

mod parser;

use parser::Tokens;

pub use parser::ParseError;

pub struct Hand(Expr);
pub struct Error;

impl Hand {
    pub fn new() -> Self {
        Self(Expr::Value(Val::Num(0)))
    }

    pub fn throw(self) -> i64 {
        self.0.throw()
    }
}

impl FromStr for Hand {
    type Err = ParseError;

    fn from_str(expr: &str) -> Result<Self, Self::Err> {
        let expr = parser::Tokens::from_str(expr)?.normalize()?.to_expr();
        Ok(Hand(expr))
    }
}

enum Expr {
    Value(Val),
    Expr {
        op: Op,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

impl Expr {
    fn throw(self) -> i64 {
        match self {
            Self::Value(Val::Num(n)) => n as i64,
            Self::Value(Val::Die(d)) => (rand::random::<u32>() % d + 1) as i64,
            Self::Expr { op, left, right } => {
                let left = left.throw();
                let right = right.throw();
                match op {
                    Op::Add => left + right,
                    Op::Sub => left - right,
                    Op::Mul => left * right,
                }
            }
        }
    }
}

#[derive(Debug, Copy, Clone)]
enum Op {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Copy, Clone)]
enum Val {
    Num(u32),
    Die(u32),
}
