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

#[derive(Debug, Clone)]
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
            Self::Value(Val::Die(d)) => (rand::random::<u32>() % d.edges + 1) as i64,
            Self::Expr {
                op: Op::Mul,
                left,
                right,
            } if right.is_die() => {
                let left = left.throw();
                (0..left).fold(0, |acc, _| acc + right.clone().throw())
            }
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

    fn is_die(&self) -> bool {
        if let Self::Value(Val::Die(_)) = self {
            true
        } else {
            false
        }
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Op {
    Add,
    Sub,
    Mul,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
enum Val {
    Num(u32),
    Die(Die),
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
struct Die {
    edges: u32,
    rolls: u32,
    drop_lowest: u32,
    drop_highest: u32,
}

impl Die {
    fn new(edges: u32) -> Self {
        Die {
            edges,
            rolls: 1,
            drop_lowest: 0,
            drop_highest: 0,
        }
    }

    fn rolls(mut self, n: u32) -> Self {
        self.rolls = n;
        self
    }
}
