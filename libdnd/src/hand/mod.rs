use serde_derive::Serialize;
use std::str::FromStr;

mod parser;

pub use parser::ParseError;
use parser::Tokens;

pub struct Hand(Expr);

impl Hand {
    pub fn throw(self) -> i64 {
        self.0.throw()
    }

    pub fn analyze(self) -> FreqGraph {
        self.0.analyze()
    }
}

impl FromStr for Hand {
    type Err = ParseError;

    fn from_str(expr: &str) -> Result<Self, Self::Err> {
        let expr = Tokens::from_str(expr)?.normalize()?.to_expr();
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

    fn analyze(self) -> FreqGraph {
        match self {
            Self::Value(Val::Num(n)) => FreqGraph::val(n),
            Self::Value(Val::Die(Die { edges, .. })) => FreqGraph::die(edges),
            Self::Expr {
                op: Op::Mul,
                left,
                right,
            } if right.is_die() => {
                let left = left.analyze();
                let right = right.analyze();
                left.values
                    .iter()
                    .enumerate()
                    .map(|(v, f)| (v as i64 + left.offset, f))
                    .fold(FreqGraph::val(0), |acc, (v, f)| {
                        acc + (0..v)
                            .fold(FreqGraph::val(0), |acc, _| acc + right.clone())
                            .times(*f)
                    })
            }
            Self::Expr { op, left, right } => {
                let left = left.analyze();
                let right = right.analyze();
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

#[derive(Debug, Clone, Serialize)]
pub struct FreqGraph {
    offset: i64,
    values: Vec<f64>,
}

impl FreqGraph {
    fn val(n: u32) -> Self {
        Self {
            offset: n as i64,
            values: vec![1f64],
        }
    }

    fn die(d: u32) -> Self {
        let vec = vec![1f64; d as usize];
        Self {
            offset: 1,
            values: vec,
        }
    }

    fn times(self, t: f64) -> Self {
        Self {
            values: self.values.into_iter().map(|f| f * t).collect(),
            offset: self.offset,
        }
    }
}

impl std::ops::Add for FreqGraph {
    type Output = Self;

    fn add(self, rhs: Self) -> Self::Output {
        let len = self.values.len() + rhs.values.len() - 1;
        let mut values = Vec::with_capacity(len);
        values.resize(len, 0f64);
        for (ln, lfreq) in self.values.iter().enumerate() {
            for (rn, rfreq) in rhs.values.iter().enumerate() {
                values[ln + rn] = values[ln + rn] + lfreq * rfreq;
            }
        }
        Self {
            offset: self.offset + rhs.offset,
            values,
        }
    }
}

impl std::ops::Sub for FreqGraph {
    type Output = Self;

    fn sub(self, rhs: Self) -> Self::Output {
        let len = self.values.len() + rhs.values.len() - 1;
        let mut values = Vec::with_capacity(len);
        values.resize(len, 0f64);
        for (ln, lfreq) in self.values.iter().enumerate() {
            for (rn, rfreq) in rhs.values.iter().enumerate() {
                values[ln + rn] = values[ln + rn] + lfreq * rfreq;
            }
        }
        // TODO: Values are probably wrong here
        Self {
            offset: self.offset - rhs.offset,
            values,
        }
    }
}

impl std::ops::Mul for FreqGraph {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self::Output {
        let max = (self.values.len() as i64 - 1 + self.offset)
            * (rhs.values.len() as i64 - 1 + rhs.offset)
            + 1;
        let offset = self.offset * rhs.offset;
        let len = max - offset;
        let mut values = Vec::with_capacity(len as usize);
        values.resize(len as usize, 0f64);
        for (ln, lfreq) in self.values.iter().enumerate() {
            for (rn, rfreq) in rhs.values.iter().enumerate() {
                let res = ((ln as i64 + self.offset) * (rn as i64 + rhs.offset) - offset) as usize;
                values[res] = values[res] + lfreq * rfreq;
            }
        }
        Self { offset, values }
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
}
