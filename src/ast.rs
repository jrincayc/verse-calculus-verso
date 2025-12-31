// Verse Calculus Implementation
//
// Based on "The Verse Calculus: A Core Calculus for Deterministic Functional Logic Programming"
// by Lennart Augustsson, Joachim Breitner, Koen Claessen, Ranjit Jhala,
// Simon Peyton Jones, Olin Shivers, Guy L. Steele Jr., and Tim Sweeney
// https://dl.acm.org/doi/abs/10.1145/3607845
//
// Implementation by Claude Sonnet 4.5 (Anthropic) with assistance from Joshua Cogliati and Gemini 3 (Google)
//
// Licensed under Creative Commons Attribution 4.0 International License
// https://creativecommons.org/licenses/by/4.0/

// src/ast.rs
// Core AST for the Verse Calculus

use std::collections::HashSet;
use std::fmt;

// ============================================================================
// Core Types
// ============================================================================

/// Variable names
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Var(pub String);

impl Var {
    pub fn new(s: impl Into<String>) -> Self {
        Var(s.into())
    }

    pub fn fresh(prefix: &str) -> Self {
        use std::sync::atomic::{AtomicUsize, Ordering};
        static COUNTER: AtomicUsize = AtomicUsize::new(0);
        let id = COUNTER.fetch_add(1, Ordering::SeqCst);
        Var(format!("{}_{}", prefix, id))
    }
}

impl fmt::Display for Var {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Integer literals
pub type IntLit = i64;

/// Primitive operators
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PrimOp {
    Add,
    Gt,
}

impl fmt::Display for PrimOp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PrimOp::Add => write!(f, "add"),
            PrimOp::Gt => write!(f, "gt"),
        }
    }
}

// ============================================================================
// Expressions (Core Syntax)
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Value(Value),
    Seq(ExprOrEqn, Box<Expr>),
    Exists(Var, Box<Expr>),
    Fail,
    Choice(Box<Expr>, Box<Expr>),
    App(Value, Value),
    One(Box<Expr>),
    All(Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprOrEqn {
    Expr(Box<Expr>),
    Eqn(Value, Box<Expr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Var(Var),
    Hnf(HeadNormalForm),
}

#[derive(Debug, Clone, PartialEq)]
pub enum HeadNormalForm {
    Int(IntLit),
    Op(PrimOp),
    Tuple(Vec<Value>),
    Lambda(Var, Box<Expr>),
}

// ============================================================================
// Programs
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub struct Program {
    pub expr: Expr,
}

impl Program {
    pub fn new(expr: Expr) -> Result<Self, String> {
        let free_vars = expr.free_vars();
        if !free_vars.is_empty() {
            return Err(format!(
                "Program must be closed, but has free variables: {:?}",
                free_vars
            ));
        }
        Ok(Program { expr: Expr::One(Box::new(expr)) })
    }
}

// ============================================================================
// Extended Syntax
// ============================================================================

#[derive(Debug, Clone, PartialEq)]
pub enum ExtendedExpr {
    Value(Value),
    Seq(Box<ExtendedExprOrEqn>, Box<ExtendedExpr>),
    Exists(Vec<Var>, Box<ExtendedExpr>),
    Fail,
    Choice(Box<ExtendedExpr>, Box<ExtendedExpr>),
    App(Box<ExtendedExpr>, Box<ExtendedExpr>),
    One(Box<ExtendedExpr>),
    All(Box<ExtendedExpr>),
    BinOp(BinOpKind, Box<ExtendedExpr>, Box<ExtendedExpr>),
    TupleCtor(Vec<ExtendedExpr>),
    LambdaPattern(Vec<Var>, Box<ExtendedExpr>),
    IfThenElse {
        cond_vars: Vec<Var>,
        cond: Box<ExtendedExpr>,
        then_branch: Box<ExtendedExpr>,
        else_branch: Box<ExtendedExpr>,
    },
    ForDo {
        vars: Vec<Var>,
        generator: Box<ExtendedExpr>,
        body: Box<ExtendedExpr>,
    },
    Let(Var, Box<ExtendedExpr>, Box<ExtendedExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExtendedExprOrEqn {
    Expr(Box<ExtendedExpr>),
    Eqn(Box<ExtendedExpr>, Box<ExtendedExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOpKind {
    Add,
    Gt,
}

// ============================================================================
// Helper Methods
// ============================================================================

impl Expr {
    pub fn free_vars(&self) -> HashSet<Var> {
        match self {
            Expr::Value(v) => v.free_vars(),
            Expr::Seq(eq, e) => {
                let mut fvs = eq.free_vars();
                fvs.extend(e.free_vars());
                fvs
            }
            Expr::Exists(x, e) => {
                let mut fvs = e.free_vars();
                fvs.remove(x);
                fvs
            }
            Expr::Fail => HashSet::new(),
            Expr::Choice(e1, e2) => {
                let mut fvs = e1.free_vars();
                fvs.extend(e2.free_vars());
                fvs
            }
            Expr::App(v1, v2) => {
                let mut fvs = v1.free_vars();
                fvs.extend(v2.free_vars());
                fvs
            }
            Expr::One(e) | Expr::All(e) => e.free_vars(),
        }
    }

    pub fn is_value(&self) -> bool {
        matches!(self, Expr::Value(_))
    }

    pub fn as_value(&self) -> Option<&Value> {
        match self {
            Expr::Value(v) => Some(v),
            _ => None,
        }
    }

    pub fn is_fail(&self) -> bool {
        matches!(self, Expr::Fail)
    }

    pub fn is_choice_free(&self) -> bool {
        match self {
            Expr::Value(_) | Expr::Fail => true,
            Expr::Seq(eq, e) => eq.is_choice_free() && e.is_choice_free(),
            Expr::Exists(_, e) => e.is_choice_free(),
            Expr::Choice(_, _) => false,
            Expr::App(_, _) => false,
            Expr::One(_) | Expr::All(_) => true,
        }
    }
}

impl ExprOrEqn {
    pub fn free_vars(&self) -> HashSet<Var> {
        match self {
            ExprOrEqn::Expr(e) => e.free_vars(),
            ExprOrEqn::Eqn(v, e) => {
                let mut fvs = v.free_vars();
                fvs.extend(e.free_vars());
                fvs
            }
        }
    }

    pub fn is_choice_free(&self) -> bool {
        match self {
            ExprOrEqn::Expr(e) => e.is_choice_free(),
            ExprOrEqn::Eqn(v, e) => e.is_choice_free(),
        }
    }

    pub fn is_eqn(&self) -> bool {
        matches!(self, ExprOrEqn::Eqn(_, _))
    }
}

impl Value {
    pub fn free_vars(&self) -> HashSet<Var> {
        match self {
            Value::Var(x) => {
                let mut set = HashSet::new();
                set.insert(x.clone());
                set
            }
            Value::Hnf(hnf) => hnf.free_vars(),
        }
    }

    pub fn is_var(&self) -> bool {
        matches!(self, Value::Var(_))
    }

    pub fn is_hnf(&self) -> bool {
        matches!(self, Value::Hnf(_))
    }

    pub fn as_var(&self) -> Option<&Var> {
        match self {
            Value::Var(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_hnf(&self) -> Option<&HeadNormalForm> {
        match self {
            Value::Hnf(hnf) => Some(hnf),
            _ => None,
        }
    }
}

impl HeadNormalForm {
    pub fn free_vars(&self) -> HashSet<Var> {
        match self {
            HeadNormalForm::Int(_) | HeadNormalForm::Op(_) => HashSet::new(),
            HeadNormalForm::Tuple(vs) => {
                let mut fvs = HashSet::new();
                for v in vs {
                    fvs.extend(v.free_vars());
                }
                fvs
            }
            HeadNormalForm::Lambda(x, e) => {
                let mut fvs = e.free_vars();
                fvs.remove(x);
                fvs
            }
        }
    }

    pub fn is_lambda(&self) -> bool {
        matches!(self, HeadNormalForm::Lambda(_, _))
    }
}

// ============================================================================
// Convenience Constructors
// ============================================================================

impl Expr {
    pub fn var(name: impl Into<String>) -> Self {
        Expr::Value(Value::Var(Var::new(name)))
    }

    pub fn int(n: IntLit) -> Self {
        Expr::Value(Value::Hnf(HeadNormalForm::Int(n)))
    }

    pub fn tuple(vs: Vec<Value>) -> Self {
        Expr::Value(Value::Hnf(HeadNormalForm::Tuple(vs)))
    }

    pub fn empty_tuple() -> Self {
        Self::tuple(vec![])
    }

    pub fn lambda(x: Var, body: Expr) -> Self {
        Expr::Value(Value::Hnf(HeadNormalForm::Lambda(x, Box::new(body))))
    }

    pub fn app(v1: Value, v2: Value) -> Self {
        Expr::App(v1, v2)
    }

    pub fn seq(eq: ExprOrEqn, e: Expr) -> Self {
        Expr::Seq(eq, Box::new(e))
    }

    pub fn eqn(v: Value, e: Expr) -> ExprOrEqn {
        ExprOrEqn::Eqn(v, Box::new(e))
    }

    pub fn exists(x: Var, e: Expr) -> Self {
        Expr::Exists(x, Box::new(e))
    }

    pub fn choice(e1: Expr, e2: Expr) -> Self {
        Expr::Choice(Box::new(e1), Box::new(e2))
    }

    pub fn one(e: Expr) -> Self {
        Expr::One(Box::new(e))
    }

    pub fn all(e: Expr) -> Self {
        Expr::All(Box::new(e))
    }
}

impl Value {
    pub fn var(name: impl Into<String>) -> Self {
        Value::Var(Var::new(name))
    }

    pub fn int(n: IntLit) -> Self {
        Value::Hnf(HeadNormalForm::Int(n))
    }

    pub fn tuple(vs: Vec<Value>) -> Self {
        Value::Hnf(HeadNormalForm::Tuple(vs))
    }
}
