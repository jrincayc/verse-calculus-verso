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

// src/syntax/pretty.rs
// Pretty printing for Verse Calculus expressions

use crate::ast::*;
use std::fmt;

// ============================================================================
// Display Implementations
// ============================================================================

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", PrettyExpr(self, 0))
    }
}

impl fmt::Display for ExprOrEqn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExprOrEqn::Expr(e) => write!(f, "{}", e),
            ExprOrEqn::Eqn(v, e) => write!(f, "{} = {}", v, e),
        }
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Value::Var(x) => write!(f, "{}", x),
            Value::Hnf(hnf) => write!(f, "{}", hnf),
        }
    }
}

impl fmt::Display for HeadNormalForm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            HeadNormalForm::Int(n) => write!(f, "{}", n),
            HeadNormalForm::Op(op) => write!(f, "{}", op),
            HeadNormalForm::Tuple(vs) => {
                write!(f, "⟨")?;
                for (i, v) in vs.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", v)?;
                }
                write!(f, "⟩")
            }
            HeadNormalForm::Lambda(x, e) => {
                write!(f, "λ{}. {}", x, e)
            }
        }
    }
}

// ============================================================================
// Pretty Printer with Precedence
// ============================================================================

struct PrettyExpr<'a>(&'a Expr, u8);

// Precedence levels (higher = binds tighter)
const PREC_CHOICE: u8 = 1;
const PREC_SEQ: u8 = 2;
const PREC_EXISTS: u8 = 3;
const PREC_APP: u8 = 4;
const PREC_ATOM: u8 = 5;

impl<'a> fmt::Display for PrettyExpr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let PrettyExpr(expr, parent_prec) = self;

        match expr {
            Expr::Value(v) => write!(f, "{}", v),

            Expr::Fail => write!(f, "fail"),

            Expr::One(e) => {
                if *parent_prec > PREC_ATOM {
                    write!(f, "(one{{{}}})", e)
                } else {
                    write!(f, "one{{{}}}", e)
                }
            }

            Expr::All(e) => {
                if *parent_prec > PREC_ATOM {
                    write!(f, "(all{{{}}})", e)
                } else {
                    write!(f, "all{{{}}}", e)
                }
            }

            Expr::App(v1, v2) => {
                let s = format!("{} {}", v1, format_value_in_app(v2));
                if *parent_prec > PREC_APP {
                    write!(f, "({})", s)
                } else {
                    write!(f, "{}", s)
                }
            }

            Expr::Choice(e1, e2) => {
                let s = format_choice(expr);
                if *parent_prec > PREC_CHOICE {
                    write!(f, "({})", s)
                } else {
                    write!(f, "{}", s)
                }
            }

            Expr::Seq(eq, e) => {
                let s = format_seq(expr);
                if *parent_prec > PREC_SEQ {
                    write!(f, "({})", s)
                } else {
                    write!(f, "{}", s)
                }
            }

            Expr::Exists(x, e) => {
                let s = format_exists(expr);
                if *parent_prec > PREC_EXISTS {
                    write!(f, "({})", s)
                } else {
                    write!(f, "{}", s)
                }
            }
        }
    }
}

fn format_value_in_app(v: &Value) -> String {
    match v {
        Value::Hnf(HeadNormalForm::Tuple(_)) => format!("({})", v),
        _ => format!("{}", v),
    }
}

fn format_choice(expr: &Expr) -> String {
    let mut parts = Vec::new();
    collect_choices(expr, &mut parts);
    parts.join(" ⊕ ")
}

fn collect_choices(expr: &Expr, parts: &mut Vec<String>) {
    match expr {
        Expr::Choice(e1, e2) => {
            collect_choices(e1, parts);
            collect_choices(e2, parts);
        }
        _ => parts.push(format!("{}", PrettyExpr(expr, PREC_CHOICE + 1))),
    }
}

fn format_seq(expr: &Expr) -> String {
    let mut parts = Vec::new();
    collect_seqs(expr, &mut parts);
    parts.join("; ")
}

fn collect_seqs(expr: &Expr, parts: &mut Vec<String>) {
    match expr {
        Expr::Seq(eq, e) => {
            parts.push(format!("{}", eq));
            collect_seqs(e, parts);
        }
        _ => parts.push(format!("{}", PrettyExpr(expr, PREC_SEQ + 1))),
    }
}

fn format_exists(expr: &Expr) -> String {
    let mut vars = Vec::new();
    let body = collect_exists(expr, &mut vars);
    format!("∃{}. {}", vars.join(" "), body)
}

fn collect_exists<'a>(expr: &'a Expr, vars: &mut Vec<String>) -> &'a Expr {
    match expr {
        Expr::Exists(x, e) => {
            vars.push(format!("{}", x));
            collect_exists(e, vars)
        }
        _ => expr,
    }
}

// ============================================================================
// Extended Expression Display
// ============================================================================

impl fmt::Display for ExtendedExpr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExtendedExpr::Value(v) => write!(f, "{}", v),
            ExtendedExpr::Fail => write!(f, "fail"),

            ExtendedExpr::BinOp(op, e1, e2) => {
                let op_str = match op {
                    BinOpKind::Add => "+",
                    BinOpKind::Gt => ">",
                };
                write!(f, "{} {} {}",
                    format_extended_in_binop(e1),
                    op_str,
                    format_extended_in_binop(e2))
            }

            ExtendedExpr::TupleCtor(es) => {
                write!(f, "⟨")?;
                for (i, e) in es.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", e)?;
                }
                write!(f, "⟩")
            }

            ExtendedExpr::App(e1, e2) => {
                write!(f, "{} {}",
                    format_extended_in_app(e1),
                    format_extended_in_app(e2))
            }

            ExtendedExpr::Seq(eq, e) => {
                write!(f, "{}; {}", eq, e)
            }

            ExtendedExpr::Exists(vars, e) => {
                write!(f, "∃")?;
                for (i, x) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, " ")?;
                    }
                    write!(f, "{}", x)?;
                }
                write!(f, ". {}", e)
            }

            ExtendedExpr::Choice(e1, e2) => {
                write!(f, "{} ⊕ {}", e1, e2)
            }

            ExtendedExpr::One(e) => write!(f, "one{{{}}}", e),
            ExtendedExpr::All(e) => write!(f, "all{{{}}}", e),

            ExtendedExpr::LambdaPattern(vars, e) => {
                write!(f, "λ⟨")?;
                for (i, x) in vars.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", x)?;
                }
                write!(f, "⟩. {}", e)
            }

            ExtendedExpr::IfThenElse { cond_vars, cond, then_branch, else_branch } => {
                write!(f, "if (")?;
                if !cond_vars.is_empty() {
                    write!(f, "∃")?;
                    for (i, x) in cond_vars.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", x)?;
                    }
                    write!(f, ". ")?;
                }
                write!(f, "{}) then {} else {}", cond, then_branch, else_branch)
            }

            ExtendedExpr::ForDo { vars, generator, body } => {
                write!(f, "for(")?;
                if !vars.is_empty() {
                    write!(f, "∃")?;
                    for (i, x) in vars.iter().enumerate() {
                        if i > 0 {
                            write!(f, " ")?;
                        }
                        write!(f, "{}", x)?;
                    }
                    write!(f, ". ")?;
                }
                write!(f, "{}) do {}", generator, body)
            }

            ExtendedExpr::Let(x, e1, e2) => {
                write!(f, "{} := {}; {}", x, e1, e2)
            }
        }
    }
}

fn format_extended_in_binop(e: &ExtendedExpr) -> String {
    match e {
        ExtendedExpr::BinOp(_, _, _) |
        ExtendedExpr::Seq(_, _) |
        ExtendedExpr::Choice(_, _) => format!("({})", e),
        _ => format!("{}", e),
    }
}

fn format_extended_in_app(e: &ExtendedExpr) -> String {
    match e {
        ExtendedExpr::Value(_) => format!("{}", e),
        _ => format!("({})", e),
    }
}

impl fmt::Display for ExtendedExprOrEqn {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            ExtendedExprOrEqn::Expr(e) => write!(f, "{}", e),
            ExtendedExprOrEqn::Eqn(e1, e2) => write!(f, "{} = {}", e1, e2),
        }
    }
}

// ============================================================================
// Utilities for debugging
// ============================================================================

impl Expr {
    pub fn to_compact_string(&self) -> String {
        format!("{}", self)
    }

    pub fn to_pretty_string(&self) -> String {
        format!("{}", PrettyPrinter::new(self, 0))
    }
}

struct PrettyPrinter<'a> {
    expr: &'a Expr,
    indent: usize,
}

impl<'a> PrettyPrinter<'a> {
    fn new(expr: &'a Expr, indent: usize) -> Self {
        PrettyPrinter { expr, indent }
    }

    fn indent_str(&self) -> String {
        "  ".repeat(self.indent)
    }
}

impl<'a> fmt::Display for PrettyPrinter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self.expr {
            Expr::Seq(eq, e) => {
                write!(f, "{}", eq)?;
                writeln!(f, ";")?;
                write!(f, "{}{}", self.indent_str(), PrettyPrinter::new(e, self.indent))
            }

            Expr::Exists(x, e) => {
                write!(f, "∃{}.", x)?;
                writeln!(f)?;
                write!(f, "{}{}", self.indent_str(), PrettyPrinter::new(e, self.indent))
            }

            Expr::Choice(e1, e2) => {
                write!(f, "{}", PrettyPrinter::new(e1, self.indent + 1))?;
                writeln!(f)?;
                write!(f, "{}⊕", self.indent_str())?;
                writeln!(f)?;
                write!(f, "{}{}", self.indent_str(), PrettyPrinter::new(e2, self.indent + 1))
            }

            _ => write!(f, "{}", self.expr),
        }
    }
}
