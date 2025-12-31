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

// src/rewrite/elimination.rs
// Elimination rules: val-elim, exi-elim, eqn-elim, fail-elim

use crate::ast::*;
use crate::core::context::ExecContext;

/// Apply one step of an elimination rule if possible
pub fn rewrite_elimination(expr: &Expr) -> Option<Expr> {
    val_elim(expr)
        .or_else(|| exi_elim(expr))
        .or_else(|| eqn_elim(expr))
        .or_else(|| fail_elim(expr))
        .or_else(|| eqn_dead(expr))
}

// ============================================================================
// Elimination Rules
// ============================================================================

/// val-elim: v; e → e
fn val_elim(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Expr(e1), e2) if e1.is_value() => {
            Some((**e2).clone())
        }
        _ => None,
    }
}

/// exi-elim: ∃x. e → e if x ∉ fvs(e)
fn exi_elim(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Exists(x, e) => {
            let fvs = e.free_vars();
            if !fvs.contains(x) {
                Some((**e).clone())
            } else {
                None
            }
        }
        _ => None,
    }
}

/// eqn-elim: ∃x. X[x = v; e] → X[e]
/// if x ∉ fvs(X[e]) and v ≠ V[x]
fn eqn_elim(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Exists(x, body) => {
            // Decompose the body to find equations
            for (ctx, focused) in ExecContext::decompose(body) {
                // Check if this is an equation x = v; rest
                if let Expr::Seq(ExprOrEqn::Eqn(lhs, rhs), rest) = &focused {
                    if let Value::Var(var) = lhs {
                        if var != x {
                            continue; // Not the variable we're looking for
                        }

                        // Found equation x = v; rest
                        // Check if RHS is a value
                        if let Expr::Value(v) = &**rhs {
                            // Check side condition: v ≠ V[x] (no occurs check)
                            if v.as_var() == Some(x) {
                                continue; // Skip x = x
                            }

                            // Check if v contains x
                            if v.free_vars().contains(x) {
                                continue;
                            }

                            // Build the result by removing the equation
                            let inner = ctx.fill((**rest).clone());

                            // Check x ∉ fvs(inner)
                            if !inner.free_vars().contains(x) {
                                return Some(inner);
                            }
                        }
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// fail-elim: X[fail] → fail
fn fail_elim(expr: &Expr) -> Option<Expr> {
    // Simple recursive check for fail in non-trivial positions
    match expr {
        Expr::Fail => None, // Already fail

        Expr::Seq(eq, rest) => {
            // Check if eq contains fail
            match eq {
                ExprOrEqn::Expr(e) if e.is_fail() => Some(Expr::Fail),
                ExprOrEqn::Eqn(_, e) if e.is_fail() => Some(Expr::Fail),
                _ => {
                    // Check if rest is fail
                    if rest.is_fail() {
                        Some(Expr::Fail)
                    } else {
                        None
                    }
                }
            }
        }

        Expr::Exists(_, e) if e.is_fail() => Some(Expr::Fail),
        Expr::Choice(e1, _) if e1.is_fail() => Some(Expr::Fail),
        Expr::Choice(_, e2) if e2.is_fail() => Some(Expr::Fail),
        Expr::One(e) if e.is_fail() => Some(Expr::Fail),
        Expr::All(e) if e.is_fail() => Some(Expr::Fail),

        _ => None,
    }
}

/// eqn-dead: v = w; e → e if v ∉ fvs(e)
fn eqn_dead(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(v, _rhs), e) => {
            if !e.free_vars().contains(v.as_var()?) {
                Some((**e).clone())
            } else {
                None
            }
        }
        _ => None,
    }
}
