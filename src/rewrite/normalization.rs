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

// src/rewrite/normalization.rs
// Normalization rules: exi-float, seq-assoc, eqn-float, exi-swap

use crate::ast::*;

/// Apply one step of a normalization rule if possible
pub fn rewrite_normalization(expr: &Expr) -> Option<Expr> {
    exi_float(expr)
        .or_else(|| seq_assoc(expr))
        .or_else(|| eqn_float(expr))
        .or_else(|| exi_swap(expr))
}

// ============================================================================
// Normalization Rules
// ============================================================================

/// exi-float: X[∃x. e] → ∃x. X[e] if x ∉ fvs(X)
/// This floats existentials outward through contexts
fn exi_float(expr: &Expr) -> Option<Expr> {
    match expr {
        // Float through sequences
        Expr::Seq(eq, e) => {
            // Check if the equation part has an exists
            match eq {
                ExprOrEqn::Expr(eq_expr) => {
                    if let Expr::Exists(x, inner) = &**eq_expr {
                        let eq_fvs = eq_expr.free_vars();
                        let rest_fvs = e.free_vars();

                        // Check if x is free in the rest (not in the equation itself)
                        if !rest_fvs.contains(x) {
                            // Float out: ∃x. inner; e
                            return Some(Expr::Exists(
                                x.clone(),
                                Box::new(Expr::Seq(
                                    ExprOrEqn::Expr(inner.clone()),
                                    e.clone(),
                                )),
                            ));
                        }
                    }
                }
                ExprOrEqn::Eqn(v, rhs) => {
                    if let Expr::Exists(x, inner) = &**rhs {
                        // Check if x is free in v or rest
                        if !v.free_vars().contains(x) && !e.free_vars().contains(x) {
                            // Float out: ∃x. v = inner; e
                            return Some(Expr::Exists(
                                x.clone(),
                                Box::new(Expr::Seq(
                                    ExprOrEqn::Eqn(v.clone(), inner.clone()),
                                    e.clone(),
                                )),
                            ));
                        }
                    }
                }
            }

            // Check if the continuation has an exists
            if let Expr::Exists(x, inner) = &**e {
                // Check if x is free in the equation
                if !eq.free_vars().contains(x) {
                    // Float out: ∃x. eq; inner
                    return Some(Expr::Exists(
                        x.clone(),
                        Box::new(Expr::Seq(eq.clone(), inner.clone())),
                    ));
                }
            }

            None
        }

        // Float through choices
        Expr::Choice(e1, e2) => {
            if let Expr::Exists(x, inner) = &**e1 {
                if !e2.free_vars().contains(x) {
                    return Some(Expr::Exists(
                        x.clone(),
                        Box::new(Expr::Choice(inner.clone(), e2.clone())),
                    ));
                }
            }

            if let Expr::Exists(x, inner) = &**e2 {
                if !e1.free_vars().contains(x) {
                    return Some(Expr::Exists(
                        x.clone(),
                        Box::new(Expr::Choice(e1.clone(), inner.clone())),
                    ));
                }
            }

            None
        }

        /* XXX should not float thru one/all by paper since X does not include those
        // Float through one/all
        Expr::One(e) => {
            if let Expr::Exists(x, inner) = &**e {
                // Always float out of one
                return Some(Expr::Exists(
                    x.clone(),
                    Box::new(Expr::One(inner.clone())),
                ));
            }
            None
        }

        Expr::All(e) => {
            if let Expr::Exists(x, inner) = &**e {
                // Always float out of all
                return Some(Expr::Exists(
                    x.clone(),
                    Box::new(Expr::All(inner.clone())),
                ));
            }
            None
        }
        */

        _ => None,
    }
}

/// seq-assoc: (eq; e₁); e₂ → eq; (e₁; e₂)
/// Makes sequences right-associative
fn seq_assoc(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Expr(e1), e2) => {
            // Check if e1 is itself a sequence
            if let Expr::Seq(inner_eq, inner_e) = &**e1 {
                // Reassociate: inner_eq; (inner_e; e2)
                return Some(Expr::Seq(
                    inner_eq.clone(),
                    Box::new(Expr::Seq(
                        ExprOrEqn::Expr(inner_e.clone()),
                        e2.clone(),
                    )),
                ));
            }
            None
        }
        _ => None,
    }
}

/// eqn-float: v = (eq; e₁); e₂ → eq; (v = e₁; e₂)
/// Floats work out of the RHS of an equation
fn eqn_float(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(v, rhs), rest) => {
            // Check if RHS is a sequence
            if let Expr::Seq(inner_eq, inner_e) = &**rhs {
                // Float: inner_eq; (v = inner_e; rest)
                return Some(Expr::Seq(
                    inner_eq.clone(),
                    Box::new(Expr::Seq(
                        ExprOrEqn::Eqn(v.clone(), inner_e.clone()),
                        rest.clone(),
                    )),
                ));
            }
            None
        }
        _ => None,
    }
}

/// exi-swap: ∃x. ∃y. e → ∃y. ∃x. e
/// Swaps adjacent existentials
/// Note: This can loop infinitely, so should be used carefully
fn exi_swap(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Exists(x, e) => {
            if let Expr::Exists(y, inner) = &**e {
                // Only swap if it helps (e.g., for variable ordering)
                // For now, we'll implement it but note it needs care
                // A better implementation would check binding order
                if x > y {
                    return Some(Expr::Exists(
                        y.clone(),
                        Box::new(Expr::Exists(x.clone(), inner.clone())),
                    ));
                }
            }
            None
        }
        _ => None,
    }
}
