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

// src/rewrite/application.rs
// Application rewrite rules (app-add, app-gt, app-beta, app-tup, etc.)

use crate::ast::*;
use crate::core::subst::substitute;

/// Apply one step of an application rule if possible
/// Returns Some(new_expr) if a rule applied, None otherwise
pub fn rewrite_application(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::App(v1, v2) => {
            // Try each application rule
            app_add(v1, v2)
                .or_else(|| app_gt(v1, v2))
                .or_else(|| app_gt_fail(v1, v2))
                .or_else(|| app_beta(v1, v2))
                .or_else(|| app_tup(v1, v2))
                .or_else(|| app_tup_0(v1, v2))
        }
        _ => None,
    }
}

// ============================================================================
// Primitive Operations
// ============================================================================

/// app-add: add⟨k₁, k₂⟩ → k₃ where k₃ = k₁ + k₂
fn app_add(v1: &Value, v2: &Value) -> Option<Expr> {
    match (v1, v2) {
        (
            Value::Hnf(HeadNormalForm::Op(PrimOp::Add)),
            Value::Hnf(HeadNormalForm::Tuple(args)),
        ) if args.len() == 2 => {
            match (&args[0], &args[1]) {
                (Value::Hnf(HeadNormalForm::Int(k1)), Value::Hnf(HeadNormalForm::Int(k2))) => {
                    Some(Expr::int(k1 + k2))
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// app-gt: gt⟨k₁, k₂⟩ → k₁ if k₁ > k₂
fn app_gt(v1: &Value, v2: &Value) -> Option<Expr> {
    match (v1, v2) {
        (
            Value::Hnf(HeadNormalForm::Op(PrimOp::Gt)),
            Value::Hnf(HeadNormalForm::Tuple(args)),
        ) if args.len() == 2 => {
            match (&args[0], &args[1]) {
                (Value::Hnf(HeadNormalForm::Int(k1)), Value::Hnf(HeadNormalForm::Int(k2))) => {
                    if k1 > k2 {
                        Some(Expr::Value(args[0].clone()))
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

/// app-gt-fail: gt⟨k₁, k₂⟩ → fail if k₁ ≤ k₂
fn app_gt_fail(v1: &Value, v2: &Value) -> Option<Expr> {
    match (v1, v2) {
        (
            Value::Hnf(HeadNormalForm::Op(PrimOp::Gt)),
            Value::Hnf(HeadNormalForm::Tuple(args)),
        ) if args.len() == 2 => {
            match (&args[0], &args[1]) {
                (Value::Hnf(HeadNormalForm::Int(k1)), Value::Hnf(HeadNormalForm::Int(k2))) => {
                    if k1 <= k2 {
                        Some(Expr::Fail)
                    } else {
                        None
                    }
                }
                _ => None,
            }
        }
        _ => None,
    }
}

// ============================================================================
// Beta Reduction
// ============================================================================

/// app-beta: (λx. e)(v) → ∃x. x = v; e if x ∉ fvs(v)
fn app_beta(v1: &Value, v2: &Value) -> Option<Expr> {
    match v1 {
        Value::Hnf(HeadNormalForm::Lambda(x, e)) => {
            // Check side condition: x ∉ fvs(v2)
            if v2.free_vars().contains(x) {
                // Would need alpha-conversion, skip for now
                // (Could implement later with alpha-convert)
                None
            } else {
                // Apply the rule: ∃x. x = v; e
                Some(Expr::Exists(
                    x.clone(),
                    Box::new(Expr::Seq(
                        ExprOrEqn::Eqn(Value::Var(x.clone()), Box::new(Expr::Value(v2.clone()))),
                        e.clone(),
                    )),
                ))
            }
        }
        _ => None,
    }
}

// ============================================================================
// Tuple Indexing
// ============================================================================

/// app-tup: ⟨v₀, ..., vₙ⟩(v) → ∃x. x = v; (x = 0; v₀) ⊕ ... ⊕ (x = n; vₙ)
/// where x is fresh and x ∉ fvs(v, v₀, ..., vₙ)
fn app_tup(v1: &Value, v2: &Value) -> Option<Expr> {
    match v1 {
        Value::Hnf(HeadNormalForm::Tuple(vals)) if !vals.is_empty() => {
            // Generate fresh variable
            let x = Var::fresh("x");

            // Build choice tree: (x = 0; v₀) ⊕ (x = 1; v₁) ⊕ ...
            let mut choice = Expr::Seq(
                ExprOrEqn::Eqn(
                    Value::Var(x.clone()),
                    Box::new(Expr::int(vals.len() as i64 - 1)),
                ),
                Box::new(Expr::Value(vals[vals.len() - 1].clone())),
            );

            for i in (0..vals.len() - 1).rev() {
                let branch = Expr::Seq(
                    ExprOrEqn::Eqn(
                        Value::Var(x.clone()),
                        Box::new(Expr::int(i as i64)),
                    ),
                    Box::new(Expr::Value(vals[i].clone())),
                );
                choice = Expr::Choice(Box::new(branch), Box::new(choice));
            }

            // Wrap in ∃x. x = v; ...
            Some(Expr::Exists(
                x.clone(),
                Box::new(Expr::Seq(
                    ExprOrEqn::Eqn(Value::Var(x), Box::new(Expr::Value(v2.clone()))),
                    Box::new(choice),
                )),
            ))
        }
        _ => None,
    }
}

/// app-tup-0: ⟨⟩(v) → fail
fn app_tup_0(v1: &Value, v2: &Value) -> Option<Expr> {
    match v1 {
        Value::Hnf(HeadNormalForm::Tuple(vals)) if vals.is_empty() => Some(Expr::Fail),
        _ => None,
    }
}
