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

// src/rewrite/unification.rs
// Unification rules: subst, u-lit, u-tup, u-fail, u-occurs, hnf-swap, var-swap, seq-swap

use crate::ast::*;
use crate::core::context::{ExecContext, ValueContext};
use crate::core::subst::substitute;

/// Apply one step of a unification rule if possible
pub fn rewrite_unification(expr: &Expr) -> Option<Expr> {
    u_lit(expr)
        .or_else(|| u_tup(expr))
        .or_else(|| u_fail(expr))
        .or_else(|| u_occurs(expr))
        .or_else(|| subst_rule(expr))
        .or_else(|| hnf_swap(expr))
}

// ============================================================================
// Basic Unification Rules
// ============================================================================

/// u-lit: k₁ = k₂; e → e if k₁ = k₂
fn u_lit(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(v1, rhs), rest) => {
            if let (
                Value::Hnf(HeadNormalForm::Int(k1)),
                Expr::Value(Value::Hnf(HeadNormalForm::Int(k2))),
            ) = (v1, &**rhs)
            {
                if k1 == k2 {
                    return Some((**rest).clone());
                }
            }
            None
        }
        _ => None,
    }
}

/// u-tup: ⟨v₁, ..., vₙ⟩ = ⟨v'₁, ..., v'ₙ⟩; e → v₁ = v'₁; ...; vₙ = v'ₙ; e
fn u_tup(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(v1, rhs), rest) => {
            if let (
                Value::Hnf(HeadNormalForm::Tuple(vals1)),
                Expr::Value(Value::Hnf(HeadNormalForm::Tuple(vals2))),
            ) = (v1, &**rhs)
            {
                if vals1.len() == vals2.len() {
                    // Build v₁ = v'₁; ...; vₙ = v'ₙ; e
                    let mut result = (**rest).clone();
                    for (v, v_prime) in vals1.iter().zip(vals2.iter()).rev() {
                        result = Expr::Seq(
                            ExprOrEqn::Eqn(v.clone(), Box::new(Expr::Value(v_prime.clone()))),
                            Box::new(result),
                        );
                    }
                    return Some(result);
                }
            }
            None
        }
        _ => None,
    }
}

/// u-fail: hnf₁ = hnf₂; e → fail
/// if u-lit and u-tup don't match and neither is a lambda
fn u_fail(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(v1, rhs), _rest) => {
            if let (Value::Hnf(hnf1), Expr::Value(Value::Hnf(hnf2))) = (v1, &**rhs) {
                // Check if they're different head constructors
                let should_fail = match (hnf1, hnf2) {
                    // Same integers - u-lit handles this
                    (HeadNormalForm::Int(k1), HeadNormalForm::Int(k2)) if k1 == k2 => false,

                    // Different integers - fail
                    (HeadNormalForm::Int(_), HeadNormalForm::Int(_)) => true,

                    // Same-length tuples - u-tup handles this
                    (HeadNormalForm::Tuple(v1), HeadNormalForm::Tuple(v2)) if v1.len() == v2.len() => false,

                    // Different-length tuples - fail
                    (HeadNormalForm::Tuple(_), HeadNormalForm::Tuple(_)) => true,

                    // Different head constructors (int vs tuple, int vs op, etc.) - fail
                    (HeadNormalForm::Int(_), _) | (_, HeadNormalForm::Int(_)) => true,
                    (HeadNormalForm::Tuple(_), _) | (_, HeadNormalForm::Tuple(_)) => true,
                    (HeadNormalForm::Op(_), _) | (_, HeadNormalForm::Op(_)) => true,

                    // Lambdas - don't fail (stuck)
                    (HeadNormalForm::Lambda(_, _), _) | (_, HeadNormalForm::Lambda(_, _)) => false,
                };

                if should_fail {
                    return Some(Expr::Fail);
                }
            }
            None
        }
        _ => None,
    }
}

/// u-occurs: x = V[x]; e → fail if V ≠ □
fn u_occurs(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(v1, rhs), _rest) => {
            if let (Value::Var(x), Expr::Value(v2)) = (v1, &**rhs) {
                // Check if x occurs in v2 (in a non-trivial context)
                if let Some(ctx) = ValueContext::find_var_in_value(v2, x) {
                    if ctx != ValueContext::Hole {
                        // x occurs in v2, fail
                        return Some(Expr::Fail);
                    }
                }
            }
            None
        }
        _ => None,
    }
}

/// subst: X[x = v; e] → (X{v/x})[x = v; e{v/x}] if v ≠ V[x]
fn subst_rule(expr: &Expr) -> Option<Expr> {
    // Find all equations in the expression
    for (ctx, focused) in ExecContext::decompose(expr) {
        if let Expr::Seq(ExprOrEqn::Eqn(lhs, rhs), rest) = &focused {
            if let Value::Var(x) = lhs {
                // Check if RHS is a value (not just an expression)
                if let Expr::Value(v) = &**rhs {
                    // Check side condition: v ≠ V[x]
                    if let Some(value_ctx) = ValueContext::find_var_in_value(v, x) {
                        if value_ctx != ValueContext::Hole {
                            // Occurs check would fail, skip
                            continue;
                        }
                    }

                    // Apply substitution to the rest
                    let rest_subst = substitute(rest, x, v);

                    // Apply substitution to the context
                    let ctx_subst = substitute_in_context(&ctx, x, v);

                    // Rebuild: X{v/x}[x = v; rest{v/x}]
                    let inner = Expr::Seq(
                        ExprOrEqn::Eqn(lhs.clone(), rhs.clone()),
                        Box::new(rest_subst),
                    );
                    let result = ctx_subst.fill(inner);

                    // Only return if we actually changed something
                    if result != *expr {
                        return Some(result);
                    }
                }
            }
        }
    }
    None
}

/// Helper: substitute in an execution context
fn substitute_in_context(ctx: &ExecContext, x: &Var, v: &Value) -> ExecContext {
    match ctx {
        ExecContext::Hole => ExecContext::Hole,
        ExecContext::EqnLeft(lhs, inner_ctx, rest) => {
            ExecContext::EqnLeft(
                substitute_value(lhs, x, v),
                Box::new(substitute_in_context(inner_ctx, x, v)),
                Box::new(substitute(rest, x, v)),
            )
        }
        ExecContext::SeqLeft(inner_ctx, rest) => {
            ExecContext::SeqLeft(
                Box::new(substitute_in_context(inner_ctx, x, v)),
                Box::new(substitute(rest, x, v)),
            )
        }
        ExecContext::SeqRight(eq, inner_ctx) => {
            let eq_subst = match eq {
                ExprOrEqn::Expr(e) => ExprOrEqn::Expr(Box::new(substitute(e, x, v))),
                ExprOrEqn::Eqn(lhs, rhs) => ExprOrEqn::Eqn(
                    substitute_value(lhs, x, v),
                    Box::new(substitute(rhs, x, v)),
                ),
            };
            ExecContext::SeqRight(eq_subst, Box::new(substitute_in_context(inner_ctx, x, v)))
        }
    }
}

/// Helper: substitute in a value
fn substitute_value(val: &Value, x: &Var, v: &Value) -> Value {
    match val {
        Value::Var(y) if x == y => v.clone(),
        _ => val.clone(),
    }
}

/// hnf-swap: hnf = v; e → v = hnf; e
fn hnf_swap(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Seq(ExprOrEqn::Eqn(lhs, rhs), rest) => {
            if lhs.is_hnf() && !rhs.is_value() {
                // LHS is hnf, RHS is not a value - can't swap
                return None;
            }
            if lhs.is_hnf() {
                if let Expr::Value(v) = &**rhs {
                    if v.is_var() {
                        // Swap: v = hnf
                        return Some(Expr::Seq(
                            ExprOrEqn::Eqn(v.clone(), Box::new(Expr::Value(lhs.clone()))),
                            rest.clone(),
                        ));
                    }
                }
            }
            None
        }
        _ => None,
    }
}
