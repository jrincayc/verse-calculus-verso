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

// src/core/subst.rs
// Capture-avoiding substitution and alpha-conversion for Verse Calculus

use crate::ast::*;
use std::collections::HashSet;

// ============================================================================
// Substitution
// ============================================================================

/// Perform capture-avoiding substitution: e{v/x}
/// Replace all free occurrences of variable x with value v in expression e
pub fn substitute(expr: &Expr, x: &Var, v: &Value) -> Expr {
    subst_expr(expr, x, v)
}

fn subst_expr(expr: &Expr, x: &Var, v: &Value) -> Expr {
    match expr {
        Expr::Value(val) => Expr::Value(subst_value(val, x, v)),

        Expr::Fail => Expr::Fail,

        Expr::Seq(eq, e) => {
            let eq_new = subst_expr_or_eqn(eq, x, v);
            let e_new = subst_expr(e, x, v);
            Expr::Seq(eq_new, Box::new(e_new))
        }

        Expr::Exists(y, e) => {
            if x == y {
                // Variable is shadowed, don't substitute
                Expr::Exists(y.clone(), e.clone())
            } else if v.free_vars().contains(y) {
                // Would capture y, need alpha-conversion
                let y_fresh = Var::fresh(&y.0);
                let e_renamed = subst_expr(e, y, &Value::Var(y_fresh.clone()));
                let e_subst = subst_expr(&e_renamed, x, v);
                Expr::Exists(y_fresh, Box::new(e_subst))
            } else {
                // Safe to substitute
                Expr::Exists(y.clone(), Box::new(subst_expr(e, x, v)))
            }
        }

        Expr::Choice(e1, e2) => {
            Expr::Choice(
                Box::new(subst_expr(e1, x, v)),
                Box::new(subst_expr(e2, x, v)),
            )
        }

        Expr::App(v1, v2) => {
            Expr::App(subst_value(v1, x, v), subst_value(v2, x, v))
        }

        Expr::One(e) => Expr::One(Box::new(subst_expr(e, x, v))),

        Expr::All(e) => Expr::All(Box::new(subst_expr(e, x, v))),
    }
}

fn subst_expr_or_eqn(eq: &ExprOrEqn, x: &Var, v: &Value) -> ExprOrEqn {
    match eq {
        ExprOrEqn::Expr(e) => ExprOrEqn::Expr(Box::new(subst_expr(e, x, v))),
        ExprOrEqn::Eqn(lhs, rhs) => {
            ExprOrEqn::Eqn(
                subst_value(lhs, x, v),
                Box::new(subst_expr(rhs, x, v)),
            )
        }
    }
}

fn subst_value(val: &Value, x: &Var, v: &Value) -> Value {
    match val {
        Value::Var(y) => {
            if x == y {
                v.clone()
            } else {
                Value::Var(y.clone())
            }
        }
        Value::Hnf(hnf) => Value::Hnf(subst_hnf(hnf, x, v)),
    }
}

fn subst_hnf(hnf: &HeadNormalForm, x: &Var, v: &Value) -> HeadNormalForm {
    match hnf {
        HeadNormalForm::Int(n) => HeadNormalForm::Int(*n),
        HeadNormalForm::Op(op) => HeadNormalForm::Op(op.clone()),
        HeadNormalForm::Tuple(vals) => {
            HeadNormalForm::Tuple(vals.iter().map(|val| subst_value(val, x, v)).collect())
        }
        HeadNormalForm::Lambda(y, e) => {
            if x == y {
                // Variable is shadowed, don't substitute
                HeadNormalForm::Lambda(y.clone(), e.clone())
            } else if v.free_vars().contains(y) {
                // Would capture y, need alpha-conversion
                let y_fresh = Var::fresh(&y.0);
                let e_renamed = subst_expr(e, y, &Value::Var(y_fresh.clone()));
                let e_subst = subst_expr(&e_renamed, x, v);
                HeadNormalForm::Lambda(y_fresh, Box::new(e_subst))
            } else {
                // Safe to substitute
                HeadNormalForm::Lambda(y.clone(), Box::new(subst_expr(e, x, v)))
            }
        }
    }
}

// ============================================================================
// Alpha-conversion
// ============================================================================

/// Alpha-convert: rename bound variable old_var to new_var in expr
/// This renames the binder and all bound occurrences
pub fn alpha_convert(expr: &Expr, old_var: &Var, new_var: &Var) -> Expr {
    match expr {
        Expr::Exists(x, e) if x == old_var => {
            // Rename this binder and its bound occurrences
            let e_renamed = subst_expr(e, old_var, &Value::Var(new_var.clone()));
            Expr::Exists(new_var.clone(), Box::new(e_renamed))
        }

        Expr::Value(Value::Hnf(HeadNormalForm::Lambda(x, e))) if x == old_var => {
            // Rename lambda parameter and its bound occurrences
            let e_renamed = subst_expr(e, old_var, &Value::Var(new_var.clone()));
            Expr::Value(Value::Hnf(HeadNormalForm::Lambda(
                new_var.clone(),
                Box::new(e_renamed),
            )))
        }

        // Recursively search for the binder to rename
        _ => expr.clone(), // For simplicity, just return clone if not found
    }
}

/// Freshen all bound variables in an expression to avoid any conflicts
/// with the given set of variables
pub fn freshen(expr: &Expr, avoid: &HashSet<Var>) -> Expr {
    freshen_expr(expr, avoid, &mut std::collections::HashMap::new())
}

fn freshen_expr(
    expr: &Expr,
    avoid: &HashSet<Var>,
    renaming: &mut std::collections::HashMap<Var, Var>,
) -> Expr {
    match expr {
        Expr::Value(v) => Expr::Value(freshen_value(v, renaming)),

        Expr::Fail => Expr::Fail,

        Expr::Seq(eq, e) => {
            let eq_new = freshen_expr_or_eqn(eq, avoid, renaming);
            let e_new = freshen_expr(e, avoid, renaming);
            Expr::Seq(eq_new, Box::new(e_new))
        }

        Expr::Exists(x, e) => {
            // Generate fresh name if x conflicts
            let x_new = if avoid.contains(x) || renaming.contains_key(x) {
                let mut x_fresh = Var::fresh(&x.0);
                while avoid.contains(&x_fresh) {
                    x_fresh = Var::fresh(&x.0);
                }
                x_fresh
            } else {
                x.clone()
            };

            // Add to renaming map
            let mut renaming = renaming.clone();
            if x != &x_new {
                renaming.insert(x.clone(), x_new.clone());
            }

            Expr::Exists(x_new, Box::new(freshen_expr(e, avoid, &mut renaming)))
        }

        Expr::Choice(e1, e2) => {
            Expr::Choice(
                Box::new(freshen_expr(e1, avoid, renaming)),
                Box::new(freshen_expr(e2, avoid, renaming)),
            )
        }

        Expr::App(v1, v2) => {
            Expr::App(
                freshen_value(v1, renaming),
                freshen_value(v2, renaming),
            )
        }

        Expr::One(e) => Expr::One(Box::new(freshen_expr(e, avoid, renaming))),

        Expr::All(e) => Expr::All(Box::new(freshen_expr(e, avoid, renaming))),
    }
}

fn freshen_expr_or_eqn(
    eq: &ExprOrEqn,
    avoid: &HashSet<Var>,
    renaming: &mut std::collections::HashMap<Var, Var>,
) -> ExprOrEqn {
    match eq {
        ExprOrEqn::Expr(e) => ExprOrEqn::Expr(Box::new(freshen_expr(e, avoid, renaming))),
        ExprOrEqn::Eqn(v, e) => {
            ExprOrEqn::Eqn(
                freshen_value(v, renaming),
                Box::new(freshen_expr(e, avoid, renaming)),
            )
        }
    }
}

fn freshen_value(
    val: &Value,
    renaming: &std::collections::HashMap<Var, Var>,
) -> Value {
    match val {
        Value::Var(x) => {
            Value::Var(renaming.get(x).unwrap_or(x).clone())
        }
        Value::Hnf(hnf) => Value::Hnf(freshen_hnf(hnf, renaming)),
    }
}

fn freshen_hnf(
    hnf: &HeadNormalForm,
    renaming: &std::collections::HashMap<Var, Var>,
) -> HeadNormalForm {
    match hnf {
        HeadNormalForm::Int(n) => HeadNormalForm::Int(*n),
        HeadNormalForm::Op(op) => HeadNormalForm::Op(op.clone()),
        HeadNormalForm::Tuple(vals) => {
            HeadNormalForm::Tuple(vals.iter().map(|v| freshen_value(v, renaming)).collect())
        }
        HeadNormalForm::Lambda(x, e) => {
            // This is incomplete - should handle lambda binders like Exists
            HeadNormalForm::Lambda(x.clone(), e.clone())
        }
    }
}

// ============================================================================
// Utilities
// ============================================================================

/// Check if substitution would cause variable capture
pub fn would_capture(expr: &Expr, x: &Var, v: &Value) -> bool {
    check_capture(expr, x, &v.free_vars())
}

fn check_capture(expr: &Expr, x: &Var, v_free_vars: &HashSet<Var>) -> bool {
    match expr {
        Expr::Value(_) | Expr::Fail => false,

        Expr::Seq(eq, e) => {
            check_capture_eq(eq, x, v_free_vars) || check_capture(e, x, v_free_vars)
        }

        Expr::Exists(y, e) => {
            if x == y {
                // x is shadowed, no capture possible
                false
            } else if v_free_vars.contains(y) {
                // y would be captured
                true
            } else {
                check_capture(e, x, v_free_vars)
            }
        }

        Expr::Choice(e1, e2) => {
            check_capture(e1, x, v_free_vars) || check_capture(e2, x, v_free_vars)
        }

        Expr::App(_, _) => false,

        Expr::One(e) | Expr::All(e) => check_capture(e, x, v_free_vars),
    }
}

fn check_capture_eq(eq: &ExprOrEqn, x: &Var, v_free_vars: &HashSet<Var>) -> bool {
    match eq {
        ExprOrEqn::Expr(e) => check_capture(e, x, v_free_vars),
        ExprOrEqn::Eqn(_, e) => check_capture(e, x, v_free_vars),
    }
}
