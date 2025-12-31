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

// src/rewrite/choice.rs
// Choice rules: one-fail, one-value, one-choice, all-fail, all-value, all-choice,
//               choose-r, choose-l, choose-assoc, choose

use crate::ast::*;
use crate::core::context::{ScopeContext, ChoiceContext};

/// Apply one step of a choice rule if possible
pub fn rewrite_choice(expr: &Expr) -> Option<Expr> {

    one_fail(expr)
        .or_else(|| {/* println!("trying one_value"); */ one_value(expr)})
        .or_else(|| {/* println!("trying one_choice"); */ one_choice(expr)})
        .or_else(|| {/* println!("trying all_fail"); */ all_fail(expr)})
        .or_else(|| {/* println!("trying all_value"); */ all_value(expr)})
        .or_else(|| {/* println!("trying all_choice"); */ all_choice(expr)})
        .or_else(|| {/* println!("trying choose_r"); */ choose_r(expr)})
        .or_else(|| {/* println!("trying choose_l"); */ choose_l(expr)})
        .or_else(|| {/* println!("trying choose_assoc"); */ choose_assoc(expr)})
        .or_else(|| {/* println!("trying choose"); */ choose(expr)})
}

// ============================================================================
// One Rules
// ============================================================================

/// one-fail: one{fail} → fail
fn one_fail(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::One(e) if e.is_fail() => Some(Expr::Fail),
        _ => None,
    }
}

/// one-value: one{v} → v
fn one_value(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::One(e) if e.is_value() => Some((**e).clone()),
        _ => None,
    }
}

/// one-choice: one{v ⊕ e} → v
fn one_choice(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::One(body) => {
            if let Expr::Choice(e1, _e2) = &**body {
                if e1.is_value() {
                    return Some((**e1).clone());
                }
            }
            None
        }
        _ => None,
    }
}

// ============================================================================
// All Rules
// ============================================================================

/// all-fail: all{fail} → ⟨⟩
fn all_fail(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::All(e) if e.is_fail() => Some(Expr::empty_tuple()),
        _ => None,
    }
}

/// all-value: all{v} → ⟨v⟩
fn all_value(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::All(e) if e.is_value() => {
            if let Expr::Value(v) = &**e {
                Some(Expr::tuple(vec![v.clone()]))
            } else {
                None
            }
        }
        _ => None,
    }
}

/// all-choice: all{v₁ ⊕ ... ⊕ vₙ} → ⟨v₁, ..., vₙ⟩
/// Collects all values from a choice tree
fn all_choice(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::All(body) => {
            // Collect all values from the choice tree
            let values = collect_choice_values(body)?;
            if values.is_empty() {
                None // No values collected, not ready
            } else {
                Some(Expr::tuple(values))
            }
        }
        _ => None,
    }
}

/// Helper: collect all values from a right-associated choice tree
/// Returns Some(vec) if all branches are values, None otherwise
pub fn collect_choice_values(expr: &Expr) -> Option<Vec<Value>> {
    match expr {
        Expr::Value(v) => Some(vec![v.clone()]),
        Expr::Choice(e1, e2) => {
            let mut values = collect_choice_values(e1)?;
            values.extend(collect_choice_values(e2)?);
            Some(values)
        }
        _ => None,
    }
}

// ============================================================================
// Choice Simplification Rules
// ============================================================================

/// choose-r: fail ⊕ e → e
fn choose_r(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Choice(e1, e2) if e1.is_fail() => Some((**e2).clone()),
        _ => None,
    }
}

/// choose-l: e ⊕ fail → e
fn choose_l(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Choice(e1, e2) if e2.is_fail() => Some((**e1).clone()),
        _ => None,
    }
}

/// choose-assoc: (e₁ ⊕ e₂) ⊕ e₃ → e₁ ⊕ (e₂ ⊕ e₃)
fn choose_assoc(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Choice(e1, e3) => {
            if let Expr::Choice(e1_1, e1_2) = &**e1 {
                // Reassociate: e1_1 ⊕ (e1_2 ⊕ e3)
                return Some(Expr::Choice(
                    e1_1.clone(),
                    Box::new(Expr::Choice(e1_2.clone(), e3.clone())),
                ));
            }
            None
        }
        _ => None,
    }
}

/// choose: SX[CX[e₁ ⊕ e₂]] → SX[CX[e₁] ⊕ CX[e₂]]
/// Floats choice outward WITHIN the scope context, duplicating CX but not SX
/// Only fires when CX is non-trivial (not just a hole)
fn choose(expr: &Expr) -> Option<Expr> {
    match expr {
         Expr::One(body) => {
            if matches!(**body, Expr::Choice(_, _)) {
                return None;
            }

            let decomps = ChoiceContext::decompose(body);

            // Find the LAST (deepest) non-trivial context with a choice
            let mut result = None;
            for (cx, focused) in decomps {
                if let Expr::Choice(e1, e2) = focused {
                    if cx == ChoiceContext::Hole {
                        continue;
                    }

                    let branch1 = cx.fill(*e1);
                    let branch2 = cx.fill(*e2);
                    let new_body = Expr::Choice(Box::new(branch1), Box::new(branch2));
                    result = Some(Expr::One(Box::new(new_body)));
                    // Don't return yet, keep looking for deeper contexts
                }
            }
            result
        }

        Expr::All(body) => {
            // Don't apply if body is directly a choice at top level
            if matches!(**body, Expr::Choice(_, _)) {
                return None;
            }

            // Same for all
            let decomps = ChoiceContext::decompose(body);

            for (cx, focused) in decomps {
                if let Expr::Choice(e1, e2) = focused {
                    // Only apply if context is non-trivial
                    if cx == ChoiceContext::Hole {
                        continue;
                    }

                    let branch1 = cx.fill(*e1);
                    let branch2 = cx.fill(*e2);
                    let new_body = Expr::Choice(Box::new(branch1), Box::new(branch2));
                    return Some(Expr::All(Box::new(new_body)));
                }
            }
            None
        }

        _ => None,
    }
}
