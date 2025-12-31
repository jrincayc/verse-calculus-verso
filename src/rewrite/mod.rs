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

// src/rewrite/mod.rs
// Rewrite rules for Verse Calculus

pub mod application;
pub mod elimination;
pub mod unification;
pub mod normalization;
pub mod choice;

pub use application::*;
pub use elimination::*;
pub use unification::*;
pub use normalization::*;
pub use choice::*;

use crate::ast::Expr;

/// Try to apply any rewrite rule (one step)
/// This tries to rewrite at the top level AND recursively in subexpressions
pub fn rewrite_step(expr: &Expr) -> Option<Expr> {
    // First try to rewrite at the top level
    if let Some(result) = rewrite_top_level(expr) {
        return Some(result);
    }

    // If no top-level rewrite, try to rewrite subexpressions
    rewrite_subexpr(expr)
}

/// Try to apply rules at the top level only
fn rewrite_top_level(expr: &Expr) -> Option<Expr> {
    //println!("trying rewrite_application");
    rewrite_application(expr)
        .or_else(|| {/* println!("trying rewrite_unification"); */ rewrite_unification(expr)})
        .or_else(|| {/* println!("trying rewrite_elimination"); */  rewrite_elimination(expr)})
        .or_else(|| {/* println!("trying rewrite_normalization"); */ rewrite_normalization(expr)})
        .or_else(|| {/* println!("trying rewrite_choice"); */ rewrite_choice(expr)})
}

/// Try to rewrite subexpressions recursively
fn rewrite_subexpr(expr: &Expr) -> Option<Expr> {
    match expr {
        Expr::Value(_) | Expr::Fail => None,

        Expr::Seq(eq, e) => {
            // Try rewriting the equation part
            match eq {
                crate::ast::ExprOrEqn::Expr(eq_expr) => {
                    if let Some(new_eq) = rewrite_step(eq_expr) {
                        return Some(Expr::Seq(
                            crate::ast::ExprOrEqn::Expr(Box::new(new_eq)),
                            e.clone(),
                        ));
                    }
                }
                crate::ast::ExprOrEqn::Eqn(v, rhs) => {
                    if let Some(new_rhs) = rewrite_step(rhs) {
                        return Some(Expr::Seq(
                            crate::ast::ExprOrEqn::Eqn(v.clone(), Box::new(new_rhs)),
                            e.clone(),
                        ));
                    }
                }
            }

            // Try rewriting the continuation
            if let Some(new_e) = rewrite_step(e) {
                return Some(Expr::Seq(eq.clone(), Box::new(new_e)));
            }

            None
        }

        Expr::Exists(x, e) => {
            if let Some(new_e) = rewrite_step(e) {
                Some(Expr::Exists(x.clone(), Box::new(new_e)))
            } else {
                None
            }
        }

        Expr::Choice(e1, e2) => {
            if let Some(new_e1) = rewrite_step(e1) {
                Some(Expr::Choice(Box::new(new_e1), e2.clone()))
            } else if let Some(new_e2) = rewrite_step(e2) {
                Some(Expr::Choice(e1.clone(), Box::new(new_e2)))
            } else {
                None
            }
        }

        Expr::App(_, _) => None, // Already handled by top-level

        Expr::One(e) => {
            if let Some(new_e) = rewrite_step(e) {
                Some(Expr::One(Box::new(new_e)))
            } else {
                None
            }
        }

        Expr::All(e) => {
            if let Some(new_e) = rewrite_step(e) {
                Some(Expr::All(Box::new(new_e)))
            } else {
                None
            }
        }
    }
}
