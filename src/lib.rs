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

// src/lib.rs
// Verse Calculus library

pub mod ast;
pub mod syntax;
pub mod core;
pub mod rewrite;

// Re-export commonly used items
pub use ast::{Expr, Value, Var, HeadNormalForm, ExprOrEqn, Program, PrimOp};
pub use syntax::{parse_expr_str, parse_extended_str};
pub use core::subst::{substitute, alpha_convert, freshen};
pub use core::context::{
    ExecContext, ValueContext, ChoiceContext, ScopeContext,
    find_subexpr, find_first_subexpr,
};
pub use rewrite::{
    rewrite_application, rewrite_elimination, rewrite_unification,
    rewrite_normalization, rewrite_choice, rewrite_step
};
