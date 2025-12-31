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

// tests/unification_test.rs
// tests Unification rules: subst, u-lit, u-tup, u-fail, u-occurs, hnf-swap, var-swap, seq-swap

use verse_calculus_verso::*;

#[test]
fn test_u_lit_same() {
    // 3 = 3; x → x
    let expr = Expr::seq(
        Expr::eqn(Value::int(3), Expr::int(3)),
        Expr::var("x"),
        );

    let result = rewrite_unification(&expr);
    assert_eq!(result, Some(Expr::var("x")));
}

#[test]
fn test_u_fail_different_ints() {
    // 3 = 4; x → fail
    let expr = Expr::seq(
        Expr::eqn(Value::int(3), Expr::int(4)),
        Expr::var("x"),
        );

    let result = rewrite_unification(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_u_tup() {
    // ⟨1, 2⟩ = ⟨x, y⟩; z → 1 = x; 2 = y; z
    let expr = Expr::seq(
        Expr::eqn(
            Value::tuple(vec![Value::int(1), Value::int(2)]),
            Expr::tuple(vec![Value::var("x"), Value::var("y")]),
            ),
        Expr::var("z"),
        );

    let result = rewrite_unification(&expr);
    assert!(result.is_some());

    // Should produce nested sequences
    let result = result.unwrap();
    match result {
        Expr::Seq(ExprOrEqn::Eqn(v1, e1), rest) => {
            assert_eq!(v1, Value::int(1));
            assert_eq!(*e1, Expr::var("x"));
            // Continue checking nested structure...
        }
        _ => panic!("Expected sequence"),
    }
}

#[test]
fn test_u_occurs() {
    // x = ⟨1, x⟩; y → fail
    let expr = Expr::seq(
        Expr::eqn(
            Value::var("x"),
            Expr::tuple(vec![Value::int(1), Value::var("x")]),
            ),
        Expr::var("y"),
        );

    let result = rewrite_unification(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_subst_simple() {
    // x = 3; x → x = 3; 3
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::var("x"),
        );

    let result = rewrite_unification(&expr);
    assert!(result.is_some());

    let result = result.unwrap();
    match result {
        Expr::Seq(ExprOrEqn::Eqn(_, _), rest) => {
            assert_eq!(*rest, Expr::int(3));
        }
        _ => panic!("Expected sequence"),
    }
}

#[test]
fn test_hnf_swap() {
    // 3 = x; y → x = 3; y
    let expr = Expr::seq(
        Expr::eqn(Value::int(3), Expr::var("x")),
        Expr::var("y"),
        );

    let result = rewrite_unification(&expr);
    assert!(result.is_some());

    let result = result.unwrap();
    match result {
        Expr::Seq(ExprOrEqn::Eqn(lhs, rhs), _) => {
            assert_eq!(lhs, Value::var("x"));
            assert_eq!(*rhs, Expr::int(3));
        }
        _ => panic!("Expected sequence"),
    }
}
