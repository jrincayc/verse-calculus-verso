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

// tests/normalization_test.rs
// Tests Normalization rules: exi-float, seq-assoc, eqn-float, exi-swap

use verse_calculus_verso::*;

#[test]
fn test_exi_float_through_seq_right() {
    // x = 3; ∃y. y → ∃y. x = 3; y (y not free in x = 3)
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::exists(Var::new("y"), Expr::var("y")),
        );

    let result = rewrite_normalization(&expr);
    assert!(result.is_some());

    match result.unwrap() {
        Expr::Exists(y, body) => {
            assert_eq!(y.0, "y");
            match *body {
                Expr::Seq(_, _) => {} // Good
                _ => panic!("Expected sequence in body"),
            }
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_exi_float_blocked_by_free_var() {
    // x = 3; ∃x. x should NOT float (x is free in equation)
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::exists(Var::new("x"), Expr::var("x")),
        );

    let result = rewrite_normalization(&expr);
    // Should not float because x is used in the equation
    assert_eq!(result, None);
}

#[test]
#[ignore] //XXX bogus
fn test_exi_float_through_one() {
    // one{∃x. x} → ∃x. one{x}
    let expr = Expr::one(Expr::exists(Var::new("x"), Expr::var("x")));

    let result = rewrite_normalization(&expr);
    assert!(result.is_some());

    match result.unwrap() {
        Expr::Exists(x, body) => {
            assert_eq!(x.0, "x");
            match *body {
                Expr::One(_) => {} // Good
                _ => panic!("Expected One in body"),
            }
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_seq_assoc() {
    // (x; y); z → x; (y; z)
    let inner_seq = Expr::seq(
        ExprOrEqn::Expr(Box::new(Expr::var("x"))),
        Expr::var("y"),
        );
    let expr = Expr::seq(
        ExprOrEqn::Expr(Box::new(inner_seq)),
        Expr::var("z"),
        );

    let result = rewrite_normalization(&expr);
    assert!(result.is_some());

    // Should produce x; (y; z)
    match result.unwrap() {
        Expr::Seq(ExprOrEqn::Expr(e1), rest) => {
            assert_eq!(*e1, Expr::var("x"));
            match *rest {
                Expr::Seq(_, _) => {} // y; z
                _ => panic!("Expected nested sequence"),
            }
        }
        _ => panic!("Expected sequence"),
    }
}

#[test]
fn test_eqn_float() {
    // x = (y = 3; y); z → y = 3; (x = y; z)
    let rhs = Expr::seq(
        Expr::eqn(Value::var("y"), Expr::int(3)),
        Expr::var("y"),
        );
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), rhs),
        Expr::var("z"),
        );

    let result = rewrite_normalization(&expr);
    assert!(result.is_some());

    // Should float y = 3 out
    match result.unwrap() {
        Expr::Seq(ExprOrEqn::Eqn(v, _), rest) => {
            assert_eq!(v, Value::var("y"));
            // Rest should be x = y; z
            match *rest {
                Expr::Seq(ExprOrEqn::Eqn(x, _), _) => {
                    assert_eq!(x, Value::var("x"));
                }
                _ => panic!("Expected nested equation"),
            }
        }
        _ => panic!("Expected sequence with equation"),
    }
}

#[test]
fn test_exi_swap() {
    // ∃x. ∃y. e → ∃y. ∃x. e (if x > y lexicographically)
    let expr = Expr::exists(
        Var::new("y"),
        Expr::exists(Var::new("x"), Expr::var("x")),
        );

    let result = rewrite_normalization(&expr);
    assert!(result.is_some());

    match result.unwrap() {
        Expr::Exists(outer, inner_box) => {
            assert_eq!(outer.0, "x");
            match *inner_box {
                Expr::Exists(inner, _) => {
                    assert_eq!(inner.0, "y");
                }
                _ => panic!("Expected inner Exists"),
            }
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_exi_swap_no_swap_if_ordered() {
    // ∃x. ∃y. e should not swap (already ordered)
    let expr = Expr::exists(
        Var::new("x"),
        Expr::exists(Var::new("y"), Expr::var("y")),
        );

    let result = rewrite_normalization(&expr);
    // Should not swap since x < y
    assert_eq!(result, None);
}
