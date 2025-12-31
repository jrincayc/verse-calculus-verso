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

// test/subst_test.rs
// Tests Capture-avoiding substitution and alpha-conversion for Verse Calculus

use verse_calculus_verso::*;
use crate::core::subst::would_capture;

#[test]
fn test_simple_substitution() {
    // x{3/x} = 3
    let expr = Expr::var("x");
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::int(3));
}

#[test]
fn test_no_substitution() {
    // y{3/x} = y
    let expr = Expr::var("y");
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::var("y"));
}

#[test]
fn test_substitution_in_tuple() {
    // ⟨x, y, x⟩{3/x} = ⟨3, y, 3⟩
    let expr = Expr::tuple(vec![
        Value::var("x"),
        Value::var("y"),
        Value::var("x"),
        ]);
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(
        result,
        Expr::tuple(vec![Value::int(3), Value::var("y"), Value::int(3)])
            );
}

#[test]
fn test_substitution_shadowed_by_exists() {
    // (∃x. x){3/x} = ∃x. x (x is shadowed)
    let expr = Expr::exists(Var::new("x"), Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::exists(Var::new("x"), Expr::var("x")));
}

#[test]
fn test_substitution_not_shadowed() {
    // (∃y. x){3/x} = ∃y. 3
    let expr = Expr::exists(Var::new("y"), Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::exists(Var::new("y"), Expr::int(3)));
}

#[test]
fn test_substitution_with_capture_avoidance() {
    // (∃y. x){y/x} should rename y to avoid capture
    let expr = Expr::exists(Var::new("y"), Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::var("y"));

    // Result should be ∃y_N. y for some fresh y_N
    match result {
        Expr::Exists(y_new, e) => {
            assert_ne!(y_new.0, "y"); // Should be renamed
            assert!(y_new.0.starts_with("y_")); // Should be a fresh variant
            assert_eq!(*e, Expr::var("y")); // Body should have y substituted
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_substitution_in_lambda() {
    // (λy. x){3/x} = λy. 3
    let expr = Expr::lambda(Var::new("y"), Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::lambda(Var::new("y"), Expr::int(3)));
}

#[test]
fn test_substitution_lambda_shadowing() {
    // (λx. x){3/x} = λx. x (x is shadowed)
    let expr = Expr::lambda(Var::new("x"), Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::lambda(Var::new("x"), Expr::var("x")));
}

#[test]
fn test_substitution_lambda_capture_avoidance() {
    // (λy. x){y/x} should rename y to avoid capture
    let expr = Expr::lambda(Var::new("y"), Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::var("y"));

    match result {
        Expr::Value(Value::Hnf(HeadNormalForm::Lambda(y_new, e))) => {
            assert_ne!(y_new.0, "y"); // Should be renamed
            assert!(y_new.0.starts_with("y_")); // Should be fresh
            assert_eq!(*e, Expr::var("y")); // Body should have y substituted
        }
        _ => panic!("Expected Lambda"),
    }
}

#[test]
fn test_substitution_in_sequence() {
    // (x = 3; x + 1){y/x} = y = 3; y + 1
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::var("x"),
        );
    let result = substitute(&expr, &Var::new("x"), &Value::var("y"));

    let expected = Expr::seq(
        Expr::eqn(Value::var("y"), Expr::int(3)),
        Expr::var("y"),
        );
    assert_eq!(result, expected);
}

#[test]
fn test_substitution_in_choice() {
    // (x ⊕ y){3/x} = 3 ⊕ y
    let expr = Expr::choice(Expr::var("x"), Expr::var("y"));
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::choice(Expr::int(3), Expr::var("y")));
}

#[test]
fn test_substitution_in_one() {
    // one{x}{3/x} = one{3}
    let expr = Expr::one(Expr::var("x"));
    let result = substitute(&expr, &Var::new("x"), &Value::int(3));
    assert_eq!(result, Expr::one(Expr::int(3)));
}

#[test]
fn test_would_capture() {
    // Check if (∃y. x){y/x} would capture y
    let expr = Expr::exists(Var::new("y"), Expr::var("x"));
    assert!(would_capture(&expr, &Var::new("x"), &Value::var("y")));

    // Check if (∃y. x){z/x} would NOT capture
    assert!(!would_capture(&expr, &Var::new("x"), &Value::var("z")));
}

#[test]
fn test_paper_example() {
    // From the paper: ∃x y. y = 3 + x; ∃z. z = x + 1; z
    // Substitute x with 7
    let expr = Expr::exists(
        Var::new("x"),
        Expr::exists(
            Var::new("y"),
            Expr::seq(
                Expr::eqn(Value::var("y"), Expr::var("x")), // Simplified from 3 + x
                Expr::exists(
                    Var::new("z"),
                    Expr::seq(
                        Expr::eqn(Value::var("z"), Expr::var("x")), // Simplified from x + 1
                        Expr::var("z"),
                        ),
                    ),
                ),
            ),
        );

    let result = substitute(&expr, &Var::new("x"), &Value::int(7));

    // After substitution, all x's should be 7, but x is shadowed by ∃x
    // So nothing should change
    assert_eq!(result, expr);
}
