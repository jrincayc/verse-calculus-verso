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

// tests/application_test.rs
// Tests the application rewrites


use verse_calculus_verso::*;

#[test]
fn test_app_add() {
    // add⟨3, 4⟩ → 7
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Op(PrimOp::Add)),
        Value::tuple(vec![Value::int(3), Value::int(4)]),
        );

    let result = rewrite_application(&expr);
    assert_eq!(result, Some(Expr::int(7)));
}

#[test]
fn test_app_add_negative() {
    // add⟨-5, 3⟩ → -2
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Op(PrimOp::Add)),
        Value::tuple(vec![Value::int(-5), Value::int(3)]),
        );

    let result = rewrite_application(&expr);
    assert_eq!(result, Some(Expr::int(-2)));
}

#[test]
fn test_app_gt_success() {
    // gt⟨5, 3⟩ → 5
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Op(PrimOp::Gt)),
        Value::tuple(vec![Value::int(5), Value::int(3)]),
        );

    let result = rewrite_application(&expr);
    assert_eq!(result, Some(Expr::int(5)));
}

#[test]
fn test_app_gt_fail() {
    // gt⟨3, 5⟩ → fail
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Op(PrimOp::Gt)),
        Value::tuple(vec![Value::int(3), Value::int(5)]),
        );

    let result = rewrite_application(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_app_gt_equal_fail() {
    // gt⟨3, 3⟩ → fail
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Op(PrimOp::Gt)),
        Value::tuple(vec![Value::int(3), Value::int(3)]),
        );

    let result = rewrite_application(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_app_beta_simple() {
    // (λx. x)(3) → ∃x. x = 3; x
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Lambda(Var::new("x"), Box::new(Expr::var("x")))),
        Value::int(3),
        );

    let result = rewrite_application(&expr);
    assert!(result.is_some());

    // Result should be ∃x. x = 3; x
    match result.unwrap() {
        Expr::Exists(x, e) => {
            assert_eq!(x.0, "x");
            match *e {
                Expr::Seq(ExprOrEqn::Eqn(v, rhs), body) => {
                    assert_eq!(v, Value::var("x"));
                    assert_eq!(*rhs, Expr::int(3));
                    assert_eq!(*body, Expr::var("x"));
                }
                _ => panic!("Expected sequence"),
            }
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_app_beta_complex() {
    // (λx. x + 1)(5) → ∃x. x = 5; x + 1
    // Note: We don't evaluate the body, just perform beta reduction
    let body = Expr::var("x"); // Simplified for test
    let expr = Expr::app(
        Value::Hnf(HeadNormalForm::Lambda(Var::new("x"), Box::new(body))),
        Value::int(5),
        );

    let result = rewrite_application(&expr);
    assert!(result.is_some());
}

#[test]
fn test_app_tup_0() {
    // ⟨⟩(5) → fail
    let expr = Expr::app(Value::tuple(vec![]), Value::int(5));

    let result = rewrite_application(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_app_tup_single() {
    // ⟨42⟩(v) → ∃x. x = v; (x = 0; 42)
    let expr = Expr::app(Value::tuple(vec![Value::int(42)]), Value::var("i"));

    let result = rewrite_application(&expr);
    assert!(result.is_some());

    // Should create choice structure with single element
    match result.unwrap() {
        Expr::Exists(_, e) => {
            match *e {
                Expr::Seq(ExprOrEqn::Eqn(_, _), body) => {
                    // Body should be the single element case
                    match *body {
                        Expr::Seq(ExprOrEqn::Eqn(_, idx), val) => {
                            assert_eq!(*idx, Expr::int(0));
                            assert_eq!(*val, Expr::int(42));
                        }
                        _ => panic!("Expected sequence for index check"),
                    }
                }
                _ => panic!("Expected sequence"),
            }
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_app_tup_multiple() {
    // ⟨10, 20, 30⟩(v) → ∃x. x = v; (x = 0; 10) ⊕ (x = 1; 20) ⊕ (x = 2; 30)
    let expr = Expr::app(
        Value::tuple(vec![Value::int(10), Value::int(20), Value::int(30)]),
        Value::var("i"),
        );

    let result = rewrite_application(&expr);
    assert!(result.is_some());

    // Should create choice structure
    match result.unwrap() {
        Expr::Exists(x, e) => {
            // Has binding x = v
            match *e {
                Expr::Seq(ExprOrEqn::Eqn(_, _), body) => {
                    // Body should be a choice tree
                    assert!(matches!(*body, Expr::Choice(_, _) | Expr::Seq(_, _)));
                }
                _ => panic!("Expected sequence"),
            }
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_no_rewrite_non_app() {
    // Should return None for non-application
    let expr = Expr::int(42);
    assert_eq!(rewrite_application(&expr), None);

    let expr = Expr::Fail;
    assert_eq!(rewrite_application(&expr), None);
}
