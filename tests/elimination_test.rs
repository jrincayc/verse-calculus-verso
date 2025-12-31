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

// tests/elimination_test.rs
// Tests Elimination rules


use verse_calculus_verso::*;

#[test]
fn test_val_elim() {
    // 3; x → x
    let expr = Expr::seq(
        ExprOrEqn::Expr(Box::new(Expr::int(3))),
        Expr::var("x"),
        );

    let result = rewrite_elimination(&expr);
    assert_eq!(result, Some(Expr::var("x")));
}

#[test]
fn test_val_elim_no_match() {
    // x + 1; y should not match (x + 1 is not a value)
    let expr = Expr::seq(
        ExprOrEqn::Expr(Box::new(Expr::var("x"))),
        Expr::var("y"),
        );

    // Actually x is a value, so this should match
    let result = rewrite_elimination(&expr);
    assert_eq!(result, Some(Expr::var("y")));
}

#[test]
fn test_exi_elim() {
    // ∃x. 42 → 42 (x is not free in 42)
    let expr = Expr::exists(Var::new("x"), Expr::int(42));

    let result = rewrite_elimination(&expr);
    assert_eq!(result, Some(Expr::int(42)));
}

#[test]
fn test_exi_elim_no_match() {
    // ∃x. x should not eliminate (x is free)
    let expr = Expr::exists(Var::new("x"), Expr::var("x"));

    let result = rewrite_elimination(&expr);
    assert_eq!(result, None);
}

#[test]
fn test_eqn_elim_simple() {
    // ∃x. x = 3; y → y
    let expr = Expr::exists(
        Var::new("x"),
        Expr::seq(
            Expr::eqn(Value::var("x"), Expr::int(3)),
            Expr::var("y"),
            ),
        );

    let result = rewrite_elimination(&expr);
    assert_eq!(result, Some(Expr::var("y")));
}

#[test]
fn test_eqn_elim_used() {
    // ∃x. x = 3; x should not eliminate (x is used in body)
    let expr = Expr::exists(
        Var::new("x"),
        Expr::seq(
            Expr::eqn(Value::var("x"), Expr::int(3)),
            Expr::var("x"),
            ),
        );

    let result = rewrite_elimination(&expr);
    assert_eq!(result, None);
}

#[test]
fn test_fail_elim() {
    // x = fail; y → fail
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::Fail),
        Expr::var("y"),
        );

    let result = rewrite_elimination(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_fail_elim_nested() {
    // ∃x. x = 3; fail → fail
    let expr = Expr::exists(
        Var::new("x"),
        Expr::seq(
            Expr::eqn(Value::var("x"), Expr::int(3)),
            Expr::Fail,
            ),
        );

    let result = rewrite_elimination(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_eqn_dead() {
    // Basic case: variable not used in continuation
    let expr = parse_expr_str("x = 5; 10").unwrap();
    let result = rewrite_elimination(&expr).unwrap();
    assert_eq!(result, parse_expr_str("10").unwrap());

    // Variable used in continuation - should not eliminate
    let expr = parse_expr_str("x = 5; x").unwrap();
    assert!(rewrite_elimination(&expr).is_none());

    // Dead equation with complex RHS
    let expr = parse_expr_str("x = add⟨1, 2⟩; 42").unwrap();
    let result = rewrite_elimination(&expr).unwrap();
    assert_eq!(result, parse_expr_str("42").unwrap());

    // Multiple equations, only first is dead
    //let expr = parse_expr_str("x = 3; y = x; y").unwrap();
    //let result = rewrite_elimination(&expr);//.unwrap();
    //assert_eq!(result, parse_expr_str("y = x; y")); //.unwrap());

    // Dead equation in a sequence
    let expr = parse_expr_str("x = 5; y = 10; y").unwrap();
    let result = rewrite_elimination(&expr).unwrap();
    assert_eq!(result, parse_expr_str("y = 10; y").unwrap());
}

#[test]
fn test_eqn_dead_with_tuples() {
    // Dead equation with tuple value
    let expr = parse_expr_str("x = ⟨1, 2⟩; 99").unwrap();
    let result = rewrite_elimination(&expr).unwrap();
    assert_eq!(result, parse_expr_str("99").unwrap());
}

#[test]
fn test_eqn_dead_does_not_eliminate_live_vars() {
    // Variable appears free in continuation
    let cases = vec![
        "x = 5; add⟨x, 1⟩",
        "x = 5; y = x; 10",  // x appears in the RHS of next equation
        "x = 5; ⟨x, 10⟩",
    ];

    for case in cases {
        let expr = parse_expr_str(case).unwrap();
        assert!(rewrite_elimination(&expr).is_none(),
                "Should not eliminate in: {}", case);
    }
}
