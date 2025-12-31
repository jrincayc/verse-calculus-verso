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

// tests/context_test.rs
// Test Context matching and manipulation for Verse Calculus

use verse_calculus_verso::*;

#[test]
fn test_exec_context_hole() {
    let ctx = ExecContext::hole();
    let expr = Expr::int(42);
    assert_eq!(ctx.fill(expr.clone()), expr);
}

#[test]
fn test_exec_context_seq_left() {
    // X; y where X is filled with 42
    let ctx = ExecContext::SeqLeft(
        Box::new(ExecContext::Hole),
        Box::new(Expr::var("y")),
        );
    let result = ctx.fill(Expr::int(42));

    let expected = Expr::Seq(
        ExprOrEqn::Expr(Box::new(Expr::int(42))),
        Box::new(Expr::var("y")),
        );
    assert_eq!(result, expected);
}

#[test]
fn test_exec_context_eqn_left() {
    // x = X; y where X is filled with 42
    let ctx = ExecContext::EqnLeft(
        Value::var("x"),
        Box::new(ExecContext::Hole),
        Box::new(Expr::var("y")),
        );
    let result = ctx.fill(Expr::int(42));

    let expected = Expr::Seq(
        ExprOrEqn::Eqn(Value::var("x"), Box::new(Expr::int(42))),
        Box::new(Expr::var("y")),
        );
    assert_eq!(result, expected);
}

#[test]
fn test_exec_context_decompose_simple() {
    let expr = Expr::int(42);
    let decompositions = ExecContext::decompose(&expr);

    assert_eq!(decompositions.len(), 1);
    assert_eq!(decompositions[0].0, ExecContext::Hole);
    assert_eq!(decompositions[0].1, expr);
}

#[test]
fn test_exec_context_decompose_sequence() {
    // x = 3; y
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::var("y"),
        );

    let decompositions = ExecContext::decompose(&expr);

    // Should find: whole expr, RHS of equation (3), and continuation (y)
    assert!(decompositions.len() >= 3);

    // Check we can reconstruct
    for (ctx, focused) in &decompositions {
        assert_eq!(ctx.fill(focused.clone()), expr);
    }
}

#[test]
fn test_exec_context_find_equations() {
    // x = 3; y = 4; x
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::seq(
            Expr::eqn(Value::var("y"), Expr::int(4)),
            Expr::var("x"),
            ),
        );

    let eqns = ExecContext::hole().find_equations(&expr, &Var::new("x"));
    assert_eq!(eqns.len(), 1);

    let eqns = ExecContext::hole().find_equations(&expr, &Var::new("y"));
    assert_eq!(eqns.len(), 1);
}

#[test]
fn test_value_context_hole() {
    let ctx = ValueContext::hole();
    let val = Value::int(42);
    assert_eq!(ctx.fill(val.clone()), val);
}

#[test]
fn test_value_context_tuple() {
    // ⟨1, V, 3⟩ where V is filled with 2
    let ctx = ValueContext::Tuple(
        vec![Value::int(1)],
        Box::new(ValueContext::Hole),
        vec![Value::int(3)],
        );

    let result = ctx.fill(Value::int(2));
    let expected = Value::tuple(vec![Value::int(1), Value::int(2), Value::int(3)]);
    assert_eq!(result, expected);
}

#[test]
fn test_value_context_find_var() {
    // ⟨1, x, 3⟩ - find x
    let val = Value::tuple(vec![Value::int(1), Value::var("x"), Value::int(3)]);
    let ctx = ValueContext::find_var_in_value(&val, &Var::new("x"));

    assert!(ctx.is_some());
    if let Some(ctx) = ctx {
        // Fill with y should give ⟨1, y, 3⟩
        let result = ctx.fill(Value::var("y"));
        let expected = Value::tuple(vec![Value::int(1), Value::var("y"), Value::int(3)]);
        assert_eq!(result, expected);
    }
}

#[test]
fn test_value_context_occurs_check() {
    // x = ⟨1, x, 3⟩ should detect occurs
    let val = Value::tuple(vec![Value::int(1), Value::var("x"), Value::int(3)]);
    let occurs = ValueContext::find_var_in_value(&val, &Var::new("x"));
    assert!(occurs.is_some());
}

#[test]
fn test_choice_context_decompose() {
    // x = 3; (1 ⊕ 2)
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::choice(Expr::int(1), Expr::int(2)),
        );

    let decompositions = ChoiceContext::decompose(&expr);

    // Should find the choice
    let has_choice = decompositions.iter().any(|(_, e)| matches!(e, Expr::Choice(_, _)));
    assert!(has_choice);
}

#[test]
fn test_scope_context_find_choice() {
    // one{1 ⊕ 2}
    let expr = Expr::one(Expr::choice(Expr::int(1), Expr::int(2)));

    let choices = ScopeContext::find_choice_in_scope(&expr);
    assert_eq!(choices.len(), 1);

    let (ctx, e1, e2) = &choices[0];
    assert_eq!(*e1, Expr::int(1));
    assert_eq!(*e2, Expr::int(2));

    // Reconstruct should give original
    let reconstructed = ctx.fill(Expr::choice(e1.clone(), e2.clone()));
    assert_eq!(reconstructed, expr);
}

#[test]
fn test_scope_choice_nested() {
    // one{(1 ⊕ 2) ⊕ 3}
    let expr = Expr::one(Expr::choice(
        Expr::choice(Expr::int(1), Expr::int(2)),
        Expr::int(3),
        ));

    let choices = ScopeContext::find_choice_in_scope(&expr);

    // Should find both choices
    assert!(choices.len() >= 2);
}

#[test]
fn test_find_subexpr() {
    // x = 3; y = 4; x
    let expr = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::seq(
            Expr::eqn(Value::var("y"), Expr::int(4)),
            Expr::var("x"),
            ),
        );

    // Find all integer literals
    let ints = find_subexpr(&expr, |e| matches!(e, Expr::Value(Value::Hnf(HeadNormalForm::Int(_)))));
    assert_eq!(ints.len(), 2); // 3 and 4
}
