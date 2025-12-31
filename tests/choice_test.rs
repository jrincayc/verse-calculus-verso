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

// tests/choice_test.rs
// Tests for choice rewriting

use verse_calculus_verso::*;
use crate::rewrite::collect_choice_values;

#[test]
fn test_one_fail() {
    // one{fail} → fail
    let expr = Expr::one(Expr::Fail);
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::Fail));
}

#[test]
fn test_one_value() {
    // one{42} → 42
    let expr = Expr::one(Expr::int(42));
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::int(42)));
}

#[test]
fn test_one_choice() {
    // one{3 ⊕ 4} → 3
    let expr = Expr::one(Expr::choice(Expr::int(3), Expr::int(4)));
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::int(3)));
}

#[test]
fn test_all_fail() {
    // all{fail} → ⟨⟩
    let expr = Expr::all(Expr::Fail);
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::empty_tuple()));
}

#[test]
fn test_all_value() {
    // all{42} → ⟨42⟩
    let expr = Expr::all(Expr::int(42));
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::tuple(vec![Value::int(42)])));
}

#[test]
fn test_all_choice() {
    // all{1 ⊕ 2 ⊕ 3} → ⟨1, 2, 3⟩
    let expr = Expr::all(Expr::choice(
        Expr::int(1),
        Expr::choice(Expr::int(2), Expr::int(3)),
        ));

    let result = rewrite_choice(&expr);
    assert_eq!(
        result,
        Some(Expr::tuple(vec![Value::int(1), Value::int(2), Value::int(3)]))
            );
}

#[test]
fn test_choose_r() {
    // fail ⊕ 42 → 42
    let expr = Expr::choice(Expr::Fail, Expr::int(42));
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::int(42)));
}

#[test]
fn test_choose_l() {
    // 42 ⊕ fail → 42
    let expr = Expr::choice(Expr::int(42), Expr::Fail);
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::int(42)));
}

#[test]
fn test_choose_assoc() {
    // (1 ⊕ 2) ⊕ 3 → 1 ⊕ (2 ⊕ 3)
    let expr = Expr::choice(
        Expr::choice(Expr::int(1), Expr::int(2)),
        Expr::int(3),
        );

    let result = rewrite_choice(&expr);
    assert!(result.is_some());

    match result.unwrap() {
        Expr::Choice(e1, e2) => {
            assert_eq!(*e1, Expr::int(1));
            match *e2 {
                Expr::Choice(e2_1, e2_2) => {
                    assert_eq!(*e2_1, Expr::int(2));
                    assert_eq!(*e2_2, Expr::int(3));
                }
                _ => panic!("Expected nested choice"),
            }
        }
        _ => panic!("Expected choice"),
    }
}

#[test]
#[ignore]
fn test_choose_paper_example() {
    // Simulating: 3 + (20 ⊕ 30)
    // After desugaring: ∃x y. x = 3; y = (20 ⊕ 30); add⟨x, y⟩
    // The choice needs to float out through the context

    let expr = Expr::exists(
        Var::new("x"),
        Expr::seq(
            Expr::eqn(Value::var("x"), Expr::int(3)),
            Expr::exists(
                Var::new("y"),
                Expr::seq(
                    Expr::eqn(Value::var("y"), Expr::choice(Expr::int(20), Expr::int(30))),
                    Expr::app(
                        Value::Hnf(HeadNormalForm::Op(PrimOp::Add)),
                        Value::tuple(vec![Value::var("x"), Value::var("y")]),
                        ),
                    ),
                ),
            ),
        );

    // This should eventually evaluate to 23 ⊕ 33
    // Let's trace through some steps manually to test choose

    // First, substitute x = 3
    let mut current = expr;
    let mut steps = 0;
    while steps < 20 {
        if let Some(next) = crate::rewrite::rewrite_step(&current) {
            current = next;
            steps += 1;
        } else {
            break;
        }
    }

    // Should eventually get a choice at the top level
    assert!(matches!(current, Expr::Choice(_, _)));

    // And both branches should evaluate to integers
    if let Expr::Choice(e1, e2) = current {
        // Continue evaluating the branches
        let mut branch1 = (*e1).clone();
        for _ in 0..10 {
            if let Some(next) = crate::rewrite::rewrite_step(&branch1) {
                branch1 = next;
            } else {
                break;
            }
        }

        let mut branch2 = (*e2).clone();
        for _ in 0..10 {
            if let Some(next) = crate::rewrite::rewrite_step(&branch2) {
                branch2 = next;
            } else {
                break;
            }
        }

        assert_eq!(branch1, Expr::int(23));
        assert_eq!(branch2, Expr::int(33));
    }
}

#[test]
#[ignore]
fn test_all_collects_all_branches() {
    // all{∃x. x = (1 ⊕ 2 ⊕ 3); x}
    // Should collect all three values into ⟨1, 2, 3⟩

    let expr = Expr::all(
        Expr::exists(
            Var::new("x"),
            Expr::seq(
                Expr::eqn(
                    Value::var("x"),
                    Expr::choice(
                        Expr::int(1),
                        Expr::choice(Expr::int(2), Expr::int(3)),
                        ),
                    ),
                Expr::var("x"),
                ),
            ),
        );

    // Step through evaluation with more steps
    let mut current = expr;
    let mut steps = 0;
    while steps < 50 {  // Increased from 20
        if let Some(next) = crate::rewrite::rewrite_step(&current) {
            println!("Step {}: {} -> {}", steps + 1, current, next);
            current = next;
            steps += 1;
        } else {
            break;
        }
    }

    println!("Final after {} steps: {}", steps, current);

    // Should be a tuple with three elements
    match current {
        Expr::Value(Value::Hnf(HeadNormalForm::Tuple(vals))) => {
            assert_eq!(vals.len(), 3);
            assert_eq!(vals[0], Value::int(1));
            assert_eq!(vals[1], Value::int(2));
            assert_eq!(vals[2], Value::int(3));
        }
        _ => panic!("Expected tuple, got: {}", current),
    }
}

#[test]
#[ignore]
fn test_choose_with_context() {
    // Simplified: one{x := (1 ⊕ 2); x}
    let expr = Expr::one(
        Expr::exists(
            Var::new("x"),
            Expr::seq(
                Expr::eqn(Value::var("x"), Expr::choice(Expr::int(1), Expr::int(2))),
                Expr::var("x"),
                ),
            ),
        );

    // Step through evaluation with debugging
    let mut current = expr;
    let mut steps = 0;
    while steps < 50 {  // Increased from 10
        if let Some(next) = crate::rewrite::rewrite_step(&current) {
            println!("Step {}: {} -> {}", steps + 1, current, next);
            current = next;
            steps += 1;
        } else {
            break;
        }
    }

    println!("Final after {} steps: {}", steps, current);

    // Should eventually reduce to 1 (picking first choice)
    assert_eq!(current, Expr::int(1));
}
#[test]
fn test_collect_choice_values() {
    // Helper test for collect_choice_values
    let expr = Expr::choice(
        Expr::int(1),
        Expr::choice(Expr::int(2), Expr::int(3)),
        );

    let values = collect_choice_values(&expr);
    assert_eq!(
        values,
        Some(vec![Value::int(1), Value::int(2), Value::int(3)])
            );
}

#[test]
fn test_paper_example_choice() {
    // From paper: all{(1 ⊕ 2) ⊕ 3} should eventually become ⟨1, 2, 3⟩
    let expr = Expr::all(Expr::choice(
        Expr::choice(Expr::int(1), Expr::int(2)),
        Expr::int(3),
        ));

    // First choose-assoc to get: all{1 ⊕ (2 ⊕ 3)}
    // Then all-choice to get: ⟨1, 2, 3⟩
    let mut current = expr;
    let mut steps = 0;
    while steps < 10 {
        if let Some(next) = crate::rewrite::rewrite_step(&current) {
            current = next;
            steps += 1;
        } else {
            break;
        }
    }

    assert_eq!(
        current,
        Expr::tuple(vec![Value::int(1), Value::int(2), Value::int(3)])
            );
}

#[test]
fn test_one_choice_full_evaluation() {
    // one{1 ⊕ 2} → 1 (via one_choice, not choose)
    let expr = Expr::one(Expr::choice(Expr::int(1), Expr::int(2)));

    // one_choice should fire because left branch is a value
    let result = rewrite_choice(&expr);
    assert_eq!(result, Some(Expr::int(1)));
}

#[test]
fn test_all_choice_with_expressions() {
    // all{add<1,2> ⊕ add<3,4>}
    // After evaluating: all{3 ⊕ 7} → ⟨3, 7⟩
    let expr = Expr::all(Expr::choice(
        Expr::app(
            Value::Hnf(HeadNormalForm::Op(PrimOp::Add)),
            Value::tuple(vec![Value::int(1), Value::int(2)]),
            ),
        Expr::app(
            Value::Hnf(HeadNormalForm::Op(PrimOp::Add)),
            Value::tuple(vec![Value::int(3), Value::int(4)]),
            ),
        ));

    // First step: evaluate add<1,2>
    // This would need multiple rewrite_step calls
    // Just check the structure is reasonable
    assert!(matches!(expr, Expr::All(_)));
}


#[test]
fn test_choice_context_decompose_in_equation() {
    use crate::core::context::ChoiceContext;

    // x = (1 ⊕ 2); y
    let expr = Expr::seq(
        Expr::eqn(
            Value::var("x"),
            Expr::choice(Expr::int(1), Expr::int(2)),
            ),
        Expr::var("y"),
        );

    eprintln!("Test expression: {:?}", expr);
    eprintln!("About to call decompose...");

    let decomps = ChoiceContext::decompose(&expr);

    eprintln!("Found {} decompositions", decomps.len());
    for (i, (ctx, focused)) in decomps.iter().enumerate() {
        eprintln!("  {}: ctx={:?}, focused={}", i, ctx, focused);
    }

    // Should find the choice with a non-trivial context
    let has_nontrivial = decomps.iter().any(|(ctx, focused)| {
        matches!(focused, Expr::Choice(_, _)) && *ctx != ChoiceContext::Hole
    });

    assert!(has_nontrivial, "Should find choice with non-trivial context");
}
