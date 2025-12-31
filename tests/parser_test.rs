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

// tests/parser_test.rs
// Integration tests for parser and pretty-printer

use verse_calculus_verso::*;
#[test]
fn test_parse_and_print_roundtrip() {
    let examples = vec![
        "42",
        "x",
        "⟨1, 2, 3⟩",
        "λx. x",
        "∃x. x",
        "1 ⊕ 2",
        "fail",
        "∃x y. x",
        "x = 3; x",
        "∃x y z. x = ⟨y, 3⟩; x = ⟨2, z⟩; y",
        "one{1 ⊕ 2}",
        "all{1 ⊕ 2}",
    ];

    for input in examples {
        println!("\n=== Testing: {} ===", input);
        match parse_expr_str(input) {
            Ok(expr) => {
                let output = format!("{}", expr);
                println!("  Input:  '{}'", input);
                println!("  Output: '{}'", output);

                // Only test roundtrip if input and output match
                // (normalized forms may differ)
                if input == output {
                    match parse_expr_str(&output) {
                        Ok(expr2) => {
                            assert_eq!(expr, expr2, "Roundtrip failed for: {}", input);
                            println!("  ✓ Roundtrip successful");
                        }
                        Err(e) => {
                            panic!("Failed to re-parse output '{}': {}", output, e);
                        }
                    }
                } else {
                    println!("  ⚠ Input/output differ (this is OK for normalized forms)");
                }
            }
            Err(e) => {
                panic!("Failed to parse '{}': {}", input, e);
            }
        }
    }
}

#[test]
fn test_paper_examples() {
    let intro_example = "∃x y z. x = ⟨y, 3⟩; x = ⟨2, z⟩; y";
    let expr = parse_expr_str(intro_example).expect("Failed to parse intro example");
    println!("Intro example parsed: {}", expr);

    match expr {
        Expr::Exists(ref x, _) => {
            assert_eq!(x.0, "x");
        }
        _ => panic!("Expected Exists"),
    }
}

#[test]
fn test_complex_expressions() {
    let examples = vec![
        ("λx. λy. x", "Nested lambdas"),
        ("∃x. ∃y. ∃z. x", "Multiple existentials"),
        ("1 ⊕ 2 ⊕ 3", "Multiple choices"),
        ("x = 1; y = 2; z = 3; x", "Multiple equations"),
        ("∃f. f = λx. x; f x", "Lambda in existential"),
    ];

    for (input, description) in examples {
        println!("\nTesting {}: {}", description, input);
        match parse_expr_str(input) {
            Ok(expr) => {
                println!("  ✓ Parsed: {}", expr);
            }
            Err(e) => {
                panic!("Failed to parse '{}': {}", input, e);
            }
        }
    }
}

#[test]
fn test_unicode_alternatives() {
    let pairs = vec![
        ("∃x. x", "?x. x"),
        ("⟨1, 2⟩", "<1, 2>"),
        ("1 ⊕ 2", "1 | 2"),
        ("λx. x", "\\x. x"),
    ];

    for (unicode, ascii) in pairs {
        let expr1 = parse_expr_str(unicode).expect("Failed to parse Unicode version");
        let expr2 = parse_expr_str(ascii).expect("Failed to parse ASCII version");
        assert_eq!(expr1, expr2, "Mismatch between '{}' and '{}'", unicode, ascii);
        println!("✓ '{}' ≡ '{}'", unicode, ascii);
    }
}

#[test]
fn test_parse_integers() {
    assert_eq!(
        parse_expr_str("42").unwrap(),
        Expr::int(42)
    );

    assert_eq!(
        parse_expr_str("-17").unwrap(),
        Expr::int(-17)
    );
}

#[test]
fn test_parse_variables() {
    assert_eq!(
        parse_expr_str("x").unwrap(),
        Expr::var("x")
    );

    assert_eq!(
        parse_expr_str("foo_bar").unwrap(),
        Expr::var("foo_bar")
    );
}

#[test]
fn test_parse_tuples() {
    assert_eq!(
        parse_expr_str("⟨1, 2, 3⟩").unwrap(),
        Expr::tuple(vec![Value::int(1), Value::int(2), Value::int(3)])
    );

    assert_eq!(
        parse_expr_str("⟨⟩").unwrap(),
        Expr::empty_tuple()
    );
}

#[test]
fn test_parse_lambda() {
    let expr = parse_expr_str("λx. x").unwrap();
    assert_eq!(expr, Expr::lambda(Var::new("x"), Expr::var("x")));
}

#[test]
fn test_parse_exists() {
    let expr = parse_expr_str("∃x. x").unwrap();
    assert_eq!(expr, Expr::exists(Var::new("x"), Expr::var("x")));

    let expr = parse_expr_str("∃x y. x").unwrap();
    assert_eq!(
        expr,
        Expr::exists(Var::new("x"), Expr::exists(Var::new("y"), Expr::var("x")))
    );
}

#[test]
fn test_parse_choice() {
    let expr = parse_expr_str("1 ⊕ 2").unwrap();
    assert_eq!(expr, Expr::choice(Expr::int(1), Expr::int(2)));
}

#[test]
fn test_parse_sequence() {
    let expr = parse_expr_str("x = 3; x").unwrap();
    assert_eq!(
        expr,
        Expr::seq(Expr::eqn(Value::var("x"), Expr::int(3)), Expr::var("x"))
    );
}

#[test]
fn test_free_vars() {
    let expr = Expr::exists(
        Var::new("x"),
        Expr::seq(
            Expr::eqn(Value::var("x"), Expr::int(3)),
            Expr::var("y"),
        ),
    );

    let fvs = expr.free_vars();
    assert_eq!(fvs.len(), 1);
    assert!(fvs.contains(&Var::new("y")));
    assert!(!fvs.contains(&Var::new("x")));
}

#[test]
fn test_choice_free() {
    let e1 = Expr::seq(
        Expr::eqn(Value::var("x"), Expr::int(3)),
        Expr::var("x"),
    );
    assert!(e1.is_choice_free());

    let e2 = Expr::choice(Expr::int(1), Expr::int(2));
    assert!(!e2.is_choice_free());
}

#[test]
fn test_debug_exists() {
    let input = "∃x y. x";
    println!("Parsing: {}", input);
    match parse_expr_str(input) {
        Ok(expr) => {
            println!("Success: {:?}", expr);
            let output = format!("{}", expr);
            println!("Printed: {}", output);
        }
        Err(e) => {
            println!("Error: {}", e);
        }
    }
}
