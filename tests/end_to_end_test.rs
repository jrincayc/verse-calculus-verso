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

// tests/end_to_end_test.rs
// Tests from parsing to output

use verse_calculus_verso::*;
#[test]
fn test_parse_and_rewrite() {
    let examples = vec![
        ("add<1,2>","3"),
        ("(\\x. x)(42)","42"),
        ("all{1 | 2 | 3}","⟨1, 2, 3⟩"),
        ("one{add<1,2> | add<3,4>}","3"),
        ("?x y z.x = <y,3>; x=<2,z>; y", "2"),
        ("x = 3; y = x; y", "3"),
        ("one{∃x. x = 3; ∃y. y = (20 | 30); add⟨x, y⟩}", "23"),
        ("all{∃x. x = 3; ∃y. y = (20 | 30); add⟨x, y⟩}", "⟨23, 33⟩"),
        ];

    for (input, output) in examples {
        println!("\n=== Testing: {} -> {} ===", input, output);
        match parse_expr_str(input) {
            Ok(expr) =>
            {
                println!("Parsed as {} {:?}", expr, expr);
                let mut step_count = 0;
                let mut current = expr;
                loop {
                    if step_count >= 100 {
                        panic!("Failed to finish rewrite after {} steps.", step_count);
                    }
                    match rewrite_step(&current) {
                        None => {
                            println!("End: {}", current);
                            assert_eq!(current.to_pretty_string(), output);
                            break;
                        }
                        Some(next) => {
                            step_count += 1;
                            println!("{} {}", step_count, next);
                            current = next;
                        }
                    }
                }
            },
            Err(e) => {
                panic!("Failed to parse '{}': {}", input, e);
            }
        }
    }
}
