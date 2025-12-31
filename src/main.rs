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
use std::io;
use std::io::Write;
use verse_calculus_verso::{parse_expr_str, rewrite_step, Expr};

fn main() {
    println!("Verse Calculus Verso REPL v0.1.0");
    println!("Type expressions to evaluate, or Ctrl-D to exit");
    println!();

    let mut infile: Box<dyn io::BufRead> = Box::new(io::stdin().lock());

    loop {
        print!("verse> ");
        let _ = std::io::stdout().flush();

        let mut line = String::new();
        let input_result = infile.read_line(&mut line);

        match input_result {
            Ok(0) => {
                println!("\nGoodbye!");
                break;
            }
            Err(e) => {
                println!("Error reading input: {}", e);
                break;
            }
            Ok(_) => {
                let trimmed = line.trim();
                if trimmed.is_empty() {
                    continue;
                }

                match parse_expr_str(trimmed) {
                    Err(e) => {
                        println!("Parse error: {}", e);
                    }
                    Ok(expr) => {
                        evaluate_and_print(expr);
                    }
                }
            }
        }
    }
}

fn evaluate_and_print(expr: Expr) {
    let mut current = expr;
    let mut step_count = 0;
    const MAX_STEPS: usize = 100;
    let show_steps = false;
    if show_steps { println!("{} {:?}", current, current);}
    loop {
        if step_count >= MAX_STEPS {
            println!("⚠ Evaluation stopped after {} steps (possible infinite loop)", MAX_STEPS);
            println!("Current expression: {}", current);
            return;
        }

        match rewrite_step(&current) {  // Changed from rewrite_application
            None => {
                println!("{}", current);
                if step_count > 0 {
                    println!("  ({} step{})", step_count, if step_count == 1 { "" } else { "s" });
                }
                return;
            }
            Some(next) => {
                step_count += 1;
                if show_steps
                {
                    println!("{} {} -> {} {:?}", step_count, current, next, next);
                    //println!("{} {} -> {}", step_count, current, next);
                }
                current = next;
            }
        }
    }
}
