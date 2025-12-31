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

// src/syntax/parser.rs
// Parser for Verse Calculus using nom

use crate::ast::*;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while1},
    character::complete::{alpha1, alphanumeric1, char, digit1, multispace0, multispace1},
    combinator::{map, opt, recognize},
    error::{context, VerboseError, ParseError},
    multi::{many0, separated_list0, separated_list1, many1},
    sequence::{delimited, pair, preceded, terminated, tuple},
    IResult,
};

type ParseResult<'a, T> = IResult<&'a str, T, VerboseError<&'a str>>;

// ============================================================================
// Lexer
// ============================================================================

fn ws<'a, F, O>(inner: F) -> impl FnMut(&'a str) -> ParseResult<O>
where
    F: FnMut(&'a str) -> ParseResult<O>,
{
    delimited(multispace0, inner, multispace0)
}

fn keyword<'a>(kw: &'a str) -> impl FnMut(&'a str) -> ParseResult<&'a str> {
    move |input| {
        let (input, _) = multispace0(input)?;  // Consume leading whitespace
        let (input, word) = tag(kw)(input)?;   // Match the keyword
        let (input, _) = multispace0(input)?;  // Consume trailing whitespace
        Ok((input, word))
    }
}

// ============================================================================
// Variables and Identifiers
// ============================================================================

fn identifier(input: &str) -> ParseResult<String> {
    context(
        "identifier",
        map(
            recognize(pair(
                alt((alpha1, tag("_"))),
                many0(alt((alphanumeric1, tag("_")))),
            )),
            |s: &str| s.to_string(),
        ),
    )(input)
}

fn variable(input: &str) -> ParseResult<Var> {
    context("variable", map(identifier, Var::new))(input)
}

// ============================================================================
// Literals
// ============================================================================

fn integer(input: &str) -> ParseResult<IntLit> {
    context(
        "integer",
        map(
            ws(recognize(pair(opt(char('-')), digit1))),
            |s: &str| s.parse().unwrap(),
        ),
    )(input)
}

// ============================================================================
// Values and Head-Normal Forms
// ============================================================================

fn value(input: &str) -> ParseResult<Value> {
    context(
        "value",
        alt((
            // Try to parse operators BEFORE variables (since ops are valid identifiers)
            map(primop, Value::Hnf),
            // Then try other head-normal forms
            map(hnf, Value::Hnf),
            // Finally try variables
            map(variable, Value::Var),
        )),
    )(input)
}

fn hnf(input: &str) -> ParseResult<HeadNormalForm> {
    context(
        "head-normal form",
        alt((
            map(integer, HeadNormalForm::Int),
            // primop is now handled in value() above
            map(lambda, |l| HeadNormalForm::Lambda(l.0, l.1)),
            map(tuple_value, HeadNormalForm::Tuple),
        )),
    )(input)
}

fn primop(input: &str) -> ParseResult<HeadNormalForm> {
    context(
        "primitive operator",
        alt((
            nom::combinator::value(HeadNormalForm::Op(PrimOp::Add), keyword("add")),
            nom::combinator::value(HeadNormalForm::Op(PrimOp::Gt), keyword("gt")),
        )),
    )(input)
}

fn tuple_value(input: &str) -> ParseResult<Vec<Value>> {
    context(
        "tuple",
        delimited(
            ws(alt((tag("⟨"), tag("<")))),
            separated_list0(ws(char(',')), value),
            ws(alt((tag("⟩"), tag(">")))),
        ),
    )(input)
}

fn lambda(input: &str) -> ParseResult<(Var, Box<Expr>)> {
    context(
        "lambda",
        map(
            tuple((
                alt((ws(char('λ')), ws(char('\\')))),
                variable,
                ws(char('.')),
                exists_expr,  // Parse body at lower precedence (no sequences at this level)
            )),
            |(_, x, _, e)| (x, Box::new(e)),
        ),
    )(input)
}

// ============================================================================
// Core Expressions
// ============================================================================

fn expr(input: &str) -> ParseResult<Expr> {
    context("expression", choice_expr)(input)
}

fn choice_expr(input: &str) -> ParseResult<Expr> {
    let (input, first) = seq_expr(input)?;
    let (input, rest) = many0(preceded(
        ws(alt((tag("⊕"), tag("|")))),
        seq_expr,
    ))(input)?;

    Ok((
        input,
        rest.into_iter()
            .fold(first, |acc, e| Expr::Choice(Box::new(acc), Box::new(e))),
    ))
}

fn seq_expr(input: &str) -> ParseResult<Expr> {
    // First, parse an exists_expr (or lower)
    let (input, first) = exists_expr(input)?;

    // Check if this is an equation: "v = expr"
    if let Expr::Value(v) = &first {
        if let Ok((input2, _)) = ws::<_, _>(char('='))(input) {
            // Parse the RHS - but NOT including sequences
            // Parse just up to (but not including) the semicolon
            let (input2, rhs) = exists_expr(input2)?;
            // Now we MUST have a semicolon
            let (input2, _) = ws(char(';'))(input2)?;
            let (input2, rest) = expr(input2)?;
            return Ok((input2, Expr::Seq(ExprOrEqn::Eqn(v.clone(), Box::new(rhs)), Box::new(rest))));
        }
    }

    // Try to parse "; expr" (sequence without equation)
    if let Ok((input2, _)) = ws::<_, _>(char(';'))(input) {
        let (input2, rest) = expr(input2)?;
        return Ok((input2, Expr::Seq(ExprOrEqn::Expr(Box::new(first)), Box::new(rest))));
    }

    // No sequence, just return the expression
    Ok((input, first))
}

fn exists_expr(input: &str) -> ParseResult<Expr> {
    // Try to parse "∃x y z. e"
    if let Ok((input, _)) = ws::<_, _>(alt((char('∃'), char('?'))))(input) {
        let (input, vars) = context("variable list",separated_list1(multispace1, variable))(input)?;
        let (input, _) = context("period separator",ws(char('.')))(input)?;
        let (input, body) = context("expression body",expr)(input)?;

        Ok((
            input,
            vars.into_iter()
                .rev()
                .fold(body, |acc, v| Expr::Exists(v, Box::new(acc))),
        ))
    } else {
        application_expr(input)
    }
}

fn application_expr(input: &str) -> ParseResult<Expr> {
    let (input, first) = atom_expr(input)?;
    let (input, rest) = many0(atom_expr)(input)?;

    if rest.is_empty() {
        Ok((input, first))
    } else {
        // Build left-associative application chain
        let mut result = first;
        for arg in rest {
            // Convert exprs to values
            if let (Expr::Value(v1), Expr::Value(v2)) = (&result, &arg) {
                result = Expr::App(v1.clone(), v2.clone());
            } else {
                return Err(nom::Err::Error(VerboseError::from_error_kind(
                    input,
                    nom::error::ErrorKind::Tag,
                )));
            }
        }
        Ok((input, result))
    }
}

fn atom_expr(input: &str) -> ParseResult<Expr> {
    let (input, _) = multispace0(input)?;  // Consume leading whitespace first
    context(
        "atom expression",
        alt((
            // Parse keywords FIRST before trying variables
            map(keyword("fail"), |_| Expr::Fail),
            map(
                delimited(keyword("one{"), expr, ws(char('}'))),
                |e| Expr::One(Box::new(e)),
            ),
            map(
                delimited(keyword("all{"), expr, ws(char('}'))),
                |e| Expr::All(Box::new(e)),
            ),
            // Now parse values (which includes variables)
            map(value, Expr::Value),
            // Parenthesized expressions
            delimited(ws(char('(')), expr, ws(char(')'))),
        )),
    )(input)
}

// ============================================================================
// Extended Syntax Parser
// ============================================================================

fn extended_expr(input: &str) -> ParseResult<ExtendedExpr> {
    context("extended expression", extended_choice)(input)
}

fn extended_choice(input: &str) -> ParseResult<ExtendedExpr> {
    let (input, first) = extended_seq(input)?;
    let (input, rest) = many0(preceded(
        ws(alt((tag("⊕"), tag("|")))),
        extended_seq,
    ))(input)?;

    Ok((
        input,
        rest.into_iter().fold(first, |acc, e| {
            ExtendedExpr::Choice(Box::new(acc), Box::new(e))
        }),
    ))
}

fn extended_seq(input: &str) -> ParseResult<ExtendedExpr> {
    let (input, first) = extended_exists(input)?;

    if let Ok((input, _)) = ws::<_, _>(char(';'))(input) {
        let (input, rest) = extended_expr(input)?;
        Ok((input, ExtendedExpr::Seq(
            Box::new(ExtendedExprOrEqn::Expr(Box::new(first))),
            Box::new(rest),
        )))
    } else {
        Ok((input, first))
    }
}

fn extended_exists(input: &str) -> ParseResult<ExtendedExpr> {
    if let Ok((input, _)) = ws::<_, _>(alt((char('∃'), char('?'))))(input) {
        let (input, vars) = separated_list1(multispace1, variable)(input)?;
        let (input, _) = ws(char('.'))(input)?;
        let (input, body) = extended_expr(input)?;

        Ok((input, ExtendedExpr::Exists(vars, Box::new(body))))
    } else {
        extended_binop(input)
    }
}

fn extended_binop(input: &str) -> ParseResult<ExtendedExpr> {
    let (input, first) = extended_app(input)?;

    if let Ok((input, op)) = ws::<_, _>(alt((char('+'), char('>'))))(input) {
        let (input, second) = extended_binop(input)?;
        let kind = match op {
            '+' => BinOpKind::Add,
            '>' => BinOpKind::Gt,
            _ => unreachable!(),
        };
        Ok((input, ExtendedExpr::BinOp(kind, Box::new(first), Box::new(second))))
    } else {
        Ok((input, first))
    }
}

fn extended_app(input: &str) -> ParseResult<ExtendedExpr> {
    let (input, first) = extended_atom(input)?;
    let (input, rest) = many0(extended_atom)(input)?;

    Ok((
        input,
        rest.into_iter().fold(first, |acc, e| {
            ExtendedExpr::App(Box::new(acc), Box::new(e))
        }),
    ))
}

fn extended_atom(input: &str) -> ParseResult<ExtendedExpr> {
    context(
        "extended atom",
        alt((
            map(value, ExtendedExpr::Value),
            map(keyword("fail"), |_| ExtendedExpr::Fail),
            map(
                delimited(keyword("one{"), extended_expr, ws(char('}'))),
                |e| ExtendedExpr::One(Box::new(e)),
            ),
            map(
                delimited(keyword("all{"), extended_expr, ws(char('}'))),
                |e| ExtendedExpr::All(Box::new(e)),
            ),
            extended_if,
            extended_for,
            delimited(ws(char('(')), extended_expr, ws(char(')'))),
        )),
    )(input)
}

fn extended_if(input: &str) -> ParseResult<ExtendedExpr> {
    let (input, _) = keyword("if")(input)?;
    let (input, _) = ws(char('('))(input)?;

    let (input, (vars, cond)) = if let Ok((input, _)) = ws::<_, _>(alt((char('∃'), char('?'))))(input) {
        let (input, vars) = separated_list1(multispace1, variable)(input)?;
        let (input, _) = ws(char('.'))(input)?;
        let (input, cond) = extended_expr(input)?;
        (input, (vars, cond))
    } else {
        let (input, cond) = extended_expr(input)?;
        (input, (vec![], cond))
    };

    let (input, _) = ws(char(')'))(input)?;
    let (input, _) = keyword("then")(input)?;
    let (input, then_branch) = extended_expr(input)?;
    let (input, _) = keyword("else")(input)?;
    let (input, else_branch) = extended_expr(input)?;

    Ok((
        input,
        ExtendedExpr::IfThenElse {
            cond_vars: vars,
            cond: Box::new(cond),
            then_branch: Box::new(then_branch),
            else_branch: Box::new(else_branch),
        },
    ))
}

fn extended_for(input: &str) -> ParseResult<ExtendedExpr> {
    let (input, _) = keyword("for")(input)?;
    let (input, _) = ws(char('('))(input)?;

    let (input, (vars, gen)) = if let Ok((input, _)) = ws::<_, _>(alt((char('∃'), char('?'))))(input) {
        let (input, vars) = separated_list1(multispace1, variable)(input)?;
        let (input, _) = ws(char('.'))(input)?;
        let (input, gen) = extended_expr(input)?;
        (input, (vars, gen))
    } else {
        let (input, gen) = extended_expr(input)?;
        (input, (vec![], gen))
    };

    let (input, _) = ws(char(')'))(input)?;
    let (input, _) = keyword("do")(input)?;
    let (input, body) = extended_expr(input)?;

    Ok((
        input,
        ExtendedExpr::ForDo {
            vars,
            generator: Box::new(gen),
            body: Box::new(body),
        },
    ))
}

// ============================================================================
// Public API
// ============================================================================

pub fn parse_expr_str(input: &str) -> Result<Expr, String> {
    match expr(input) {
        Ok(("", result)) => Ok(result),
        Ok((remaining, result)) => Err(format!(
            "Parse succeeded '{:?}' but input remained: '{}'",
            result, remaining
        )),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            Err(format!("Parse error: {:?}", e))
        }
        Err(nom::Err::Incomplete(_)) => Err("Incomplete input".to_string()),
    }
}

pub fn parse_extended_str(input: &str) -> Result<ExtendedExpr, String> {
    match extended_expr(input) {
        Ok(("", result)) => Ok(result),
        Ok((remaining, _)) => Err(format!(
            "Parse succeeded but input remained: '{}'",
            remaining
        )),
        Err(nom::Err::Error(e)) | Err(nom::Err::Failure(e)) => {
            Err(format!("Parse error: {:?}", e))
        }
        Err(nom::Err::Incomplete(_)) => Err("Incomplete input".to_string()),
    }
}
