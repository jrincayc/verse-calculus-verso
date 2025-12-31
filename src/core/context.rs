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

// src/core/context.rs
// Context matching and manipulation for Verse Calculus
// Implements the contexts from Figure 4 of the paper

use crate::ast::*;

// ============================================================================
// Context Types (from Figure 4 in the paper)
// ============================================================================

/// Execution context: X ::= □ | v = X ; e | X ; e | eq; X
/// Used for rules like subst, eqn-elim
#[derive(Debug, Clone, PartialEq)]
pub enum ExecContext {
    Hole,
    EqnLeft(Value, Box<ExecContext>, Box<Expr>),  // v = X; e
    SeqLeft(Box<ExecContext>, Box<Expr>),          // X; e
    SeqRight(ExprOrEqn, Box<ExecContext>),         // eq; X
}

/// Value context: V ::= □ | ⟨v₁, ..., V, ..., vₙ⟩
/// Used for occurs check (u-occurs)
#[derive(Debug, Clone, PartialEq)]
pub enum ValueContext {
    Hole,
    Tuple(Vec<Value>, Box<ValueContext>, Vec<Value>), // ⟨v₁..., V, ...vₙ⟩
}

/// Choice context: CX ::= □ | v = CX ; e | CX ; e | ceq; CX | ∃x. CX
/// Like ExecContext but only choice-free expressions to the left
#[derive(Debug, Clone, PartialEq)]
pub enum ChoiceContext {
    Hole,
    EqnLeft(Value, Box<ChoiceContext>, Box<Expr>),  // v = CX; e
    SeqLeft(Box<ChoiceContext>, Box<Expr>),          // CX; e
    SeqRight(ChoiceExprOrEqn, Box<ChoiceContext>),   // ceq; CX
    Exists(Var, Box<ChoiceContext>),                 // ∃x. CX
}

/// Choice-free expression or equation
#[derive(Debug, Clone, PartialEq)]
pub enum ChoiceExprOrEqn {
    Expr(Expr),  // Must be choice-free
    Eqn(Value, Expr),  // v = ce where ce is choice-free
}

/// Scope context: SX ::= one{SC} | all{SC}
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeContext {
    One(ScopeChoice),
    All(ScopeChoice),
}

/// Scope choice: SC ::= □ | SC ⊕ e | e ⊕ SC
#[derive(Debug, Clone, PartialEq)]
pub enum ScopeChoice {
    Hole,
    Left(Box<ScopeChoice>, Box<Expr>),   // SC ⊕ e
    Right(Box<Expr>, Box<ScopeChoice>),  // e ⊕ SC
}

// ============================================================================
// Execution Context Operations
// ============================================================================

impl ExecContext {
    /// Create a hole context
    pub fn hole() -> Self {
        ExecContext::Hole
    }

    /// Fill the hole in the context with an expression
    pub fn fill(&self, expr: Expr) -> Expr {
        match self {
            ExecContext::Hole => expr,

            ExecContext::EqnLeft(v, ctx, e) => {
                let inner = ctx.fill(expr);
                Expr::Seq(ExprOrEqn::Eqn(v.clone(), Box::new(inner)), e.clone())
            }

            ExecContext::SeqLeft(ctx, e) => {
                let inner = ctx.fill(expr);
                Expr::Seq(ExprOrEqn::Expr(Box::new(inner)), e.clone())
            }

            ExecContext::SeqRight(eq, ctx) => {
                let inner = ctx.fill(expr);
                Expr::Seq(eq.clone(), Box::new(inner))
            }
        }
    }

    /// Try to decompose an expression into a context and a focused subexpression
    /// Returns None if no valid decomposition exists
    pub fn decompose(expr: &Expr) -> Vec<(ExecContext, Expr)> {
        let mut results = Vec::new();

        // The expression itself with hole context
        results.push((ExecContext::Hole, expr.clone()));

        // Try to decompose sequences
        if let Expr::Seq(eq, e) = expr {
            match eq {
                ExprOrEqn::Eqn(v, rhs) => {
                    // Decompose RHS of equation: v = X; e
                    for (inner_ctx, focused) in ExecContext::decompose(rhs) {
                        results.push((
                            ExecContext::EqnLeft(v.clone(), Box::new(inner_ctx), e.clone()),
                            focused,
                        ));
                    }
                }
                ExprOrEqn::Expr(lhs) => {
                    // Decompose LHS: X; e
                    for (inner_ctx, focused) in ExecContext::decompose(lhs) {
                        results.push((
                            ExecContext::SeqLeft(Box::new(inner_ctx), e.clone()),
                            focused,
                        ));
                    }
                }
            }

            // Decompose RHS: eq; X
            for (inner_ctx, focused) in ExecContext::decompose(e) {
                results.push((
                    ExecContext::SeqRight(eq.clone(), Box::new(inner_ctx)),
                    focused,
                ));
            }
        }

        results
    }

    /// Find all occurrences of equations `x = v` in the context
    pub fn find_equations(&self, expr: &Expr, target_var: &Var) -> Vec<(ExecContext, Value, Expr)> {
        let mut results = Vec::new();

        for (ctx, focused) in ExecContext::decompose(expr) {
            if let Expr::Seq(ExprOrEqn::Eqn(v, rhs), rest) = &focused {
                if let Some(x) = v.as_var() {
                    if x == target_var {
                        results.push((ctx, v.clone(), (**rest).clone()));
                    }
                }
            }
        }

        results
    }
}

// ============================================================================
// Value Context Operations
// ============================================================================

impl ValueContext {
    /// Create a hole context
    pub fn hole() -> Self {
        ValueContext::Hole
    }

    /// Fill the hole in the context with a value
    pub fn fill(&self, val: Value) -> Value {
        match self {
            ValueContext::Hole => val,
            ValueContext::Tuple(before, ctx, after) => {
                let mut vals = before.clone();
                vals.push(ctx.fill(val));
                vals.extend(after.clone());
                Value::Hnf(HeadNormalForm::Tuple(vals))
            }
        }
    }

    /// Check if a variable occurs within a value (for occurs check)
    /// Returns Some(context) if the variable occurs, None otherwise
    pub fn find_var_in_value(val: &Value, target: &Var) -> Option<ValueContext> {
        match val {
            Value::Var(x) if x == target => Some(ValueContext::Hole),

            Value::Hnf(HeadNormalForm::Tuple(vals)) => {
                for (i, v) in vals.iter().enumerate() {
                    if let Some(inner_ctx) = Self::find_var_in_value(v, target) {
                        let before = vals[..i].to_vec();
                        let after = vals[i + 1..].to_vec();
                        return Some(ValueContext::Tuple(before, Box::new(inner_ctx), after));
                    }
                }
                None
            }

            _ => None,
        }
    }

    /// Try to decompose expression into choice context, ensuring left side is choice-free
    pub fn decompose(expr: &Expr) -> Vec<(ChoiceContext, Expr)> {
        let mut results = Vec::new();

        // The expression itself
        results.push((ChoiceContext::Hole, expr.clone()));

        // Try to decompose through choice-free constructs
        match expr {
            Expr::Seq(eq, e) => {
                // REMOVED: the early return that was blocking us!

                match eq {
                    ExprOrEqn::Eqn(v, rhs) => {  // REMOVED: if rhs.is_choice_free()
                        // Recurse into RHS of equation
                        for (inner_ctx, focused) in ChoiceContext::decompose(&**rhs) {
                            results.push((
                                ChoiceContext::EqnLeft(v.clone(), Box::new(inner_ctx), e.clone()),
                                focused,
                                ));
                        }
                    }
                    ExprOrEqn::Expr(lhs) if lhs.is_choice_free() => {
                        for (inner_ctx, focused) in ChoiceContext::decompose(&**lhs) {
                            results.push((
                                ChoiceContext::SeqLeft(Box::new(inner_ctx), e.clone()),
                                focused,
                                ));
                        }
                    }
                    _ => {}
                }

                // Always try to decompose RHS (the continuation)
                for (inner_ctx, focused) in ChoiceContext::decompose(&**e) {
                    let ceq = match eq {
                        ExprOrEqn::Expr(e) => ChoiceExprOrEqn::Expr((**e).clone()),
                        ExprOrEqn::Eqn(v, e) => ChoiceExprOrEqn::Eqn(v.clone(), (**e).clone()),
                    };
                    results.push((
                        ChoiceContext::SeqRight(ceq, Box::new(inner_ctx)),
                        focused,
                        ));
                }
            }

            Expr::Exists(x, e) => {
                for (inner_ctx, focused) in ChoiceContext::decompose(&**e) {
                    results.push((
                        ChoiceContext::Exists(x.clone(), Box::new(inner_ctx)),
                        focused,
                        ));
                }
            }

            _ => {}
        }

        results
    }
}

// ============================================================================
// Choice Context Operations
// ============================================================================

impl ChoiceContext {
    /// Create a hole context
    pub fn hole() -> Self {
        ChoiceContext::Hole
    }

    /// Fill the hole in the context with an expression
    pub fn fill(&self, expr: Expr) -> Expr {
        match self {
            ChoiceContext::Hole => expr,

            ChoiceContext::EqnLeft(v, ctx, e) => {
                let inner = ctx.fill(expr);
                Expr::Seq(ExprOrEqn::Eqn(v.clone(), Box::new(inner)), e.clone())
            }

            ChoiceContext::SeqLeft(ctx, e) => {
                let inner = ctx.fill(expr);
                Expr::Seq(ExprOrEqn::Expr(Box::new(inner)), e.clone())
            }

            ChoiceContext::SeqRight(ceq, ctx) => {
                let inner = ctx.fill(expr);
                let eq = match ceq {
                    ChoiceExprOrEqn::Expr(e) => ExprOrEqn::Expr(Box::new(e.clone())),
                    ChoiceExprOrEqn::Eqn(v, e) => ExprOrEqn::Eqn(v.clone(), Box::new(e.clone())),
                };
                Expr::Seq(eq, Box::new(inner))
            }

            ChoiceContext::Exists(x, ctx) => {
                let inner = ctx.fill(expr);
                Expr::Exists(x.clone(), Box::new(inner))
            }
        }
    }

    /// Try to decompose expression into choice context, ensuring left side is choice-free
    pub fn decompose(expr: &Expr) -> Vec<(ChoiceContext, Expr)> {
        let mut results = Vec::new();

        // The expression itself
        results.push((ChoiceContext::Hole, expr.clone()));

        // Try to decompose through choice-free constructs
        match expr {
            Expr::Seq(eq, e) => {
                // REMOVED: the early return that was blocking us!

                match eq {
                    ExprOrEqn::Eqn(v, rhs) => {  // REMOVED: if rhs.is_choice_free()
                        // Recurse into RHS of equation
                        for (inner_ctx, focused) in ChoiceContext::decompose(&**rhs) {
                            results.push((
                                ChoiceContext::EqnLeft(v.clone(), Box::new(inner_ctx), e.clone()),
                                focused,
                                ));
                        }
                    }
                    ExprOrEqn::Expr(lhs) if lhs.is_choice_free() => {
                        for (inner_ctx, focused) in ChoiceContext::decompose(&**lhs) {
                            results.push((
                                ChoiceContext::SeqLeft(Box::new(inner_ctx), e.clone()),
                                focused,
                                ));
                        }
                    }
                    _ => {}
                }

                // Always try to decompose RHS (the continuation)
                for (inner_ctx, focused) in ChoiceContext::decompose(&**e) {
                    let ceq = match eq {
                        ExprOrEqn::Expr(e) => ChoiceExprOrEqn::Expr((**e).clone()),
                        ExprOrEqn::Eqn(v, e) => ChoiceExprOrEqn::Eqn(v.clone(), (**e).clone()),
                    };
                    results.push((
                        ChoiceContext::SeqRight(ceq, Box::new(inner_ctx)),
                        focused,
                        ));
                }
            }
            Expr::Exists(x, e) => {
                // Don't decompose through exists if body is a choice
                // (the choice should be handled at its current level first)
                if !matches!(**e, Expr::Choice(_, _)) {
                    for (inner_ctx, focused) in ChoiceContext::decompose(&**e) {
                        results.push((
                            ChoiceContext::Exists(x.clone(), Box::new(inner_ctx)),
                            focused,
                            ));
                    }
                }
            }

            _ => {}
        }

        results
    }
}

// ============================================================================
// Scope Context Operations
// ============================================================================

impl ScopeContext {
    /// Fill the hole with an expression
    pub fn fill(&self, expr: Expr) -> Expr {
        match self {
            ScopeContext::One(sc) => Expr::One(Box::new(sc.fill(expr))),
            ScopeContext::All(sc) => Expr::All(Box::new(sc.fill(expr))),
        }
    }

    /// Try to find a choice within a one or all context
    pub fn find_choice_in_scope(expr: &Expr) -> Vec<(ScopeContext, Expr, Expr)> {
        let mut results = Vec::new();

        match expr {
            Expr::One(e) => {
                for (sc, e1, e2) in ScopeChoice::find_choice(e) {
                    results.push((ScopeContext::One(sc), e1, e2));
                }
            }
            Expr::All(e) => {
                for (sc, e1, e2) in ScopeChoice::find_choice(e) {
                    results.push((ScopeContext::All(sc), e1, e2));
                }
            }
            _ => {}
        }

        results
    }
}

impl ScopeChoice {
    /// Fill the hole with an expression
    pub fn fill(&self, expr: Expr) -> Expr {
        match self {
            ScopeChoice::Hole => expr,
            ScopeChoice::Left(sc, e) => {
                Expr::Choice(Box::new(sc.fill(expr)), e.clone())
            }
            ScopeChoice::Right(e, sc) => {
                Expr::Choice(e.clone(), Box::new(sc.fill(expr)))
            }
        }
    }

    /// Find a choice and return all possible decompositions
    /// Returns (context, left_branch, right_branch)
    pub fn find_choice(expr: &Expr) -> Vec<(ScopeChoice, Expr, Expr)> {
        let mut results = Vec::new();

        if let Expr::Choice(e1, e2) = expr {
            // The choice itself
            results.push((ScopeChoice::Hole, (**e1).clone(), (**e2).clone()));

            // Decompose left branch
            for (inner_sc, left, right) in Self::find_choice(e1) {
                results.push((
                    ScopeChoice::Left(Box::new(inner_sc), e2.clone()),
                    left,
                    right,
                ));
            }

            // Decompose right branch
            for (inner_sc, left, right) in Self::find_choice(e2) {
                results.push((
                    ScopeChoice::Right(e1.clone(), Box::new(inner_sc)),
                    left,
                    right,
                ));
            }
        }

        results
    }
}

// ============================================================================
// Helper Functions
// ============================================================================

/// Find all positions where an expression matches a pattern
pub fn find_subexpr<F>(expr: &Expr, predicate: F) -> Vec<(ExecContext, Expr)>
where
    F: Fn(&Expr) -> bool,
{
    ExecContext::decompose(expr)
        .into_iter()
        .filter(|(_, e)| predicate(e))
        .collect()
}

/// Find the first matching subexpression
pub fn find_first_subexpr<F>(expr: &Expr, predicate: F) -> Option<(ExecContext, Expr)>
where
    F: Fn(&Expr) -> bool,
{
    ExecContext::decompose(expr)
        .into_iter()
        .find(|(_, e)| predicate(e))
}
