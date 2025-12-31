# Verse Calculus Verso

## Why

This is an experiment in vibe coding.  Claude Sonnet 4.5 was given the
paper: "The Verse Calculus: A Core Calculus for Deterministic
Functional Logic Programming" and asked to design an implementation,
and then asked to implement it. I occasionally suggested tests,
implemented parts of main.rs, and helped a bit with debugging (Gemini
also help with the debugging occasionally), but the majority of the
code and work was from Claude.  I did not track my time carefully, but
it was less than 8 hours of developer time. This is about the limit
for what I can get vibe coded as of 2025 December, and this definitely
would have been impossible with 2024 LLMs that I tried.

This is missing the rewrite rules var-swap and seq-swap, and the
syntax sugar for extended expressions. I am fairly certain that
additional testing can find edge cases that don't work, and other
problems.

The REPL that this includes has been helpful to me for understanding
Verse Calculus better and so I am putting this on github for others to
use, or have as an example of what can be done with LLMs in late 2025.

I probably will not work on this further, but if anyone wants to
submit a pull request, I will try and review it and either accept it
or explain why not.

Enjoy,
Joshua Cogliati

## Compiling and Running

This uses the Rust Programming language and the cargo package manager.

Compile this with:
```
cargo build
```

Test with:
```
cargo test
```

Example run with:
```
./target/debug/verse-calculus-verso
Verse Calculus Verso REPL v0.1.0
Type expressions to evaluate, or Ctrl-D to exit

verse> all{∃x. x = 3; ∃y. y = (20 | 30); add⟨x, y⟩}
⟨23, 33⟩
  (21 steps)
verse> ?x y z.x = <y,3>; x=<2,z>; y
2
  (9 steps)
verse>
Goodbye!
```

## Additional useful information

This code and documentation contain information and algorithms from
the paper: "The Verse Calculus: A Core Calculus for Deterministic
Functional Logic Programming" by Lennart Augustsson, Joachim Breitner,
Koen Claessen, Ranjit Jhala, Simon Peyton Jones, Olin Shivers, Guy
L. Steele Jr., Tim Sweeney which is licensed under a Creative Commons
Attribution 4.0 International License.

## From Figure 1 of The Verse Calculus paper:

```
VC Syntax:

Integers	𝑘
Variables 	𝑥, 𝑦, 𝑧, 𝑓 , 𝑔
Programs	𝑝 ::= one{e} where fvs(𝑒) = ∅
Expressions	𝑒 ::= v | 𝑒𝑞; e | ∃x. e | fail | e₁ ⊕ e₂ | v₁ v₂ | one{e} | all{e}
Equations or Expressions  𝑒𝑞 ::= e | v = e
Values	  v ::= 𝑥 | hnf
Head values hnf ::= 𝑘 | 𝑜𝑝 | ⟨v₁, ···, vₙ ⟩ | 𝜆x. e
Primops	    𝑜𝑝 ::= gt | add


Concrete syntax: “ ” and “;” are right-associative.
	 “=” binds more tightly than “;”.
	 “𝜆” and “∃” each scope as far to the right as possible.
For example, (𝜆y. ∃x. x = 1; x + y) means (𝜆y. (∃x. ((x = 1); (x + y)))).
Parentheses may be used freely to aid readability and override default precedence.
fvs(e) means the free variables of e; in VC, 𝜆 and ∃ are the only binders.


Desugaring of extended expressions
e₁ + e₂	   means       add⟨e₁, e₂ ⟩
e₁ > e₂	   means       gt⟨e₁, e₂ ⟩
∃x₁ x₂ ··· xₙ . e      means  ∃x₁ . ∃x₂ . ···∃xₙ . e
x := e₁ ; e₂  means    ∃x. x = e₁ ; e₂
e₁ e₂	means†	       f := e₁ ; x := e₂ ; f x with f, x fresh
⟨e₁ , ···, eₙ ⟩	       means†  x₁ := e₁ ; ···; xₙ := eₙ ; ⟨x₁, ···, xₙ ⟩ with xᵢ fresh
e₁ = e₂	   means‡      x := e₁; x = e₂ ; x with x fresh
𝜆⟨x₁, ···, xₙ ⟩. e     means	𝜆p. ∃x₁ ··· xₙ . p = ⟨x₁ , ···, xₙ ⟩; e	with p fresh, n ⩾ 0
if (∃x₁ ···xₙ . e₁ ) then e₂ else e₃	means  (one{(∃x₁ ···xₙ . e₁ ; 𝜆⟨⟩. e₂ ) (𝜆⟨⟩. e₃ )})⟨⟩

† Apply this rule only if at least one of the eᵢ is not a value v.
‡ Apply this rule only if either (i) e₁ is not a value v, or (ii) e₁ = e₂ is not to the left of a “;”.
```

## From Figure 3 of The Verse Calculus paper:

```
Application:
app-add	add⟨k₁ , k₂ ⟩ −→ k₃	where 𝑘₃ = 𝑘₁ + 𝑘₂
app-gt	gt⟨k₁ , k₂ ⟩ −→ k₁	if 𝑘₁ > 𝑘₂
app-gt-fail   gt⟨k₁ , k₂ ⟩ −→ fail if 𝑘₁ ⩽ 𝑘
app-beta𝛼     (𝜆x. e)(v) −→ ∃x. x = v; e if 𝑥 ∉ fvs(v)
app-tup	      ⟨v0, ···, vₙ ⟩(v) −→ ∃x. x = v; (x = 0; v0) ⊕ ··· ⊕ (x = n; vₙ )	fresh x ∉ fvs(v, v0, ···, vₙ )
app-tup-0     ⟨⟩(v) −→ fail

Unification:
u-lit	k₁ = k₂ ; e −→ e	if 𝑘₁ = 𝑘₂
u-tup	⟨v₁ , ···, vₙ ⟩ = ⟨v₁′ , ···, vₙ′ ⟩; e −→ v₁ = v₁′ ; ···; vₙ = vₙ′ ; e
u-fail	hnf₁ = hnf₂ ; e −→ fail      if u-lit, u-tup do not match and neither hnf₁ nor hnf₂ is a lambda
u-occurs     x = V [ x ]; e −→ fail  if V ≠ □
subst	     𝑋 [ x = v; e ] −→ (𝑋 {v/x}) [ x = v; e{v/x} ]	if v ≠ V [ x ]
hnf-swap     hnf = v; e −→ v = hnf ; e
var-swap     y = x; e −→ x = y; e  if x ≺ y
seq-swap     𝑒𝑞; x = v; e −→ x = v; 𝑒𝑞; e unless (𝑒𝑞 is y = v ′ and y ⪯ x)

Elimination:
val-elim	v; e −→ e
exi-elim	∃x. e −→ e	if x ∉ fvs(e)
eqn-elim	∃x. 𝑋 [ x = v; e ] −→ 𝑋 [ e ]	if x ∉ fvs(𝑋 [ e ]) and v ≠ V [ x ]
fail-elim	𝑋 [ fail] −→ fail

Normalization:
exi-float𝛼	𝑋 [ ∃x. e ] −→ ∃x. 𝑋 [ e ]	if 𝑥 ∉ fvs(𝑋 )
seq-assoc	(𝑒𝑞; e₁ ); e₂ −→ 𝑒𝑞; (e₁; e₂ )
eqn-float	v = (𝑒𝑞; e₁ ); e₂ −→ 𝑒𝑞; (v = e₁ ; e₂ )
exi-swap	∃x. ∃y. e −→ ∃y. ∃x. e

Choice:
one-fail	one{fail} −→ fail
one-value	one{v} −→ v
one-choice	one{v ⊕ e} −→ v
all-fail	all{fail} −→ ⟨⟩
all-value	all{v} −→ ⟨v⟩
all-choice	all{v₁ ⊕ ··· ⊕ vₙ } −→ ⟨v₁, ···, vₙ ⟩
choose-r	fail ⊕ e −→ e
choose-l	e ⊕ fail −→ e
choose-assoc	(e₁ ⊕ e₂) ⊕ e₃ −→ e₁ ⊕ (e₂ ⊕ e₃ )
choose		SX [𝐶𝑋 [ e₁ ⊕ e₂ ] ] −→ SX [𝐶𝑋 [ e₁ ] ⊕ 𝐶𝑋 [ e₂ ] ]

Note: In the rules marked with a superscript 𝛼, use 𝛼-conversion to satisfy the side condition.
```

## From Figure 4 of The Verse Calculus paper:

```
Contexts:
Execution contexts	𝑋 ::= □ | v = 𝑋 ; e | 𝑋 ; e | 𝑒𝑞; 𝑋
Value contexts		𝑉 ::= □ | ⟨v₁, ···, V, ···, vₙ ⟩
Scope contexts		𝑆𝑋 ::= one{SC} | all{SC}
      			𝑆𝐶 ::= □ | SC ⊕ e | e ⊕ SC
Choice contexts		𝐶𝑋 ::= □ | v =𝐶𝑋 ; e | 𝐶𝑋 ; e | ceq; 𝐶𝑋 | ∃x. 𝐶𝑋
Choice-free exprs	𝑐𝑒 ::= v | ceq; ce | one{e} | all{e} | ∃x. ce | op(v)
	    		𝑐𝑒𝑞 ::= ce | v = ce

Note: The □ in 𝑋 can only be an expression, not an equation.
```