# evaluator

By Anthony Carrico <acarrico@memebeam.org>, this work is in the public
domain.

This is an expander and evaluator written to help (me) understand and
explore *Binding as Sets of Scopes — Notes on a new model of macro
expansion for Racket*, by Matthew Flatt.

  * http://www.cs.utah.edu/~mflatt/scope-sets-3/
  * http://www.cs.utah.edu/~mflatt/scope-sets-2/
  * http://www.cs.utah.edu/~mflatt/scope-sets/

I'm going step by step, so you may have to rewind the repository to
follow these steps.

**Update:** It looks like there is another implementation out there,
  [sweet.js](https://github.com/mozilla/sweet.js/pull/461).

# Day 1 — Core Language and Evaluator

This section's git tag is *core-language*.

Section eight of *Sets of Scopes* has a model based on:

  * *Macros that Work Together — Compile-Time Bindings, Partial
  Expansion, and Definition Contexts*, Matthew Flatt, Ryan Culpepper,
  David Darais, and Robert Bruce Findler.

This paper has a nice step-by-step development of the model, so it is
a good place to start.

*Macros that Work Together* uses syntax-objects with marks and
substitution like the original (Dybvig) syntax-case expander. One
special feature is that it strictly separates *parsing* and
*expansion*. In this case, *parsing* is defined as converting a fully
expanded syntax-object into an abstract syntax tree (AST) for
evaluation. This feature allows syntax to be expanded more than once.

**core-lang.rkt**

For step one, in this repository, I've created Typed Racket data
structures and an evaluator for ASTs.

Some differences from the paper and other things to notice:

* functions have a list of variables instead of just one.

* this evaluator has closure values, whereas the model uses deep
  substitution when applying a function.

* the primitive evaluator procedure, **Prim-eval**, has list
  primitives, as in the paper, but anything could be added here.

**scanner.rkt**
**test.rkt**

I've added scanners for testing and playing.

```
(check-equal? (scan 'x)
              (Sym 'x))
```

Racket's **#%name** style symbols scan as primitives, and there is a
scanner for environments. Putting these together you can create an
initial environment:

```
(define ast-env (AstEnv-scan '((cons #%cons) (car #%car) (cdr #%cdr)
                               (list-ref #%list-ref) (list #%list))))
```

For testing, the little helper procedure **check-Ast-eval** uses that
environment to run checks:

```
(check-Ast-eval '(list-ref (#%val (a b c)) 0)
                'a)
```

The scanner can also read primitives, functions, and closures as
values, which is helpful for testing and playing, especially at this
early step:

```
(check-Ast-eval '((#%val (#%fun (y) ((#%val (#%fun (x) y)) 0))) 1)
                1)

(check-Ast-eval '((#%val (#%closure (#%fun () x) ((x 5)))))
                5)
```

# Day 2 — Syntax Objects

This section's git tag is *syntax-objects*.

Carrying on with *Macros that Work Together* section 3.2,
syntax-objects are the representation to be manipulated by macros and
parsed into the core language for evaluation.

At this stage, a syntax-object is just a value with context, and the
context has not been defined, so it has type **Val** for now. I think
that some context will eventually need to be applied deeply or pushed
down lazily (wraps in Dybvig), but I'll wait and see how that fits
into *Macros that Work Together* as it comes up in the paper.

The scanner gets a clause to read syntax-objects:

```
(check-equal? (scan '(#%stx 2 context)) (Stx 2 (Sym 'context)))
```

The primitive evaluator gets two new clauses **stx-e** and **mk-stx**
as in the paper:

```
(check-Ast-eval '(mk-stx 1 (#%val (#%stx 2 context)))
                '(#%stx 1 context))

(check-Ast-eval '(stx-e (mk-stx 1 (#%val (#%stx 2 context))))
                '1)
```

**Update:** the PrimOp *mk-stx* originally accepted *(Seq (listof
  Val))* but now requires *(Seq (listof Stx))*. For now, this follows
  the paper, but the paper also mentions that this is relaxed in
  Racket.

# Days 3, 4, 5, 6 :( — Parser

This section's git tag is *parser*.

As with the previous sections, it took one sitting to write the
parser, however it took several more sittings to get decent looking
code to typecheck. I knew I should get this right, because the parser
is very similar to the expander. The problem is that currently,
*match* doesn't always cooperate with Typed Racket. I found a bug
report (14900 at bugs.racket-lang.org) which gave me the clue to try
*#{x : X}* style binding annotations in match patterns. This resolved
my issues, and everything fell into place.

As mentioned in the introduction, *Macros that Work Together* defines
*parsing* as converting a fully expanded syntax-object into an AST.
The paper shows both *symbol-driven* and *identifier-driven* parsers.
The former compares identifiers by name, the later *resolve*s (will
resolve) identifiers by their (as yet undefined) lexical context. I
skipped directly to the identifier-driven parser.

The value scanner can be used to test the parser, but it is tedious.
To create syntax-objects easily, I've implemented another scanner,
*Stx-scan*. It is very simple, since it has just three cases,
sequence, symbol, and integer. To summarize:

  * *scan* converts a Racket value (type Any) to a
  value (type Val)
  * *Stx-scan* converts a Racket value (type Any) to a syntax-object
  (type Stx)
  * *parse* converts a syntax-object (type Stx) to an AST (type Ast)

Testing looks like this:

```
(check-equal? (parse (Stx-scan '(lambda (x y) (x y))))
              (Fun (list (Var 'x) (Var 'y)) (App (list (Var 'x) (Var 'y)))))
```

The value scanner can also be used:

```
(check-equal? (parse (Stx-scan '(lambda (x y) (x y))))
              (scan '(#%fun (x y) (x y))))
```

Or even the syntax-object scanner:

```
(check-equal? (parse (Stx-scan '(syntax (x y z))))
              (Stx-scan '(x y z)))
```

# Day 7, 8, 9 — Expander

This section's git tag is *expander*.

*Macros that Work Together* says, "The next step ... is to create an
expander that takes a syntax object for a source program and returns a
syntax object for the expanded program."

  * *expand* converts a source syntax-object (type Stx) to a
    syntax-object (type Stx) that fits the parser.

The expander must also *recognize* the forms seen by the parser as
mandated earlier in the paper, "the output of the expand function must
also be a suitable input to expand, and expand must be idempotent."

It follows that since the parser is already done, the framework of the
expander can be cut-and-pasted from the parser. Anytime I
cut-and-paste code, I wonder about abstraction. In this case,
expansion could be a mode switch for the parser or vice versa. There
are quite a few problems with that idea, starting with the different
return types differ (Stx vs. Ast). Still, the patterns and error
checking might be shared somehow.

Another practical issue is building a list and reversing it into a
*Seq*. I used type *Seq* for lists in the evaluator to emphasize that
it is an ordered composite, but not to imply a single linked list. The
code would be nicer with a *Seq* data structure that can cons at both
ends.

The expander must generate fresh names, so I've created the
*CompState* type to keep track of a counter. *CompState* could be
extended if other state is needed. The counter is a simple way to
generate the fresh names, but it would be nice if the expander's
output was independent of the particulars of the expander itself.
Perhaps if syntax-objects were (explictly) graphs instead of trees,
variable references could connect to bindings instead of using fresh
names.

This notion of a graph data structure representing not just the
syntax, but also environment, and compiler state distracted me for a
while. It is intriguing to think about passing a node with parsed
context above and unparsed syntax below. In this case, transformers
could easily determine if they were in an expression, definition,
match, module or other context by looking *up*. Identifiers cut and
pasted by macros could be closed over and then moved in and out of
these contexts.

Meanwhile, I've implemented the simple thing. It is tricky to test the
expanded syntax with all its context, so the *check-expand* helper
parses the output. Even so, knowledge of variable renaming is
necessary to write the checks. I should probably write a checker that
checks syntax vs. syntax, dealing with context and alpha conversion,
but for now testing looks like this:

```
(check-expand '(lambda (lambda) lambda)
              (Fun (list (Var '#%0-lambda)) (Var '#%0-lambda)))
```

```
(check-expand '(lambda (x) (lambda (y) (x y)))
              (Fun (list (Var '#%0-x))
                   (Fun (list (Var '#%1-y))
                        (App (list (Var '#%0-x) (Var '#%1-y))))))
```

Just to be sure I've achieved the goal, I also run the expanded
outputs back through the expander to test idempotence:

```
(check-re-expand '(lambda (lambda) lambda)
                 (Fun (list (Var '#%1-lambda)) (Var '#%1-lambda)))
```

If I had a better checker, I could avoid duplicating test cases when
checking re-expansion.

Finally, note that I have extended the model in the paper to allow
identifiers to be bound as transformers. In addition to being useful,
it simplifies error checking.

# Day 10 — Evaluation

This section's git tag is *evaluation*.

Before introducing macros, it is worth hooking up all the pieces. All
of this is in test.rkt. First I've gathered together consistent
initial state: *initial-eval-env*, *initial-expand-env*, and
*initial-state*. Now the expansion environment contains variable
bindings for the primitive operations in addition to *lambda*,
*quote*, and *syntax*. With that, I can define *check-eval* to take a
full trip through the system:

```
(define (eval i)
  (define-values (state expanded)
    (expand initial-state initial-expand-env (Stx-scan i)))
  (Ast-eval (parse expanded) initial-eval-env))

(define (check-eval i o)
(check-equal? (eval i) (scan o)))
```

The input is scanned to syntax, expanded, parsed, evaluated, and
compared to the scanned expected output. I've more-or-less duplicated
the old *check-Ast-eval* checks with *check-eval*. They look like
this:

```
(check-eval '(list-ref (list 'a 'b 'c) '0)
            'a)
```

The *check-Ast-eval* checks include some literal closure values, but
the syntax scanner can't create closure literals, however it isn't
really necessary since the closures in those tests can easily be
created with lambda:

```
(check-eval '((lambda (y) ((lambda (x) y) '0)) '1)
            '1)
```
# Day 11 — Binding and Using Macros

This section's git tag is *macros*

As a historical note, unhygienic macros go all the way back to Timothy
Hart's, *MACRO Definitions for LISP*,
[AIM-057](https://github.com/acarrico/ai-memo), 1963-10-12.

With the machinery that is in place, the *let-syntax* transformer is
simple: just parse, evaluate, and bind the macro transformer. It is
not expanded, keeping with the model in the paper. Expansion would
bring questions about the appropriate expansion environment for macro
transformers. I'll probably need to address these questions later,
since it is painful to write macros in a language without macros. Even
so, I do need an evaluation environment for macro transformers, so
I've added one to the compiler state.

For macro use, the expander has a new clause to perform macro
applications and re-expand the resulting syntax-object. This could be
done with *Ast-eval*, but to be a little cleaner, I've added
*Ast-apply-values* to the core.

For testing, I've added *let-syntax* to the environment. I've also
added a new primitive *+*, which lets me try the unhygienic examples
from the paper:

```
(check-eval
 '(let-syntax thunk (lambda (e)
                      (mk-stx
                       (list #'lambda #'(a)
                             (car (cdr (stx-e e))))
                       e))
              ((thunk (+ '1 '2)) '0))
 '3)

(check-eval
 '(let-syntax thunk (lambda (e)
                      (mk-stx
                       (list #'lambda #'(a)
                             (car (cdr (stx-e e))))
                       e))
              (((lambda (a) (thunk (+ a '1))) '5) '0))
 ;; Unhygienic answer:
 '1
 ;; Hygienic answer:
 ;; '6
 )
```
