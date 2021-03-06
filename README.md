# evaluator

By Anthony Carrico <acarrico@memebeam.org>, this work is in the public
domain.

This is an expander and evaluator written to help (me) understand and
explore *Binding as Sets of Scopes — Notes on a new model of macro
expansion for Racket*, by Matthew Flatt.

  * http://www.cs.utah.edu/plt/scope-sets/
  * http://www.cs.utah.edu/~mflatt/scope-sets-5/
  * http://www.cs.utah.edu/~mflatt/scope-sets-4/
  * http://www.cs.utah.edu/~mflatt/scope-sets-3/
  * http://www.cs.utah.edu/~mflatt/scope-sets-2/
  * http://www.cs.utah.edu/~mflatt/scope-sets/

I'm going step by step, so you may have to rewind the repository to
follow these steps.

**Update:** It looks like there is another implementation out there,
  [sweet.js](https://github.com/mozilla/sweet.js/pull/461).

**Update:** J. Ian Johnson is also working on a [typed racket
  implementation of these models](https://github.com/ianj/macro-models).

**Update:** Matthew Flatt's sets of scopes expander has been merged
  into Racket's main branch.

# Session 35 — phase

This sections git tag is *phase*

I'm using more keywords this session. The algorithm needs more
arguments, and long arbitrarily ordered argument lists are poor style.

Scope sets on syntax are now indexed by phase. The *Transform* type
now takes *#:phase*. Phase is passed around through the expander,
parser, and evaluator, so macro transformers can finally use the
expander itself. The compiler state did carry an *eval-env* for macro
expanders, and that is joined by an *expand-env*.

While this session is focused on phase, the *Transform* type now takes
*#:prune*, and a new *syntax-transform* prunes scopes (previously the
*quote-transform* handled syntax). With the additional transform, the
*make-default-initial-state* helper also got keywords.

# Session 34 — sets-of-scopes

This section's git tag is *sets-of-scopes*

This session changes the expander from mark+subst (Dybvig style) to
sets-of-scopes (Flatt style). Much is ripped out and replaced in
"syntax-lang.rkt"; the scope operations are extended to syntax
operations here, and the parser, expander, evaluator, etc. are adapted
as necessary.

The binding table is now part of the compiler state needs which needs
to be passed around even wider than before (to the parser). For
convenience, I've added several ops directly on it:

* CompState-fresh-scope
* CompState-resolve-id
* CompState-bind-id
* CompState-parse

Unfortunately "binding" is an overloaded term. Following Flatt,
*Binding* is now the representation stored in the new global binding
table. The old compile time *Binding* is now *CompileTimeBinding*.

To keep the expander small and lucid, the primitive transformers are
in a new module: "transformers.rkt". More tests have moved into
submodules.

Scopes are added for binding forms and macro use sites, but none of
the other issues addressed in
[scope-sets-5](http://www.cs.utah.edu/~mflatt/scope-sets-5) are dealt
with or tested yet. Most of the evaluator is working as before, except
that *lexpand* is commented out (along with its tests). The existing
tests all pass :).

# Session 33 — bindings

This section's git tag is *bindings*

Mathew Flatt defines the binding table as "the global table that maps
a ⟨symbol, scope set⟩ pair to a representation of a binding". He
suggests storing entries in the scopes, so that when a scope becomes
unreachable, so does its entries.

I've been using immutable persistent state so far, so it makes sense
for me to use an actual table for the binding table. Unfortunately
Racket doesn't provide immutable persistent weak tables. For now, I've
used a weak table to partition the binding table by scope
(*BindingTable-extend* takes a hint to determine the new entry's
partition). The side effect is ok for now, but could be a problem if I
did something crazy that branched the expander (speculative
expansion?).

# Session 32

Add another operator, SetofScopeOps-merge, to merge lazy set-of-scopes
operations.

# Session 31

Now that sets-of-scopes have landed in Racket, submodules seem to work
in typed racket. These changes break up the big test file.

# Session 29, 30

This section's git tag is *scopes*

The *Scope* type is a structure with an *id* field. The *id* is
wrapped in a structure to make it a reference type so it can key weak
tables. A *SetofScopes* is just a *(Setof Scope)*. The model requires
three scope operations *add*, *remove*, and *flip*. The standard set
ops can be used for strict syntax, but lazy operations will be
required for lazy syntax. The *SetofScopeOps* type represents the lazy
ops, and *SetofScopeOps-apply* applies them to a *SetofScopes*. There
are various ways the lazy ops could be represented and optimized. I've
chosen to keep lazy ops as three sets (of scopes), *add*, *remove*,
and *flip*, where each scope is in at most one of these sets. More
importantly, I've added tests for each combination of one and two ops.
Of course the tests caught a bug in one of the combinations. I also
uncovered two racket bugs writing this little module.

# Session 28

I've come to the point where I've done all but *3.8 Definition
Contexts* of *Macros that Work Together*. In [*Binding as Sets of
Scopes Notes on a new model of macro expansion for
Racket*](http://www.cs.utah.edu/~mflatt/scope-sets-5/index.html),
Matthew Flatt writes, "A specification of hygiene in terms of renaming
accommodates simple binding forms well, but it becomes unwieldy for
recursive definition contexts (Flatt et al. 2012, section 3.8)".

Rather than implement that unwieldy version, I'm ready to skip on to
sets-of-scopes (which has matured in the meantime).

I've changed from "day" to the more accurate "session" in my log
headings, and I've reversed the entries. Now I'll prepend new entries
to keep the fresh stuff on top.

# Day 25, 26, 27 — local expand

This section's git tag is *lexpand*

There is a small error in the *nostops* function in *Macros that Work
Together* that confused me a little bit. When it clears the stops, it
should restore the old bindings. I checked with the Racket mailing
list and Ryan Culpepper confirmed the issue.

There are a lot of ways to represent the compile time environment
(type Env). I've been using an association list. It isn't necessary to
"shadow" bindings, so the stack behavior wasn't used or necessary.
The stack behavior could be used to keep the old bindings around
when *lexpand* installs stop bindings, but it seems a bit silly to
look linearly through the list all the time. Instead, my new Env
representation has two fields: a table that maps names to binding, and
a set of stop names.

To look up a name in the new Env:

  * if the name is in the stop set: return the stop transformer binding,
  * otherwise if the name is in the table: return the value,
  * otherwise the name is unbound.

Since the original bindings are always in the table, *lexpand* (even
nested) can just change the stop set.

The next change is to arrange for the expander to pass the mark used
for a macro application to the evaluator for use in *lexpand* (other
uses of the evaluator just pass *#f*).

The compiler state is threaded through the expander. Consequently, to
support local expansion, the compiler state must now also be threaded
through the evaluator. An alternative would be to explicitly pass the
state to macro transformers.

Finally, local expansion introduces a circular dependency between the
evaluator and the expander. I've resolved this by adding the expander
function to the compiler state where the evaluator can find it.

With all this infrastructure in place, *lexpand* slipped in easily as
a new case in the evaluator (once I remembered to resolve the stop
ids).

```
(check-eval '(let-syntax public (lambda (e) (syntax-error))
               (let-syntax class (lambda (e)
                                   ((lambda (e2) (car (cdr (stx-e e2))))
                                    (lexpand (car (cdr (stx-e e))) (list #'public))))
                           (class (public '8))))
            8)
```

# Day 23, 24 — syntax-local-value

This section's git tag is *lvalue*

Previously, the compile time environment was exclusive to the
expander. In this section, it is made available during the application
of macro transformers with a new primitive: *lvalue* (aka
*syntax-local-value*).

To manage the new dependencies, the compile time environment and
evaluator have been split into new modules. The compile time
environment is now passed to evaluator by the expander. At runtime,
the evaluator just gets an empty compile time environment.

The evaluation environment for macro expanders (in *CompState*) now
has a binding for *lvalue*, but not the runtime evaluation
environment:

```
(check-exn #rx"expand: unbound identifier*" (lambda () (eval 'lvalue)))

(check-eval '((lambda (x)
                (let-syntax n #'x
                  (let-syntax m (lambda (stx) (lvalue #'n))
                    m)))
              '42)
            42)
```

# Day 22 — Basic Hygienic Expander

This section's git tag is *basic-hygienic*

This session completes the basic hygienic expander. Renaming the bound
Id in the *let-syntax* transformer was the missing piece, an easy
change since all the machinery was in place for *lambda* last session.

To finish off, I've added the "Scope Examples" from 3.6.1 of *Macros
that Work Together* to the tests.

# Day 17, 18, 19, 20, 21 — Marks and Substitutions.

This section's git tag is *mark-subst*

I'm picking this project up here after about a month off. This section
takes care of marks and substitutions and properly resolving Ids. It
is a little tedious to program. A good resource is Robert Hieb and
Kent Dybvig's 1992 distribution of syntax-case called *psyntax*.

At this point, macro applications are marked before and after
expansion, and *lambda* does renaming. One hygiene problem remains: I
haven't added renaming to *let-syntax*.

For testing, *check-expand* and *check-re-expand* compare with an
*Ast-equal?* predicate, which does equality mod renaming, but
*Ast-equal?* doesn't compare literal syntax, so I've had to comment
out expansion tests like ```(syntax x)```. These were more reasonable
to check in the unhygienic expander because they carried no context.

The *thunk* macro test passes with the hygienic result this time :).

# Day 14, 15, 16 — More Syntax Language Updates

This section's git tag is *more-syntax-language*

By now I should be doing marking and renaming, but I'm still tinkering
with my types. *StxSeq* and *StxAtom* had weird definitions; the match
expanders didn't actually match the types. I've refined the datatypes
while fixing that.

  * The name *Atom* now only applies to the atoms allowed in syntax
    expressions, namely *Integer* and *Sym*. Having another
    distinction for atomic values is potentially confusing, and was
    not really being used anyway.

  * I've made the implementation of syntax-objects polymorphic, so
    there can be a type for indentifiers: *Id*. Likewise,
    syntax-object sequences, have type *Form*.

  * *StxContent* is now simply *Exp*, defined as *(U Atom (Seq Stx))*.

  * *Stx* is an *Exp* together with *Ctx*, but behind the type are two
    variants, *LazyStx* and *StrictStx*. The *Stx* match expander
    pushes lazy context down.

  * finally, *Seq* is more abstract, for example:

```
(Seq (list (Sym 'x) (Sym 'y) (Sym 'z))))
```

is now:

```
(Seq (Sym 'x) (Sym 'y) (Sym 'z)))
```

Likewise, the new *Seq* match expander directly supports list-like
syntax, but it still binds matched elements in Racket lists (rather
than *Seq*s). Racket's vector match expander works this way too. It
doesn't seem like Racket has support for defining match expanders on
new sequences without delegating the task to lists, but this is
something I'll have to investigate.

I had a little trouble here with Typed Racket. The error had the
accessor *StrictStx-exp* used before it was defined. Moving *Exp*'s
*make-predicate* down fixed that, so I've defined that whole cluster
of types before any potential usage.

# Day 13 — Lazy Context

This section's git tag is *lazy-context*

*Macros that Work Together* adds context by marking and renaming
syntax-objects. In the model, these operations work deeply, "the mark
and rename meta-functions push *Mark* and *Rename* records down to all
ctx chains in a syntax object". In *Syntactic Abstraction in Scheme*,
Dybvig, Heib, and Bruggeman point out that earlier systems added
context naively leading to quadradic behavior. Rather than applying
context to every node eagerly, Dybvig wraps context around nodes, and
pushes it down when children are exposed.

To achieve this, I've replaced the *Stx* structure with *StxLazy* and
*StxStrict*. I've used *define-type*, *define-match-expander*, and
*make-predicate* to imitate the syntax of the original *Stx* struct.
No changes were required in the rest of the code.

That is pretty cool. It highlights the power of defining syntax in
general. However, the specific ways the specifications are combined
are ad hoc: *define-match-expander* can provide two meanings to an
identifier, one in a match context, and one in an expression context,
and with the *#:omit-define-syntaxes* keyword, *define-type* can add a
third meaning, in a type expression context. Racket is getting the job
done, but it brings back my reflections in the expander section above,
about what context the expander should provide to macros, and what
mechanisms for building up syntax should be available, etc. I will
again try to ignore these reflections.

# Day 12 — Syntax Language Update

This section's git tag is *syntax-language*

**syntax-lang.rkt**

I'm going to have to get serious about a data structure for syntax
context soon, but first I'm refining the core language.

  * I've distingished *StxAtom* from *Atom*.
  * I've made *Seq* polymorphic, so I can distinguish *(Seq Val)* and *(Seq Stx)*.

Together these allow a more precise definition of *Stx*, so syntax is
cleanly separated from (other) values. I've also collected up some of
the predicates and match expanders. The changes make all the pattern
matching in the rest of the code somewhat simpler.

Coincidentally, Matthias Felleisen and company have just released
[*The Racket Manifesto*](http://www.ccs.neu.edu/home/matthias/manifesto/sec_pl-pl.html),
which says *Racket insists on separating the various stages of
language processing, particularly enforcing a strict separation of
compile-time from run-time code. For example, the rewriting rules
generate pure syntax and may not embed other language values inside
this syntax.*

No new tests in this section, but the existing tests and the type
checker made these changes relatively painless. I did run into a
subtyping issue with *Racket v6.1.1* when I split some of
**core-lang.rkt** into the new module **syntax-lang.rkt**, but this
works fine in a Racket snapshot (thanks to Sam Tobin-Hochstadt for
testing this). If you are following along with a bad version, and you
don't want to grab a snapshot, you could probably combine the two
files to avoid the error.

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
