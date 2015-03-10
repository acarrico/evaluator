# evaluator

By Anthony Carrico <acarrico@memebeam.org>, this work is in the public
domain.

This is an expander and evaluator written to help (me) understand and
explore *Binding as Sets of Scopes — Notes on a new model of macro
expansion for Racket*, by Matthew Flatt.

  * http://www.cs.utah.edu/~mflatt/scope-sets-2/
  * http://www.cs.utah.edu/~mflatt/scope-sets/

I'm going step by step, so you may have to rewind the repository to
follow these steps.

# Core Language and Evaluator

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

# Syntax Objects

This section's git tag is *syntax-objects*.

Carrying on with *Macros that Work Together* section 3.2,
syntax-objects are the representation to be manipulated by macros and
parsed into the core language for evaluation.

At this stage, a syntax-object is just a value with context, and the
context has not been defined, so it has type **Val** for now. I think
that some context will eventually need to be applied deeply or pushed
down lazily (wraps in Dybvig), but I'll wait and see how that fits
into *Macros that Work Together* as it comes up in the paper.

