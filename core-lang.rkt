#lang typed/racket/base

(require "syntax-lang.rkt")

(provide
 (all-from-out "syntax-lang.rkt")
 Ast Ast? (struct-out Var) (struct-out App)
 Val Val? (struct-out Fun)
 (struct-out PrimOp)
 (struct-out PrimAst) (struct-out Closure)
 AstEnv)

(define-type Ast (U Var App Val))
(struct Var ((name : Symbol)) #:transparent)
(struct App ((args : (Listof Ast))) #:transparent)
(define-type Val (U Fun (Seq Val) Atom PrimOp PrimAst Closure Stx))
(struct Fun ((vars : (Listof Var)) (body : Ast)) #:transparent)
(define Val? (make-predicate Val))
(struct PrimOp ((name : Symbol)) #:transparent)
(struct PrimAst ((ast : Ast)) #:transparent)
(define-type AstEnv (Listof (List Var Val)))
(struct Closure ((fun : Fun) (env : AstEnv)) #:transparent)
(define Ast? (make-predicate Ast))
