#lang typed/racket/base

(require "syntax-lang.rkt"
         racket/match)

(provide
 (all-from-out "syntax-lang.rkt")
 Ast Ast? (struct-out Var) (struct-out App) Ast-equal?
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

;; NOTE: this is mostly for testing, there are issues with how to test
;; equality.
(define (Ast-equal? x y) : Boolean
  (and (Ast? x)
       (Ast? y)
       (let recurse ((x : Ast x)
                     (y : Ast y)
                     (env : (Listof (Pairof Symbol Symbol)) '()))
         (match* (x y)
           (((Var x-name) (Var y-name))
            (let ((binding (assq x-name env)))
              (and binding (eq? y-name (cdr binding)))))
           (((App x-args) (App y-args))
            (and (= (length x-args) (length y-args))
                 (for/and ((x-arg x-args)
                           (y-arg y-args))
                   (recurse x-arg y-arg env))))
           (((Fun x-vars x-body) (Fun y-vars y-body))
            (and (= (length x-vars) (length y-vars))
                 (recurse x-body y-body
                          (for/fold ((env env))
                                    ((x-var x-vars) (y-var y-vars))
                            (cons (cons (Var-name x-var) (Var-name y-var)) env)))))
           (((? Atom? x) (? Atom? y))
            ;; ISSUE: true now, but probably should have Atom=?
            (equal? x y))
           (((Sym x) (Sym y))
            (eq? x y))
           ((_ _)
            (error "Ast-equal?: unrecognized Ast (fixme, probably)" x y))))))
