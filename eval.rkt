#lang typed/racket/base

(require racket/match
         "core-lang.rkt"
         "env.rkt")
(provide
 Ast-eval
 Ast-apply-values)

(define (Ast-eval (ast : Ast) (env : AstEnv)) : Val
  (match ast
    ((App (list (Closure (Fun (list vars ...) body) env*) args ...))
     (unless (= (length vars) (length args))
       (error "Ast-eval: wrong number of arguments" vars args))
     (define env** : AstEnv
       (for/fold ((env** env*))
                 ((var vars) (arg args))
         (cons (list var (Ast-eval arg env)) env**)))
     (Ast-eval body env**))
    ((App (list (? PrimOp? op) args ...))
     (Prim-eval op (for/list ((arg args)) (Ast-eval arg env))))
    ((App (list op-ast args ...))
     (match (Ast-eval op-ast env)
       ((? (make-predicate (U Closure PrimOp)) op)
        (Ast-eval (App (cons op args)) env))
       (op
        (error "Ast-eval: operator must be a Closure or PrimOp" op))))
    ((Var name)
     (let loop ((env env))
       (if (pair? env)
           (let ((binding (car env)))
             (if (eq? name (Var-name (car binding)))
                 (cadr binding)
                 (loop (cdr env))))
           (error "Ast-eval: variable not in current environment" name))))
    ((? Fun? fn)
     (Closure fn env))
    ((? Val? val)
     val)))

(define (Prim-eval (op : PrimOp) (args : (Listof Val))) : Val
  (match* (op args)
    (((PrimOp 'cons) (list elem (Seq elems ...)))
     (list->Seq (cons elem elems)))
    (((PrimOp 'car) (list (Seq elems ...)))
     (car elems))
    (((PrimOp 'cdr) (list (Seq elems ...)))
     (list->Seq (cdr elems)))
    (((PrimOp 'list-ref) (list (Seq elems ...) (? (make-predicate Integer) index)))
     (list-ref elems index))
    (((PrimOp 'list) _)
     (list->Seq args))
    (((PrimOp 'stx-e) (list (Stx val ctx)))
     val)
    (((PrimOp 'mk-stx) (list (? Exp? val) (Stx _ ctx)))
     (Stx val ctx))
    (((PrimOp 'mk-stx) (list val (Stx _ ctx)))
     (error "mk-stx: bad expression:" val))
    (((PrimOp 'mk-stx) (list (? Exp? val) ctx))
     (error "mk-stx: bad context:" ctx))
    (((PrimOp 'mk-stx) args)
     (error "mk-stx: requires an expression and a context" args))
    (((PrimOp '+) (list (? (make-predicate Integer) x) (? (make-predicate Integer) y)))
     (+ x y))
    ((_ _)
     (error "Prim-eval bad primitive form" op args))))

(define (Ast-apply-values (closure : Closure) (args : (Listof Val)))
  ;; NOTE: this does not evaluate the operator or the args.
  (match closure
    ((Closure (Fun (list vars ...) body) env)
     (unless (= (length vars) (length args))
       (error "Ast-apply: wrong number of arguments" vars args))
     (define env* : AstEnv
       (for/fold ((env env))
                 ((var vars) (arg args))
         (cons (list var arg) env)))
     (Ast-eval body env*))))
