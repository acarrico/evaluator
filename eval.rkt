#lang typed/racket/base

(require racket/match
         "core-lang.rkt"
         "env.rkt")
(provide
 Ast-eval
 Ast-apply-values)

(define (Ast-eval (ast : Ast) (ast-env : AstEnv)) : Val
  (match ast
    ((App (list (Closure (Fun (list vars ...) body) ast-env*) args ...))
     (unless (= (length vars) (length args))
       (error "Ast-eval: wrong number of arguments" vars args))
     (define ast-env** : AstEnv
       (for/fold ((ast-env** ast-env*))
                 ((var vars) (arg args))
         (cons (list var (Ast-eval arg ast-env)) ast-env**)))
     (Ast-eval body ast-env**))
    ((App (list (? PrimOp? op) args ...))
     (Prim-eval op (for/list ((arg args)) (Ast-eval arg ast-env))))
    ((App (list op-ast args ...))
     (match (Ast-eval op-ast ast-env)
       ((? (make-predicate (U Closure PrimOp)) op)
        (Ast-eval (App (cons op args)) ast-env))
       (op
        (error "Ast-eval: operator must be a Closure or PrimOp" op))))
    ((Var name)
     (let loop ((ast-env ast-env))
       (if (pair? ast-env)
           (let ((binding (car ast-env)))
             (if (eq? name (Var-name (car binding)))
                 (cadr binding)
                 (loop (cdr ast-env))))
           (error "Ast-eval: variable not in current environment" name))))
    ((? Fun? fn)
     (Closure fn ast-env))
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
    ((Closure (Fun (list vars ...) body) ast-env)
     (unless (= (length vars) (length args))
       (error "Ast-apply: wrong number of arguments" vars args))
     (define ast-env* : AstEnv
       (for/fold ((ast-env ast-env))
                 ((var vars) (arg args))
         (cons (list var arg) ast-env)))
     (Ast-eval body ast-env*))))
