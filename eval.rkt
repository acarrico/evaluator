#lang typed/racket/base

(require racket/match
         "core-lang.rkt"
         "env.rkt"
         "scope.rkt"
         "binding.rkt")
(provide
 Ast-eval
 Ast-apply-values)

(: Ast-eval (-> Ast AstEnv CompState Env (U Scope #f) #:phase Phase (Values CompState Val)))

(define (Ast-eval ast ast-env state env scope #:phase ph)
  (match ast
    ((App (list (Closure (Fun (list vars ...) body) ast-env*) args ...))
     (unless (= (length vars) (length args))
       (error "Ast-eval: wrong number of arguments" vars args))
     (define-values (#{state* : CompState} #{ast-env** : AstEnv})
       (for/fold ((state state)
                  (ast-env* ast-env*))
                 ((var vars) (arg args))
         (define-values (state* arg*) (Ast-eval arg ast-env state env scope #:phase ph))
         (values state* (cons (list var arg*) ast-env*))))
     (Ast-eval body ast-env** state* env scope #:phase ph))
    ((App (list (PrimOp 'lvalue) arg-ast))
     (define-values (state* arg)
       (Ast-eval arg-ast ast-env state env scope #:phase ph))
     (match arg
       ((Id id)
        (define name (Binding-name (CompState-resolve-id state* id #:phase ph)))
        (match (Env-ref env name)
          ((ValBinding val) (values state* val))
          (_
           (error "lvalue: the Id must be bound to a value in the compile time environment" name env))))
       (_
        (error "lvalue takes an Id"))))
    ((App (list (PrimOp 'lvalue) _ ...))
     (error "lvalue takes one argument"))
    #;((App (list (PrimOp 'lexpand) exp-ast stops-ast))
     (define-values (state* stx) (Ast-eval exp-ast ast-env state env scope #:phase ph))
     (define-values (state** stops) (Ast-eval stops-ast ast-env state* env scope #:phase ph))
     (cond ((not (Scope? scope))
            (error "lexpand: not in a macro application"))
           ((not (Stx? stx))
            (error "lexpand: first arg should be the syntax object to expand" stx))
           ((not (Stops? stops))
            (error "lexpand: second arg should be the set (list) of stop ids" stops))
           (else
            (define i-unmarked (Stx-mark stx scope))
            (define-values (state*** o-unmarked)
              ((CompState-expand state**) state** (Env-set-stops env stops #:comp-state state**) i-unmarked))
            (define o-marked (Stx-mark o-unmarked scope))
            (values state*** o-marked))))
    ((App (list (PrimOp 'lexpand) _ ...))
     (error "lexpand takes two arguments" ast))
    ((App (list (? PrimOp? op) #{args : (Listof Ast)} ...))
     (define-values (state* reverse-arg-vals)
       (for/fold ((state : CompState state)
                  (reverse-arg-vals : (Listof Val) '()))
                 ((arg : Ast args))
         (define-values (#{state* : CompState} #{arg-val : Val})
           (Ast-eval arg ast-env state env scope #:phase ph))
         (values state* (cons arg-val reverse-arg-vals))))
     (Prim-eval op (reverse reverse-arg-vals) state*))
    ((App (list op-ast args ...))
     (match/values (Ast-eval op-ast ast-env state env scope #:phase ph)
       ((state* (? (make-predicate (U Closure PrimOp)) op))
        (Ast-eval (App (cons op args)) ast-env state* env scope #:phase ph))
       ((state* op)
        (error "Ast-eval: operator must be a Closure or PrimOp" op))))
    ((Var name)
     (let loop ((ast-env ast-env))
       (if (pair? ast-env)
           (let ((binding (car ast-env)))
             (if (eq? name (Var-name (car binding)))
                 (values state (cadr binding))
                 (loop (cdr ast-env))))
           (error "Ast-eval: variable not in current environment" name))))
    ((? Fun? fn)
     (values state (Closure fn ast-env)))
    ((? Val? val)
     (values state val))))

(: Prim-eval (-> PrimOp (Listof Val) CompState (Values CompState Val)))

(define (Prim-eval op args state)
  (match* (op args)
    (((PrimOp 'cons) (list elem (Seq elems ...)))
     (values state (list->Seq (cons elem elems))))
    (((PrimOp 'car) (list (Seq elems ...)))
     (values state (car elems)))
    (((PrimOp 'cdr) (list (Seq elems ...)))
     (values state (list->Seq (cdr elems))))
    (((PrimOp 'list-ref) (list (Seq elems ...) (? (make-predicate Integer) index)))
     (values state (list-ref elems index)))
    (((PrimOp 'list) _)
     (values state (list->Seq args)))
    (((PrimOp 'stx-e) (list (Stx val ctx)))
     (values state val))
    (((PrimOp 'mk-stx) (list (? Exp? val) (Stx _ ctx)))
     (values state (Stx val ctx)))
    (((PrimOp 'mk-stx) (list val (Stx _ ctx)))
     (error "mk-stx: bad expression:" val))
    (((PrimOp 'mk-stx) (list (? Exp? val) ctx))
     (error "mk-stx: bad context:" ctx))
    (((PrimOp 'mk-stx) args)
     (error "mk-stx: requires an expression and a context" args))
    (((PrimOp '+) (list (? (make-predicate Integer) x) (? (make-predicate Integer) y)))
     (values state (+ x y)))
    ((_ _)
     (error "Prim-eval bad primitive form" op args))))

(: Ast-apply-values
   (-> Closure (Listof Val) CompState Env (U Scope #f) #:phase Phase (Values CompState Val)))

(define (Ast-apply-values closure args state env scope #:phase ph)
  ;; NOTE: this does not evaluate the operator or the args.
  (match closure
    ((Closure (Fun (list vars ...) body) ast-env)
     (unless (= (length vars) (length args))
       (error "Ast-apply: wrong number of arguments" vars args))
     (define ast-env* : AstEnv
       (for/fold ((ast-env ast-env))
                 ((var vars) (arg args))
         (cons (list var arg) ast-env)))
     (Ast-eval body ast-env* state env scope #:phase ph))))

(module+ test
  (require
   typed/rackunit
   "scanner.rkt"
   "initial-state.rkt"
   )

  (: transform Transform)
  (define (transform x y z #:phase phase #:prune scopes)
    (error "null transform for eval.rkt tests."))

  (define-values (initial-eval-env initial-expand-env initial-state)
    (make-default-initial-state
     #:expand transform
     #:quote transform
     #:syntax transform
     #:lambda transform
     #:let-syntax transform))

  (define (check-Ast-eval i o)
    (define-values (state result)
      (Ast-eval (Ast-scan i) initial-eval-env initial-state (empty-Env) #f #:phase 0))
    (check-true (equal? result (scan o))))

  (check-Ast-eval 'cons
                  '#%cons)
  (check-Ast-eval '((#%val (#%fun (y) y)) 1)
                  1)
  (check-Ast-eval '((#%val #%list-ref) (#%val (a b c)) 0)
                  'a)
  (check-Ast-eval '(list-ref (#%val (a b c)) 0)
                  'a)
  (check-Ast-eval '(list-ref (list (#%val a) (#%val b) (#%val c)) 0)
                  'a)
  (check-Ast-eval '(car (list 1 2))
                  '1)
  (check-Ast-eval '(cdr (cons 1 (list 2)))
                  '(2))
  (check-Ast-eval '(cdr (list 1 2))
                  '(2))
  (check-Ast-eval '(list)
                  '())
  (check-Ast-eval '((#%val (#%closure (#%fun () x) ((x 5)))))
                  5)
  (check-Ast-eval '((#%val (#%fun (y) ((#%val (#%fun (x) y)) 0))) 1)
                  1)

  ;; syntax-objects:

  (check-Ast-eval '(mk-stx 1 (#%val (#%stx 2)))
                  '(#%stx 1))

  (check-Ast-eval '(stx-e (mk-stx 1 (#%val (#%stx 2))))
                  '1)
  )
