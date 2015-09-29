#lang typed/racket/base

(require
 racket/match
 "core-lang.rkt"
 "scanner.rkt"
 "env.rkt"
 "expander.rkt"
 )

(provide
 initial-eval-env
 initial-expand-env
 initial-state
 )

(define (make-initial-state
         ;; Initial bindings available during evaluation:
         (src-bindings : (Listof (List Symbol Any)))
         ;; More bindings only available during the application of a
         ;; macro transformer:
         (t-src-bindings : (Listof (List Symbol Any))))
  : (Values AstEnv Env CompState)
  (define core-expand-env : Env
    (for/fold ((env (empty-Env)))
              ((entry : (List Symbol Binding)
                      (list (list 'lambda (TransformBinding fun-transform))
                            (list 'quote (TransformBinding quote-transform))
                            (list 'syntax (TransformBinding quote-transform))
                            (list 'let-syntax (TransformBinding let-syntax-transform)))))
      (match entry ((list name binding)
                    (Env-set env name binding)))))
  (define-values (eval-env expand-env)
    (for/fold ((#{eval-env : AstEnv} '())
               (#{expand-env : Env} core-expand-env))
              ((#{src-binding : (List Symbol Any)} src-bindings))
      (match src-binding
        ((list name val)
         (values
          (cons (list (Var name) (scan val)) eval-env)
          (Env-set expand-env name (VarBinding (Stx (Sym name) (EmptyCtx)))))))))
  (define t-eval-env
    (for/fold ((#{t-eval-env : AstEnv} eval-env))
              ((#{src-binding : (List Symbol Any)} t-src-bindings))
      (match src-binding
        ((list name val)
          (cons (list (Var name) (scan val)) t-eval-env)))))
  (values eval-env expand-env (CompState 0 t-eval-env expand)))

(define-values (initial-eval-env initial-expand-env initial-state)
  (make-initial-state '((cons #%cons)
                        (car #%car)
                        (cdr #%cdr)
                        (list-ref #%list-ref)
                        (list #%list)
                        (stx-e #%stx-e)
                        (mk-stx #%mk-stx)
                        (+ #%+))
                      '((lvalue #%lvalue)
                        (lexpand #%lexpand))))
