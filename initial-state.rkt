#lang typed/racket/base

(require
 racket/match
 "binding.rkt"
 "core-lang.rkt"
 "scanner.rkt"
 "env.rkt"
 )

(provide
 ;; -> (values initial-eval-env initial-expand-env initial-state)
 make-default-initial-state
 )

(: extend-envs
   (-> (Listof (List Symbol Any)) AstEnv Env (Values AstEnv Env)))
(define (extend-envs src-bindings eval-env expand-env)
  (for/fold ((#{eval-env : AstEnv} eval-env)
             (#{expand-env : Env} expand-env))
            ((#{src-binding : (List Symbol Any)} src-bindings))
    (match src-binding
      ((list name val)
       (values
        (cons (list (Var name) (scan val)) eval-env)
        (Env-set expand-env name (VarBinding (Stx (Sym name) (EmptyCtx)))))))))

(: make-initial-state
   (-> #:src-bindings (Listof (List Symbol Any))
       #:t-src-bindings (Listof (List Symbol Any))
       #:expand Transform
       #:quote Transform
       #:syntax Transform
       #:lambda Transform
       #:let-syntax Transform
       (Values AstEnv Env CompState)))

(define (make-initial-state
         ;; Initial bindings available during evaluation:
         #:src-bindings src-bindings
         ;; More bindings only available during the application of a
         ;; macro transformer:
         #:t-src-bindings t-src-bindings
         #:expand expand-transform
         #:quote quote-transform
         #:syntax syntax-transform
         #:lambda lambda-transform
         #:let-syntax let-syntax-transform)
  (define core-expand-env : Env
    (for/fold ((env (empty-Env)))
              ((entry : (List Symbol CompileTimeBinding)
                      (list (list 'lambda (TransformBinding lambda-transform))
                            (list 'quote (TransformBinding quote-transform))
                            (list 'syntax (TransformBinding syntax-transform))
                            (list 'let-syntax (TransformBinding let-syntax-transform)))))
      (match entry ((list name binding)
                    (Env-set env name binding)))))
  (define-values (eval-env expand-env)
    (extend-envs src-bindings '() core-expand-env))
  (define-values (t-eval-env t-expand-env)
    (extend-envs t-src-bindings eval-env expand-env))
  (values eval-env
          expand-env
          (CompState 0 (empty-BindingTable) t-eval-env t-expand-env expand-transform)))

;; NOTE: expand and the transformers are passed in to break a cycle
;; with the eval.rkt tests (which don't use an expander anyway):
(: make-default-initial-state
   (-> #:expand Transform
       #:quote Transform
       #:syntax Transform
       #:lambda Transform
       #:let-syntax Transform
       (Values AstEnv Env CompState)))

(define (make-default-initial-state #:expand expand-transform
                                    #:quote quote-transform
                                    #:syntax syntax-transform
                                    #:lambda lambda-transform
                                    #:let-syntax let-syntax-transform)
  (make-initial-state #:src-bindings '((cons #%cons)
                                       (car #%car)
                                       (cdr #%cdr)
                                       (list-ref #%list-ref)
                                       (list #%list)
                                       (stx-e #%stx-e)
                                       (mk-stx #%mk-stx)
                                       (+ #%+))
                      #:t-src-bindings '((lvalue #%lvalue)
                                         (lexpand #%lexpand))
                      #:expand expand-transform
                      #:quote quote-transform
                      #:syntax syntax-transform
                      #:lambda lambda-transform
                      #:let-syntax let-syntax-transform))
