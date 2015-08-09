#lang typed/racket/base

(require racket/match
         racket/set
         "core-lang.rkt")
(provide
 (struct-out CompState)
 Transform
 (struct-out Unbound)
 (struct-out TransformBinding)
 (struct-out ValBinding)
 (struct-out VarBinding)
 Binding
 empty-Env Env Env-set Env-ref
 Stops? Env-set-stops)

(define-type Stops (Seq Id))
(define Stops? (make-predicate Stops))

(define-type Transform (-> CompState Env Stx (Values CompState Stx)))
(struct Unbound ())
(struct TransformBinding ((transform : Transform)) #:transparent)
(struct ValBinding ((val : Val)) #:transparent)
(struct VarBinding ((id : Stx)) #:transparent)
(define-type Binding (U TransformBinding ValBinding VarBinding Unbound))
;; compile-time environment:
(struct Env ((bindings : (HashTable Symbol Binding)) (stops : (Setof Symbol))) #:transparent)
(struct CompState ((next-fresh : Natural)
                   ;; evaluation environment for macro expanders:
                   (eval-env : AstEnv)
                   ;; NOTE: The expander function is here primarily to deal
                   ;; with a circular dependency among eval and
                   ;; expand.
                   (expand : Transform)
                   )
  #:transparent)

(define (empty-Env) : Env (Env (hasheq) (seteq)))

(define (Env-set (env : Env) (name : Symbol) (binding : Binding)) : Env
  (define new-bindings
    (hash-update
     (Env-bindings env)
     name
     (lambda (old)
       (if (Unbound? old)
           binding
           (error "Env-set: symbol already in env" name env)))
     (lambda () (Unbound))))

  (struct-copy Env env (bindings new-bindings)))

(: stop-transform Transform)
(define (stop-transform state env i)
  (values state i))

(define stop-transform-binding
  (TransformBinding stop-transform))

(define (Env-ref (env : Env) (name : Symbol)) : Binding
  (if (set-member? (Env-stops env) name)
      stop-transform-binding
      (hash-ref (Env-bindings env) name Unbound)))

(define (Env-set-stops (env : Env) (stops : Stops)) : Env
  (define new-Stops : (Setof Symbol)
    (match stops
      ((Seq (ResolvedId (Sym #{names : (Listof Symbol)})) ...)
       (list->seteq names))))
  (struct-copy Env env (stops new-Stops)))
