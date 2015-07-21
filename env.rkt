#lang typed/racket/base

(require racket/match
         "core-lang.rkt")
(provide
 (struct-out CompState)
 Transform
 (struct-out Unbound)
 (struct-out TransformBinding)
 (struct-out ValBinding)
 (struct-out VarBinding)
 Binding
 empty-Env Env Env-set Env-ref)

(define-type Transform (-> CompState Env Stx (Values CompState Stx)))
(struct Unbound ())
(struct TransformBinding ((transform : Transform)) #:transparent)
(struct ValBinding ((val : Val)) #:transparent)
(struct VarBinding ((id : Stx)) #:transparent)
(define-type Binding (U TransformBinding ValBinding VarBinding Unbound))
 ;; compile-time environment:
(define-type Env (HashTable Symbol Binding))
(struct CompState ((next-fresh : Natural)
                   ;; evaluation environment for macro expanders:
                   (eval-env : AstEnv)
                   )
  #:transparent)

(define (empty-Env) : Env (hasheq))

(define (Env-set (env : Env) (name : Symbol) (binding : Binding)) : Env
  (hash-update
   env
   name
   (lambda (old)
     (if (Unbound? old)
         binding
         (error "Env-set: symbol already in env" name env)))
   (lambda () (Unbound))))

(define (Env-ref (env : Env) (name : Symbol)) : Binding
  (hash-ref env name Unbound))
