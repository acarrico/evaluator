#lang typed/racket/base

(require racket/match
         "core-lang.rkt")
(provide
 (struct-out CompState)
 Transform
 (struct-out TransformBinding)
 (struct-out ValBinding)
 (struct-out VarBinding)
 Binding
 Env)

(define-type Transform (-> CompState Env Stx (Values CompState Stx)))
(struct TransformBinding ((transform : Transform)) #:transparent)
(struct ValBinding ((val : Val)) #:transparent)
(struct VarBinding ((id : Stx)) #:transparent)
(define-type Binding (U TransformBinding ValBinding VarBinding))
 ;; compile-time environment:
(define-type Env (Listof (List Symbol Binding)))
(struct CompState ((next-fresh : Natural)
                   ;; evaluation environment for macro expanders:
                   (eval-env : AstEnv)
                   )
  #:transparent)
