#lang typed/racket/base

(require racket/match
         racket/set
         "core-lang.rkt"
         "scope.rkt"
         "binding.rkt"
         "parser.rkt")
(provide
 (struct-out CompState) CompState-fresh-scope CompState-resolve-id CompState-bind-id CompState-parse
 Transform
 (struct-out Unbound)
 (struct-out TransformBinding)
 (struct-out ValBinding)
 (struct-out VarBinding)
 CompileTimeBinding
 empty-Env Env Env-set Env-ref
 Stops? Env-set-stops)

(define-type Stops (Seq Id))
(define Stops? (make-predicate Stops))

(define-type Transform
  (-> CompState Env Stx #:phase Phase #:prune SetofScopes
      (Values CompState Stx)))

(struct Unbound ())
(struct TransformBinding ((transform : Transform)) #:transparent)
(struct ValBinding ((val : Val)) #:transparent)
(struct VarBinding ((id : Stx)) #:transparent)
(define-type CompileTimeBinding (U TransformBinding ValBinding VarBinding Unbound))

;; compile-time environment:
(struct Env ((table : (HashTable Symbol CompileTimeBinding)) (stops : (Setof Symbol))) #:transparent)

(struct CompState ((next-fresh : Natural)
                   (binding-table : BindingTable)
                   ;; evaluation environment for macro expanders:
                   (eval-env : AstEnv)
                   ;; expansion environment for macro expanders:
                   (expand-env : Env)
                   ;; NOTE: The expander function is here primarily to deal
                   ;; with a circular dependency among eval and
                   ;; expand.
                   (expand : Transform)
                   )
  #:transparent)

(define (CompState-fresh-scope (state : CompState))
  : (Values CompState Scope)
  (define index (CompState-next-fresh state))
  (values
   (struct-copy CompState state (next-fresh (+ index 1)))
   (Scope index)))

(: CompState-resolve-id
   (-> CompState Id #:phase Phase Binding))
(define (CompState-resolve-id state id #:phase ph)
  (Id-resolve id #:phase ph #:bindings (CompState-binding-table state)))

(: CompState-bind-id
   (-> CompState Id Binding #:hint Scope #:phase Phase CompState))
(define (CompState-bind-id state id binding #:hint hint #:phase ph)
  (define new-binding-table
    (Id-bind (CompState-binding-table state) id binding #:hint hint #:phase ph))
  (struct-copy
   CompState state
   (binding-table new-binding-table)))

(: CompState-parse (-> CompState Stx #:phase Phase Ast))
(define (CompState-parse state i #:phase ph)
  (parse i #:bindings (CompState-binding-table state) #:phase ph))

(define (empty-Env) : Env (Env (hasheq) (seteq)))

(define (Env-set (env : Env) (binding : Symbol) (compile-time-binding : CompileTimeBinding)) : Env
  (define new-table
    (hash-update
     (Env-table env)
     binding
     (lambda (old)
       (if (Unbound? old)
           compile-time-binding
           (error "Env-set: binding already in env" binding env)))
     (lambda () (Unbound))))

  (struct-copy Env env (table new-table)))

(: stop-transform Transform)
(define (stop-transform state env i #:phase phase #:prune scopes)
  (values state i))

(define stop-transform-binding
  (TransformBinding stop-transform))

(define (Env-ref (env : Env) (binding : Symbol)) : CompileTimeBinding
  (if (set-member? (Env-stops env) binding)
      stop-transform-binding
      (hash-ref (Env-table env) binding Unbound)))

(: Env-set-stops (-> Env Stops #:comp-state CompState #:phase Phase Env))
(define (Env-set-stops env stops #:comp-state state #:phase ph)
  (define new-Stops : (Setof Symbol)
    (match stops
      ((Seq (Id #{ids : (Listof Id)}) ...)
       (define names : (Listof Symbol)
         (for/list ((id : Id ids))
           (Binding-name (CompState-resolve-id state id #:phase ph))))
       (list->seteq names))))
  (struct-copy Env env (stops new-Stops)))
