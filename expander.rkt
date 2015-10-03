#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "binding.rkt"
         "core-lang.rkt"
         "env.rkt"
         "eval.rkt"
         )

(provide expand)

(define (expand-macro (transform : Closure) (state : CompState) (env : Env) (i : Stx))
  : (Values CompState Stx)
  (define-values (state* scope-use-site)
    (CompState-fresh-scope state))
  (define-values (state** scope-introduced)
    (CompState-fresh-scope state*))
  (define-values (state*** o)
    (Ast-apply-values transform
                      (list (Stx-flip-scope (Stx-add-scope i scope-use-site) scope-introduced))
                      state**
                      env
                      ;; ************* TEMP **************
                      #; mark
                      #f
                      ))
  (if (Stx? o)
      ;; Mark output after applying the transformer:
      (expand state*** env (Stx-flip-scope o scope-introduced))
      (error "expand: macro transformer did not return syntax" o)))

(define (expand-list (initial-state : CompState) (env : Env) (stxes : (Listof Stx)))
  : (Values CompState (Listof Stx))
  (define-values (state rev-expanded-stxes)
    (for/fold ((state initial-state)
               (rev-expanded-stxes : (Listof Stx) '()))
              ((stx stxes))
      : (values CompState (Listof Stx))
      (define-values (next-state expanded-stx) (expand state env stx))
      (values next-state (cons expanded-stx rev-expanded-stxes))))
  (values state (reverse rev-expanded-stxes)))

(define (expand/id (initial-state : CompState) (env : Env) (i : Stx) (dispatch-id : Id))
  : (Values CompState Stx)
  (define name (Binding-name (CompState-resolve-id initial-state dispatch-id)))
  (match (Env-ref env name)
    ((TransformBinding transform)
     (transform initial-state env i))
    ((ValBinding (and (Closure (Fun (list _) _) _) transform))
     (expand-macro transform initial-state env i))
    ((ValBinding _)
     (error "expand: arbitrary ValBinding not supported."))
    ((VarBinding id)
     (match i
       ;; Lone variable reference:
       ((Id _)
        (values initial-state id))
       ;; Variable reference is the operator in a sequence:
       ((Stx (Seq _ #{args : (Listof Stx)} ...) ctx)
        (define-values (state expanded-args)
          (expand-list initial-state env args))
        (values state (Stx (list->Seq (cons id expanded-args)) ctx)))))
    ((Unbound)
     (error "expand: unbound identifier" i name))))

(: expand Transform)
(define (expand initial-state env i)
  (match i
    ((Id id)
     (expand/id initial-state env i id))
    ((Form (Id id) _ ...)
     (expand/id initial-state env i id))
    ;; expand subforms:
    ((Stx (Seq #{stxes : (Listof Stx)} ...) ctx)
     (define-values (state expanded-stxes)
       (expand-list initial-state env stxes))
     (values state (Stx (list->Seq expanded-stxes) ctx)))
    ;; not accepting other syntax (for now):
    (_
     (error "expand: unrecognized form" i))))
