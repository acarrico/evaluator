#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "binding.rkt"
         "core-lang.rkt"
         "env.rkt"
         "eval.rkt"
         "scope.rkt"
         )

(provide expand)

(: expand-macro
   (-> Closure CompState Env Stx #:phase Phase #:prune SetofScopes
       (Values CompState Stx)))

(define (expand-macro transform state env i #:phase ph #:prune scopes)
  (define-values (state* scope-use-site)
    (CompState-fresh-scope state))
  (define-values (state** scope-introduced)
    (CompState-fresh-scope state*))
  (define-values (state*** o)
    (Ast-apply-values transform
                      (list (Stx-flip-scope
                             (Stx-add-scope i scope-use-site #:phase ph)
                             scope-introduced #:phase ph))
                      state**
                      env
                      ;; ************* TEMP **************
                      #; mark
                      #f
                      #:phase ph
                      ))
  (if (Stx? o)
      ;; Mark output after applying the transformer:
      (expand state*** env (Stx-flip-scope o scope-introduced #:phase ph) #:phase ph #:prune scopes)
      (error "expand: macro transformer did not return syntax" o)))

(: expand-list
   (-> CompState Env (Listof Stx) #:phase Phase #:prune SetofScopes
       (Values CompState (Listof Stx))))
(define (expand-list initial-state env stxes #:phase ph #:prune scopes)
  (define-values (state rev-expanded-stxes)
    (for/fold ((state initial-state)
               (rev-expanded-stxes : (Listof Stx) '()))
              ((stx stxes))
      : (values CompState (Listof Stx))
      (define-values (next-state expanded-stx)
        (expand state env stx #:phase ph #:prune scopes))
      (values next-state (cons expanded-stx rev-expanded-stxes))))
  (values state (reverse rev-expanded-stxes)))

(: expand/id
   (-> CompState Env Stx Id #:phase Phase #:prune SetofScopes
       (Values CompState Stx)))
(define (expand/id initial-state env i dispatch-id
                   #:phase ph #:prune scopes)
  (define name (Binding-name (CompState-resolve-id initial-state dispatch-id #:phase ph)))
  (match (Env-ref env name)
    ((TransformBinding transform)
     (transform initial-state env i #:phase ph #:prune scopes))
    ((ValBinding (and (Closure (Fun (list _) _) _) transform))
     (expand-macro transform initial-state env i #:phase ph #:prune scopes))
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
          (expand-list initial-state env args #:phase ph #:prune scopes))
        (values state (Stx (list->Seq (cons id expanded-args)) ctx)))))
    ((Unbound)
     (error "expand: unbound identifier" i name))))

(: expand Transform)
(define (expand initial-state env i
                #:phase ph
                #:prune scopes)
  (match i
    ((Id id)
     (expand/id initial-state env i id #:phase ph #:prune scopes))
    ((Form (Id id) _ ...)
     (expand/id initial-state env i id #:phase ph #:prune scopes))
    ;; expand subforms:
    ((Stx (Seq #{stxes : (Listof Stx)} ...) ctx)
     (define-values (state expanded-stxes)
       (expand-list initial-state env stxes #:phase ph #:prune scopes))
     (values state (Stx (list->Seq expanded-stxes) ctx)))
    ;; not accepting other syntax (for now):
    (_
     (error "expand: unrecognized form" i))))
