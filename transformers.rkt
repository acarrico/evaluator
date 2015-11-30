#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "scope.rkt"
         "binding.rkt"
         "core-lang.rkt"
         "env.rkt"
         "eval.rkt"
         "expander.rkt")

(provide fun-transform
         quote-transform
         syntax-transform
         let-syntax-transform
         )

(define (build-fresh-name (name : (U Sym Id)) (index : Natural)) : Sym
  (define sym (match name ((Sym s) s) ((Stx (Sym s) _) s)))
  (Sym
   (string->symbol
    (format "#%~a-~a"
            index
            (match (symbol->string sym)
              ((regexp #px"\\#\\%(\\d+)-(.*)" (list _ _ base)) base)
              (base base))))))

(define (CompState-fresh-name (state : CompState) (name : (U Sym Id)))
  : (Values CompState Sym)
  (define index (CompState-next-fresh state))
  (values
   (struct-copy CompState state (next-fresh (+ index 1)))
   (build-fresh-name name index)))

(define (CompState-fresh-names (state : CompState) (names : (Listof (U Sym Id))))
   : (Values CompState (Listof Sym))
  (define start (CompState-next-fresh state))
  (values
   (struct-copy CompState state (next-fresh (+ start (length names))))
   (for/list ((index (in-naturals start))
              (name names))
     ;; ISSUE: for/list somehow ends up with Integer for index, see
     ;; Racket bug number 13287, so cast for now:
     (build-fresh-name name (cast index Natural)))))

(: let-syntax-transform Transform)
(define (let-syntax-transform state env i #:phase ph #:prune scopes)
  (match i
    ((Form _ (Id id) rhs body)

     (define-values (state* new-scope)
       (CompState-fresh-scope state))

     (define-values (state** new-name)
       (CompState-fresh-name state* id))

     (define new-id
       (Id-add-scope id new-scope #:phase ph))

     (define state***
       (CompState-bind-id state** new-id (Binding (Sym-name new-name))
                          #:hint new-scope #:phase ph))

     (define expand-phase (+ ph 1))

     (define-values (state**** expanded-rhs)
       (expand state*** (CompState-expand-env state***) rhs
               #:phase expand-phase #:prune (empty-SetofScopes)))

     (define-values (state***** transformer)
       (Ast-eval (CompState-parse state**** expanded-rhs #:phase expand-phase)
                 (CompState-eval-env state****) state**** env #f #:phase ph))

     (define env*
       (Env-set env (Sym-name new-name) (ValBinding transformer)))

     (define body*
       (Stx-add-scope body new-scope #:phase ph))

     (define scopes* (SetofScopes-add scopes new-scope))

     (expand state***** env* body* #:phase ph #:prune scopes*))))

(: fun-transform Transform)
(define (fun-transform state env i #:phase ph #:prune scopes)
  (match i
    ((Stx (Seq lambda-id
               (Stx (Seq (Id #{ids : (Listof Id)}) ...) var-list-ctx)
               body)
          outer-ctx)

     (unless (bound-identifiers-distinct? ids #:phase ph)
       (error "expand: lambda requires distinct vars" i))

     (define-values (state* new-scope)
       (CompState-fresh-scope state))

     (define-values (state** new-names)
       (CompState-fresh-names state* ids))

     (define new-ids : (Listof Id)
       (for/list ((id ids)) (Id-add-scope id new-scope #:phase ph)))

     (define state*** : CompState
       (for/fold ((state state**))
                 ((new-id new-ids)
                  (new-name new-names))
         (CompState-bind-id state new-id (Binding (Sym-name new-name))
                            #:hint new-scope
                            #:phase ph)))

     (define env* : Env
       (for/fold ((env env))
                 ((new-id new-ids)
                  (new-name new-names))
         (Env-set env (Sym-name new-name) (VarBinding new-id))))

     (define-values (state**** body*)
       (expand state*** env* (Stx-add-scope body new-scope #:phase ph) #:phase ph #:prune scopes))

     ;; Construct the output:
     (values
      state****
      (Stx (Seq lambda-id
                (Stx (list->Seq new-ids) var-list-ctx)
                body*)
           outer-ctx)))

    (_
     (error
      "expand: lambda requires two subforms, a list of distinct vars and a body"
      i))))

(: quote-transform Transform)
(define (quote-transform state env i #:phase ph #:prune scopes)
  (match i
    ((Form _ _)
     (values state i))
    (_
     (error "expand: quote requires exactly one subform" i))))

(: prune (-> Stx SetofScopes #:phase Phase Stx))
(define (prune stx scopes #:phase ph)
  ;; ISSUE: would a bulk remove-scope be better?
  (Stx-remove-scopes stx scopes #:phase ph))

(: syntax-transform Transform)
(define (syntax-transform state env i #:phase ph #:prune scopes)
  (match i
    ((Stx (Seq id-stx quoted-stx) ctx)
     (define pruned-stx (prune quoted-stx scopes #:phase ph))
     (define pruned-form (Stx (list->Seq (list id-stx pruned-stx)) ctx))
     (values state pruned-form))
    (_
     (error "expand: syntax requires exactly one subform" i))))

(module+ test
  (require
   typed/rackunit
   "scanner.rkt"
   "initial-state.rkt")

  (define-values (initial-eval-env initial-expand-env initial-state)
    (make-default-initial-state
     #:expand expand
     #:quote quote-transform
     #:syntax syntax-transform
     #:lambda fun-transform
     #:let-syntax let-syntax-transform))

  (define (check-expand i o)
    (define-values (state expanded)
      (expand initial-state initial-expand-env (Stx-scan i) #:phase 0 #:prune (empty-SetofScopes)))
    (check Ast-equal? (CompState-parse state expanded #:phase 0) o))

  (define (check-re-expand i o)
    (define-values (state expanded)
      (expand initial-state initial-expand-env (Stx-scan i) #:phase 0 #:prune (empty-SetofScopes)))
    (define-values (state* expanded*)
      (expand state initial-expand-env expanded #:phase 0 #:prune (empty-SetofScopes)))
    (check Ast-equal? (CompState-parse state* expanded* #:phase 0) o))

  (check-expand '(lambda (x) x)
                (Fun (list (Var 'x)) (Var 'x)))

  (check-expand '(lambda (lambda) lambda)
                (Fun (list (Var '#%0-lambda)) (Var '#%0-lambda)))

  (check-expand '(lambda (x) (lambda (x) x))
                (Fun (list (Var '#%0-x)) (Fun (list (Var '#%1-x)) (Var '#%1-x))))

  (check-expand '(lambda (x) (lambda (y) (x y)))
                (Fun (list (Var '#%0-x))
                     (Fun (list (Var '#%1-y))
                          (App (list (Var '#%0-x) (Var '#%1-y))))))

  (check-expand '(quote x)
                (Sym 'x))

  ;; With the hygienic expander, we easily can't check literal syntax
  ;; anymore:
  #;(check-expand '(syntax x) (Stx (Sym 'x) (EmptyCtx)))

  (check-expand '(lambda (lambda) 'lambda)
                (Fun (list (Var '#%0-lambda)) (Sym 'lambda)))

  #;(check-expand '(lambda (lambda) #'lambda)
      (Fun (list (Var '#%0-lambda)) (Stx (Sym 'lambda) (EmptyCtx))))

  ;; test idempotence:

  (check-re-expand '(lambda (x) x)
                   (Fun (list (Var '#%1-x)) (Var '#%1-x)))

  (check-re-expand '(lambda (lambda) lambda)
                   (Fun (list (Var '#%1-lambda)) (Var '#%1-lambda)))

  (check-re-expand '(lambda (x) (lambda (x) x))
                   (Fun (list (Var '#%2-x)) (Fun (list (Var '#%3-x)) (Var '#%3-x))))

  (check-re-expand '(lambda (x) (lambda (y) (x y)))
                   (Fun (list (Var '#%2-x))
                        (Fun (list (Var '#%3-y))
                             (App (list (Var '#%2-x) (Var '#%3-y))))))

  (check-re-expand '(quote x)
                   (Sym 'x))

  #;(check-re-expand '(syntax x)
  (Stx (Sym 'x) (EmptyCtx)))

  (check-re-expand '(lambda (lambda) 'lambda)
                   (Fun (list (Var '#%1-lambda)) (Sym 'lambda)))

  #;(check-re-expand '(lambda (lambda) #'lambda)
  (Fun (list (Var '#%1-lambda)) (Stx (Sym 'lambda) (EmptyCtx))))

  )
