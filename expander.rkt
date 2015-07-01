#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "core-lang.rkt"
         "parser.rkt"
         "env.rkt"
         "eval.rkt")

(provide fun-transform
         quote-transform
         let-syntax-transform
         expand)

(define (build-fresh-name (name : Sym) (index : Natural)) : Sym
  (Sym
   (string->symbol
    (format "#%~a-~a"
            index
            (match (symbol->string (Sym-name name))
              ((regexp #px"\\#\\%(\\d+)-(.*)" (list _ _ base)) base)
              (base base))))))

(define (CompState-fresh-mark (state : CompState))
  : (Values CompState Mark)
  (define index (CompState-next-fresh state))
  (values
   (struct-copy CompState state (next-fresh (+ index 1)))
   index))

(define (CompState-fresh-name (state : CompState) (name : Sym))
  : (Values CompState Sym)
  (define index (CompState-next-fresh state))
  (values
   (struct-copy CompState state (next-fresh (+ index 1)))
   (build-fresh-name name index)))

(define (CompState-fresh-names (state : CompState) (names : (Listof Sym)))
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
(define (let-syntax-transform state env i)
  (match i
    ((Form _ (ResolvedId resolved-name) rhs body)
     (define transformer
       (Ast-eval (parse rhs) (CompState-eval-env state) env))
     (define-values (state* fresh-name)
       (CompState-fresh-name state resolved-name))
     (match-define (list renamed-body)
       (rename-stxes (list resolved-name) (list fresh-name) (list body)))

     (define env* (cons (list (Sym-name fresh-name) (ValBinding transformer)) env))
     (expand state* env* renamed-body))))

(: fun-transform Transform)
(define (fun-transform state env i)
  (match i
    ((Stx (Seq lambda-id
               (and vars
                    (Form
                     (ResolvedId #{resolved-names : (Listof Sym)})
                     ...))
               body)
          outer-ctx)
     #:when (distinct-names? resolved-names)

     ;; Rename variables:
     (define-values (state* fresh-names)
       (CompState-fresh-names state resolved-names))
     (match-define (list renamed-vars renamed-body)
       (rename-stxes resolved-names fresh-names (list vars body)))

     ;; Extend the environment:
     (match-define (Stx (Seq new-ids ...) _) renamed-vars)

     (define new-env : Env
       (for/fold ((new-env env))
                 ((fresh-name fresh-names) (new-id new-ids))
         (cons (list (Sym-name fresh-name) (VarBinding new-id)) new-env)))

     ;; Expand the body:
     (define-values (state** new-body)
       (expand state* new-env renamed-body))

     ;; Construct the output:
     (values
      state**
      (Stx (Seq lambda-id renamed-vars new-body) outer-ctx)))

    (_
     (error
      "expand: lambda requires two subforms, a list of distinct vars and a body"
      i))))

(: quote-transform Transform)
(define (quote-transform state env i)
  (match i
    ((Form _ _)
     (values state i))
    (_
     (error "expand: quote requires exactly one subform" i))))

(define (expand-macro (transform : Closure) (initial-state : CompState) (env : Env) (i : Stx))
  : (Values CompState Stx)
  (define-values (next-state mark) (CompState-fresh-mark initial-state))
  ;; Mark input before applying the transformer:
  (define o (Ast-apply-values transform (list (Stx-mark i mark)) env))
  (if (Stx? o)
      ;; Mark output after applying the transformer:
      (expand next-state env (Stx-mark o mark))
      (error "expand: macro transformer did not return syntax" o)))

(define (expand-list (initial-state : CompState) (env : Env) (stxes : (Listof Stx)))
  (define-values (state rev-expanded-stxes)
    (for/fold ((state initial-state)
               (rev-expanded-stxes : (Listof Stx) '()))
              ((stx stxes))
      : (values CompState (Listof Stx))
      (define-values (next-state expanded-stx) (expand state env stx))
      (values next-state (cons expanded-stx rev-expanded-stxes))))
  (values state (reverse rev-expanded-stxes)))

(define (expand/name (initial-state : CompState) (env : Env) (i : Stx) (name : Symbol))
  : (Values CompState Stx)
  (match (assq name env)
    ((list _ binding)
     (match binding
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
           (define-values (state expanded-args) (expand-list initial-state env args))
           (values state (Stx (list->Seq (cons id expanded-args)) ctx)))))))
    (_
     (error "expand: unbound identifier" i name))))

(: expand Transform)
(define (expand initial-state env i)
  (match i
    ;; dispatch on name: ISSUE: can't get 'or' pattern to typecheck here:
    ((ResolvedId (Sym #{name : Symbol}))
     (expand/name initial-state env i name))
    ((Form (ResolvedId (Sym #{name : Symbol})) _ ...)
     (expand/name initial-state env i name))
    ;; expand subforms:
    ((Stx (Seq #{stxes : (Listof Stx)} ...) ctx)
     (define-values (state expanded-stxes)
       (expand-list initial-state env stxes))
     (values state (Stx (list->Seq expanded-stxes) ctx)))
    ;; not accepting other syntax (for now):
    (_
     (error "expand: unrecognized form" i))))
