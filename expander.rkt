#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "core-lang.rkt"
         "parser.rkt")

(provide Env CompState TransformBinding
         VarBinding
         fun-transform
         quote-transform
         let-syntax-transform
         expand)

(define-type Transform (-> CompState Env Stx (Values CompState Stx)))
(struct TransformBinding ((transform : Transform)) #:transparent)
(struct ValBinding ((val : Val)) #:transparent)
(struct VarBinding ((id : Stx)) #:transparent)
(define-type Binding (U TransformBinding ValBinding VarBinding))
(define-type Env (Listof (List Symbol Binding)))
(struct CompState ((next-fresh : Natural)
                   (eval-env : AstEnv))
  #:transparent)

(define (build-fresh-name (name : Symbol) (index : Natural)) : Symbol
  (string->symbol
   (format "#%~a-~a"
           index
           (match (symbol->string name)
             ((regexp #px"\\#\\%(\\d+)-(.*)" (list _ _ base)) base)
             (base base)))))

(define (CompState-fresh-name (state : CompState) (name : Symbol))
  : (Values CompState Symbol)
  (define index (CompState-next-fresh state))
  (values
   (struct-copy CompState state (next-fresh (+ index 1)))
   (build-fresh-name name index)))

(define (CompState-fresh-names (state : CompState) (names : (Listof Symbol)))
  : (Values CompState (Listof Symbol))
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
    ((StxSeq _ (ResolvedId name) rhs body)
     (define transformer (Ast-eval (parse rhs) (CompState-eval-env state)))
     (define env* (cons (list name (ValBinding transformer)) env))
     (expand state env* body))))

(: fun-transform Transform)
(define (fun-transform state env i)
  (match i
    ((Stx (Seq (list lambda-id
                     (and (Stx _ vars-ctx)
                          (StxSeq
                           (ResolvedId #{resolved-names : (Listof Symbol)})
                           ...))
                     body))
          outer-ctx)
     #:when (distinct-names? resolved-names)
     ;; Rename ids, extend the environment, expand the body:
     (define-values (state* new-names) (CompState-fresh-names state resolved-names))
     (define new-ids : (Listof Stx)
       (map (lambda ((name : Symbol)) (Stx (Sym name) empty-context))
            new-names))
     (define new-env : Env
       (for/fold ((new-env env))
                 ((resolved-name resolved-names) (new-id new-ids))
         (cons (list resolved-name (VarBinding new-id)) new-env)))
     (define-values (state** new-body)
       (expand state* new-env body))
     ;; Construct the output:
     (values
      state**
      (Stx (Seq (list lambda-id (Stx (Seq new-ids) vars-ctx) new-body)) outer-ctx)))
    (_
     (error
      "expand: lambda requires two subforms, a list of distinct vars and a body"
      i))))

(: quote-transform Transform)
(define (quote-transform state env i)
  (match i
    ((StxSeq _ _)
     (values state i))
    (_
     (error "expand: quote requires exactly one subform" i))))

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
        (define o (Ast-apply-values transform (list i)))
        (if (Stx? o)
            (expand initial-state env o)
            (error "expand: macro transformer did not return syntax" o)))
       ((ValBinding _)
        (error "expand: arbitrary ValBinding not supported."))
       ((VarBinding id)
        (match i
          ;; Lone variable reference:
          ((Id _)
           (values initial-state id))
          ;; Variable reference is the operator in a sequence:
          ((Stx (Seq (list _ #{args : (Listof Stx)} ...)) ctx)
           (define-values (state expanded-args) (expand-list initial-state env args))
           (values state (Stx (Seq (cons id expanded-args)) ctx)))))))
    (_
     (error "expand: unbound identifier" i name))))

(: expand Transform)
(define (expand initial-state env i)
  (match i
    ;; dispatch on name: ISSUE: can't get 'or' pattern to typecheck here:
    ((ResolvedId #{name : Symbol})
     (expand/name initial-state env i name))
    ((StxSeq (ResolvedId #{name : Symbol}) _ ...)
     (expand/name initial-state env i name))
    ;; expand subforms:
    ((Stx (Seq (list #{stxes : (Listof Stx)} ...)) ctx)
     (define-values (state expanded-stxes)
       (expand-list initial-state env stxes))
     (values state (Stx (Seq expanded-stxes) ctx)))
    ;; not accepting other syntax (for now):
    (_
     (error "expand: unrecognized form" i))))
