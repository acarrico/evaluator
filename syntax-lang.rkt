#lang typed/racket/base

(require racket/match
         (for-syntax typed/racket/base))

(provide
 (struct-out Seq)
 (struct-out Sym)
 Atom Atom?
 Exp Exp?
 Ctx Ctx? EmptyCtx EmptyCtx?
 Stx Stx?
 Id Id? ResolvedId
 Form Form?
 )

(struct (T) Seq ((elems : (Listof T))) #:transparent)

(struct Sym ((name : Symbol)) #:transparent)

(define-type Atom (U Sym Integer))
(define-type Exp (U Atom (Seq Stx)))
(struct EmptyCtx () #:transparent)
(define-type Ctx (U EmptyCtx))
;; The context applies to every node in the expression:
(struct (T) LazyStx ((strict : (StrictStx T)) (ctx : Ctx)) #:transparent)
;; The context applies to the top node of the expression:
(struct (T) StrictStx ((exp : T) (ctx : Ctx)) #:transparent)
(define-type StxOf (All (T) (U (LazyStx T) (StrictStx T))))
(define-type Stx (StxOf Exp) #:omit-define-syntaxes)

(define-type Id (StxOf Sym) #:omit-define-syntaxes)
(define-type Form (StxOf (Seq Stx)) #:omit-define-syntaxes)

(define Atom? (make-predicate Atom))
(define Exp? (make-predicate Exp))
(define Ctx? (make-predicate Ctx))
(define Stx? (make-predicate Stx))
(define Id? (make-predicate Id))
(define Form? (make-predicate Form))

(define-match-expander Stx
  (lambda (stx)
    (syntax-case stx ()
      ((_ val-pat ctx-pat)
       #'(? Stx? (app Stx->StrictStx (StrictStx val-pat ctx-pat))))))
  (lambda (stx)
    (syntax-case stx ()
      ((_ exp ctx) #'(StrictStx exp ctx))
      (_ #'StxStrict))))

(define (Stx->StrictStx (i : Stx)) : (StrictStx Exp)
  (match i
    ((? StrictStx? o) o)
    ((LazyStx (StrictStx exp ctx-inner) ctx-outer)
     (StrictStx
      (if (list? exp)
          (for/list ((stx exp)) (LazyStx stx ctx-outer))
          exp)
      (Ctx-merge ctx-inner ctx-outer)))))

(define (Ctx-merge (inner : Ctx) (outer : Ctx)) : Ctx
  (error "Ctx-merge: not implemented."))

(define (resolve (stx : Stx)) : Symbol
  (match stx
    ((Stx (Sym name) _) name)
    (_
     (error "resolve: expected symbol syntax"))))

(define-match-expander Id
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat) #'(and (Stx (Sym _) _) pat)))))

(define-match-expander ResolvedId
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat) #'(Id (app resolve pat))))))

(define-match-expander Form
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq (list pat ...)) _)))))
