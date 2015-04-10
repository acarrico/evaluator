#lang typed/racket/base

(require racket/match
         (for-syntax typed/racket/base))

(provide
 (struct-out Seq)
 (struct-out Sym)
 StxAtom StxAtom?
 StxSeq StxSeq?
 StxContent StxContent?
 Ctx Ctx? EmptyCtx EmptyCtx?
 Stx Stx?
 Id
 ResolvedId)

(struct (T) Seq ((elems : (Listof T))) #:transparent)

(struct Sym ((name : Symbol)) #:transparent)

(define-type StxAtom (U Sym Integer) #:omit-define-syntaxes)
(define StxAtom? (make-predicate StxAtom))
(define-match-expander StxAtom
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat)
       #'(Stx (? StxAtom? pat) _)))))

(define-type StxSeq (Seq Stx) #:omit-define-syntaxes)
(define (StxSeq? x) (make-predicate (Seq Stx)))
(define-match-expander StxSeq
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq (list pat ...)) _)))))

(define-type StxContent (U StxSeq StxAtom))
(define StxContent? (make-predicate StxContent))

(struct EmptyCtx () #:transparent)

(define-type Ctx (U EmptyCtx))
(define Ctx? (make-predicate Ctx))

(define (Ctx-merge (inner : Ctx) (outer : Ctx)) : Ctx
  (error "Ctx-merge: not implemented."))

(struct StxLazy ((stx : StxStrict) (ctx : Ctx)) #:transparent)
(struct StxStrict ((val : StxContent) (ctx : Ctx)) #:transparent)
(define-type Stx (U StxLazy StxStrict) #:omit-define-syntaxes)
(define Stx? (make-predicate Stx))

(define-match-expander Stx
  (lambda (stx)
    (syntax-case stx ()
      ((_ val-pat ctx-pat)
       #'(? Stx? (app Stx->StxStrict (StxStrict val-pat ctx-pat))))))
  (lambda (stx)
    (syntax-case stx ()
      ((_ more ...) #'(StxStrict more ...))
      (_ #'StxStrict))))

(define (Stx->StxStrict (i : Stx)) : StxStrict
  (match i
    ((? StxStrict? o) o)
    ((StxLazy (StxStrict content ctx-inner) ctx-outer)
     (StxStrict
      (if (list? content)
          (for/list ((stx content)) (StxLazy stx ctx-outer))
          content)
      (Ctx-merge ctx-inner ctx-outer)))))

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
