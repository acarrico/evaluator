#lang typed/racket/base

(require racket/match
         (for-syntax typed/racket/base))

(provide
 (struct-out Seq)
 (struct-out Sym)
 StxAtom StxAtom?
 StxSeq StxSeq?
 StxContent StxContent?
 Ctx Ctx? empty-context
 (struct-out Stx)
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

(define-type Ctx Sym)
(define Ctx? (make-predicate Ctx))
(define empty-context (Sym 'context))

(struct Stx ((val : StxContent) (ctx : Ctx)) #:transparent)

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
