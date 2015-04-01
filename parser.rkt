#lang typed/racket

;; ISSUE: how to use typed/racket/base but get remove-duplicates?

(require racket/match
         "core-lang.rkt")

(provide
 distinct-names?
 parse)

(define (distinct-names? (names : (Listof Symbol)))
  ;; ISSUE: not the greatest algorithm of all time:
  (= (length (remove-duplicates names))
     (length names)))

(define (parse-lambda (i : Stx)) : Ast
  (match i
    ((StxSeq _ (StxSeq (ResolvedId #{names : (Listof Symbol)}) ...) (? Stx? body))
     #:when (distinct-names? names)
     (Fun (map Var names) (parse body)))
    (_
     (error
      "parse: lambda requires two subforms, a list of distinct vars and a body"
      i))))

(define (strip (i : Stx)) : Val
  (match i
    ((StxSeq elems ...) (Seq (map strip elems)))
    ((StxAtom atom) atom)))

(define (parse-quote (i : Stx)) : Ast
  (match i
    ((StxSeq _ quoted) (strip quoted))
    (_ (error "parse: quote requires exactly one subform" i))))

(define (parse-syntax (i : Stx)) : Ast
  (match i
    ((StxSeq _ stx) stx)
    (_ (error "parse: syntax requires exactly one subform" i))))

(define (parse-application (i : Stx)) : Ast
  (match i
    ((StxSeq #{op+args-stx : (Listof Stx)} ..1)
     (App (map parse op+args-stx)))
    (_
     (error "parse: application requires at least one subform" i))))

(define (parse (i : Stx)) : Ast
  (match i
    ((ResolvedId 'lambda) (parse-lambda i))
    ((StxSeq (ResolvedId 'lambda) _ ...) (parse-lambda i))
    ((ResolvedId 'quote) (parse-quote i))
    ((StxSeq (ResolvedId 'quote) _ ...) (parse-quote i))
    ((ResolvedId 'syntax) (parse-syntax i))
    ((StxSeq (ResolvedId 'syntax) _ ...) (parse-syntax i))
    ((StxSeq _ ...) (parse-application i))
    ((ResolvedId name) (Var name))
    ;; not accepting other values (for now):
    (_
     (error "parse: unrecognized form" i))))
