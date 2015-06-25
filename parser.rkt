#lang typed/racket

;; ISSUE: how to use typed/racket/base but get remove-duplicates?

(require racket/match
         "core-lang.rkt")

(provide
 distinct-names?
 parse)

(define (distinct-names? (names : (Listof Sym)))
  ;; ISSUE: not the greatest algorithm of all time:
  (= (length (remove-duplicates (map Sym-name names)))
     (length names)))

(define (parse-lambda (i : Stx)) : Ast
  (match i
    ((Form _ (Form (ResolvedId #{names : (Listof Sym)}) ...) (? Stx? body))
     #:when (distinct-names? names)
     (Fun (map (compose Var Sym-name) names) (parse body)))
    (_
     (error
      "parse: lambda requires two subforms, a list of distinct vars and a body"
      i))))

(define (strip (i : Stx)) : Val
  (match i
    ((Form elems ...) (list->Seq (map strip elems)))
    ((Stx atom _) atom)))

(define (parse-quote (i : Stx)) : Ast
  (match i
    ((Form _ quoted) (strip quoted))
    (_ (error "parse: quote requires exactly one subform" i))))

(define (parse-syntax (i : Stx)) : Ast
  (match i
    ((Form _ stx) stx)
    (_ (error "parse: syntax requires exactly one subform" i))))

(define (parse-application (i : Stx)) : Ast
  (match i
    ((Form #{op+args-stx : (Listof Stx)} ..1)
     (App (map parse op+args-stx)))
    (_
     (error "parse: application requires at least one subform" i))))

(define (parse (i : Stx)) : Ast
  (match i
    ((ResolvedId (Sym 'lambda)) (parse-lambda i))
    ((Form (ResolvedId (Sym 'lambda)) _ ...) (parse-lambda i))
    ((ResolvedId (Sym 'quote)) (parse-quote i))
    ((Form (ResolvedId (Sym 'quote)) _ ...) (parse-quote i))
    ((ResolvedId (Sym 'syntax)) (parse-syntax i))
    ((Form (ResolvedId (Sym 'syntax)) _ ...) (parse-syntax i))
    ((Form _ ...) (parse-application i))
    ((ResolvedId (Sym name)) (Var name))
    ;; not accepting other values (for now):
    (_
     (error "parse: unrecognized form" i))))
