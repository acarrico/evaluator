#lang typed/racket

;; ISSUE: how to use typed/racket/base but get remove-duplicates?

(require (for-syntax typed/racket/base)
         racket/match
         "core-lang.rkt")

(provide
 Stx-Seq
 Id
 ResolvedId
 distinct-names?
 parse)

(define-match-expander Stx-Seq
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq (and (list (struct Stx _) (... ...))
                        (list pat ...))) _)))))

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

(define (distinct-names? (names : (Listof Symbol)))
  ;; ISSUE: not the greatest algorithm of all time:
  (= (length (remove-duplicates names))
     (length names)))

(define (parse-lambda (i : Stx)) : Ast
  (match i
    ((Stx-Seq _ (Stx-Seq (ResolvedId #{names : (Listof Symbol)}) ...) (? Stx? body))
     #:when (distinct-names? names)
     (Fun (map Var names) (parse body)))
    (_
     (error
      "parse: lambda requires two subforms, a list of distinct vars and a body"
      i))))

(define (strip (i : Stx)) : Val
  (match i
    ((Stx (Seq (? (make-predicate (Listof Stx)) elems)) _)
     (Seq (map strip elems)))
    ((Stx atom _) atom)))

(define (parse-quote (i : Stx)) : Ast
  (match i
    ((Stx-Seq _ (? Stx? stx))
     (strip stx))
    (_
     (error "parse: quote requires exactly one subform" i))))

(define (parse-syntax (i : Stx)) : Ast
  (match i
    ((Stx-Seq _ (? Stx? stx))
     stx)
    (_
     (error "parse: syntax requires exactly one subform" i))))

(define (parse-application (i : Stx)) : Ast
  (match i
    ((Stx-Seq (? Stx? #{op+args-stx : (Listof Stx)}) ..1)
     (App (map parse op+args-stx)))
    (_
     (error "parse: application requires at least one subform" i))))

(define (parse (i : Stx)) : Ast
  (match i
    ((ResolvedId 'lambda) (parse-lambda i))
    ((Stx-Seq (ResolvedId 'lambda) _ ...) (parse-lambda i))
    ((ResolvedId 'quote) (parse-quote i))
    ((Stx-Seq (ResolvedId 'quote) _ ...) (parse-quote i))
    ((ResolvedId 'syntax) (parse-syntax i))
    ((Stx-Seq (ResolvedId 'syntax) _ ...) (parse-syntax i))
    ((Stx-Seq _ ...) (parse-application i))
    ((ResolvedId name) (Var name))
    ;; not accepting other values (for now):
    (_
     (error "parse: unrecognized form" i))))
