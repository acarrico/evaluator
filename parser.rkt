#lang typed/racket

;; ISSUE: how to use typed/racket/base but get remove-duplicates?

(require racket/match
         "core-lang.rkt"
         "binding.rkt")

(provide
 distinct-names?
 parse)

(module+ test (require typed/rackunit))

(define (distinct-names? (names : (Listof Symbol)))
  ;; ISSUE: not the greatest algorithm of all time:
  (= (length (remove-duplicates names))
     (length names)))

(module+ test
  (check-true (distinct-names? '(x y z)))
  (check-false (distinct-names? '(x x z))))

(define (parse-lambda (i : Stx) (table : BindingTable)) : Ast
  (match i
    ((Form _ (Form (Id #{vars : (Listof Id)}) ...) (? Stx? body))
     (define names : (Listof Symbol)
       (for/list ((var : Id vars)) (Binding-name (Id-resolve var table))))
     (if (distinct-names? names)
         (Fun (map Var names) (parse body table))
         (error "parse: lambda bound vars must be distinct" i)))
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

(define (parse-application (i : Stx) (table : BindingTable)) : Ast
  (match i
    ((Form #{op+args-stx : (Listof Stx)} ..1)
     (App (for/list ((stx op+args-stx)) (parse stx table))))
    (_
     (error "parse: application requires at least one subform" i))))

(define (parse (i : Stx) (table : BindingTable)) : Ast
  (define (parse/id (id : Id) (form? : Boolean))
    (define name (Binding-name (Id-resolve id table)))
    (case name
      ((lambda) (parse-lambda i table))
      ((quote) (parse-quote i))
      ((syntax) (parse-syntax i))
      (else
       (if form?
           ;; ISSUE: expanding operator twice:
           (parse-application i table)
           (Var name)))))

  (match i
    ((Id id) (parse/id id #f))
    ((Form (Id id)  _ ...) (parse/id id #t))
    ((Form _ ...) (parse-application i table))
    ;; not accepting other values (for now):
    (_
     (error "parse: unrecognized form" i))))

(module+ test
  (require
   "scanner.rkt"
   "binding.rkt"
   )

  (define table (empty-BindingTable))

  (check-true
   (equal? (parse (Stx-scan 'x) table)
           (Var 'x)))

  (check-true
   (equal? (parse (Stx-scan '(x y z)) table)
           (App (list (Var 'x) (Var 'y) (Var 'z)))))

  (check-true
   (equal? (parse (Stx-scan '(lambda (x y) (x y))) table)
           (Fun (list (Var 'x) (Var 'y)) (App (list (Var 'x) (Var 'y))))))

  (check-true
   (equal?
    (parse (Stx-scan '(lambda (x y) (x y))) table)
    (scan '(#%fun (x y) (x y)))))

  (check-true
   (equal?
    (parse (Stx-scan '(quote 1)) table)
    1))

  (check-true
   (equal?
    (parse (Stx-scan '(quote (x y z))) table)
    (Seq (Sym 'x) (Sym 'y) (Sym 'z))))

  (check-true
   (equal?
    (parse (Stx-scan '(syntax (x y z))) table)
    (Stx-scan '(x y z))))
  )
