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

(: parse-lambda (-> Stx #:phase Phase #:bindings BindingTable Ast))

(define (parse-lambda i #:phase ph #:bindings bindings)
  (match i
    ((Form _ (Form (Id #{vars : (Listof Id)}) ...) (? Stx? body))
     (define names : (Listof Symbol)
       (for/list ((var : Id vars))
         (Binding-name (Id-resolve var #:phase ph #:bindings bindings))))
     (if (distinct-names? names)
         (Fun (map Var names) (parse body #:phase ph #:bindings bindings))
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

(define (parse-application (i : Stx) (phase : Phase) (bindings : BindingTable)) : Ast
  (match i
    ((Form #{op+args-stx : (Listof Stx)} ..1)
     (App (for/list ((stx op+args-stx)) (parse stx #:phase phase #:bindings bindings))))
    (_
     (error "parse: application requires at least one subform" i))))

(: parse (-> Stx #:phase Phase #:bindings BindingTable Ast))
(define (parse i #:phase ph #:bindings bindings)
  (define (parse/id (id : Id) (form? : Boolean))
    (define name (Binding-name (Id-resolve id #:phase ph #:bindings bindings)))
    (case name
      ((lambda) (parse-lambda i #:phase ph #:bindings bindings))
      ((quote) (parse-quote i))
      ((syntax) (parse-syntax i))
      (else
       (if form?
           ;; ISSUE: expanding operator twice:
           (parse-application i ph bindings)
           (Var name)))))

  (match i
    ((Id id) (parse/id id #f))
    ((Form (Id id)  _ ...) (parse/id id #t))
    ((Form _ ...) (parse-application i ph bindings))
    ;; not accepting other values (for now):
    (_
     (error "parse: unrecognized form" i))))

(module+ test
  (require
   "scanner.rkt"
   "binding.rkt"
   )

  (define bindings (empty-BindingTable))

  (check-true
   (equal? (parse (Stx-scan 'x) #:phase 0 #:bindings bindings)
           (Var 'x)))

  (check-true
   (equal? (parse (Stx-scan '(x y z)) #:phase 0 #:bindings bindings)
           (App (list (Var 'x) (Var 'y) (Var 'z)))))

  (check-true
   (equal? (parse (Stx-scan '(lambda (x y) (x y))) #:phase 0 #:bindings bindings)
           (Fun (list (Var 'x) (Var 'y)) (App (list (Var 'x) (Var 'y))))))

  (check-true
   (equal?
    (parse (Stx-scan '(lambda (x y) (x y))) #:phase 0 #:bindings bindings)
    (scan '(#%fun (x y) (x y)))))

  (check-true
   (equal?
    (parse (Stx-scan '(quote 1)) #:phase 0 #:bindings bindings)
    1))

  (check-true
   (equal?
    (parse (Stx-scan '(quote (x y z))) #:phase 0 #:bindings bindings)
    (Seq (Sym 'x) (Sym 'y) (Sym 'z))))

  (check-true
   (equal?
    (parse (Stx-scan '(syntax (x y z))) #:phase 0 #:bindings bindings)
    (Stx-scan '(x y z))))
  )
