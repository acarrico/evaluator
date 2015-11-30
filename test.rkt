#lang typed/racket/base

(require
 racket/match
 typed/rackunit
 "scope.rkt"
 "core-lang.rkt"
 "scanner.rkt"
 "env.rkt"
 "eval.rkt"
 "expander.rkt"
 "transformers.rkt"
 "initial-state.rkt"

 (submod "scanner.rkt" test)
 (submod "parser.rkt" test)
 (submod "eval.rkt" test)
 (submod "transformers.rkt" test)
 )

(define-values (initial-eval-env initial-expand-env initial-state)
  (make-default-initial-state
   #:expand expand
   #:quote quote-transform
   #:syntax syntax-transform
   #:lambda fun-transform
   #:let-syntax let-syntax-transform))

;;; Evaluation

(define (eval i)
  (define-values (state expanded)
    (expand initial-state initial-expand-env (Stx-scan i) #:phase 0 #:prune (empty-SetofScopes)))
  (define-values (state* result)
    (Ast-eval (CompState-parse state expanded #:phase 0) initial-eval-env state (empty-Env) #f #:phase 0))
  result)

(define (check-eval i o)
  (check-true (equal? (eval i) (scan o))))

(define (eval-equal? i o)
  (check-true (equal? (eval i) (scan o))))

(check eval-equal?
  'cons
  '#%cons)

(check eval-equal?
  '(cons '1 '())
  '(1))

(check eval-equal?
  '((lambda (y) y) '1)
  '1)

(check eval-equal?
  '(list-ref '(a b c) '0)
  'a)

(check eval-equal?
  '(list-ref (list 'a 'b 'c) '0)
  'a)

(check eval-equal?
  '(car (list '1 '2))
  '1)

(check eval-equal?
  '(cdr (cons '1 (list '2)))
  '(2))

(check eval-equal?
  '(cdr (list '1 '2))
  '(2))

(check eval-equal?
  '(list)
  '())

(check eval-equal?
  '(((lambda (x) (lambda () x)) '5))
  '5)

(check eval-equal?
  '((lambda (y) ((lambda (x) y) '0)) '1)
  '1)

;;; Macros

;;; NOTE: In 'Macros that Work Together', all functions take one
;;; argument, so the thunk macro may be a little confusing: it takes
;;; one ignored argument.

(check eval-equal?
  '(let-syntax thunk (lambda (e)
                       (mk-stx
                        (list #'lambda #'(a)
                              (car (cdr (stx-e e))))
                        e))
               ((thunk (+ '1 '2)) '0))
  '3)

(check eval-equal?
  '(let-syntax thunk (lambda (e)
                       (mk-stx
                        (list #'lambda #'(a)
                              (car (cdr (stx-e e))))
                        e))
               (((lambda (a) (thunk (+ a '1))) '5) '0))
  ;; Unhygienic answer:
  ;;'1
  ;; Hygienic answer:
  '6
  )

;; Here are the "Scope Examples" from 3.6.1 of *Macros that Work
;; Together*:
(check eval-equal?
  '(((lambda (x)
       (let-syntax m (lambda (stx) #'x)
                   (lambda (x) (+ (m) x))))
     '1)
    '42)
  '43)

(check eval-equal?
  '(((lambda (x)
       (let-syntax n (lambda (stx)
                       ; expand (n e) to (lambda (x) (+ e x))
                       (mk-stx
                        (list #'lambda (mk-stx (list #'x) stx)
                              (mk-stx
                               (list #'+ (car (cdr (stx-e stx))) #'x)
                               stx))
                        stx))
                   (n '1)))
     '1)
    '42)
  '43)

;; The lvalue primitive should only be available during the
;; application of a macro transformer:
(check-exn #rx"expand: unbound identifier*" (lambda () (eval 'lvalue)))

;; Testing the lvalue primitive:
(check eval-equal?
  '((lambda (x)
      (let-syntax n #'x
                  (let-syntax m (lambda (stx) (lvalue #'n))
                              m)))
    '42)
  42)

;; The lexpand primitive should only be available during the
;; application of a macro transformer:
(check-exn #rx"expand: unbound identifier*" (lambda () (eval 'lexpand)))

;; Testing the lexpand primitive:
#;(check eval-equal?
  '(let-syntax public (lambda (e) (syntax-error))
               (let-syntax class (lambda (e)
                                   ((lambda (e2) (car (cdr (stx-e e2))))
                                    (lexpand (car (cdr (stx-e e))) (list #'public))))
                           (class (public '8))))
  8)

#;(check-exn
 #rx"expand: unbound identifier*"
 (lambda ()
   (eval
    '(let-syntax stop (lambda (e) (car (cdr (stx-e e))))
       (let-syntax ex (lambda (e) (lexpand (car (cdr (stx-e e)))
                                           (list #'stop)))
         (ex (lambda (x)
               (let-syntax arg (lambda (e) #'(stop x))
                 (arg)))))))))

