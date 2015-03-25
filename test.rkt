#lang typed/racket/base

(require "core-lang.rkt" "scanner.rkt" "parser.rkt" "expander.rkt"
         racket/match)

(require typed/rackunit)

(define (make-initial-state (src-bindings : (Listof (List Symbol Any))))
  : (Values AstEnv Env CompState)
  (define core-expand-env
    (list (list 'lambda (TransformBinding fun-transform))
          (list 'quote (TransformBinding quote-transform))
          (list 'syntax (TransformBinding quote-transform))))
  (for/fold ((#{eval-env : AstEnv} '())
             (#{expand-env : Env} core-expand-env)
             (state (CompState 0)))
            ((#{src-binding : (List Symbol Any)} src-bindings))
    (match src-binding
      ((list name val)
       (values
        (cons (list (Var name) (scan val)) eval-env)
        (cons (list name (VarBinding (Stx (Sym name) empty-context))) expand-env)
        state)))))

(define-values (initial-eval-env initial-expand-env initial-state)
  (make-initial-state '((cons #%cons)
                        (car #%car)
                        (cdr #%cdr)
                        (list-ref #%list-ref)
                        (list #%list)
                        (stx-e #%stx-e)
                        (mk-stx #%mk-stx))))

;; Scanner:
(check-equal? (scan 'x)
              (Sym 'x))
(check-equal? (scan '(#%fun (x y) (y x)))
              (Fun (list (Var 'x) (Var 'y)) (App (list (Var 'y) (Var 'x)))))
(check-equal? (scan '(#%ast x))
              (PrimAst (Var 'x)))
(check-equal? (scan '(#%ast (x y z)))
              (PrimAst (App (list (Var 'x) (Var 'y) (Var 'z)))))
(check-equal? (scan '(x y z))
              (Seq (list (Sym 'x) (Sym 'y) (Sym 'z))))
(check-equal? (scan '#%cons) (PrimOp 'cons))
(check-equal? (scan 1) 1)
(check-equal? (scan '(#%stx 2 context))
              (Stx 2 (Sym 'context)))

;; Ast Evaluator:

(define (check-Ast-eval i o)
  (check-equal? (Ast-eval (Ast-scan i) initial-eval-env) (scan o)))

(check-Ast-eval 'cons
                '#%cons)
(check-Ast-eval '((#%val (#%fun (y) y)) 1)
                1)
(check-Ast-eval '((#%val #%list-ref) (#%val (a b c)) 0)
                'a)
(check-Ast-eval '(list-ref (#%val (a b c)) 0)
                'a)
(check-Ast-eval '(list-ref (list (#%val a) (#%val b) (#%val c)) 0)
                'a)
(check-Ast-eval '(car (list 1 2))
                '1)
(check-Ast-eval '(cdr (cons 1 (list 2)))
                '(2))
(check-Ast-eval '(cdr (list 1 2))
                '(2))
(check-Ast-eval '(list)
                '())
(check-Ast-eval '((#%val (#%closure (#%fun () x) ((x 5)))))
                5)
(check-Ast-eval '((#%val (#%fun (y) ((#%val (#%fun (x) y)) 0))) 1)
                1)

;; syntax-objects:

(check-Ast-eval '(mk-stx 1 (#%val (#%stx 2 context)))
                '(#%stx 1 context))

(check-Ast-eval '(stx-e (mk-stx 1 (#%val (#%stx 2 context))))
                '1)

;; parser

(check-equal? (parse (Stx-scan 'x))
              (Var 'x))

(check-equal? (parse (Stx-scan '(x y z)))
              (App (list (Var 'x) (Var 'y) (Var 'z))))

(check-equal? (parse (Stx-scan '(lambda (x y) (x y))))
              (Fun (list (Var 'x) (Var 'y)) (App (list (Var 'x) (Var 'y)))))

(check-equal? (parse (Stx-scan '(lambda (x y) (x y))))
              (scan '(#%fun (x y) (x y))))

(check-equal? (parse (Stx-scan '(quote 1)))
              1)

(check-equal? (parse (Stx-scan '(quote (x y z))))
              (Seq (list (Sym 'x) (Sym 'y) (Sym 'z))))

(check-equal? (parse (Stx-scan '(syntax (x y z))))
              (Stx-scan '(x y z)))

;; expander

(define (check-expand i o)
  (define-values (state expanded) (expand initial-state initial-expand-env (Stx-scan i)))
  (check-equal? (parse expanded) o))

(define (check-re-expand i o)
  (define-values (state expanded) (expand initial-state initial-expand-env (Stx-scan i)))
  (define-values (state* expanded*) (expand state initial-expand-env expanded))
  (check-equal? (parse expanded*) o))

(check-expand '(lambda (x) x)
              (Fun (list (Var '#%0-x)) (Var '#%0-x)))

(check-expand '(lambda (lambda) lambda)
              (Fun (list (Var '#%0-lambda)) (Var '#%0-lambda)))

(check-expand '(lambda (x) (lambda (x) x))
              (Fun (list (Var '#%0-x)) (Fun (list (Var '#%1-x)) (Var '#%1-x))))

(check-expand '(lambda (x) (lambda (y) (x y)))
              (Fun (list (Var '#%0-x))
                   (Fun (list (Var '#%1-y))
                        (App (list (Var '#%0-x) (Var '#%1-y))))))

(check-expand '(quote x)
              (Sym 'x))

(check-expand '(syntax x)
              (Stx (Sym 'x) empty-context))

(check-expand '(lambda (lambda) 'lambda)
              (Fun (list (Var '#%0-lambda)) (Sym 'lambda)))

(check-expand '(lambda (lambda) #'lambda)
              (Fun (list (Var '#%0-lambda)) (Stx (Sym 'lambda) empty-context)))

;; test idempotence:

(check-re-expand '(lambda (x) x)
              (Fun (list (Var '#%1-x)) (Var '#%1-x)))

(check-re-expand '(lambda (lambda) lambda)
                 (Fun (list (Var '#%1-lambda)) (Var '#%1-lambda)))

(check-re-expand '(lambda (x) (lambda (x) x))
                 (Fun (list (Var '#%2-x)) (Fun (list (Var '#%3-x)) (Var '#%3-x))))

(check-re-expand '(lambda (x) (lambda (y) (x y)))
                 (Fun (list (Var '#%2-x))
                      (Fun (list (Var '#%3-y))
                           (App (list (Var '#%2-x) (Var '#%3-y))))))

(check-re-expand '(quote x)
                 (Sym 'x))

(check-re-expand '(syntax x)
                 (Stx (Sym 'x) empty-context))

(check-re-expand '(lambda (lambda) 'lambda)
                 (Fun (list (Var '#%1-lambda)) (Sym 'lambda)))

(check-re-expand '(lambda (lambda) #'lambda)
                 (Fun (list (Var '#%1-lambda)) (Stx (Sym 'lambda) empty-context)))

;;; Evaluation

(define (eval i)
  (define-values (state expanded)
    (expand initial-state initial-expand-env (Stx-scan i)))
  (Ast-eval (parse expanded) initial-eval-env))

(define (check-eval i o)
  (check-equal? (eval i) (scan o)))

(check-eval 'cons
            '#%cons)

(check-eval '(cons '1 '())
            '(1))

(check-eval '((lambda (y) y) '1)
            '1)

(check-eval '(list-ref '(a b c) '0)
            'a)

(check-eval '(list-ref (list 'a 'b 'c) '0)
            'a)

(check-eval '(car (list '1 '2))
            '1)

(check-eval '(cdr (cons '1 (list '2)))
            '(2))

(check-eval '(cdr (list '1 '2))
            '(2))

(check-eval '(list)
            '())

(check-eval '(((lambda (x) (lambda () x)) '5))
            '5)

(check-eval '((lambda (y) ((lambda (x) y) '0)) '1)
            '1)
