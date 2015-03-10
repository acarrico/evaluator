#lang typed/racket/base

(require "core-lang.rkt")
(require "scanner.rkt")

(require typed/rackunit)

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
(define ast-env (AstEnv-scan '((cons #%cons)
                               (car #%car)
                               (cdr #%cdr)
                               (list-ref #%list-ref)
                               (list #%list)
                               (stx-e #%stx-e)
                               (mk-stx #%mk-stx))))

(define (check-Ast-eval i o)
  (check-equal? (Ast-eval (Ast-scan i) ast-env) (scan o)))

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
