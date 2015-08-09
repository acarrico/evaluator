#lang typed/racket/base

(require "core-lang.rkt" "scanner.rkt" "parser.rkt"
         "env.rkt" "eval.rkt" "expander.rkt"
         racket/match)

(require typed/rackunit)

(define (make-initial-state
         ;; Initial bindings available during evaluation:
         (src-bindings : (Listof (List Symbol Any)))
         ;; More bindings only available during the application of a
         ;; macro transformer:
         (t-src-bindings : (Listof (List Symbol Any))))
  : (Values AstEnv Env CompState)
  (define core-expand-env : Env
    (for/fold ((env (empty-Env)))
              ((entry : (List Symbol Binding)
                      (list (list 'lambda (TransformBinding fun-transform))
                            (list 'quote (TransformBinding quote-transform))
                            (list 'syntax (TransformBinding quote-transform))
                            (list 'let-syntax (TransformBinding let-syntax-transform)))))
      (match entry ((list name binding)
                    (Env-set env name binding)))))
  (define-values (eval-env expand-env)
    (for/fold ((#{eval-env : AstEnv} '())
               (#{expand-env : Env} core-expand-env))
              ((#{src-binding : (List Symbol Any)} src-bindings))
      (match src-binding
        ((list name val)
         (values
          (cons (list (Var name) (scan val)) eval-env)
          (Env-set expand-env name (VarBinding (Stx (Sym name) (EmptyCtx)))))))))
  (define t-eval-env
    (for/fold ((#{t-eval-env : AstEnv} eval-env))
              ((#{src-binding : (List Symbol Any)} t-src-bindings))
      (match src-binding
        ((list name val)
          (cons (list (Var name) (scan val)) t-eval-env)))))
  (values eval-env expand-env (CompState 0 t-eval-env expand)))

(define-values (initial-eval-env initial-expand-env initial-state)
  (make-initial-state '((cons #%cons)
                        (car #%car)
                        (cdr #%cdr)
                        (list-ref #%list-ref)
                        (list #%list)
                        (stx-e #%stx-e)
                        (mk-stx #%mk-stx)
                        (+ #%+))
                      '((lvalue #%lvalue)
                        (lexpand #%lexpand))))

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
              (Seq (Sym 'x) (Sym 'y) (Sym 'z)))
(check-equal? (scan '#%cons) (PrimOp 'cons))
(check-equal? (scan 1) 1)
(check-equal? (scan '(#%stx 2))
              (Stx 2 (EmptyCtx)))

;; Ast Evaluator:

(define (check-Ast-eval i o)
  (define-values (state result)
    (Ast-eval (Ast-scan i) initial-eval-env initial-state (empty-Env) #f))
  (check-equal? result (scan o)))

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

(check-Ast-eval '(mk-stx 1 (#%val (#%stx 2)))
                '(#%stx 1))

(check-Ast-eval '(stx-e (mk-stx 1 (#%val (#%stx 2))))
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
              (Seq (Sym 'x) (Sym 'y) (Sym 'z)))

(check-equal? (parse (Stx-scan '(syntax (x y z))))
              (Stx-scan '(x y z)))

;; expander

(define (Ast-equal? x y) : Boolean
  (and (Ast? x)
       (Ast? y)
       (let recurse ((x : Ast x)
                     (y : Ast y)
                     (env : (Listof (Pairof Symbol Symbol)) '()))
         (match* (x y)
           (((Var x-name) (Var y-name))
            (let ((binding (assq x-name env)))
              (and binding (eq? y-name (cdr binding)))))
           (((App x-args) (App y-args))
            (and (= (length x-args) (length y-args))
                 (for/and ((x-arg x-args)
                           (y-arg y-args))
                   (recurse x-arg y-arg env))))
           (((Fun x-vars x-body) (Fun y-vars y-body))
            (and (= (length x-vars) (length y-vars))
                 (recurse x-body y-body
                          (for/fold ((env env))
                                    ((x-var x-vars) (y-var y-vars))
                            (cons (cons (Var-name x-var) (Var-name y-var)) env)))))
           (((? Atom? x) (? Atom? y))
            ;; ISSUE: true now, but probably should have Atom=?
            (equal? x y))
           (((Sym x) (Sym y))
            (eq? x y))
           ((_ _)
            (error "Ast-equal?: unrecognized Ast (fixme, probably)" x y))))))

(define (check-expand i o)
  (define-values (state expanded)
    (expand initial-state initial-expand-env (Stx-scan i)))
  (check Ast-equal? (parse expanded) o))

(define (check-re-expand i o)
  (define-values (state expanded)
    (expand initial-state initial-expand-env (Stx-scan i)))
  (define-values (state* expanded*) (expand state initial-expand-env expanded))
  (check Ast-equal? (parse expanded*) o))

(check-expand '(lambda (x) x)
              (Fun (list (Var 'x)) (Var 'x)))

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

;; With the hygienic expander, we easily can't check literal syntax
;; anymore:
#;(check-expand '(syntax x)
              (Stx (Sym 'x) (EmptyCtx)))

(check-expand '(lambda (lambda) 'lambda)
              (Fun (list (Var '#%0-lambda)) (Sym 'lambda)))

#;(check-expand '(lambda (lambda) #'lambda)
              (Fun (list (Var '#%0-lambda)) (Stx (Sym 'lambda) (EmptyCtx))))

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

#;(check-re-expand '(syntax x)
                 (Stx (Sym 'x) (EmptyCtx)))

(check-re-expand '(lambda (lambda) 'lambda)
                 (Fun (list (Var '#%1-lambda)) (Sym 'lambda)))

#;(check-re-expand '(lambda (lambda) #'lambda)
                 (Fun (list (Var '#%1-lambda)) (Stx (Sym 'lambda) (EmptyCtx))))

;;; Evaluation

(define (eval i)
  (define-values (state expanded)
    (expand initial-state initial-expand-env (Stx-scan i)))
  (define-values (state* result)
    (Ast-eval (parse expanded) initial-eval-env state (empty-Env) #f))
  result)

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

;;; Macros

;;; NOTE: In 'Macros that Work Together', all functions take one
;;; argument, so the thunk macro may be a little confusing: it takes
;;; one ignored argument.

(check-eval
 '(let-syntax thunk (lambda (e)
                      (mk-stx
                       (list #'lambda #'(a)
                             (car (cdr (stx-e e))))
                       e))
              ((thunk (+ '1 '2)) '0))
 '3)

(check-eval
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
(check-eval
 '(((lambda (x)
      (let-syntax m (lambda (stx) #'x)
        (lambda (x) (+ (m) x))))
    '1)
   '42)
 '43)

(check-eval
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
(check-eval '((lambda (x)
                (let-syntax n #'x
                  (let-syntax m (lambda (stx) (lvalue #'n))
                    m)))
              '42)
            42)

;; The lexpand primitive should only be available during the
;; application of a macro transformer:
(check-exn #rx"expand: unbound identifier*" (lambda () (eval 'lexpand)))

;; Testing the lexpand primitive:
(check-eval '(let-syntax public (lambda (e) (syntax-error))
               (let-syntax class (lambda (e)
                                   ((lambda (e2) (car (cdr (stx-e e2))))
                                    (lexpand (car (cdr (stx-e e))) (list #'public))))
                           (class (public '8))))
            8)

(check-exn
 #rx"expand: unbound identifier*"
 (lambda ()
   (eval
    '(let-syntax stop (lambda (e) (car (cdr (stx-e e))))
       (let-syntax ex (lambda (e) (lexpand (car (cdr (stx-e e)))
                                           (list #'stop)))
         (ex (lambda (x)
               (let-syntax arg (lambda (e) #'(stop x))
                 (arg)))))))))
