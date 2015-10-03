#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "core-lang.rkt")

(provide
 scan
 Ast-scan
 AstEnv-scan
 Stx-scan
 )

;; scanner
;;
;; NOTE: the #%name syntax reads as a symbol in Racket, but I'm
;; using it to flag primitive values.
;;
;; NOTE: this scanner can scan things like Funs and Closures for
;; testing purposes.
(define-match-expander primitive
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat)
       #'(? symbol?
            (app symbol->string
                 (regexp #px"\\#\\%(.*)"
                         (list _ (? string? (app string->symbol pat))))))))))

(define (scan (i : Any)) : Val
  (match i
    ;; Scan Closure:
    ((list (primitive 'closure) fun env)
     (define f (scan fun))
     (unless (Fun? f)
       (error "scan: expected a fun in closure" fun))
     (Closure f (AstEnv-scan env)))
    ;; Scan Fun:
    ((list (primitive 'fun) (list (? symbol? #{vars : (Listof Symbol)}) ...) body)
     (Fun (map Var vars) (Ast-scan body)))
    ;; Scan Stx:
    ((list (primitive 'stx) raw-exp)
     (Stx
      (match (scan raw-exp)
        ((? Exp? exp) exp)
        (_ (error "scan: bad exp for Stx" raw-exp)))
      ;; NOTE: Ctx not a Val, so it can't be scanned, so don't bother
      ;; trying to read context:
      (EmptyCtx)))
    ;; Scan PrimAst:
    ((list (primitive 'ast) ast) (PrimAst (Ast-scan ast)))
    ;; Scan Seq:
    ((list subs ...) (list->Seq (map scan subs)))
    ;; Scan PrimOp:
    ((primitive name) (PrimOp name))
    ;; Scan Sym
    ((? symbol? name) (Sym name))
    ;; Scan Integer
    ((? (make-predicate Integer) i) i)
    (_ (error "scan: unrecognized syntax" i))))

(define (Ast-scan (i : Any)) : Ast
  (match i
    ;; Quoting to scan symbols and sequences as values:
    ((list (primitive 'val) val) (scan val))
    ;; Scan App:
    ((list op-args ...) (App (map Ast-scan op-args)))
    ;; Scan Var:
    ((? symbol? name) (Var name))
    ;; Everything else as a value:
    (_ (scan i))))

(define (AstEnv-scan (i : Any)) : AstEnv
  (match i
    ((list (list (? symbol? #{names : (Listof Symbol)}) vals) ...)
     (for/list ((name names)
                (val vals))
       (list (Var name) (scan val))))))

(define (Stx-scan (i : Any)) : Stx
  (match i
    ;; Scan Seq:
    ((list subs ...) (Stx (list->Seq (map Stx-scan subs)) (EmptyCtx)))
    ;; Scan Sym
    ((? symbol? name) (Stx (Sym name) (EmptyCtx)))
    ;; Scan Integer
    ((? (make-predicate Integer) i) (Stx i (EmptyCtx)))
    (_ (error "Stx-scan: unrecognized syntax" i))))

(module+ test
  (require typed/rackunit)

  ;; Scanner:
  (check equal? (scan 'x)
                (Sym 'x))
  (check equal? (scan '(#%fun (x y) (y x)))
                (Fun (list (Var 'x) (Var 'y)) (App (list (Var 'y) (Var 'x)))))
  (check equal? (scan '(#%ast x))
                (PrimAst (Var 'x)))
  (check equal? (scan '(#%ast (x y z)))
                (PrimAst (App (list (Var 'x) (Var 'y) (Var 'z)))))
  (check equal? (scan '(x y z))
                (Seq (Sym 'x) (Sym 'y) (Sym 'z)))
  (check equal? (scan '#%cons) (PrimOp 'cons))
  (check equal? (scan 1) 1)
  (check equal? (scan '(#%stx 2))
                (Stx 2 (EmptyCtx))))
