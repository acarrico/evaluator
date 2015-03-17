#lang typed/racket/base

(require (for-syntax typed/racket/base)
         racket/match
         "core-lang.rkt")

(provide
 scan
 Ast-scan
 AstEnv-scan
 Stx-scan
 empty-context
 )

(define Val? (make-predicate Val))

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
    ((list (primitive 'stx) stx ctx) (Stx (scan stx) (scan ctx)))
    ;; Scan PrimAst:
    ((list (primitive 'ast) ast) (PrimAst (Ast-scan ast)))
    ;; Scan Seq:
    ((list subs ...) (Seq (map scan subs)))
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

(define empty-context (Sym 'context))

(define (Stx-scan (i : Any)) : Stx
  (match i
    ;; Scan Seq:
    ((list subs ...) (Stx (Seq (map Stx-scan subs)) empty-context))
    ;; Scan Sym
    ((? symbol? name) (Stx (Sym name) empty-context))
    ;; Scan Integer
    ((? (make-predicate Integer) i) (Stx i empty-context))
    (_ (error "Stx-scan: unrecognized syntax" i))))
