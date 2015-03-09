#lang typed/racket

(require "core-lang.rkt")

(provide
 scan
 Ast-scan
 AstEnv-scan
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
    ((list (primitive 'fun) (list (? symbol? vars) ...) body)
     ;; ISSUE: typed Racket doesn't recognize that all the vars are
     ;; symbols here.
     (if (andmap symbol? vars)
         (Fun (map Var vars) (Ast-scan body))
         (error "this won't happen")))
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
    ((list (list (? symbol? names) vals) ...)
     (for/list ((name names)
                (val vals))
       (if (symbol? name)
           (list (Var name) (scan val))
           (error "typed racket doesn't do match right yet"))))))
