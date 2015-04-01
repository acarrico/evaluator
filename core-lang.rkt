#lang typed/racket/base

(require racket/match
         (for-syntax typed/racket/base))

(provide
 Ast (struct-out Var) (struct-out App)
 Val Val? (struct-out Fun) (struct-out Seq)
 StxAtom StxAtom? Atom Atom?
 (struct-out Sym) (struct-out PrimOp)
 (struct-out PrimAst) (struct-out Closure)
 StxSeq StxSeq?
 StxContent StxContent?
 Ctx empty-context
 (struct-out Stx)
 Id
 ResolvedId
 AstEnv
 Ast-eval
 Ast-apply-values)

(define-type Ast (U Var App Val))
(struct Var ((name : Symbol)) #:transparent)
(struct App ((args : (Listof Ast))) #:transparent)
(define-type Val (U Fun (Seq Val) Atom))
(struct Fun ((vars : (Listof Var)) (body : Ast)) #:transparent)
(struct (T) Seq ((elems : (Listof T))) #:transparent)
(define Val? (make-predicate Val))
(define-type StxAtom (U Sym Integer) #:omit-define-syntaxes)
(define StxAtom? (make-predicate StxAtom))
(define-match-expander StxAtom
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat)
       #'(Stx (? StxAtom? pat) _)))))
(define-type Atom (U StxAtom PrimOp PrimAst Closure Stx))
(define Atom? (make-predicate Atom))
(struct Sym ((name : Symbol)) #:transparent)
(struct PrimOp ((name : Symbol)) #:transparent)
(struct PrimAst ((ast : Ast)) #:transparent)
(struct Closure ((fun : Fun) (env : AstEnv)) #:transparent)

(define-type StxSeq (Seq Stx) #:omit-define-syntaxes)
(define (StxSeq? x) (make-predicate (Seq Stx)))
(define-match-expander StxSeq
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq (list pat ...)) _)))))

(define-type StxContent (U StxSeq StxAtom))
(define StxContent? (make-predicate StxContent))

(define-type Ctx Val)
(define empty-context (Sym 'context))

(struct Stx ((val : StxContent) (ctx : Ctx)) #:transparent)

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

(define-type AstEnv (Listof (List Var Val)))

(define (Ast-eval (ast : Ast) (env : AstEnv)) : Val
  (match ast
    ((App (list (Closure (Fun (list vars ...) body) env*) args ...))
     (unless (= (length vars) (length args))
       (error "Ast-eval: wrong number of arguments" vars args))
     (define env** : AstEnv
       (for/fold ((env** env*))
                 ((var vars) (arg args))
         (cons (list var (Ast-eval arg env)) env**)))
     (Ast-eval body env**))
    ((App (list (? PrimOp? op) args ...))
     (Prim-eval op (for/list ((arg args)) (Ast-eval arg env))))
    ((App (list op-ast args ...))
     (match (Ast-eval op-ast env)
       ((? (make-predicate (U Closure PrimOp)) op)
        (Ast-eval (App (cons op args)) env))
       (op
        (error "Ast-eval: operator must be a Closure or PrimOp" op))))
    ((Var name)
     (let loop ((env env))
       (if (pair? env)
           (let ((binding (car env)))
             (if (eq? name (Var-name (car binding)))
                 (cadr binding)
                 (loop (cdr env))))
           (error "Ast-eval: variable not in current environment" name))))
    ((? Fun? fn)
     (Closure fn env))
    ((? Val? val)
     val)))

(define (Prim-eval (op : PrimOp) (args : (Listof Val))) : Val
  (match* (op args)
    (((PrimOp 'cons) (list elem (Seq elems)))
     (Seq (cons elem elems)))
    (((PrimOp 'car) (list (Seq elems)))
     (car elems))
    (((PrimOp 'cdr) (list (Seq elems)))
     (Seq (cdr elems)))
    (((PrimOp 'list-ref) (list (Seq elems) (? (make-predicate Integer) index)))
     (list-ref elems index))
    (((PrimOp 'list) _)
     (Seq args))
    (((PrimOp 'stx-e) (list (Stx val ctx)))
     val)
    (((PrimOp 'mk-stx) (list (? StxContent? val) (Stx _ ctx)))
     (Stx val ctx))
    (((PrimOp '+) (list (? (make-predicate Integer) x) (? (make-predicate Integer) y)))
     (+ x y))
    ((_ _)
     (error "Prim-eval bad primitive form" op args))))

(define (Ast-apply-values (closure : Closure) (args : (Listof Val)))
  ;; NOTE: this does not evaluate the operator or the args.
  (match closure
    ((Closure (Fun (list vars ...) body) env)
     (unless (= (length vars) (length args))
       (error "Ast-apply: wrong number of arguments" vars args))
     (define env* : AstEnv
       (for/fold ((env env))
                 ((var vars) (arg args))
         (cons (list var arg) env)))
     (Ast-eval body env*))))
