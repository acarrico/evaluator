#lang typed/racket/base

(require racket/match
         racket/set
         (for-syntax typed/racket/base)
         "scope.rkt"
         "binding.rkt")

(provide
 Seq list->Seq
 (struct-out Sym) Sym=?
 Atom Atom?
 Exp Exp?
 Ctx Ctx? EmptyCtx
 Stx Stx? Stx-add-scope Stx-remove-scope Stx-flip-scope
 Id Id? Id-resolve Id-add-scope Id-bind
 bound-identifier=? bound-identifiers-distinct?
 Form Form?
 )

(struct (T) ListSeq ((elems : (Listof T))) #:transparent)
(define-type Seq (All (T) (U (ListSeq T))) #:omit-define-syntaxes)

(struct Sym ((name : Symbol)) #:transparent)
(define (Sym=? (x : Sym) (y : Sym)) (eq? (Sym-name x) (Sym-name y)))

(define-type Atom (U Sym Integer))
(define-type Exp (U Atom (Seq Stx)))

(struct Ctx ((scopes : SetofScopes)) #:transparent)

(define (EmptyCtx) : Ctx (Ctx (empty-SetofScopes)))
(define (EmptyCtx? (ctx : Ctx)) (set-empty? (Ctx-scopes ctx)))

(define (Ctx-add-scope (ctx : Ctx) (scope : Scope))
  (struct-copy Ctx ctx (scopes (SetofScopes-add (Ctx-scopes ctx) scope))))

(define (Ctx-remove-scope (ctx : Ctx) (scope : Scope))
  (struct-copy Ctx ctx (scopes (SetofScopes-remove (Ctx-scopes ctx) scope))))

(define (Ctx-flip-scope (ctx : Ctx) (scope : Scope))
  (struct-copy Ctx ctx (scopes (SetofScopes-flip (Ctx-scopes ctx) scope))))

(struct LazyCtx ((ops : SetofScopeOps)) #:transparent)

(define (LazyCtx-add-scope (ctx : LazyCtx) (scope : Scope))
  (struct-copy LazyCtx ctx (ops (SetofScopeOps-add (LazyCtx-ops ctx) scope))))

(define (LazyCtx-remove-scope (ctx : LazyCtx) (scope : Scope))
  (struct-copy LazyCtx ctx (ops (SetofScopeOps-remove (LazyCtx-ops ctx) scope))))

(define (LazyCtx-flip-scope (ctx : LazyCtx) (scope : Scope))
  (struct-copy LazyCtx ctx (ops (SetofScopeOps-flip (LazyCtx-ops ctx) scope))))

;; The context applies to every node in the expression:
(struct (T) LazyStx ((strict : (StrictStx T)) (ctx : LazyCtx)) #:transparent)
;; The context applies to the top node of the expression:
(struct (T) StrictStx ((exp : T) (ctx : Ctx)) #:transparent)
(define-type UnionStx (All (T) (U (LazyStx T) (StrictStx T))))
(define-type Stx (UnionStx Exp) #:omit-define-syntaxes)

(define-type Id (UnionStx Sym) #:omit-define-syntaxes)
(define-type Form (UnionStx (Seq Stx)) #:omit-define-syntaxes)

(define Atom? (make-predicate Atom))
(define Exp? (make-predicate Exp))
(define Stx? (make-predicate Stx))
(define Id? (make-predicate Id))
(define Form? (make-predicate Form))

(define list->Seq ListSeq)

(define #:forall (T) (ListSeq* . (elems : T *)) : (ListSeq T) (ListSeq elems))
(define-match-expander Seq
  (lambda (stx)
    (syntax-case stx ()
      ((_ pats ...)
       #'(? ListSeq? (app ListSeq-elems (list pats ...))))))
  (lambda (stx)
    (syntax-case stx ()
      ((_ inits ...) #'(ListSeq* inits ...))
      (_ #'ListSeq*))))

(define-match-expander Stx
  (lambda (stx)
    (syntax-case stx ()
      ((_ val-pat ctx-pat)
       #'(? Stx? (app Stx->StrictStx (StrictStx val-pat ctx-pat))))))
  (lambda (stx)
    (syntax-case stx ()
      ((_ exp ctx) #'(StrictStx exp ctx))
      (_ #'StxStrict))))

(define (Stx->StrictStx (i : Stx)) : (StrictStx Exp)
  (match i
    ((? StrictStx? o) o)
    ((LazyStx (StrictStx exp inner) outer)
     (StrictStx
      (match exp
        ((Seq elements ...)
         (list->Seq
          (for/list : (Listof Stx) ((elem : Stx elements))
            (match elem
              ((? StrictStx? strict-elem)
               (LazyStx strict-elem outer))
              ((LazyStx strict-elem inner)
               (LazyStx strict-elem (LazyCtx-merge outer inner)))))))
        (_ exp))
      (LazyCtx-apply outer inner)))))

(define (LazyCtx-apply (outer : LazyCtx) (inner : Ctx)) : Ctx
  (Ctx (SetofScopeOps-apply (LazyCtx-ops outer) (Ctx-scopes inner))))

(define (LazyCtx-merge (outer : LazyCtx) (inner : LazyCtx)) : LazyCtx
  (LazyCtx (SetofScopeOps-merge (LazyCtx-ops outer) (LazyCtx-ops inner))))

(define-match-expander Id
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat) #'(? Id? pat)))))

(define (Id-resolve (id : Id) (table : BindingTable)) : Binding
  (match id
    ((Stx (Sym name) (Ctx scopes))
     (BindingTable-resolve table name scopes))))

(define (Id-bind (id : Id) (binding : Binding) (table : BindingTable) (hint : Scope))
  : BindingTable
  (match id
    ((Stx (Sym name) ctx)
     (BindingTable-extend
      table
      hint
      name
      (Ctx-scopes ctx)
      binding))))

(define (Id->name+scopes (id : Id))
  : (List Symbol SetofScopes)
  (match id
    ((Stx (Sym name) (Ctx scopes)) (list name scopes))))

(define (bound-identifier=? (x : Id) (y : Id)) : Boolean
  (equal? (Id->name+scopes x) (Id->name+scopes y)))

(define (bound-identifiers-distinct? (ids : (Listof Id)))
  : Boolean
  (define keys (map Id->name+scopes ids))
  (= (length keys)
     (set-count (list->set ids))))

(define-match-expander Form
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq pat ...) _)))))

;; ISSUE: could avoid making lazy atoms, as Id-add-scope does:
(define (Stx-add-scope (i : Stx) (scope : Scope)) : Stx
  (match i
    ((LazyStx strict ctx)
     (LazyStx strict (LazyCtx-add-scope ctx scope)))
    ((StrictStx (? Atom? atom) ctx)
     (StrictStx atom (Ctx-add-scope ctx scope)))
    ((? StrictStx? stxs)
     (LazyStx stxs (LazyCtx (SetofScopeOps-add (empty-SetofScopeOps) scope))))))

;; ISSUE: it would be nice if Stx-add-scope was polymorphic, so adding
;; scope to an Id would give an Id, but I can't figure that out, so I
;; use Id-add-scope too. On the other hand, it actually gives a
;; StrictStx in every case, which is nice:
(define (Id-add-scope (i : Id) (scope : Scope)) : Id
  (match i
    ((LazyStx (StrictStx sym ctx) lazy-ctx)
     (StrictStx sym (Ctx-add-scope (LazyCtx-apply lazy-ctx ctx) scope)))
    ((StrictStx sym ctx)
     (StrictStx sym (Ctx-add-scope ctx scope)))))

(define (Stx-flip-scope (i : Stx) (scope : Scope)) : Stx
  (match i
    ((LazyStx strict ctx)
     (LazyStx strict (LazyCtx-flip-scope ctx scope)))
    ((StrictStx (? Atom? atom) ctx)
     (StrictStx atom (Ctx-flip-scope ctx scope)))
    ((? StrictStx? stxs)
     (LazyStx stxs (LazyCtx (SetofScopeOps-flip (empty-SetofScopeOps) scope))))))

(define (Stx-remove-scope (i : Stx) (scope : Scope)) : Stx
  (match i
    ((LazyStx strict ctx)
     (LazyStx strict (LazyCtx-remove-scope ctx scope)))
    ((StrictStx (? Atom? atom) ctx)
     (StrictStx atom (Ctx-remove-scope ctx scope)))
    ((? StrictStx? stxs)
     (LazyStx stxs (LazyCtx (SetofScopeOps-remove (empty-SetofScopeOps) scope))))))
