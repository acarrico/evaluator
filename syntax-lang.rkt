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
 Phase
 Ctx Ctx? EmptyCtx
 Stx Stx? Stx-add-scope Stx-remove-scope Stx-flip-scope
 Stx-remove-scopes
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

(define-type Phase Integer)

(struct Ctx ((phase->scopes : (HashTable Phase SetofScopes))) #:transparent)

(define (Ctx-update-scopes
         (ctx : Ctx)
         (phase : Phase)
         (updater : (-> SetofScopes Scope SetofScopes))
         (scope : Scope)) : Ctx
  (define old-table : (HashTable Phase SetofScopes) (Ctx-phase->scopes ctx))
  (define old-scopes : SetofScopes (hash-ref old-table phase empty-SetofScopes))
  (define new-scopes (updater old-scopes scope))
  (define new-table (hash-set old-table phase new-scopes))
  (struct-copy Ctx ctx (phase->scopes new-table)))

(define (EmptyCtx) : Ctx (Ctx (hash)))

(define (Ctx-add-scope (ctx : Ctx) (phase : Phase) (scope : Scope))
  (Ctx-update-scopes ctx phase SetofScopes-add scope))

(define (Ctx-remove-scope (ctx : Ctx) (phase : Phase) (scope : Scope))
  (Ctx-update-scopes ctx phase SetofScopes-remove scope))

(define (Ctx-flip-scope (ctx : Ctx) (phase : Phase) (scope : Scope))
  (Ctx-update-scopes ctx phase SetofScopes-flip scope))

(struct LazyCtx ((phase->ops : (HashTable Phase SetofScopeOps))) #:transparent)
(define (EmptyLazyCtx) : LazyCtx (LazyCtx (hash)))

(define (LazyCtx-update-scopes (ctx : LazyCtx) (phase : Phase) (updater : (-> SetofScopeOps Scope SetofScopeOps)) (scope : Scope)) : LazyCtx
  (define old-table (LazyCtx-phase->ops ctx))
  (define old-ops (hash-ref old-table phase empty-SetofScopeOps))
  (define new-ops (updater old-ops scope))
  (define new-table (hash-set old-table phase new-ops))
  (struct-copy LazyCtx ctx (phase->ops new-table)))

(define (LazyCtx-add-scope (ctx : LazyCtx) (phase : Phase) (scope : Scope))
  (LazyCtx-update-scopes ctx phase SetofScopeOps-add scope))

(define (LazyCtx-remove-scope (ctx : LazyCtx) (phase : Phase) (scope : Scope))
  (LazyCtx-update-scopes ctx phase SetofScopeOps-remove scope))

(define (LazyCtx-flip-scope (ctx : LazyCtx) (phase : Phase) (scope : Scope))
  (LazyCtx-update-scopes ctx phase SetofScopeOps-flip scope))

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
  (struct-copy
   Ctx
   inner
   (phase->scopes
    (for/fold
        ((phase->scopes : (HashTable Phase SetofScopes) (Ctx-phase->scopes inner)))
        (((#{phase : Phase} #{ops : SetofScopeOps}) (LazyCtx-phase->ops outer)))
      (hash-set
       phase->scopes
       phase
       (SetofScopeOps-apply ops (hash-ref phase->scopes phase empty-SetofScopes)))))))

(define (LazyCtx-merge (outer : LazyCtx) (inner : LazyCtx)) : LazyCtx
  (struct-copy
   LazyCtx
   inner
   (phase->ops
    (for/fold
        ((phase->ops : (HashTable Phase SetofScopeOps) (LazyCtx-phase->ops inner)))
        (((#{phase : Phase} #{ops : SetofScopeOps}) (LazyCtx-phase->ops outer)))
      (hash-set
       phase->ops
       phase
       (SetofScopeOps-merge ops (hash-ref phase->ops phase empty-SetofScopeOps)))))))

(define-match-expander Id
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat) #'(? Id? pat)))))

(: Id-resolve (-> Id #:phase Phase #:bindings BindingTable Binding))
(define (Id-resolve id #:phase phase #:bindings bindings)
  (match id
    ((Stx (Sym name) (Ctx phase->scopes))
     (define scopes (hash-ref phase->scopes phase (lambda () (empty-SetofScopes))))
     (BindingTable-resolve bindings name scopes))))

(: Id-bind (-> BindingTable Id Binding #:hint Scope #:phase Phase BindingTable))
(define (Id-bind table id binding #:hint hint #:phase phase)
  (match id
    ((Stx (Sym name) ctx)
     (BindingTable-extend
      table
      hint
      name
      (hash-ref (Ctx-phase->scopes ctx) phase)
      binding))))

(: Id->name+scopes (-> Id #:phase Phase (List Symbol SetofScopes)))
(define (Id->name+scopes id #:phase phase)
  (match id
    ((Stx (Sym name) (Ctx phase->scopes))
     (list name (hash-ref phase->scopes phase (lambda () (empty-SetofScopes)))))))

(: bound-identifier=? (-> Id Id #:phase Phase Boolean))
(define (bound-identifier=? x y #:phase ph)
  (equal? (Id->name+scopes x #:phase ph) (Id->name+scopes y #:phase ph)))

(: bound-identifiers-distinct? (-> (Listof Id) #:phase Phase Boolean))
(define (bound-identifiers-distinct? ids #:phase ph)
  (define keys (map (lambda ((id : Id)) (Id->name+scopes id #:phase ph)) ids))
  (= (length keys)
     (set-count (list->set ids))))

(define-match-expander Form
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq pat ...) _)))))

;; ISSUE: could avoid making lazy atoms, as Id-add-scope does:
(: Stx-add-scope (-> Stx Scope #:phase Phase Stx))
(define (Stx-add-scope i scope #:phase phase)
  (match i
    ((LazyStx strict ctx)
     (LazyStx strict (LazyCtx-add-scope ctx phase scope)))
    ((StrictStx (? Atom? atom) ctx)
     (StrictStx atom (Ctx-add-scope ctx phase scope)))
    ((? StrictStx? stxs)
     (LazyStx stxs (LazyCtx-add-scope (EmptyLazyCtx) phase scope)))))

;; ISSUE: it would be nice if Stx-add-scope was polymorphic, so adding
;; scope to an Id would give an Id, but I can't figure that out, so I
;; use Id-add-scope too. On the other hand, it actually gives a
;; StrictStx in every case, which is nice:
(: Id-add-scope (-> Id Scope #:phase Phase Id))
(define (Id-add-scope i scope #:phase phase)
  (match i
    ((LazyStx (StrictStx sym ctx) lazy-ctx)
     (StrictStx sym (Ctx-add-scope (LazyCtx-apply lazy-ctx ctx) phase scope)))
    ((StrictStx sym ctx)
     (StrictStx sym (Ctx-add-scope ctx phase scope)))))

(: Stx-flip-scope (-> Stx Scope #:phase Phase Stx))
(define (Stx-flip-scope i scope #:phase phase)
  (match i
    ((LazyStx strict ctx)
     (LazyStx strict (LazyCtx-flip-scope ctx phase scope)))
    ((StrictStx (? Atom? atom) ctx)
     (StrictStx atom (Ctx-flip-scope ctx phase scope)))
    ((? StrictStx? stxs)
     (LazyStx stxs (LazyCtx-flip-scope (EmptyLazyCtx) phase scope)))))

(: Stx-remove-scope (-> Stx Scope #:phase Phase Stx))
(define (Stx-remove-scope i scope #:phase phase)
  (match i
    ((LazyStx strict ctx)
     (LazyStx strict (LazyCtx-remove-scope ctx phase scope)))
    ((StrictStx (? Atom? atom) ctx)
     (StrictStx atom (Ctx-remove-scope ctx phase scope)))
    ((? StrictStx? stxs)
     (LazyStx stxs (LazyCtx-remove-scope (EmptyLazyCtx) phase scope)))))

;; ISSUE: should do a bulk removal.
(: Stx-remove-scopes (-> Stx SetofScopes #:phase Phase Stx))
(define (Stx-remove-scopes i scopes #:phase ph)
  : Stx
  (for/fold ((stx : Stx i))
            ((scope : Scope scopes))
    (Stx-remove-scope stx scope #:phase ph)))
