#lang typed/racket/base

(require racket/match
         (for-syntax typed/racket/base))

(provide
 Seq list->Seq
 (struct-out Sym) Sym=?
 Atom Atom?
 Exp Exp?
 Mark Mark?
 Ctx Ctx? EmptyCtx EmptyCtx?
 Stx Stx?
 Id Id? ResolvedId
 Form Form?
 Stx-mark
 rename-stxes
 resolve-tree
 )

(struct (T) ListSeq ((elems : (Listof T))) #:transparent)
(define-type Seq (All (T) (U (ListSeq T))) #:omit-define-syntaxes)

(struct Sym ((name : Symbol)) #:transparent)
(define (Sym=? (x : Sym) (y : Sym)) (eq? (Sym-name x) (Sym-name y)))

(define-type Atom (U Sym Integer))
(define-type Exp (U Atom (Seq Stx)))

(define-type Mark Natural)
(define Mark? (make-predicate Mark))
(define-type Marks (Listof Mark))
(define (Marks=? (xs : Marks) (ys : Marks))
  (equal? xs ys))
(define (Marks-cons (m0 : Mark) (ms : Marks))
  (match ms
    ((list m1 more ...) #:when (= m0 m1) more)
    (_ (cons m0 ms))))

(struct Subst ((from : Sym) (to : Sym) (marks : Marks)) #:transparent)
(define-type Ctx (Listof (U Mark Subst)))
(define (EmptyCtx) : Ctx '())
(define EmptyCtx? null?)
(define (Ctx-cons (elem : (U Mark Subst)) (ctx : Ctx))
  (match* (elem ctx)
    (((? Mark? m0) (list (? Mark? m1) more ...)) #:when (= m0 m1) more)
    ((_ _) (cons elem ctx))))

;; The context applies to every node in the expression:
(struct (T) LazyStx ((strict : (StrictStx T)) (ctx : Ctx)) #:transparent)
;; The context applies to the top node of the expression:
(struct (T) StrictStx ((exp : T) (ctx : Ctx)) #:transparent)
(define-type UnionStx (All (T) (U (LazyStx T) (StrictStx T))))
(define-type Stx (UnionStx Exp) #:omit-define-syntaxes)

(define-type Id (UnionStx Sym) #:omit-define-syntaxes)
(define-type Form (UnionStx (Seq Stx)) #:omit-define-syntaxes)

(define Atom? (make-predicate Atom))
(define Exp? (make-predicate Exp))
(define Ctx? (make-predicate Ctx))
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
    ((LazyStx o (list)) o)
    ((LazyStx (StrictStx exp ctx-inner) ctx-outer)
     (StrictStx
      (match exp
        ((Seq elements ...)
         (list->Seq
          (for/list : (Listof Stx) ((elem : Stx elements))
            (match elem
              ((? StrictStx? strict-elem)
               (LazyStx strict-elem ctx-outer))
              ((LazyStx strict-elem more-outer)
               (LazyStx strict-elem (Ctx-merge ctx-outer more-outer)))))))
        (_ exp))
      (Ctx-merge ctx-inner ctx-outer)))))

(define (Ctx-merge (outer : Ctx) (inner : Ctx)) : Ctx
  (match* (outer inner)
    ((outer (list)) outer)
    (((list) inner) inner)
    (((list elem outer ...) (list (? Mark? inner-mark) more-inner ...))
     (let recurse ((elem elem)
                   (outer outer))
       (match* (elem outer)
         ((elem (list next-elem outer ...))
          (cons elem (recurse next-elem outer)))
         (((? Mark? mark) _)
          #:when (eq? mark inner-mark)
          more-inner)
         ((_ _)
          (cons elem inner)))))
    ((_ _)
     (append outer inner))))

(define-match-expander Id
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat) #'(and (Stx (Sym _) _) pat)))))

;; If possible, peel one Mark or Subst off the Id's Ctx,
;; returning:
;;   * the outer Mark and an Id with the rest of the Ctx, or
;;   * the outer Subst and an Id with the rest of the Ctx, or
;;   * #f and the original Id.
(define (Id-peel (id : Id)) : (Values (U Mark Subst #f) Id)
  (match id
    ((StrictStx name (list elem more ...)) (values elem (StrictStx name more)))
    ((StrictStx name _) (values #f id))
    ;; NOTE: the last element of the lazy part of the context might be
    ;; a mark that cancels:
    ((LazyStx (StrictStx name ctx) (list elem))
     (Id-peel (StrictStx name (Ctx-cons elem ctx))))
    ((LazyStx strict (list elem)) (values elem strict))
    ((LazyStx strict (list elem more ...)) (values elem (LazyStx strict more)))
    ((LazyStx strict '()) (Id-peel strict))))

(define (Id-resolve/marks (id : Id)) : (Values Sym Marks)
  (match/values (Id-peel id)
    ((#f (StrictStx name _))
     (values name '()))
    ((elem id*)
     (define-values (name marks) (Id-resolve/marks id*))
     (match elem
       ((? Mark? mark)
        (values name (cons mark marks)))
       ((Subst from-name to-name from-marks)
        #:when (and (Sym=? name from-name) (Marks=? marks from-marks))
        (values to-name '()))
       (_
        (values name marks))))))

(define (Id-resolve (id : Id)) : Sym
  (define-values (name marks) (Id-resolve/marks id))
  name)

(define-match-expander ResolvedId
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat) #'(? Id? (app Id-resolve pat))))))

(define-match-expander Form
  (lambda (stx)
    (syntax-case stx ()
      ((_ pat ...)
       #'(Stx (Seq pat ...) _)))))

(define (Stx-mark (i : Stx) (m0 : Mark)) : Stx
  (match i
    ((LazyStx strict (list (? Mark? m1)))
     #:when (= m0 m1)
     strict)
    ((LazyStx strict (list (? Mark? m1) more ...))
     #:when (= m0 m1)
     (LazyStx strict more))
    ((LazyStx strict ctx)
     (LazyStx strict (cons m0 ctx)))
    ((StrictStx (? Atom? atom) (list (? Mark? m1) more ...))
     #:when (= m0 m1)
     (StrictStx atom more))
    ((? StrictStx? i)
     (LazyStx i (list m0)))))

(define (rename-stxes (froms : (Listof Sym)) (tos : (Listof Sym)) (stxs : (Listof Stx)))
  : (Listof Stx)

  (define substs : (Listof Subst)
    (for/list ((from : Sym froms)
               (to : Sym tos))
      (Subst from to '())))

  (for/list ((i stxs))
    (match i
      ((LazyStx strict ctx)
       (LazyStx strict (append substs ctx)))
      ((? StrictStx? strict)
       (LazyStx strict substs)))))

(define (resolve-tree (stx : Stx)) : Any
  (match stx
    ((ResolvedId (Sym name))
     name)
    ((Stx (? (make-predicate Integer) x) _)
     x)
    ((Stx (Seq elems ...) _)
     (map resolve-tree elems))))

