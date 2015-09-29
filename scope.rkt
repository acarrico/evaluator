#lang typed/racket/base

(module+ canonical-scope)
(module+ test)

(require racket/match
         racket/set)

(provide
 (struct-out Scope)
 SetofScopes SetofScopes? empty-SetofScopes
 SetofScopes-add
 SetofScopes-remove
 SetofScopes-flip
 (struct-out SetofScopeOps)
 empty-SetofScopeOps
 SetofScopeOps-apply
 SetofScopeOps-add
 SetofScopeOps-remove
 SetofScopeOps-flip
 SetofScopeOps-merge
 )

;; NOTE: Scopes are structs so we can count on the garbage collector
;; detecting unreferenced scopes when used as keys in weak tables.
;; Only mutable to give it object identity.
(struct Scope ((id : Natural)) #:transparent #:mutable)

(define-type SetofScopes (Setof Scope))

(define SetofScopes? (make-predicate SetofScopes))

(define (empty-SetofScopes) : SetofScopes
  (seteq))

(define (SetofScopes-add (scopes : SetofScopes) (scope : Scope)) : SetofScopes
  (set-add scopes scope))

(define (SetofScopes-remove (scopes : SetofScopes) (scope : Scope)) : SetofScopes
  (set-remove scopes scope))

(define (SetofScopes-flip (scopes : SetofScopes) (scope : Scope)) : SetofScopes
  (set-symmetric-difference scopes (seteq scope)))

(struct SetofScopeOps ((adds : SetofScopes)
                       (removes : SetofScopes)
                       (flips : SetofScopes))
  #:transparent)

(define (empty-SetofScopeOps)
  (SetofScopeOps (seteq) (seteq) (seteq)))

(define (SetofScopeOps-apply (ops : SetofScopeOps) (scopes : SetofScopes)) : SetofScopes
  (match ops
    ((SetofScopeOps adds removes flips)
     (set-union
      (set-symmetric-difference
       (set-subtract scopes removes)
       flips)
      adds))))

(define (SetofScopeOps-add (ops : SetofScopeOps) (scope : Scope))
  (match ops
    ((SetofScopeOps adds removes flips)
     (SetofScopeOps (set-add adds scope)
                    (set-remove removes scope)
                    (set-remove flips scope)))))

(define (SetofScopeOps-remove (ops : SetofScopeOps) (scope : Scope))
  (match ops
    ((SetofScopeOps adds removes flips)
     (SetofScopeOps (set-remove adds scope)
                    (set-add removes scope)
                    (set-remove flips scope)))))

(define (SetofScopeOps-flip (ops : SetofScopeOps) (scope : Scope))
  (match ops
    ((SetofScopeOps adds removes flips)
     (cond ((set-member? adds scope)
            (SetofScopeOps (set-remove adds scope)
                           (set-add removes scope)
                           flips))
           ((set-member? removes scope)
            (SetofScopeOps (set-add adds scope)
                           (set-remove removes scope)
                           flips))
           ((set-member? flips scope)
            (SetofScopeOps adds removes (set-remove flips scope)))
           (else
            (SetofScopeOps adds removes (set-add flips scope)))))))

(define (SetofScopeOps-merge (outer : SetofScopeOps) (inner : SetofScopeOps))
  : SetofScopeOps
  ;; ISSUE: combine ops one at a time, could be better:
  (match outer
    ((SetofScopeOps adds rems flips)
     (define ops/a
       (for/fold ((ops : SetofScopeOps inner)) ((op adds)) (SetofScopeOps-add ops op)))
     (define ops/ar
       (for/fold ((ops : SetofScopeOps ops/a)) ((op rems)) (SetofScopeOps-remove ops op)))
     (define ops/arf
       (for/fold ((ops : SetofScopeOps ops/ar)) ((op flips)) (SetofScopeOps-flip ops op)))
     ops/arf)))

(module+ test
  (require typed/rackunit racket/list)
  (require (submod ".." canonical-scope))

  ;; ISSUE: typed racket bug 15143 (check-equal? (seteq) (seteq))
  (: check-set-equal? (All (A) (-> (Setof A) (Setof A) Any)))
  (define (check-set-equal? a b)
    (check-true (equal? a b)))

  (check-set-equal? (SetofScopes-add (empty-SetofScopes) (canonical-scope 0))
                    (canonical-scopes '(0)))
  (check-set-equal? (SetofScopes-remove (empty-SetofScopes) (canonical-scope 0))
                    (canonical-scopes '()))
  (check-set-equal? (SetofScopes-flip (empty-SetofScopes) (canonical-scope 0))
                    (canonical-scopes '(0)))

  (check-set-equal? (SetofScopes-add
                     (SetofScopes-add (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '(0)))
  (check-set-equal? (SetofScopes-add
                     (SetofScopes-remove (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '(0)))
  (check-set-equal? (SetofScopes-add
                     (SetofScopes-flip (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '(0)))

  (check-set-equal? (SetofScopes-remove
                     (SetofScopes-add (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '()))
  (check-set-equal? (SetofScopes-remove
                     (SetofScopes-remove (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '()))
  (check-set-equal? (SetofScopes-remove
                     (SetofScopes-flip (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '()))

  (check-set-equal? (SetofScopes-flip
                     (SetofScopes-add (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '()))
  (check-set-equal? (SetofScopes-flip
                     (SetofScopes-remove (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '(0)))
  (check-set-equal? (SetofScopes-flip
                     (SetofScopes-flip (empty-SetofScopes) (canonical-scope 0))
                     (canonical-scope 0))
                    (canonical-scopes '()))

  (check-set-equal? (SetofScopes-add (canonical-scopes '(0)) (canonical-scope 0))
                    (canonical-scopes '(0)))
  (check-set-equal? (SetofScopes-remove (canonical-scopes '(0)) (canonical-scope 0))
                    (canonical-scopes '()))
  (check-set-equal? (SetofScopes-flip (canonical-scopes '(0)) (canonical-scope 0))
                    (canonical-scopes '()))

  (define-type RawOp (List (U 'add 'remove 'flip) Natural))
  (define-type RawOps (Listof (List (U 'add 'remove 'flip) Natural)))

  (define (RawOps->ops (raw-ops : RawOps)) : SetofScopeOps
    (for/fold ((ops : SetofScopeOps (empty-SetofScopeOps)))
              ((raw-op : RawOp raw-ops))
      (match raw-op
        ((list 'add scope-index)
         (SetofScopeOps-add ops (canonical-scope scope-index)))
        ((list 'remove scope-index)
         (SetofScopeOps-remove ops (canonical-scope scope-index)))
        ((list 'flip scope-index)
         (SetofScopeOps-flip ops (canonical-scope scope-index))))))

  (define (RawOps-apply (raw-ops : RawOps) (init-scopes : SetofScopes))
    : SetofScopes
    (for/fold ((scopes init-scopes))
              ((raw-op : RawOp raw-ops))
      (match raw-op
        ((list 'add scope-index)
         (set-add scopes (canonical-scope scope-index)))
        ((list 'remove scope-index)
         (set-remove scopes (canonical-scope scope-index)))
        ((list 'flip scope-index)
         (set-symmetric-difference scopes (seteq (canonical-scope scope-index)))))))

  (: check-scope-ops (-> (Listof Natural) RawOps (Listof Natural) Any))
  (define (check-scope-ops init raw-ops result)
    (define init-scopes (canonical-scopes init))
    (define result-scopes (canonical-scopes result))

    ;; Check to make sure the given results are correct:
    (check-set-equal? (RawOps-apply raw-ops init-scopes) result-scopes)

    ;; Check SetofScopeOps-apply:
    (define ops (RawOps->ops raw-ops))
    (check-set-equal?
     (SetofScopeOps-apply ops init-scopes)
     result-scopes)

    ;; Check SetofScopeOps-merge:
    (for ((index (+ 1 (length raw-ops))))
      (let-values (((inner-ops outer-ops) (split-at raw-ops index)))
        (check-set-equal?
         (SetofScopeOps-apply
          (SetofScopeOps-merge (RawOps->ops outer-ops) (RawOps->ops inner-ops))
          init-scopes)
         result-scopes)))
    )

  (check-scope-ops '(1 5) '((add 0)) '(0 1 5))
  (check-scope-ops '(1 6) '((remove 0)) '(1 6))
  (check-scope-ops '(1 7) '((flip 0)) '(0 1 7))

  (check-scope-ops '(1 8) '((add 0) (add 0)) '(0 1 8))
  (check-scope-ops '(1 9) '((remove 0) (add 0)) '(0 1 9))
  (check-scope-ops '(1 10) '((flip 0) (add 0)) '(0 1 10))
  (check-scope-ops '(1 11) '((add 0) (remove 0)) '(1 11))
  (check-scope-ops '(1 12) '((remove 0) (remove 0)) '(1 12))
  (check-scope-ops '(1 13) '((flip 0) (remove 0)) '(1 13))
  (check-scope-ops '(1 14) '((add 0) (flip 0)) '(1 14))
  (check-scope-ops '(1 15) '((remove 0) (flip 0)) '(0 1 15))
  (check-scope-ops '(1 16) '((flip 0) (flip 0)) '(1 16))

  (check-scope-ops '(0 1 17) '((add 0)) '(0 1 17))
  (check-scope-ops '(0 1 18) '((remove 0)) '(1 18))
  (check-scope-ops '(0 1 19) '((flip 0)) '(1 19))

  (check-scope-ops '(0 1 2) '((add 0) (flip 0)) '(1 2))

  (check-scope-ops
   '(0 1 2)
   '((add 0) (flip 0) (add 1) (add 2) (add 3) (remove 3) (flip 3) (flip 0))
   '(0 1 2 3))
  )

(module+ canonical-scope
  (provide
   canonical-scope
   canonical-scopes)

  (define canonical-table : (HashTable Natural Scope) (make-hash))
  (define (canonical-scope (nat : Natural)) : Scope
    (hash-ref canonical-table
              nat
              (lambda ()
                (define scope (Scope nat))
                (hash-set! canonical-table nat scope)
                scope)))

  (define (canonical-scopes (nats : (Listof Natural)))
    : SetofScopes
    (list->seteq
     (ann (for/list ((nat : Natural nats))
            (canonical-scope nat))
          (Listof Scope))))

  (module+ test
    (require typed/rackunit)

    ;; ISSUE: Problem Report 15153 check-eq? unreliable in typed/racket:
    (check-true (eq? (canonical-scope 0) (canonical-scope 0)))
    (check-false (eq? (canonical-scope 0) (Scope 0)))
    (check-false (eq? (canonical-scope 0) (canonical-scope 1))))
  )

(module+ test
  (require (submod ".." canonical-scope test)))
