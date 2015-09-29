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
                       (flips : SetofScopes)))

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

(module+ test
  (require typed/rackunit)
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

  (: check-scope-ops (-> (Listof Natural)
                         (Listof (List (U 'add 'remove 'flip) Natural))
                         (Listof Natural)
                         Any))

  (define (check-scope-ops init raw-ops result)

    (define init-scopes (canonical-scopes init))

    (define-values (#{ops : SetofScopeOps}
                    #{check : SetofScopes})
      (for/fold ((ops (empty-SetofScopeOps))
                 (check init-scopes))
                ((raw-op : (List (U 'add 'remove 'flip) Natural) raw-ops))
        (match raw-op
          ((list 'add scope-index)
           (define scope (canonical-scope scope-index))
           (values
            (SetofScopeOps-add ops scope)
            (set-add check scope)))
          ((list 'remove scope-index)
           (define scope (canonical-scope scope-index))
           (values
            (SetofScopeOps-remove ops scope)
            (set-remove check scope)))
          ((list 'flip scope-index)
           (define scope (canonical-scope scope-index))
           (values
            (SetofScopeOps-flip ops scope)
            (set-symmetric-difference check (seteq scope)))))))

    (define result-scopes (canonical-scopes result))

    (define apply-scopes (SetofScopeOps-apply ops init-scopes))

    (check-set-equal? check result-scopes)
    (check-set-equal? apply-scopes result-scopes))

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
