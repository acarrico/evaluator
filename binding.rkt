#lang typed/racket/base

(require
 racket/match
 racket/set
 "scope.rkt")

(provide
 (struct-out Binding)
 BindingTable
 empty-BindingTable
 BindingTable-extend
 BindingTable-resolve
 )

;; ISSUE: The binding table is not a persistent data structure. It
;; seems ok to ignore this issue for now, since the side effects are
;; benign: extending the table with new entries and garbage collecting
;; unreferenced entries. The new entries would become a problem if the
;; expander was to branch somehow (speculative expansion?), and use
;; the same scope two ways.

(struct Binding ((name : Symbol)) #:transparent)
(struct BindingEntry ((scopes : SetofScopes) (binding : Binding)) #:transparent)
(define-type BindingSubtable (HashTable Symbol (Listof BindingEntry)))
(define-type BindingTable (HashTable Scope BindingSubtable))

(define (empty-BindingTable) : BindingTable (make-weak-hash))

(define (BindingTable-subtable (table : BindingTable) (scope : Scope))
  : (U BindingSubtable #f)
  ((inst hash-ref Scope BindingSubtable #f) table scope #f))

(define (BindingSubtable-ref (subtable : BindingSubtable) (name : Symbol))
  ((inst hash-ref Symbol (Listof BindingEntry) (Listof BindingEntry))
   subtable
   name
   (lambda () '())))

(define (BindingTable-ref (table : BindingTable) (scope : Scope) (name : Symbol))
  : (Listof BindingEntry)
  (define maybe-subtable (BindingTable-subtable table scope))
  (if maybe-subtable
      (BindingSubtable-ref maybe-subtable name)
      '()))

;; The table is divided internally into subtables for each scope.
;; Store the binding in the hint's subtable. A subtable becomes
;; garbage when its scope is no longer (otherwise) referenced, so a
;; good hint will allow the binding to be collected quicker.
(define (BindingTable-extend (table : BindingTable)
                             (hint : Scope)
                             (name : Symbol)
                             (scopes : SetofScopes)
                             (binding : Binding))
  : BindingTable
  (hash-update!
   table
   hint
   (lambda ((subtable : BindingSubtable))
     : BindingSubtable
     (hash-update
      subtable
      name
      (lambda ((binding-entries : (Listof BindingEntry)))
        (cons (BindingEntry scopes binding) binding-entries))
      (lambda () '())))
   (lambda ()
     : BindingSubtable
     (hasheq name '())))
  table)

(define (BindingTable-resolve (table : BindingTable)
                              (name : Symbol)
                              (scopes : SetofScopes))
  : Binding
  (define-values (binding-entries count)
    (for*/fold ((best-binding-entries : (Listof BindingEntry) '())
                (best-count : Natural 0))
               ((scope scopes)
                (binding-entry (BindingTable-ref table scope name)))
      (if (subset? (BindingEntry-scopes binding-entry) scopes)
          (let ((count (set-count (BindingEntry-scopes binding-entry))))
            (cond ((< count best-count)
                   (values best-binding-entries best-count))
                  ((= count best-count)
                   (values (cons binding-entry best-binding-entries) best-count))
                  (else
                   (values (list binding-entry) count))))
          (values best-binding-entries best-count))))
  (match binding-entries
    ((list)
     ;; no matching bindings, so use the original name:
     (Binding name))
    ((list binding-entry)
     ;; one best binding, so use it:
     (BindingEntry-binding binding-entry))
    (else
     ;; Oh no! More than one best binding:
     (error "BindingTable-resolve: ambiguous reference!"))))

(module+ test
  (require typed/rackunit)

  (require (submod "scope.rkt" canonical-scope))

  ;; Something that isn't in the table should use the original name:
  (check equal?
    (BindingTable-resolve
     (BindingTable-extend
      (empty-BindingTable) (canonical-scope 0) 'x (canonical-scopes '(0 1)) (Binding 'x-01))
     'y
     (canonical-scopes '(0 1)))
    (Binding 'y))

  (define s0 (canonical-scope 0))
  (define set-s0 (canonical-scopes '(0)))
  (define s1 (canonical-scope 1))
  (define set-s01 (canonical-scopes '(0 1)))
  (define set-s012 (canonical-scopes '(0 1 2)))
  (define a0 (Binding 'a-0))
  (define entry-a0 (BindingEntry set-s0 a0))
  (define b0 (Binding 'b-0))
  (define entry-b0 (BindingEntry set-s0 b0))
  (define a01 (Binding 'a-01))
  (define entry-a01 (BindingEntry set-s01 a01))
  (define a012 (Binding 'a-012))
  (define entry-a012 (BindingEntry set-s012 a012))

  (define binding-table
    (BindingTable-extend (empty-BindingTable) s0 'a set-s0 a0))

  (check-true
   (equal?
    (BindingTable-ref binding-table s0 'a)
    (list entry-a0)))

  (set! binding-table
        (BindingTable-extend binding-table s0 'b set-s0 b0))

  (check-true
   (equal?
    (BindingTable-ref binding-table s0 'b)
    (list entry-b0)))

  (set! binding-table
        (BindingTable-extend binding-table s1 'a set-s01 a01))

  (check-true
   (equal?
    (BindingTable-ref binding-table s1 'a)
    (list entry-a01)))

  (set! binding-table
        (BindingTable-extend binding-table s1 'a set-s012 a012))

  (check-true
   (equal?
    (list->set (BindingTable-ref binding-table s1 'a))
    (set entry-a01 entry-a012)))

  (check-true
   (equal?
    (BindingTable-resolve binding-table 'a set-s0)
    a0))

  (check-true
   (equal?
    (BindingTable-resolve binding-table 'a set-s01)
    a01))

  (check-true
   (equal?
    (BindingTable-resolve binding-table 'a set-s012)
    a012))

  )
