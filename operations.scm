;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import graph)

;; Apply operation to context
;;
(define (apply-operation-in-context graph context-selector new-subgraph)
  (define (matches-context? elem)
    (equal? elem (context-selector graph))) ; optimize
  (define (do-in-context graph-tail)
    (map
     (lambda (elem)
       (if (pair? elem)
           (call-with-values (lambda () (break matches-context? elem))
                             (lambda (a b)
                               (if (equal? b '()) ; If nothing found (b is null)
                                   (do-in-context elem)
                                   (append a new-subgraph (cdr b)))))
           elem))
     graph-tail))
  (car (do-in-context (list graph)))) ; Iteration must start at top level

;; Apply operation to a graph and all contexts matching
;;
(define (apply-operation 
          operation
          graph
          context-selector
          constraints
          operation-validator)
  (let do-until-valid ()
    (let ((new-graph (operation graph context-selector constraints)))
      (if (operation-validator new-graph)
        new-graph
        (do-until-valid)))))

;-------------------------------------------------------------------------------
; Basic operations
;-------------------------------------------------------------------------------

;; Identity
;;
(define (op-identity graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   (context-selector graph)))

;; Remove
;;
(define (op-remove graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Topology modifications
;-------------------------------------------------------------------------------

;; Split a room
;;
(define (op-split graph context-selector constraints)
  (let ((new-uuid (make-uuid))
        (subgraph (context-selector graph)))
    (add-wall
      (apply-operation-in-context
        graph
        context-selector
        (list
          (cons (car subgraph)
                (append (cdr subgraph)
                        (list `(wall (@ (uid ,new-uuid))))))))
      (point-from-relative-in-wall
        (room-wall (car (rooms graph)) graph 0)
        (constraints (random-real)))
      (point-from-relative-in-wall
        (room-wall (car (rooms graph)) graph 2)
        (constraints (random-real)))
      new-uuid)))

;; Merge two rooms
;;
(define (op-merge graph context-selector constraints)
  '())

;; Create new element
;;
(define (op-create-element graph context-selector constraints)
  '())

;; Remove element
;;
(define (op-remove-element graph context-selector constraints)
  '())

;; Move
;;
(define (op-move graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Boundary modifications
;-------------------------------------------------------------------------------

;; Expand
;;
(define (op-expand graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Contract
;;
(define (op-contract graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Post-operations
;-------------------------------------------------------------------------------

;; Stabilize structure: add, move or replace pilars
;;
(define (op-stabilize-structure graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Fix accesses
;;
(define (op-fix-accesses graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Fix malformed graph
;;
(define (op-fix-malformed graph context-selector constraints)
  (remove
    (lambda (lst)
      (if (equal? lst '()) #t #f))
    graph))

;; Fix everything
;;
(define (op-fix-malformed graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))
