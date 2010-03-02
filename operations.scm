;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import graph)
(import utilities)

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
  (let* ((subgraph (context-selector graph))
         (first-wall (room-wall graph (context-selector graph) 0)) ; TODO: First wall selected with constraint
         (second-wall (room-wall graph (context-selector graph) 2)) ; TODO: Second wall selected with constraint, taking first
         (first-split-point (constraints (random-real)))
         (second-split-point (constraints (random-real)))
         (new-wall-uid (make-uuid))
         (first-wall-uid-1-half (make-uuid))
         (first-wall-uid-2-half (make-uuid))
         (second-wall-uid-1-half (make-uuid))
         (second-wall-uid-2-half (make-uuid)))
    ; TODO: Check context somehow and redirect to correct splitting algorithm
    (receive (fore aft)
             (room-break graph subgraph (wall-uid first-wall) (wall-uid second-wall))
      (apply-operation-in-context
        graph
        context-selector
        (append
          (list
            (append `(room (@ (uid ,(make-uuid))))
                    (cdr fore)
                    (list `(wall (@ (uid ,first-wall-uid-1-half))))
                    (list `(wall (@ (uid ,new-wall-uid))))
                    (list `(wall (@ (uid ,second-wall-uid-2-half)))))
            (append `(room (@ (uid ,(make-uuid))))
                    (cdr aft)
                    (list `(wall (@ (uid ,second-wall-uid-1-half))))
                    (list `(wall (@ (uid ,new-wall-uid))))
                    (list `(wall (@ (uid ,first-wall-uid-2-half)))))
            (create-wall
              (point-from-relative-in-wall first-wall first-split-point) ; TODO: With constraints (orthogonality and elements)
              (point-from-relative-in-wall second-wall second-split-point) ; TODO: With constraints (orthogonality and elements)
              new-wall-uid))
          (create-splitted-wall
            (find-wall-with-uid graph (wall-uid (car fore)))
            first-split-point
            first-wall-uid-1-half
            first-wall-uid-2-half)
          (create-splitted-wall
            (find-wall-with-uid graph (wall-uid (car aft)))
            second-split-point
            second-wall-uid-1-half
            second-wall-uid-2-half)))))) ; TODO: añadir puerta

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

;; Fix room topology
;;
(define (op-fix-room-topology graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Fix everything
;;
(define (op-fix-everything graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))
