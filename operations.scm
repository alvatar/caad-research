;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import graph)

;; Apply operation to context
;;
(define (apply-operation-in-context graph context-builder new-subgraph)
  (define (matches-context? elem)
    (equal? elem (context-builder graph)))
  (define (do-in-context graph-tail)
    (clean-graph
     (map
      (lambda (elem)
        (if (pair? elem)
            (call-with-values (lambda () (break matches-context? elem))
                              (lambda (a b)
                                (if (equal? b '()) ; If nothing found (b is null)
                                    (do-in-context elem)
                                    (append a new-subgraph (cdr b)))))
            elem))
      graph-tail)))
  (car (do-in-context (list graph)))) ; Iteration must start at top level

;; Apply operation to a graph and all contexts matching
;;
(define (apply-operation 
          operation
          graph
          context-builder
          constraints
          operation-validator)
  (let do-until-valid ()
    (let ((new-graph (operation graph context-builder constraints)))
      (if (operation-validator new-graph)
        new-graph
        (do-until-valid)))))

;; Clean graph
;;
(define (clean-graph graph)
  (remove
    (lambda (lst)
      (if (equal? lst '()) #t #f))
    graph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Identity
;;
(define (op-identity graph context-builder constraints)
  (apply-operation-in-context
   graph
   context-builder
   (context-builder graph)))

;; Remove
;;
(define (op-remove graph context-builder constraints)
  (apply-operation-in-context
   graph
   context-builder
   '()))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Topology modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Partition a room
;;
(define (op-partition graph context-builder constraints)
  (let ((new-uuid (make-uuid))
        (subgraph (context-builder graph)))
    (add-wall
      (apply-operation-in-context
        graph
        context-builder
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
(define (op-merge graph context-builder constraints)
  '())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boundary modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Expand
;;
(define (op-expand graph context-builder constraints)
  (apply-operation-in-context
   graph
   context-builder
   '()))
