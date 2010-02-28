;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import graph)
(import utilities)

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
            (call-with-values (lambda () (break-deep matches-context? elem))
                              (lambda (a b)
                                (if (equal? b '())
                                    (do-in-context elem)
                                    new-subgraph)))
            elem))
      graph-tail)))
  (do-in-context graph))

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

;; Partition
;;
(define (op-partition graph context-builder constraints)
  (let ((new-uuid (make-uuid))
        (subgraph (context-builder graph)))
        (add-wall
          (apply-operation-in-context
            graph
            context-builder
            (cons (car subgraph)
                  (append (cdr subgraph)
                          (list `(wall (@ (uid ,new-uuid)))))))
          (point-from-relative-in-wall
            (room-wall (car (rooms graph)) graph 0)
            (random-real))
          (point-from-relative-in-wall
            (room-wall (car (rooms graph)) graph 2)
            (random-real))
          new-uuid)))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))

