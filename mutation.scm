(import graph)
(import operations)
(import context)
(import validators)
(import constraints)

(define (make-graph-mutations graph)
  (list ((make-operation/context-set) graph)))

;; All the logic of which operations should be performed stems from here
;;
(define (make-operation/context-set)
  (lambda (graph)
    (apply-operation
      op-split
      graph
      biggest-room
      unconstrained
      all-valid)))
