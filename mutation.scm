(import graph)
(import operations)
(import context)
(import validators)

(define (make-graph-mutations graph constraints)
  (list ((make-operation/context-set) graph)))

;; All the logic of which operations should be performed stems from here
;;
(define (make-operation/context-set)
  (lambda (graph)
    (apply-operation
      op-partition
      all-valid
      biggest-room
      graph)))
