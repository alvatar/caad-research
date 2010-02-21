(import graph)
(import graph-operations)
(import context)
(import constraints)

(define (make-graph-mutations graph constraints)
  (list ((make-operation/context-set) graph)))

;; All the logic of what operations should be done stems from here
;;
(define (make-operation/context-set)
  (lambda
    (graph)
      (partition graph any-context)))

