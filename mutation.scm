(import graph)
(import graph-operations)
(import context)
(import constraints)

(define (make-graph-mutations graph constraints)
  (list ((make-operation/context-set) graph)))

;; All the logic of which operations should be performed stems from here
;;
(define (make-operation/context-set)
  (lambda (graph)
    (apply-operation op-partition biggest-room graph)))
