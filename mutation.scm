(import graph)
(import graph-operations)
(import constraints)

(define (make-graph-mutations graph constraints)
  (list ((make-operations-set) graph)))

;; Operation selector
;;
(define (make-operations-set)
  (lambda
    (graph)
      (partition graph)))

