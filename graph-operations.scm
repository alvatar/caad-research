;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import graph)

;; Apply to context
;;
(define (apply-operation-to-context operation context)
  (map
    (lambda
      (sub)
      (operation sub))
    context))

;; Partition
;;
(define (partition graph context-builder)
  (define (partition-impl subgraph)
    (if
      (equal? (car subgraph) 'wall)
      `(wall ; TODO
       (pt (@ (y ,(number->string (* 0.9 (random-integer 500)))) (x "150.0")))
       (pt (@ (y "100.0") (x "450.0"))))
      subgraph))
  (apply-operation-to-context partition-impl (context-builder graph)))

;; Remove
;;
(define (remove graph context-builder)
  (define (partition-impl subgraph)
    subgraph)
  (apply-operation-to-context remove-impl (context-builder graph)))

;; Expand
;;
(define (expand graph context-builder)
  (define (partition-impl subgraph)
    subgraph)
  (apply-operation-to-context expand-impl (context-builder graph)))


