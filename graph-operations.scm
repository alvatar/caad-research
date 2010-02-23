;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import graph)

;; Apply to context
;;
(define (apply-to-context operation context-builder graph)
  (define (do-in-context graph-tail)
    (let
      ((context (context-builder graph)))
      (if
        (and (list? graph-tail) (not (null? graph-tail))) ; LIST? quitar
        (if
          (equal? graph-tail (context-builder graph))
          ;(begin (print-graph graph-tail) graph-tail)
          (operation graph graph-tail)
          (do-in-context (cdr graph-tail)))
        '())))
  (do-in-context graph))

;; Partition
;;
(define (op-partition graph subgraph)
  ;(define (partition-impl subgraph)
    (if
      (equal? (car subgraph) 'wall)
      ; `(wall ; TODO
       ; (pt (@ (y ,(number->string (* 0.9 (random-integer 500)))) (x "150.0")))
       ; (pt (@ (y "100.0") (x "450.0"))))
       subgraph
       subgraph))
  ;(apply-to-context (context-builder graph) graph))

;; Remove
;;
; (define (remove graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context remove-impl (context-builder graph)))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))


