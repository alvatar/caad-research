;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import graph)

;; Apply to context
;;
(define (apply-to-context operation context-builder graph)
  (define matches-context? equal?)
  (define (do-in-context graph-tail)
    (remove
      (lambda (lst) (if (equal? lst '()) #t #f))
      (map
        (lambda (graph-elem)
          (if
            (not (null? graph-elem))
            (if
              (matches-context? graph-elem (context-builder graph)) ; TODO: optimize context-builder with let
              (operation graph graph-elem)
              (if
                (list? graph-elem)
                (begin (do-in-context graph-elem)
                     ;(display graph-tail)(newline)
                     graph-elem)
                graph-elem))))
        graph-tail)))
  (if
    (equal? (context-builder graph) graph)
    (operation graph graph)
    (do-in-context graph)))

;; Identity
;;
(define (op-identity graph subgraph)
  subgraph)

;; Remove
;;
(define (op-remove graph subgraph)
  '())

;; Partition
;;
(define (op-partition graph subgraph)
  (add-wall graph)
  (if
    (equal? (car subgraph) 'room)
    ; `(architecture (wall ; TODO
     ; (pt (@ (y ,(number->string (* 0.9 (random-integer 500)))) (x "150.0")))
     ; (pt (@ (y "100.0") (x "450.0")))))
      ;'()
      '(room (@ (label "salon_transformado"))
       (wall (@ (uid "0000000000")))
       (wall (@ (uid "5493820876")))
       (wall (@ (uid "5394501263")))
       (wall (@ (uid "0034923049")))
       )
     subgraph))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))


