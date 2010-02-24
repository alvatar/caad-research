;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import graph)

;; Apply operation to context
;;
(define (apply-operation-to-context operation context-builder graph)
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
              (operation 'local graph graph-elem)
              (if
                (list? graph-elem)
                (begin (do-in-context graph-elem)
                     ;(display graph-tail)(newline)
                     graph-elem)
                graph-elem))))
        graph-tail)))
  (if
    (equal? (context-builder graph) graph)
    (operation 'local graph graph)
    (do-in-context graph)))

;; Apply operation to a graph and all contexts matching
;;
(define (apply-operation operation context-builder graph)
  (operation 'global (apply-operation-to-context operation context-builder graph) graph))

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
(define (op-partition level graph subgraph)
  (cond
    ((eqv? level 'local)
     (if
       (equal? (car subgraph) 'room)
       ; `(architecture (wall ; TODO
       ; (pt (@ (y ,(number->string (* 0.9 (random-integer 500)))) (x "150.0")))
       ; (pt (@ (y "100.0") (x "450.0")))))
       ;'()
       (cons (car subgraph)
             (append (cdr subgraph)
                     (list '(wall (@ (uid "SDFSDFSFSDFSDF")))))) ; TODO: UID generator
       subgraph))
    ((eqv? level 'global)
     (add-wall graph
               (make-point (exact->inexact (random-integer 200)) 280.0)
               (make-point (exact->inexact (random-integer 200)) 380.0) ))))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))


