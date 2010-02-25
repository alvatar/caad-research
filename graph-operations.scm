;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import graph)

;; Apply operation to context
;;
(define (apply-operation-to-context operation context-builder graph)
  (define matches-context? equal?)
  (define (do-in-context graph-tail)
    (remove
      (lambda (lst)
        (if (equal? lst '()) #t #f))
      (map
        (lambda (graph-elem)
          (if (not (null? graph-elem))
              (if (matches-context? graph-elem (context-builder graph)) ; TODO: optimize context-builder with let
                  (operation 'local graph graph-elem)
                  (if (list? graph-elem)
                      (begin (do-in-context graph-elem)
                           ;(display graph-tail)(newline)
                           graph-elem)
                      graph-elem))))
        graph-tail)))
  (if (equal? (context-builder graph) graph)
      (operation 'local graph graph)
      (do-in-context graph)))

;; Apply operation to a graph and all contexts matching
;;
(define (apply-operation operation context-builder graph)
  (operation 'prepare)
  (operation 'global (apply-operation-to-context operation context-builder graph)))

;; Identity
;;
(define (op-identity args #!optional graph subgraph)
  (cond
   ((eqv? args 'prepare)
    '())
   ((eqv? args 'local)
    subgraph)
   ((eqv? args 'global)
    graph)))

;; Remove
;;
(define (op-remove args #!optional graph subgraph)
  (cond
   ((eqv? args 'prepare)
    '())
   ((eqv? args 'local)
    '())
   ((eqv? args 'global)
    graph)))

;; Partition
;;
(define op-partition
  (let ((new-uuid (make-uuid)))
    (lambda (args #!optional graph subgraph)
      (cond
       ((eqv? args 'prepare)
        (set! new-uuid (make-uuid)))
       ((eqv? args 'local)
        (if (eqv? (car subgraph) 'room) ; Checks if this is the right context
            (cons (car subgraph)
                  (append (cdr subgraph)
                          (list `(wall (@ (uid ,new-uuid))))))
            subgraph))
       ((eqv? args 'global)
        (add-wall graph
                  (make-point (exact->inexact (random-integer 200)) 280.0)
                  (make-point (exact->inexact (random-integer 200)) 380.0)
                  new-uuid))))))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))


