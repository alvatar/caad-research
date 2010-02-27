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
                           graph-elem)
                      graph-elem))))
        graph-tail)))
  (if (equal? (context-builder graph) graph)
      (operation 'local graph graph)
      (do-in-context graph)))

;; Apply operation to a graph and all contexts matching
;;
(define (apply-operation operation operation-validator context-builder graph)
  (let ((new-graph ((lambda () ; Execute this to bind to an operation only if is valid
                     (let do-until-valid ()
                       (operation 'prepare)
                       (if (operation-validator (operation 'local graph (context-builder graph)))
                           (apply-operation-to-context operation context-builder graph)
                           (do-until-valid)))))))
    (if (equal? new-graph graph) ; Only if the graph was modified
        graph
        (operation 'global new-graph))))

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
       (print-graph graph)
        (add-wall graph
                  (point-from-relative-in-wall
                   (room-wall (car (rooms graph)) graph 0)
                   (random-real))
                  (point-from-relative-in-wall
                   (room-wall (car (rooms graph)) graph 2)
                   (random-real))
                  new-uuid))))))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))


