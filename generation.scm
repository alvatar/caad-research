;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms for graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import graph)
(import generation-elements)
(import strategies)

;-------------------------------------------------------------------------------
; General procedures
;-------------------------------------------------------------------------------

;;; Generation algorithm components type
;;; iteration-steps: must return a values construct with "graph" and "world"
;;; termination-predicates: must return whether this step should be terminated

;(define-structure generation-components iteration-steps termination-predicates)

;;; Make a generator procedure that takes an initial graph as argument

#|
(define (generate-from-graph generation-components)
  (lambda (graph)
    (define (execute-step it-steps term-preds graph world)
      (cond
       ((or (null? it-steps) (null? term-preds))
        graph)
       (((car term-preds) graph world) ; execute predicate
        (execute-step (cdr it-steps) (cdr term-preds) graph world))
       (else
        (receive (g w) ; execute iteration step
          ((car it-steps) graph world)
          (execute-step it-steps term-preds g w)))))
      (execute-step
        (generation-components-iteration-steps generation-components)
        (generation-components-termination-predicates generation-components)
        graph
        '())))
        |#
(define (generate-from-graph steps)
  (define (execute-step rest-steps graph world)
    (cond
     ((null? rest-steps)
      graph)
     (else
      (receive (g w)
        ((car rest-steps) graph world)
        (execute-step (cdr rest-steps) g w)))))
  (lambda (graph)
    (execute-step steps graph '())))

;;; Generator: creates a procedure that can be used for generating graphs. It takes
;;;   care of choosing the right generation approach and the generation algorithmic
;;;   components
;;; generation-hints: gives information to help choosing generation algorithms
;;; seed-data: the data used for generating (genetic codes or model), used for checking
;;;   if the generator can be really used with the seed-data

(define (generator generation-hints seed-data)
  (define (select-components)
    hinted-brownian-agents) ; TODO: expand for more components' types
  (cond
    ((and (not generation-hints) (graph? seed-data))
     (generate-from-graph (select-components)))
    (else
     (error "generator: No generation algorithm for this kind of seed data"))))
