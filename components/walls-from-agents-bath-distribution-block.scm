;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: make partitions from agents, using the distribution as a
;;; positive agent, participating in distribution from the beginning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../context)
(import ../core/syntax)
(import ../core/functional)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../graph)
(import ../graph-visualization)
(import ../math/exact-algebra)
(import ../math/inexact-algebra) ; TODO: Could be removed!
;(import ../operators)
(import ../graph-operations)
(import ../output)
(import ../visualization)


(define (add-bath-corridor-block graph world)
  (define (calculate-corridor-width) 1.0)
  (values
   (receive (parallel-1 parallel-2)
            (~generate.parallels-at-distance (let ((base-point
                                                    (car (agent-positions
                                                          (find-agent (world-agents world)
                                                                      'distribution)))))
                                               (point+direction->line
                                                base-point
                                                (graph:wall-perpendicular
                                                 (graph:closest-wall graph base-point))))
                                             (calculate-corridor-width))
            (visualization:line-now parallel-1)
            (visualization:line-now parallel-2)
            (compose
             (op:cut (graph+line->context graph
                                          parallel-1))
             (op:cut (graph+line->context graph
                                          parallel-2))))
   world))

(define (step2 graph world) (pp 'step2) (values graph world))

(define (step3 graph world) (pp 'step3) (values graph world))

(define (walls-from-agents/distribution&bath-block graph world)
  (let ((finished-agents (world-agents world)))
    ((compose-right
      add-bath-corridor-block
      step2
      step3)
     graph
     (make-world finished-agents '()))))
    