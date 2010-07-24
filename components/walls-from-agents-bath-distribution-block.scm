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
(import ../core/randomization)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../graph)
(import ../graph-visualization)
(import ../math/exact-algebra)
(import ../operators)
(import ../graph-operations)
(import ../output)
(import ../visualization)
(import auxiliary-graph-elements)


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
            (op:cut
             (graph+line->context (op:cut (graph+line->context graph
                                                               parallel-1))
                                  parallel-2)))
   world))

(define (add-rest-of-rooms graph world)
  (define (find-next-room-to-partition graph)
    (find (lambda (r)
            (> (num-agents-in-room graph (world-agents world) r) 1))
          (graph:find.rooms graph)))
  (define (choose-point room)
    (let* ((agents (binary-shuffle-list (agents-in-room graph
                                                        (world-agents world)
                                                        room)))
           (reference-agent (car agents)))
      (~generate.random-point/two-points
       (car (agent-positions reference-agent))
       (fold (lambda (a min-dist)
               (min min-dist (~distance.point-point
                              (car (agent-positions reference-agent))
                              (car (agent-positions a)))))
             +inf.0
             (cdr agents)))))
  (values (let do-all-rooms ((graph graph)
                             (room (find-next-room-to-partition graph)))
            (let* ((new-graph (op:cut (room+line->context graph
                                                         room
                                                         (point+direction->line
                                                          (choose-point room)
                                                          distribution-direction))))
                   (next-room (find-next-room-to-partition new-graph)))
              (if next-room
                  (do-all-rooms
                   new-graph
                   next-room)
                  new-graph)))
          world))

(define (step3 graph world) (pp 'step3) (values graph world))

(define (walls-from-agents/distribution&bath-block graph world)
  (let ((finished-agents (world-agents world)))
    ((compose-right
      add-bath-corridor-block
      add-rest-of-rooms
      step3)
     graph
     (make-world finished-agents '()))))
    