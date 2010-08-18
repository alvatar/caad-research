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
(import ../core/list)
(import ../core/randomization)
(import ../core/debugging)
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

;;; First step of wall generation

(define (add-bath-corridor-block graph world)
  (define (calculate-corridor-width) (inexact->exact 1.6))
  (receive (parallel-1 parallel-2)
           (generate.parallels-at-distance (let ((base-point
                                                  (car (agent-positions
                                                        (find-agent (world-agents world)
                                                                    'distribution)))))
                                             (point+direction->line
                                              base-point
                                              (graph:wall-perpendicular
                                               (graph:closest-wall graph base-point))))
                                           (/ (calculate-corridor-width) 2))
                                        ;(visualization:line-now parallel-1)
                                        ;(visualization:line-now parallel-2)
           (values
            (op:cut
             (graph+line->context (op:cut (graph+line->context graph
                                                               parallel-1))
                                  parallel-2))
            (cons (line->direction parallel-1) world))))

;;; Second step of wall generation

(define (add-rest-of-rooms graph relief)
  (let ((distribution-direction (car relief))
        (world (cdr relief)))

    (define (find-next-room-to-partition graph)
      (find (lambda (r)
              (> (num-agents-in-room graph (world-agents world) r) 1))
            (graph:find.rooms graph)))

    (define (choose-point graph room)
      (let* ((agents (binary-shuffle-list (agents-in-room graph
                                                          (world-agents world)
                                                          room)))
             (reference-point (car (agent-positions (car agents)))))
        (generate.random-point/two-points
         reference-point
         (car (agent-positions (most (lambda (a b)
                                       (min (~distance.point-point (car (agent-positions a))
                                                                   reference-point)
                                            (~distance.point-point (car (agent-positions b))
                                                                   reference-point)))
                                     (cdr agents)))))))

    (values
     (let do-all-rooms ((graph graph)
                        (room (find-next-room-to-partition graph)))
       ;; (visualization:forget-all)
       ;; (visualize-graph graph)
       ;; (visualize-world world graph)
       ;; (visualization:do-now)
       (let* ((new-graph (op:cut (room+line->context graph
                                                     room
                                                     (point+direction->line
                                                      (choose-point graph room)
                                                      (direction:perpendicular
                                                       distribution-direction)))))
              (next-room (find-next-room-to-partition new-graph)))
         (if next-room
             (do-all-rooms new-graph
                           next-room)
             new-graph)))
     world)))

;;; Check if there is the proper relationship between agents and rooms, fix if needed

(define (merge-residual-space graph world)
  (define (choose-merge-room room)
    (find (lambda (r)
            (aif res
                 null?
                 (graph:filter.common-room-walls r room)
                 #f
                 res))
          (remove
           (lambda (x) (equal? x room))
           (graph:find.rooms graph))))
  (let loop-until-fixed ((graph graph))
    (aif wrong-room
         (find (lambda (r)
                 (not (= (num-agents-in-room graph (world-agents world) r) 1)))
               (graph:find.rooms graph))
         (loop-until-fixed (op:merge
                            (room+room->context graph
                                                wrong-room
                                                (choose-merge-room wrong-room))))
         (values graph world))))

;;; Give the proper name to rooms

(define (name-rooms graph world)
  (values
   (fold
    (lambda (room graph)
      (let ((agent (agents-in-room
                    graph
                    (world-agents world)
                    room)))
        (if (null? agent)
            (begin (display "No agent found in a room!!\n")
                   graph)
            (op:rename graph
                       room
                       (symbol->string
                        (agent-label
                         (car agent)))))))
    graph
    (graph:find.rooms graph))
   world))

;;; Utility drawing step

(define (draw-result graph world)
  (visualization:forget-all)
  (visualize-graph graph)
  (visualize-world world graph)
  (visualization:do-now)
  (values graph world))

;;; The component

(define (walls-from-agents/distribution&bath-block graph world)
  (let ((finished-agents (world-agents world)))
    ((compose-right
      add-bath-corridor-block
      add-rest-of-rooms
      draw-result
      merge-residual-space
      name-rooms
      draw-result)
     graph
     (make-world finished-agents '()))))