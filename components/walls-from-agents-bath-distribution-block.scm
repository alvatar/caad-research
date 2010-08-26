;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: make partitions from agents, using the distribution as a
;;; positive agent, participating in distribution from the beginning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../context
        ../core/syntax
        ../core/functional
        ../core/list
        ../core/randomization
        ../core/debugging
        ../geometry/generation
        ../geometry/kernel
        ../geometry/locus
        ../geometry/query
        ../graph
        ../graph-visualization
        ../math/exact-algebra
        ../math/inexact-algebra
        ../operators
        ../graph-operations
        ../logic
        ../output
        ../visualization
        auxiliary-graph-elements
        generation-elements)

;;; First step of wall generation

(define (add-bath-corridor-container graph world)
  (let ((corridor-width #e1.6)
        (push-away-non-corridor-agents
         (lambda (graph)
           (let ((agents (world-agents world)))
             (aif wrong-agents
                  null?
                  ;; Find out if there are any wrong agents inside the corridor container
                  (remove
                   (lambda (a) (eq? (agent-label a) 'distribution))
                   (find.agents-in-room graph
                                        agents
                                        (find.room/agent graph agents 'distribution)))
                  world
                  (make-world
                   ;; Map all the agents -> move the wrong agents
                   (map-if (lambda (a) (any (lambda (wa) (eq? (agent-label wa) (agent-label a))) wrong-agents))
                           (lambda (a) (move-agent-head
                                   a
                                   (let ((p (agent-head-position a)))
                                     ;; It will choose another wall if the agent ends up outside
                                     (let cycle-nearest-walls ((sorted-walls (sort.distance.agent<->walls graph a)))
                                       (let ((wpseq (wall-pseq (car sorted-walls))))
                                         (aif valid-point
                                              (lambda (vp) (graph:point-inside? graph vp))
                                              (vect2:inexact->exact
                                               (vect2+ (vect2:*scalar
                                                        (vect2:~normalize
                                                         (point&pseq-perpendicular->direction p wpseq))
                                                        (+ (sqrt (~distance.point-pseq p wpseq)) 1)) ; 1 m. away from wall
                                                       p))
                                              valid-point
                                              (cycle-nearest-walls (cdr sorted-walls))))))))
                           agents)
                   '()))))))
    (receive (parallel-1 parallel-2)
             (generate.parallels-at-distance (let ((base-point
                                                    (car (agent-positions
                                                          (find-agent (world-agents world)
                                                                      'distribution)))))
                                               (point&direction->line
                                                base-point
                                                (graph:wall-perpendicular
                                                 (graph:closest-wall graph base-point))))
                                             (/ corridor-width 2))
                                        ;(visualization:line-now parallel-1)
                                        ;(visualization:line-now parallel-2)
             ;; TODO: IMPORTANT if too close to a wall, add only one
             (let ((new-graph
                    (op:cut
                     (graph&line->context (op:cut (graph&line->context graph
                                                                       parallel-1))
                                          parallel-2))))
               (values
                new-graph
                ;; Passes also the distribution direction
                (cons (line->direction parallel-1)
                      ;; Push away all agents that fall inside the corridor
                      (push-away-non-corridor-agents new-graph)))))))

;;; Second step of wall generation

(define (add-rest-of-rooms graph relief)
  (let ((distribution-direction (car relief))
        (world (cdr relief)))
    (let ((find-next-room-to-partition
           (lambda (graph)
             (find (lambda (r)
                     (> (count.agents-in-room graph (world-agents world) r) 1))
                   (graph:find.rooms graph))))
          (choose-point
           (lambda (graph room)
             (let* ((agents (binary-shuffle-list (find.agents-in-room graph
                                                                      (world-agents world)
                                                                      room)))
                    (reference-point (agent-head-position (car agents))))
               (generate.random-point/two-points
                reference-point
                (car (agent-positions (most (lambda (a b)
                                              (min (~distance.point-point (car (agent-positions a))
                                                                          reference-point)
                                                   (~distance.point-point (car (agent-positions b))
                                                                          reference-point)))
                                            (cdr agents)))))))))

      (let room-cycle ((graph graph)
                       (agents (world-agents world)))
        (uif (find-next-room-to-partition graph)
             (let ((agents-in-room (find.agents-in-room graph
                                                        agents
                                                        ?it))
                   (new-graph (op:cut (room&line->context graph
                                                          ?it
                                                          (point&direction->line
                                                           (choose-point graph ?it)
                                                           (direction:perpendicular
                                                            distribution-direction))))))
               (room-cycle new-graph
                           agents))
             (values graph
                     world))))))

;;; Check if there is the proper relationship between agents and rooms, fix if needed

(define (merge-residual-space graph world)
  (define-choice! "merge-room?"
    (choose-merge-room room)
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
                 (not (= (count.agents-in-room graph (world-agents world) r) 1)))
               (graph:find.rooms graph))
         (loop-until-fixed (op:merge
                            (room&room->context graph
                                                wrong-room
                                                (choose-merge-room wrong-room))))
         (values graph world))))

;;; Give the proper name to rooms

(define (name-rooms graph world)
  (values
   (fold
    (lambda (room graph)
      (let ((agent (find.agents-in-room
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
      add-bath-corridor-container
      add-rest-of-rooms
      draw-result
      merge-residual-space
      name-rooms
      draw-result)
     graph
     (make-world finished-agents '()))))