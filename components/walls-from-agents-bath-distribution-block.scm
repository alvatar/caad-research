;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: make partitions from agents, using the distribution as a
;;; positive agent, participating in distribution from the beginning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../context
        ../core/tagged-list
        ../core/debugging
        ../core/functional
        ../core/list
        ../core/randomization
        ../core/syntax
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
        agent
        auxiliary-graph-elements
        world)

;;; First step of wall generation

(define (add-bath-corridor-container graph world exit)
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
                                                (direction:perpendicular
                                                 (segment->direction
                                                  (pseq->segment
                                                   (wall-pseq
                                                    (graph:nearest-wall graph base-point)))))))
                                             (/ corridor-width 2))
                                        ;(visualization:line-now parallel-1)
                                        ;(visualization:line-now parallel-2)
             ;; TODO: IMPORTANT if too close to a wall, add only one
             (let ((new-graph
                    ((compose op:cut line->context+arguments)
                     ((compose op:cut line->context+arguments)
                      graph
                      parallel-1)
                     parallel-2)))
               (values
                new-graph
                ;; Passes also the distribution direction
                (cons (line->direction parallel-1)
                      ;; Push away all agents that fall inside the corridor
                      (push-away-non-corridor-agents new-graph))
                exit)))))

;;; Second step of wall generation

(define (add-rest-of-rooms graph relief exit)
  (let ((distribution-direction (car relief))  ; TODO: instead of relief, use room-axis
        (world (cdr relief)))
    (let ((find-next-room-to-partition
           (lambda (graph)
             (find (lambda (r)
                     (> (count.agents-in-room graph (world-agents world) r) 1))
                   (graph:filter.rooms graph))))
          (choose-point
           (lambda (graph room)
             (let* ((agents (binary-shuffle-list (find.agents-in-room graph
                                                                      (world-agents world)
                                                                      room)))
                    (reference-point (agent-head-position (car agents))))
               (vect2:inexact->exact
                (~generate.random-point/two-points
                 reference-point
                 (car (agent-positions (most (lambda (a b)
                                               (min (~distance.point-point (car (agent-positions a))
                                                                           reference-point)
                                                    (~distance.point-point (car (agent-positions b))
                                                                           reference-point)))
                                             (cdr agents))))))))))

      (let room-cycle ((graph graph)
                       (agents (world-agents world)))
        (uif (find-next-room-to-partition graph)
             (let ((agents-in-room (find.agents-in-room graph
                                                        agents
                                                        ?it))
                   (new-graph ((compose op:cut room&line->context+arguments)
                               graph
                               ?it
                               (point&direction->line
                                (choose-point graph ?it)
                                (direction:perpendicular
                                 distribution-direction)))))
               (room-cycle new-graph
                           agents))
             (values graph
                     world
                     exit))))))

;;; Check if there is the proper relationship between agents and rooms, fix if needed

(define (merge-residual-space graph world exit)
  (define-choice! "choose-room-to-merge"
    (choose-merge-room room)
    (find (lambda (r)
            (aif res
                 null?
                 (graph:filter.common-room-walls r room)
                 #f
                 res))
          (remove
           (lambda (x) (equal? x room))
           (graph:filter.rooms graph))))
  (let loop-until-fixed ((graph graph))
    (aif wrong-room
         (find (lambda (r)
                 (not (= (count.agents-in-room graph (world-agents world) r) 1)))
               (graph:filter.rooms graph))
         (loop-until-fixed (op:merge
                            (many->context graph
                                           wrong-room
                                           (choose-merge-room wrong-room))))
         (values graph world exit))))

;;; Give the proper name to rooms

(define (name-rooms graph world exit)
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
                       (list@ (element room)
                              (name (symbol->string
                                     (agent-label
                                      (car agent)))))))))
    graph
    (graph:filter.rooms graph))
   world
   exit))

;;; Adjust the measures of the spaces to snap near structurals

(define (snap-to-structurals graph world exit)
  (values (let ((min-dist-wall-structural 1.0))
            (fold
             (lambda (str-center graph)
               ;; adjust each wall to the current structural
               (let recur ((graph graph)
                           (walls (graph:filter.walls graph)))
                 (if (null? walls)
                     graph
                     ;; find walls that are too close to a structural
                     (receive
                      (wall walls-tail)
                      (car+cdr walls)
                      (if (and (not (graph:exterior-wall? wall graph))
                               (< (~distance.point-pseq str-center
                                                        (wall-pseq wall))
                                  min-dist-wall-structural))
                          ;; find the room 
                          (recur (let* ((room (find (lambda (r) (graph:point-in-room? graph r str-center))
                                                    (graph:filter.rooms graph)))
                                        (guides
                                         (graph:filter.walls-connected/wall/room graph wall room)))
                                   (if (null? guides)
                                       graph
                                       (op:glide graph
                                                 (list@ (element wall)
                                                        (constraints
                                                         (list@ (method 'keep-direction)
                                                                (guides guides)
                                                                (traits 'remove-holes)))
                                                        (movement
                                                         (list@ (method 'towards)
                                                                (room room)
                                                                (unit 'exact)
                                                                (value (~distance.point-pseq
                                                                        str-center
                                                                        (wall-pseq wall)))))))))
                                 walls-tail)
                          (recur graph walls-tail))))))
             graph
             (map (lambda (str) (pseq:centroid (structural-pseq str)))
                  (graph:filter.structurals graph))))
          world
          exit))

;;; Add bathroom

(define (add-bathroom graph world exit)
  (let* ((distribution-room (graph:find.room/uid graph "distribution"))
         #;(new-wall-point ((compose op:cut room&line->context+arguments)
                          graph
                          distribution-room
                          (point&direction->line
                           (pseq:centroid
                            (graph:room->pseq
                             graph
                             distribution-room))
                           (direction:perpendicular
                            (find.direction/longest-midsegment
                             (graph:room->pseq graph distribution-room))))))) ; TODO: guard for squared rooms
   (values
    graph
    (make-world
     (cons (world-agents world)
           (make-agent 'bathroom
                       (list (make-point 0.0 0.0))
                       '()
                       '()))
     (world-fields world))
    exit)))

;;; Add the doors

(define (add-doors graph world exit)
  (values graph world exit))

;;; Utility drawing step

(define (draw-result graph world exit)
  (visualization:forget-all)
  (visualize-graph graph)
  (visualize-world world graph)
  (visualization:do-now)
  (values graph world exit))

;;; The component

(define (walls-from-agents/distribution&bath-block graph world exit)
  (let ((finished-agents (world-agents world)))
    ((compose-right
      add-bath-corridor-container
      add-rest-of-rooms
      merge-residual-space
      name-rooms
      snap-to-structurals
      add-bathroom
      add-doors
      ;; draw-result
      )
     graph
     (make-world finished-agents '())
     exit)))