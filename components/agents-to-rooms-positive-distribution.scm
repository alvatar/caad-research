;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: make partitions from agents, using the distribution as a
;;; positive agent, participating in distribution from the beginning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../context)
(import ../core/syntax)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/generation)
(import ../graph)
(import ../graph-visualization)
(import ../math/exact-algebra)
(import ../math/inexact-algebra) ; TODO: Could be removed!
(import ../operators)
(import ../graph-operations)
(import ../output)
(import ../visualization)


(define (agents-to-rooms-positive-distribution graph world)
  (let
      ((new-graph
        graph))
                                        ;(graph-regeneration-from-agents graph (world-agents world))))
    (graph-regeneration-from-agents graph (world-agents world))
                                        ;(visualization:forget-all)
                                        ;(pp new-graph)
                                        ;(time (visualize-graph new-graph))
                                        ;(visualization:do-now)
                                        ;(visualize-world world new-graph)
    (display "REGENERATION DONE\n")
    (step)

    (values
     new-graph
     (make-world 
      (world-agents world)
      (world-fields world)))))


(define (graph-regeneration-from-agents graph agents)
  ;; (define (choose-agent-a lis)
  ;;   (list-ref lis (random-integer (length lis))))
  ;; (define (choose-agent-b agent-a lis)
  ;;   (fold
  ;;     (lambda (a current)
  ;;       (let ((~distance-current (<distance> agent-a a)))
  ;;         (if (< distance-current (car current))
  ;;             (list distance-current a)
  ;;           current)))
  ;;     (car lis)
  ;;     (cdr lis)))
  ;; (define (make-partition-in-graph-with-references in-room agent-a agent-b)
  ;;   (op-split
  ;;     graph
  ;;     <context>
  ;;     <constraints>
  ;;       (<calculate-point-between-agents> agent-a agent-b)))

  (define (find-next-room-to-partition)
    (find
     (lambda (r)
       (> (num-agents-in-room r) 1))
     (graph:find-rooms graph)))
  
  (define (make-partition-in-graph room)
    (op:split-room
     (receive (points walls)
              (room-line-intersection
               graph
               room
               (visualization:line-now (point+direction->line (vect2+
                                          (vect2:random)
                                          (pseq:centroid (room->pseq graph room))) ; TODO: limit random bias
                                         (graph:wall-perpendicular
                                          (find-longest-wall-in-room graph room)))))
                                        ;(pp graph)
                                        ;(pp (wall-list->pseq-list walls))
              (if (or (not (= 2 (length walls)))
                      (not (= 2 (length points))))
                  (error "NO BIEN"))
              (make-context-tree `[,graph
                                   ()
                                   (,room
                                    ()
                                    (,(car walls)
                                     (,(cadr walls)
                                      ()
                                      (,(cadr points)
                                       ()
                                       ()))
                                     (,(car points)
                                      ()
                                      ())))]))))
  ;; (make-context-tree `[,graph
  ;;                       ()
  ;;                       (,room
  ;;                         ()
  ;;                         (,(room-wall graph room 1);(car walls)
  ;;                          (,(room-wall graph room 3);(cadr walls)
  ;;                           ()
  ;;                           (,(random-real);(cadr points)
  ;;                             ()
  ;;                             ()))
  ;;                          (,(random-real);(car points)
  ;;                            ()
  ;;                            ())))])))

  (define (check-graph graph)
    graph)                              ; TODO: NEXT!

  (pp graph)
                                        ;(for-each (lambda (e) (if (or (room? e) (wall? e)) (pp e))) (graph-architecture graph))
  (visualize-graph graph)
  (visualize-world (make-world agents '()) graph)
  (visualization:do-now)
  (display "\n---------------------------\nSTEP\n")
  (visualization:forget-all)
                                        ;(step)
  ;; Iterate with new graph looking for rooms with more than one agent
  (aif next-room (find-next-room-to-partition)
       (aif new-graph (check-graph (make-partition-in-graph next-room))
            (graph-regeneration-from-agents new-graph agents)
            (graph-regeneration-from-agents graph agents))
       graph))
