;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: evolutionary algorithm for agents distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/syntax)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../graph)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)
(import ../graph-operations)
(import ../visualization)
(import ../graph-visualization)
(import element-interrelations)

(export agents-evolutionary-distribution)


(define (score-agents-interrelationships agents)
  0.0)

(define (score-agent-orientations a g)
  0.0)

(define (score-agent-illumination agents g)
  (let ((windows (graph:find.windows g)))
    (smallest
     (map
      (lambda (a)
        (case (agent-label a)
          ((distribution)
           0.0)
          ((kitchen)
           0.0)
          ((living)
           (* -1.0 (smallest (map (lambda (w) (max 4.0 (distance.agent<->window a w))) windows))))
          ((room1)
           (* -1.0 (smallest (map (lambda (w) (max 4.0 (distance.agent<->window a w))) windows))))
          ((room2)
           (* -1.0 (smallest (map (lambda (w) (max 4.0 (distance.agent<->window a w))) windows))))
          ((room3)
           (* -1.0 (smallest (map (lambda (w) (max 4.0 (distance.agent<->window a w))) windows))))))
      agents))))

(define (score-agent-distances-to-elements agents g)
  (average
   (map (lambda (a)
          (case (agent-label a)
            ((distribution)
             (* -1.0
                (max ; Only the biggest distance from the necessary elements is important
                 (smallest (map (lambda (e) (distance.agent<->entry a e)) (graph:find.entries g)))
                 (smallest (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g))))))
            ((kitchen)
             (* -1.0
                (smallest
                 (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g)))))
            ((living)
             0.0)
            ((room1)
             0.0)
            ((room2)
             0.0)
            ((room3)
             0.0)
            (else (error "Agent type doesn't exist"))))
        agents)))

(define (score-agent-required-geometrical agents g)
  (average
   (list
    (* -1.0
       (average
        (map (lambda (a1)
               (biggest (map (lambda (a2) (inverse (distance.agent<->agent a1 a2)))
                             (delete a1 agents))))
             agents)))
    (* -1.0
       (average
        (map (lambda (w)
               (biggest (map (lambda (a) (inverse (distance.agent<->wall a w))) agents)))
             (graph:find.walls g)))))))

(define (score agents graph)
  (p (+ (score-agents-interrelationships agents)
        (score-agent-illumination agents graph)
        (score-agent-distances-to-elements agents graph)
        (score-agent-required-geometrical agents graph)
        )))

;; TODO: CREATE FROM OLD ONES (SAME NUMBER)
(define (generate-agents limit-polygon)
  (list
   (make-agent
    'distribution
    (list (pseq:make-random-point-inside limit-polygon))
    '()
    (lambda (world a) a))
   (make-agent
    'kitchen
    (list (pseq:make-random-point-inside limit-polygon))
    '()
    (lambda (world a) a))
   (make-agent
    'living
    (list (pseq:make-random-point-inside limit-polygon))
    '()
    (lambda (world a) a))
   (make-agent
    'room1
    (list (pseq:make-random-point-inside limit-polygon))
    '()
    (lambda (world a) a))
   (make-agent
    'room2
    (list (pseq:make-random-point-inside limit-polygon))
    '()
    (lambda (world a) a))
   (make-agent
    'room3
    (list (pseq:make-random-point-inside limit-polygon))
    '()
    (lambda (world a) a))))

(define (agents-evolutionary-distribution graph world)
  (let ((limit-polygon (graph:limits graph)))
    (let evolve ((agents (generate-agents limit-polygon)))
      (visualization:forget-all)
      (visualize-graph graph)
      (visualize-world (make-world agents '()) graph)
      (visualization:do-now)
      
      (let ((new-agents (generate-agents limit-polygon)))
        (evolve (if (< (score agents graph) (score new-agents graph))
                    new-agents
                    agents)))) ; TODO REMEMBER LAST SCORE!

    (values
     graph
     (make-world 
      agents
      '()))))
