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
(import ../graph-operations)
(import ../visualization)
(import ../graph-visualization)
(import element-interrelations)

(export agents-evolutionary-distribution)


(define (score-agent-interrelationships a agents)
  1.0)

(define (score-agent-orientations a)
  0.0)

(define (score-agent-illumination a g)
  (ps
   (map (lambda (w) (distance.agent<->window a w)) (graph:find.windows g))))

(define (score-agent-distances-to-elements a)
  1.0)

(define (score-agent-required-geometrical a)
  1.0)

(define (score agents graph)
  (p (fold
      (lambda (a sc)
        (+ sc
           (case (agent-label a)
             ((distribution)
              (+ (* 1.0 (score-agent-interrelationships a agents))
                 (* 1.0 (score-agent-orientations a))
                 (* 1.0 (score-agent-distances-to-elements a))
                 (* 1.0 (score-agent-illumination a graph))
                 (* 1.0 (score-agent-required-geometrical a))))
             ((kitchen)
              (+ (* 1.0 (score-agent-interrelationships a agents))
                 (* 1.0 (score-agent-orientations a))
                 (* 1.0 (score-agent-distances-to-elements a))
                 (* 1.0 (score-agent-illumination a graph))
                 (* 1.0 (score-agent-required-geometrical a))))
             ((living)
              (+ (* 1.0 (score-agent-interrelationships a agents))
                 (* 1.0 (score-agent-orientations a))
                 (* 1.0 (score-agent-distances-to-elements a))
                 (* 1.0 (score-agent-illumination a graph))
                 (* 1.0 (score-agent-required-geometrical a))))
             ((room1)
              (+ (* 1.0 (score-agent-interrelationships a agents))
                 (* 1.0 (score-agent-orientations a))
                 (* 1.0 (score-agent-distances-to-elements a))
                 (* 1.0 (score-agent-illumination a graph))
                 (* 1.0 (score-agent-required-geometrical a))))
             ((room2)
              (+ (* 1.0 (score-agent-interrelationships a agents))
                 (* 1.0 (score-agent-orientations a))
                 (* 1.0 (score-agent-distances-to-elements a))
                 (* 1.0 (score-agent-illumination a graph))
                 (* 1.0 (score-agent-required-geometrical a))))
             ((room3)
              (+ (* 1.0 (score-agent-interrelationships a agents))
                 (* 1.0 (score-agent-orientations a))
                 (* 1.0 (score-agent-distances-to-elements a))
                 (* 1.0 (score-agent-illumination a graph))
                 (* 1.0 (score-agent-required-geometrical a))))
             (else (error "Agent type doesn't exist")))))
      0.0
      agents)))

(define (agents-evolutionary-distribution graph world)
  (let ((limit-polygon (graph:limits graph)))
    (let evolve ((agents '()))
      (visualization:forget-all)
      (visualize-graph graph)
      (visualize-world (make-world agents '()) graph)
      (visualization:do-now)
      
      (let ((new-agents
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
               (lambda (world a) a)))))
        (evolve (if (> (score agents graph) (score new-agents graph)) agents new-agents))))

    (values
     graph
     (make-world 
      agents
      '()))))
