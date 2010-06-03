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


(define (score-agent-interrelationships a agents)
  0.0)

(define (score-agent-orientations a g)
  0.0)

(define (score-agent-illumination a g)
  (- (sum
      (map (lambda (w) (distance.agent<->window a w)) (graph:find.windows g))))
  0.0)

(define (score-agent-distances-to-elements a g)
  (sum
   (case (agent-label a)
     ((distribution)
      (map (lambda (e) (- (distance.agent<->entry a e))) (graph:find.entries g)))
     ((kitchen)
      (map (lambda (e) (- (distance.agent<->entry a e))) (graph:find.entries g)))
     ((living)
      '(1.0))
     ((room1)
      '(1.0))
     ((room2)
      '(1.0))
     ((room3)
      '(1.0))
     (else (error "Agent type doesn't exist")))))

(define (score-agent-required-geometrical a g)
  (sum
   (map (lambda (e) (distance.agent<->wall a e)) (graph:find.walls g)))
  0.0)

(define (score agents graph)
  (p (fold
      (lambda (a sc)
        (+ sc
           (+ (score-agent-interrelationships a agents)
              (score-agent-orientations a graph)
              (score-agent-distances-to-elements a graph)
              (score-agent-illumination a graph)
              (score-agent-required-geometrical a graph))))
      0.0
      agents)))

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
                    agents))))

    (values
     graph
     (make-world 
      agents
      '()))))
