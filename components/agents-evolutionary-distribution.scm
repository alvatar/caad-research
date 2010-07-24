;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: evolutionary algorithm for agents distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std srfi/26))

(import ../core/functional)
(import ../core/list)
(import ../generation-elements)
(import ../geometry/generation)
(import ../geometry/kernel)
(import ../graph)
(import ../graph-operations)
(import ../graph-visualization)
(import ../visualization)
(import auxiliary-evolution-evaluation)

(export agents-evolutionary-distribution)


(define (score agents graph limits)
  (+  ;(score-agents-interrelationships agents)
      ;(debug-score "illumination" (score-agent-illumination agents graph))
      ;(debug-score "distances to elements" (score-agent-distances-to-elements agents graph))
      ;(debug-score "orientation" (score-agent-orientations agents graph limits))
   (score-agent-illumination agents graph)
   (score-agent-distances-to-elements agents graph)
   (score-agent-orientations agents graph limits)))

(define (generate-agents limit-polygon)
  (let ((make-agent-type
         (cut make-agent <> (list (generate.random-point-inside limit-polygon)) '() '())))
   (list ; TODO: This list is generated from an input argument
    (make-agent-type 'distribution)
    (make-agent-type 'kitchen)
    (make-agent-type 'living)
    (make-agent-type 'room)
    (make-agent-type 'room)
    (make-agent-type 'room))))

(define agents-regenerator
  (lambda (limit-polygon)
   (let ((slots (generate.point-mesh-centered (pseq->bbox limit-polygon) 2.0 5.0 5.0)))
     (lambda (agents)
       (map
        (lambda (a p) (make-agent
                  (agent-label a)
                  (list p)
                  '()
                  '()))
        agents
        (pick-random//repetition slots (length agents)))))))

;;; Evolutionary algorithm

(define (agents-evolutionary-distribution graph world)
  (let ((limit-polygon (graph:limits graph)))
    (let evolve ((old-agents (generate-agents limit-polygon))
                 (old-score 0.0))
      (visualization:forget-all)
      (visualize-graph graph)
      (visualize-world (make-world old-agents '()) graph)
      (visualization:do-now)
      (let ((new-agents ((agents-regenerator limit-polygon) old-agents))) ; TODO: we are regenerating
        (if (< old-score (score new-agents
                                graph
                                limit-polygon))
            (evolve new-agents (score new-agents
                                      graph
                                      limit-polygon))
            (evolve old-agents old-score))))

    (values
     graph
     (make-world agents '()))))
