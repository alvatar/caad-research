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
(import ../operations-low)
(import ../visualization)
(import ../graph-visualization)

;-------------------------------------------------------------------------------
; Algorithm steps
;-------------------------------------------------------------------------------

(define (agents-evolutionary-distribution graph world)

  (define (score agents)
    (fold
     (lambda (a sc)
       (+ sc
          (case (agent-label a)
            ((distribution)
             5.0)
            ((kitchen)
             4.0)
            ((living)
             3.0)
            ((room1)
             2.0)
            ((room2)
             2.0)
            ((room3)
             2.0)
            (else (error "Agent type doesn't exist")))))
     0.0
     agents))
  
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
        (evolve (if (> (score agents) (score new-agents)) agents new-agents))))

    (values
     graph
     (make-world 
      agents
      '()))))
