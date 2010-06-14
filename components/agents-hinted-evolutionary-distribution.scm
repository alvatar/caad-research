;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: evolutionary algorithm for agents distribution with some
;;; some positions predefined or hinted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std srfi/11))
(import (std srfi/26))

(import ../core/functional)
(import ../core/list)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/generation)
(import ../geometry/kernel)
(import ../graph)
(import ../graph-operations)
(import ../graph-visualization)
(import ../visualization)
(import evolution-evaluation)

(export agents-hinted-evolutionary-distribution)


(define (score agents graph limits)
  (+ (score-agent-illumination agents graph)
     (score-agent-orientations agents graph limits)))

(define (agent-seeds limit-polygon)
  (let ((make-agent-type
         (cut make-agent <> (list (generate.random-point-inside limit-polygon)) '() '())))
   (list ; TODO: This list is generated from an input argument
    (make-agent-type 'distribution)
    (make-agent-type 'kitchen)
    (make-agent-type 'living)
    (make-agent-type 'room)
    (make-agent-type 'room)
    (make-agent-type 'room))))

;; (define (hint-agents type/hint slots agents)
;;   (let-values (((hinted-agents free-agents)
;;                 (partition (lambda (a)
;;                              (any (lambda (ha) (equal? ha a))
;;                                   (map car type/hint)))
;;                            agents)))
;;     (ps hinted-agents)
;;     (ps free-agents)
;;     (append
;;      (map-cond ())
;;      (map
;;       (lambda (a p) (make-agent
;;                 (agent-label a)
;;                 (list p)
;;                 '()
;;                 '()))
;;       free-agents
;;       (pick-random//repetition slots (length free-agents))))))

(define agents-regenerator
  (lambda (limit-polygon)
   (let ((slots (generate.point-mesh-centered (pseq->bbox limit-polygon) 2.0 5.0 5.0))
         (make-agent-simple (cut make-agent <> <> '() '())))
     (lambda (agents)
       (map-fold
        (lambda (a fslots)
          (case (agent-label a)
            ((distribution)
             (receive (p rslots)
                      (pick-random+rember fslots)
                      (values (make-agent-simple
                               (agent-label a)
                               (list p))
                              rslots)))
            ((kitchen)
             (receive (p rslots)
                      (pick-random+rember fslots)
                      (values (make-agent-simple
                               (agent-label a)
                               (list p))
                              rslots)))
            (else
             (receive (p rslots)
                      (pick-random+rember fslots)
                      (values (make-agent-simple
                               (agent-label a)
                               (list p))
                              rslots)))))
        slots
        agents)))))

;;; Evolutionary algorithm

(define (agents-hinted-evolutionary-distribution graph world)
  (let* ((limit-polygon (graph:limits graph))
         (regenerate-agents (agents-regenerator limit-polygon)))
    (let evolve ((old-agents (agent-seeds limit-polygon))
                 (old-score 0.0))
      (visualization:forget-all)
      (visualize-graph graph)
      (visualize-world (make-world old-agents '()) graph)
      (visualization:do-now)
      (let ((new-agents (regenerate-agents old-agents))) ; TODO: we are regenerating
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
