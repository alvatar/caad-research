;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: evolutionary algorithm for agents distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std srfi/26))
(import (std srfi/69))

(import ../core/syntax)
(import ../core/functional)
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

;;; Helper function for debugging scores

(define debug-score
  (let ((maxs-table (make-hash-table)))
    (lambda (type s)
      (let ((max-score (hash-table-ref maxs-table type (lambda () -inf.0))))
       (if (> s max-score)
           (hash-table-set! maxs-table type s))
       (display "\n************ ")
       (display type)
       (display " ************\n")
       (display "score: ")
       (display s)
       (display "\nmax-score: ")
       (display max-score)
       s))))


(define (score-agents-interrelationships agents)
  ;; 1) Build a list of proximity groups (lists) '((room living) (bath) (kitchen))
  ;; 2) Use min-hamming-distance to analyze differences in each group
  ;; 3) Score according to differences in group distributions
  0.0)

(define (score-agent-orientations a g)
  0.0)

(define (score-agent-illumination agents g)
  (let ((windows (graph:find.windows g)))
    (pick-min
     (map
      (lambda (a)
        (case (agent-label a)
          ((distribution)
           0.0)
          ((kitchen)
           0.0)
          ((living)
           (* -1.0 (pick-min (map (lambda (w) (max 4.0 (distance.agent<->window a w))) windows))))
          ((room)
           (* -1.0 (pick-min (map (lambda (w) (max 4.0 (distance.agent<->window a w))) windows))))))
      agents))))

(define (score-agent-distances-to-elements agents g)
  (mean
   (map (lambda (a)
          (case (agent-label a)
            ((distribution)
             (* -2.0
                (max ; Only the biggest distance from the necessary elements is important
                 (pick-min (map (lambda (e) (distance.agent<->entry a e)) (graph:find.entries g)))
                 (pick-min (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g))))))
            ((kitchen)
             (* -2.0
                (pick-min
                 (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g)))))
            ((living)
             0.0)
            ((room)
             0.0)
            (else (error "Agent type doesn't exist"))))
        agents)))

;; (define (score-agent-required-geometrical agents g)
;;   (mean
;;    (list
;;     (* -1.0
;;        (mean
;;         (map (lambda (a1)
;;                (pick-max (map (lambda (a2) (inverse (distance.agent<->agent a1 a2)))
;;                              (delete a1 agents))))
;;              agents)))
;;     (* -1.0
;;        (mean
;;         (map (lambda (w)
;;                (pick-max (map (lambda (a) (inverse (distance.agent<->wall a w))) agents)))
;;              (graph:find.walls g)))))))

(define (score agents graph)
  (+  (score-agents-interrelationships agents)
      (debug-score "illumination" (score-agent-illumination agents graph))
      (debug-score "distances to elements" (score-agent-distances-to-elements agents graph))
                                        ;(score-agent-required-geometrical agents graph)
      ))

(define (generate-agents limit-polygon) ; TODO: 1) IMPORTANTE: Generar con restricciones!!
  (let ((make-agent-type
         (cut make-agent <> (list (generate.random-point-inside limit-polygon)) '() '())))
   (list ; TODO: This list is generated from an input argument
    (make-agent-type 'distribution)
    (make-agent-type 'kitchen)
    (make-agent-type 'living)
    (make-agent-type 'room)
    (make-agent-type 'room)
    (make-agent-type 'room))))

(define (regenerate-agents agents limit-polygon)
  (map
   (lambda (a p) (make-agent
             (agent-label a)
             (list p)
             '()
             '()))
   agents
   (generate.random-points/separation&boundaries (length agents) limit-polygon 4.0 2.0)))

(define (agents-evolutionary-distribution graph world)
  (let ((limit-polygon (graph:limits graph)))
    (let evolve ((agents (generate-agents limit-polygon)))
      (visualization:forget-all)
      (visualize-graph graph)
      (visualize-world (make-world agents '()) graph)
      (visualization:do-now)
      (let ((new-agents (regenerate-agents agents limit-polygon)))
        (evolve (if (< (score agents graph) (score new-agents graph))
                    new-agents
                    agents)))) ; TODO REMEMBR LAST SCORE!

    (values
     graph
     (make-world 
      agents
      '()))))
