;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: evolutionary algorithm for agents distribution with some
;;; some positions predefined or hinted
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        (std srfi/11)
        (std srfi/26)
        ../core/list
        ../core/randomization
        ../core/debugging
        ../geometry/generation
        ../geometry/kernel
        ../graph
        ../graph-operations
        ../graph-visualization
        ../math/exact-algebra
        ../visualization
        agent
        auxiliary-evolution-evaluation
        auxiliary-graph-elements
        world)

(export agents-hinted-evolutionary-distribution)

(%activate-checks)


(define (score agents graph limits)
  (+ (score-agent-illumination agents graph)
     (score-agent-orientations agents graph limits)))

(define (agent-seeds limit-polygon)
  (let ((make-agent-type
         (cut make-agent <> (list (~generate.random-point-inside limit-polygon)) '() '())))
   (list ; TODO: This list is generated from an input argument
    (make-agent-type 'distribution)
    (make-agent-type 'kitchen)
    (make-agent-type 'living)
    (make-agent-type 'room)
    (make-agent-type 'room)
    (make-agent-type 'room))))

(define (avrg-dist-to-elements graph p)
  ;;(pick-min (map (lambda (e) (~distance.point-point p (pipe-position e))) (graph:filter.pipes graph)))
  (pick-min (map (lambda (e) (~distance.point-pseq p (entry-pseq e))) (graph:filter.entries graph))))

(define agents-regenerator
  (lambda (limit-polygon)
    (let ((slots (generate.point-mesh-centered (pseq:bbox limit-polygon)
                                               1.0
                                               2.0
                                               2.0
                                               (lambda (p) (vect2:inexact->exact
                                                       (make-point
                                                        (+ (point-x p) (* (random-exact*) 0.1))
                                                        (+ (point-y p) (* (random-exact*) 0.1)))))))
          (make-agent-simple (cute make-agent <> <> '() '())))
      (lambda (graph agents)
        (map-fold
         (lambda (a foldedslots)
           (case (agent-label a)
             ((distribution)
              (receive (p rslots)
                       (most+rember (lambda (s1 s2)
                                      (< (avrg-dist-to-elements graph s1)
                                         (avrg-dist-to-elements graph s2)))
                                    foldedslots)
                       (values (make-agent-simple
                                (agent-label a)
                                (list p))
                               rslots)))
             ((kitchen)
              (receive (p rslots)
                       (find+rember (lambda (s)
                                      (< (pick-min
                                          (map (lambda (pipe)
                                                 (~distance.point-point s (pipe-position pipe)))
                                               (graph:filter.pipes graph)))
                                         80.0)) ; is it closer than this?
                                    foldedslots)
                       (values (make-agent-simple
                                (agent-label a)
                                (list (%accept #t "agents-regenerator couldn't find a place for the kichen agent"
                                               p)))
                               rslots)))
             (else
              (receive (p rslots)
                       (pick-random+rember foldedslots)
                       (values (make-agent-simple
                                (agent-label a)
                                (list p))
                               rslots)))))
         (binary-shuffle-list slots)
         agents)))))

;;; Evolutionary algorithm

(define (agents-hinted-evolutionary-distribution graph world exit)
  (let* ((limit-polygon (graph:limits graph))
         (regenerate-agents (agents-regenerator limit-polygon)))
    (let evolve ((old-agents (agent-seeds limit-polygon))
                 (old-score 0.0)
                 (num-iterations 0))
      (let ((new-agents (regenerate-agents graph old-agents))) ; TODO: we are regenerating//it is calculated even if not used
        (cond
         ((> num-iterations 100)
          (values graph
                  (make-world
                   old-agents
                   '())
                  exit))
         ((< old-score (score new-agents
                              graph
                              limit-polygon))
          ;; <---- DEBUG VISUALIZATION
          (visualization:forget-all)
          (visualize-graph graph)
          (visualize-world (make-world old-agents '()) graph)
          (visualization:do-now)
          (evolve new-agents
                  (score new-agents
                         graph
                         limit-polygon)
                  (add1 num-iterations)))
         (else
          (evolve old-agents
                  old-score
                  (add1 num-iterations))))))))
