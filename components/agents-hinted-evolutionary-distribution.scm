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
(import ../core/randomization)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/generation)
(import ../geometry/kernel)
(import ../graph)
(import ../graph-operations)
(import ../graph-visualization)
(import ../math/exact-algebra)
(import ../visualization)
(import evolution-evaluation)
(import element-interrelations)

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

(define (avrg-dist-to-elements graph p)
  ;;(pick-min (map (lambda (e) (~distance.point-point p (pipe-position e))) (graph:find.pipes graph)))
  (pick-min (map (lambda (e) (~distance.point-pseq p (entry-pseq e))) (graph:find.entries graph))))

(define agents-regenerator
  (lambda (limit-polygon)
    (let ((slots (generate.point-mesh-centered (pseq->bbox limit-polygon) 2.0 5.0 5.0))
          (make-agent-simple (cut make-agent <> <> '() '())))
      
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
                                               (graph:find.pipes graph)))
                                         8.0)) ; closer than 8 m.
                                    foldedslots)
                       (values (make-agent-simple
                                (agent-label a)
                                (list p))
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

(define (agents-hinted-evolutionary-distribution graph world)
  (let ((limit-polygon (graph:limits graph)))
    
    (define (agents-step)
      (let ((regenerate-agents (agents-regenerator limit-polygon)))
        (let evolve ((old-agents (agent-seeds limit-polygon))
                     (old-score 0.0)
                     (num-iterations 0))
          (visualization:forget-all)
          (visualize-graph graph)
          (visualize-world (make-world old-agents '()) graph)
          (visualization:do-now)
          (let ((new-agents (regenerate-agents graph old-agents))) ; TODO: we are regenerating and it is calculated even if not used
            (cond
             ((> num-iterations 10)
              (walls-step old-agents))
             ((< old-score (score new-agents
                                  graph
                                  limit-polygon))
              (evolve new-agents
                      (score new-agents
                             graph
                             limit-polygon)
                      (add1 num-iterations)))
             (else
              (evolve old-agents
                      old-score
                      (add1 num-iterations))))))))
    (define (walls-step finished-agents)
      (define (step1 graph world) (pp 'step1) (values graph world))
      (define (step2 graph world) (pp 'step2) (values graph world))
      (define (step3 graph world) (pp 'step3) (values graph world))
      ((compose-right
        step1
        step2
        step3)
       graph
       (make-world finished-agents '())))
    
    (agents-step)))
