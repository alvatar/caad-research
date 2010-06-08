;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: evolutionary algorithm for agents distribution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std srfi/26))
(import (std srfi/69))
(import (std srfi/95))

(import ../core/syntax)
(import ../core/functional)
(import ../core/list)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../graph)
(import ../graph-visualization)
(import ../graph-operations)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)
(import ../visualization)
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
       (display "\n")
       s))))

(define (score-agents-interrelationships agents)
  ;; 1) Build a list of proximity groups (lists) '((room living) (bath) (kitchen))
  ;; 2) Use min-hamming-distance to analyze differences in each group
  ;; 3) Score according to differences in group distributions
  0.0)

(define (score-agent-orientations agents g limits)
  (let* ((bb (pseq->bbox limits))
         (bblt (bbox-lefttop bb))
         (bbrt (bbox-righttop bb))
         (bblb (bbox-leftbottom bb))
         (bbrb (bbox-rightbottom bb))
         (east-angle
          (-              ; invert to use it for aligning bounding box
           (direction->angle-rad (graph:north->east (car (graph-environment g))))))
         (rotfunc (cute rotate.point-w/reference
                        (bbox:centroid bb)
                        <>
                        east-angle))
         (north (rotfunc (segment:mid-point (make-segment bblt bbrt))))
         (south (rotfunc (segment:mid-point (make-segment bblb bbrb))))
         (east (rotfunc (segment:mid-point (make-segment bbrt bbrb))))
         (west (rotfunc (segment:mid-point (make-segment bblt bblb)))))
    (define (agent-orientation a)
      (caar (sort
             `((north ,(distance.agent<->point a north))
               (south ,(distance.agent<->point a south))
               (east ,(distance.agent<->point a east))
               (west ,(distance.agent<->point a west)))
             (lambda (a b) (< (cadr a) (cadr b))))))

    ;; (visualization:forget-all)
    ;; (visualize-graph g)
    ;; (visualize-world (make-world agents '()) g)
    ;; (visualization:point-list-now 0.5 (list north east south west))
    (mean
     (delete
      'nada
      (map (lambda (a)
             (case (agent-label a)
               ((distribution) 'nada)
               ((kitchen)
                (if (equal? 'south (agent-orientation a)) 1.0 0.0))
               ((living)
                (if (equal? 'south (agent-orientation a)) 1.0 0.0))
               ((room)
                (if (equal? 'north (agent-orientation a)) 1.0 0.0))))
           agents)))))

(define (score-agent-illumination agents g) ; TODO: this should think about obstruction, not distances
  (let ((windows (graph:find.windows g)))
    (mean
     (delete
      'nada
      (map
       (lambda (a)
         (case (agent-label a)
           ((distribution) 'nada)
           ((kitchen) 'nada)
           ((living)
            (clamp&invert&normalize ; IDEA: find mathematical solution to control distribution of power 1 vs. x
             (pick-min (map (lambda (w) (distance.agent<->window a w)) windows))
             2.0
             12.0))                      ; TODO: 2.0 is wall separation
           ((room)
            (clamp&invert&normalize
             (pick-min (map (lambda (w) (distance.agent<->window a w)) windows))
             2.0
             12.0))
           (else (error "Agent type doesn't exist"))))
       agents)))))
;; IDEA: optimal positions through forces

(define (score-agent-distances-to-elements agents g)
  (mean
   (delete
    'nada
    (map
     (lambda (a)
       (case (agent-label a)
         ((distribution)
          (clamp&invert&normalize
           (max ; Only the biggest distance from the necessary elements is important
            (pick-min (map (lambda (e) (distance.agent<->entry a e)) (graph:find.entries g)))
            ;(pick-min (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g)))
            )
           8.0 ; TODO: the best point that satisfies this (calculated with forces??)
           20.0)) ; TODO: This number should be analyzed from graph (the max possible distance)
         ((kitchen)
          (clamp&invert&normalize
           (pick-min
            (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g)))
           0.0
           20.0))
         ((living) 'nada)
         ((room) 'nada)
         (else (error "Agent type doesn't exist"))))
     agents))))

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
        (list-tabulate (length agents) (lambda (x) (pick-random slots)))
        )))))

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
