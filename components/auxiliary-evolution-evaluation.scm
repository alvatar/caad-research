;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evaluation system for the evolutionary algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        (std srfi/26)
        (std srfi/69)
        (std srfi/95)
        ../core/syntax
        ../core/functional
        ../core/debugging
        ../geometry/kernel
        ../graph
        ../graph-operations
        ../math/exact-algebra
        ../math/inexact-algebra
        auxiliary-graph-elements
        generation-elements)


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
  (let* ((bb (pseq:bbox limits))
         (bblt (bbox-lefttop bb))
         (bbrt (bbox-righttop bb))
         (bblb (bbox-leftbottom bb))
         (bbrb (bbox-rightbottom bb))
         (east-angle
          (-              ; invert to use it for aligning bounding box
           (direction:angle-rad (graph:north->east (car (graph-environment g))))))
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
             `((north ,(~distance.agent<->point a north))
               (south ,(~distance.agent<->point a south))
               (east ,(~distance.agent<->point a east))
               (west ,(~distance.agent<->point a west)))
             (lambda (a b) (< (cadr a) (cadr b))))))

    ;; (visualization:forget-all)
    ;; (visualize-graph g)
    ;; (visualize-world (make-world agents '()) g)
    ;; (visualization:point-list-now 0.5 (list north east south west))
    (sum
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
    (sum
     (delete
      'nada
      (map
       (lambda (a)
         (case (agent-label a)
           ((distribution) 'nada)
           ((kitchen) 'nada)
           ((living)
            (clamp&invert&normalize ; IDEA: find mathematical solution to control distribution of power 1 vs. x
             (pick-min (map (lambda (w) (~distance.agent<->window a w)) windows))
             2.0
             12.0))                      ; TODO: 2.0 is wall separation
           ((room)
            (clamp&invert&normalize
             (pick-min (map (lambda (w) (~distance.agent<->window a w)) windows))
             2.0
             12.0))
           (else (error "Agent type doesn't exist"))))
       agents)))))
;; IDEA: optimal positions through forces

(define (score-agent-distances-to-elements agents g)
  (sum
   (delete
    'nada
    (map
     (lambda (a)
       (case (agent-label a)
         ((distribution)
          (clamp&invert&normalize
           (max ; Only the biggest distance from the necessary elements is important
            (pick-min (map (lambda (e) (~distance.agent<->entry a e)) (graph:find.entries g)))
            ;(pick-min (map (lambda (e) (distance.agent<->pipe a e)) (graph:find.pipes g)))
            )
           8.0 ; TODO: the best point that satisfies this (calculated with forces??)
           20.0)) ; TODO: This number should be analyzed from graph (the max possible distance)
         ((kitchen)
          (clamp&invert&normalize
           (pick-min
            (map (lambda (e) (~distance.agent<->pipe a e)) (graph:find.pipes g)))
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
