;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import core/syntax)
(import dev/debugging)
(import math/exact-algebra)
(import graph-operations)
(import graph-visualization)
(import visualization)

;;; Produce a selector function

(define (selector type)
  (case type
    ((keep-best)
     (lambda (new pool)
       (if (or (null? pool)
               (> (total-score new)
                  (total-score (car pool))))
           (begin
             (if (not (null? pool))
                 (begin      ; temporary for displaying better results
                   (step)
                   (visualization:forget-all)
                   (visualize-graph (car pool))
                   (visualization:do-now)))
             (list new))
           pool)))
    (else
     (error "selector type not implemented"))))

;;; Evaluation for selection

(define (total-score graph)
  (pv (mean (score-room-sizes graph)
            (score-room-proportions graph))))

;;; Score room sizes

(define (score-room-sizes graph)
  (let ((graph-area (graph:total-area graph)))
    (define (room-minimum) (/ graph-area 12)) ; TODO: 12?
    (define (room-expected) (/ graph-area 8)) ; TODO: 8 is 6 plus a bit more
    (let/cc garbage
            (let ((minimum-area (room-minimum))
                  (expected-area (room-expected)))
              (ps (map
                   (lambda (r)
                     (let ((room-area (graph:room-area graph r)))
                       (if (< room-area minimum-area)
                           (garbage 0)
                           (abs (- expected-area room-area)))))
                   (graph:find.rooms graph)))))))

;;; Score room proportions

(define (score-room-proportions graph)
  (random-real))
