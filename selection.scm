;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import dev/debugging)
(import math/exact-algebra)

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
            (score-room-accesses graph))))

;;; Score room sizes

(define (score-room-sizes graph)
  0)

;;; Score room accesses

(define (score-room-accesses graph)
  (random-real))
