;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import core/syntax)
(import dev/debugging)
(import math/exact-algebra)
(import graph)
(import graph-operations)
(import graph-visualization)
(import visualization)

;;; Produce a selector function

(define (selector type)
  (case type
    ((keep-best)
     (lambda (new pool)
       (if (or (null? pool)
               (> (piv "total-score" (total-score new))
                  0 #;(total-score (car pool))))
           (begin
             (visualization:forget-all)
             (visualize-graph new)
             (visualization:do-now)
             (for-each (lambda (r)
                         (display (room-uid r))
                         (display ": ")
                         (display (exact->inexact (graph:room-area new r)))
                         (newline))
                       (graph:find.rooms new))
             (step)
             (list new))
           pool)))
    (else
     (error "selector type not implemented"))))

;;; Evaluation for selection

(define (total-score graph)
  (score-room-sizes graph))

;;; Score room sizes

(define (score-room-sizes graph)
  (let ((graph-area (graph:total-area graph)))
    (define (room-expected) (/ graph-area 8)) ; TODO: 8 is 6 plus a bit more
    (let/cc garbage
            (let ((expected-area (room-expected))) ; TODO!!!!
              (mean
               (map (lambda (r)
                      (let ((room-area (graph:room-area graph r)))
                        (if (< room-area
                               (case (string->symbol (room-uid r))
                                 ((room)
                                  (/ graph-area 14)) ; Room minimum area related to total
                                 ((kitchen)
                                  (/ graph-area 18)) ; Kitchen minimum area related to total
                                 ((living)
                                  (/ graph-area 10)) ; Living-room minimum area related to total
                                 (else 0)))
                            (begin (piv "WRONG ROOM AREA" room-area) (garbage 0))
                            (abs (- expected-area room-area)))))
                    (graph:find.rooms graph)))))))

;;; Score room proportions

(define (score-room-proportions graph)
  +inf.0)
