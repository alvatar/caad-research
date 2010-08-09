;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Selection
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import core/syntax
        core/debugging
        core/logging
        math/exact-algebra
        graph
        graph-operations
        graph-visualization
        visualization)

(%activate-logging)

(define-structure evaluated-graph graph score)

;;; Produce a selector function

(define (selector type)
  (case type
    ((keep-best)
     (lambda (pool new-graph)
       (let ((new-score (total-score new-graph)))
         (iff (> new-score 0) ; TODO: select only if better than all in the pool
              (begin
                (display "Solution selected!\n")
                (visualization:forget-all)
                (visualize-graph new-graph)
                (visualization:do-loop)
                (make-evaluated-graph new-graph
                                      (exact->inexact
                                       new-score)))))))
    (else
     (error "selector type not implemented"))))

;;; Evaluation for selection

(define (total-score graph)
  #;(if (correct-room-aspect-ratios? graph)
      (score-room-sizes graph)
      0)
  1)

;;; Score room sizes

(define (score-room-sizes graph)
  (let ((graph-area (graph:total-area graph)))
    (let/cc unacceptable-room
            (let ((expected-area (/ graph-area 8)))
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
                            (%log "Incorrect room sizes!" (unacceptable-room 0))
                            (abs (- expected-area room-area)))))
                    (graph:find.rooms graph)))))))

;;; Score room proportions

(define (correct-room-aspect-ratios? graph)
  (not
   (find (lambda (r)
           (let ((room-aspect-ratio (graph:room-aspect-ratio graph r)))
             (case (string->symbol (room-uid r))
               ((room) (> room-aspect-ratio 3.5))
               ((kitchen) (> room-aspect-ratio 3.2))
               ((living) (> room-aspect-ratio 3.0))
               (else #f))))
         (graph:find.rooms graph))))

;;; Score accesses

(define (score-room-accesses graph)
  +inf.0)