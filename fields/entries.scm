;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry-point field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../geometry)
(import ../graph)
(import ../math)
(import ../utils/misc)

(define (make-entries-field graph size-x size-y mapped-x mapped-y limit-polygon)
  (merge-2d-u8fields
    (map
      (lambda (entry)
           (make-2d-u8field-with-resolution
             4
             size-x
             size-y
             mapped-x
             mapped-y
             (lambda (v) 30)
             #;(lambda (v) (if (point-in-polygon? limit-polygon (vect2->point v))
                           (let ((d (fl* 20.0 (fl-distance-point-segment (vect2->point v) entry))))
                             (if (fl> d 255.0) 255 (##flonum.->fixnum d)))
                           0))))
      (graph-entries graph))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))
