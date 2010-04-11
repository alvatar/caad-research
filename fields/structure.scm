;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../geometry)
(import ../graph)
(import ../math)
(import ../utils/misc)

(define (make-structure-field graph size-x size-y mapped-x mapped-y)
  (let ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph))))
    (merge-2d-u8fields
      (map
        (lambda (structural)
          (let ((str-points (point-list-close (structural->point-list structural graph)))
                (fadeout-factor (fl/ 255.0 2.0))) ; 2 meters
            (pp (structural->point-list structural graph))
            (make-2d-u8field-with-resolution
              4
              size-x
              size-y
              mapped-x
              mapped-y
              (lambda (v)
                (if (point-in-polygon? limit-polygon (vect2->point v))
                    (let ((d (fl* fadeout-factor
                                  (fl-distance-point-point-list (vect2->point v) str-points))))
                      (if (fl> d 255.0) 255 (##flonum.->fixnum d)))
                  0)))))
        (graph-structurals graph))
      (lambda (a b)
        (let ((sum (fx- (fx+ a b) 255)))
          (if (fx< sum 0) 0 sum))))))
