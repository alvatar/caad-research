;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Structure field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../geometry/kernel)
(import ../math/algebra)

(import ../analysis)
(import ../graph)
(import ../fields-2d)
(import ../utils/misc)

(define (make-structure-field graph size-x size-y mapped-x mapped-y limit-polygon)
  (merge-u8-2dfields
    (map
      (lambda (structural)
        (let ((str-points (point-list-close (structural->point-list structural graph)))
              (fadeout-factor (fl/ 255.0 2.0))) ; 2 meters
          (produce-u8-2dfield-with-resolution
            4
            size-x
            size-y
            mapped-x
            mapped-y
            (lambda (v)
              (if (point-in-polygon? limit-polygon v)
                  (let ((d (fl* fadeout-factor
                                (fl-distance-point-point-list v str-points))))
                    (if (fl> d 255.0) 0 (fx- 255 (##flonum.->fixnum d))))
                255)))))
      (graph-structurals graph))
    (lambda (a b)
      (let ((sum (fx+ a b)))
        (if (fx> sum 255) 255 sum)))))
