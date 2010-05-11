;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Light field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../math/algebra)
(import ../geometry/kernel)

(import ../analysis)
(import ../graph)
(import ../fields-2d)
(import ../utils/misc)

(define (make-light-field graph size-x size-y mapped-x mapped-y limit-polygon)
  (merge-u8-2dfields
    (map ; produces a field per light-source
      (lambda (source)
        (cond
          ((vect2? source) ; For point-lights
           (error "unimplemented"))
          ((= (length source) 2) ; For segments
           (produce-u8-2dfield-with-resolution
             4
             size-x
             size-y
             mapped-x
             mapped-y
             (lambda (v) (if (point-in-polygon? limit-polygon v)
                           (let ((d (fl* 20.0 (fl-distance-point-segment v source))))
                             (if (fl> d 255.0) 0 (fx- 255 (##flonum.->fixnum d))))
                           255))))
          ((>= (length source) 3) ; For polylines
           (error "unimplemented"))))
      (all-wall-element-points-all-walls->point-list 'window graph))
    (lambda (a b)
      (let ((sum (fx+ a b)))
        (if (fx> sum 255) 255 sum)))))
