;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Light field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../geometry)
(import ../graph)
(import ../math)
(import ../utils/misc)

(define (make-light-field graph size-x size-y mapped-x mapped-y)
  (let ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
        (light-sources (all-wall-element-points-all-walls->point-list 'window graph)))
    (merge-2d-u8fields
      (map ; produces a field per light-source
        (lambda (source)
          (cond
            ((vect2? source) ; For point-lights
             (error "unimplemented"))
            ((= (length source) 2) ; For segments
             (make-2d-u8field-with-resolution
               4
               size-x
               size-y
               mapped-x
               mapped-y
               (lambda (v) (if (point-in-polygon? limit-polygon (vect2->point v))
                             (let ((d (fl* 20.0 (fl-distance-point-segment (vect2->point v) source))))
                               (if (fl> d 255.0) 255 (##flonum.->fixnum d)))
                             0))))
            ((>= (length source) 3) ; For polylines
             (error "unimplemented"))))
        light-sources)
      (lambda (a b)
        (let ((sum (fx- (fx+ a b) 255)))
          (if (fx< sum 0) 0 sum))))))
