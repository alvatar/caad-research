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

(define (make-light-field graph size-x size-y)
  (merge-2d-u8fields
    (let ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
          (light-sources (map*
                           inexact-point->exact-point
                           (all-wall-element-points-all-walls->point-list 'window graph))))
      (map ; produces a field per light-source
        (lambda (source)
            (cond
             ((vect2? source)
              (make-2d-scaled-u8field
                4
                size-x
                size-y
                (lambda (v) (if (point-in-polygon? limit-polygon (vect2->point v))
                                (let ((d (fx* 2 (fx-distance-point-point (vect2->point v) source))))
                                  (if (> d 255) 255 d))
                              0))))
             ((= (length source) 2)
              (make-2d-scaled-u8field
                4
                size-x
                size-y
                (lambda (v) (if (point-in-polygon? limit-polygon (vect2->point v))
                                (let ((d (fx* 2 (fx-distance-point-segment (vect2->point v) source))))
                                  (if (> d 255) 255 d))
                              0))))
             ((>= (length source) 3)
              (make-2d-scaled-u8field
                4
                size-x
                size-y
                (lambda (v) 0.7)))))
      light-sources))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))
