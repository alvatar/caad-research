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
          (light-sources (all-wall-element-points-all-walls->point-list 'window graph)))
      (map ; produces a field per light-source
        (lambda (source)
            (cond
             ((vect2? source) ; For point-lights
              (make-2d-u8field
                size-x
                size-y
                18.0
                26.0
                (lambda (v) (if (point-in-polygon? limit-polygon (vect2->point v))
                                (let ((d (fx* 2 (fx-distance-point-point (vect2->point v) source))))
                                  (if (> d 255) 255 d))
                              0))))
             ((= (length source) 2) ; For segments
              (make-2d-u8field
                size-x
                size-y
                18.0
                26.0
                (lambda (v) (if (point-in-polygon? limit-polygon (vect2->point v))
                                (let ((d (* 20.0 (distance-point-segment (vect2->point v) source))))
                                  (if (> d 255) 255 (inexact->exact (round d))))
                              0))))
             ((>= (length source) 3) ; For polylines (unimplemented)
              (make-2d-u8field
                size-x
                size-y
                18.0
                26.0
                (lambda (v) 0.7)))))
      light-sources))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))
