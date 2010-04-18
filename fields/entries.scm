;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Entry-point field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../geometry)
(import ../graph)
(import ../math)
(import ../fields-2d)
(import ../utils/misc)

(define (make-entries-field graph size-x size-y mapped-x mapped-y limit-polygon)
  (merge-u8-2dfields
    (map
      (lambda (entry)
        (let ((entry-seg (entry->point-list graph entry)))
          (produce-u8-2dfield-with-resolution
            4
            size-x
            size-y
            mapped-x
            mapped-y
            (lambda (v) (if (point-in-polygon? limit-polygon v)
                          (let ((d (fl* 20.0 (fl-distance-point-segment v entry-seg))))
                            (if (fl> d 255.0) 255 (##flonum.->fixnum d)))
                          0)))))
      (graph-entries graph))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))
