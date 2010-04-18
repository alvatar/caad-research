;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Pipes field
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../analysis)
(import ../geometry)
(import ../graph)
(import ../math)
(import ../fields-2d)
(import ../utils/misc)

(define (make-pipes-field graph size-x size-y mapped-x mapped-y limit-polygon)
  (merge-u8-2dfields
    (map ; produces a field per light-source
      (lambda (pipe)
        (let ((pipe-pos (pipe-position pipe)))
          (produce-u8-2dfield-with-resolution
            4
            size-x
            size-y
            mapped-x
            mapped-y
            (lambda (v) (if (point-in-polygon? limit-polygon v)
                          (let ((d (fl* 20.0 (fl-distance-point-point v pipe-pos))))
                            (if (fl> d 255.0) 255 (##flonum.->fixnum d)))
                          0)))))
      (graph-pipes graph))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))
