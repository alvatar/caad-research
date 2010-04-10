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
  (merge-2d-u8fields
    (map
      (lambda (structural)
        (make-2d-u8field-with-resolution
          4
          size-x
          size-y
          mapped-x
          mapped-y
          (lambda (v) 30)))
      (graph-structurals graph))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))
