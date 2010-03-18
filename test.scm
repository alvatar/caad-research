;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedure testing (prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import geometry)

(define (check res)
  (if res
      (display "-- ok --\n")
    (raise "Test didn't pass")))
    
(check (list=~ (segment-polygon-intersection
                 (list (make-point 1.0 1.0) (make-point 2.0 2.0))
                 (list (make-point 1.0 2.0) (make-point 1.5 3.0) (make-point 2.0 1.0)))
               '((1.8 1.8) (1.5 1.5))
               0.000001))

(check (equal?
         (point-in-polygon?
           (list (make-point 0.0 0.0) (make-point 2.0 0.0) (make-point 2.0 2.0) (make-point 0.0 2.0)) ; Inner
           (make-point 1.0 1.0))
         #t))

(check (equal?
         (point-in-polygon?
           (list (make-point 0.0 0.0) (make-point 2.0 0.0) (make-point 2.0 2.0) (make-point 0.0 2.0)) ; Border
           (make-point 0.0 2.0))
         #t))

(check (equal?
         (point-in-polygon?
           (list (make-point 0.0 0.0) (make-point 2.0 0.0) (make-point 2.0 2.0) (make-point 0.0 2.0)) ; Outer
           (make-point 1.0 3.0))
         #f))
