;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedure testing (prototype)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import srfi/64)
(import geometry/kernel)

#|
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
|#

(test-begin "arrays")

(test-assert (number? 1))

(test-equal "shape" #t #t)

(test-equal "False false" #t #f)

(test-equal #t #f)

(test-equal "make-array" 'a (car '(a b)))

;(test-error #t (vector-ref '#(1 2) 9))

(test-end "arrays")
