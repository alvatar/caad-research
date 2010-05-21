;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algebraic kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
(compile-options force-compile: #t)

(import ../core/functional)
(import exact-algebra)

;-------------------------------------------------------------------------------
; Constants
;-------------------------------------------------------------------------------

(define pi 3.14159265)
(define pi2 6.28318531)
(define pi/2 1.57079633)
(define pi/-2 -1.57079633)
(define pi/4 0.785398163)
(define pi3/4 2.35619449)

(define equal-accuracy 0.000001)

;-------------------------------------------------------------------------------
; Arithmetic
;-------------------------------------------------------------------------------

;;; Is equal? (with precision) for inexacts

(define (=~e a b e)
  ;(< (abs (- a b)) e)) -> Faster in interpreted code
  (and (>= a (- b e))
       (<= a (+ b e))))

;;; Is equal? (for inexacts)

(define (=~ a b)
  (=~e a b equal-accuracy))

;;; Is equal to zero? (for inexacts)

(define (~zero? a)
  (=~e a 0.0 equal-accuracy))

;;; Inverse function

(define (~inverse x)
  (/ 1.0 x))

;;; Average between two values

(define (~average a b)
  (/ (+ a b) 2.0))

;-------------------------------------------------------------------------------
; Vector dimension 2
;-------------------------------------------------------------------------------

;;; Are these vectors equal?

(define (vect2:~=? v1 v2)
  (vect2:=?e v1 v2 equal-accuracy))

;;; Are these vectors equal? (with epsilon)

(define (vect2:~=?e v1 v2 e)
  (and (=~e (vect2-x v1)
            (vect2-x v2)
            e)
       (=~e (vect2-y v1)
            (vect2-y v2)
            e)))

;;; Random vect2

(define (vect2:~random)
  (make-vect2 (fl+ -1.0 (fl* (random-real) 2.0))
              (fl+ -1.0 (fl* (random-real) 2.0))))

;;; Zero vector

(define (vect2:~zero)
  (make-vect2 0.0 0.0))

;;; Square root function of a vector

(define (vect2:~sqrt vec)
  (make-vect2 (sqrt (vect2-x vec))
              (sqrt (vect2-y vec))))

;;; Calculate vector length

(define (vect2:~magnitude vec)
  (sqrt (+ (square (vect2-x vec))
           (square (vect2-y vec)))))

;;; Normalize vector

(define (vect2:~normalize vec)
  (let ((div (vect2:magnitude vec)))
    (make-vect2 (/ (vect2-x vec) div)
                (/ (vect2-y vec) div))))

;;; Utility procedure to make a vector with each component 1.0 divided by the
;;; component of the given one

(define (vect2:~1/vect2 vec)
  (make-vect2 (/ 1.0 (vect2-x vec))
              (/ 1.0 (vect2-y vec))))