;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mathematical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
;(compile-options force-compile: #t)

(import (std srfi/1))

;-------------------------------------------------------------------------------
; Constants
;-------------------------------------------------------------------------------

(define pi 3.14159265)
(define pi2 6.28318531)
(define pi/2 1.57079633)
(define pi/-2 -1.57079633)

(define equal-accuracy 0.000001)

;-------------------------------------------------------------------------------
; Arithmetic
;-------------------------------------------------------------------------------

;; Is equal? (with precision) for lists
;;
(define (=~e* a b precision)
  (if (and (list? a) (list? b))
      (every (lambda (e1 e2) (=~e* e1 e2 precision))
             a
             b)
    (=~e a b precision)))

;; Is equal? for lists
;;
(define (=~* a b precision)
  (=~e* a b equal-accuracy))

;; Is equal? (with precision) for inexacts
;;
(define (=~e a b e)
  (< (abs (- a b)) e))

;; Is equal? for inexacts
;;
(define (=~ a b)
  (=~e a b equal-accuracy))

;; Average between two values
;;
(define (average a b)
  (/ (+ a b) 2.0))

;; Fixnum square
;;
(define (fxsquare x)
  (fx* x x))

;; Square
;;
(define (square x)
  (* x x))

;; Fixnum increment
;;
(define (incr x)
  (fx+ x 1))

;; Fixnum decrement
;;
(define (decr x)
  (fx- x 1))

;-------------------------------------------------------------------------------
; Vector dimension 2
;-------------------------------------------------------------------------------

;;; vect2 type

(define-structure vect2 u v)

;; Vector addition
;;
(define (vect2+vect2 v1 v2)
  (make-vect2 (+ (vect2-u v1)
                 (vect2-u v2))
              (+ (vect2-v v1)
                 (vect2-v v2))))

;; Vector substraction
;;
(define (vect2-vect2 v1 v2)
  (make-vect2 (- (vect2-u v1)
                 (vect2-u v2))
              (- (vect2-v v1)
                 (vect2-v v2))))

;;; Vector * scalar

(define (vect2*scalar v a)
  (make-vect2 (* (vect2-u v) a)
              (* (vect2-v v) a)))

;;; Vector / scalar

(define (vect2/scalar v a)
  (make-vect2 (/ (vect2-u v) a)
              (/ (vect2-v v) a)))

;; Are these vectors equal?
;;
(define (vect2=? v1 v2)
  (vect2=?e v1 v2 equal-accuracy))

;; Are these vectors equal? (with epsilon)
;;
(define (vect2=?e v1 v2 e)
  (and (=~e (vect2-u v1)
            (vect2-u v2)
            e)
       (=~e (vect2-v v1)
            (vect2-v v2)
            e)))

;; Calculate vector length
;;
(define (vect2-length vec)
  (sqrt (+ (expt (vect2-u vec) 2)
           (expt (vect2-v vec) 2))))

;; Normalize vector
;;
(define (vect2-normalize vec)
  (let ((div (vect2-length vec)))
    (make-vect2 (/ (abs (vect2-u vec)) div)
                (/ (abs (vect2-v vec)) div))))

;;; Vector rotation

(define (vect2-rotation vec r-angle)
  (make-vect2 (- (* (vect2-u vec) (cos r-angle))
                 (* (vect2-v vec) (sin r-angle)))
              (+ (* (vect2-v vec) (cos r-angle))
                 (* (vect2-u vec) (sin r-angle)))))
