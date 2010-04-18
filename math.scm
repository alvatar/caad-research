;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mathematical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)(extended-bindings)(block)(not safe))
(compile-options force-compile: #t)

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

;;; Is equal? (with precision) for lists

(define (=~e* a b precision)
  (if (and (list? a) (list? b))
      (every (lambda (e1 e2) (=~e* e1 e2 precision))
             a
             b)
    (=~e a b precision)))

;;; Is equal? for lists

(define (=~* a b precision)
  (=~e* a b equal-accuracy))

;;; Is equal? (with precision) for inexacts

(define (=~e a b e)
  (< (abs (- a b)) e))

;;; Is equal? for inexacts

(define (=~ a b)
  (=~e a b equal-accuracy))

;;; Inverse function

(define (inverse x)
  (/ 1.0 x))

;;; Average between two values

(define (average a b)
  (/ (+ a b) 2.0))

;;; Square

(define (square x)
  (* x x))

(define (fxsquare x)
  (fx* x x))

(define (flsquare x)
  (fl* x x))

;;; Fixnum increment

(define (incr x)
  (fx+ x 1))

;;; Fixnum decrement

(define (decr x)
  (fx- x 1))

;-------------------------------------------------------------------------------
; Vector dimension 2
;-------------------------------------------------------------------------------

;;; vect2 type

(define-structure vect2 x y)

;;; Vector addition

(define (vect2:+vect2 v1 v2)
  (make-vect2 (+ (vect2-x v1)
                 (vect2-x v2))
              (+ (vect2-y v1)
                 (vect2-y v2))))

(define-syntax vect2+
  (syntax-rules ()
    ((_ vec1)
     vec1)
    ((_ vec1 vec2)
     (vect2:+vect2 vec1 vec2))
    ((_ vec1 vec2 rest ...)
     (vect2+ (vect2:+vect2 vec1 vec2) rest ...))))

;;; Vector substraction

(define (vect2:-vect2 v1 v2)
  (make-vect2 (- (vect2-x v1)
                 (vect2-x v2))
              (- (vect2-y v1)
                 (vect2-y v2))))

(define-syntax vect2-
  (syntax-rules ()
    ((_ vec1)
     (vect2:symmetric vec1))
    ((_ vec1 vec2)
     (vect2:-vect2 vec1 vec2))
    ((_ vec1 vec2 rest ...)
     (vect2- (vect2:-vect2 vec1 vec2) rest ...))))

;;; Vector dot product

(define (vect2:*vect2 v1 v2)
  (make-vect2 (* (vect2-x v1)
                 (vect2-x v2))
              (* (vect2-y v1)
                 (vect2-y v2))))

(define-syntax vect2*
  (syntax-rules ()
    ((_ vec1)
     vec1)
    ((_ vec1 vec2)
     (vect2:*vect2 vec1 vec2))
    ((_ vec1 vec2 rest ...)
     (vect2* (vect2:*vect2 vec1 vec2) rest ...))))

;;; Vector * scalar

(define (vect2:*scalar v a)
  (make-vect2 (* (vect2-x v) a)
              (* (vect2-y v) a)))

;;; Vector / scalar

(define (vect2:/scalar v a)
  (make-vect2 (/ (vect2-x v) a)
              (/ (vect2-y v) a)))

;;; Are these vectors equal?

(define (vect2:=? v1 v2)
  (vect2:=?e v1 v2 equal-accuracy))

;;; Are these vectors equal? (with epsilon)

(define (vect2:=?e v1 v2 e)
  (and (=~e (vect2-x v1)
            (vect2-x v2)
            e)
       (=~e (vect2-y v1)
            (vect2-y v2)
            e)))

;;; Square root function of a vector

(define (vect2:sqrt vec)
  (make-vect2 (sqrt (vect2-x vec))
              (sqrt (vect2-y vec))))

;;; Calculate vector length

(define (vect2:magnitude vec)
  (sqrt (+ (expt (vect2-x vec) 2)
           (expt (vect2-y vec) 2))))

;;; Calculate squared vector length

(define (vect2:squaredmagnitude vec)
  (+ (expt (vect2-x vec) 2)
     (expt (vect2-y vec) 2)))

;;; Calculate the symmetric vector

(define (vect2:symmetric vec)
  (make-vect2 (- (vect2-x vec))
              (- (vect2-y vec))))

;;; Normalize vector

(define (vect2:normalize vec)
  (let ((div (vect2:magnitude vec)))
    (make-vect2 (/ (abs (vect2-x vec)) div)
                (/ (abs (vect2-y vec)) div))))

;;; Utility procedure to make a vector with each component 1.0 divided by the
;;; component of the given one

(define (vect2:1/vect2 vec)
  (make-vect2 (/ 1.0 (vect2-x vec))
              (/ 1.0 (vect2-y vec))))
