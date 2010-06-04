;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algebraic kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
(compile-options force-compile: #t)

(import (std srfi/1))
(import (std srfi/16))
(import ../core/functional)

;-------------------------------------------------------------------------------
; Arithmetic
;-------------------------------------------------------------------------------

;;; add 1

(define (add1 x)
  (+ x 1))

;;; substract 1

(define (sub1 x)
  (- x 1))

;;; Inverse function

(define (inverse x)
  (/ 1 x))

;;; Square

(define (square x)
  (* x x))

;;; 1 arg: mean of all values in a list
;;; 2 args: mean of the two values

(define mean ; TODO: Check if optimizes, build specific mean2/mean-list generator
  (case-lambda
   ((l) (/ (sum l) (length l)))
   ((a b) (/ (+ a b) 2)))) ; TODO: OPTIMIZE!

;;; Takes the smallest value of a list

(define (pick-min l)
  (reduce (lambda (x prev) (min x prev)) l l))

;;; Takes the biggest value of a list

(define (pick-max l)
  (reduce (lambda (x prev) (max x prev)) l l))

;;; Computes the sum of all values

(define (sum l)
  (reduce (lambda (x prev) (+ x prev)) l l))

;-------------------------------------------------------------------------------
; Vector dimension 2
;-------------------------------------------------------------------------------

;;; vect2 type
(define-structure vect2 x y)

;;; Vector addition

(define-associative vect2+ (vect2:+vect2 v1 v2)
  (make-vect2 (+ (vect2-x v1)
                 (vect2-x v2))
              (+ (vect2-y v1)
                 (vect2-y v2))))

;;; Vector substraction

(define-associative vect2- (vect2:-vect2 v1 v2)
  (make-vect2 (- (vect2-x v1)
                 (vect2-x v2))
              (- (vect2-y v1)
                 (vect2-y v2))))

;;; Vector dot product

(define-associative vect2* (vect2:*vect2 v1 v2)
  (make-vect2 (* (vect2-x v1)
                 (vect2-x v2))
              (* (vect2-y v1)
                 (vect2-y v2))))

;;; Vector * scalar

(define (vect2:*scalar v a)
  (make-vect2 (* (vect2-x v) a)
              (* (vect2-y v) a)))

;;; Vector / scalar

(define (vect2:/scalar v a)
  (make-vect2 (/ (vect2-x v) a)
              (/ (vect2-y v) a)))

;;; Are these vectors equal? (with epsilon)

(define (vect2:= v1 v2)
  (and (= (vect2-x v1)
          (vect2-x v2))
       (= (vect2-y v1)
          (vect2-y v2))))

;;; Zero vector

(define (vect2:zero)
  (make-vect2 0 0))

;;; Random exact vector (range -1 -> 1)

(define (vect2:random)
  (make-vect2 (+ -1 (* (inexact->exact (random-real)) 2))
              (+ -1 (* (inexact->exact (random-real)) 2))))

;;; Calculate squared vector length

(define (vect2:squaremagnitude vec)
  (+ (square (vect2-x vec))
     (square (vect2-y vec))))

;;; Calculate the symmetric vector

(define (vect2:symmetric vec)
  (make-vect2 (- (vect2-x vec))
              (- (vect2-y vec))))

;;; Absolute vector

(define (vect2:abs vec)
  (make-vect2 (abs (vect2-x vec))
              (abs (vect2-y vec))))

;;; Utility procedure to make a vector with each component 1.0 divided by the
;;; component of the given one

(define (vect2:1/vect2 vec)
  (make-vect2 (/ 1 (vect2-x vec))
              (/ 1 (vect2-y vec))))

;;; Clamp to low and high vect2

(define (vect2:clamp-vect2 vec lo-vec hi-vec)
  (let ((x (vect2-x vec))
        (y (vect2-y vec))
        (lox (vect2-x lo-vec))
        (loy (vect2-y lo-vec))
        (hix (vect2-x hi-vec))
        (hiy (vect2-y hi-vec)))
    (make-vect2
      (cond
       ((< x lox)
        lox)
       ((>= x hix)
        hix)
       (else
        x))
      (cond
       ((< y loy)
        lox)
       ((>= y hiy)
        hiy)
       (else
        y)))))

;;; Clamp to low and high values

(define (vect2:clamp-values vec lo hi)
  (let ((x (vect2-x vec))
        (y (vect2-y vec)))
    (make-vect2
      (cond
       ((< x lo)
        lo)
       ((>= x hi)
        hi)
       (else
        x))
      (cond
       ((< y lo)
        lo)
       ((>= y hi)
        hi)
       (else
        y)))))
