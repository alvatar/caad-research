;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algebraic kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (mostly-generic))
(compile-options force-compile: #t)

(import (std srfi/1))
(import (std srfi/16))
(import ../core/functional)

;-------------------------------------------------------------------------------
; Arithmetic
;-------------------------------------------------------------------------------

;;; Inverse function

(define inverse (curry / 1))

;;; Square

(define (square x) (* x x))

;-------------------------------------------------------------------------------
; Statistic
;-------------------------------------------------------------------------------

;;; Takes the smallest value of a list

(define (pick-min l) (apply min l))

;;; Takes the biggest value of a list

(define (pick-max l) (apply max l))

;;; Computes the sum of all values

(define (sum l) (apply + l))

;;; 1 arg: mean of all values in a list
;;; 2 args: mean of the two values

(define mean ; TODO: Check if optimizes, build specific mean2/mean-list generator
  (case-lambda
   ((l) (/ (sum l) (length l))) ; OPTIMIZE
   ((a b) (/ (+ a b) 2))))

;;; Weighed mean of the values of the first list, the second are the weights

(define (weighted-mean vl wl)
  (if (not (= (length vl) (length wl)))
      (error "Values and weights lists are not of the same length")) ; TODO: Change to arg-checks
  (let ((q (fold (lambda (v w num.den) (list
                                   (+ (* v w) (car num.den))
                                   (+ w (cadr num.den))))
                 '(0 0) vl wl)))
    (/ (car q) (cadr q))))

;-------------------------------------------------------------------------------
; Intervals
;-------------------------------------------------------------------------------

;;; Clamp value between low and high values

(define (clamp x lo hi)
  (cond ((< x lo) lo)
        ((> x hi) hi)
        (else x)))

;;; Normalize value in a range

(define (normalize x lo hi) ; TODO: should check boundaries?
  (/ (- x lo) (- hi lo)))

;;; Takes a value and two boundaries, using any as reference, inverts the intervals

(define (invert x lo hi) ; TODO: should check boundaries (or clamp)?
  (+ lo (- hi x)))

;;; Clamp value and normalize it to the given boundaries

(define (clamp&normalize x lo hi) ; TODO: try with compose
  (normalize (clamp x lo hi) lo hi))

;;; Clamp & normalize, plus an additional inversion of the reference

(define (clamp&invert&normalize x lo hi) ; TODO: compose!
  (normalize (invert (clamp x lo hi) lo hi) lo hi))

;;; Cut the value if lower than

(define (cut-low x lo)
  (if (< x lo)
      lo
      x))

;;; Cut the value if higher than

(define (cut-high x hi)
  (if (> x hi)
      hi
      x))

;;; If greater than, otherwise return value

(define (if> a b f)
  (if (> a b)
      f
      a))

;;; If smaller than, otherwise return value

(define (if< a b f)
  (if (< a b)
      f
      a))

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

;;; Calculate x projection

(define (vect2:x-projection vec)
  (make-vect2 (vect2-x vec)
              0))

;;; Calculate y projection

(define (vect2:y-projection vec)
  (make-vect2 0
              (vect2-y vec)))

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
      (cond ((< x lox) lox)
            ((> x hix)  hix)
            (else x))
      (cond ((< y loy) lox)
            ((> y hiy) hiy)
            (else y)))))

;;; Clamp to low and high values

(define (vect2:clamp-values vec lo hi)
  (let ((x (vect2-x vec))
        (y (vect2-y vec)))
    (make-vect2
      (cond ((< x lo) lo)
            ((> x hi) hi)
            (else x))
      (cond ((< y lo) lo)
            ((> y hi) hi)
            (else y)))))
