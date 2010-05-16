;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Algebraic kernel
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
(compile-options force-compile: #t)

(import ../core/functional)

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

;;; Is equal? (with precision) for lists
;;; TODO: implement in terms of tree iteration
#|
(define (=~e* a b precision)
  (if (and (list? a) (list? b))
      (every (lambda (e1 e2) (=~e* e1 e2 precision))
             a
             b)
    (=~e a b precision)))

;;; Is equal? for lists

(define (=~* a b precision)
  (=~e* a b equal-accuracy))
    |#

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

;;; add 1

(define (add1 x)
  (+ x 1))

;;; substract 1

(define (sub1 x)
  (- x 1))

;;; Inverse function

(define (inverse x)
  (/ 1.0 x))

;;; Average between two values

(define (average a b)
  (/ (+ a b) 2.0))

;;; Square

(define (square x)
  (* x x))

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

;;; Random vect2

(define (vect2:random)
  (make-vect2 (fl+ -1.0 (fl* (random-real) 2.0))
              (fl+ -1.0 (fl* (random-real) 2.0))))

;;; Zero vector

(define (vect2:zero)
  (make-vect2 0.0 0.0))

;;; Square root function of a vector

(define (vect2:sqrt vec)
  (make-vect2 (sqrt (vect2-x vec))
              (sqrt (vect2-y vec))))

;;; Calculate vector length

(define (vect2:magnitude vec)
  (sqrt (+ (square (vect2-x vec))
           (square (vect2-y vec)))))

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

;;; Normalize vector

(define (vect2:normalize vec)
  (let ((div (vect2:magnitude vec)))
    (make-vect2 (/ (vect2-x vec) div)
                (/ (vect2-y vec) div))))

;;; Utility procedure to make a vector with each component 1.0 divided by the
;;; component of the given one

(define (vect2:1/vect2 vec)
  (make-vect2 (/ 1.0 (vect2-x vec))
              (/ 1.0 (vect2-y vec))))

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
