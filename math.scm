;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mathematical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
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

(define-structure vect2 u v)

;;; Vector addition

(define (vect2+vect2 v1 v2)
  (make-vect2 (+ (vect2-u v1)
                 (vect2-u v2))
              (+ (vect2-v v1)
                 (vect2-v v2))))

;;; Vector substraction

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

;;; Are these vectors equal?

(define (vect2=? v1 v2)
  (vect2=?e v1 v2 equal-accuracy))

;;; Are these vectors equal? (with epsilon)

(define (vect2=?e v1 v2 e)
  (and (=~e (vect2-u v1)
            (vect2-u v2)
            e)
       (=~e (vect2-v v1)
            (vect2-v v2)
            e)))

;;; Calculate vector length

(define (vect2-length vec)
  (sqrt (+ (expt (vect2-u vec) 2)
           (expt (vect2-v vec) 2))))

;;; Calculate squared vector length

(define (vect2-squaredlength vec)
  (+ (expt (vect2-u vec) 2)
     (expt (vect2-v vec) 2)))

;;; Calculate the inverse vector

(define (vect2-inverse vec)
  (make-vect2 (- (vect2-u vec))
              (- (vect2-v vec))))

;;; Normalize vector

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

;-------------------------------------------------------------------------------
; Vector fields
;-------------------------------------------------------------------------------

;;; Make bidimensional u8 integers field

(define (make-2d-u8field
          samples-num-x
          samples-num-y
          mapped-dim-x
          mapped-dim-y
          proc)
  (let* ((point (make-vect2 0 0))
         (buffer-len (fx* samples-num-x samples-num-y))
         (pixel-size-x (fl/ mapped-dim-x (fixnum->flonum samples-num-x)))
         (pixel-size-y (fl/ mapped-dim-y (fixnum->flonum samples-num-y)))
         (pixel-size (flmax pixel-size-x pixel-size-y)))
    (do ((vec (make-u8vector buffer-len))
         (i 0 (fx+ i 1)))
        ((fx>= i buffer-len) vec)
      (vect2-u-set! point (fl* pixel-size (fixnum->flonum (fxmodulo i samples-num-y))))
      (vect2-v-set! point (fl* pixel-size (fixnum->flonum (fxquotient i samples-num-y))))
      (u8vector-set! vec i (proc point)))))

;;; Make bidimensional u8 integers field (1 to 1 scale)

(define (make-2d-u8field-1:1 size-x size-y proc)
  (let ((point (make-vect2 0 0))
        (len (fx* size-x size-y)))
      (do ((vec (make-u8vector len))
           (i 0 (fx+ i 1))) ((fx>= i len) vec)
        (vect2-u-set! point (fxmodulo i size-y))
        (vect2-v-set! point (fxquotient i size-y))
        (u8vector-set! vec i (proc point)))))

;;; Make bidimensional u8 integers field (with resolution, 1 to 1 scale)

(define (make-2d-u8field-with-resolution-1:1 res size-x size-y proc)
  (define (set-area! vec i j value)
    (let it-j ((area-j 0))
      (cond
       ((fx< area-j res)
        (let it-i ((area-i 0))
          (cond
           ((fx< area-i res)
            (u8vector-set! vec
                           (fx+ (fx+ i (fx* size-x (fx+ j area-j))) area-i)
                           value)
            (it-i (incr area-i)))
           (else #t)))
        (it-j (incr area-j)))
       (else #t))))
  (let ((point (make-vect2 0 0))
        (len (fx* size-x size-y)))
    (do ((vec (make-u8vector len))
         (j 0 (fx+ j res)))
      ((fx>= j size-y) vec)
      (do ((i 0 (fx+ i res)))
        ((fx>= i size-x))
        (vect2-u-set! point i)
        (vect2-v-set! point j)
        (set-area! vec i j (proc point))))))

;;; Make bidimensional u8 integers field (with resolution)

(define (make-2d-u8field-with-resolution
          res
          samples-num-x
          samples-num-y
          mapped-dim-x
          mapped-dim-y
          proc)
  (define (set-area! vec i j value)
    (let it-j ((area-j 0))
      (cond
       ((fx< area-j res)
        (let it-i ((area-i 0))
          (cond
           ((fx< area-i res)
            (u8vector-set! vec
                           (fx+ (fx+ i (fx* samples-num-x (fx+ j area-j))) area-i)
                           value)
            (it-i (incr area-i)))
           (else #t)))
        (it-j (incr area-j)))
       (else #t))))
  (let* ((point (make-vect2 0 0))
         (buffer-len (fx* samples-num-x samples-num-y))
         (pixel-size-x (fl/ mapped-dim-x (fixnum->flonum samples-num-x)))
         (pixel-size-y (fl/ mapped-dim-y (fixnum->flonum samples-num-y)))
         (pixel-size (flmax pixel-size-x pixel-size-y))
         (limit-x (- samples-num-x res))
         (limit-y (- samples-num-y res)))
    (do ((vec (make-u8vector buffer-len))
         (j 0 (fx+ j res)))
      ((fx>= j limit-y) vec)
      (do ((i 0 (fx+ i res)))
        ((fx>= i limit-x))
        (vect2-u-set! point (fl* pixel-size (fixnum->flonum i)))
        (vect2-v-set! point (fl* pixel-size (fixnum->flonum j)))
        (set-area! vec i j (proc point))))))

;;; Merge bidimesional u8 integer fields

(define (merge-2d-u8fields fields proc)
  (define (merge-point value p rest-fields)
    (cond
     ((null? rest-fields)
      value)
     (else
      (merge-point
        (proc value (u8vector-ref (car rest-fields) p))
        p
        (cdr rest-fields)))))
  (cond
   ((< (length fields) 1) ; Improve
    '())
   ((< (length fields) 2)
    (car fields))
   (else
    (let ((len (u8vector-length (car fields))))
      (do ((vec (make-u8vector len))
           (i 0 (fx+ i 1)))
          ((fx>= i len) vec)
        (u8vector-set! vec
                       i
                       (merge-point (u8vector-ref (car fields) i)
                                    i
                                    (cdr fields))))))))

;-------------------------------------------------------------------------------
; List fields
;-------------------------------------------------------------------------------

;;; Produce 2d fields with a lambda

(define (make-2d-field size-x size-y proc)
  (let ((limit-x (decr size-x))
        (limit-y (decr size-y)))
    (define (iter x y lis)
      (cond
       ((and (fx= x 0) (fx= y 0))
        lis)
       ((fx= x 0)
        (iter limit-x (decr y) (cons (proc (make-vect2 0 y)) lis)))
       (else
        (iter (decr x) y (cons (proc (make-vect2 x y)) lis)))))
    (iter limit-x limit-y '())))

;;; Flatten a list of fields (merge them)

(define (flatten-2d-fields field-list)
  (list->u8vector
    (reduce
      (lambda (f1 f2)
        (map (lambda (a b)
               (let ((sum (- 255 (+ (- 255 a) (- 255 b)))))
                 (if (< sum 0) 0 sum)))
             f1
             f2))
      '()
      field-list)))
