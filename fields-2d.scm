;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Data structures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
;(compile-options force-compile: #t)

(import (std srfi/1))

(import math/algebra)

;-------------------------------------------------------------------------------
; u8 fields
;-------------------------------------------------------------------------------

;;; u8 field structure

(define-structure u8-2dfield data size-x size-y mapped-x mapped-y)

;;; Make bidimensional u8 integers field

(define (produce-u8-2dfield
          samples-num-x
          samples-num-y
          mapped-dim-x
          mapped-dim-y
          proc)
  (make-u8-2dfield
    (let* ((point (make-vect2 0 0))
           (buffer-len (fx* samples-num-x samples-num-y))
           (pixel-size-x (fl/ mapped-dim-x (fixnum->flonum samples-num-x)))
           (pixel-size-y (fl/ mapped-dim-y (fixnum->flonum samples-num-y)))
           (pixel-size (flmax pixel-size-x pixel-size-y)))
      (do ((vec (make-u8vector buffer-len))
           (i 0 (fx+ i 1)))
          ((fx>= i buffer-len) vec)
        (vect2-x-set! point (fl* pixel-size (fixnum->flonum (fxmodulo i samples-num-y))))
        (vect2-y-set! point (fl* pixel-size (fixnum->flonum (fxquotient i samples-num-y))))
        (u8vector-set! vec i (proc point))))
    samples-num-x
    samples-num-y
    mapped-dim-x
    mapped-dim-y))

;;; Make bidimensional u8 integers field (1 to 1 scale)

(define (produce-u8-2dfield-1:1 size-x size-y proc)
  (make-u8-2dfield
    (let ((point (make-vect2 0 0))
          (len (fx* size-x size-y)))
        (do ((vec (make-u8vector len))
             (i 0 (fx+ i 1))) ((fx>= i len) vec)
          (vect2-x-set! point (fxmodulo i size-y))
          (vect2-y-set! point (fxquotient i size-y))
          (u8vector-set! vec i (proc point))))
    size-x
    size-y
    size-x
    size-y))

;;; Make bidimensional u8 integers field (with resolution, 1 to 1 scale)

(define (produce-u8-2dfield-with-resolution-1:1 res size-x size-y proc)
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
            (it-i (add1 area-i)))
           (else #t)))
        (it-j (add1 area-j)))
       (else #t))))
  (make-u8-2dfield
    (let ((point (make-vect2 0 0))
          (len (fx* size-x size-y)))
      (do ((vec (make-u8vector len))
           (j 0 (fx+ j res)))
        ((fx>= j size-y) vec)
        (do ((i 0 (fx+ i res)))
          ((fx>= i size-x))
          (vect2-x-set! point i)
          (vect2-y-set! point j)
          (set-area! vec i j (proc point)))))
    size-x
    size-y
    size-x
    size-y))

;;; Make bidimensional u8 integers field (with resolution)

(define (produce-u8-2dfield-with-resolution
          res
          samples-num-x
          samples-num-y
          mapped-dim-x
          mapped-dim-y
          proc)
  (define (set-area! vec vec-len i j value)
    (let it-j ((area-j 0))
      (cond
       ((fx< area-j res)
        (let it-i ((area-i 0))
          (cond
           ((fx< area-i res)
            (let ((buffer-pos (fx+ (fx+ i (fx* samples-num-x (fx+ j area-j))) area-i)))
              (if (fx< buffer-pos vec-len) ; OPTIMIZE: is there a way to avoid this passing the number of iterations to the procedure?
                  (u8vector-set! vec
                                 buffer-pos
                                 value))
                (it-i (add1 area-i))))
           (else #t)))
        (it-j (add1 area-j)))
       (else #t))))
  (make-u8-2dfield
    (let* ((point (make-vect2 0 0))
           (buffer-len (fx* samples-num-x samples-num-y))
           (pixel-size-x (fl/ mapped-dim-x (fixnum->flonum samples-num-x)))
           (pixel-size-y (fl/ mapped-dim-y (fixnum->flonum samples-num-y)))
           (pixel-size (flmax pixel-size-x pixel-size-y))
           (limit-x samples-num-x)
           (limit-y samples-num-y))
      (do ((vec (make-u8vector buffer-len))
           (j 0 (fx+ j res)))
        ((fx>= j limit-y) vec)
        (do ((i 0 (fx+ i res)))
          ((fx>= i limit-x))
          (vect2-x-set! point (fl* pixel-size (fixnum->flonum i)))
          (vect2-y-set! point (fl* pixel-size (fixnum->flonum j)))
          (set-area! vec buffer-len i j (proc point)))))
    samples-num-x
    samples-num-y
    mapped-dim-x
    mapped-dim-y))

;;; Merge bidimesional u8 integer fields

(define (merge-u8-2dfields fields proc)
  (define (merge-point value p rest-fields)
    (cond
     ((null? rest-fields)
      value)
     (else
      (merge-point
        (proc value (u8vector-ref (u8-2dfield-data (car rest-fields)) p))
        p
        (cdr rest-fields)))))
  (let ((first-field (car fields)))
    (make-u8-2dfield
      (cond
       ((< (length fields) 1)
        (error "merge-u8-2dfields: you passed a null list of fields"))
       ((< (length fields) 2)
        (u8-2dfield-data first-field))
       (else
        (let* ((f (car fields))
               (field-data (u8-2dfield-data f))
               (len (u8vector-length field-data)))
          (do ((vec (make-u8vector len))
               (i 0 (fx+ i 1)))
              ((fx>= i len) vec)
            (u8vector-set! vec
                           i
                           (merge-point (u8vector-ref field-data i)
                                        i
                                        (cdr fields)))))))
      (u8-2dfield-size-x first-field)
      (u8-2dfield-size-y first-field)
      (u8-2dfield-mapped-x first-field)
      (u8-2dfield-mapped-y first-field))))

;;; Get value from field position

(define (u8-2dfield-position->value field pos)
  (let* ((coords (u8-2dfield-position->coords field pos))
         (i (vect2-x coords))
         (j (vect2-y coords)))
    (u8vector-ref (u8-2dfield-data field) (fx+ (fx* (u8-2dfield-size-x field) j) i))))

;;; Get value from field coordinates

(define (u8-2dfield-coords->value field coords)
  (u8vector-ref (u8-2dfield-data field)
                (fx+ (fx* (u8-2dfield-size-x field)
                          (vect2-y coords))
                     (vect2-x coords))))

;;; Convert from euclidean position to field coordinates

(define (u8-2dfield-position->coords field pos)
  (let* ((x (vect2-x pos))
         (y (vect2-y pos))
         (mapped (max (u8-2dfield-mapped-x field) (u8-2dfield-mapped-y field)))
         (i (inexact->exact (floor (/ (* x (u8-2dfield-size-x field)) mapped))))
         (j (inexact->exact (floor (/ (* y (u8-2dfield-size-y field)) mapped)))))
    (make-vect2 i j)))

;;; Transform coordinates to reflective boundaries topology

(define (u8-2dfield-coords->reflective-coords field coords)
  (vect2:clamp-vect2
    coords
    (make-vect2 0 0)
    (make-vect2
      (- (u8-2dfield-size-x field) 1)
      (- (u8-2dfield-size-y field) 1))))

;;; Transform coordinates to toroidal topology

(define (u8-2dfield-coords->toroidal-coords field coords)
  coords) ; TODO

;-------------------------------------------------------------------------------
; List fields
;-------------------------------------------------------------------------------

;;; Produce 2d fields with a lambda

(define (make-2d-field size-x size-y proc)
  (let ((limit-x (sub1 size-x))
        (limit-y (sub1 size-y)))
    (define (iter x y lis)
      (cond
       ((and (fx= x 0) (fx= y 0))
        lis)
       ((fx= x 0)
        (iter limit-x (sub1 y) (cons (proc (make-vect2 0 y)) lis)))
       (else
        (iter (sub1 x) y (cons (proc (make-vect2 x y)) lis)))))
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
