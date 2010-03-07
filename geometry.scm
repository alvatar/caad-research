;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

;-------------------------------------------------------------------------------
; General
;-------------------------------------------------------------------------------

;; Is equal? (with precision)
;;
(define (=~ a b precision)
  (define (test t1 t2)
    (< (abs (- a b)) precision))
  (if (and (list? a) (list? b))
      (every (lambda (e1 e2) (=~ e1 e2 precision))
             a
             b)
    (test a b)))

;; Are the point lists equal? (with precision)
;;
; (define (equal-point-lists? lis1 lis2 precision)
  ; (every
    ; (lambda (a b)
      ; (every (lambda (e) (< e precision)) (map abs (map - a b))))
    ; lis1
    ; lis2))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Get coordinate from point
;;
(define (point-coord coordinate point)
  (define (find-coordinate point)
    (cond
     ((null-list? point)
      (raise "You sent me a null point. Seriously, what should I do with this?? Boy, I'm having a bad day thanks to you."))
     ((equal? (caar point) coordinate)
      (string->number (cadar point)))
     (else
      (find-coordinate (cdr point)))))
  (find-coordinate point))

;; Extract a list of point coordinates
;;
(define (extract-point-coords point)
  `(,(point-coord 'x point)
    ,(point-coord 'y point)))

;; Get point n from point list
;;
(define (point-n n point-list)
  (cdr (list-ref point-list n)))

;; Make point
;;
(define (make-point x y)
  (if (or (null? x) (null? y))
      (raise "Error making point: null arguments")
      (list (list 'y (number->string y))
            (list 'x (number->string x)))))

;; Calculate absolute point given segment and percentage
;;
(define (point-from-relative-in-segment point-a point-b percentage)
  (if (or (null-list? point-a) (null-list? point-b))
    (raise "Wrong points passed")
    (let* ((Ax (point-coord 'x point-a))
           (Ay (point-coord 'y point-a))
           (ABx (- (point-coord 'x point-b) Ax))
           (ABy (- (point-coord 'y point-b) Ay)))
      (make-point
        (+ Ax (* ABx percentage))
        (+ Ay (* ABy percentage))))))

;; Calculate the distance between two points
;;
(define (distance-point-point a b)
  (let ((p1 (extract-point-coords a))
        (p2 (extract-point-coords b)))
    (sqrt (+ (expt (- (car p1) (car p2)) 2)
             (expt (- (cadr p1) (cadr p2)) 2)))))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

;; Tell whether the point is an end point of the segment
;;
(define (is-end-point? segment point)
  (let ((point-x (car point))
        (point-y (cadr point)))
    (or (and
          (=~ (caar segment) point-x 0.0001)
          (=~ (cadar segment) point-y 0.0001))
        (and
          (=~ (caadr segment) point-x 0.0001)
          (=~ (cadadr segment) point-y 0.0001)))))
