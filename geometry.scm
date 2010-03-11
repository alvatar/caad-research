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

;; Calculate absolute point given segment and percentage
;;
(define (point-from-relative-in-segment point-a point-b percentage)
  (if (or (null-list? point-a) (null-list? point-b))
    (raise "Wrong points passed")
    (let* ((Ax (car point-a))
           (Ay (cadr point-a))
           (ABx (- (car point-b) Ax))
           (ABy (- (cadr point-b) Ay)))
        (list (+ Ax (* ABx percentage))
              (+ Ay (* ABy percentage))))))

;; Calculate the distance between two points
;;
(define (distance-point-point a b)
  (sqrt (+ (expt (- (car p1) (car p2)) 2)
           (expt (- (cadr p1) (cadr p2)) 2))))

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

;; Tell whether the two segments are connected
;;
(define (segments-are-connected? seg1 seg2)
  (or (is-end-point? seg2 (car seg1))
      (is-end-point? seg2 (cadr seg1))))

;; Calculate vector length
;;
(define (evector-length vec)
  (sqrt (+ (expt (car vec) 2)
           (expt (cadr vec) 2))))

;; Normalize vectors
;;
(define (evector-normalize vec)
  (let ((div (evector-length vec)))
    (list (/ (car vec) div)
          (/ (cadr vec) div))))

;; Tell whether the segments are parallel
;;
(define (parallel? seg1 seg2)
  (let ((vec1 (list (abs (- (caadr seg1) (caar seg1)))
                    (abs (- (cadadr seg1) (cadar seg1)))))
        (vec2 (list (abs (- (caadr seg2) (caar seg2)))
                    (abs (- (cadadr seg2) (cadar seg2))))))
    (=~ (evector-normalize vec1) (evector-normalize vec2) 0.01)))
