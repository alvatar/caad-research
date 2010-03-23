;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import math)

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Point type : TODO!!! PROPAGATE and REFACTOR
;;
#;(define-record-type point
  (make-point x y)
  point?
  (x point-x)
  (y point-y))
(define (point-x point)
  (car point))
(define (point-y point)
  (cadr point))
(define (make-point x y)
  (list x y))
(define (point? p)
  (and (list? p) (number? (car p)) (number? (cadr p))))

;; Point + Point
;;
(define (point+point a b)
  (if (and (point? a) (point? b))
      (make-point (+ (point-x a) (point-x b))
                  (+ (point-y a) (point-y b)))
    (raise "point-point+: both arguments #1 and #2 must be points")))

;; Point - Point
;;
(define (point-point a b)
  (if (and (point? a) (point? b))
      (make-point (- (point-x a) (point-x b))
                  (- (point-y a) (point-y b)))
    (raise "point-point+: both arguments #1 and #2 must be points")))

;; Point * scalar
;;
(define (point*scalar a b)
  (if (and (point? a) (number? b))
      (make-point (/ (point-x a) b)
                  (/ (point-y a) b))
    (raise "point-scalar/: argument #1 must be a point and #2 must be a scalar")))

;; Point / scalar
;;
(define (point/scalar a b)
  (if (and (point? a) (number? b))
      (make-point (/ (point-x a) b)
                  (/ (point-y a) b))
    (raise "point-scalar/: argument #1 must be a point and #2 must be a scalar")))

;; Is equal? (with precision) for points
;;
(define (point=~ a b precision)
  (define (test f1 f2)
    (< (abs (- f1 f2)) precision))
  (and (test (point-x a) (point-x b))
       (test (point-y a) (point-y b))))

;; Calculate the distance between two points
;;
(define (distance-point-point a b)
  (sqrt (+ (expt (- (car p1) (car p2)) 2)
           (expt (- (cadr p1) (cadr p2)) 2))))

;; Calculate the mid point between two points
;;
(define (mid-point a b)
  (make-point (average (point-x a) (point-x b))
              (average (point-y a) (point-y b))))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

;; Tell whether the point is an end point of the segment
;;
(define (is-end-point? segment point)
  (let ((point-x (car point))
        (point-y (cadr point)))
    (or (and
          (list=~ (caar segment) point-x 0.0001)
          (list=~ (cadar segment) point-y 0.0001))
        (and
          (list=~ (caadr segment) point-x 0.0001)
          (list=~ (cadadr segment) point-y 0.0001)))))

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
    (list=~ (evector-normalize vec1) (evector-normalize vec2) 0.01)))

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

;-------------------------------------------------------------------------------
; Point-lists: Polygons and Paths
;-------------------------------------------------------------------------------

;; Point-list centroid
;;
(define (point-list-centroid plis)
  (define (iter n sum plis-tail)
    (cond
     ((null? plis-tail)
      (point/scalar sum (exact->inexact n)))
     (else
      (iter
        (+ 1 n)
        (point+point sum (car plis-tail))
        (cdr plis-tail)))))
  (cond
   ((null? plis)
    (raise "point-list-centroid: argument #1 should be a point list"))
   (else
    (iter 0 (make-point 0.0 0.0) plis))))

;; Point-list right-most point
;;
(define (point-list-right-most plis)
  (define (iter current plis-tail)
    (cond
     ((null? plis-tail)
      current)
     (else
      (let ((next (car plis-tail)))
        (if (> (point-x next) (point-x current))
            (iter next (cdr plis-tail))
          (iter current (cdr plis-tail)))))))
  (if (null-list? plis)
      (raise "point-list-right-most: argument #1 must be a point list")
    (iter (car plis) (cdr plis))))

;; Is point in polygon?
;;
;; http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
(define (point-in-polygon? polygon point)
  (define (iter-intersection counter p1 ptail)
    (if (null-list? ptail)
        counter
      (let* ((p2 (car ptail))
             (p1x (point-x p1))
             (p1y (point-y p1))
             (p2x (point-x p2))
             (p2y (point-y p2))
             (px (point-x point))
             (py (point-y point)))
        (if (and (>= py (min p1y p2y)) ; Should this be > ? Handles borders better
                 (<= py (max p1y p2y))
                 (<= px (max p1x p2x))
                 (not (=~ (point-y p1) (point-y p2)))
                 (or (=~ p1x p2x)
                     (<= px (/ (* (- py p1y) (- p2x p1x))
                               (+ (- p2y p1y) p1x)))))
            (iter-intersection (+ counter 1) p2 (cdr ptail))
          (iter-intersection counter p2 (cdr ptail))))))
  (odd? (iter-intersection 0 (car polygon) (cdr polygon))))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

;; Segment-segment intersection
;;
(define (segment-segment-intersection sg1 sg2)
  (let* ((a1 (car sg1))
         (a2 (cadr sg1))
         (b1 (car sg2))
         (b2 (cadr sg2))
         (ua-t (- (* (- (point-x b2) (point-x b1))
                     (- (point-y a1) (point-y b1)))
                  (* (- (point-y b2) (point-y b1))
                     (- (point-x a1) (point-x b1)))))
         (ub-t (- (* (- (point-x a2) (point-x a1))
                     (- (point-y a1) (point-y b1)))
                  (* (- (point-y a2) (point-y a1))
                     (- (point-x a1) (point-x b1)))))
         (u-b (- (* (- (point-y b2) (point-y b1))
                    (- (point-x a2) (point-x a1)))
                 (* (- (point-x b2) (point-x b1))
                    (- (point-y a2) (point-y a1))))))
    (if (=~ u-b 0.0)
        (if (or (=~ ua-t 0.0) (=~ ub-t 0.0))
            'coincident
          'parallel)
      (let ((ua (/ ua-t u-b))
            (ub (/ ub-t u-b)))
        (if (and (<= 0 ua)
                 (<= ua 1)
                 (<= 0 ub)
                 (<= ub 1))
            (make-point (* (+ (point-x a1) ua)
                           (- (point-x a2) (point-x a1)))
                        (* (+ (point-y a1) ua)
                           (- (point-y a2) (point-y a1))))
          'no-intersection)))))

;; Segment-polyline intersection
;;
(define (segment-polyline-intersection seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (segment-segment-intersection seg (list (car pol-rest) (cadr pol-rest)))))
      (if (or (null-list? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
        (if (point? inters)
            (append-next (append intersections (list inters)) (cdr pol-rest))
          (append-next intersections (cdr pol-rest))))))
  (append-next '() pol))

;; Segment-polygon (closed polyline) intersection
;;
(define (segment-polygon-intersection seg pol)
  (segment-polyline-intersection seg (append pol (list (car pol)))))
