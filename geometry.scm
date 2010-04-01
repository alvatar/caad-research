;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(compile-options cc-options: "-w" force-compile: #t)

(import (std srfi/1))

(import math)

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Point type
;;
(define-structure point x y)

;; Are these points equal? (with epsilon)
;;
(define (point=?e v1 v2 e)
  (and (=~e (point-x v1)
            (point-x v2)
            e)
       (=~e (point-y v1)
            (point-y v2)
            e)))

;; Are these points equal?
;;
(define (point=? v1 v2)
  (point=?e v1 v2 equal-accuracy)) ; TODO: Find the right accuracy

;; Point to vector
;;
(define (point->vect2 p)
  (make-vect2 (point-x p) (point-y p)))

;; Point to vector
;;
(define (vect2->point vec)
  (make-point (vect2-u vec) (vect2-v vec)))

;; Is equal? (with precision) for points
;;
(define (point=~ a b precision)
  (define (test f1 f2)
    (< (abs (- f1 f2)) precision))
  (and (test (point-x a) (point-x b))
       (test (point-y a) (point-y b))))

;; Calculate the mid point between two points
;;
(define (mid-point a b)
  (make-point (average (point-x a) (point-x b))
              (average (point-y a) (point-y b))))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

;; First point of segment
;;
(define (segment-first-point seg)
  (car seg))

;; Second point of segment
;;
(define (segment-second-point seg)
  (cadr seg))

;; Segment vector
;;
(define (segment->vect2 seg)
  (vect2---vect2
    (point->vect2 (segment-second-point seg))
    (point->vect2 (segment-first-point seg))))

;; Segment length
;;
(define (segment-length seg)
  (vect2-length (segment->vect2 seg)))

;; Tell whether the point is an end point of the segment
;;
(define (is-end-point? segment point)
  (let* ((px (point-x point))
         (py (point-y point))
         (a (segment-first-point segment))
         (ax (point-x a))
         (ay (point-y a))
         (b (segment-second-point segment))
         (bx (point-x b))
         (by (point-y b)))
    (or (and
          (=~ ax px)
          (=~ ay py))
        (and
          (=~ bx px)
          (=~ by py)))))

;; Tell whether the two segments are connected
;;
(define (segments-are-connected? seg1 seg2)
  (or (is-end-point? seg2 (segment-first-point seg1))
      (is-end-point? seg2 (segment-second-point seg1))))

;; Tell whether the segments are parallel
;;
(define (parallel? seg1 seg2)
  (vect2=?e
    (vect2-normalize (segment->vect2 seg1))
    (vect2-normalize (segment->vect2 seg2))
    0.01))

;; Calculate absolute point given segment and percentage
;;
(define (point-from-relative-in-segment seg percentage)
  (let ((vec (segment->vect2 seg))
        (O (segment-first-point seg)))
    (make-point (+ (point-x O) (* (vect2-u vec) percentage))
                (+ (point-y O) (* (vect2-v vec) percentage)))))

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
; Distance
;-------------------------------------------------------------------------------

;; Calculate the distance between two points
;;
(define (distance-point-point a b)
  (sqrt (+ (expt (- (point-x a) (point-x b)) 2)
           (expt (- (point-y a) (point-y b)) 2))))

;; Calculate the distance between two points (integer arithmetic)
;;
(define (distance-point-point-integer a b)
  (integer-sqrt (+ (expt (- (point-x a) (point-x b)) 2)
           (expt (- (point-y a) (point-y b)) 2))))

;; Calculate the distance between point and segment
;;
(define (distance-point-segment p sg)
  0.0)

;; Calculate the distance between a point and a point list
;;
(define (distance-point-point-list p plis)
  0.0)

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
