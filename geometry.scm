;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

;-------------------------------------------------------------------------------
; Mathematical
;-------------------------------------------------------------------------------

;; Is equal? (with precision) for lists
;;
(define (list=~ a b precision)
  (define (test t1 t2)
    (< (abs (- a b)) precision))
  (if (and (list? a) (list? b))
      (every (lambda (e1 e2) (list=~ e1 e2 precision))
             a
             b)
    (test a b)))

;; Is equal? (with precision) for inexacts
;;
(define (=~ a b precision)
  (< (abs (- a b)) precision))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Point type : TODO!!! PROPAGATE!!!
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
    (if (=~ u-b 0.0 0.000001) ; TODO: find the right precision
        (if (or (=~ ua-t 0.0 0.000001) (=~ ub-t 0.0 0.000001))
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
