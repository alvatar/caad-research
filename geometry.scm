;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)(extended-bindings)(block)(not safe))
(compile-options force-compile: #t)

(import (std srfi/1))

(import math)
(import utils/misc)

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;;; Point type

(define-structure point x y)

;;; Are these points equal? (with epsilon)

(define (point=?e v1 v2 e)
  (and (=~e (point-x v1)
            (point-x v2)
            e)
       (=~e (point-y v1)
            (point-y v2)
            e)))

;;; Are these points equal?

(define (point=? v1 v2)
  (point=?e v1 v2 equal-accuracy))

;;; Point to vector

(define (point->vect2 p)
  (make-vect2 (point-x p) (point-y p)))

;;; Make a vector from two points

(define (segment-direction->vect2 a b)
  (make-vect2 (- (point-x b) (point-x a))
              (- (point-y b) (point-y a))))

;;; Point to vector

(define (vect2->point vec)
  (make-point (vect2-u vec) (vect2-v vec)))

;;; Is equal? (with precision) for points

(define (point=~ a b precision)
  (define (test f1 f2)
    (< (abs (- f1 f2)) precision))
  (and (test (point-x a) (point-x b))
       (test (point-y a) (point-y b))))

;;; Fixnum point coordinates conversion

(define (inexact-point->exact-point p)
  (make-point (inexact->exact (round (point-x p)))
              (inexact->exact (round (point-y p)))))

;;; Flonum point coordinates conversion

(define (exact-point->inexact-point p)
  (make-point (exact->inexact (point-x p))
              (exact->inexact (point-y p))))

;;; Calculate the mid point between two points

(define (mid-point a b)
  (make-point (average (point-x a) (point-x b))
              (average (point-y a) (point-y b))))

;;; Point rotation

(define (point-rotation ref p r-angle)
  (vect2->point
    (vect2+vect2 (point->vect2 ref)
      (vect2-rotation
        (vect2-vect2 (point->vect2 p) (point->vect2 ref))
        r-angle))))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

;;; First point of segment

(define (segment-first-point seg)
  (car seg))

;;; Second point of segment

(define (segment-second-point seg)
  (cadr seg))

;;; Segment to vector

(define (segment->vect2 seg)
  (vect2-vect2
    (point->vect2 (segment-second-point seg))
    (point->vect2 (segment-first-point seg))))

;;; Segment length

(define (segment-length seg)
  (vect2-length (segment->vect2 seg)))

;;; Tell whether the point is an end point of the segment

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

;;; Tell whether the two segments are connected

(define (segments-are-connected? seg1 seg2)
  (or (is-end-point? seg2 (segment-first-point seg1))
      (is-end-point? seg2 (segment-second-point seg1))))

;;; Tell whether the segments are parallel

(define (parallel? seg1 seg2)
  (vect2=?e
    (vect2-normalize (segment->vect2 seg1))
    (vect2-normalize (segment->vect2 seg2))
    0.01))

;;; Calculate absolute point given segment and percentage

(define (point-from-relative-in-segment seg percentage)
  (let ((vec (segment->vect2 seg))
        (O (segment-first-point seg)))
    (make-point (+ (point-x O) (* (vect2-u vec) percentage))
                (+ (point-y O) (* (vect2-v vec) percentage)))))

;;; Calculate the segment's mid point

(define (segment-mid-point seg)
  (mid-point (segment-first-point seg) (segment-second-point seg)))

;-------------------------------------------------------------------------------
; Point-lists: Polygons and Paths
;-------------------------------------------------------------------------------

;;; Close a point-list (repeats first point in the last position)

(define (point-list-close plis)
  (snoc plis (car plis)))

;;; Point-list centroid

(define (point-list-centroid plis)
  (define (iter n sum plis-tail)
    (cond
     ((null? plis-tail)
      (vect2/scalar sum (exact->inexact n)))
     (else
      (iter
        (+ 1 n)
        (vect2+vect2 (point->vect2 sum) (point->vect2 (car plis-tail)))
        (cdr plis-tail)))))
  (cond
   ((null? plis)
    (error "point-list-centroid: argument #1 should be a point list"))
   (else
    (iter 0 (make-point 0.0 0.0) plis))))

;;; Point-list right-most point

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
      (error "point-list-right-most: argument #1 must be a point list")
    (iter (car plis) (cdr plis))))

;;; Is point in polygon?

;; http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
(define (point-in-polygon? point-list p)
  (define (iter c a-points b-points)
    (cond
     ((null? a-points)
      c)
     ((and (not (eq? (> (point-y (car a-points)) (point-y p))
                     (> (point-y (car b-points)) (point-y p))))
           (< (point-x p)
              (+ (/ (* (- (point-x (car b-points)) (point-x (car a-points)))
                       (- (point-y p) (point-y (car a-points))))
                    (- (point-y (car b-points)) (point-y (car a-points))))
                 (point-x (car a-points)))))
      (iter (not c) (cdr a-points) (cdr b-points)))
     (else
      (iter c (cdr a-points) (cdr b-points)))))
  (iter #f point-list (cons (last point-list) point-list)))

;;; Return a random point that is inside a given polygon

(define (random-point-in-polygon point-list)
  (define (try origin delta)
    (let ((p (make-point (+ (point-x origin)
                            (* (point-x delta) (random-real)))
                         (+ (point-y origin)
                            (* (point-y delta) (random-real))))))
      (if (point-in-polygon? point-list p)
          p
        (try origin delta))))
  (let* ((bounding-box (point-list->bounding-box point-list))
         (bb-left-corner (car bounding-box))
         (bb-right-corner (cadr bounding-box)))
    (try
      bb-left-corner
      (vect2->point
        (vect2-vect2 (point->vect2 bb-right-corner)
                     (point->vect2 bb-left-corner))))))

;;; Find a common point of two given point lists

(define (point-list-common-point? plis1 plis2)
  (find
    (lambda (e)
      (any (lambda (it) (point=? it e)) plis1))
    plis2))

;;; Calculate the tangent vector in a point-list given the relative position

(define (point-list-tangent-in-relative point-list rel)
  (if (not (= (length point-list) 2))
      (error "point-list-tangent-in-relative: only segments implemented"))
  (vect2-normalize
    (segment-direction->vect2
      (point-from-relative-in-segment point-list rel)
      (segment-second-point point-list)))) ; TODO: handle pollywalls

;-------------------------------------------------------------------------------
; Distance
;-------------------------------------------------------------------------------

;;; Calculate the distance between two points

(define (distance-point-point a b)
  (sqrt (+ (square (- (point-x a) (point-x b)))
           (square (- (point-y a) (point-y b))))))

(define (fx-distance-point-point a b)
  (##flonum.->fixnum
    (flsqrt (fixnum->flonum (fx+ (fxsquare (fx- (point-x a) (point-x b)))
                                 (fxsquare (fx- (point-y a) (point-y b))))))))

(define (fl-distance-point-point a b)
  (flsqrt (fl+ (flsquare (fl- (point-x a) (point-x b)))
               (flsquare (fl- (point-y a) (point-y b))))))

;;; Calculate the distance between point and segment

(define (distance-point-segment p sg)
  (let* ((p1x (point-x (segment-first-point sg)))
         (p1y (point-y (segment-first-point sg)))
         (p2x (point-x (segment-second-point sg)))
         (p2y (point-y (segment-second-point sg)))
         (px (point-x p))
         (py (point-y p))
         (su (- p2x p1x))
         (sv (- p2y p1y))
         (div (+ (* su su) (* sv sv)))
         (u (/ (+ (* (- px p1x) su)
                  (* (- py p1y) sv))
               div)))
    (cond
     ((> u 1)
      (set! u 1))
     ((< u 0)
      (set! u 0)))
    (let* ((x (+ p1x (* u su)))
           (y (+ p1y (* u sv)))
           (dx (- x px))
           (dy (- y py)))
      (sqrt (+ (* dx dx) (* dy dy))))))

(define (fx-distance-point-segment p sg)
  (let* ((p1x (point-x (segment-first-point sg)))
         (p1y (point-y (segment-first-point sg)))
         (p2x (point-x (segment-second-point sg)))
         (p2y (point-y (segment-second-point sg)))
         (px (point-x p))
         (py (point-y p))
         (su (fx- p2x p1x))
         (sv (fx- p2y p1y))
         (div (fx+ (fx* su su) (fx* sv sv)))
         (u (/ (fx+ (fx* (fx- px p1x) su)
                    (fx* (fx- py p1y) sv))
               div)))
    (cond
     ((> u 1)
      (set! u 1))
     ((< u 0)
      (set! u 0)))
    (let* ((x (fx+ p1x (round (* u su))))
           (y (fx+ p1y (round (* u sv))))
           (dx (fx- x px))
           (dy (fx- y py)))
      (##flonum.->fixnum (flsqrt (fixnum->flonum (fx+ (fx* dx dx) (fx* dy dy))))))))

(define (fl-distance-point-segment p sg)
  (let* ((p1x (point-x (segment-first-point sg)))
         (p1y (point-y (segment-first-point sg)))
         (p2x (point-x (segment-second-point sg)))
         (p2y (point-y (segment-second-point sg)))
         (px (point-x p))
         (py (point-y p))
         (su (fl- p2x p1x))
         (sv (fl- p2y p1y))
         (div (fl+ (fl* su su) (fl* sv sv)))
         (u (fl/ (fl+ (fl* (fl- px p1x) su)
                      (fl* (fl- py p1y) sv))
                 div)))
    (cond
     ((fl> u 1.0)
      (set! u 1.0))
     ((fl< u 0.0)
      (set! u 0.0)))
    (let* ((x (fl+ p1x (fl* u su)))
           (y (fl+ p1y (fl* u sv)))
           (dx (fl- x px))
           (dy (fl- y py)))
      (flsqrt (fl+ (fl* dx dx) (fl* dy dy))))))

;;; Calculate the distance between a point and a point list

(define (distance-point-point-list p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (distance-point-segment
           p
           (list (car plis) (cadr plis)))
         (distance-point-point-list
           p
           (cdr plis))))))

(define (fl-distance-point-point-list p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (fl-distance-point-segment
           p
           (list (car plis) (cadr plis)))
         (fl-distance-point-point-list
           p
           (cdr plis))))))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

;;; Segment-segment intersection

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

;;; Segment-polyline intersection

(define (segment-polyline-intersection seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (segment-segment-intersection seg (list (car pol-rest) (cadr pol-rest)))))
      (if (or (null-list? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
        (if (point? inters)
            (append-next (append intersections (list inters)) (cdr pol-rest))
          (append-next intersections (cdr pol-rest))))))
  (append-next '() pol))

;;; Segment-polygon (closed polyline) intersection

(define (segment-polygon-intersection seg pol)
  (segment-polyline-intersection seg (append pol (list (car pol)))))

;-------------------------------------------------------------------------------
; Bounding box
;-------------------------------------------------------------------------------

;;; Calculate the bounding point of a point-list

(define (point-list->bounding-box point-list)
  (let ((first (car point-list))
        (rest (cdr point-list)))
    (list
      (make-point (fold (lambda (point x) (min x (point-x point)))
                        (point-x first)
                        rest)
                  (fold (lambda (point y) (min y (point-y point)))
                        (point-y first)
                        rest))
      (make-point (fold (lambda (point x) (max x (point-x point)))
                        (point-x first)
                        rest)
                  (fold (lambda (point y) (max y (point-y point)))
                        (point-y first)
                        rest)))))
