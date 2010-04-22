;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometrical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
(compile-options force-compile: #t)

(import (std srfi/1))

(import math)
(import utils/misc)

;-------------------------------------------------------------------------------
; Point operations
;-------------------------------------------------------------------------------

;;; Point rotation

(define (rotation:point vec r-angle)
  (make-vect2 (- (* (vect2-x vec) (cos r-angle))
                 (* (vect2-y vec) (sin r-angle)))
              (+ (* (vect2-y vec) (cos r-angle))
                 (* (vect2-x vec) (sin r-angle)))))

;;; Point rotation

(define (rotation:point-w/reference ref p r-angle)
  (vect2:+vect2 ref
    (rotation:point
      (vect2:-vect2 p ref)
      r-angle)))

;;; Point translation

(define (translation:point p vec)
  (vect2:+vect2 p vec))

;-------------------------------------------------------------------------------
; Segments
;-------------------------------------------------------------------------------

;;; First point of segment

(define (segment:first-point seg)
  (car seg))

;;; Second point of segment

(define (segment:second-point seg)
  (cadr seg))

;;; Segment's direction vector

(define (segment:direction seg)
  (vect2:-vect2
    (segment:second-point seg)
    (segment:first-point seg)))

;;; Segment length

(define (segment:length seg)
  (vect2:magnitude (segment:direction seg)))

;;; Tell whether the point is an end point of the segment

(define (segment:is-end-point? segment point)
  (let* ((px (vect2-x point))
         (py (vect2-y point))
         (a (segment:first-point segment))
         (ax (vect2-x a))
         (ay (vect2-y a))
         (b (segment:second-point segment))
         (bx (vect2-x b))
         (by (vect2-y b)))
    (or (and
          (=~ ax px)
          (=~ ay py))
        (and
          (=~ bx px)
          (=~ by py)))))

;;; Tell whether the two segments are connected

(define (segment:connected-segment? seg1 seg2)
  (or (segment:is-end-point? seg2 (segment:first-point seg1))
      (segment:is-end-point? seg2 (segment:second-point seg1))))

;;; Tell whether the segments are parallel

(define (segment:parallel-segment? seg1 seg2)
  (vect2:=?e
    (vect2:normalize (segment:direction seg1))
    (vect2:normalize (segment:direction seg2))
    0.01))

;;; Calculate absolute point given segment and percentage

(define (segment:relative-position->point seg percentage)
  (let ((vec (segment:direction seg))
        (O (segment:first-point seg)))
    (make-vect2 (+ (vect2-x O) (* (vect2-x vec) percentage))
                (+ (vect2-y O) (* (vect2-y vec) percentage)))))

;;; Calculate the segment's mid point

(define (segment:mid-point seg)
  (let ((a (segment:first-point seg))
        (b (segment:second-point seg)))
    (make-vect2 (average (vect2-x a) (vect2-x b))
                (average (vect2-y a) (vect2-y b)))))

;-------------------------------------------------------------------------------
; Polysegments
;-------------------------------------------------------------------------------

;;; Close a point-list (repeats first point in the last position)

(define (polysegment:close plis)
  (snoc plis (car plis)))

;;; Polysegment centroid

(define (polysegment:centroid plis)
  (define (iter n sum plis-tail)
    (cond
     ((null? plis-tail)
      (vect2:/scalar sum (exact->inexact n)))
     (else
      (iter
        (+ 1 n)
        (vect2:+vect2 sum (car plis-tail))
        (cdr plis-tail)))))
  (cond
   ((null? plis)
    (error "point-list-centroid: argument #1 should be a point list"))
   (else
    (iter 0 (make-vect2 0.0 0.0) plis))))

;;; Polysegment extreme point

(define (polysegment:extreme plis f)
  (define (iter current plis-tail)
    (cond
     ((null? plis-tail)
      current)
     (else
       (iter (f current (car plis-tail)) (cdr plis-tail)))))
  (if (null-list? plis)
      (error "polysegment:extreme : argument #1 must be a point list")
    (iter (car plis) (cdr plis))))

;;; Polysegment right-most point

(define (polysegment:extreme-right plis)
  (polysegment:extreme
    plis
    (lambda (current next)
      (if (< (vect2-x current) (vect2-x next))
          next
        current))))

;;; Polysegment left-most point

(define (polysegment:extreme-left plis)
  (polysegment:extreme
    plis
    (lambda (current next)
      (if (> (vect2-x current) (vect2-x next))
          next
        current))))

;;; Polysegment top-most point

(define (polysegment:extreme-top plis)
  (polysegment:extreme
    plis
    (lambda (current next)
      (if (< (vect2-y current) (vect2-y next))
          next
        current))))

;;; Polysegment bottom-most point

(define (polysegment:extreme-bottom plis)
  (polysegment:extreme
    plis
    (lambda (current next)
      (if (> (vect2-x current) (vect2-x next))
          next
        current))))

;;; Find a common point of two given point lists

(define (polysegment:common-point? plis1 plis2)
  (find
    (lambda (e)
      (any (lambda (it) (vect2:=? it e)) plis1))
    plis2))

;;; Calculate the tangent vector in a point-list given the relative position

(define (polysegment:tangent-in-relative point-list rel)
  (if (not (= (length point-list) 2))
      (error "point-list-tangent-in-relative: only segments implemented"))
  (vect2:normalize
    (segment:direction
      (list
        (segment:relative-position->point point-list rel)
        (segment:second-point point-list))))) ; TODO: handle polysegments

;-------------------------------------------------------------------------------
; Polygons
;-------------------------------------------------------------------------------

;;; Is point in polygon?

;; http://www.ecse.rpi.edu/Homepages/wrf/Research/Short_Notes/pnpoly.html
(define (polygon:point-inside? point-list p)
  (define (iter c a-points b-points)
    (cond
     ((null? a-points)
      c)
     ((and (not (eq? (> (vect2-y (car a-points)) (vect2-y p))
                     (> (vect2-y (car b-points)) (vect2-y p))))
           (< (vect2-x p)
              (+ (/ (* (- (vect2-x (car b-points)) (vect2-x (car a-points)))
                       (- (vect2-y p) (vect2-y (car a-points))))
                    (- (vect2-y (car b-points)) (vect2-y (car a-points))))
                 (vect2-x (car a-points)))))
      (iter (not c) (cdr a-points) (cdr b-points)))
     (else
      (iter c (cdr a-points) (cdr b-points)))))
  (iter #f point-list (cons (last point-list) point-list)))

;;; Return a random point that is inside a given polygon

(define (polygon:make-random-point-inside point-list)
  (define (try origin delta)
    (let ((p (make-vect2 (+ (vect2-x origin)
                            (* (vect2-x delta) (random-real)))
                         (+ (vect2-y origin)
                            (* (vect2-y delta) (random-real))))))
      (if (polygon:point-inside? point-list p)
          p
        (try origin delta))))
  (let* ((bounding-box (polysegment:bounding-box point-list))
         (bb-left-corner (bounding-box-lefttop bounding-box))
         (bb-right-corner (bounding-box-rightbottom bounding-box)))
    (try
      bb-left-corner
      (vect2:-vect2 bb-right-corner
                    bb-left-corner))))

;-------------------------------------------------------------------------------
; Distance
;-------------------------------------------------------------------------------

;;; Calculate the distance between two points

(define (distance:point-point a b)
  (sqrt (+ (square (- (vect2-x a) (vect2-x b)))
           (square (- (vect2-y a) (vect2-y b))))))

(define (fx-distance:point-point a b)
  (##flonum.->fixnum
    (flsqrt (fixnum->flonum (fx+ (fxsquare (fx- (vect2-x a) (vect2-x b)))
                                 (fxsquare (fx- (vect2-y a) (vect2-y b))))))))

(define (fl-distance:point-point a b)
  (flsqrt (fl+ (flsquare (fl- (vect2-x a) (vect2-x b)))
               (flsquare (fl- (vect2-y a) (vect2-y b))))))

;;; Calculate the distance between point and segment

(define (distance:point-segment p sg)
  (let* ((p1x (vect2-x (segment:first-point sg)))
         (p1y (vect2-y (segment:first-point sg)))
         (p2x (vect2-x (segment:second-point sg)))
         (p2y (vect2-y (segment:second-point sg)))
         (px (vect2-x p))
         (py (vect2-y p))
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

(define (fx-distance:point-segment p sg)
  (let* ((p1x (vect2-x (segment:first-point sg)))
         (p1y (vect2-y (segment:first-point sg)))
         (p2x (vect2-x (segment:second-point sg)))
         (p2y (vect2-y (segment:second-point sg)))
         (px (vect2-x p))
         (py (vect2-y p))
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

(define (fl-distance:point-segment p sg)
  (let* ((p1x (vect2-x (segment:first-point sg)))
         (p1y (vect2-y (segment:first-point sg)))
         (p2x (vect2-x (segment:second-point sg)))
         (p2y (vect2-y (segment:second-point sg)))
         (px (vect2-x p))
         (py (vect2-y p))
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

(define (distance:point-polysegment p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (distance:point-segment
           p
           (list (car plis) (cadr plis)))
         (distance:point-polysegment
           p
           (cdr plis))))))

(define (fl-distance:point-polysegment p plis)
  (cond
   ((or (null? plis) (null? (cdr plis)))
    +inf.0)
   (else
    (min (fl-distance:point-segment
           p
           (list (car plis) (cadr plis)))
         (fl-distance:point-polysegment
           p
           (cdr plis))))))

;-------------------------------------------------------------------------------
; Intersections
;-------------------------------------------------------------------------------

;;; Segment-segment intersection

(define (intersection:segment-segment sg1 sg2)
  (let* ((a1 (car sg1))
         (a2 (cadr sg1))
         (b1 (car sg2))
         (b2 (cadr sg2))
         (ua-t (- (* (- (vect2-x b2) (vect2-x b1))
                     (- (vect2-y a1) (vect2-y b1)))
                  (* (- (vect2-y b2) (vect2-y b1))
                     (- (vect2-x a1) (vect2-x b1)))))
         (ub-t (- (* (- (vect2-x a2) (vect2-x a1))
                     (- (vect2-y a1) (vect2-y b1)))
                  (* (- (vect2-y a2) (vect2-y a1))
                     (- (vect2-x a1) (vect2-x b1)))))
         (u-b (- (* (- (vect2-y b2) (vect2-y b1))
                    (- (vect2-x a2) (vect2-x a1)))
                 (* (- (vect2-x b2) (vect2-x b1))
                    (- (vect2-y a2) (vect2-y a1))))))
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
            (make-vect2 (* (+ (vect2-x a1) ua)
                           (- (vect2-x a2) (vect2-x a1)))
                        (* (+ (vect2-y a1) ua)
                           (- (vect2-y a2) (vect2-y a1))))
          'no-intersection)))))

;;; Segment-polysegment intersection

(define (intersection:segment-polysegment seg pol)
  (define (append-next intersections pol-rest)
    (let ((inters (intersection:segment-segment seg (list (car pol-rest) (cadr pol-rest)))))
      (if (or (null-list? pol-rest) (< (length pol-rest) 3))
          (append intersections (list inters))
        (if (vect2? inters)
            (append-next (append intersections (list inters)) (cdr pol-rest))
          (append-next intersections (cdr pol-rest))))))
  (append-next '() pol))

;;; Segment-polygon (closed polysegment) intersection

(define (intersection:segment-polygon seg plis)
  (intersection:segment-polysegment seg (polysegment:close plis)))

;-------------------------------------------------------------------------------
; Bounding boxes
;-------------------------------------------------------------------------------

(define-structure bounding-box lefttop rightbottom)

;;; Calculate the bounding point of a polysegment

(define (polysegment:bounding-box point-list)
  (let ((first (car point-list))
        (rest (cdr point-list)))
    (make-bounding-box
      (make-vect2 (fold (lambda (point x) (min x (vect2-x point)))
                        (vect2-x first)
                        rest)
                  (fold (lambda (point y) (min y (vect2-y point)))
                        (vect2-y first)
                        rest))
      (make-vect2 (fold (lambda (point x) (max x (vect2-x point)))
                        (vect2-x first)
                        rest)
                  (fold (lambda (point y) (max y (vect2-y point)))
                        (vect2-y first)
                        rest)))))

;;; Calculate the diagonal segment connecting the two extremes of the bb

(define (bounding-box:diagonal-segment bb)
  (list (bounding-box-lefttop bb)
        (bounding-box-rightbottom bb)))

;;; Bounding box size segment

(define (bounding-box:size-segment bb)
  (segment:direction (bounding-box:diagonal-segment bb)))
