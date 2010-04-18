;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph analysis procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import geometry)
(import graph)
(import math)
(import utils/misc)

;-------------------------------------------------------------------------------
; Graph selection
;-------------------------------------------------------------------------------

(define (accept? graph)
  #f) ; TODO

(define (select-graph graph-list)
  (car graph-list)) ; TODO

;-------------------------------------------------------------------------------
; Finders/selectors
;-------------------------------------------------------------------------------

;;; Find walls connected to a given one

(define (find-walls-connected-to graph uid)
  (let ((wall (find-element-with-uid graph uid)))
    (define (find-walls-with-point point)
      (define (iter wall-list connected-walls)
        (if (null-list? wall-list)
            connected-walls
          (iter
            (cdr wall-list)
            (if (is-end-point? (wall->point-list (car wall-list)) point)
                (append connected-walls (list (car wall-list)))
              connected-walls))))
      (iter (graph-walls graph) '()))
    (list
      (remove (lambda (elem)
                (equal? elem wall))
              (find-walls-with-point (archpoint->point (wall-first-point wall))))
      (remove (lambda (elem)
                (equal? elem wall))
              (find-walls-with-point (archpoint->point (wall-last-point wall)))))))

;;; Find common wall

(define (room-find-common-wall rooms)
  (let ((walls-room-a (room-wall-refs (car rooms)))
        (walls-room-b (room-wall-refs (cadr rooms))))
    (define (iter lis1)
      (let ((first (car lis1)))
        (if (null-list? lis1)
            (error "find-common-wall: No common wall found")
          (if (any (lambda (elem) (equal? elem first)) walls-room-b)
              (element-uid first)
            (iter (cdr lis1))))))
    (iter walls-room-b)))

;;; Find the exterior walls

(define (graph-find-exterior-walls graph)
  (define (iter exterior-walls rest-walls)
    (cond
     ((null? rest-walls)
      exterior-walls) ; TODO: check if closed and do something about it, TODO: multiple contours
     ((exterior-wall? (car rest-walls) graph)
      (iter (cons (car rest-walls) exterior-walls) (cdr rest-walls)))
     (else
      (iter exterior-walls (cdr rest-walls)))))
  (sort-wall-list-connected graph (iter '() (graph-walls graph))))

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Is this wall exterior?

(define (exterior-wall? wall graph)
  (define (point-in-any-room? p)
    (any (lambda (room) (point-in-room? graph room p))
         (graph-rooms graph)))
  (let* ((wall-points (wall->point-list wall))
         (mid-p (point-from-relative-in-wall wall 0.5))
         (tangent-p (point-list-tangent-in-relative wall-points 0.5))
         (p1 (point-rotation mid-p (vect2+
                                     mid-p
                                     (vect2:*scalar tangent-p 10.0))
                                   pi/2))
         (p2 (point-rotation mid-p (vect2+
                                     mid-p
                                     (vect2:*scalar tangent-p 10.0))
                                   pi/-2)))
    (not (and (point-in-any-room? p1)
                            (point-in-any-room? p2)))))

;;; Is the wall described in a reverse order from a given reference?

(define (wall-is-reversed? wall point)
  (> (distance-point-point (wall->point-list point) (wall->point-list (wall-first-point wall)))
     (distance-point-point (wall->point-list point) (wall->point-list (wall-last-point wall)))))

;;; Are these walls connected?

(define (walls-are-connected? wall1 wall2)
  (segments-are-connected? ; TODO: segments to paths
    (wall->point-list wall1)
    (wall->point-list wall2)))

;;; Is point in room?

(define (point-in-room? graph room point)
  (point-in-polygon? (room->point-list graph room) point))

;-------------------------------------------------------------------------------
; Geometrical calculations
;-------------------------------------------------------------------------------

;;; Get graph limits (extreme points)

(define (graph-limit-x graph)
  500.0) ; TODO: calculate
(define (graph-limit-y graph)
  500.0) ; TODO: calculate

;;; Calculate bounding box

(define (graph-bounding-box graph)
  (point-list->bounding-box (wall-list->point-list (graph-find-exterior-walls graph))))

;;; Calculate wall mid point

(define (wall-mid-point wall)
  (let ((wall-points (wall->point-list wall)))
    (mid-point
      (segment-first-point wall-points)
      (segment-second-point wall-points))))

;;; Calculate point given wall and percentage

(define (point-from-relative-in-wall wall percentage) ; TODO: generalize to polywalls
  (point-from-relative-in-segment
    (list
      (archpoint->point (wall-point-n wall 1))
      (archpoint->point (wall-point-n wall 2)))
    percentage))

;;; Walls common point

(define (walls-common-point wall1 wall2)
  (let ((cp (point-list-common-point?
              (wall->point-list wall1)
              (wall->point-list wall2))))
    (if cp cp
      (begin
        (pp (wall->point-list wall1))
        (pp (wall->point-list wall2))
        (error "walls-common-point: given walls don't have any common point")))))

;;; Convert a list of walls into a list of points

(define (wall-list->point-list wall-list)
  (define (iter point-list rest-walls)
    ;(if (and (not (null? point-list)) (eq? (car point-list) #f)) (begin (pp point-list) (error "wall-list->point-list: Walls are not connected!")))
    (if (< (length rest-walls) 2)
        point-list
      (iter
        (cons (walls-common-point
                (car rest-walls)
                (cadr rest-walls))
              point-list)
        (cdr rest-walls))))
  (iter '() (snoc wall-list (car wall-list))))

;;; Calculate the points that enclose a room polygon as a list

(define (room->point-list graph room)
  (wall-list->point-list (room-walls graph room)))

;;; Calculate room area

(define (room-area room)
  ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
  99.9) ; TODO

;-------------------------------------------------------------------------------
; Helper procedures
;-------------------------------------------------------------------------------

;;; Sort walls in a wall list so they are connected properly

(define (sort-wall-list-connected graph wall-list) ; TODO: check if the last and the first are really connected
  (define (iter sorted remaining)
    (define (find-next first wall-list) ; (it sorts backwards)
      (cond
       ((null? wall-list)
        (pp first)
        (error "room-sort-walls: This wall cannot be connected to any other one"))
       ((walls-are-connected? (reference-to-element graph first) (reference-to-element graph (car wall-list)))
        (car wall-list))
       (else
        (find-next first (cdr wall-list)))))
    (if (null? remaining)
        sorted
      (let ((next (find-next (car sorted) remaining)))
        (iter (cons next sorted) (remove (lambda (e) (equal? e next)) remaining))))) ; (it sorts backwards)
  
  (if (null? wall-list)
      (error "sort-wall-list-connected: argument #2 (wall-list) is null")
    (iter (list (car wall-list)) (cdr wall-list))))

;;; Sort walls in a room, so they are connected

#|
(define (room-sort-walls graph room) ; TODO: check if the last and the first are really connected
;;;;; IS THIS RIGHT? ISn't sort-walls-connected better?
  (let ((walls (room-wall-refs room)))
    (define (iter sorted remaining)
      (define (find-next first wall-list) ; (it sorts backwards)
        (cond
         ((null-list? wall-list)
          (display first)(newline)
          (error "room-sort-walls: This wall cannot be connected to any other one"))
         ((walls-are-connected? (reference-to-element graph first) (reference-to-element graph (car wall-list)))
          (car wall-list))
         (else
          (find-next first (cdr wall-list)))))
      (if (null-list? remaining)
          sorted
        (let ((next (find-next (car sorted) remaining)))
          (iter (cons next sorted) (remove (lambda (e) (equal? e next)) remaining))))) ; (it sorts backwards)
    `(,(append `(room (@ (uid ,(element-uid room))))
                         (iter (list (car walls)) (cdr walls))))))
                         |#
