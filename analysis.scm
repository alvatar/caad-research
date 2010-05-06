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
; Finders/selectors
;-------------------------------------------------------------------------------

;;; Find walls connected to a given one

(define (find-walls-connected-to graph uid)
  (let ((wall (find-element/uid graph uid)))
    (define (find-walls-with-point point)
      (define (iter wall-list connected-walls)
        (if (null-list? wall-list)
            connected-walls
          (iter
            (cdr wall-list)
            (if (is-end-point? (wall->polysegment (car wall-list)) point)
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
            (error "No common wall found")
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
  (let* ((wall-points (wall->polysegment wall))
         (mid-p (point-from-relative-in-wall wall 0.5))
         (tangent-p (polysegment:tangent-in-relative wall-points 0.5))
         (p1 (rotation:point-w/reference mid-p (vect2+
                                     mid-p
                                     (vect2:*scalar tangent-p wall-thickness))
                                   pi/2))
         (p2 (rotation:point-w/reference mid-p (vect2+
                                     mid-p
                                     (vect2:*scalar tangent-p wall-thickness))
                                   pi/-2)))
    (not (and (point-in-any-room? p1)
                            (point-in-any-room? p2)))))

;;; Is the wall described in a reverse order from a given reference?

(define (wall-is-reversed? wall point)
  (> (distance-point-point (wall->polysegment point) (wall->polysegment (wall-first-point wall)))
     (distance-point-point (wall->polysegment point) (wall->polysegment (wall-last-point wall)))))

;;; Are these walls connected?

(define (walls-are-connected? wall1 wall2)
  (segment:connected-segment? ; TODO: segments to paths
    (wall->polysegment wall1)
    (wall->polysegment wall2)))

;;; Is point in room?

(define (point-in-room? graph room point)
  (polygon:point-inside? (room->point-list graph room) point))

;-------------------------------------------------------------------------------
; Geometrical calculations
;-------------------------------------------------------------------------------

;;; Calculate bounding box

(define (graph-bounding-box graph)
  (polysegment:bounding-box (wall-list->polysegment (graph-find-exterior-walls graph))))

;;; Calculate wall mid point

(define (wall-mid-point wall)
  (let ((wall-points (wall->polysegment wall)))
    (mid-point
      (segment:first-point wall-points)
      (segment:second-point wall-points))))

;;; Calculate point given wall and percentage

(define (point-from-relative-in-wall wall percentage) ; TODO: generalize to polywalls
  (segment:relative-position->point
    (list
      (archpoint->point (wall-point-n wall 1))
      (archpoint->point (wall-point-n wall 2)))
    percentage))

;;; Walls common point

(define (walls-common-point wall1 wall2)
  (aif cp (polysegment:common-point?
            (wall->polysegment wall1)
            (wall->polysegment wall2))
       cp
    (begin
      (pp (wall->polysegment wall1))
      (pp (wall->polysegment wall2))
      (error "Given walls don't have any common point"))))

;;; Convert a list of walls into a polysegment

(define (wall-list->polysegment wlis)
  (cond
   ((null? (cdr wlis))
    (wall->polysegment (car wlis)))
   (else
    (polysegment:append
      (wall->polysegment (car wlis))
      (wall-list->polysegment (cdr wlis))))))
      
;;; Calculate the points that enclose a room polygon as a list

(define (room->point-list graph room)
  (wall-list->polysegment (room-walls graph room))) ; First point because it's equal to last

;;; Calculate room area

(define (room-area room)
  ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
  99.9) ; TODO

;;; Calculate south from north direction

(define (north->south vec)
  (rotation:point vec pi))

;;; Calculate north-east from north direction

(define (north->north-east vec)
  (rotation:point vec (/ pi 4.0)))

;-------------------------------------------------------------------------------
; Helper procedures
;-------------------------------------------------------------------------------

;;; Sort walls in a wall list so they are connected properly

(define (sort-wall-list-connected graph wall-list) ; TODO: check if the last and the first are really connected
  (define (iter sorted remaining)
    (define (find-next first wall-list) ; (it sorts backwards)
      (cond
       ((null? wall-list)
        #f)
       ((walls-are-connected? (reference->element graph first) (reference->element graph (car wall-list)))
        (car wall-list))
       (else
        (find-next first (cdr wall-list)))))
    (if (null? remaining)
        sorted
      (aif next (find-next (car sorted) remaining)
        (iter (cons next sorted) (remove (lambda (e) (equal? e next)) remaining))
        (begin
          (display "----------\n")
          (pp sorted)
          (display "----------\n")
          (pp remaining)
          (error "sort-walls-connected -- This wall cannot be connected to any other")))))
  
  (if (null? wall-list)
      (error "Argument #2 (wall-list) is null")
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
         ((walls-are-connected? (reference->element graph first) (reference->element graph (car wall-list)))
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
