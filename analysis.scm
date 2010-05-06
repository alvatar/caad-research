;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph analysis procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import auxiliary-operations)
(import geometry)
(import graph)
(import math)
(import utils/misc)

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
                                     (vect2:*scalar tangent-p equal-accuracy))
                                   pi/2))
         (p2 (rotation:point-w/reference mid-p (vect2+
                                     mid-p
                                     (vect2:*scalar tangent-p equal-accuracy))
                                   pi/-2)))
    (not (and (point-in-any-room? p1)
                            (point-in-any-room? p2)))))

;;; Is point in room?

(define (point-in-room? graph room point)
  (polygon:point-inside? (room->point-list graph room) point))

;-------------------------------------------------------------------------------
; Finders/selectors
;-------------------------------------------------------------------------------

;;; Find walls connected to a given one

(define (find-walls-connected/uid graph uid)
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

(define (find-common-room-walls rooms)
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

(define (find-exterior-walls graph)
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
; Geometrical properties
;-------------------------------------------------------------------------------

;;; Calculate bounding box

(define (graph-bounding-box graph)
  (polysegment:bounding-box (wall-list->polysegment (find-exterior-walls graph))))

;;; Calculate the polysegment that describes a list of walls

(define (wall-list->polysegment wlis)
  (cond
   ((null? (cdr wlis))
    (wall->polysegment (car wlis)))
   (else
    (polysegment:append
      (wall->polysegment (car wlis))
      (wall-list->polysegment (cdr wlis))))))
      
;;; Calculate the polysegment that describes a room

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
