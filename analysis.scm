;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph analysis procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import core/debug)
(import core/functional)
(import core/syntax)
(import geometry/kernel)
(import math/exact-algebra)
(import math/inexact-algebra)

(import auxiliary-operations)
(import graph)

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Is this wall exterior?

(define (exterior-wall? wall graph)
  (define (point-in-any-room? p)
    (any (lambda (room) (point-in-room? graph room p))
         (graph:find-rooms graph)))
  (let* ((wall-points (wall-pseq wall))
         (mid-p (pseq:relative-position->point wall-points 0.5))
         (tangent-p (pseq:tangent-in-relative wall-points 0.5))
         (p1 (rotation:point-w/reference mid-p
                                         (vect2+
                                          mid-p
                                          (vect2:*scalar tangent-p equal-accuracy))
                                         pi/2))
         (p2 (rotation:point-w/reference mid-p
                                         (vect2+
                                          mid-p
                                          (vect2:*scalar tangent-p equal-accuracy))
                                         pi/-2)))
    (not (and (point-in-any-room? p1)
              (point-in-any-room? p2)))))

;;; Is point in room?

(define (point-in-room? graph room point)
  (pseq:point-inside? (room->pseq graph room) point))

;-------------------------------------------------------------------------------
; Finders/selectors
;-------------------------------------------------------------------------------

;;; TODO
;; (define (find-walls/point graph point)
;;   (define (iter wall-list connected-walls)
;;     (if (null? wall-list)
;;         connected-walls
;;         (iter
;;          (cdr wall-list)
;;          (if (is-end-point? (wall->pseq (car wall-list)) point)
;;              (append connected-walls (list (car wall-list)))
;;              connected-walls))))
;;   (iter (graph:find-walls graph) '()))

;;; Find walls connected to a given one

(define (find-walls-connected/uid graph uid)
  (let ((wall (find-wall/uid graph uid)))
    (list
      (remove (lambda (elem)
                (equal? elem wall))
              (find-walls/point (archpoint->point (wall-first-point wall))))
      (remove (lambda (elem)
                (equal? elem wall))
              (find-walls/point (archpoint->point (wall-last-point wall)))))))

;;; Find longest wall in room

(define (find-longest-wall-in-room graph room)
  (let ((walls (graph:find-room-walls graph room)))
    (fold
      (lambda (w maxw)
        (if (< (pseq:~length (wall-pseq maxw))
               (pseq:~length (wall-pseq w)))
             w
             maxw))
      (car walls)
      (cdr walls))))

;;; Find common wall

(define (find-common-room-walls rooms)
  (let ((walls-room-a (room-wall-refs (car rooms)))
        (walls-room-b (room-wall-refs (cadr rooms))))
    (define (iter lis1)
      (let ((first (car lis1)))
        (if (null? lis1)
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
  (sort-wall-list-connected graph (iter '() (graph:find-walls graph))))

;-------------------------------------------------------------------------------
; Geometrical properties
;-------------------------------------------------------------------------------

;;; Calculate bounding box

(define-memoized/key-gen graph-bounding-box 
  (lambda (graph) (graph-uid graph))
  (lambda (graph)
    (pseq:bounding-box (wall-list->pseq (find-exterior-walls graph)))))

;;; External polygon

(define (analysis:graph-limits graph)
  (wall-list->pseq (find-exterior-walls graph)))

;;; Calculate the pseq that describes a list of walls
;;; TODO: reduce?
(define (wall-list->pseq wlis)
  (cond
   ((null? (cdr wlis))
    (wall-pseq (car wlis)))
   (else
    (pseq:append
      (wall-pseq (car wlis))
      (wall-list->pseq (cdr wlis))))))
      
;;; Calculate the pseq that describes a room

(define (room->pseq graph room)
  (wall-list->pseq (graph:find-room-walls graph room)))

;-------------------------------------------------------------------------------
; Geometrical operations
;-------------------------------------------------------------------------------

;;; Intersection of room and line returns a list of intersected walls and
;;; intersection points

(define (room-line-intersection graph room line)
  (let* ((walls (graph:find-room-walls graph room))
         (intersections (map
                         (lambda (w)
                           (intersection:line-segment
                            line
                            (pseq->segment (wall-pseq w))))
                         walls)))
    (unzip2
     (filter-map (lambda (p)
                   (and
                    (point? (car p))
                    (list (segment:point->relative-position
                           (pseq->segment (wall-pseq (cadr p)))
                           (car p))
                          (cadr p))))
		 (zip intersections walls)))))
   
;;; Calculate room area

(define (room-area room)
  ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
  99.9) ; TODO

;;; Calculate south from north direction

(define (north->south vec)
  (rotation:point vec pi))

;;; Calculate north-east from north direction

(define (north->north-east vec)
  (rotation:point vec pi/4))
