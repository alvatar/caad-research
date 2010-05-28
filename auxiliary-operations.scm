;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level and auxiliary operations on a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import core/debug)
(import core/list)
(import core/functional)
(import core/syntax)
(import core/debug)
(import geometry/kernel)
(import math/exact-algebra)
(import math/inexact-algebra)
(import graph)

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Are these walls connected?

(define (walls-are-connected? wall1 wall2)
  (pseq:connected-pseq?
    (wall-pseq wall1)
    (wall-pseq wall2)))

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
; Geometrical calculations
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

;;; Walls common point

(define (walls-common-point wall1 wall2)
  (aif cp (pseq:common-point?
            (wall-pseq wall1)
            (wall-pseq wall2))
       cp
       (begin (pp (wall-pseq wall1))
              (pp (wall-pseq wall2))
              (error "Given walls don't have any common point"))))

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

;; (define (room-area room)
;;   ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
;;   99.9) ; TODO

;;; Calculate south from north direction

(define (north->south vec)
  (rotation:point vec pi))

;;; Calculate north-east from north direction

(define (north->north-east vec)
  (rotation:point vec pi/4))

;-------------------------------------------------------------------------------
; Low-level manipulation of the graph
;-------------------------------------------------------------------------------

(define (create-splitted-wall wall split-point-relative uuid1 uuid2)
  (let ((split-point (pseq:relative-position->point (wall-pseq wall) split-point-relative))
        (first-point (first (wall-pseq wall)))
        (second-point (last (wall-pseq wall))))
    (list ; TODO: make as values and use properly!!!
     (make-wall uuid1
                (list first-point split-point)
                '()
                '())
     (make-wall uuid2
                (list split-point second-point)
                '()
                '()))))

;;; Update refs to walls in rooms

(define (update-wall-refs-in-rooms graph uid new-uids)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)

   (map (lambda (e)
          (if (room? e)
              (make-room
               (room-uid e)
               (msubst* new-uids uid (room-walls e))) ; TODO: Improve, no msubst*!
              e))
        (graph-architecture graph))))

;;; Try to merge into one wall if the two given are parallel

(define (try-to-merge-if-parallel-walls wall-list new-uid)
  (let ((wall-a-points (wall->pseq (car wall-list))) ; TODO: try to generalize
        (wall-b-points (wall->pseq (cadr wall-list))))
    (if (parallel? wall-a-points wall-b-points)
        (let ((first-point (if (segment:is-end-point? wall-b-points (car wall-a-points))
                               (cadr wall-a-points)
                             (car wall-a-points)))
              (second-point (if (segment:is-end-point? wall-a-points (car wall-b-points))
                                (cadr wall-b-points)
                              (car wall-b-points))))
          (list (point-list->wall
                (list first-point second-point)
                new-uid)))
        wall-list)))

;;; Break in two lists from where a wall was found
;;; Warning! This assumes that rooms contain topologically connected walls

(define (room-break graph room first-wall-uid second-wall-uid)
                                        ; TODO: check if walls are ordered
  (break (lambda (wall) (equal? second-wall-uid wall))
         (rotate-until-first
          (lambda (wall) (equal? first-wall-uid wall))
          (room-walls room))))

;;; Fix order of walls in a room

(define (sort-room-walls graph room)
  (make-room (room-uid room)
             (map (lambda (w)
                    (wall-uid w))
                  (sort-wall-list-connected graph (graph:find-room-walls graph room)))))

;;; Sort walls in a wall list so they are connected properly

(define (sort-wall-list-connected graph wall-list) ; TODO: check if the last and the first are really connected
  (define (iter sorted remaining)
    (define (find-next first wall-list) ; (it sorts backwards)
      (cond
       ((null? wall-list)
        #f)
       ((walls-are-connected? first (car wall-list))
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