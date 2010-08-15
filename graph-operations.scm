;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level and auxiliary operations on a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
;(compile-options force-compile: #t)

(import (std srfi/1))
(import core/list
        core/functional
        core/syntax
        core/debugging
        geometry/kernel
        math/exact-algebra
        math/inexact-algebra
        graph)

(%activate-checks)

;-------------------------------------------------------------------------------
; Selectors
;-------------------------------------------------------------------------------

(define (graph:find.rooms g)
  (filter (lambda (e) (room? e)) (graph-architecture g)))

(define (graph:find.walls g)
  (filter (lambda (e) (wall? e)) (graph-architecture g)))

(define (graph:find.structurals g)
  (filter (lambda (e) (structural? e)) (graph-architecture g)))

(define (graph:find.entries g)
  (filter (lambda (e) (entry? e)) (graph-architecture g)))

(define (graph:find.pipes g)
  (filter (lambda (e) (pipe? e)) (graph-architecture g)))

(define (graph:find.windows g)
  (reduce append '() (map (lambda (w) (wall-windows w)) (graph:find.walls g))))

(define (graph:find.doors g)
  (reduce append '() (map (lambda (w) (wall-doors w)) (graph:find.walls g))))

(define (graph:find.wall/uid graph uid)
  (aif element (find
                (lambda (e) (and (wall? e) (equal? uid (wall-uid e))))
                (graph-architecture graph))
       element
       (begin (display "UID: ")(display uid)(newline)
              (error "Wall with such UID not found"))))

;;; TODO: memoize?
(define (graph:find.room-walls graph room)
  (map (lambda (r) (graph:find.wall/uid graph r)) (room-walls room)))

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Are these walls connected?

(define (graph:walls-are-connected? wall1 wall2)
  (pseq:connected-pseq?
   (wall-pseq wall1)
   (wall-pseq wall2)))

;;; Is this wall exterior?

(define (graph:exterior-wall? wall graph)
  (define (point-in-any-room? p)
    (any (lambda (room) (graph:point-in-room? graph room p))
         (graph:find.rooms graph)))
  (let* ((wall-points (wall-pseq wall))
         (mid-p (pseq:relative-position->point wall-points 0.5))
         (tangent-p (pseq:tangent-in-relative wall-points 0.5))
         (p1 (rotate.point-w/reference mid-p
                                         (vect2+
                                          mid-p
                                          (vect2:*scalar tangent-p equal-accuracy))
                                         pi/2))
         (p2 (rotate.point-w/reference mid-p
                                         (vect2+
                                          mid-p
                                          (vect2:*scalar tangent-p equal-accuracy))
                                         -pi/2)))
    (not (and (point-in-any-room? p1)
              (point-in-any-room? p2)))))

;;; Is point in room?

(define (graph:point-in-room? graph room point)
  (pseq:point-inside? (graph:room->pseq graph room) point))

;;; Is the wall of this room?

(define (graph:room-wall-uid? room wall-uid)
  (%accept (string? wall-uid) "this doesn't look like a UUID")
  (find (lambda (wuid) (equal? wuid wall-uid)) (room-walls room)))

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

;;; Find walls to a given one

(define (graph:find.walls-connected-to graph wall)
  (let ((wallp (wall-pseq wall))
        (inspected-walls (remove
                          (lambda (w) (equal? wall w))
                          (graph:find.walls graph))))
    (values
     (filter (lambda (w) (pseq:is-end-point? (wall-pseq w) (first wallp))) inspected-walls)
     (filter (lambda (w) (pseq:is-end-point? (wall-pseq w) (last wallp))) inspected-walls))))

;;; Find longest wall in room

(define (graph:find.longest-wall-in-room graph room)
  (let ((walls (graph:find.room-walls graph room)))
    (fold
      (lambda (w maxw)
        (if (< (pseq:~length (wall-pseq maxw))
               (pseq:~length (wall-pseq w)))
             w
             maxw))
      (car walls)
      (cdr walls))))

;;; Find common wall

(define (graph:find.common-room-walls room-a room-b)
  (filter
   (lambda (a) (any (lambda (b) (equal? a b)) (room-walls room-b)))
   (room-walls room-a)))

;;; Find the exterior walls

(define (graph:find.exterior-walls graph)
  (define (iter exterior-walls rest-walls)
    (cond
     ((null? rest-walls)
      exterior-walls) ; TODO: check if closed and do something about it, TODO: multiple contours
     ((graph:exterior-wall? (car rest-walls) graph)
      (iter (cons (car rest-walls) exterior-walls) (cdr rest-walls)))
     (else
      (iter exterior-walls (cdr rest-walls)))))
  (graph:sort.wall-list-connected graph (iter '() (graph:find.walls graph))))

;-------------------------------------------------------------------------------
; Geometrical calculations
;-------------------------------------------------------------------------------

;;; Calculate bounding box

(define-memoized/key-gen graph:bounding-box 
  (lambda (graph) (graph-uid graph))
  (lambda (graph)
    (pseq->bbox (graph:wall-list->pseq (graph:find.exterior-walls graph)))))

;;; External polygon extraction

(define (graph:limits graph)
  (graph:wall-list->pseq (graph:find.exterior-walls graph)))

;;; Total area of the graph

(define (graph:total-area graph)
  (pseq:area (graph:limits graph)))

;;; Calculate the pseq that describes a list of walls

(define (graph:wall-list->pseq wlis)
  (cond
   ((null? (cdr wlis))
    (wall-pseq (car wlis)))
   (else
    (pseq:append
      (wall-pseq (car wlis))
      (graph:wall-list->pseq (cdr wlis))))))


;;; Calculate a wall's perpendicular through a point

(define (graph:wall-perpendicular wall #!optional p)
  (if p
      (error "unimplemented with perpendicular-through-point")
      (direction:perpendicular
       (segment->direction
        (pseq->segment
         (wall-pseq
          wall))))))

;;; Calculate the closest wall to a point

(define (graph:closest-wall graph point)
  (min/generator (lambda (w)
                   (~distance.point-pseq point (wall-pseq w)))
                 (graph:find.walls graph)))

;;; Calculate the pseq that describes a room

(define (graph:room->pseq graph room)
  (graph:wall-list->pseq (graph:find.room-walls graph room)))

;;; Walls common point

(define (graph:walls-common-point wall1 wall2)
  (aif cp (pseq:common-point?
            (wall-pseq wall1)
            (wall-pseq wall2))
       cp
       (begin (pp (wall-pseq wall1))
              (pp (wall-pseq wall2))
              (error "Given walls don't have any common point"))))

;;; Intersection of room and line returns a list of intersected walls and
;;; intersection points
;;; Output: 2 values of the same size (walls and intersections)

(define (graph:room-relative-line-intersections graph room line)
  (let* ((walls (graph:find.room-walls graph room))
         (intersections (map
                         (lambda (w)
                           (intersection.line-segment
                            line
                            (pseq->segment (wall-pseq w))))
                         walls)))
    (unzip2
     (filter-map (lambda (e)
                   (let ((wall (car e))
                         (intersection (cadr e)))
                    (and
                     (point? intersection)
                     (list wall
                           (segment:point->relative-position
                            (pseq->segment (wall-pseq wall))
                            intersection)))))
                 (zip walls intersections)))))

;;; Returns all the intersections of a line with the graph
;;; Output: 3 values of the same size (rooms, walls and intersections)

;;; TODO: this functions should be rethought, it's output is not useful like this

(define (graph:relative-line-intersections graph line)
  (fold/values
   (lambda (room rooms walls intersections)
     (receive (new-wall new-intr)
              (graph:room-relative-line-intersections graph room line)
              (values (if (not-null? new-wall) (append rooms room) rooms)
                      (append walls new-wall)
                      (append intersections new-intr))))
   '(() () ())
   (graph:find.rooms graph)))

;;; Calculate room area

(define (graph:room-area graph room)
  (pseq:area (graph:room->pseq graph room)))

;;; Calculate room aspect ratio

(define (graph:room-aspect-ratio graph room)
  (let ((bbxsg (bbox:size-segment
                (pseq->bbox (graph:room->pseq graph room)))))
    (max (vect2:x/y bbxsg)
         (vect2:y/x bbxsg))))

;;; Calculate south from north direction

(define (graph:north->south vec)
  (rotate.point vec pi))

;;; Calculate north-east from north direction

(define (graph:north->north-east vec)
  (rotate.point vec -pi/4))

;;; Calculate east from north direction

(define (graph:north->east vec)
  (rotate.point vec -pi/2)) ; TODO: perpendicular

;-------------------------------------------------------------------------------
; Graph modification
;-------------------------------------------------------------------------------

;;; Create a two new walls where one was before, given a splitting point

(define (graph:split-wall wall split-point-relative uuid1 uuid2)
  (let ((split-point (pseq:relative-position->point (wall-pseq wall) split-point-relative))
        (first-point (first (wall-pseq wall)))
        (second-point (last (wall-pseq wall))))
    (values
     (make-wall-plain uuid1
                      (list first-point split-point))
     (make-wall-plain uuid2
                      (list split-point second-point)))))

;;; Update refs to walls in rooms

(define (graph:update-wall-refs-in-rooms graph uid new-uids)
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

;;; Try and merge into one wall if the two given are parallel

(define (graph:try-to-merge-if-parallel-walls wall-list new-uid)
  (let ((wall-a-points (wall-pseq (car wall-list))) ; TODO: generalize for more than 2
        (wall-b-points (wall-pseq (cadr wall-list))))
    (if (pseq:parallel-pseq? wall-a-points wall-b-points)
        (let ((first-point (if (pseq:is-end-point? wall-b-points (car wall-a-points))
                               (cadr wall-a-points)
                               (car wall-a-points)))
              (second-point (if (pseq:is-end-point? wall-a-points (car wall-b-points))
                                (cadr wall-b-points)
                                (car wall-b-points))))
          (list (make-wall-plain
                 new-uid
                 (list first-point second-point))))
        wall-list)))

;;; Break in two lists from where a wall was found
;;; Warning! This assumes that rooms contain topologically connected walls

(define (graph:room-break graph room first-wall-uid second-wall-uid)
                                        ; TODO: check if walls are ordered
  (break (lambda (wall) (equal? second-wall-uid wall))
         (find-rotate
          (lambda (wall) (equal? first-wall-uid wall))
          (room-walls room))))

;;; Fix order of walls in a room

(define (graph:sort.room-walls graph room)
  (make-room (room-uid room)
             (map (lambda (w)
                    (wall-uid w))
                  (graph:sort.wall-list-connected graph (graph:find.room-walls graph room)))))

;;; Sort walls in a wall list so they are connected properly

(define (graph:sort.wall-list-connected graph wall-list) ; TODO: check if the last and the first are really connected
  (define (iter sorted remaining)
    (define (find-next first wall-list) ; (it sorts backwards)
      (cond
       ((null? wall-list)
        #f)
       ((graph:walls-are-connected? first (car wall-list))
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
          (error "graph:sort.walls-connected -- This wall cannot be connected to any other")))))
  
  (if (null? wall-list)
      (error "Argument #2 (wall-list) is null")
    (iter (list (car wall-list)) (cdr wall-list))))
