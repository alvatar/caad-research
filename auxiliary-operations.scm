;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level and auxiliary operations on a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import geometry)
(import graph)
(import math)
(import utils/misc)

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Is the wall described in a reverse order from a given reference?

(define (wall-is-reversed? wall point)
  (> (distance-point-point (wall->polysegment point) (wall->polysegment (wall-first-point wall)))
     (distance-point-point (wall->polysegment point) (wall->polysegment (wall-last-point wall)))))

;;; Are these walls connected?

(define (walls-are-connected? wall1 wall2)
  (segment:connected-segment? ; TODO: segments to paths
    (wall->polysegment wall1)
    (wall->polysegment wall2)))

;-------------------------------------------------------------------------------
; Geometrical calculations
;-------------------------------------------------------------------------------

;;; Calculate point given wall and percentage

(define (point-from-relative-in-wall wall percentage) ; TODO: generalize to polywalls
  (segment:relative-position->point
    (list
      (archpoint->point (wall-point-n wall 1))
      (archpoint->point (wall-point-n wall 2)))
    percentage))

;;; Calculate wall mid point

(define (wall-mid-point wall)
  (let ((wall-points (wall->polysegment wall)))
    (mid-point
      (segment:first-point wall-points)
      (segment:second-point wall-points))))

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

;-------------------------------------------------------------------------------
; Low-level manipulation of the graph
;-------------------------------------------------------------------------------

;;; Create 2 walls splitting one in a point

(define (create-splitted-wall wall split-point-relative uuid1 uuid2)
  (let ((split-point (point-from-relative-in-wall wall split-point-relative))
        (first-point (wall-first-point wall))
        (second-point (wall-last-point wall)))
  `((wall (@ (uid ,uuid1))
         (pt (@ (y ,(number->string (archpoint-coord 'y first-point)))
                (x ,(number->string (archpoint-coord 'x first-point)))))
         (pt (@ (y ,(number->string (vect2-y split-point)))
                (x ,(number->string (vect2-x split-point))))))
   (wall (@ (uid ,uuid2))
         (pt (@ (y ,(number->string (vect2-y split-point)))
                (x ,(number->string (vect2-x split-point)))))
         (pt (@ (y ,(number->string (archpoint-coord 'y second-point)))
                (x ,(number->string (archpoint-coord 'x second-point)))))))))

;;; Update refs to doors in rooms

(define (update-wall-refs-in-rooms graph uid new-uids)
  (msubst*
    (map (lambda (u) (uid->reference 'wall u)) new-uids)
    `(wall (@ (uid ,uid)))
    graph))

;;; Try to merge into one wall if the two given are parallel

(define (try-to-merge-if-parallel-walls wall-list new-uid)
  (let ((wall-a-points (wall->point-list (car wall-list))) ; TODO: try to generalize
        (wall-b-points (wall->point-list (cadr wall-list))))
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
  (break (lambda (wall) (equal? second-wall-uid (element-uid wall)))
         (rotate-until-first
           (lambda (wall) (equal? first-wall-uid (element-uid wall)))
           (room-wall-refs room))))

;;; Fix order of walls in a room

(define (sort-room-walls graph room)
  `((room (@ (uid ,(element-uid room)))
          ,@(lelements->lreferences (sort-wall-list-connected graph (room-walls graph room))))))

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