


;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(compile-options force-compile: #t)

(import (std srfi/1))
(import (std misc/uuid))

(import context)
(import core/functional)
(import core/list)
(import geometry/kernel)
(import math/exact-algebra)
(import graph-operations)
(import graph)

(export op:split-room)
(export op:merge-rooms)

;-------------------------------------------------------------------------------
; Basic operations
;-------------------------------------------------------------------------------

;;; Identity

(define (op:identity context-tree)
  (context-tree:root context-tree))

;;; Add element

(define (op:add graph element)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (cons element (graph-architecture graph))))

;;; Remove element from graph

(define (op:remove graph element)
  (remove
    (lambda (e)
      (equal? e element))
    graph))

;;; Remove element-list from graph

(define (op:remove-multiple graph le)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (remove (lambda (e)
             (any (lambda (e2) (equal? e2 e)) le))
           (graph-architecture graph))))

;;; Move

(define (op:move graph context-tree)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Topological operations
;-------------------------------------------------------------------------------

;;; The generic CUT operation: given a graph and a set of points with a minimum
;;; length of 2, first select the 2 walls where those first and last points lay.
;;; Then split the room if they belong to the same one and the path doesn't
;;; intersect any other wall.

(define (op:cut context-tree)
  (error "unimplemented cut"))

;;; Split a room

(define (op:split-room context-tree) ;graph context-selector constraints)
  (let ((graph (context-tree:first-in-level context-tree 0))
        (room (context-tree:first-in-level context-tree 1)) ; TODO: with a room list
        (walls (context-tree:level context-tree 2))
        (split-points (context-tree:level context-tree 3)))
    ;; Pick up contexts and generate uids
    (let ((first-wall (car walls))
          (second-wall (cadr walls))
          (first-split-point (car split-points))
          (second-split-point (cadr split-points))
          (new-wall-uid (make-uuid))
          (first-wall-uid-1-half (make-uuid))
          (first-wall-uid-2-half (make-uuid))
          (second-wall-uid-1-half (make-uuid))
          (second-wall-uid-2-half (make-uuid)))
      (receive (fore aft)
               (room-break
                graph
                room
                (wall-uid first-wall)
                (wall-uid second-wall))
               (let ((graph/new-elements
                      (make-graph
                       (make-uuid)
                       (graph-environment graph)
                       `(
                         ;; Substitution of old room by the 2 new ones
                         ,@(remove (lambda (e) (equal? e room)) (graph-architecture graph))
                         ,(make-room (make-uuid)
                                     `(,@(cdr fore)
                                       ,first-wall-uid-1-half
                                       ,new-wall-uid
                                       ,second-wall-uid-2-half))
                         ,(make-room (make-uuid)
                                     `(,@(cdr aft)
                                       ,second-wall-uid-1-half
                                       ,new-wall-uid
                                       ,first-wall-uid-2-half))
                         ;; Splitting wall
                         ,(make-wall
                           new-wall-uid
                           (list
                            (pseq:relative-position->point (wall-pseq first-wall) first-split-point)
                            (pseq:relative-position->point (wall-pseq second-wall) second-split-point))
                           '()
                           '())
                         ;; Split touched walls at the splitting point (add 2 new ones)
                         ,@(let ((create-first-splitted-wall (curry create-splitted-wall first-wall first-split-point)))
                             (if (pseq:is-end-point?
                                  (wall-list->pseq (map (lambda (u) (graph:find-wall/uid graph u)) (cdr fore)))
                                  (first (wall-pseq first-wall)))
                                 (create-first-splitted-wall first-wall-uid-1-half first-wall-uid-2-half)
                                 (create-first-splitted-wall first-wall-uid-2-half first-wall-uid-1-half)))
                         ,@(let ((create-second-splitted-wall (curry create-splitted-wall second-wall second-split-point)))
                             (if (pseq:is-end-point?
                                  (wall-list->pseq (map (lambda (u) (graph:find-wall/uid graph u)) (cdr aft)))
                                  (first (wall-pseq second-wall)))
                                 (create-second-splitted-wall second-wall-uid-1-half second-wall-uid-2-half)
                                 (create-second-splitted-wall second-wall-uid-2-half second-wall-uid-1-half)))))))
                 (op:fix-room-topology
                  ;; Remove touched walls
                 (op:remove-multiple
                   ;; Update references of rooms to old walls
                   (update-wall-refs-in-rooms
                    (update-wall-refs-in-rooms
                     graph/new-elements
                     (wall-uid first-wall)
                     (list first-wall-uid-1-half
                           first-wall-uid-2-half))
                    (wall-uid second-wall)
                    (list  second-wall-uid-1-half
                           second-wall-uid-2-half))
                   (list first-wall
                         second-wall)
                   )
                  )
                 )))))

;;; Merge two rooms

(define (op:merge-rooms graph context-selector constraints)
  (if (null-list? (context-selector graph)) ; TODO: TEMP!!!
      graph
      
  (let* ((merged-rooms (context-selector graph))
         (room-a (car merged-rooms))
         (room-b (cadr merged-rooms))
         (common-wall-uid (room-find-common-wall merged-rooms))
         (wall-bifurcations (find-walls-connected/uid graph common-wall-uid)) ; TODO: filter out walls that don't belong to either room
         (possible-uid-1 (make-uuid))
         (possible-uid-2 (make-uuid))
         (touched-walls-a (try-to-merge-if-parallel-walls (car wall-bifurcations) possible-uid-1))
         (touched-walls-b (try-to-merge-if-parallel-walls (cadr wall-bifurcations) possible-uid-2))
         (rest-of-walls-a (remove-elements (room-wall-refs room-a) (append 
                                                                     (make-refs-from-elements (car wall-bifurcations))
                                                                     (make-refs-from-elements (cadr wall-bifurcations))
                                                                     `(,(make-ref-from-uid 'wall common-wall-uid)))))
         (rest-of-walls-b (remove-elements (room-wall-refs room-b) (append
                                                                     (make-refs-from-elements (car wall-bifurcations))
                                                                     (make-refs-from-elements (cadr wall-bifurcations))
                                                                     `(,(make-ref-from-uid 'wall common-wall-uid)))))
         (update-walls (lambda (graph2 stay leave)
           (if (= (length stay) 2) ; did merging work?
               graph2 ; It didn't so don't transform the touched walls
             (add-element
               (remove-elements
                 graph2
                 leave)
               (car stay)))))
         (graph-with-changed-walls
           (remove-element ; Remove common wall always
             (update-walls ; Update touched walls depending on merging success
               (update-walls ; Idem
                 graph
                 touched-walls-a
                 (car wall-bifurcations))
               touched-walls-b
               (cadr wall-bifurcations))
             (find-element/uid graph common-wall-uid))))
    (apply-operation-in-context
             (apply-operation-in-context
               graph-with-changed-walls
               (car merged-rooms)
               (room-sort-walls
                 graph-with-changed-walls
                   (append `(room (@ (uid ,(make-uuid))))
                           rest-of-walls-a
                           (make-refs-from-elements touched-walls-a)
                           rest-of-walls-b
                           (make-refs-from-elements touched-walls-b))))
             (cadr merged-rooms)
             '()))))

;-------------------------------------------------------------------------------
; Boundary modifications
;-------------------------------------------------------------------------------

;;; Expand

(define (op:expand graph context-selector constraints)
  (error "unimplemented"))

;;; Shrink

(define (op:shrink graph context-selector constraints)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Post-operations
;-------------------------------------------------------------------------------

;;; Stabilize structure: add, move or replace pilars (TODO)

(define (op:stabilize-structure graph context-selector constraints)
  (error "unimplemented"))

;;; Fix accesses (TODO)

(define (op:fix-accesses graph context-selector constraints)
  (error "unimplemented"))

;;; Fix malformed graph (TODO)

(define (op:fix-malformed graph context-selector constraints)
  (error "unimplemented"))

;;; Fix room topology

(define (op:fix-room-topology graph)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (map (lambda (e)
          (if (room? e)
              (sort-room-walls graph e)
              e))
        (graph-architecture graph))))

;;; Fix everything (TODO)

(define (op:fix-everything graph context-selector constraints)
  (error "unimplemented"))
