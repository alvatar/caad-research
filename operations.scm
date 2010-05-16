;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))

(import core/functional)
(import core/list)
(import geometry/kernel)
(import math/algebra)

(import analysis)
(import auxiliary-operations)
(import context-tree)
(import graph)

(export op:split-room)
(export op:merge-rooms)

;;; Apply operation to context

(define (apply-operation-in-context graph context new-subgraph)
  (define (matches-context? elem)
    (equal? elem context))
  (define (do-in-context graph-tail)
    (map
     (lambda (elem)
       (if (pair? elem)
           (call-with-values (lambda () (break matches-context? elem))
                             (lambda (a b)
                               (if (equal? b '()) ; If nothing found (b is null)
                                   (do-in-context elem)
                                   (append a new-subgraph (cdr b)))))
           elem))
     graph-tail))
  (car (do-in-context (list graph)))) ; Iteration must start at top level

;;; Apply operation to a graph and all contexts matching

(define (apply-operation 
          operation
          graph
          context-selector
          constraints
          operation-validator)
  (let do-until-valid ()
    (let ((new-graph (operation graph context-selector constraints)))
      (if (operation-validator new-graph)
        new-graph
        (do-until-valid)))))

;-------------------------------------------------------------------------------
; Basic operations
;-------------------------------------------------------------------------------

;;; Identity

(define (op:identity context-tree)
  (context-tree:root context-tree))

;;; Add element

(define (op:add graph element)
  (append graph `(,element))) ; TODO: branch for graph vs context-tree

;;; Remove element from graph

(define (op:remove graph element)
  (remove
    (lambda (e)
      (equal? e element))
    graph))

;;; Remove element-list from graph

(define (op:remove-multiple graph element-list)
  (remove
    (lambda (e)
      (any (lambda (e-in-element-list)
             (equal? e-in-element-list e))
           element-list))
    graph))

;;; Move

(define (op:move graph context-tree)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Topological operations
;-------------------------------------------------------------------------------

;;; Split a room

(define (op:split-room context-tree) ;graph context-selector constraints)
  (let ((graph (context-tree:first-in-level context-tree 0))
        (room (context-tree:first-in-level context-tree 1)) ; TODO: with a room list
        (walls (context-tree:level context-tree 2))
        (split-points (context-tree:level context-tree 3)))
    ;; Pick up contexts and generate uids
    (let ((first-wall (car walls)) ; TODO: wall selected with constraint
          (second-wall (cadr walls)) ;(room-wall graph (context-selector graph) 2))
          (first-split-point (car split-points)) ; (constraints (random-real)))
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
                 (element->reference first-wall)
                 (element->reference second-wall))
        (let ((graph/new-elements
                (append
                  ;; Substitution of old room by the 2 new ones
                  (apply-operation-in-context
                    graph
                    room
                    (list
                      (append `(room (@ (uid ,(make-uuid))))
                              (cdr fore)
                              (list `(wall (@ (uid ,first-wall-uid-1-half))))
                              (list `(wall (@ (uid ,new-wall-uid))))
                              (list `(wall (@ (uid ,second-wall-uid-2-half)))))
                      (append `(room (@ (uid ,(make-uuid))))
                              (cdr aft)
                              (list `(wall (@ (uid ,second-wall-uid-1-half))))
                              (list `(wall (@ (uid ,new-wall-uid))))
                              (list `(wall (@ (uid ,first-wall-uid-2-half)))))))
                  ;; Splitting wall
                  (list (pseq->wall
                          (list
                            (point-from-relative-in-wall first-wall first-split-point)
                            (point-from-relative-in-wall second-wall second-split-point))
                          new-wall-uid))
                  ;; Split touched walls at the splitting point (add 2 new ones)
                  (let ((create-first-splitted-wall (curry create-splitted-wall first-wall first-split-point)))
                    (if (pseq:is-end-point?
                          (wall-list->pseq (lreferences->lelements graph (cdr fore)))
                          (archpoint->point (wall-first-point first-wall)))
                        (create-first-splitted-wall
                          first-wall-uid-1-half
                          first-wall-uid-2-half)
                      (create-first-splitted-wall
                        first-wall-uid-2-half
                        first-wall-uid-1-half)))
                  (let ((create-second-splitted-wall (curry create-splitted-wall second-wall second-split-point)))
                    (if (pseq:is-end-point?
                          (wall-list->pseq (lreferences->lelements graph (cdr aft)))
                          (archpoint->point (wall-first-point second-wall)))
                        (create-second-splitted-wall
                          second-wall-uid-1-half
                          second-wall-uid-2-half)
                      (create-second-splitted-wall
                        second-wall-uid-2-half
                        second-wall-uid-1-half))))))
        (op:fix-room-topology
          ;; Remove touched walls
          (op:remove-multiple
            ;; Update references of rooms to old walls
            (update-wall-refs-in-rooms
              (update-wall-refs-in-rooms
                graph/new-elements
                (element-uid first-wall)
                (list
                  first-wall-uid-1-half
                  first-wall-uid-2-half))
              (element-uid second-wall)
              (list
                second-wall-uid-1-half
                second-wall-uid-2-half))
            (list
              first-wall
              second-wall))))))))

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
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Shrink

(define (op:shrink graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Post-operations
;-------------------------------------------------------------------------------

;;; Stabilize structure: add, move or replace pilars (TODO)

(define (op:stabilize-structure graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Fix accesses (TODO)

(define (op:fix-accesses graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Fix malformed graph (TODO)

(define (op:fix-malformed graph context-selector constraints)
  (remove
    (lambda (lst)
      (equal? lst '()))
    graph))

;;; Fix room topology

(define (op:fix-room-topology graph)
  (fold
    (lambda (r graph)
      (msubst*
        (sort-room-walls graph r)
        r
        graph))
    graph
    (graph-rooms graph)))

;;; Fix everything (TODO)

(define (op:fix-everything graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))
