
;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (compile-options force-compile: #t)

(import (std srfi/1
             srfi/11
             misc/uuid))
(import context
        core/functional
        core/list
        core/debugging
        ds/n-ary
        geometry/kernel
        math/exact-algebra
        graph-operations
        graph)

(%activate-checks)

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
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (remove (lambda (e)
             (equal? e element))
           (graph-architecture graph))))

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
  (error "unimplemented op:move"))

;;; Rename element

(define (op:rename graph room name)  ; TODO: generalize to any context
  (op:add
   (op:remove graph room)
   (make-room name
              (room-walls room))))

;-------------------------------------------------------------------------------
; Topological operations
;-------------------------------------------------------------------------------

;;; The generic CUT operation: given a graph and a set of points with a minimum
;;; length of 2, first select the 2 walls where those first and last points lay
;;; in case they are not given. Then split the room if they belong to the same
;;; one and the path doesn't intersect any other wall.

(define (op:cut context #!optional points)
  (define (cut-room-opposed graph room walls split-points) ;graph context-selector constraints)
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
               (graph:room-break
                graph
                room
                (wall-uid first-wall)
                (wall-uid second-wall))
               (let-values (((splitted-wall-1a splitted-wall-1b)
                             (let ((create-walls (curry graph:split-wall
                                                        first-wall
                                                        first-split-point)))
                               (if (pseq:is-end-point?
                                    (graph:wall-list->pseq (map (lambda (u) (graph:find.wall/uid graph u)) (cdr fore)))
                                    (first (wall-pseq first-wall)))
                                   (create-walls first-wall-uid-1-half first-wall-uid-2-half)
                                   (create-walls first-wall-uid-2-half first-wall-uid-1-half))))
                            ((splitted-wall-2a splitted-wall-2b)
                             (let ((create-walls (curry graph:split-wall
                                                        second-wall
                                                        second-split-point)))
                               (if (pseq:is-end-point?
                                    (graph:wall-list->pseq (map (lambda (u) (graph:find.wall/uid graph u)) (cdr aft)))
                                    (first (wall-pseq second-wall)))
                                   (create-walls second-wall-uid-1-half second-wall-uid-2-half)
                                   (create-walls second-wall-uid-2-half second-wall-uid-1-half)))))
                 (let
                     ((graph/new-elements
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
                          ,(make-wall-plain
                            new-wall-uid
                            (list
                             (pseq:relative-position->point (wall-pseq first-wall) first-split-point)
                             (pseq:relative-position->point (wall-pseq second-wall) second-split-point)))
                          ;; Split touched walls at the splitting point (add 2 new ones)
                          ,splitted-wall-1a
                          ,splitted-wall-1b
                          ,splitted-wall-2a
                          ,splitted-wall-2b))))
                   (op:fix-wall-order
                    ;; Remove touched walls
                    (op:remove-multiple
                     ;; Update references of rooms to old walls
                     (graph:update-wall-refs-in-rooms
                      (graph:update-wall-refs-in-rooms
                       graph/new-elements
                       (wall-uid first-wall)
                       (list first-wall-uid-1-half
                             first-wall-uid-2-half))
                      (wall-uid second-wall)
                      (list  second-wall-uid-1-half
                             second-wall-uid-2-half))
                     (list first-wall
                           second-wall))))))))
  
  (if (< (n-ary:depth context) 2)       ; no information about walls
      (error "unimplemented op:cut detection of walls")
      (let ((graph (n-ary:level context 0))
            (rooms (car (n-ary:level context 1))) ; TODO: LEVEL IS REGISTERED IN n-ary:level??
            (walls (car (n-ary:level context 2))) ; TODO
            (split-points (flatten (n-ary:level context 3)))) ; TODO
        (let ((wall1 (car walls))
              (wall2 (cadr walls)))
          (cond
           ((equal? wall1 wall2)        ; same wall
            (error "same wall splitting is not implemented"))
           ((graph:walls-are-connected? wall1 wall2) ; connected walls
            (error "connected walls splitting is not implemented"))
           (else                        ; opposing walls
            (cut-room-opposed graph
                              rooms
                              walls
                              split-points)))))))

;;; Merge two rooms

(define (op:merge-rooms context)
  (let ((graph (n-ary:level context 0))
        (rooms (n-ary:level context 1)))
    (%deny (< (length rooms) 2) "The number of rooms is less than 2")
    (let* ((room-a (car rooms))
           (room-b (cadr rooms))
           (common-wall-uid (car (graph:find.common-room-walls room-a room-b)))) ; TODO: consider when more than one wal is common
      (let-values (((wall-bifurcations-1 wall-bifurcations-2)
                    (apply/values
                     (lambda (wall-list)
                       (filter (lambda (w) (or (graph:room-wall-uid? room-a (wall-uid w))
                                          (graph:room-wall-uid? room-b (wall-uid w))))
                               wall-list))
                     (graph:find.walls-connected-to graph
                                                    (graph:find.wall/uid graph
                                                                         common-wall-uid)))))
        (error "AQUI")
        (let ((possible-uid-1 (make-uuid))
              (possible-uid-2 (make-uuid))
              (touched-walls-a (graph:try-and-merge-if-parallel-walls (car wall-bifurcations) possible-uid-1))
              (touched-walls-b (graph:try-and-merge-if-parallel-walls (cadr wall-bifurcations) possible-uid-2)))

          (step))))

  
  #;(let* ((merged-rooms (context-selector graph))
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
          (remove-element               ; Remove common wall always
           (update-walls ; Update touched walls depending on merging success
            (update-walls               ; Idem
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

;;; Snap walls to closest structure

(define (op:snap-walls/structure context #!optional threshold)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Error correction
;-------------------------------------------------------------------------------

;;; Fix wall order in all rooms

(define (op:fix-wall-order graph)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (map (lambda (e)
          (if (room? e)
              (graph:sort.room-walls graph e)
              e))
        (graph-architecture graph))))

;;; Fix walls connectivity

(define (op:fix-wall-connections context)
  (error "unimplemented"))