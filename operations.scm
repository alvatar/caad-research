;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import context-tree)
(import geometry)
(import graph)
(import utils/misc)

(export op-split-room)
(export op-merge-rooms)

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

(define (op-identity graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   (context-selector graph)))

;;; Add element

(define (op-add graph context-selector constraints) ; TODO: This should be an operation
  '())

;;; Remove

(define (op-remove graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Move

(define (op-move graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Topology modifications
;-------------------------------------------------------------------------------

;;; Split a room

(define (op-split-room context-tree) ;graph context-selector constraints)
  (let* ((graph (context-tree:level context-tree 0))
         (room (car (context-tree:level context-tree 1))) ; TODO: room list ;(context-selector graph))
         (walls (context-tree:level context-tree 2))
         (split-points (context-tree:level context-tree 3))
         ;; pick up contexts
         (first-wall (car walls)) ;(room-wall graph (context-selector graph) 0)) ; TODO: wall selected with constraint
         (second-wall (cadr walls)) ;(room-wall graph (context-selector graph) 2))
         (first-split-point (car split-points))
         (second-split-point (cadr split-points)) ; (constraints (random-real)))
         ;; generate unique identifiers
         (new-wall-uid (make-uuid))
         (first-wall-uid-1-half (make-uuid))
         (first-wall-uid-2-half (make-uuid))
         (second-wall-uid-1-half (make-uuid))
         (second-wall-uid-2-half (make-uuid)))
         (display "----------------------------------------------\n")
         (pp graph)
         (display "----------------------------------------------\n")
         (pp room)
         (display "----------------------------------------------\n")
         (pp walls)
         (display "----------------------------------------------\n")
         (pp split-points)
         (display "----------------------------------------------\n")
    (receive (fore aft)
             (room-break graph room (element-uid first-wall) (element-uid second-wall))
      (append
        (apply-operation-in-context
          graph
          context-selector
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
        (list (point-list->wall
                (list
                  (point-from-relative-in-wall first-wall first-split-point) ; TODO: With constraints (orthogonality and elements)
                  (point-from-relative-in-wall second-wall second-split-point)) ; TODO: With constraints (orthogonality and elements)
          new-wall-uid))
        (if (is-end-point?
              (wall-list->point-list (reference-list-to-elements graph (cdr fore)))
              (archpoint->point (wall-first-point first-wall)))
            (create-splitted-wall
              (find-element-with-uid graph (element-uid (car fore)))
              first-split-point
              first-wall-uid-1-half
              first-wall-uid-2-half)
            (create-splitted-wall
              (find-element-with-uid graph (element-uid (car fore)))
              first-split-point
              first-wall-uid-2-half
              first-wall-uid-1-half))
        (if (is-end-point?
#;(define (polywall->point-list wall-lis)
  (define (iter pt-lis lis)
    (if (null-list? lis)
        pt-lis
      (iter (append pt-lis (wall->point-list (car lis))) (cdr lis))))
  (iter '() wall-lis))
              (wall-list->point-list (reference-list-to-elements graph (cdr aft)))
              (archpoint->point (wall-first-point second-wall)))
            (create-splitted-wall
              (find-element-with-uid graph (element-uid (car aft)))
              second-split-point
              second-wall-uid-1-half
              second-wall-uid-2-half)
            (create-splitted-wall
              (find-element-with-uid graph (element-uid (car aft)))
              second-split-point
              second-wall-uid-2-half
              second-wall-uid-1-half))))))
            ; TODO: añadir puerta ¡
            ; TODO: eliminar muros partidos
            ; TODO: eliminar referencias a muros eliminados

;;; Merge two rooms

(define (op-merge-rooms graph context-selector constraints)
  (if (null-list? (context-selector graph)) ; TODO: TEMP!!!
      graph
      
  (let* ((merged-rooms (context-selector graph))
         (room-a (car merged-rooms))
         (room-b (cadr merged-rooms))
         (common-wall-uid (room-find-common-wall merged-rooms))
         (wall-bifurcations (find-walls-connected-to graph common-wall-uid)) ; TODO: filter out walls that don't belong to either room
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
             (find-element-with-uid graph common-wall-uid))))
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

(define (op-expand graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Shrink

(define (op-shrink graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Post-operations
;-------------------------------------------------------------------------------

;;; Stabilize structure: add, move or replace pilars

(define (op-stabilize-structure graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Fix accesses

(define (op-fix-accesses graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Fix malformed graph

(define (op-fix-malformed graph context-selector constraints)
  (remove
    (lambda (lst)
      (equal? lst '()))
    graph))

;;; Fix room topology

(define (op-fix-room-topology graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;;; Fix everything

(define (op-fix-everything graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Helper operations
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

;;; Try to merge into one wall if the two given are parallel

(define (try-to-merge-if-parallel-walls wall-list new-uid)
  (let ((wall-a-points (wall->point-list (car wall-list))) ; TODO: try to generalize
        (wall-b-points (wall->point-list (cadr wall-list))))
    (if (parallel? wall-a-points wall-b-points)
        (let ((first-point (if (is-end-point? wall-b-points (car wall-a-points))
                               (cadr wall-a-points)
                             (car wall-a-points)))
              (second-point (if (is-end-point? wall-a-points (car wall-b-points))
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
