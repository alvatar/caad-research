;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import geometry)
(import graph)
(import utils/misc)

;; Apply operation to context
;;
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

;; Apply operation to a graph and all contexts matching
;;
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

;; Identity
;;
(define (op-identity graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   (context-selector graph)))

;; Remove
;;
(define (op-remove graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Topology modifications
;-------------------------------------------------------------------------------

;; Split a room
;;
(define (op-split graph context-selector constraints)
  (let* ((subgraph (context-selector graph))
         (first-wall (room-wall graph (context-selector graph) 0)) ; TODO: First wall selected with constraint
         (second-wall (room-wall graph (context-selector graph) 2)) ; TODO: Second wall selected with constraint, taking first
         (first-split-point (constraints (random-real)))
         (second-split-point (constraints (random-real)))
         (new-wall-uid (make-uuid))
         (first-wall-uid-1-half (make-uuid))
         (first-wall-uid-2-half (make-uuid))
         (second-wall-uid-1-half (make-uuid))
         (second-wall-uid-2-half (make-uuid)))
    ; TODO: Check context somehow and redirect to correct splitting algorithm
    (receive (fore aft)
             (room-break graph subgraph (element-uid first-wall) (element-uid second-wall))
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

;; Merge two rooms
;;
(define (op-merge graph context-selector constraints)
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

;; Create new element
;;
(define (op-create-element graph context-selector constraints)
  '())

;; Remove element
;;
(define (op-remove-element graph context-selector constraints)
  '())

;; Move
;;
(define (op-move graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Boundary modifications
;-------------------------------------------------------------------------------

;; Expand
;;
(define (op-expand graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Contract
;;
(define (op-contract graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;-------------------------------------------------------------------------------
; Post-operations
;-------------------------------------------------------------------------------

;; Stabilize structure: add, move or replace pilars
;;
(define (op-stabilize-structure graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Fix accesses
;;
(define (op-fix-accesses graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Fix malformed graph
;;
(define (op-fix-malformed graph context-selector constraints)
  (remove
    (lambda (lst)
      (equal? lst '()))
    graph))

;; Fix room topology
;;
(define (op-fix-room-topology graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))

;; Fix everything
;;
(define (op-fix-everything graph context-selector constraints)
  (apply-operation-in-context
   graph
   context-selector
   '()))
