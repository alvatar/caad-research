;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural high-level operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std misc/uuid))
(import geometry)
(import graph)
(import utilities)

;; Apply operation to context
;;
(define (apply-operation-in-context graph context-selector new-subgraph)
  (define (matches-context? elem)
    (equal? elem (context-selector graph))) ; optimize
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
        (list (point-list-to-wall
                (list
                  (make-archpoint (point-from-relative-in-wall first-wall first-split-point)) ; TODO: With constraints (orthogonality and elements)
                  (make-archpoint (point-from-relative-in-wall second-wall second-split-point))) ; TODO: With constraints (orthogonality and elements)
          new-wall-uid))
        (if (is-end-point?
              (extract-polywall-points (reference-list-to-elements graph (cdr fore)))
              (extract-archpoint-coords (wall-first-point first-wall)))
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
              (extract-polywall-points (reference-list-to-elements graph (cdr aft)))
              (extract-archpoint-coords (wall-first-point second-wall)))
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
  (define (update-walls graph wall-list)
    '())
  (let* ((merge-rooms (context-selector graph))
         (room-a (car merge-rooms))
         (room-b (cadr merge-rooms))
         (common-wall-uid (find-common-wall merge-rooms))
         (wall-bifurcations (find-walls-connected-to graph common-wall-uid)) ; TODO: filter out walls that don't belong to either room
         (possible-uid-1 (make-uuid))
         (possible-uid-2 (make-uuid))
         (touched-walls-a (try-to-merge-if-parallel-walls (car wall-bifurcations) possible-uid-1))
         (touched-walls-b (try-to-merge-if-parallel-walls (cadr wall-bifurcations) possible-uid-2))
         (rest-of-walls-a (room-remove-walls (car wall-bifurcations) room-a))
         (rest-of-walls-b (room-remove-walls (cadr wall-bifurcations) room-b))
         (walls-a
           (if (> (length touched-walls-a) 1)
               '()
             '()))
        ; It has not been merged, stay like that
        ; It has been merged: remove two previous and add new one
         (walls-b
           (if (> (length touched-walls-b) 1)
               '()
             '()))
         (graph-with-changed-room
           (apply-operation-in-context
             graph
             context-selector
             (list
               (append `(room (@ (uid ,(make-uuid))))
                       rest-of-walls-a
                       walls-a
                       rest-of-walls-b
                       walls-b))))
         )
    (remove-element
      (update-walls
        (update-walls
          graph-with-changed-room
          walls-a)
        walls-b)
      wall-uid)))

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
      (if (equal? lst '()) #t #f))
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
