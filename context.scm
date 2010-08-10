;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for filtering out subgraph lists from an architecture graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import core/list
        core/debugging
        graph
        graph-operations
        ds/binary-tree
        visualization)

(%activate-checks)

;-------------------------------------------------------------------------------
; Context tree
;-------------------------------------------------------------------------------

(define-structure binary-tree node sibling child)

;;; Create a context-tree

(define make-context-tree list->binary-tree)

;;; Get the context-tree root node

(define context-tree:root binary-tree:root)

;;; Extract a list of all nodes in the same level

(define context-tree:level binary-tree:level)

;;; Find the first node in a level

(define context-tree:first-in-level binary-tree:leftmost-in-level)

;-------------------------------------------------------------------------------
; Context builders
;-------------------------------------------------------------------------------

;;; All

;; (define all-graph->context
;;   (lambda (graph) graph))

;;; Returns the smallest room as context

;; (define smallest-room->context '())

;;; Takes the biggest room as context

;; (define (biggest-room->context graph)
;;   (let ((rooms-list (graph:find-rooms graph)))
;;     (if
;;      (null? rooms-list)
;;      '()
;;      (car rooms-list))))                ; TODO

;;; Takes the two first rooms as context

;; (define (two-rooms->context graph)
;;   (let ((rooms-list (rooms-in-graph graph)))
;;     (if (> (length rooms-list) 1)
;;         (take rooms-list 2)
;;         '())))

;;; Takes a graph and an infinite line and builds the context with the
;;; computation of their intersections

(define (graph+line->context graph line)
  (receive (rooms walls intersections)
           (graph:relative-line-intersections graph line)
           (begin
             (%deny (or (null? walls)
                        (null? intersections))
                    "no intersections found, can't create context")
             `(#f ,graph       ; #f for the n-ary tree internal nodes
                  (#f          ; TODO: DOESN'T WORK WITH MORE THAN ONE
                   ,rooms
                   ,@(zip (make-list (length walls) #f)
                          walls
                          intersections))))))

;;; Takes a graph, a room and an infinite line and builds the context with
;;; the computation of the intersection between the room and the line

(define (room+line->context graph room line)
  (receive (walls intersections)
           (graph:room-relative-line-intersections graph room line)
           (begin
             (%deny (or (null? walls)
                        (null? intersections))
                    "no intersections found, can't create context")
             `(#f ,graph        ; #f for the n-ary tree internal nodes
                  (#f
                   ,room
                   ,@(zip (make-list (length walls) #f)
                          walls
                          intersections))))))

;;; Builds the context from two rooms

(define (room+room->context graph r1 r2)
  `(#f ,graph
       ,r1
       ,r2))

;-------------------------------------------------------------------------------
; Context transformers
;-------------------------------------------------------------------------------

(define (context:add-free-point context point)
  (error "unimplemented"))