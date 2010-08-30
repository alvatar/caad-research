;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedures creating contexts and arguments for operators, serving as
;;; helpers between geometrical operations and high level-architectural
;;; operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        core/list
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
; Generic context & arguments builders
;-------------------------------------------------------------------------------

;;; Defines a context as a graph and a list of arbitrary elements

(define (many->context graph . many)
  `(#f ,graph
       ,@many))

;;; Define a context as a graph and a list of any element

(define (any->context graph any)
  `(#f ,graph
       ,any))

;-------------------------------------------------------------------------------
; Specialized context & arguments builders
;-------------------------------------------------------------------------------

;;; Takes a graph and an infinite line and builds the context with the
;;; computation of their intersections

(define (line->context+arguments graph line)
  (receive (rooms walls intersections)
           (graph:relative-line-intersections graph line)
           (begin
             (%deny (or (null? walls)
                        (null? intersections))
                    "no intersections found, can't create context")
             (values
              `(#f ,graph      ; #f for the n-ary tree internal nodes
                   (#f         ; TODO: DOESN'T WORK WITH MORE THAN ONE. IMPORTANT!
                    ,rooms
                    ,@walls))
              `(@split-points ,intersections)))))

;;; Takes a graph, a room and an infinite line and builds the context with
;;; the computation of the intersection between the room and the line

(define (room&line->context+arguments graph room line)
  (receive (walls intersections)
           (graph:room-relative-line-intersections graph room line)
           (begin
             (%deny (or (null? walls)
                        (null? intersections))
                    "no intersections found, can't create context")
             (values
              `(#f ,graph       ; #f for the n-ary tree internal nodes
                   (#f
                    ,room
                    ,@walls))
              `(@split-points ,intersections)))))
