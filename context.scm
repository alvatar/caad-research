;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for filtering out subgraph lists from an architecture graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import graph)
(import ds/btree)

;-------------------------------------------------------------------------------
; Context tree
;-------------------------------------------------------------------------------

(define-structure btree node sibling child)

;;; Create a context-tree

(define make-context-tree list->btree)

;;; Get the context-tree root node

(define context-tree:root btree:root)

;;; Extract a list of all nodes in the same level

(define context-tree:level btree:level)

;;; Find the first node in a level

(define context-tree:first-in-level btree:leftmost-in-level)

;-------------------------------------------------------------------------------
; Vector dimension 2
;-------------------------------------------------------------------------------

;;; All

(define all-graph
  (lambda (graph) graph))

;;; Returns the smallest room as context

(define smallest-room '())

;;; Takes the biggest room as context

(define (biggest-room graph)
  (let ((rooms-list (graph:find-rooms graph)))
    (if
     (null? rooms-list)
     '()
     (car rooms-list))))                ; TODO

;;; Takes the two first rooms as context

(define (two-rooms graph)
  (let ((rooms-list (rooms-in-graph graph)))
    (if (> (length rooms-list) 1)
        (take rooms-list 2)
        '())))
