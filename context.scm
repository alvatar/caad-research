;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for filtering out subgraph lists from an architecture graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import graph)

;; All
;;
(define all-graph
  (lambda (graph) graph))

;; Returns the smallest room as context
;;
(define smallest-room '())

;; Takes the biggest room as context
;;
(define (biggest-room graph)
  (let
    ((rooms-list (rooms-in-graph graph)))
    (if
      (null? rooms-list)
      '()
      (car rooms-list)))) ; TODO

;; Takes the two first rooms as context
;;
(define (two-rooms graph)
  (let ((rooms-list (rooms-in-graph graph)))
    (if (> (length rooms-list) 1)
        (take rooms-list 2)
      '())))
