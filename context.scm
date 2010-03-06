;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for filtering out subgraph lists from an architecture graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import graph)

;; All
;;
(define all-graph
  (lambda (graph) graph))

;; Returns the smallest room as context
;;
(define smallest-room '())

;; Returns the biggest room as context
;;
(define (biggest-room graph)
  (let
    ((rooms-list (rooms graph)))
    (if
      (null? rooms-list)
      '()
      (car rooms-list)))) ; TODO
  ; (define (iter graph current-room current-max)
    ; (if
      ; (list? graph)
        ; (if (> (room-area ROOM) current-max)
            ; (iter REST_OF_GRAPH ROOM (room-area ROOM))
            ; (iter REST_OF_GRAPH ROOM current-max))
        ; ROOM))
  ; (iter graph 0.0)
  ; '())
