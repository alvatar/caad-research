;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functions for filtering out subgraph lists from an architecture graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  graph)
  ; (define (iter graph current-room current-max)
    ; (if
      ; (list? graph)
        ; (if (> (room-area ROOM) current-max)
            ; (iter REST_OF_GRAPH ROOM (room-area ROOM))
            ; (iter REST_OF_GRAPH ROOM current-max))
        ; ROOM))
  ; (iter graph 0.0)
  ; '())
