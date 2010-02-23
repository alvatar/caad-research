;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Architectural operations on the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import graph)

;; Apply to context
;;
(define (apply-to-context operation context-builder graph)
  (define (do-in-context graph-tail)
    (map
      (lambda (graph-elem)
        (if
          (not (null? graph-elem)) ; LIST? quitar : tiene que ir dentro de la iteración
          (if
            (equal? graph-elem (context-builder graph))
            (operation graph graph-elem)
            (if
              (list? graph-elem)
              (begin (do-in-context graph-elem)
                   ;(display graph-tail)(newline)
                   graph-elem)
              graph-elem))))
      graph-tail))
  (do-in-context graph))

;; Identity
;;
(define (op-identity graph subgraph)
  subgraph)

;; Partition
;;
(define (op-partition graph subgraph)
  (if
    (equal? (car subgraph) 'room)
    ; `(architecture (wall ; TODO
     ; (pt (@ (y ,(number->string (* 0.9 (random-integer 500)))) (x "150.0")))
     ; (pt (@ (y "100.0") (x "450.0")))))
      ;'()
      '(room (@ (label "follodromo"))
       (wall (@ (uid "0000000000")))
       (wall (@ (uid "5493820876")))
       (wall (@ (uid "5394501263")))
       (wall (@ (uid "0034923049")))
       )
     ;(begin (display "ROOM\n") subgraph)
     subgraph))

;; Remove
;;
; (define (remove graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context remove-impl (context-builder graph)))

;; Expand
;;
; (define (expand graph context-builder)
  ; (define (partition-impl subgraph)
    ; subgraph)
  ; (apply-operation-to-context expand-impl (context-builder graph)))


