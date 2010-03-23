;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mathematical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Arithmetic
;-------------------------------------------------------------------------------

;; Is equal? (with precision) for lists
;;
(define (list=~ a b precision)
  (define (test t1 t2)
    (< (abs (- a b)) precision))
  (if (and (list? a) (list? b))
      (every (lambda (e1 e2) (list=~ e1 e2 precision))
             a
             b)
    (test a b)))

;; Is equal? (with precision) for inexacts
;;
(define (=~e a b e)
  (< (abs (- a b)) e))

;; Is equal? with defined precision
;;
(define (=~ a b)
  (=~e a b 0.0000001)) ; TODO: Find the right accuracy

;; Average between two values
;;
(define (average a b)
  (/ (+ a b) 2.0))
