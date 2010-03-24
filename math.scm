;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Mathematical operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

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

;-------------------------------------------------------------------------------
; Vector dimension 2
;-------------------------------------------------------------------------------

(define-structure vect2 u v)

;; Vector addition
;;
(define (vect2-+-vect2 v1 v2)
  (make-vect2 (+ (vect2-u v1)
                 (vect2-u v2))
              (+ (vect2-v v1)
                 (vect2-v v2))))

;; Vector substraction
;;
(define (vect2---vect2 v1 v2)
  (make-vect2 (- (vect2-u v1)
                 (vect2-u v2))
              (- (vect2-v v1)
                 (vect2-v v2))))

;; Calculate vector length TODO: any length
;;
(define (vect2-length vec)
  (sqrt (+ (expt (vect2-u vec) 2)
           (expt (vect2-v vec) 2))))

;; Normalize vectors
;;
(define (vect2-normalize vec)
  (let ((div (vect2-length vec)))
    (list (/ (abs (vect2-u vec)) div)
          (/ (abs (vect2-v vec)) div))))
