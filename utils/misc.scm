;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General utilities and algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

;(compile-options cc-options: "-w" force-compile: #t)

;; 0.0-1.0 range to u8 integer
;;
(define (normalized-inexact->integer value)
  (modulo (inexact->exact (round (* 255 value))) 255))

;; snoc
;;
(define snoc
  (lambda (ls x)
    (append ls (list x))))

;; Take the first one of a list, if is not a list, take the element itself
;;
(define (first-or-element list-or-element)
  (if (list? list-or-element)
      (car list-or-element)
    (list-or-element)))

;; Rotates the list until the first one satisfies the predicate
;;
(define (rotate-until-first pred lis)
  (define (iter lis-iter n)
    (let ((x (car lis-iter))
          (l (length lis)))
      (cond
       ((= n l)
        (raise "Full list rotation done without satisfying predicate"))
       ((pred x)
          lis-iter)
       (else
        (iter (append (cdr lis-iter) (list x)) (+ n 1))))))
  (iter lis 0))

;; Call/cc with one extra argument
;;
(define (call/cc1 procedure value)
  (call/cc (lambda (k) (procedure k value))))
