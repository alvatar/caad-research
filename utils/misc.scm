;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General utilities and algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
;(compile-options force-compile: #t)

(import (std srfi/1))

;(compile-options cc-options: "-w" force-compile: #t)

;-------------------------------------------------------------------------------
; Debugging
;-------------------------------------------------------------------------------

;;; Debug print

(define (p v)
  (pp v)
  v)

;;; Debug print and step in place

(define-syntax ps
  (syntax-rules ()
    ((_ form)
     (let ((res form))
       (pp res)
       (step)
       res))))

;-------------------------------------------------------------------------------
; Miscelaneous procedures
;-------------------------------------------------------------------------------

;;; snoc

(define snoc
  (lambda (ls x)
    (append ls (list x))))

;;; Rotates the list until the first one satisfies the predicate

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

;;; atom?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;; Recursive map

(define (map* f lis)
  (cond
   ((null? lis)
    '())
   ((atom? lis)
    (f lis))
   (else
    (cons (map* f (car lis)) (map* f (cdr lis))))))

;;; U combinator

(define U
  (lambda (f) (f f)))

;;; Y combinator

(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

;-------------------------------------------------------------------------------
; Macro procedures
;-------------------------------------------------------------------------------

;;; Syntax error macro

(define-syntax syntax-error
  (syntax-rules ()
    ((syntax-error) (syntax-error "Bad use of syntax error!"))))

;;; Anaforic if

(define-syntax aif
  (syntax-rules ()
    ((_ var expr iftrue iffalse)
     (let ((var expr))
       (if var
         iftrue
         iffalse)))
    ((_ var test expr iftrue iffalse)
     (let ((var expr))
       (if (test var)
         iftrue
         iffalse)))))

;;; When

(define-syntax when
  (syntax-rules ()
    ((_ condition form . forms)
     (if condition (begin form . forms) #f))))

;;; Bind only one variable

(define-syntax let1
  (syntax-rules ()
    ((_ var expr body ...)
     (let ((var expr)) body ...))))

;;; Do a fixed number of times

(define-syntax dotimes
  (syntax-rules ()
    ((_ (var n res) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit) res)
       . body))
    ((_ (var n) . body)
     (do ((limit n)
          (var 0 (+ var 1)))
         ((>= var limit))
       . body))))

;;; Begin returning the value of the first expression

(define-syntax begin0
  (syntax-rules ()
    ((_ expr0 expr1 ...)
     (let ((return expr0))
       expr1 ...
       return))))

;;; Extract only the nth-value from a function returning multiple values

(define-syntax nth-value
  (syntax-rules ()
    ((_ n values-producing-form)
     (call-with-values
       (lambda () values-producing-form)
       (lambda all-values
         (list-ref all-values n))))))
