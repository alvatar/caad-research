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
; Miscelaneous procedures
;-------------------------------------------------------------------------------

;;; Debug print

(define (p v)
  (pp v)
  v)

;;; Debug print and step

(define (ps v)
  (pp v)
  (step)
  v)

;;; 0.0-1.0 range to u8 integer

(define (normalized-inexact->integer value)
  (modulo (inexact->exact (round (* 255 value))) 255))

;;; snoc

(define snoc
  (lambda (ls x)
    (append ls (list x))))

;;; Take the first one of a list, if is not a list, take the element itself

#;(define (first-or-element list-or-element)
  (if (list? list-or-element)
      (car list-or-element)
    (list-or-element)))

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

;;; Call/cc with one extra argument

(define (call/cc1 procedure value)
  (call/cc (lambda (k) (procedure k value))))

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

;;; When

(define-syntax when
  (syntax-rules ()
    ((_ condition form . forms)
     (if condition (begin form . forms) #f))))

;;; Extract only the nth-value from a function returning multiple values

(define-syntax nth-value
  (syntax-rules ()
    ((_ n values-producing-form)
     (call-with-values
       (lambda () values-producing-form)
       (lambda all-values
         (list-ref all-values n))))))

;;; If-let
;;; if-form that binds the evaluated value for use inside the #t #f forms

(define-syntax if-let
  (syntax-rules ()
    ((_ (a value))
     (let ((a value))
       (if a a #f)))
    ((_ (a value) form-t)
     (let ((a value))
       (if a form-t #f)))
    ((_ (a value) form-t form-f)
     (let ((a value))
       (if a form-t form-f)))))
