;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

;;; Letcc macro (hoping and skipping)

(define-syntax letcc
  (syntax-rules ()
    ((_ c . body)
     (call-with-current-continuation
       (lambda (c) . body)))))
    
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
