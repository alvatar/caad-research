;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax extensions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (mostly-generic))
(compile-options force-compile: #t)

;;; Syntax error macro
;;; TODO: Update places with error instead of syntax-error

(define-syntax syntax-error
  (syntax-rules ()
    ((_)
     (error "Bad use of syntax error!"))
    ((_ arg)
     (error arg))))

;;; Anaforic if

(define-syntax aif
  (syntax-rules ()
    ((_ var expr iftrue iffalse)
     (let ((var expr))
       (if var
         iftrue
         iffalse)))
    ((_ var pred expr iftrue iffalse)
     (let ((var expr))
       (if (pred var)
         iftrue
         iffalse)))))

;;; When

(define-syntax when
  (syntax-rules ()
    ((_ condition form . forms)
     (if condition (begin form . forms) #f))))

;;; Unless

(define-syntax unless
  (syntax-rules ()
    ((_ test consequent)
     (if (not test) consequent))))

;;; Letcc macro (hoping and skipping)

(define-syntax let/cc
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

;;; Define values allows sharing state between functions
;; UNTESTED
;; (define-values (inc dec reset)
;;   (let ((state 0))
;;     (define (inc)  (set! state (+ state 1)) state)
;;     (define (dec)  (set! state (- state 1)) state)
;;     (define (reset)(set! state 0)           state)
;;     (values inc dec reset)))

(define-syntax define-values
  (syntax-rules ()
    ((_ "gentmp" (tmp ...) () (var ...) expr)
     (begin (define var (undefined)) ...
            (receive (tmp ...) expr
                     (set! var tmp) ...
                     (undefined))))
    ((_ "gentmp" (tmp ...) (v v2 ...) (var ...) expr)
     (define-values "gentmp" (tmp ... tmp1) (v2 ...) (var ...) expr))
    ((_ (var  ...) expr)
     (define-values "gentmp" () (var ...) (var ...) expr))
    ((_ . else)
     (syntax-error "malformed define-values" (define-values . else)))))
