;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional programming utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (mostly-generic))
;(compile-options force-compile: #t)

(import (std srfi/1))

(export U Y Y! compose compose-right
        define-associative
        curry define-curried lambda-curried uncurry
        define-memoized define-memoized/key-gen)

;-------------------------------------------------------------------------------
; Functional operators
;-------------------------------------------------------------------------------

;;; U-combinator

(define U
  (lambda (f) (f f)))

;;; Y-combinator

(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

;;; The applicative-order imperative y-combinator (by Peter Landin)

(define Y!
  (lambda (f)
    (letrec
        ((h (f (lambda (arg) (h arg)))))
      h)))

;;; Function composition

(define (composer reducer . fns)
  (reducer (lambda (fn chain)
            (lambda args
              (call-with-values (lambda () (apply fn args)) chain)))
          values
          fns))

(define (compose . fns)
  (apply composer reduce fns))

(define (compose-right . fns)
  (apply composer reduce-right fns))

;-------------------------------------------------------------------------------
; Associative functions
;-------------------------------------------------------------------------------

;;; Defines a function and its associated associative function (that will take
;;; any number of arguments and apply it to the result of the two previous ones)

(define-syntax define-associative-aux
  (syntax-rules ()
    ((_ name f)
     (define-syntax name
       (syntax-rules ()
         ((_ arg1 arg2)
          (f arg1 arg2))
         ((_ arg1 arg2 . rest)
          (name (f arg1 arg2) . rest)))))))

(define-syntax define-associative
  (syntax-rules ()
    ((_ name (f arg1 arg2) body)
     (begin
       (define (f arg1 arg2) body)
       (define-associative-aux name f)))))

;-------------------------------------------------------------------------------
; Currying / uncurrying
;-------------------------------------------------------------------------------

;;; Explicit currying of an arbitrary function

(define (curry fun arg1 . args)
  (if (pair? args)
      (lambda x
        (apply fun (append (cons arg1 args) x)))
      (lambda x
        (apply fun (cons arg1 x)))))

;;; Define an automatically curryable function
;;;
;;; (define-curried (foo x y z) (+ x (/ y z))) ;; foo has arity 3
;;; ((foo 3) 1 2) ;; (foo 3) is a procedure with arity 2
;;; ((foo 3 1) 2) ;; (foo 3 2) is a procedure with arity 1

(define-syntax curried
  (syntax-rules ()
    ((_ () body ...)
     (lambda () body ...))
    ((_ (arg) body ...)
     (lambda (arg) body ...))
    ((_ (arg args ...) body ...)
     (lambda (arg . rest)
       (let ((next (curried (args ...) body ...)))
         (if (null? rest)
             next
             (apply next rest)))))))

(define-syntax define-curried
  (syntax-rules ()
    ((_ (name args ...) body ...)
     (define name (curried (args ...) body ...)))))

;;; Curried lambda
;;;
;;; (lambda-curried (x y z) (+ x y z)) =>
;;;   (lambda (x) (lambda (y) (lambda (z) (+ x y z))))
;;; (map map (map (lambda-curried (a b) (* a b)) '(1 2 3)) '((4 5 6) (7 8 9) (10 11 12)))

(define-macro (lambda-curried bindings . body)
  (define (fold-right kons knil lis1)
    (let recur ((lis lis1))
       (if (null? lis) knil
	    (let ((head (car lis)))
	      (kons head (recur (cdr lis)))))))
  (if (null? bindings) `(lambda () ,@body)
    (fold-right (lambda (arg curr-body) `(lambda (,arg) ,curr-body))
	 (cons 'begin body) bindings)))

;;; Uncurrying
;;;
;;; (uncurry (lambda (a) (lambda (b) (lambda (c) (+ a b c)))) 5 2 1)

(define (uncurry f . arglist)
  (if (null? arglist) f
    (apply uncurry (f (car arglist)) (cdr arglist))))

;-------------------------------------------------------------------------------
; Memoization
;-------------------------------------------------------------------------------

;;; Function computation memoization specifying a key generation procedure

(define (memoize/key-gen key-gen f)
  (let ((memos '())) ; OPTIMIZE: hash table!
    (lambda args
      (let ((key (apply key-gen args)))
        (apply
          values
          (cond
           ((assoc key memos)
            => cdr)
           (else
             (call-with-values
               (lambda ()
                 (apply f args))
               (lambda results
                 (set! memos ; Put the new result in memos
                   (cons (cons key results)
                         memos))
                 results)))))))))

;;; Function computation memoization with default key generation

(define (memoize f)
  (memoize/key-gen (lambda (args) args) f))

;;; Macro for memoized function definition (with default key generator)

(define-syntax define-memoized
  (syntax-rules (lambda)
    ((_ (name args ...) body ...)
     (define name
       (letrec ((name (lambda (args ...) body ...)))
         (memoize name))))
    ((_ name (lambda (args ...) body ...))
     (define-memoized (name args ...) body ...))))

;;; Macro for memoized function definition (specifying a key generator)

(define-syntax define-memoized/key-gen
  (syntax-rules ()
	((_ name
       (lambda (args-for-key ...) body-for-key ...)
       (lambda (args ...) body ...))
	 (define name
	   (letrec ((name (lambda (args ...) body ...)))
         (memoize/key-gen
		   (lambda (args-for-key ...) body-for-key ...)
           name))))))
