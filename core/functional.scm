;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Functional programming utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;-------------------------------------------------------------------------------
; Functional operators
;-------------------------------------------------------------------------------

;;; U combinator

(define U
  (lambda (f) (f f)))

;;; Y combinator

(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

;;; Function composition

(define (compose . fns)
  (define (make-chain fn chain)
    (lambda args
      (call-with-values (lambda () (apply fn args)) chain)))
  (reduce make-chain values fns))

;-------------------------------------------------------------------------------
; Currying / uncurrying
;-------------------------------------------------------------------------------

;;; Explicit currying of an arbitrary function

(define (curry fun . args)
  (lambda x
    (apply fun (append args x))))

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
  (let ((memos '()))
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
