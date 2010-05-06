;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General utilities and algorithms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
;(compile-options force-compile: #t)

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
; List procedures
;-------------------------------------------------------------------------------

;;; atom?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;; snoc

(define snoc
  (lambda (ls x)
    (append ls (list x))))

;;; Recursive map

(define (map* f l)
  (cond
   ((null? l)
    '())
   ((atom? l)
    (f l))
   (else
    (cons (map* f (car l)) (map* f (cdr l))))))

;;; Recursive substitution in a list

(define (subst* new old l)
  (xsubst* cons new old l))

;;; Recursive multiple substitution in a list

(define (msubst* new old l)
  (xsubst* append new old l))

;;; Recursive substitution in a list

(define (xsubst* f new old l)
  ((letrec ((X (lambda (l)
    (cond
      ((null? l)
       '())
      ((atom? (car l)) ; Atoms level
       (cond
         ((eq? (car l) old)
          (f
            new
            (X (cdr l))))
         (else
           (cons
             (car l)
             (X (cdr l))))))
      ((equal? (car l) old) ; Sublist level
       (f new (X (cdr l))))
      (else
        (cons
          (X (car l))
          (X (cdr l)))))))) X) l))

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

;-------------------------------------------------------------------------------
; Functional/lambda
;-------------------------------------------------------------------------------

;;; U combinator

(define U
  (lambda (f) (f f)))

;;; Y combinator

(define Y
  (lambda (X)
    (U (lambda (proc)
         (X (lambda (arg) ((U proc) arg)))))))

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

;-------------------------------------------------------------------------------
; Currying
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

;-------------------------------------------------------------------------------
; Utility macros
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
