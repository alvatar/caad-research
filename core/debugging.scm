;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging and checking data utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Debug print

(define-syntax pv
  (syntax-rules ()
    ((_ text form)
     (call-with-values
         (lambda () form)
       (lambda args
         (display text)
         (display ": ") ; TODO: concat
         (for-each display args)
         (newline)
         (apply values args))))
    ((_ form)
     (call-with-values
         (lambda () form)
       (lambda args
         (for-each pp args)
         (apply values args))))))

;;; Debug print making sure that numeric values are translated into inexact

(define-syntax piv
  (syntax-rules ()
    ((_ text form)
     (call-with-values
         (lambda () form)
       (lambda args
         (display text)
         (display ": ")                 ; TODO: concat
         (for-each (lambda (a)
                     (if (number? a)
                         (display (exact->inexact a))
                         (display a))
                     (newline))
                   args)
         (apply values args))))
    ((_ form)
     (call-with-values
         (lambda () form)
       (lambda args
         (for-each (lambda (a)
                     (if (number? a)
                         (display (exact->inexact a))
                         (display a))
                     (newline))
                   args)
         (apply values args))))))

;;; Debug print and step in place

(define-syntax ps
  (syntax-rules ()
    ((_ form ...)
     (call-with-values
         (lambda () (begin form ...))
       (lambda args
         (for-each pp args)
         (step)
         (apply values args))))))

;;; Run-time checks

(define-syntax %accept
  (syntax-rules ()
    ((_ test)
     #f)
    ((_ test msg)
     #f)
    ((_ test msg . forms)
     (begin . forms))))

(define-syntax %deny
  (syntax-rules ()
    ((_ test)
     #f)
    ((_ test msg)
     #f)
    ((_ test msg . forms)
     (begin . forms))))

(define-macro (%activate-checks) ; TODO: port to nested define-checks with new BH
  '(begin
     
;;; Test the procedure and issue an error if #f, otherwise continue running
     
     (define-syntax %accept
       (syntax-rules ()
         ((_ test)
          (if (not test)
              (raise "checked value not accepted")))
         ((_ test msg)
          (if (not test)
              (raise msg)))
         ((_ #t msg . forms)
          (let ((result (begin . forms)))
            (if result
                result
                (raise msg))))
         ((_ test msg . forms)
          (let ((result (begin . forms)))
            (if (test result)
                result
                (raise msg))))))
     
;;; Test the procedure and issue an error if #t, otherwise continue running
     
     (define-syntax %deny
       (syntax-rules ()
         ((_ test)
          (if test
              (raise "checked value denied")))
         ((_ test msg)
          (if test
              (raise msg)))
         ((_ #t msg . forms)
          (let ((result (begin . forms)))
            (if result
                (raise msg)
                result)))
         ((_ test msg . forms)
          (let ((result (begin . forms)))
            (if (test result)
                (raise msg)
                result)))))))

;;; Value tracing for checking identified value evolution over the program

(define %%traced-vals '())

(define-syntax %register/id
  (syntax-rules ()
    ((_ ?id ?v)
     (set! %%traced-vals
           (cons
            (cons
             ?id
             (call-with-values
                 (lambda () ?v)
               (lambda vs (apply list vs))))
            %%traced-vals)))))

(define-syntax %compare/id
  (syntax-rules ()
    ((_ ?id ?comparator ?v)
     (?comparator
      (cdr (assq ?id %%traced-vals))
      (call-with-values
          (lambda () ?v)
        (lambda vs (apply list vs)))))))

(define (%show-trace/id id)
  (let ((id-string (symbol->string id)))
    (let loop ((traced %%traced-vals)
               (n 0))
      (cond
       ((null? traced) (void))
       ((eq? (caar traced) id)
        (begin (display "$:") (display n) (display " ")
               (pv id-string (cdar traced))
               (loop (cdr traced) (- n 1))))))
    (void)))

(define (%untrace-vals)
  (set! %%traced-vals '()))
