;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging utilities
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

;;; Debug print making sure that 

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

;;; Do a test so it can be activated or deactivated

(define-syntax assert-aux
  (syntax-rules ()
    ((_ "ignore" test proc)
     #f)
    ((_ "do" test proc)
     (if (not test)
         (begin
           (display "todo: where did this happen?\n")
           (error "Assert failed!" proc))))))

;;; run-time checks

(define-syntax activate-asserts ; FIXME: Currently explodes blackhole, but should work
  (syntax-rules ()
    ((_)
     (define-syntax assert
       (syntax-rules ()
         ((_ test proc)
          (if (not test)
              (begin
                (display "todo: where did this happen?\n")
                (error "Assert failed!" proc)))))))))

;;; Test the procedure and issue an error if #f, otherwise continue running

(define-syntax %accept
  (syntax-rules ()
    ((_ msg test)
     (if (not test)
         (error msg)))
    ((_ msg test . forms)
     (if test
         (begin . forms)
         (error msg)))))

;;; Test the procedure and issue an error if #t, otherwise continue running

(define-syntax %deny
  (syntax-rules ()
    ((_ msg test)
     (if test
         (error msg)))
    ((_ msg test . forms)
     (if test
         (error msg)
         (begin . forms)))))
