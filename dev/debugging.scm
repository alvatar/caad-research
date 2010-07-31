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

;;; Run-time checks

(define-syntax %accept
  (syntax-rules ()
    ((_ test)
     #f)
    ((_ msg test)
     #f)
    ((_ msg test . forms)
     (begin . forms))))

(define-syntax %deny
  (syntax-rules ()
    ((_ test)
     #f)
    ((_ msg test)
     #f)
    ((_ msg test . forms)
     (begin . forms))))

(define-macro (%activate-checks) ; TODO: port to nested define-checks with new BH
  '(begin
     ;; Test the procedure and issue an error if #f, otherwise continue running
     (define-syntax %accept
       (syntax-rules ()
         ((_ test)
          (if (not test)
              (error "checked value not accepted")))
         ((_ msg test)
          (if (not test)
              (error msg)))
         ((_ msg test . forms)
          (if test
              (begin . forms)
              (error msg)))))
     ;; Test the procedure and issue an error if #t, otherwise continue running
     (define-syntax %deny
       (syntax-rules ()
         ((_ test)
          (if test
              (error "checked value denied")))
         ((_ msg test)
          (if test
              (error msg)))
         ((_ msg test . forms)
          (if test
              (error msg)
              (begin . forms)))))))
