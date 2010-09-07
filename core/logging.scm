;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logging utilities and macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

;;; Log with a given text 

(define-syntax %log
  (syntax-rules ()
    ((_ ?text) #f)
    ((_ ?text ?form ?forms ...) (begin ?form ?forms ...))))

;;; Log if a condition is met

(define-syntax %log-if
  (syntax-rules ()
    ((_ ?text ?test) #f)))

;;; Effectively activate logs

(define-macro (%activate-log)      ; TODO: nested define-sytax with BH
  '(define-syntax %log
     (syntax-rules ()
       ((_ ?text ?forms ...)
        (begin (display ?text)
               (newline)
               ?forms ...))))
  '(define-syntax %log-if
     (syntax-rules ()
       ((_ ?text ?test ?forms ...)
        (if ?test
            (begin (display ?text)
                   (newline)))))))
