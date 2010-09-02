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
    ((_ text . forms)
     (begin . forms))))

(define-macro (%activate-logging) ; TODO: nested define-sytax with BH
  '(define-syntax %log
    (syntax-rules ()
      ((_ text . forms)
       (begin (display text)
              (newline)
              . forms)))))