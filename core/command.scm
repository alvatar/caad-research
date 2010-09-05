;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple, generic approach to serializable commands and arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import syntax)

;;; Make arguments

(define-syntax @args
  (syntax-rules ()
    ((_ (key contents) ...)
     `((key ,contents) ...))))

;;; Make a command

(define-syntax @command
  (syntax-rules ()
    ((_ command (key contents) ...)
     `(command ((key , contents) ...)))))

;;; Get an argument

(define-syntax @get
  (syntax-rules ()
    ((_ key command)
     (aif element (assq 'key command)
          (cadr element)
          (error "argument not found")))))