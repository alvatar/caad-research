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

(define-syntax @list
  (syntax-rules ()
    ((_ (?key ?contents) ...)
     `((?key ,?contents) ...))))

;;; Get an argument

(define-syntax @get
  (syntax-rules ()
    ((_ ?key ?command)
     (aif element (assq '?key ?command)
          (cadr element)
          (error "argument not found")))))

;;; Let-arguments

(define-syntax @let
  (syntax-rules ()
    ((_ ((?arg-name ...) ?args-obj . ?more) ?forms ...)
     '(let ((?arg-name (@get ?arg-name ?args-obj)) ...)
       ?forms ...))))

(define-syntax @let
  (syntax-rules ()
    ((_ ((?arg-name ...) ?args-obj . ?more) ?forms ...) ; entry point
     (@let "recur" ((?arg-name (@get ?arg-name ?args-obj)) ...) ?more ?forms ...))
    ((_ "recur" (?let-bindings ...) () ?forms ...) ; end recursion
     '(let (?let-bindings ...) ?forms ...))
    ((_ "recur" (?let-bindings ...) ((?arg-name ...) ?args-obj . ?more) ?forms ...)
     (@let "recur" (?let-bindings ... (?arg-name (@get ?arg-name ?args-obj)) ...) ?more ?forms ...))))