;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Debugging utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(declare (standard-bindings)(extended-bindings)(block)(not safe))
;(compile-options force-compile: #t)

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

;;; Argument checks per module
;;;
;;; (define-syntax check-arg
;;;  (syntax-rules ()
;;;    ((_ predicate arg proc)
;;;     (check-arg-per-module "do" predicate arg proc))))

(define-syntax check-arg-per-module
  (syntax-rules ()
    ((_ "ignore" predicate arg proc)
     #f)
    ((_ "do" predicate arg proc)
     (if (not (predicate arg))
         (begin
           (pp arg)
           (error "Argument not accepted" proc))))))
