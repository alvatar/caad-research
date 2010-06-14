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

;; (define (p v)
;;   (pp v)
;;   v)

(define-syntax pv
  (syntax-rules ()
    ((_ form ...)
     (call-with-values
         (lambda () (begin form ...))
         (lambda args
           (for-each pp args)
           (apply values args))))))

;;; Debug print and step in place

(define-syntax ps
  (syntax-rules ()
    ((_ form)
     (let ((res form))
       (pp res)
       (step)
       res))))

;;; Do a test so it can be activated or deactivated

(define-syntax assert-aux
  (syntax-rules ()
    ((_ "ignore" test proc)
     #f)
    ((_ "do" test proc)
     (if (not test)
         (begin
           (display "todo: line inspection\n")
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
               (display "todo: line inspection\n")
               (error "Assert failed!" proc)))))))))
