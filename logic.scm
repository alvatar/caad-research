;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Logic and decision-making
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

;;; A choice

(define-structure choice id options chosen)

;;; The stack of taken choices

(define *choices* '())

;;; Define a procedure as a choice, so it can be tracked

(define-syntax define-choice!
  (syntax-rules ()
    ;; Open results allow any kind of choice
    ((_ ?id (?f ?args ...) . ?body)
     (define ?f
       (begin
         (set! *choices* (cons (make-choice
                                ?id
                                '*open*
                                '*pending*)
                               *choices*))
         (lambda (?args ...)
           (let ((result (begin . ?body)))
             (map! (lambda (e) (if (equal? (choice-id e) ?id)
                              (make-choice
                               (choice-id e)
                               (choice-options e)
                               result)
                              e))
                   *choices*)
             result)))))
    ;; Boolean choices
    ((_ ?id "boolean" (?f ?args ...) . ?body)
     (define ?f
       (begin
         (set! *choices* (cons (make-choice
                                ?id
                                (list #t #f)
                                '*pending*)
                               *choices*))
         (lambda (?args ...)
           (let ((result (begin . ?body)))
             (map! (lambda (e) (if (equal? (choice-id e) ?id)
                              (make-choice
                               (choice-id e)
                               (choice-options e)
                               result)
                              e))
                   *choices*)
             result)))))))

;;; Show a backtrack of choices

(define (pp-choices-backtrack) ; TODO
  (pp *choices*))