;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple, generic approach to serializable commands and arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

(import (std srfi/1))
(import list
        debugging)

(%activate-checks)

;;; Get an argument given the key

(define (get-arg arguments key)
  (let ((argname (string->symbol (string-append
                                  "@"
                                  (if (symbol? key) (symbol->string key) key)))))
    (let ((res (find-tail (lambda (a) (eq? argname a))
                          arguments)))
      (and (list? res)
           (not-null? (cdr res))
           (cadr res)))))