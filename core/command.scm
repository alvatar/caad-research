;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A simple, generic approach to serializable commands and arguments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import list
        debugging)

(%activate-checks)

(define (get-arg arguments key)
  (let ((argname (string->symbol (string-append
                                  "@"
                                  (if (symbol? key) (symbol->string key) key)))))
    (%accept #t "the given key can't be found in the argument list"
             (let ((res (find-tail (lambda (a) (eq? argname a))
                                   arguments)))
               (and (list? res)
                    (not-null? (cdr res))
                    (cadr res))))))