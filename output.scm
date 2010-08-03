;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import core/list)

(define output-dir "xml-output")

;;; Output a single graph

(define (output graph)
  (let ((output-dir-path (path-normalize output-dir))
        (file-path-gen (lambda (dir-path)
                         (string-append dir-path
                                        (number->string
                                         (random-integer 999999999))
                                        ".xml"))))
    (if (not (file-exists? output-dir-path))
        (create-directory output-dir-path))
    (let file-name ((file-path (file-path-gen output-dir-path)))
      (if (file-exists? file-path)
          (file-name (file-path-gen output-dir-path))
          (call-with-output-file
              file-path
            (lambda (file)
              (display "aaaaaaa\n" file)))))))

;;; Output a graph-pool

(define (output-pool graph-pool)
  (let loop ((graph-pool graph-pool)
             (num-graphs 0))
    (if (null? graph-pool)
        num-graphs
        (begin
          (output (car graph-pool))
          (loop (cdr graph-pool)
                (add1 num-graphs))))))

;-------------------------------------------------------------------------------
; Pretty-printing output
;-------------------------------------------------------------------------------

(define (print-graph sxml)
  (define-macro (pp-code-eval . thunk) ; Pretty print the code as it is evatuated
    `(begin
       ,@(apply
          append  ; should better use `map-union' from "sxpathlib.scm"
          (map
           (lambda (s-expr)
             (cond
               ((string? s-expr)  ; string - just display it
                `((display ,s-expr)
                  (newline)))
               ((and (pair? s-expr) (eq? (car s-expr) 'define))
                ; definition - pp and eval it
                `((pp ',s-expr)
                  ,s-expr))
               ((and (pair? s-expr)
                     (memq (car s-expr) '(newline cond-expand)))
                ; just eval it
                `(,s-expr))
               (else  ; for anything else - pp it and pp result
                `((pp ',s-expr)
                  (display "==>")
                  (newline)
                  (pp ,s-expr)
                  (newline)))))
           thunk))))
  (pp-code-eval sxml))
