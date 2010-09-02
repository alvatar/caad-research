;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

(import (std srfi/1
             string/sxml-to-xml)
        core/debugging
        core/list
        sxml-graph)

;;; Relative directory for XML outputs

(define output-dir "xml-output")

;;; Output a single graph

(define output
  (let ((file-path-gen (lambda (dir-path)
                         (string-append dir-path
                                        (number->string
                                         (random-integer 999999999))
                                        ".xml"))))
    (lambda (graph)
      (let get-dir ((output-dir-path (path-normalize output-dir)))
        (if (file-exists? output-dir-path)
            (let gen-file ((file-path (file-path-gen output-dir-path)))
              (if (file-exists? file-path)
                  (gen-file (file-path-gen output-dir-path))
                  (call-with-output-file
                      file-path
                    (lambda (file)
                      (display (sxml->xml-string-fragment
                                (graph->sxml-graph graph))
                               file)))))
            (begin
              (create-directory output-dir-path)
              (get-dir (path-normalize output-dir))))))))

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
