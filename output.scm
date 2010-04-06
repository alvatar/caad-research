;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Output procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import analysis)
(import graph)
(import visualization)



(define (output)
  (raise "File output not implemented"))

;-------------------------------------------------------------------------------
; Visualization output
;-------------------------------------------------------------------------------

;;; Draw graph

(define (visualize-graph graph)
  (visualization:do-later
    'graph
    (lambda (backend)
      ;; Paint wall
      (define (paint-wall wall)
        (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
        (visualization:paint-set-line-cap backend 'square)
        (visualization:paint-set-line-width backend 5.0)
        (visualization:paint-path backend (wall->point-list wall)))
      ;; Paint doors in the wall
      (define (paint-doors-in-wall wall)
        (for-each
          (lambda
            (door)
            (visualization:paint-set-line-cap backend 'butt)
            (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
            (visualization:paint-set-line-width backend 6.0)
            (visualization:paint-path backend (wall-element->point-list door wall))
            (visualization:paint-set-color backend 1.0 0.1 0.1 1.0)
            (visualization:paint-set-line-width backend 3.0)
            (visualization:paint-path backend (wall-element->point-list door wall)))
          (wall-doors wall)))
      ;; Paint windows in the wall
      (define (paint-windows-in-wall wall)
        (visualization:paint-set-color backend 1.0 1.0 0.1 1.0)
        (visualization:paint-set-line-cap backend 'butt)
        (visualization:paint-set-line-width backend 3.0)
        (for-each
          (lambda
            (window)
            (visualization:paint-path backend (wall-element->point-list window wall)))
          (wall-windows wall)))
      ;; Paint pilar
      (define (paint-pilar pilar)
        '())
      ;; Paint room
      (define (paint-room graph room)
        (visualization:paint-set-color backend 0.0 0.0 0.3 0.3)
        (visualization:paint-polygon backend (room->point-list graph room)))
      ;; Paint entry
      (define (paint-entry wall)
        '())
      ;; Paint pipe
      (define (paint-pipe wall)
        '())

      (for-each
        (lambda
          (elem)
          (if (null-list? elem)
              (error "Malformed SXML")
            (cond
              ((equal? (car elem) 'wall)
               (paint-wall
                 elem)
               (paint-windows-in-wall 
                 elem)
               (paint-doors-in-wall 
                 elem))
              ((equal? (car elem) 'pilar)
               (paint-pilar elem))
              ((equal? (car elem) 'room)
               (paint-room graph elem))
              ((equal? (car elem) 'entry)
               ;(paint-entry (make-wall-list-from-uids (make-uid-list elem) graph)))
               '())
              ((equal? (car elem) 'pipe)
               ;(paint-pipe (make-wall-list-from-uids (make-uid-list elem) graph))))))
               '()))))
        (graph-parts graph))))
  (visualization:layer-depth-set! 'graph 5))

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
