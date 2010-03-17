;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visual representation of the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import opengl/gl)
(import sdl/sdl)
(import cairo/cairo)

(import global)

(export visualize-when-possible)
(export visualize-now)
(export visualize-now-layers)
(export visualize-now-and-forget)
(export visualize-forget-layers)
(export visualize-forget-all)
(export paint-path)
(export paint-polygon)
(export paint-set-color)
(export paint-circle-fill)
(export paint-circle-border)

(define maxx 500)
(define maxy 500)

(define (representation-cleanup)
  (SDL::exit))

;; Visualization loop, controlled with continuations
;;
(define visualize-loop-with-continuation
  (letrec
    ((layer-selector #f)
     (control-state
       (lambda (return)
         (let* ((cairo-surface (SDL::set-video-mode maxx maxy 0 (+ SDL::hwsurface
                                                                   SDL::hwpalette
                                                                   SDL::doublebuf)))
                (image-surface (cairo-image-surface-create-for-data
                                 (SDL::surface-pixels cairo-surface)
                                 CAIRO_FORMAT_RGB24
                                 maxx
                                 maxy
                                 (SDL::screen-pitch cairo-surface)))
                (cairo (cairo-create image-surface)))
           (set! return (call/cc
                          (lambda (resume-here)
                            (set! control-state resume-here)
                            (return))))
           (let loop ()
             (SDL::delay 4)
             (let ((event (SDL::event-exit)))
               (cond
                ((= event 27) ; 27 = escape TODO!
                 (begin 
                   (SDL::exit)
                   (exit 0)))
                ((= event 32) ; 32 = space TODO!

                 (return))))

             (cairo-set-source-rgba cairo 1.0 1.0 1.0 1.0)
             (cairo-rectangle cairo 0.0 0.0 (exact->inexact maxx) (exact->inexact maxy))
             (cairo-fill cairo)

             (for-each
               (lambda (e)
                 (if (layer-selector e)
                     ((painter-procedure e) cairo)))
               external-painters)

             (SDL::flip cairo-surface)

             (loop))))))

    (lambda (lsel)
      (set! layer-selector lsel)
      (call/cc control-state))))

;; Entry point for visualization with layer selection
(define (immediate-visualization-selector layer-selector)
  (visualize-loop-with-continuation layer-selector))

;; Execute now the sequence of representation of the selected layers
;;
(define (visualize-now-layers layers)
  (immediate-visualization-selector
    (lambda (e)
      (any (lambda (l) (equal? (painter-layer e) l))
           layers))))

;; Execute now the full sequence of representation of the all the layers
;;
(define (visualize-now)
  (immediate-visualization-selector
    (lambda (e) #t)))

;; Execute now the full sequence of representation of the all the layers
;;
(define (visualize-now-and-forget)
  (visualize-now)
  (visualize-forget-all))

;; Remove layer
;;
(define (visualize-forget-layers layers)
  (set! external-painters
    (remove
      (lambda (e)
      (any (lambda (l) (equal? (painter-layer e) l))
           layers))
      external-painters)))

;; Cleans all the external visualization procedures pending of execution
;;
(define (visualize-forget-all)
  (set! external-painters (list (make-painter 'null (lambda (a) '())))))

;; Receive a procedure for visualization from another module, so it can be
;; executed at its right time
;;
(define-structure painter layer procedure)

(define (visualize-when-possible layer new-procedure)
  (append-painter! external-painters (make-painter layer new-procedure)))

(define external-painters (list (make-painter 'null (lambda (a) '())))) ; Why should I add something so it is a list?

(define (append-painter! list-procs proc)
  (if (null? (cdr list-procs))
      (set-cdr! list-procs (list proc))
    (append-painter! (cdr list-procs) proc)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; High-level procedures for painting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Paint a path given a list of 2d points
;;
(define (paint-path cairo points)
  (if (null-list? points)
      (raise "Trying to paint a path with a null list of points")
    (begin
      (cairo-new-path cairo)
      (cairo-move-to cairo
                     (caar points)
                     (cadar points))
      (for-each
        (lambda
          (point)
          (cairo-line-to cairo
                         (car point)
                         (cadr point)))
        (cdr points))
      (cairo-stroke cairo))))

;; Paint a polygon given a list of 2d points
;;
(define (paint-polygon cairo points)
  (if (null-list? points)
    (raise "Trying to paint a polygon with a null list of points"))
    (cairo-new-path cairo)
    (cairo-move-to cairo
                   (caar points)
                   (cadar points))
    (for-each
      (lambda
        (point)
        (cairo-line-to cairo
                       (car point)
                       (cadr point)))
      (cdr points))
    (cairo-close-path cairo)
    (cairo-fill cairo))

;; Set paint color
;;
(define (paint-set-color cairo r g b a)
  (cairo-set-source-rgba cairo r g b a))

;; Paint a fill circle given a point and a radius
;;
(define (paint-circle-fill cairo x y r)
  (cairo-new-path cairo)
  (cairo-move-to cairo x y)
  (cairo-arc cairo x y r 0.0 pi2)
  (cairo-fill cairo))

;; Paint a circle border given a point and a radius
;;
(define (paint-circle-border cairo x y r)
  (cairo-new-path cairo)
  (cairo-move-to cairo x y)
  (cairo-arc cairo x y r 0.0 pi2)
  (cairo-stroke cairo))
