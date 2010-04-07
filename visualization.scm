;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visualization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std srfi/95))
(import opengl/gl)
(import sdl/sdl)
(import cairo/cairo)

(import geometry)
(import math)

(export maxx)
(export maxy)
(export visualization:do-later)
(export visualization:do-now)
(export visualization:do-now-layers)
(export visualization:forget-layers)
(export visualization:forget-all)
(export visualization:layer-depth-set!)
(export visualization:paint-path)
(export visualization:paint-polygon)
(export visualization:paint-set-color)
(export visualization:paint-circle-fill)
(export visualization:paint-circle-border)
(export visualization:paint-set-line-cap)
(export visualization:paint-set-line-width)
(export visualization:paint-set-line-style)
(export visualization:paint-text)
(export visualization:create-image)
(export visualization:paint-image)
(export visualization:image-set!)
(export visualization:translate)

(define maxx 500)
(define maxy 500)

(define (representation-cleanup)
  (SDL::exit))

;;; Visualization loop, controlled with continuations

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
             (SDL::delay 40)
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
             ;(return)

             (loop))))))

    (lambda (lsel)
      (set! layer-selector lsel)
      (call/cc control-state))))

;;; Entry point for visualization with layer selection

(define (immediate-visualization-selector layer-selector)
  (visualize-loop-with-continuation layer-selector))

;;; Execute now the sequence of representation of the selected layers

(define (visualization:do-now-layers layers)
  (immediate-visualization-selector
    (lambda (e)
      (any (lambda (l) (equal? (painter-layer e) l))
           layers))))

;;; Execute now the full sequence of representation of the all the layers

(define (visualization:do-now)
  (immediate-visualization-selector
    (lambda (e) #t)))

;;; Remove layer

(define (visualization:forget-layers layers)
  (set! external-painters
    (remove
      (lambda (e)
      (any (lambda (l) (equal? (painter-layer e) l))
           layers))
      external-painters)))

;;; Cleans all the external visualization procedures pending of execution

(define (visualization:forget-all)
  (set! external-painters (list (make-painter '%0 0 (lambda (a) '())))))

;;; Does this layer exist already?

(define (visualization:layer-exists? layer)
  (any
    (lambda (p)
      (equal? (painter-layer p) layer))
    external-painters))

;;; Layer depth

(define (visualization:layer-depth layer)
  (painter-depth (find ; Only looks at first occurence of layer
                   (lambda (p)
                     (equal? (painter-layer p) layer))
                   external-painters)))

;;; Layer depth set

(define (visualization:layer-depth-set! layer depth)
  (for-each
    (lambda (p)
      (if (equal? (painter-layer p) layer)
        (painter-depth-set! p depth)
        p))
    external-painters))

;;; Receive a procedure for visualization from another module, so it can be
;;; executed at its right time

(define-structure painter layer depth procedure)

(define (visualization:do-later layer new-procedure)
  (append-painter! external-painters (make-painter
                                       layer
                                       (if (visualization:layer-exists? layer)
                                           (visualization:layer-depth layer)
                                         0)
                                       new-procedure))
  (sort!
    external-painters
    (lambda (p1 p2)
      (< (painter-depth p1) (painter-depth p2)))))

(define external-painters (list (make-painter '%0 0 (lambda (a) '())))) ; Why should I add something so it is a list?

(define (append-painter! list-procs proc)
  (if (null? (cdr list-procs))
      (set-cdr! list-procs (list proc))
    (append-painter! (cdr list-procs) proc)))

;-------------------------------------------------------------------------------
; High-level procedures for painting
;-------------------------------------------------------------------------------

;;; Paint a path given a list of 2d points

(define (visualization:paint-path cairo points)
  (if (null? points)
      (error "Trying to paint a path with a null list of points"))
  (cairo-new-path cairo)
  (cairo-move-to cairo
                 (point-x (car points))
                 (point-y (car points)))
  (for-each
    (lambda (point)
      (cairo-line-to cairo
                     (point-x point)
                     (point-y point)))
    (cdr points))
  (cairo-stroke cairo))

;;; Paint a polygon given a list of 2d points

(define (visualization:paint-polygon cairo points)
  (if (null? points)
      (error "Trying to paint a polygon with a null list of points"))
  (cairo-new-path cairo)
  (cairo-move-to cairo
                 (point-x (car points))
                 (point-y (car points)))
  (for-each
    (lambda (point)
      (cairo-line-to cairo
                     (point-x point)
                     (point-y point)))
    (cdr points))
  (cairo-close-path cairo)
  (cairo-fill cairo))

;;; Set paint color

(define (visualization:paint-set-color cairo r g b a)
  (cairo-set-source-rgba cairo r g b a))

;;; Paint a fill circle given a point and a radius

(define (visualization:paint-circle-fill cairo x y r)
  (cairo-new-path cairo)
  (cairo-move-to cairo x y)
  (cairo-arc cairo x y r 0.0 pi2)
  (cairo-fill cairo))

;;; Paint a circle border given a point and a radius

(define (visualization:paint-circle-border cairo x y r)
  (cairo-new-path cairo)
  (cairo-move-to cairo x y)
  (cairo-arc cairo x y r 0.0 pi2)
  (cairo-stroke cairo))

;;; Set line cap

(define (visualization:paint-set-line-cap cairo style)
  (cond
    ((equal? style 'square)
     (cairo-set-line-cap cairo CAIRO_LINE_CAP_SQUARE))
    ((equal? style 'butt)
     (cairo-set-line-cap cairo CAIRO_LINE_CAP_BUTT))))

;;; Set line width

(define (visualization:paint-set-line-width cairo width)
  (cairo-set-line-width cairo width))

;;; Set line style

(define (visualization:paint-set-line-style cairo style-as-list)
  (if (null? style-as-list)
      (cairo-restore-line-style cairo)
    (cairo-set-line-style cairo style-as-list)))

;;; Paint text

(define (visualization:paint-text cairo text font size x y)
  (cairo-set-font-size cairo size)
  (cairo-select-font-face cairo font CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_BOLD)
  (cairo-move-to cairo x y)
  (cairo-show-text cairo text))

;;; Create image surface

(define (visualization:create-image cairo)
  (make-cairo-a8-image cairo maxx maxy))

;;; Paint image

(define (visualization:paint-image cairo image)
  (cairo-set-source-surface cairo (data-image-surface-ptr image) 0.0 0.0)
  (cairo-paint cairo))

;;; Set image

(define (visualization:image-set! image vect)
  (cairo-a8-image-set! image vect))

;;; Translate world

(define (visualization:translate cairo x y)
  (cairo-translate cairo x y))
