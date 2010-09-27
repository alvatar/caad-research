;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Visualization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1
             srfi/95))
(import core/system-conditional
        core/syntax
        ffi/sdl
        ffi/cairo
        geometry/kernel
        math/exact-algebra
        math/inexact-algebra)

(export maxx maxy)
(export visualization:exit
        visualization:do-later
        visualization:do-now
        visualization:do-now-layers
        visualization:do-loop
        visualization:forget-layers
        visualization:forget-all
        visualization:layer-depth-set!
        visualization:paint-path
        visualization:paint-polygon
        visualization:paint-set-color
        visualization:paint-circle-fill
        visualization:paint-circle-border
        visualization:paint-set-line-cap
        visualization:paint-set-line-width
        visualization:paint-set-line-style
        visualization:paint-text
        visualization:create-image
        visualization:paint-image
        visualization:image-set!
        visualization:translate
        visualization:scale
        visualization:reset-transformations
        visualization:pseq-now
        visualization:point-list-now
        visualization:point-now
        visualization:line-now)

(define maxx 500)
(define maxy 500)

;;; Force visualization cleanup

(define (visualization:exit)
  (SDL::exit))

;;; Visualization control routine

(define visualization-control
  (let* ((osx-only (%if-sys "Darwin" (SDL::init-osx)))
         (error (SDL::init SDL::init-video)) ; TODO: check this error
         (sdl-surface (SDL::set-video-mode maxx maxy 0 (+ SDL::hwsurface
                                                          SDL::hwpalette
                                                          SDL::doublebuf)))
         (image-surface (cairo-image-surface-create-for-data
                         (SDL::surface-pixels sdl-surface)
                         CAIRO_FORMAT_RGB24
                         maxx
                         maxy
                         (SDL::screen-pitch sdl-surface)))
         (cairo (cairo-create image-surface))
         (draw-frame (lambda (layer-selector)
                       (cairo-set-source-rgba cairo 1.0 1.0 1.0 1.0)
                       (cairo-rectangle cairo 0.0 0.0 (exact->inexact maxx) (exact->inexact maxy))
                       (cairo-fill cairo)

                       (for-each
                        (lambda (e)
                          (if (layer-selector e)
                              ((painter-procedure e) cairo visualization-environment)))
                        external-painters)

                       (SDL::flip sdl-surface))))
    (lambda (layer-selector mode)
      (let/cc leave
              (let loop ()
                ;; (SDL::delay 20)
                (let ((event (SDL::event-exit)))
                  (cond
                   ((= event 27)        ; 27 = escape TODO!
                    (begin 
                      (SDL::exit)
                      (exit 0)))
                   ((= event 32)        ; 32 = space TODO!
                    (leave))))
                (draw-frame layer-selector)
                (cond
                 ((equal? mode 'single-frame) mode)
                 ((equal? mode 'loop) (loop))
                 (else (error "wrong draw mode specified"))))))))

;-------------------------------------------------------------------------------
; Visualization environment
;-------------------------------------------------------------------------------

(define-structure visualization:environment scale translation)

(define visualization-environment (make-visualization:environment
                                    (make-vect2 0.0 0.0)
                                    (make-vect2 0.0 0.0)))

;-------------------------------------------------------------------------------
; Painters
;-------------------------------------------------------------------------------

;;; Execute now the sequence of representation of the selected layers

(define (visualization:do-now-layers layers)
  (visualization-control
   (lambda (e)
     (any (lambda (l)
            (equal? (painter-layer e) l))
          layers))
   'single-frame))

;;; Execute now the full sequence of representation of the all the layers

(define (visualization:do-now)
  (visualization-control
   (lambda (e) #t)
   'single-frame))

;;; Execute now the representation sequence in a loop, controlled by SDL

(define (visualization:do-loop)
  (visualization-control
   (lambda (e) #t)
   'loop))

;;; Remove layer

(define (visualization:forget-layers layers)
  (set! external-painters
        (remove
         (lambda (e)
           (any (lambda (l)
                  (equal? (painter-layer e) l))
                layers))
         external-painters)))

;;; Cleans all the external visualization procedures pending of execution

(define (visualization:forget-all)
  (set! external-painters (list (make-painter '%0 0 (lambda (backend env-vis) '())))))

;;; Does this layer exist already?

(define (visualization:layer-exists? layer)
  (any
   (lambda (p)
     (equal? (painter-layer p) layer))
   external-painters))

;;; Layer depth

(define (visualization:layer-depth layer)
  (painter-depth (find        ; Only looks at first occurence of layer
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
   external-painters)
  (sort!
   external-painters
   (lambda (p1 p2)
     (< (painter-depth p1) (painter-depth p2)))))

;;; Receive a procedure for visualization from another module, so it can be
;;; executed at its right time

(define-structure painter layer depth procedure)

(define (visualization:do-later layer new-procedure #!optional depth)
  (append-painter! external-painters (make-painter
                                      layer
                                      (if (visualization:layer-exists? layer)
                                          (visualization:layer-depth layer)
                                          0)
                                      new-procedure))
  (if depth (visualization:layer-depth-set! layer depth)))

(define external-painters (list (make-painter '%0 0 (lambda (backend env-vis) '())))) ; Why should I add something so it is a list?

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
                 (exact->inexact (vect2-x (car points)))
                 (exact->inexact (vect2-y (car points))))
  (for-each
   (lambda (point)
     (cairo-line-to cairo
                    (exact->inexact (vect2-x point))
                    (exact->inexact (vect2-y point))))
   (cdr points))
  (cairo-stroke cairo))

;;; Paint a polygon given a list of 2d points

(define (visualization:paint-polygon cairo points)
  (if (null? points)
      (error "Trying to paint a polygon with a null list of points"))
  (cairo-new-path cairo)
  (cairo-move-to cairo
                 (exact->inexact (vect2-x (car points)))
                 (exact->inexact (vect2-y (car points))))
  (for-each
   (lambda (point)
     (cairo-line-to cairo
                    (exact->inexact (vect2-x point))
                    (exact->inexact (vect2-y point))))
   (cdr points))
  (cairo-close-path cairo)
  (cairo-fill cairo))

;;; Set paint color

(define (visualization:paint-set-color cairo r g b a)
  (cairo-set-source-rgba cairo r g b a))

;;; Paint a fill circle given a point and a radius

(define (visualization:paint-circle-fill cairo x y r) ;; TODO INEXACT
  (cairo-new-path cairo)
  (cairo-move-to cairo (exact->inexact x) (exact->inexact y))
  (cairo-arc cairo (exact->inexact x) (exact->inexact y) (exact->inexact r) 0.0 ~pi2)
  (cairo-fill cairo))

;;; Paint a circle border given a point and a radius

(define (visualization:paint-circle-border cairo x y r) ;; TODO INEXACT 
  (cairo-new-path cairo)
  (cairo-move-to cairo (exact->inexact x) (exact->inexact y))
  (cairo-arc cairo (exact->inexact x) (exact->inexact y) (exact->inexact r) 0.0 ~pi2)
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
  (cairo-move-to cairo (exact->inexact x) (exact->inexact y))
  (cairo-show-text cairo text))

;;; Create image surface

(define (visualization:create-image cairo)
  (make-cairo-a8-image cairo maxx maxy))

;;; Paint image

(define (visualization:paint-image cairo image alpha)
  (cairo-set-source-surface cairo (data-image-surface-ptr image) 0.0 0.0)
  (cairo-paint-with-alpha cairo alpha))

;;; Set image

(define (visualization:image-set! image vect)
  (cairo-a8-image-set! image vect))

;;; Translate world

(define (visualization:translate cairo vec)
  (cairo-translate cairo (exact->inexact (vect2-x vec)) (exact->inexact (vect2-y vec)))
  (visualization:environment-translation-set! visualization-environment vec))

;;; Scale world

(define (visualization:scale cairo vec)
  (cairo-scale cairo (exact->inexact (vect2-x vec)) (exact->inexact (vect2-y vec)))
  (visualization:environment-scale-set! visualization-environment vec))

;;; Reset transformations

(define (visualization:reset-transformations cairo)
  (cairo-identity-matrix cairo)
  (set! visualization-environment (make-visualization:environment
                                    (make-vect2 0.0 0.0)
                                    (make-vect2 0.0 0.0))))

;-------------------------------------------------------------------------------
; Utility and convenience functions
;-------------------------------------------------------------------------------

;;; Draw now a path in green

(define (visualization:pseq-now pseq)
  (visualization:do-later
   '%now-helpers
   (lambda (backend vis-env)
     (visualization:paint-set-line-width backend 0.1)
     (visualization:paint-set-color backend 0.0 1.0 0.0 1.0)
     (visualization:paint-path backend pseq)))
  (visualization:layer-depth-set! '%now-helpers 100)
  (visualization:do-now)
  pseq)

;;; Draw now a list of points in green

(define (visualization:point-list-now diam plis)
  (visualization:do-later
   '%now-helpers
   (lambda (backend vis-env)
     (for-each
      (lambda (p) 
        (visualization:paint-set-color backend 0.0 1.0 0.0 1.0)
        (visualization:paint-circle-fill backend (vect2-x p) (vect2-y p) diam))
      plis)))
  (visualization:layer-depth-set! '%now-helpers 100)
  (visualization:do-now)
  plis)

;;; Draw now a point in green

(define (visualization:point-now diam p)
  (visualization:do-later
   '%now-helpers
   (lambda (backend vis-env)
     (visualization:paint-set-color backend 0.0 1.0 0.0 1.0)
     (visualization:paint-circle-fill backend (vect2-x p) (vect2-y p) diam)))
  (visualization:layer-depth-set! '%now-helpers 100)
  (visualization:do-now)
  p)

;;; Draw a line in red

(define (visualization:line-now line)
  (visualization:do-later
   '%now-helpers
   (lambda (backend vis-env)
     (visualization:paint-set-color backend 1.0 0.0 0.0 1.0)
     (visualization:paint-set-line-cap backend 'square)
     (visualization:paint-set-line-width backend .02)
     (visualization:paint-path backend (segment->pseq (line->segment line -100.0 100.0)))))
  (visualization:layer-depth-set! '%now-helpers 81)
  (visualization:do-now)
  line)
