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
(import graph)

(export visualize-graph) ; TODO: Move to graph.scm
(export visualize-when-possible)
(export visualize-now)
(export visualize-from-scratch)
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
    ((graph #f)
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

               (graph-to-cairo graph cairo)
#|
             (cond
              ((equal? layer 'graph)
               (graph-to-cairo graph cairo))
              ((equal? layer 'external)
               (for-each
                 (lambda (e)
                   (e cairo))
                 external-procedures)))
                 |#

             (SDL::flip cairo-surface)

             (loop))))))

    (lambda (g)
      (set! graph g)
      (call/cc control-state))))

;; Visualization entry points
;;
(define (visualize-graph graph)
  (visualize-loop-with-continuation graph))

;; Draw graph
;;
(define (graph-to-cairo graph cairo)
  ;; Paint wall
  (define (paint-wall wall)
    (cairo-set-source-rgba cairo 0.1 0.1 0.1 1.0)
    (cairo-set-line-cap cairo CAIRO_LINE_CAP_SQUARE)
    (cairo-set-line-width cairo 5.0)
    (paint-path cairo (extract-wall-points wall)))
  ;; Paint doors in the wall
  (define (paint-doors-in-wall wall)
    (for-each
      (lambda
        (door)
        (cairo-set-line-cap cairo CAIRO_LINE_CAP_BUTT)
        (cairo-set-source-rgba cairo 1.0 1.0 1.0 1.0)
        (cairo-set-line-width cairo 6.0)
        (paint-path cairo (extract-wall-element-points door wall))
        (cairo-set-source-rgba cairo 1.0 0.1 0.1 1.0)
        (cairo-set-line-width cairo 3.0)
        (paint-path cairo (extract-wall-element-points door wall)))
      (wall-doors wall)))
  ;; Paint windows in the wall
  (define (paint-windows-in-wall wall)
    (cairo-set-source-rgba cairo 1.0 1.0 0.1 1.0)
    (cairo-set-line-cap cairo CAIRO_LINE_CAP_BUTT)
    (cairo-set-line-width cairo 3.0)
    (for-each
      (lambda
        (window)
        (paint-path cairo (extract-wall-element-points window wall)))
      (wall-windows wall)))
  ;; Paint pilar
  (define (paint-pilar pilar)
    (cairo-new-path cairo)
    (cairo-stroke cairo)
    '())
  ;; Paint room
  (define (paint-room graph room)
    ;(cairo-set-source-rgba cairo (random-real) (random-real) (random-real) 0.5)
    (cairo-set-source-rgba cairo 0.0 0.0 0.0 0.5)
    (paint-polygon cairo (extract-room-points graph room)))
  ;; Paint entry
  (define (paint-entry wall)
    (cairo-new-path cairo)
    (cairo-stroke cairo)
    '())
  ;; Paint pipe
  (define (paint-pipe wall)
    (cairo-new-path cairo)
    (cairo-stroke cairo)
    '())

  (for-each
    (lambda
      (elem)
      (if (null-list? elem)
          (raise "Malformed SXML")
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
    (graph-parts graph)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; External access to representation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Receive a procedure for visualization from another module, so it can be
;; executed at its right time
;;
(define (visualize-when-possible new-procedure)
  (append-visualization-procedure! external-procedures new-procedure))

(define external-procedures (list (lambda (a) '()))) ; Why should I add something so it is a list?
(define (append-visualization-procedure! list-procs proc)
  (if (null? (cdr list-procs))
      (set-cdr! list-procs (list proc))
    (append-visualization-procedure! (cdr list-procs) proc)))

;; Execute now the full sequence of representation of the graph and external
;; visualization procedures
;;
(define (visualize-now)
  '())

;; Cleans all the external visualization procedures pending of execution
;;
(define (visualize-from-scratch)
  '())

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
