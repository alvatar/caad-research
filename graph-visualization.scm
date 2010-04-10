;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A predesigned band strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import analysis)
(import geometry)
(import graph)
(import math)
(import utils/misc)
(import visualization)

;-------------------------------------------------------------------------------
; Visualization output
;-------------------------------------------------------------------------------

;;; Draw graph

(define (visualize-graph graph)
  (let* ((limits (graph-bounding-box graph))
         (size-vec (vect2-vect2 (point->vect2 (cadr limits))
                                (point->vect2 (car limits))))
         (frame-factor 0.7)
         (max-scale-x (/ maxx (* (vect2-u size-vec))))
         (max-scale-y (/ maxy (* (vect2-v size-vec))))
         (vis-scale (* frame-factor (min max-scale-x max-scale-y))))

  (visualization:do-later
    'graph
    (lambda (backend vis-env)
      ;; Paint wall
      (define (paint-wall wall)
        (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
        (visualization:paint-set-line-cap backend 'square)
        (visualization:paint-set-line-width backend .25)
        (visualization:paint-path backend (wall->point-list wall)))
      ;; Paint doors in the wall
      (define (paint-doors-in-wall wall)
        (for-each
          (lambda
            (door)
            (visualization:paint-set-line-cap backend 'butt)
            (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
            (visualization:paint-set-line-width backend 0.25)
            (visualization:paint-path backend (wall-element->point-list door wall))
            (visualization:paint-set-color backend 1.0 0.1 0.1 1.0)
            (visualization:paint-set-line-width backend 0.15)
            (visualization:paint-path backend (wall-element->point-list door wall)))
          (wall-doors wall)))
      ;; Paint windows in the wall
      (define (paint-windows-in-wall wall)
        (visualization:paint-set-color backend 1.0 1.0 0.1 1.0)
        (visualization:paint-set-line-cap backend 'butt)
        (visualization:paint-set-line-width backend 0.1)
        (for-each
          (lambda
            (window)
            (visualization:paint-path backend (wall-element->point-list window wall)))
          (wall-windows wall)))
      ;; Paint structural
      (define (paint-structural structural)
        (let ((structural-points (structural->point-list structural graph)))
        (visualization:paint-set-line-width backend 0.02)
        (visualization:paint-set-color backend 0.2 0.2 0.2 1.0)
        (visualization:paint-polygon backend structural-points)
        (visualization:paint-set-color backend 0.0 0.0 0.0 1.0)
        (visualization:paint-path backend (snoc structural-points (car structural-points)))))
      ;; Paint room
      (define (paint-room room)
        (visualization:paint-set-color backend 0.0 0.0 0.3 0.3)
        (visualization:paint-polygon backend (room->point-list graph room)))
      ;; Paint entry
      (define (paint-entry entry)
        (let ((door-mid-point (segment-mid-point (entry->point-list entry graph))))
          (visualization:paint-set-color backend 1.0 0.45 0.45 0.4)
          (visualization:paint-circle-fill backend
                                           (point-x door-mid-point)
                                           (point-y door-mid-point)
                                           0.4)))
      ;; Paint pipe
      (define (paint-pipe pipe)
        (let ((pos (pipe-position pipe)))
          (visualization:paint-set-line-width backend 0.02)
          (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 0.2)
          (visualization:paint-set-color backend 0.0 0.0 0.0 1.0)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 0.15)
          (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 0.1)))

      (for-each
        (lambda
          (elem)
          (if (null? elem)
              (raise "Malformed SXML")
            (cond
              ((equal? (car elem) 'wall)
               (paint-wall
                 elem)
               (paint-windows-in-wall 
                 elem)
               (paint-doors-in-wall 
                 elem))
              ((equal? (car elem) 'structural)
               (paint-structural elem))
              ((equal? (car elem) 'room)
               (paint-room elem))
              ((equal? (car elem) 'entry)
               (paint-entry elem))
              ((equal? (car elem) 'pipe)
               (paint-pipe elem)))))
        (graph-parts graph))))
  (visualization:layer-depth-set! 'graph 80)

  (visualization:do-later
    'visual-aids
    (let ((x-dir-mark `(,(make-point (- (exact->inexact maxx)) 0.0)
                        ,(make-point (exact->inexact maxx) 0.0)))
          (y-dir-mark `(,(make-point 0.0 (- (exact->inexact maxy)))
                        ,(make-point 0.0 (exact->inexact maxy)))))
      (lambda (backend vis-env)
        (visualization:paint-set-line-style backend '(0.1 0.1))
        (visualization:paint-set-line-width backend 0.1)
        (visualization:paint-set-color backend 0.0 0.0 0.0 0.4)
        (visualization:paint-path backend x-dir-mark)
        (visualization:paint-path backend y-dir-mark)
        (visualization:paint-set-line-style backend '()))))
  (visualization:layer-depth-set! 'visual-aids 70)
  
  (visualization:do-later
    '%framing
    (lambda (backend vis-env)
      (visualization:translate backend (vect2-inverse (point->vect2 (car limits))))
      (visualization:scale backend (make-vect2 vis-scale vis-scale))
      (visualization:translate
        backend
        (make-vect2
          (/ (* (- maxx (* vis-scale (vect2-u size-vec))) 0.5) vis-scale)
          (/ (* (- maxy (* vis-scale (vect2-v size-vec))) 0.5) vis-scale))))) ; 0.5 stands for half the displacement
  (visualization:layer-depth-set! '%framing 0)

  (visualization:do-later
    '%cleanup
    (lambda (backend vis-env)
      (visualization:reset-transformations backend)))
  (visualization:layer-depth-set! '%cleanup 99)))
