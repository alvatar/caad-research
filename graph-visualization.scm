;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A predesigned band strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import core/list)
(import geometry/kernel)
(import math/exact-algebra)

(import operations-low)
(import graph)
(import visualization)

;-------------------------------------------------------------------------------
; Visualization output
;-------------------------------------------------------------------------------

;;; Draw graph

(define (visualize-graph graph)
  (let* ((limits (graph:bounding-box graph))
         (size-vec (bounding-box:size-segment limits))
         (frame-factor 0.7)
         (max-scale-x (/ maxx (* (vect2-x size-vec))))
         (max-scale-y (/ maxy (* (vect2-y size-vec))))
         (vis-scale (* frame-factor (min max-scale-x max-scale-y))))

  (visualization:do-later
    'graph
    (lambda (backend vis-env)
      ;; Paint wall
      (define (paint-wall wall)
        (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
        (visualization:paint-set-line-cap backend 'square)
        (visualization:paint-set-line-width backend .25)
        (visualization:paint-path backend (wall-pseq wall))
        (paint-windows-in-wall wall)
        (paint-doors-in-wall wall))
      ;; Paint doors in the wall
      (define (paint-doors-in-wall wall)
        (for-each
         (lambda (door)
            (visualization:paint-set-line-cap backend 'butt)
            (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
            (visualization:paint-set-line-width backend 0.25)
            (visualization:paint-path backend (door-pseq door))
            (visualization:paint-set-color backend 1.0 0.1 0.1 1.0)
            (visualization:paint-set-line-width backend 0.15)
            (visualization:paint-path backend (door-pseq door)))
          (wall-doors wall)))
      ;; Paint windows in the wall
      (define (paint-windows-in-wall wall)
        (visualization:paint-set-color backend 1.0 1.0 0.1 1.0)
        (visualization:paint-set-line-cap backend 'butt)
        (visualization:paint-set-line-width backend 0.1)
        (for-each
          (lambda (window)
            (visualization:paint-path backend (window-pseq window)))
          (wall-windows wall)))
      ;; Paint structural
      (define (paint-structural structural)
        (let ((pts (structural-pseq structural)))
        (visualization:paint-set-line-width backend 0.02)
        (visualization:paint-set-color backend 0.2 0.2 0.2 1.0)
        (visualization:paint-polygon backend pts)
        (visualization:paint-set-color backend 0.0 0.0 0.0 1.0)
        (visualization:paint-path backend pts)))
      ;; Paint room
      (define (paint-room room)
        (visualization:paint-set-color backend 0.0 0.0 0.3 0.3)
        (visualization:paint-polygon backend (graph:room->pseq graph room)))
      ;; Paint entry
      (define (paint-entry entry)
        (let ((door-mid-point (segment:mid-point (entry-pseq entry))))
          (visualization:paint-set-color backend 1.0 0.45 0.45 0.4)
          (visualization:paint-circle-fill backend
                                           (vect2-x door-mid-point)
                                           (vect2-y door-mid-point)
                                           0.4)))
      ;; Paint pipe
      (define (paint-pipe pipe)
        (let ((pos (pipe-position pipe)))
          (visualization:paint-set-line-width backend 0.02)
          (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.2)
          (visualization:paint-set-color backend 0.0 0.0 0.0 1.0)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.15)
          (visualization:paint-set-color backend 1.0 1.0 1.0 1.0)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.1)))

      (for-each
        (lambda
          (elem)
          (if (null? elem)
              (raise "Malformed SXML")
            (cond
              ((wall? elem)
               (paint-wall elem))
              ((structural? elem)
               (paint-structural elem))
              ((room? elem)
               (paint-room elem))
              ((entry? elem)
               ;(paint-entry elem))
               '())
              ((pipe? elem)
               (paint-pipe elem)))))
        (graph-architecture graph))))
  (visualization:layer-depth-set! 'graph 80)

  (visualization:do-later
    'visual-aids
    (let ((x-dir-mark `(,(make-vect2 (- (exact->inexact maxx)) 0.0)
                        ,(make-vect2 (exact->inexact maxx) 0.0)))
          (y-dir-mark `(,(make-vect2 0.0 (- (exact->inexact maxy)))
                        ,(make-vect2 0.0 (exact->inexact maxy)))))
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
      (visualization:translate backend (vect2:symmetric (bounding-box-lefttop limits)))
      (visualization:scale backend (make-vect2 vis-scale vis-scale))
      (visualization:translate
        backend
        (make-vect2
          (/ (* (- maxx (* vis-scale (vect2-x size-vec))) 0.5) vis-scale)
          (/ (* (- maxy (* vis-scale (vect2-y size-vec))) 0.5) vis-scale))))) ; 0.5 stands for half the displacement
  (visualization:layer-depth-set! '%framing 0)

  (visualization:do-later
    '%cleanup
    (lambda (backend vis-env)
      (visualization:reset-transformations backend)))
  (visualization:layer-depth-set! '%cleanup 99)))
