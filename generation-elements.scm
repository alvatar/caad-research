;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elements used by generations algorithms and strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import analysis)
(import geometry)
(import graph)
(import utils/misc)
(import visualization)

;-------------------------------------------------------------------------------
; Agents
;-------------------------------------------------------------------------------

;;; Agent type

(define-structure agent label node-positions proc)

;;; Agent new state evaluation

(define (agent-new-state agent world)
  (if (agent? agent)
      ((agent-proc agent) world agent)
    (error "agent-new-state: argument #1 is not an agent")))

;;; Agent visualization

(define (visualize-agent a)
  (visualization:do-later
    'agents
    (lambda (backend)
      ;; Paint nodes string
      (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
      (visualization:paint-set-line-width backend 0.5)
      (visualization:paint-path backend (agent-node-positions a))
      ;; Paint nodes
      (for-each
        (lambda (pos)
          (visualization:paint-set-color backend 1.0 1.0 1.0 0.9)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 5.0)
          (visualization:paint-set-color backend 1.0 0.0 0.0 0.9)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 3.0))
      (agent-node-positions a))
      ;; Paint label
      (let ((pos (point-list-right-most (agent-node-positions a))))
        (visualization:paint-set-color backend 0.4 0.4 0.4 1.0)
        (visualization:paint-text backend
                                  (symbol->string (agent-label a))
                                  "Arial"
                                  10.0
                                  (+ (point-x pos) 9.0)
                                  (+ (point-y pos) 3.0)))))
  (visualization:layer-depth-set! 'agents 10))

;-------------------------------------------------------------------------------
; Fields
;-------------------------------------------------------------------------------

;;; Visualize light field

(define (visualize-field field)
  (visualization:do-later
    'fields
    (lambda (backend)
      (let ((image (visualization:create-image backend))) ; TODO: created in other place
        (visualization:image-set! image field)
        (visualization:paint-image backend image))))
  (visualization:layer-depth-set! 'fields 1))

;;; Make light field

(define (make-light-field graph size-x size-y)
  (merge-2d-u8fields
    (let ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
          (light-sources (map*
                           inexact-point->exact-point
                           (all-wall-element-points-all-walls->point-list 'window graph))))
      (map ; produces a field per light-source
        (lambda (source)
            (cond
             ((point? source)
              (make-2d-scaled-u8field
                4
                size-x
                size-y
                (lambda (p) (if (point-in-polygon? limit-polygon p)
                                (let ((d (fx* 2 (fx-distance-point-point p source))))
                                  (if (> d 255) 255 d))
                              0))))
             ((= (length source) 2)
              (make-2d-scaled-u8field
                4
                size-x
                size-y
                (lambda (p) (if (point-in-polygon? limit-polygon p)
                                (let ((d (fx* 2 (fx-distance-point-segment p source))))
                                  (if (> d 255) 255 d))
                              0))))
             ((>= (length source) 3)
              (make-2d-scaled-u8field
                4
                size-x
                size-y
                (lambda (p) 0.7)))))
      light-sources))
    (lambda (a b)
      (let ((sum (fx- (fx+ a b) 255)))
        (if (fx< sum 0) 0 sum)))))

;-------------------------------------------------------------------------------
; World
;-------------------------------------------------------------------------------

;;; World type

(define-structure world agents fields)

;;; World visualization

(define (visualize-world world)
  (for-each
    visualize-field
    (world-fields world))
  (for-each
    visualize-agent
    (world-agents world))
  (visualization:do-now)
  (visualization:forget-layers '(agents fields)))
