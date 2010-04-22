;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elements used by generations algorithms and strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import analysis)
(import math)
(import fields-2d)
(import graph)
(import geometry)
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

;;; Find an agent given a list

(define (find-agent agents label)
  (find
    (lambda (a) (equal? label (agent-label a)))
    agents))

;-------------------------------------------------------------------------------
; World
;-------------------------------------------------------------------------------

;;; World type

(define-structure world agents fields)

;-------------------------------------------------------------------------------
; Visualization
;-------------------------------------------------------------------------------

;;; Field visualization

(define (visualize-field field size-vec)
  (visualization:do-later
    'fields
    (lambda (backend vis-env)
      (let* ((image (visualization:create-image backend)) ; TODO: created in other place
             (max-dim (max (vect2-x size-vec) (vect2-y size-vec)))
             (image-scale (vect2*
                            (make-vect2 max-dim max-dim)
                            (make-vect2 (inverse maxx) (inverse maxy)))))
        (visualization:scale backend image-scale)
        (visualization:image-set! image (u8-2dfield-data field))
        (visualization:paint-image backend image 0.5)
        (visualization:scale backend (vect2:1/vect2 image-scale)))))
  (visualization:layer-depth-set! 'fields 10))

;;; Agent visualization

(define (visualize-agent a)
  (visualization:do-later
    'agents
    (lambda (backend vis-env)
      ;; Paint nodes string
      (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
      (visualization:paint-set-line-width backend 0.05)
      (visualization:paint-path backend (agent-node-positions a))
      ;; Paint nodes
      (for-each
        (lambda (pos)
          (visualization:paint-set-color backend 1.0 1.0 1.0 0.9)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.4)
          (visualization:paint-set-color backend 1.0 0.0 0.0 0.9)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.25))
      (agent-node-positions a))
      ;; Paint label
      (let ((pos (polysegment:extreme-right (agent-node-positions a))))
        (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
        (visualization:paint-text backend
                                  (symbol->string (agent-label a))
                                  "Arial"
                                  0.75
                                  (+ (vect2-x pos) 0.6)
                                  (+ (vect2-y pos) 0.2)))))
  (visualization:layer-depth-set! 'agents 90))

;;; World visualization

(define (visualize-world world graph)
  (let* ((bb (graph-bounding-box graph))
         (size-vec (bounding-box:size-segment bb)))
    #|
    (for-each
      (lambda (f) (visualize-field f size-vec))
      (world-fields world))
      |#
    (for-each
      visualize-agent
      (world-agents world))
    (visualization:do-now)
    (visualization:forget-layers '(agents fields))))
