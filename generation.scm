;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms for graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import analysis)
(import fields-2d)
(import generation-elements)
(import geometry)
(import graph)
(import math)
(import utils/misc)
(import visualization)
(import strategies/predesigned-band)

;-------------------------------------------------------------------------------
; General procedures
;-------------------------------------------------------------------------------

;;; Generate graph from a base model graph (any graph taken for modification...)

(define (generate-from-model graph)
  (define (execute-step it-steps term-preds graph world)
    (cond
     ((or (null? it-steps) (null? term-preds))
      graph)
     (((car term-preds) graph world)
      (execute-step (cdr it-steps) (cdr term-preds) graph world))
     (else
      (receive (g w)
        ((car it-steps) graph world)
        (execute-step it-steps term-preds g w)))))
  (let ((components (select-components)))
    (execute-step
      (generation-components-iteration-steps components)
      (generation-components-termination-predicates components)
      graph
      '())))

;;; Generation algorithm components type
;;; iteration-steps: must return a values construct with "graph" and "world"
;;; termination-predicates: must return whether this step should be terminated

(define-structure generation-components iteration-steps termination-predicates)

;;; Select components for the algorithm

(define (select-components)
  place-and-partition)

;-------------------------------------------------------------------------------
; Place and partition generation algorithms
;
; 1. Placing each agent in the an area according to a design algorithm
; 2. Agents fight for a better place for themselves
; 3. Partition algorithm makes a first approach of the space partitioning
;-------------------------------------------------------------------------------

;;; Put them in a social environment: evolve them

(define (evolve-mono-nodal-agents graph world)
  ;; Stop condition
  (define (stop?) #f)
  ;; Distribution method
  (define (agents-receive-new-states)
    (map ; Produce a list of cells with their new state
      (lambda (a)
        ;; Sends a message with the "world" argument to
        ;; agents, in order to let them produce their new state
        (agent-new-state a world))
      (world-agents world)))
  (define (world-merge-agents world agents)
    (make-world agents (world-fields world)))

  (visualize-world world graph)
  (if (stop?)
      (values graph world)
    (evolve-mono-nodal-agents
      graph
      (world-merge-agents
        world
        (agents-receive-new-states)))))

;;; Pseudo-geometrization of agents: make them multimodal

(define (make-multi-nodal-agents graph world)
  (values graph world))

;;; Build geometry from agents' current positions

(define (make-rooms-from-agents graph agents)
  (values graph world))

;;; Algorithm parts put together

(define place-and-partition
  (make-generation-components
    (list describe-world-predesigned-band ; TODO: which world description?
          evolve-mono-nodal-agents
          make-multi-nodal-agents
          make-rooms-from-agents)
    (list (lambda (graph world) (not (null? world)))
          (lambda (graph world) #f))))

;-------------------------------------------------------------------------------
; Visualization of elements
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
      (let ((pos (point-list-right-most (agent-node-positions a))))
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
         (size-vec (vect2- (cadr bb)
                           (car bb))))
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
