;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms for graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import analysis)
(import generation-elements)
(import geometry)
(import graph)
(import utils/misc)
(import visualization)
(import strategies/predesigned-band)

;-------------------------------------------------------------------------------
; General procedures
;-------------------------------------------------------------------------------

(define (generate-from-model graph)
  (define (select-strategy)
    place-and-partition)
  ((select-strategy) graph))

;-------------------------------------------------------------------------------
; Place and partition generation algorithm
;
; 1. Placing each agent in the an area according to a design algorithm
; 2. Agents fight for a better place for themselves
; 3. Partition algorithm makes a first approach of the space partitioning
;-------------------------------------------------------------------------------

;;; Generation algorithm components type

(define-structure generation-components world-descriptor room-partitioning)

;;; Select components for the algorithm

(define (select-components)
  (make-generation-components
    describe-world-predesigned-band
    make-rooms-from-agents)) ; TODO

;;; Place and partition algorithm

(define (place-and-partition graph)
  (let ((a (select-components)))
    ((generation-components-room-partitioning a)
      graph
      (world-agents
        (evolve-socially
          graph
          (init-world
            graph
            ((generation-components-world-descriptor a) graph)))))))

;;; Do all initial things with the world prior to simulation

(define (init-world graph world)
  ;; 1st: pull the agents inside if they are outside the limit
  world)

;;; Put them in a social environment: evolve them

(define (evolve-socially graph world)
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

  (visualize-world world)
  (if (stop?)
      world
    (evolve-socially 
      graph
      (world-merge-agents
        world
        (agents-receive-new-states)))))

;;; Build geometry from agents' current positions

(define (make-rooms-from-agents graph agents)
  graph)

;;; Field visualization

(define (visualize-field field)
  (visualization:do-later
    'fields
    (lambda (backend)
      (let ((image (visualization:create-image backend))) ; TODO: created in other place
        (visualization:image-set! image field)
        (visualization:paint-image backend image 1.0))))
  (visualization:layer-depth-set! 'fields 2))

;;; Agent visualization

(define (visualize-agent a)
  (visualization:do-later
    'agents
    (lambda (backend)
      ;; Paint nodes string
      (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
      (visualization:paint-set-line-width backend 0.05)
      (visualization:paint-path backend (agent-node-positions a))
      ;; Paint nodes
      (for-each
        (lambda (pos)
          (visualization:paint-set-color backend 1.0 1.0 1.0 0.9)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 0.4)
          (visualization:paint-set-color backend 1.0 0.0 0.0 0.9)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 0.25))
      (agent-node-positions a))
      ;; Paint label
      (let ((pos (point-list-right-most (agent-node-positions a))))
        (visualization:paint-set-color backend 0.4 0.4 0.4 1.0)
        (visualization:paint-text backend
                                  (symbol->string (agent-label a))
                                  "Arial"
                                  0.75
                                  (+ (point-x pos) 0.6)
                                  (+ (point-y pos) 0.2)))))
  (visualization:layer-depth-set! 'agents 20))

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
