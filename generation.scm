;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms for graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import graph)
(import generation-elements)
(import utils/misc)
(import strategies/predesigned-band)

;-------------------------------------------------------------------------------
; General procedures
;-------------------------------------------------------------------------------

;;; Generation algorithm components type
;;; iteration-steps: must return a values construct with "graph" and "world"
;;; termination-predicates: must return whether this step should be terminated

(define-structure generation-components iteration-steps termination-predicates)

;;; Generate graph feeding it an initial graph

(define (generate-from-graph graph)
  (define (execute-step it-steps term-preds graph world)
    (cond
     ((or (null? it-steps) (null? term-preds))
      graph)
     (((car term-preds) graph world) ; execute predicate
      (execute-step (cdr it-steps) (cdr term-preds) graph world))
     (else
      (receive (g w) ; execute iteration step
        ((car it-steps) graph world)
        (execute-step it-steps term-preds g w)))))
  (let ((components (select-components)))
    (execute-step
      (generation-components-iteration-steps components)
      (generation-components-termination-predicates components)
      graph
      '())))

;;; Export generation algorithm

(define (generator seed-data)
  (cond
    ((graph? seed-data)
     (generate-from-graph seed-data))
    (else
     (error "generator: No generation algorithm for this kind of seed data"))))

;;; Select components for the algorithm

(define (select-components)
  agents-based-algorithm-components)

;-------------------------------------------------------------------------------
; Agents-based place and partition generation algorithms
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

(define agents-based-algorithm-components
  (make-generation-components
    (car predesigned-band)
    (cadr predesigned-band)))
