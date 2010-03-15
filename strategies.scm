;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strategies define the process to generate a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (process-through-strategies graph)
  (define (select-strategy)
    place-and-partition)
  ((select-strategy) graph))

;; Place and partition
;;
;; Strategy consisting of:
;; 1. Placing each agent in the an area according to a design algorithm
;; 2. Agents fight for a better place for themselves
;; 3. Partition algorithm makes a first approach of the space partitioning
;;
(define (place-and-partition graph)
  (define (make-agents)
    '())
  (define (place-agents agents)
    agents)
  (define (evolve-socially agents)
    agents)
  (define (make-rooms-from-agents agents)
    graph)
  (make-rooms-from-agents
    (evolve-socially
      (place-agents (make-agents)))))
