;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strategies define the process to generate a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import termite/termite)
(import visualization)

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
    `(,(make-agent 'entrance 20.0 30.0)
      ,(make-agent 'distrib 30.0 90.0)
      ,(make-agent 'storage 40.0 20.0)
      ,(make-agent 'bath 50.0 50.0)
      ,(make-agent 'room1 60.0 180.0)
      ,(make-agent 'room2 70.0 30.0)
      ,(make-agent 'room3 80.0 40.0)
      ,(make-agent 'living 190.0 30.0)
      ,(make-agent 'kitchen 100.0 10.0)))
  (define (place-agents agents)
    (pp agents)
    agents)
  (define (evolve-socially agents)
    (visualize-agents agents)
    agents)
  (define (make-rooms-from-agents agents)
    graph)
  (make-rooms-from-agents
    (evolve-socially
      (place-agents (make-agents)))))

(define-structure agent label x y)

(define (visualize-agents agents)
  (define (visualize-agent a)
    (visualize-when-possible
      (lambda (backend)
        (paint-set-color backend 0.0 0.0 0.0 0.7)
        (paint-circle-fill backend (agent-x a) (agent-y a) 10.0))))
  (for-each
    visualize-agent
    agents))
