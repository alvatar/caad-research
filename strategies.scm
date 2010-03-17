;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strategies define the process to generate a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import termite/termite)

(import global)
(import graph)
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
    (let*
      ((limit-x (graph-limit-x graph))
       (limit-y (graph-limit-y graph))
       (basic-set
        `(,(make-agent 'entrance (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'bath (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'room1 (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'living (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'kitchen (* limit-x (random-real)) (* limit-y (random-real)))))
       (more
        `(,(make-agent 'distrib (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'storage (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'room2 (* limit-x (random-real)) (* limit-y (random-real)))
          ,(make-agent 'room3 (* limit-x (random-real)) (* limit-y (random-real))))))
      (append basic-set more)))
  (define (place-agents agents)
    ;; 1st: pull the agents inside if they are outside the limit
    agents)
  (define (evolve-socially agents)
    (define (iter agents)
      (visualize-forget-layers '(agents))
      (visualize-agents agents)
      (visualize-now)
      ;(iter (map (lambda (e) (make-agent 'asdf (* 400 (random-real)) (* 400 (random-real)) )) agents)))
      agents)
    (iter agents))
  (define (make-rooms-from-agents agents)
    graph)

  (make-rooms-from-agents
    (evolve-socially
      (place-agents (make-agents)))))

(define-structure agent label x y)

(define (visualize-agents agents)
  (define (visualize-agent a)
    (visualize-when-possible
      'agents
      (lambda (backend)
        (paint-set-color backend 0.0 0.0 0.0 1.0)
        (paint-text backend (symbol->string (agent-label a)) "Arial" 10.0 100.0 100.0)
        (paint-set-color backend 1.0 1.0 1.0 0.9)
        (paint-circle-fill backend (agent-x a) (agent-y a) 6.0)
        (paint-set-color backend 1.0 0.0 0.0 0.9)
        (paint-circle-fill backend (agent-x a) (agent-y a) 3.0))))
  (for-each
    visualize-agent
    agents))
