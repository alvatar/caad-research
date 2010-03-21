;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Place and partition strategy
;;;
;;; 1. Placing each agent in the an area according to a design algorithm
;;; 2. Agents fight for a better place for themselves
;;; 3. Partition algorithm makes a first approach of the space partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../graph)
(import ../visualization)
(import ../global)

(import termites)

;; Place and partition algorithm
;;
(define (place-and-partition graph)
  (make-rooms-from-agents
    graph
    (evolve-socially
      graph
      (place-agents
        graph
        (make-agents)))))

;; Make the agents
;;
(define (make-agents)
  (let*
    ((limit-x 400.0) ; TODO
     (limit-y 400.0)
     (basic-set
      `(,(make-agent 'entrance (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'bath (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'room1 (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'living (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'kitchen (* limit-x (random-real)) (* limit-y (random-real)) #f)))
     (more
      `(,(make-agent 'distrib (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'storage (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'room2 (* limit-x (random-real)) (* limit-y (random-real)) #f)
        ,(make-agent 'room3 (* limit-x (random-real)) (* limit-y (random-real)) #f))))
    (append basic-set more)))

;; Place the agents inside the limits
;;
(define (place-agents graph agents)
  ;; 1st: pull the agents inside if they are outside the limit
  agents)

;; Introduce them the social environment: evolve them
;;
(define (evolve-socially graph agents)
  (send-broadcast world agents)
  (visualize-forget-layers '(agents))
  (visualize-agents agents)
  (visualize-now)
  (evolve-socially agents))

    ;(iter (map (lambda (e) (make-agent (agent-label e) (* 400 (random-real)) (* 400 (random-real)) #f)) agents)))

;; Build geometry from agents' current positions
;;
(define (make-rooms-from-agents graph agents)
  graph)

;; Agent type
;;
(define-record-type agent
  (make-agent label x y proc)
  agent?
  (label agent-label)
  (x agent-x)
  (y agent-y)
  (proc agent-proc))

;; Agent visualization
;;
(define (visualize-agents agents)
  (define (visualize-agent a)
    (visualize-when-possible
      'agents
      (lambda (backend)
        (paint-set-color backend 1.0 1.0 1.0 0.9)
        (paint-circle-fill backend (agent-x a) (agent-y a) 6.0)
        (paint-set-color backend 1.0 0.0 0.0 0.9)
        (paint-circle-fill backend (agent-x a) (agent-y a) 3.0)
        (paint-set-color backend 0.3 0.3 0.3 1.0)
        (paint-text backend
          (symbol->string (agent-label a))
          "montecarlo"
          10.0
          (+ (agent-x a) 9.0)
          (+ (agent-y a) 3.0)))))
  (for-each
    visualize-agent
    agents))
