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
    (world-agents
      (evolve-socially
        graph
        (bind-agents
          graph
          (make-world))))))

;; Make the agents and environment
;;
(define (make-world)
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

;; Place the agents inside the limits and bind them to the graph
;;
(define (bind-agents graph world)
  ;; 1st: pull the agents inside if they are outside the limit
  world)

;; Introduce them the social environment: evolve them
;;
(define (evolve-socially graph world)
  (define (stop?) #f)

  (visualize-forget-layers '(place-and-partition))
  (visualize-world world)
  (visualize-now)
  (if (stop?)
      world
    (evolve-socially 
      graph
      (world-merge-cells
        world
        (map ; Produce a list of cells with their new state
          (lambda (a)
            ;; Sends a message with the "world" argument to
            ;; agent a to produce its new state
            (agent-new-state world a)
            '())
          (world-agents world))))))

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

;; World type
;;
(define-record-type world
  (make-world agents maps)
  world?
  (agents world-agents)
  (maps world-maps))

;; World visualization
;;
(define (visualize-world world)
  (define (visualize-agent a)
    (visualize-when-possible
      'place-and-partition
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
    (world-agents world)))
