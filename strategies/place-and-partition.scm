;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Place and partition strategy
;;;
;;; 1. Placing each agent in the an area according to a design algorithm
;;; 2. Agents fight for a better place for themselves
;;; 3. Partition algorithm makes a first approach of the space partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../visualization)
(import ../geometry)
(import ../global)
(import ../graph)

;; Place and partition algorithm
;;
(define (place-and-partition graph)
  (make-rooms-from-agents
    graph
    (world-agents
      (evolve-socially
        graph
        (init-world
          graph
          (describe-world))))))

;; Create all necessary things to begin simulation
;;
(define (describe-world)
  (let*
    ((limit-x 400.0) ; TODO
     (limit-y 400.0)
     (basic-set
      `(,(make-agent
           'entrance
           (let ((a (* limit-x (random-real)))
                 (b (* limit-x (random-real))))
             (list (make-node (make-point a b))
                   (make-node (make-point (+ 50.0 a) (+ 50.0 b)))))
           (lambda (world agent) agent))
        ,(make-agent
           'bath
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))
        ,(make-agent
           'room1
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))
        ,(make-agent
           'living
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))
        ,(make-agent
           'kitchen
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))))
     (more
      `(,(make-agent
           'distrib
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))
        ,(make-agent
           'storage
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))
        ,(make-agent
           'room2
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent))
        ,(make-agent
           'room3
           (list (make-node (make-point (* limit-x (random-real)) (* limit-y (random-real)))))
           (lambda (world agent) agent)))))
    (make-world 
      (append basic-set more)
      (make-maps))))

;; Do all initial things with the world prior to simulation
;;
(define (init-world graph world)
  ;; 1st: pull the agents inside if they are outside the limit
  world)

;; Introduce them the social environment: evolve them
;;
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
    (make-world agents (world-maps world)))

  (visualize-forget-layers '(place-and-partition))
  (visualize-world world)
  (visualize-now)
  (if (stop?)
      world
    (evolve-socially 
      graph
      (world-merge-agents
        world
        (agents-receive-new-states)))))

;; Build geometry from agents' current positions
;;
(define (make-rooms-from-agents graph agents)
  graph)

;-------------------------------------------------------------------------------
; Agents
;-------------------------------------------------------------------------------

;; Node type
;;
(define-record-type node
  (make-node position)
  node?
  (position node-position))

;; Agent type
;;
(define-record-type agent
  (make-agent label nodes proc)
  agent?
  (label agent-label)
  (nodes agent-nodes)
  (proc agent-proc))

;; Agent new state evaluation
;;
(define (agent-new-state agent world)
  (if (agent? agent)
      ((agent-proc agent) world agent)
    (raise "agent-new-state: argument #1 is not an agent")))

;; Calculate agent's center of gravity
;;
(define (agent-center-of-gravity agent)
      (define (iter center nodes-tail)
        (cond
         ((null? nodes-tail)
          center)
         (else
          (iter
            (mid-point
              center
              (node-position (car nodes-tail)))
            (cdr nodes-tail)))))
  (if (agent? agent)
      (let ((nodes (agent-nodes agent)))
        (iter (node-position (car nodes)) (cdr nodes)))
    (raise "agent-center-of-gravity: argument #1 is not an agent")))

(define (visualize-agent a)
  (visualize-when-possible
    'place-and-partition
    (lambda (backend)
      (let ((pos (agent-center-of-gravity a)))
        (paint-set-color backend 0.3 0.3 0.3 1.0)
        (paint-text backend
                    (symbol->string (agent-label a))
                    "montecarlo"
                    10.0
                    (+ (point-x pos) 9.0)
                    (+ (point-y pos) 3.0)))
      (for-each
        (lambda (n)
          (let ((pos (node-position n)))
            (paint-set-color backend 1.0 1.0 1.0 0.9)
            (paint-circle-fill backend (point-x pos) (point-y pos) 6.0)
            (paint-set-color backend 1.0 0.0 0.0 0.9)
            (paint-circle-fill backend (point-x pos) (point-y pos) 3.0)))
      (agent-nodes a)))))

;-------------------------------------------------------------------------------
; Maps
;-------------------------------------------------------------------------------

(define (make-map data)
  data)

(define (make-maps)
  '())

;-------------------------------------------------------------------------------
; World
;-------------------------------------------------------------------------------

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
  (for-each
    visualize-agent
    (world-agents world)))
