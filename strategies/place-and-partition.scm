;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Place and partition strategy
;;;
;;; 1. Placing each agent in the an area according to a design algorithm
;;; 2. Agents fight for a better place for themselves
;;; 3. Partition algorithm makes a first approach of the space partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../constants)
(import ../geometry)
(import ../graph)
(import ../visualization)

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
             (list (make-point a b)
                   (make-point (+ (* 50.0 (random-real)) a) (+ (* 50.0 (random-real)) b))
                   (make-point (+ (* 50.0 (random-real)) a) (+ (* 50.0 (random-real)) b))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (let ((a (* limit-x (random-real)))
                     (b (* limit-x (random-real))))
                 (list (make-point a b)
                       (make-point (+ (* 50.0 (random-real)) a) (+ (* 50.0 (random-real)) b))
                       (make-point (+ (* 50.0 (random-real)) a) (+ (* 50.0 (random-real)) b))))
               (agent-proc agent))))
        ,(make-agent
           'bath
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'room1
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'living
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'kitchen
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))))
     (more
      `(,(make-agent
           'distrib
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'storage
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'room2
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent))))
        ,(make-agent
           'room3
           (list (make-point (* limit-x (random-real)) (* limit-y (random-real))))
           (lambda (world agent)
             (make-agent
               (agent-label agent)
               (agent-node-positions agent)
               (agent-proc agent)))))))
    (make-world 
      (append basic-set more)
      '())))

;; Do all initial things with the world prior to simulation
;;
(define (init-world graph world)
  ;; 1st: pull the agents inside if they are outside the limit
  world)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TODO
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

  (visualize-maps)
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

;; Agent type
;;
(define-record-type agent
  (make-agent label node-positions proc)
  agent?
  (label agent-label)
  (node-positions agent-node-positions)
  (proc agent-proc))

;; Agent new state evaluation
;;
(define (agent-new-state agent world)
  (if (agent? agent)
      ((agent-proc agent) world agent)
    (raise "agent-new-state: argument #1 is not an agent")))

;; Agent visualization
;;
(define (visualize-agent a)
  (visualize-when-possible
    'place-and-partition
    (lambda (backend)
      ;; Paint nodes string
      (paint-set-color backend 0.1 0.1 0.1 1.0)
      (paint-set-line-width backend 0.5)
      (paint-path backend (agent-node-positions a))
      ;; Paint nodes
      (for-each
        (lambda (pos)
          (paint-set-color backend 1.0 1.0 1.0 0.9)
          (paint-circle-fill backend (point-x pos) (point-y pos) 5.0)
          (paint-set-color backend 1.0 0.0 0.0 0.9)
          (paint-circle-fill backend (point-x pos) (point-y pos) 3.0))
      (agent-node-positions a))
      ;; Paint label
      (let ((pos (point-list-right-most (agent-node-positions a))))
        (paint-set-color backend 0.4 0.4 0.4 1.0)
        (paint-text backend
                    (symbol->string (agent-label a))
                    "montecarlo"
                    10.0
                    (+ (point-x pos) 9.0)
                    (+ (point-y pos) 3.0)))))
  (visualization:layer-depth-set! 'place-and-partition 10))

;-------------------------------------------------------------------------------
; Maps
;-------------------------------------------------------------------------------

(define (make-maps backend)
  (list 
    (create-image backend)))

(define (visualize-maps)
    (visualize-when-possible
      'maps
      (lambda (backend)
        (let ((images (make-maps backend)))
          (for-each
            (lambda (m)
              (paint-image backend m))
            images))))
    (visualization:layer-depth-set! 'maps 1))

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
