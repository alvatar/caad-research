;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Place and partition strategy
;;;
;;; 1. Placing each agent in the an area according to a design algorithm
;;; 2. Agents fight for a better place for themselves
;;; 3. Partition algorithm makes a first approach of the space partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../constants)
(import ../geometry)
(import ../graph)
(import ../utils/misc)
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
          (describe-world graph))))))

;; Create all necessary things to begin simulation
;;
(define (describe-world graph)
  (let*
    ((limit-x 500) ; TODO
     (limit-y 500)
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
               (agent-proc agent))))))
     (light-field (make-light-field graph limit-x limit-y)))
    (make-world 
      (append basic-set more)
      (list light-field))))

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
    (make-world agents (world-fields world)))

  (visualize-world world)
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
  (visualization:do-later
    'agents
    (lambda (backend)
      ;; Paint nodes string
      (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
      (visualization:paint-set-line-width backend 0.5)
      (visualization:paint-path backend (agent-node-positions a))
      ;; Paint nodes
      (for-each
        (lambda (pos)
          (visualization:paint-set-color backend 1.0 1.0 1.0 0.9)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 5.0)
          (visualization:paint-set-color backend 1.0 0.0 0.0 0.9)
          (visualization:paint-circle-fill backend (point-x pos) (point-y pos) 3.0))
      (agent-node-positions a))
      ;; Paint label
      (let ((pos (point-list-right-most (agent-node-positions a))))
        (visualization:paint-set-color backend 0.4 0.4 0.4 1.0)
        (visualization:paint-text backend
                                  (symbol->string (agent-label a))
                                  "Arial"
                                  10.0
                                  (+ (point-x pos) 9.0)
                                  (+ (point-y pos) 3.0)))))
  (visualization:layer-depth-set! 'agents 10))

;-------------------------------------------------------------------------------
; Fields
;-------------------------------------------------------------------------------

(define (visualize-field field)
  (visualization:do-later
    'fields
    (lambda (backend)
      (let ((image (visualization:create-image backend))) ; TODO: created in other place
        (visualization:image-set! image field)
        (visualization:paint-image backend image))))
  (visualization:layer-depth-set! 'fields 1))

;; Produce 2d fields with a lambda
;;
(define (produce-2d-field size-x size-y proc)
  (let ((limit-x (- size-x 1))
        (limit-y (- size-y 1)))
    (define (iter x y lis)
      (cond
       ((and (= x 0) (= y 0))
        lis)
       ((= x 0)
        (iter limit-x (- y 1) (cons (proc (make-point 0 y)) lis)))
       (else
        (iter (- x 1) y (cons (proc (make-point x y)) lis)))))
    (iter limit-x limit-y '())))
#|
(define (produce-2d-field size-x size-y proc)
  (let ((len (* size-x size-y)))
    (do ((vec (make-u8vector len))
         (i 0 (+ i 1)))
        ((>= i len) vec)
      (u8vector-set! vec i (proc (make-point
                                         (modulo i size-y)
                                         (floor (/ i size-y))))))))
  |#

;; Flatten a list of fields (merge them)
;;
(define (flatten-2d-fields field-list)
  (list->u8vector
    (reduce
      (lambda (f1 f2)
        (map (lambda (a b) (+ a b)) f1 f2))
      '()
      field-list)))

;; Make light field
;;
(define (make-light-field graph size-x size-y)
  (time
  (flatten-2d-fields
    (let ((light-sources `(,(make-point 200 80)))) ; TODO
      (map ; produces a field per light-source
        (lambda (s)
            (cond
             ((point? s)
              (produce-2d-field
                size-x
                size-y
                (lambda (p) (let ((d (distance-point-point-integer p s)))
                              (if (> d 255) 255 d)))))
             ((= (length s) 2)
              (produce-2d-field-list
                size-x
                size-y
                (lambda (p) 0.6)))
             ((>= (length s) 3)
              (produce-2d-field-list
                size-x
                size-y
                (lambda (p) 0.7)))))
      light-sources)))))

;-------------------------------------------------------------------------------
; World
;-------------------------------------------------------------------------------

;; World type
;;
(define-structure world agents fields)

;; World visualization
;;
(define (visualize-world world)
  (for-each
    visualize-field
    (world-fields world))
  (for-each
    visualize-agent
    (world-agents world))
  (visualization:do-now)
  (visualization:forget-layers '(agents fields)))
