;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Place and partition strategy
;;;
;;; 1. Placing each agent in the an area according to a design algorithm
;;; 2. Agents fight for a better place for themselves
;;; 3. Partition algorithm makes a first approach of the space partitioning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../termite/termite)
(import ../graph)
(import ../visualization)
(import ../global)

(define (place-and-partition graph)
  (make-rooms-from-agents
    graph
    (evolve-socially
      (place-agents (make-agents graph)))))

(define (make-agents graph)
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
    (iter (map (lambda (e) (make-agent (agent-label e) (* 400 (random-real)) (* 400 (random-real)) )) agents)))
    ;agents)
  (iter agents))
(define (make-rooms-from-agents graph agents)
  graph)

(define-record-type agent
  (make-agent label x y)
  agent?
  (label agent-label)
  (x agent-x)
  (y agent-y))

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

;-------------------------------------------------------------------------------
; Agents
;-------------------------------------------------------------------------------

(define-type cell
  id: 713cb0a4-16ea-4b18-a18e-7a9e33e7b92b
  unprintable:
  pid)

(define (cell obj)
  (make-cell
   (spawn
     (lambda ()
       (cell-loop obj)))))

(define (cell-loop obj)
  (recv
    ((from tag 'ref)
     (! from (list tag obj))
     (cell-loop obj))
   
    (('set! obj) 
     (cell-loop obj))))

(define (cell-ref c) (!? (cell-pid c) 'ref))
(define (cell-set! c obj) (! (cell-pid c) (list 'set! obj)))
