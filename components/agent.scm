;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Agents used by components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../geometry/kernel
        ../geometry/query
        ../graph
        ../math/exact-algebra
        ../visualization)

;;; Agent type

(define-structure agent label positions memory proc)

;;; Move agent

(define (move-agent a new-pos)
  (make-agent (agent-label a)
              new-pos
              (agent-memory a)
              (agent-proc a)))

;;; Move agent head

(define (move-agent-head a new-pos)
  (move-agent a (list new-pos)))

;;; Agent head position

(define (agent-head-position a)
  (car (agent-positions a)))

;;; Agent new state evaluation

(define (agent-new-state agent world)
  ((agent-proc agent) world agent))

;;; Find an agent given a list

(define (find-agent agents label)
  (find (lambda (a) (equal? label (agent-label a))) agents))

;;; Agent visualization

(define (visualize-agent a)
  (visualization:do-later
    'agents
    (lambda (backend vis-env)
      ;; Paint nodes string
      (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
      (visualization:paint-set-line-width backend 0.05)
      (visualization:paint-path backend (agent-positions a))
      ;; Paint nodes
      (for-each
        (lambda (pos)
          (visualization:paint-set-color backend 1.0 1.0 1.0 0.9)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.2)
          (visualization:paint-set-color backend 1.0 0.0 0.0 0.9)
          (visualization:paint-circle-fill backend (vect2-x pos) (vect2-y pos) 0.1))
        (agent-positions a))
      ;; Paint trace
      (map-in-order ; FIXME: this should be for-each with different list lengths
        (lambda (pos-a pos-b)
          (if (not-null? pos-b)
              (begin (visualization:paint-set-color backend 1.0 1.0 0.0 1.0)
                     (visualization:paint-set-line-width backend 0.2)
                     (visualization:paint-path backend (list pos-a pos-b)))))
        (agent-positions a)
        (agent-memory a))
      ;; Paint label
      (let ((pos (pseq:extreme-right (agent-positions a))))
        (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
        (visualization:paint-text backend
                                  (symbol->string (agent-label a))
                                  "Arial"
                                  0.3
                                  (+ (vect2-x pos) 0.3)
                                  (+ (vect2-y pos) 0.1))))
    90))
