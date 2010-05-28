;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: Forces system with agents
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/debug)
(import ../core/syntax)
(import ../geometry/kernel)
(import ../auxiliary-operations)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)
(import ../generation-elements)
(import ../graph)
(import ../visualization)
(import element-interactions)


(define (agents-forces-system graph world)
  (define (stop? agents)
    (every
     (lambda (a)
       (vect2:~=e
        (car (agent-positions a))
        (car (agent-memory a))
        0.01))
     agents))
  (let* ((wall-pseq-list (map (lambda (w) (wall-pseq w)) (graph:find-walls graph)))
         (pipes-center-list (map (lambda (p) (pipe-position p)) (graph:find-pipes graph)))
         (entry-points (entry-pseq (car (graph:find-entries graph))))
         (north (car (graph-environment graph))))
    (let loop ((agents (world-agents world)))
      (visualize-world (make-world agents '()) graph)
      (visualization:do-now)
      (if (stop? agents)
          (values
           graph
           (make-world 
            agents
            (world-fields world)))
          (loop
           (map
            (lambda (a)
              (let ((a-label (agent-label a)))
                (cond
                 ((equal? a-label 'distribution)
                  (make-agent
                   a-label
                   (let ((pos (car (agent-positions a))))
                     (list
                      (translation:point
                       pos
                       (vect2+
                        (vect2:*scalar
                         (agent-walls-interaction pos wall-pseq-list) -1.0)
                        (vect2:*scalar
                         (agent-pipes-interaction pos pipes-center-list) 0.4)
                        (vect2:*scalar
                         (agent-entry-interaction pos entry-points) 0.6)))))
                   (agent-positions a)
                   (agent-proc a)))
                 ((equal? a-label 'kitchen)
                  (make-agent
                   a-label
                   (let ((pos (car (agent-positions a))))
                     (list
                      (translation:point
                       pos
                       (vect2+
                        (vect2:*scalar
                         (agent-walls-interaction pos wall-pseq-list) -1.0)
                        (vect2:*scalar
                         (agent-pipes-interaction pos pipes-center-list) 0.3)))))
                   (agent-positions a)
                   (agent-proc a)))
                 ((equal? a-label 'living)
                  (make-agent
                   a-label
                   (let ((pos (car (agent-positions a))))
                     (list
                      (translation:point
                       pos
                       (vect2+
                        (vect2:*scalar
                         (agent-walls-interaction pos wall-pseq-list) -1.0)
                        (vect2:*scalar (north->south north) 1.4)))))
                   (agent-positions a)
                   (agent-proc a)))
                 ((equal? a-label 'room1)
                  (make-agent
                   a-label
                   (let ((pos (car (agent-positions a))))
                     (list
                      (translation:point
                       pos
                       (vect2+
                        (vect2:*scalar
                         (agent-walls-interaction pos wall-pseq-list) -1.0)
                        (vect2:*scalar (north->north-east north) 1.0)))))
                   (agent-positions a)
                   (agent-proc a)))
                 ((equal? a-label 'room2)
                  (make-agent
                   a-label
                   (let ((pos (car (agent-positions a))))
                     (list
                      (translation:point
                       pos
                       (vect2+
                        (vect2:*scalar
                         (agent-walls-interaction pos wall-pseq-list) -1.0)
                        (vect2:*scalar (north->north-east north) 1.0)))))
                   (agent-positions a)
                   (agent-proc a)))
                 ((equal? a-label 'room3)
                  (make-agent
                   a-label
                   (let ((pos (car (agent-positions a))))
                     (list
                      (translation:point
                       pos
                       (vect2+
                        (vect2:*scalar
                         (agent-walls-interaction pos wall-pseq-list) -1.0)
                        (vect2:*scalar (north->north-east north) 1.0)))))
                   (agent-positions a)
                   (agent-proc a)))
                 (else
                  (error "iteration-step-2: Unhandled agent type:" a-label)))))
            agents))))))
