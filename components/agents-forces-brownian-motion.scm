;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: Agent forces system with some brownian motion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/syntax)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../graph)
(import ../math/exact-algebra)
(import ../visualization)
(import auxiliary-element-interrelations)

(define (agents-forces-brownian-motion graph world)
  (let* ((wall-pseq-list (map (lambda (w) (wall-pseq w)) (graph:find-walls graph))))
    (let loop ((counter 0)
               (agents (world-agents world)))
      (visualize-world (make-world agents '()) graph)
      (visualization:do-now)
      (if (>= counter 4)                ; stop condition
          (values graph
                  (make-world agents
                              (world-fields world)))
          (loop
           (add1 counter)
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
                         (agent-walls-interaction pos wall-pseq-list) -0.5)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'kitchen)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'living)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room1)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room2)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room3)) -1.0)
                        (vect2:random)
                        ))))
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
                         (agent-walls-interaction pos wall-pseq-list) -0.5)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'distribution)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'living)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room1)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room2)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room3)) -1.0)
                        (vect2:random)
                        ))))
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
                         (agent-walls-interaction pos wall-pseq-list) -0.5)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'kitchen)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'distribution)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room1)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room2)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room3)) -1.0)
                        (vect2:random)
                        ))))
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
                         (agent-walls-interaction pos wall-pseq-list) -0.5)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'kitchen)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'distribution)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'living)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room2)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room3)) -1.0)
                        (vect2:random)
                        ))))
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
                         (agent-walls-interaction pos wall-pseq-list) -0.5)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'kitchen)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'distribution)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'living)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room1)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room3)) -1.0)
                        (vect2:random)
                        ))))
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
                         (agent-walls-interaction pos wall-pseq-list) -0.5)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'kitchen)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'distribution)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'living)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room1)) -1.0)
                        (vect2:*scalar
                         (agent-agent-interaction a (find-agent agents 'room2)) -1.0)
                        (vect2:random)
                        ))))
                   (agent-positions a)
                   (agent-proc a)))
                 (else
                  (error "iteration-step-3: Unhandled agent type:" a-label)))))
            agents))))))
