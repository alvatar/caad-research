;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A predesigned band strategy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../analysis)
(import ../fields-2d)
(import ../fields/entries)
(import ../fields/light)
(import ../fields/pipes)
(import ../fields/structure)
(import ../geometry)
(import ../generation-elements)
(import ../graph)
(import ../math)

;;; Create all necessary things to begin simulation

(define (describe-world-predesigned-band graph world)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (bb-vect (segment-vector (point-list->bounding-box limit-polygon)))
     (bb-x (vect2-x bb-vect))
     (bb-y (vect2-y bb-vect))
     (light-field (make-light-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (entries-field (make-entries-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (structure-field (make-structure-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (pipes-field (make-pipes-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (path-list (wall-list->path-list (graph-walls graph)))
     (agents (list
       (make-agent
         'distribution
         (list (random-point-in-polygon limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))

                   #;(pp (vect2:*scalar
                       (field-least-potential-vector structure-field pos) 1.0))
               (list
                 (point-translation
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos path-list) 0.01)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) 0.2)
                     (vect2:*scalar
                       (field-least-potential-vector entries-field pos) 1.0)))))
             (agent-proc a))))
       (make-agent
         'kitchen
         (list (random-point-in-polygon limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (point-translation
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos path-list) 0.01)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) 0.2)))))
             (agent-proc a))))
       (make-agent
         'living
         (list (random-point-in-polygon limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (point-translation
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos path-list) 0.01)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) 0.2)))))
             (agent-proc a))))
       (make-agent
         'room1
         (list (random-point-in-polygon limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (point-translation
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos path-list) 0.01)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) 0.2)))))
             (agent-proc a))))
       (make-agent
         'room2
         (list (random-point-in-polygon limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (point-translation
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos path-list) 0.01)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) 0.2)))))
             (agent-proc a))))
       (make-agent
         'room3
         (list (random-point-in-polygon limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (point-translation
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos path-list) 0.01)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) 0.2)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) 0.2)))))
             (agent-proc a)))))))
       (values
         graph
         (make-world 
           agents
           (list ;light-field
                 entries-field
                 ;structure-field
                 ;pipes-field
                 )))))

;;; Agent-agent interaction vector

(define (agent-agent-interaction agent1 agent2)
  (let* ((pos1 (car (agent-node-positions agent1)))
         (pos2 (car (agent-node-positions agent2)))
         (vec (vect2- pos2 pos1)))
    (vect2:/scalar (vect2- vec) (vect2:squaremagnitude vec))))

;;; Calculate least potential vector given a field and a point in it

(define (field-least-potential-vector field pos)
  (define (make-coords center)
    (map
      (lambda (c)
        (u8-2dfield-coords->reflective-coords
          field
          (make-vect2
            (fx+ (car c) (vect2-x center))
            (fx+ (cadr c) (vect2-y center)))))
      '((0 0)
        (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1) (0 -1) (1 -1)
        (2 0) (2 1) (2 2) (1 2) (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-2 -1) (-2 -2) (-1 -2) (0 -2) (1 -2) (2 -2) (2 -1))))
  (pp pos)
  (pp (u8-2dfield-position->coords field pos))
  (pp (u8-2dfield-position->value field pos))
  (make-vect2 0.0 0.0)
  #;(cadr
    (fold
      (lambda (c current-max)
        (let ((value-in-coords (u8-2dfield-coords->value field c)))
          (if (> value-in-coords (car current-max))
              (list value-in-coords (vect2- c pos))
            current-max)))
      (list 0 (make-vect2 0 0))
      (make-coords (u8-2dfield-position->coords field pos)))))

;;; Agent-walls interaction vector

(define (agent-walls-interaction agent-pos path-list)
  (fold
    (lambda (p vec)
      (vect2+ (vect2-
                (mid-point (segment-first-point p) (segment-second-point p))
                agent-pos)
              vec))
    (make-vect2 0.0 0.0)
    path-list))
