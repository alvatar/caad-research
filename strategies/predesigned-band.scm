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

;-------------------------------------------------------------------------------
; Iteration steps and termination predicates
;-------------------------------------------------------------------------------

;;; Iteration step 1

(define (predesigned-band-iteration-step-1 graph world)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (bb-vect (segment:direction (bounding-box:diagonal-segment (polysegment:bounding-box limit-polygon))))
     (bb-x (vect2-x bb-vect))
     (bb-y (vect2-y bb-vect))
     (agents (list
       (make-agent
         'distribution
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a) a))
       (make-agent
         'kitchen
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a) a))
       (make-agent
         'living
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a) a))
       (make-agent
         'room1
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a) a))
       (make-agent
         'room2
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a) a))
       (make-agent
         'room3
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a) a)))))

       (values
         graph
         (make-world 
           agents
           '()))))

;;; Termination predicate 1

(define (predesigned-band-termination-predicate-1 graph world)
  (not (null? world)))

;;; Iteration step 2

(define (predesigned-band-iteration-step-2 graph world)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (bb-vect (segment:direction (bounding-box:diagonal-segment (polysegment:bounding-box limit-polygon))))
     (bb-x (vect2-x bb-vect))
     (bb-y (vect2-y bb-vect))
     (wall-path-list (wall-list->path-list (graph-walls graph)))
     (pipes-center-list (pipes-list->center-positions (graph-pipes graph)))
     (entry-path (entry->point-list graph (car (graph-entries graph)))) ; TODO: only one entry taken into account
     (north (graph-north graph))
     (agents
       (map
         (lambda (a)
           (let ((a-label (agent-label a)))
             (cond
              ((equal? a-label 'distribution)
               (make-agent
                 a-label
                 (let ((pos (car (agent-node-positions a))))
                   (list
                     (translation:point
                       pos
                       (vect2+
                         (vect2:*scalar
                           (agent-walls-interaction pos wall-path-list) -1.0)
                         (vect2:*scalar
                           (agent-pipes-interaction pos pipes-center-list) 0.4)
                         (vect2:*scalar
                           (agent-entry-interaction pos entry-path) 0.6)))))
                 (agent-proc a)))
              ((equal? a-label 'kitchen)
               (make-agent
                 a-label
                 (let ((pos (car (agent-node-positions a))))
                   (list
                     (translation:point
                       pos
                       (vect2+
                         (vect2:*scalar
                           (agent-walls-interaction pos wall-path-list) -1.0)
                         (vect2:*scalar
                           (agent-pipes-interaction pos pipes-center-list) 0.3)))))
                 (agent-proc a)))
              ((equal? a-label 'living)
               (make-agent
                 a-label
                 (let ((pos (car (agent-node-positions a))))
                   (list
                     (translation:point
                       pos
                       (vect2+
                         (vect2:*scalar
                           (agent-walls-interaction pos wall-path-list) -1.0)
                         (vect2:*scalar (north->south north) 1.4)))))
                 (agent-proc a)))
              ((equal? a-label 'room1)
               (make-agent
                 a-label
                 (let ((pos (car (agent-node-positions a))))
                   (list
                     (translation:point
                       pos
                       (vect2+
                         (vect2:*scalar
                           (agent-walls-interaction pos wall-path-list) -1.0)
                         (vect2:*scalar (north->north-east north) 1.0)))))
                 (agent-proc a)))
              ((equal? a-label 'room2)
               (make-agent
                 a-label
                 (let ((pos (car (agent-node-positions a))))
                   (list
                     (translation:point
                       pos
                       (vect2+
                         (vect2:*scalar
                           (agent-walls-interaction pos wall-path-list) -1.0)
                         (vect2:*scalar (north->north-east north) 1.0)))))
                 (agent-proc a)))
              ((equal? a-label 'room3)
               (make-agent
                 a-label
                 (let ((pos (car (agent-node-positions a))))
                   (list
                     (translation:point
                       pos
                       (vect2+
                         (vect2:*scalar
                           (agent-walls-interaction pos wall-path-list) -1.0)
                         (vect2:*scalar (north->north-east north) 1.0)))))
                 (agent-proc a)))
              (else
                (error "predesigned-band-iteration-step-2: Unhandled agent type:" a-label)))))
         (world-agents world))))
     (visualize-world world graph)
     (values
       graph
       (make-world 
         agents
         '()))))

;;; Termination predicate 2

(define (predesigned-band-termination-predicate-2 graph world)
  #f)

;;; Iteration step 3

(define (predesigned-band-iteration-step-3 graph world)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (bb-vect (segment:direction (bounding-box:diagonal-segment (polysegment:bounding-box limit-polygon))))
     (bb-x (vect2-x bb-vect))
     (bb-y (vect2-y bb-vect))
     ;(light-field (make-light-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     ;(entries-field (make-entries-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     ;(structure-field (make-structure-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     ;(pipes-field (make-pipes-field graph graph-space-size-x graph-space-size-y bb-x bb-y limit-polygon))
     (wall-path-list (wall-list->path-list (graph-walls graph)))
     (pipes-center-list (pipes-list->center-positions (graph-pipes graph)))
     (entry-path (entry->point-list graph (car (graph-entries graph)))) ; TODO: only one entry taken into account
     (agents (list
       (make-agent
         'distribution
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (translation:point
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos wall-path-list) -1.0)
                     #|
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) -0.5)
                       |#
                     #|
                     (vect2:*scalar
                       (field-least-potential-vector structure-field pos) -0.05)
                       |#
                     #;(vect2:*scalar
                       (field-least-potential-vector pipes-field pos) 0.4)
                     #;(vect2:*scalar
                       (field-least-potential-vector entries-field pos) 0.6)
                     (vect2:*scalar
                       (agent-pipes-interaction pos pipes-center-list) 0.4)
                     (vect2:*scalar
                       (agent-entry-interaction pos entry-path) 0.6)
                       ))))
             (agent-proc a))))
       (make-agent
         'kitchen
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world)))
               (list
                 (translation:point
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos wall-path-list) -1.0)
                     #|
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) -0.3)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) -0.8)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) -0.8)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) -0.8)
                       |#
                     #|
                     (vect2:*scalar
                       (field-least-potential-vector structure-field pos) -0.05)
                       |#
                     #;(vect2:*scalar
                       (field-least-potential-vector pipes-field pos) 0.3)
                     (vect2:*scalar
                       (agent-pipes-interaction pos pipes-center-list) 0.3)
                       ))))
             (agent-proc a))))
       (make-agent
         'living
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world))
                   (south (north->south (graph-north graph))))
               (list
                 (translation:point
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos wall-path-list) -1.0)
                     #|
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) -0.3)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) -0.6)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) -0.6)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) -0.6)
                       |#
                     #|
                     #;(let ((lv (vect2:*scalar (field-least-potential-vector light-field pos) 0.1)))
                       (if (vect2:=? lv (make-vect2 0.0 0.0))
                           (vect2:*scalar (vect2:random) 0.2)
                         lv))
                     (vect2:*scalar
                       (field-least-potential-vector structure-field pos) -0.05)
                     |#
                     (vect2:*scalar south 1.4)
                       ))))
             (agent-proc a))))
       (make-agent
         'room1
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world))
                   (north-east (north->north-east (graph-north graph))))
               (list
                 (translation:point
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos wall-path-list) -1.0)
                     #|
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) -0.4)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) -0.6)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) -0.8)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) -0.5)
                       |#
                     #|
                     (vect2:*scalar
                       (field-least-potential-vector structure-field pos) -0.05)
                       |#
                     (vect2:*scalar north-east 1.0)
                       ))))
             (agent-proc a))))
       (make-agent
         'room2
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world))
                   (north-east (north->north-east (graph-north graph))))
               (list
                 (translation:point
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos wall-path-list) -1.0)
                     #|
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) -0.4)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) -0.6)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) -0.8)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) -0.5)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room3)) -0.5)
                       |#
                     #|
                     (vect2:*scalar
                       (field-least-potential-vector structure-field pos) -0.05)
                       |#
                     (vect2:*scalar north-east 1.0)
                       ))))
             (agent-proc a))))
       (make-agent
         'room3
         (list (polygon:make-random-point-inside limit-polygon))
         (lambda (world a)
           (make-agent
             (agent-label a)
             (let ((pos (car (agent-node-positions a)))
                   (agents (world-agents world))
                   (north-east (north->north-east (graph-north graph))))
               (list
                 (translation:point
                   pos
                   (vect2+
                     (vect2:*scalar
                       (agent-walls-interaction pos wall-path-list) -1.0)
                     #|
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'distribution)) -0.4)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'living)) -0.6)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'kitchen)) -0.8)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room1)) -0.9)
                     (vect2:*scalar
                       (agent-agent-interaction a (find-agent agents 'room2)) -0.5)
                       |#
                     #|
                     (vect2:*scalar
                       (field-least-potential-vector structure-field pos) -0.05)
                       |#
                     (vect2:*scalar north-east 1.0)
                       ))))
             (agent-proc a)))))))

  #|
  (visualize-world world graph)
  (if (stop?)
      (values graph world)
    (evolve-mono-nodal-agents
      graph
      (world-merge-agents
        world
        (agents-receive-new-states))))

|#




       (values
         graph
         (make-world 
           agents
           '()))))

;;; Termination predicate 3

(define (predesigned-band-termination-predicate-3 graph world)
  (not (null? world))) ; TODO

;;; Algorithm steps

(define predesigned-band
  (list
    (list predesigned-band-iteration-step-1
          predesigned-band-iteration-step-2
          predesigned-band-iteration-step-3)
    (list predesigned-band-termination-predicate-1
          predesigned-band-termination-predicate-2
          predesigned-band-termination-predicate-3)))

;-------------------------------------------------------------------------------
; Elements' interaction
;-------------------------------------------------------------------------------

;;; Agent-agent interaction vector

(define (agent-agent-interaction agent1 agent2)
  (let* ((pos1 (car (agent-node-positions agent1)))
         (pos2 (car (agent-node-positions agent2)))
         (vec (vect2- pos2 pos1)))
    (vect2:/scalar vec (vect2:squaremagnitude vec))))

;;; Calculate least potential vector given a field and a point in it

(define (field-least-potential-vector field pos)
  (define (make-coords center)
    (map
      (lambda (c)
        (u8-2dfield-coords->reflective-coords
          field
          (make-vect2
            (fx+ (fx* 5 (car c)) (vect2-x center))
            (fx+ (fx* 5 (cadr c)) (vect2-y center)))))
      '((0 0)
        (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1) (0 -1) (1 -1)
        (2 0) (2 1) (2 2) (1 2) (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-2 -1) (-2 -2) (-1 -2) (0 -2) (1 -2) (2 -2) (2 -1))))
  (let ((pos-coords (u8-2dfield-position->coords field pos)))
    (vect2:/scalar
      (cadr
        (fold
          (lambda (c current-max)
            (let ((value-in-coords (u8-2dfield-coords->value field c)))
              (if (> value-in-coords (car current-max))
                  (list value-in-coords (vect2- c pos-coords))
                current-max)))
          (list 0 (make-vect2 0 0))
          (make-coords pos-coords)))
      5.0)))

;;; Agent-walls interaction vector

(define (agent-walls-interaction agent-pos path-list)
  (fold
    (lambda (p vec)
      (let ((distance-vec (vect2-
                            agent-pos
                            (segment:mid-point p))))
        (vect2+ (vect2:/scalar distance-vec (vect2:magnitude distance-vec))
                vec)))
    (make-vect2 0.0 0.0)
    path-list))

;;; Agent-pipes interaction vector

(define (agent-pipes-interaction agent-pos center-list)
  (fold
    (lambda (p vec)
      (let ((distance-vec (vect2-
                            p
                            agent-pos)))
        (vect2+ (vect2:/scalar distance-vec (vect2:magnitude distance-vec))
                vec)))
    (make-vect2 0.0 0.0)
    center-list))

;;; Agent-pipes interaction vector

(define (agent-entry-interaction agent-pos entry-segment)
  (let ((distance-vec (vect2-
                        (segment:mid-point entry-segment)
                        agent-pos)))
    (vect2:/scalar
      distance-vec
      (vect2:magnitude distance-vec))))
