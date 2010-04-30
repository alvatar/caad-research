;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
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

(export hinted-brownian-agents)

;-------------------------------------------------------------------------------
; Algorithm steps
;-------------------------------------------------------------------------------

;;; Step 1

(define (iteration-step-1 graph world)
  (let*
    ((limit-polygon (wall-list->point-list (graph-find-exterior-walls graph)))
     (agents (list
       (make-agent
         'distribution
         (list (polygon:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'kitchen
         (list (polygon:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'living
         (list (polygon:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'room1
         (list (polygon:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'room2
         (list (polygon:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'room3
         (list (polygon:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a)))))

       (values
         graph
         (make-world 
           agents
           '()))))

;;; Step 2

(define (iteration-step-2 graph world)
  (define (stop? agents)
    (every
      (lambda (a)
        (vect2:=?e
          (car (agent-positions a))
          (car (agent-memory a))
          0.01))
      agents))
  (let* ((wall-path-list (wall-list->path-list (graph-walls graph)))
         (pipes-center-list (pipes-list->center-positions (graph-pipes graph)))
         (entry-path (entry->point-list graph (car (graph-entries graph))))
         (north (graph-north graph)))
    (let loop ((agents (world-agents world)))
      (visualize-world (make-world agents '()) graph)
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
                               (agent-walls-interaction pos wall-path-list) -1.0)
                             (vect2:*scalar
                               (agent-pipes-interaction pos pipes-center-list) 0.4)
                             (vect2:*scalar
                               (agent-entry-interaction pos entry-path) 0.6)))))
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
                               (agent-walls-interaction pos wall-path-list) -1.0)
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
                               (agent-walls-interaction pos wall-path-list) -1.0)
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
                               (agent-walls-interaction pos wall-path-list) -1.0)
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
                               (agent-walls-interaction pos wall-path-list) -1.0)
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
                               (agent-walls-interaction pos wall-path-list) -1.0)
                             (vect2:*scalar (north->north-east north) 1.0)))))
                     (agent-positions a)
                     (agent-proc a)))
                  (else
                    (error "iteration-step-2: Unhandled agent type:" a-label)))))
            agents))))))

;;; Step 3

(define (iteration-step-3 graph world)
  (let* ((wall-path-list (wall-list->path-list (graph-walls graph))))
    (let loop ((counter 0)
               (agents (world-agents world)))
      (visualize-world (make-world agents '()) graph)
      (if (>= counter 10) ; stop condition
          (values
            graph
            (make-world 
              agents
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
                               (agent-walls-interaction pos wall-path-list) -0.5)
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
                               (agent-walls-interaction pos wall-path-list) -0.5)
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
                               (agent-walls-interaction pos wall-path-list) -0.5)
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
                               (agent-walls-interaction pos wall-path-list) -0.5)
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
                               (agent-walls-interaction pos wall-path-list) -0.5)
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
                               (agent-walls-interaction pos wall-path-list) -0.5)
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

;;; Step 4

(define (iteration-step-4 graph world)
  (let
    ((new-graph
       graph))

     ;(pp (graph-regeneration '((a b c d)) ))
     (step)

     (values
       new-graph
       (make-world 
         (world-agents world)
         (world-fields world)))))

;;; Algorithm steps

(define hinted-brownian-agents
  (list iteration-step-1
        iteration-step-2
        iteration-step-3
        iteration-step-4))

;-------------------------------------------------------------------------------
; Walls generation
;-------------------------------------------------------------------------------

(define (graph-regeneration graph agents)
  #;(define (choose-agent-a lis)
    (list-ref lis (random-integer (length lis))))
  #;(define (choose-agent-b agent-a lis)
    (fold
      (lambda (a current)
        (let ((distance-current (<distance> agent-a a)))
          (if (< distance-current (car current))
              (list distance-current a)
            current)))
      (car lis)
      (cdr lis)))
  #;(define (make-partition-in-graph-with-references in-room agent-a agent-b)
    (op-split
      graph
      <context>
      <constraints>
        (<calculate-point-between-agents> agent-a agent-b)))
  (define (make-partition-in-graph in-room)
    graph)
  (define (find-next-room-to-partition)
    room
    #f)
  (define (agents-in-room room)
    (filter
      (lambda (a)
        (every
          (lambda (p)
            (polygon:point-inside? (room->point-list graph room) p))
          (agent-positions a))) ; TODO: wrong! if an agent is between to rooms, what to do?
       agents))
  ;; Iterate with new graph looking for rooms with more than one agent
  (let ((next-room (find-next-room-to-partition)))
    (if next-room
        (let* (;(agents-inside (agents-in-room next-room))
               ;(agent-a (choose-agent-a agents-inside))
               ;(agent-b (choose-agent-b agent-a lis))
               (new-graph (make-partition-in-graph
                            next-room
                            ;agent-a
                            ;agent-b
                            )))
            (if (<checker> new-graph) ; Continue with this graph only if it's good enough
                (graph-regeneration new-graph agents)
              (graph-regeneration graph agents)))
      graph)))

;-------------------------------------------------------------------------------
; Elements' interaction
;-------------------------------------------------------------------------------

;;; Agent-agent interaction vector

(define (agent-agent-interaction agent1 agent2)
  (let* ((pos1 (car (agent-positions agent1)))
         (pos2 (car (agent-positions agent2)))
         (vec (vect2- pos2 pos1)))
    (if (vect2:=?e pos1 pos2 0.1)
        (vect2:*scalar (vect2:random) 0.2)
      (vect2:/scalar vec (vect2:squaremagnitude vec)))))

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



(define (agent-walls-interaction-simple agent-pos path-list)
  (fold
    (lambda (p vec)
      (let ((distance-vec (vect2-
                            agent-pos
                            (segment:mid-point p))))
        (vect2+ distance-vec
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
