;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/debug)
(import ../core/syntax)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../math/exact-algebra)
(import ../math/inexact-algebra) ; TODO: Could be removed!

(import ../analysis)
;(import ../fields-2d)
(import ../generation-elements)
(import ../graph)
(import ../graph-visualization)
(import ../operations)
(import ../output)
(import ../visualization)

(export hinted-brownian-agents)

;-------------------------------------------------------------------------------
; Algorithm steps
;-------------------------------------------------------------------------------

;;; Step 1

(define (iteration-step-1 graph world)
  (let*
    ((limit-polygon (wall-list->pseq (find-exterior-walls graph)))
     (agents (list
       (make-agent
         'distribution
         (list (pseq:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'kitchen
         (list (pseq:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'living
         (list (pseq:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'room1
         (list (pseq:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'room2
         (list (pseq:make-random-point-inside limit-polygon))
         (list (vect2:zero))
         (lambda (world a) a))
       (make-agent
         'room3
         (list (pseq:make-random-point-inside limit-polygon))
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
        (vect2:=e
          (car (agent-positions a))
          (car (agent-memory a))
          0.01))
      agents))
  (let* ((wall-pseq-list (wall-list->pseq-list (graph-walls graph)))
         (pipes-center-list (pipes-list->center-positions (graph-pipes graph)))
         (entry-pseq (entry->pseq graph (car (graph-entries graph))))
         (north (graph-north graph)))
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
                               (agent-entry-interaction pos entry-pseq) 0.6)))))
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

;;; Step 3

(define (iteration-step-3 graph world)
  (let* ((wall-pseq-list (wall-list->pseq-list (graph-walls graph))))
    (let loop ((counter 0)
               (agents (world-agents world)))
      (visualize-world (make-world agents '()) graph)
      (visualization:do-now)
      (if (>= counter 4) ; stop condition
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

;;; Step 4

(define (iteration-step-4 graph world)
  (let
    ((new-graph
       graph))
       ;(graph-regeneration-from-agents graph (world-agents world))))
(graph-regeneration-from-agents graph (world-agents world))
     ;(visualization:forget-all)
     ;(pp new-graph)
     ;(time (visualize-graph new-graph))
     ;(visualization:do-now)
     ;(visualize-world world new-graph)
     (display "REGENERATION DONE\n")
     (step)

     (values
       new-graph
       (make-world 
         (world-agents world)
         (world-fields world)))))

;;; Algorithm steps

(define hinted-brownian-agents
  (list iteration-step-1 ; TODO: name them and generalize-externalize when possible
        iteration-step-2
        iteration-step-3
        iteration-step-4))

;-------------------------------------------------------------------------------
; Walls generation
;-------------------------------------------------------------------------------

(define (graph-regeneration-from-agents graph agents)
  ;; (define (choose-agent-a lis)
  ;;   (list-ref lis (random-integer (length lis))))
  ;; (define (choose-agent-b agent-a lis)
  ;;   (fold
  ;;     (lambda (a current)
  ;;       (let ((distance-current (<distance> agent-a a)))
  ;;         (if (< distance-current (car current))
  ;;             (list distance-current a)
  ;;           current)))
  ;;     (car lis)
  ;;     (cdr lis)))
  ;; (define (make-partition-in-graph-with-references in-room agent-a agent-b)
  ;;   (op-split
  ;;     graph
  ;;     <context>
  ;;     <constraints>
  ;;       (<calculate-point-between-agents> agent-a agent-b)))

  (define (agents-in-room r)
    (filter
      (lambda (a)
        (every
          (lambda (p)
            (pseq:point-inside? (room->pseq graph r) p))
          (agent-positions a))) ; TODO: wrong! if an agent is between two rooms, what to do?
       agents))

  (define (num-agents-in-room r)
    (let ((pol (room->pseq graph r)))
      (fold
        (lambda (a num)
          (if (pseq:point-inside? pol (agent-head-position a))
              (add1 num)
            num))
        0
        agents)))

  (define (find-next-room-to-partition)
    (find
      (lambda (r)
        (> (num-agents-in-room r) 1))
      (graph-rooms graph)))
  
  (define (line->segment line)
    (cond
     ((~zero? (line-a line))
      (make-segment
       (make-point -100.0 (/ (- (line-c line)) (line-b line)))
       (make-point 100.0 (/ (- (line-c line)) (line-b line)))))
     ((~zero? (line-b line))
      (make-segment
       (make-point (/ (- (line-c line)) (line-a line)) -100.0)
       (make-point (/ (- (line-c line)) (line-a line)) 100.0)))
     (else
      (let ((any-origin (make-point 0.0 (/ (- (line-c line)) (line-b line))))
            (dirmult (vect2:*scalar (line->direction line) 100.0)))
        (make-segment (vect2- any-origin dirmult) (vect2+ any-origin dirmult))))))

  (define (d line)
    (visualization:do-later
     'debug-aids
     (lambda (backend vis-env)
       (visualization:paint-set-color backend 1.0 0.0 0.0 1.0)
       (visualization:paint-set-line-cap backend 'square)
       (visualization:paint-set-line-width backend .1)
       (visualization:paint-path backend (segment->pseq (line->segment line)))))
    (visualization:layer-depth-set! 'debug-aids 81)
    ;(visualization:do-now)
    line)

  (define (make-partition-in-graph room)
    (op:split-room
     (receive (points walls)
              (room-line-intersection
               graph
               room
               (d (point+direction->line (vect2+
                                          (vect2:random)
                                          (pseq:centroid (room->pseq graph room))) ; TODO: limit random bias
                                         (direction:perpendicular
                                          (segment->direction
                                           (pseq->segment
                                            (wall->pseq
                                             (find-longest-wall-in-room graph room))))))))
              ;(pp graph)
              ;(pp (wall-list->pseq-list walls))
              ;(pp points)
              ;(if (or (not (= 2 (length walls)))
                      ;(not (= 2 (length points))))
                  ;(error "NO BIEN"))
              (make-context-tree `[,graph
                                   ()
                                   (,room
                                    ()
                                    (,(car walls)
                                     (,(cadr walls)
                                      ()
                                      (,(cadr points)
                                       ()
                                       ()))
                                     (,(car points)
                                      ()
                                      ())))]))))
        ;; (make-context-tree `[,graph
        ;;                       ()
        ;;                       (,room
        ;;                         ()
        ;;                         (,(room-wall graph room 1);(car walls)
        ;;                          (,(room-wall graph room 3);(cadr walls)
        ;;                           ()
        ;;                           (,(random-real);(cadr points)
        ;;                             ()
        ;;                             ()))
        ;;                          (,(random-real);(car points)
        ;;                            ()
        ;;                            ())))])))

  (define (check-graph graph)
    graph) ; TODO: NEXT!

  ;(pp graph)
  (visualize-graph graph)
  (visualize-world (make-world agents '()) graph)
  (visualization:do-now)
  (display "\n---------------------------\nSTEP\n")
  (visualization:forget-all)

  ;(step)
  ;; Iterate with new graph looking for rooms with more than one agent
  (aif next-room (find-next-room-to-partition)
    (aif new-graph (check-graph (make-partition-in-graph next-room))
      (graph-regeneration-from-agents new-graph agents)
      (graph-regeneration-from-agents graph agents))
    graph))

;-------------------------------------------------------------------------------
; Elements' interaction
;-------------------------------------------------------------------------------

;;; Agent-agent interaction vector

(define (agent-agent-interaction agent1 agent2)
  (let* ((pos1 (car (agent-positions agent1)))
         (pos2 (car (agent-positions agent2)))
         (vec (vect2- pos2 pos1)))
    (if (vect2:=e pos1 pos2 0.1)
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

(define (agent-walls-interaction agent-pos pseq-list)
  (fold
    (lambda (p vec)
      (let ((distance-vec (vect2-
                            agent-pos
                            (segment:mid-point (pseq->segment p)))))
        (vect2+ (vect2:/scalar distance-vec (vect2:magnitude distance-vec))
                vec)))
    (make-vect2 0.0 0.0)
    pseq-list))

;;; Agent-walls interaction vector (not squared distance)

(define (agent-walls-interaction-simple agent-pos pseq-list)
  (fold
    (lambda (p vec)
      (let ((distance-vec (vect2-
                            agent-pos
                            (segment:mid-point (pseq->segment p)))))
        (vect2+ distance-vec
                vec)))
    (make-vect2 0.0 0.0)
    pseq-list))

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

(define (agent-entry-interaction agent-pos entry)
  (let ((distance-vec (vect2-
                        (segment:mid-point (pseq->segment entry))
                        agent-pos)))
    (vect2:/scalar
      distance-vec
      (vect2:magnitude distance-vec))))
