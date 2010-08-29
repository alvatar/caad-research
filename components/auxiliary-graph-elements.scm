;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph and element auxiliary procedures common to various components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1
             srfi/95)
        ../core/list
        ../geometry/kernel
        ../graph
        ../graph-operations
        agent
        world)

;;; Distance agent-point

(define (~distance.agent<->point agent point)
  (~distance.point-point
   (car (agent-positions agent))
   point)) ; TODO: multi-nodal agents

;;; Distance agent-wall

;; (define (~distance.agent<->wall agent wall)
;;   (~distance.point-pseq
;;    (car (agent-positions agent)) ; TODO: multi-nodal agents
;;    (wall-pseq wall)))

;;; Squared distance agent-wall

(define (squareddistance.agent<->wall agent wall)
  (squareddistance.point-pseq
   (car (agent-positions agent)) ; TODO: multi-nodal agents
   (wall-pseq wall)))

;;; Distance agent-window

(define (~distance.agent<->window agent window)
  (~distance.point-pseq
   (car (agent-positions agent)) ; TODO: multi-nodal agents
   (window-pseq window)))

;;; Distance agent-agent

;; (define (~distance.agent<->agent a1 a2)
;;   (~distance.point-point
;;    (car (agent-positions a1)) ; TODO: multi-nodal agents
;;    (car (agent-positions a2))))

;;; Squared distance agent-agent

(define (squareddistance.agent<->agent a1 a2)
  (squareddistance.point-point
   (car (agent-positions a1)) ; TODO: multi-nodal agents
   (car (agent-positions a2))))

;;; Distance agent-pipe

(define (~distance.agent<->pipe a p)
  (~distance.point-point
   (car (agent-positions a))
   (pipe-position p)))

;;; Distance agent-entry

(define (distance.agent<->entry a e)
  (~distance.point-pseq
   (car (agent-positions a))
   (entry-pseq e)))

;;; Distance agent-cardinal limits (pseq representing a side with an orientation)
;;; TODO: UNTESTED

(define (distance.agent<->cardinal-limits a orientation external-walls)
  (let* ((limits (graph:wall-list->pseq exterior-walls))
         (c (pseq:centroid limits)))
   (map (lambda (w)
          (~distance.point-pseq
           (car (agent-positions a))n
           (wall-pseq w)))
        (pseq:relative-position->point
         (pseq:clip/lines-clockwise external-walls
                    (point&direction->line c (rotate·direction orientation (- (/ pi 8))))
                    (point&direction->line c (rotate·direction orientation (/ pi 8))))
         #e1/2))))

;;; Illumination of the point

(define (light.agent a windows)         ; TODO: multi-nodal
  (sum
   (map (lambda (w)
          (~distance.point-segment
           (car (agent-positions a))
           (windows-pseq w))))))

;-------------------------------------------------------------------------------
; Finders
;-------------------------------------------------------------------------------

;;; Find the room that contains an agent

(define (find.room/agent graph agents agentl)
  (find
   (lambda (r)
     (find (lambda (a) (equal? (agent-label a) agentl)) (find.agents-in-room graph agents r)))
   (graph:find.rooms graph)))

;;; Find agents in a room

(define (find.agents-in-room graph agents room)
  (filter
   (lambda (a)
     (every
      (lambda (p)
        (pseq:point-inside? (graph:room->pseq graph room) p))
      (agent-positions a))) ; TODO: wrong! if an agent is between two rooms, what to do?
   agents))

;;; Find closest wall to an agent

(define (find.nearest-wall<->agent graph agent)
  (let ((walls (graph:find.walls graph))
        (p (agent-head-position agent)))
    (fold
     (lambda (w closest)
       (if (< (squareddistance.point-pseq p (wall-pseq w))
              (squareddistance.point-pseq p (wall-pseq closest)))
           w
           closest))
     (car walls)
     (cdr walls))))

;;; Find nearest room to an agent + its distance

(define (find.nearest-wall<->agent+squareddistance graph agent)
  (pair->2-values
   (let* ((walls (graph:find.walls graph))
          (first-wall (car walls))
          (p (agent-head-position agent)))
     (fold
      (lambda (w nearest-w.d)                ; w.d = (wall . distance)
        (let ((dist-current-wall (squareddistance.point-pseq p (wall-pseq w))))
          (if (< dist-current-wall
                 (cdr nearest-w.d))
              (cons w dist-current-wall)
              nearest-w.d)))
      (cons first-wall (squareddistance.point-pseq p (wall-pseq first-wall)))
      (cdr walls)))))

;-------------------------------------------------------------------------------
; Sorting
;-------------------------------------------------------------------------------

;;; Sort agents descendingly by distance to a given one

(define (sort.distance.agent<->agents a as)
  (sort as
        (lambda (a1 a2) (< (distance·agent<->agent a a1)
                      (distance·agent<->agent a a2)))))

;;; Sort the walls according to distance with agents

(define (sort.distance.agent<->walls graph a)
  (let ((p (agent-head-position a)))
    (sort (graph:find.walls graph)
          (lambda (w1 w2) (< (squareddistance.point-pseq p (wall-pseq w1))
                        (squareddistance.point-pseq p (wall-pseq w2)))))))

;-------------------------------------------------------------------------------
; Misc.
;-------------------------------------------------------------------------------

;;; Cound the agents in a room

(define (count.agents-in-room graph agents r)
  (let ((pol (graph:room->pseq graph r)))
    (fold
     (lambda (a num)
       (if (pseq:point-inside? pol (agent-head-position a))
           (add1 num)
           num))
     0
     agents)))
