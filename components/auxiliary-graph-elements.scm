;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph and element auxiliary procedures common to various components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../core/list
        ../geometry/kernel
        ../graph
        ../graph-operations
        generation-elements)

;;; Find agents in a room

(define (find.agents-in-room graph agents room)
  (filter
   (lambda (a)
     (every
      (lambda (p)
        (pseq:point-inside? (graph:room->pseq graph room) p))
      (agent-positions a))) ; TODO: wrong! if an agent is between two rooms, what to do?
   agents))

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

;;; Find the room that contains an agent

(define (find.room/agent graph agents agentl)
  (find
   (lambda (r)
     (find (lambda (a) (equal? (agent-label a) agentl)) (find.agents-in-room graph agents r)))
   (graph:find.rooms graph)))

;;; Find closest room to an agent

(define (find.closest-wall/agent graph agent)
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