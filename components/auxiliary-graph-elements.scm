;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph and element auxiliary procedures common to various components
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/list)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../graph-operations)

(define (agents-in-room graph agents room)
  (filter
   (lambda (a)
     (every
      (lambda (p)
        (pseq:point-inside? (graph:room->pseq graph room) p))
      (agent-positions a))) ; TODO: wrong! if an agent is between two rooms, what to do?
   agents))

(define (num-agents-in-room graph agents r)
  (let ((pol (graph:room->pseq graph r)))
    (fold
     (lambda (a num)
       (if (pseq:point-inside? pol (agent-head-position a))
           (add1 num)
           num))
     0
     agents)))
