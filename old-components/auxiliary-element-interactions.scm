;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        ../core/syntax
        ../core/debugging
        ../geometry/kernel
        ../graph
        ../graph-visualization
        ../math/exact-algebra
        ../math/inexact-algebra
        generation-elements)

;;; Agent-agent interaction vector

(define (interaction.agent<->agent agent1 agent2)
  (let* ((pos1 (car (agent-positions agent1)))
         (pos2 (car (agent-positions agent2)))
         (vec (vect2- pos2 pos1)))
    (if (vect2:~=e pos1 pos2 0.1)
        (vect2:*scalar (vect2:random) 0.2)
        (vect2:/scalar vec (vect2:squaremagnitude vec)))))

;;; Agent-walls interaction vector

(define (interaction.agent<->walls agent-pos pseq-list)
  (fold
   (lambda (p vec)
     (let ((~distance-vec (vect2-
                          agent-pos
                          (segment:mid-point (pseq->segment p)))))
       (vect2+ (vect2:/scalar distance-vec (vect2:~magnitude distance-vec))
               vec)))
   (make-vect2 0.0 0.0)
   pseq-list))

;;; Agent-walls interaction vector (not squared distance)

(define (interaction.agent<->walls-simple agent-pos pseq-list)
  (fold
   (lambda (p vec)
     (let ((~distance-vec (vect2-
                          agent-pos
                          (segment:mid-point (pseq->segment p)))))
       (vect2+ distance-vec
               vec)))
   (make-vect2 0.0 0.0)
   pseq-list))

;;; Agent-pipes interaction vector

(define (interaction.agent<->pipes agent-pos center-list)
  (fold
   (lambda (p vec)
     (let ((~distance-vec (vect2-
                          p
                          agent-pos)))
       (vect2+ (vect2:/scalar distance-vec (vect2:~magnitude distance-vec))
               vec)))
   (make-vect2 0.0 0.0)
   center-list))

;;; Agent-pipes interaction vector

(define (interaction.agent<->entry agent-pos entrypseq)
  (let ((~distance-vec (vect2-
                       (segment:mid-point (pseq->segment entrypseq))
                       agent-pos)))
    (vect2:/scalar
     distance-vec
     (vect2:~magnitude distance-vec))))
