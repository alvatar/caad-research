;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std srfi/95))

(import ../core/syntax)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../graph)
(import ../graph-visualization)
(import ../math/exact-algebra)
(import ../math/inexact-algebra)


;-------------------------------------------------------------------------------
; Elements interactions
;-------------------------------------------------------------------------------

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
     (let ((distance-vec (vect2-
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
     (let ((distance-vec (vect2-
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
     (let ((distance-vec (vect2-
                          p
                          agent-pos)))
       (vect2+ (vect2:/scalar distance-vec (vect2:~magnitude distance-vec))
               vec)))
   (make-vect2 0.0 0.0)
   center-list))

;;; Agent-pipes interaction vector

(define (interaction.agent<->entry agent-pos entrypseq)
  (let ((distance-vec (vect2-
                       (segment:mid-point (pseq->segment entrypseq))
                       agent-pos)))
nn    (vect2:/scalar
     distance-vec
     (vect2:~magnitude distance-vec))))

;-------------------------------------------------------------------------------
; Elements geometrical interrelations
;-------------------------------------------------------------------------------

;;; Distance agent-wall

(define (distance.agent<->wall agent wall)
  (distance:point-pseq
   (car (agent-positions agent)) ; TODO: multi-nodal agents
   (wall-pseq wall)))

;;; Distance agent-agent

(define (distance.agent<->agent a1 a2)
  (distance:point-point
   (car (agent-positions a1)) ; TODO: multi-nodal agents
   (car (agent-positions a2))))

;;; Distance agent-pipe

(define (distance.agent<->pipe a p)
  (distance:point-point
   (car (agent-positions a))
   (pipe-position p)))

;;; Distance agent-entry

(define (distance·agent<->entry a e)
  (distance:point-pseq
   (car (agent-positions a))
   (entry-pseq e)))

;;; Distance agent-cardinal limits (pseq representing a side with an orientation)
;;; TODO: UNTESTED

(define (distance.point-pseqagent<->cardinal-limits a orientation external-walls)
  (let* ((limits (graph:wall-list->pseq exterior-walls))
         (c (pseq:centroid limits)))
   (map (lambda (w)
          (distance:point-pseq
           (car (agent-positions a))n
           (wall-pseq w)))
        (pseq:relative-position->point
         (pseq:clip/lines-clockwise external-walls
                    (point+direction->line c (rotate·direction orientation (- (/ pi 8))))
                    (point+direction->line c (rotate·direction orientation (/ pi 8))))
         #e1/2))))

;;; Sort agents descendingly by distance to a given one

(define (sort.distance.agent<->agents a as)
  (sort as
        (lambda (a1 a2)
          (<
           (distance·agent<->agent a a1)
           (distance·agent<->agent a a2)))))

;;; Illumination of the point

(define (light.agent a windows)         ; TODO: multi-nodal
  (sum
   (map (lambda (w)
          (distance:point-segment
           (car (agent-positions a))
           (windows-pseq w))))))