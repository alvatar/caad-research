;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/syntax)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../graph)
(import ../graph-visualization)
(import ../math/exact-algebra)
(import ../math/inexact-algebra) ; TODO: Could be removed!


;;; Agent-agent interaction vector

(define (agent-agent-interaction agent1 agent2)
  (let* ((pos1 (car (agent-positions agent1)))
         (pos2 (car (agent-positions agent2)))
         (vec (vect2- pos2 pos1)))
    (if (vect2:~=e pos1 pos2 0.1)
        (vect2:*scalar (vect2:random) 0.2)
        (vect2:/scalar vec (vect2:squaremagnitude vec)))))

;;; Calculate least potential vector given a field and a point in it

;; (define (field-least-potential-vector field pos)
;;   (define (make-coords center)
;;     (map
;;      (lambda (c)
;;        (u8-2dfield-coords->reflective-coords
;;         field
;;         (make-vect2
;;          (fx+ (fx* 5 (car c)) (vect2-x center))
;;          (fx+ (fx* 5 (cadr c)) (vect2-y center)))))
;;      '((0 0)
;;        (1 0) (1 1) (0 1) (-1 1) (-1 0) (-1 -1) (0 -1) (1 -1)
;;        (2 0) (2 1) (2 2) (1 2) (0 2) (-1 2) (-2 2) (-2 1) (-2 0) (-2 -1) (-2 -2) (-1 -2) (0 -2) (1 -2) (2 -2) (2 -1))))
;;   (let ((pos-coords (u8-2dfield-position->coords field pos)))
;;     (vect2:/scalar
;;      (cadr
;;       (fold
;;        (lambda (c current-max)
;;          (let ((value-in-coords (u8-2dfield-coords->value field c)))
;;            (if (> value-in-coords (car current-max))
;;                (list value-in-coords (vect2- c pos-coords))
;;                current-max)))
;;        (list 0 (make-vect2 0 0))
;;        (make-coords pos-coords)))
;;      5.0)))

;;; Agent-walls interaction vector

(define (agent-walls-interaction agent-pos pseq-list)
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
       (vect2+ (vect2:/scalar distance-vec (vect2:~magnitude distance-vec))
               vec)))
   (make-vect2 0.0 0.0)
   center-list))

;;; Agent-pipes interaction vector

(define (agent-entry-interaction agent-pos entrypseq)
  (let ((distance-vec (vect2-
                       (segment:mid-point (pseq->segment entrypseq))
                       agent-pos)))
    (vect2:/scalar
     distance-vec
     (vect2:~magnitude distance-vec))))
