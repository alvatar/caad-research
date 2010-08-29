;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: place agents randomly inside exterior walls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../core/syntax
        ../core/debugging
        ../geometry/kernel
        ../geometry/generation
        ../graph
        ../math/exact-algebra
        generation-elements)

;-------------------------------------------------------------------------------
; Algorithm steps
;-------------------------------------------------------------------------------

(define (agents-place-randomly graph world)
  (let*
      ((limit-polygon (analysis:graph-limits graph))
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
