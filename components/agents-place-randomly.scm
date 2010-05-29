;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Component: place agents randomly inside exterior walls
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../auxiliary-operations)
(import ../core/syntax)
(import ../dev/debugging)
(import ../generation-elements)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../graph)
(import ../math/exact-algebra)
(import ../operations)

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
