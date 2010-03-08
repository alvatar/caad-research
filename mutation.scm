;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph mutation algorithms, using high-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import graph)
(import operations)
(import context)
(import validators)
(import constraints)

(define (make-graph-mutations graph)
  (list ((make-operation/context-set) graph)))

;; All the logic of which operations should be performed stems from here
;;
(define (make-operation/context-set)
  (lambda (graph)
    (apply-operation
      op-merge
      graph
      two-rooms
      unconstrained
      all-valid)))
