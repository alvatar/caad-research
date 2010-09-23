;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; World is all the environmental and auxiliary data carried through
;;; algorthim's components along with the graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../geometry/kernel
        ../geometry/bounding-box
        ../graph-operations
        ../visualization
        agent)

;;; World type

(define-structure world agents fields)

;;; World visualization

(define (visualize-world world graph)
  (let* ((bb (graph:bounding-box graph))
         (size-vec (bbox:size-segment bb)))
    (visualization:forget-layers '(agents fields))
    ;; (for-each
    ;;   (lambda (f) (visualize-field f size-vec))
    ;;   (world-fields world))
    (for-each
      visualize-agent
      (world-agents world))))
