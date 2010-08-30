;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Operator testbed
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import (std string/xml-to-sxml
             misc/uuid
             srfi/1)
        ../context
        ../core/debugging
        ../core/functional
        ../input
        ../geometry/kernel
        ../geometry/generation
        ../graph-visualization
        ../graph-operations
        ../math/exact-algebra
        ../operators
        ../visualization)

(random-source-randomize! default-random-source)

(define (visualize-title text)
  (visualization:do-later
   'title
   (lambda (backend vis-env)
     (visualization:paint-set-color backend 0.1 0.1 0.1 1.0)
     (visualization:paint-text backend text "Arial" 0.5 0.1 -0.5))
   50))

;; (let ((graph (input-from-xml "xml-input/two_rooms.xml")))
;;   (pp graph)
;;   (visualize-graph graph)
;;   (visualize-title "op:move - wall (original graph)")
;;   (visualization:do-loop)

;;   (let ((transformed-graph
;;          (op:move (wall&constraints->context graph
;;                                              (car
;;                                               (graph:find.walls graph)))
;;                   `(@movement 1.0))))
;;     (pp transformed-graph)
;;     (visualization:forget-all)
;;     (visualize-graph transformed-graph)
;;     (visualize-title "op:move - wall (transformed graph)")
;;     (visualization:do-loop)
;;     (visualization:forget-all))
;;   (visualization:forget-all))

;; (let ((graph (input-from-xml "xml-input/one_room.xml")))
;;   (pp graph)
;;   (visualize-graph graph)
;;   (visualize-title "op:cut (original graph)")
;;   (visualization:do-loop)

;;   (let* ((graph-limits (graph:wall-list->pseq
;;                         (graph:find.exterior-walls graph)))
;;          (transformed-graph
;;           ((compose op:cut line->context+arguments) 
;;            graph
;;            (point&point->line
;;             (pseq:centroid graph-limits)
;;             (vect2:*scalar (generate.random-point)
;;                            (vect2:max-component
;;                             (bbox:size-segment (pseq:bbox graph-limits))))))))
;;     (pp transformed-graph)
;;     (visualization:forget-all)
;;     (visualize-graph transformed-graph)
;;     (visualize-title "op:cut (transformed graph)")
;;     (visualization:do-loop))
;;   (visualization:forget-all))

(let ((graph (input-from-xml "xml-input/two_rooms.xml")))
  (pp graph)
  (visualize-graph graph)
  (visualize-title "op:merge (original graph)")
  (visualization:do-loop)

  (let ((transformed-graph
         (op:merge (apply many->context
                          graph
                          (graph:find.rooms graph)))))
    (pp transformed-graph)
    (visualization:forget-all)
    (visualize-graph transformed-graph)
    (visualize-title "op:merge (transformed graph)")
    (visualization:do-loop)
    (visualization:forget-all))
  (visualization:forget-all))