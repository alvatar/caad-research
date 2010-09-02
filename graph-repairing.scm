;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph error checking and correction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

(import core/list
        graph-operations
        graph)

;;; Fix wall order in all rooms

(define (graph:fix-wall-order graph)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (map-if room? (lambda (e) (graph:sort.room-walls graph e)) (graph-architecture graph))))

;;; Fix walls connectivity

(define (graph:fix-wall-snapping graph)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (map-if room? (lambda (r) (graph:snap.room-walls graph r 0)) (graph-architecture graph))))

;;; Fix everything

(define (graph:fix.everything graph)
  (graph:fix-wall-snapping graph))