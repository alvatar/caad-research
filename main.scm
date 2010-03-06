;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import input)
(import graph)
(import utilities)
(import visualization)
(import mutation)
(import constraints)
(import filters)

(define (main)
  (random-source-randomize! default-random-source) ; Randomizes seed for UUID generation
  (visualize-graph '())
  (let loop
    ((archgraph-list (list (generate-graph-from-xml (input)))))
    (for-each
      (lambda
        (graph)
        (print-graph graph)
        (visualize-graph graph))
      archgraph-list)
    (loop
      (filter-graph-list
        (make-graph-mutations (first-or-element archgraph-list)))))

  (representation-cleanup)

  (exit 0))
(main)
