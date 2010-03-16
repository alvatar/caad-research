;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import analysis)
(import constraints)
(import filters)
(import graph)
(import input)
(import mutation)
(import strategies)
(import utilities)
(import visualization)

(define (main)
  (define (refine-result graph)
    ;(print-graph graph)
    (visualize-graph graph)
    (if (accept? graph)
        (output graph)
      (refine-result (select-graph (filter-graphs (make-graph-mutations graph))))))

  (random-source-randomize! default-random-source) ; Randomizes seed for UUID generation
  (visualize-graph '())
  (refine-result (process-through-strategies (generate-graph-from-xml (input))))
  (representation-cleanup)

  (exit 0))
(main)
