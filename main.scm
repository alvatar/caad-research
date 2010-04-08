;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import analysis)
(import constraints)
(import filters)
(import graph)
(import graph-visualization)
(import input)
(import mutation)
(import output)
(import generation)
(import utils/misc)
(import visualization)

(define (main)
  (define (refine-result graph)
    (print-graph graph)
    (visualization:forget-all)
    (visualize-graph graph)
    (visualization:do-now)
    (visualization:forget-all)
    (if (accept? graph)
        (output graph)
      (refine-result (select-graph (filter-graphs (make-graph-mutations graph))))))

  (random-source-randomize! default-random-source) ; Randomizes seed for UUID generation
  (let ((graph (generate-graph-from-xml (input))))
    (visualize-graph graph)
    (visualization:do-now)
    (refine-result (generate-from-model graph)))

  (exit 0))
(main)
