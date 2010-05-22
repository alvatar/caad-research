;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import evolution)
(import generation)
(import selection)

(import graph-visualization)
(import input)
(import output)
(import visualization)

(define (main)
  (random-source-randomize! default-random-source) ; Randomizes seed
  (let ((graph (sxml-graph->graph (xml->sxml-graph (input)))))
    (visualize-graph graph)
    (visualization:do-now)
    (let ((output-graphs
            (evolution-cycle evolver generator selector graph)))
      (for-each
        (lambda (g)
          (step)
          (output g))
      output-graphs)))

  (exit 0))
(main)
