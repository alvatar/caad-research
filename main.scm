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
(import visualization)

(define (main)
  (random-source-randomize! default-random-source) ; Randomizes seed
  (let ((graph (generate-graph-from-xml (input))))
    (visualize-graph graph)
    (visualization:do-now)
    (evolution-cycle evolver generator selector (list graph)))

  (exit 0))
(main)
