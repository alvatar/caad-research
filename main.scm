;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import evolution
        graph
        graph-visualization
        input
        output
        visualization)

(define (main)
  (random-source-randomize! default-random-source) ; Randomizes seed
  (let ((graph (sxml-graph->graph (xml->sxml-graph (input)))))
    (visualize-graph graph)
    (visualization:do-now)
    (visualization:forget-all)
    (visualize-graph graph)
    (visualization:do-now)              ; TODO: this is ugly
    (visualization:forget-all)
   (for-each
     (lambda (g)
       (output g))
     (evolution '(best @max-iterations 1000)
                'hinted-evolutionary
                'keep-best
                graph)))

  (exit 0))
(main)
