;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import evolution
        graph-visualization
        input
        output
        visualization)

(define (main)
  (random-source-randomize! default-random-source) ; Randomizes seed
  (output-pool
   (evolution '(fill-pool @pool-size 1)
              'hinted-evolutionary
              'keep-best
              (input-from-xml)))
  (exit 0))
(main)
