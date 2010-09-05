;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import core/command
        evolution
        input
        output
        visualization)

((lambda ()
   (random-source-randomize! default-random-source)
   (output-pool
    (evolution (@args (evolver-type 'fill-pool)
                      (pool-size 1))
               'bath-block
               'keep-best
               (input-from-xml "xml-input/plan_1.xml")))
   (visualization:exit)
   (exit 0)))
