;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; An independent tool for visualizing xml statically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(import (std string/xml-to-sxml))
(import ../web/parse/ssax-sxml/sxml-tools/sxpath)

(import ../input)
(import ../graph-visualization)
(import ../graph)
(import ../visualization)

(define (xml->sxml-graph xml-string)
  (let ((sxml (xml-string->sxml xml-string)))
    (car
     ((sxpath '(ensanche floorPlan architecture)) sxml))))

(define (main)
  (let ((graph (sxml-graph->graph
                (xml->sxml-graph
                 (let*
                     ((xml-file (open-input-file "../data/arch_1.xml"))
                      (xml-string (read-line xml-file #f))
                      (close-port xml-file))
                   xml-string)))))
    (let loop ()
      (visualize-graph graph)
      (visualization:do-now)
      (visualization:forget-all)
      (loop)))

  (exit 0))
(main)
