;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std string/xml-to-sxml))
(import core/debugging
        sxml-graph
        web/parse/ssax-sxml/sxml-tools/sxpath)

;-------------------------------------------------------------------------------
; File handling
;-------------------------------------------------------------------------------

;;; Read XML from file

(define (input-from-xml)
  (call-with-input-file "xml-input/porto_cristo.xml"
    (lambda (file)
      (sxml-graph->graph
       (xml->sxml-graph
        (read-line file #f))))))

;;; Create graph from XML

(define (xml->sxml-graph xml-string)
  (let ((sxml (xml-string->sxml xml-string)))
    (car
     ((sxpath '(ensanche floorplan architecture)) sxml))))
