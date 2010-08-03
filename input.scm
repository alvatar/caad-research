;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std string/xml-to-sxml))
(import sxml-graph
        web/parse/ssax-sxml/sxml-tools/sxpath)

;-------------------------------------------------------------------------------
; File handling
;-------------------------------------------------------------------------------

;;; Read XML from file

(define (input-from-xml)
  (let ((xml-file (open-input-file "xml-input/arch_1.xml")))
    (dynamic-wind
        (lambda () #f)
        (lambda ()
          (sxml-graph->graph
           (xml->sxml-graph
            (read-line xml-file #f))))
        (lambda ()
          (close-port xml-file)))))

;-------------------------------------------------------------------------------
; Graph importation
;-------------------------------------------------------------------------------

;;; Create graph from XML

(define (xml->sxml-graph xml-string)
  (let ((sxml (xml-string->sxml xml-string)))
    (car
     ((sxpath '(ensanche floorPlan architecture)) sxml))))
