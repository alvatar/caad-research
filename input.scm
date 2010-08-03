;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Input procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std string/xml-to-sxml))

(import web/parse/ssax-sxml/sxml-tools/sxpath)

;-------------------------------------------------------------------------------
; File handling
;-------------------------------------------------------------------------------

(define (input)
  (let*
    ((xml-file (open-input-file "xml-input/arch_1.xml"))
     (xml-string (read-line xml-file #f))
     (close-port xml-file))
     xml-string))

;-------------------------------------------------------------------------------
; Graph importation
;-------------------------------------------------------------------------------

;;; Create graph from XML

(define (xml->sxml-graph xml-string)
  (let ((sxml (xml-string->sxml xml-string)))
    (car
     ((sxpath '(ensanche floorPlan architecture)) sxml))))
