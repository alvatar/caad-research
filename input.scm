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
    ((xml-file (open-input-file "data/arch_4.xml"))
     (xml-string (read-line xml-file #f))
     (close-port xml-file))
     xml-string))

;-------------------------------------------------------------------------------
; Graph generation
;-------------------------------------------------------------------------------

;;; Generate graph from XML

(define (generate-graph-from-xml xml-string)
  (let* ((sxml (xml-string->sxml xml-string))
         (architecture
          (car
           ((sxpath '(ensanche floorPlan architecture)) sxml))))
    architecture))

