(import web/parse/ssax-sxml/sxml-tools/sxpath)
(import (std string/xml-to-sxml))
(import xml-macros)

;; Generate graph from XML
;;
(define (generate-graph-from-xml xml-string)
  (let*
    ((sxml (xml-string->sxml xml-string))
    (architecture ((sxpath '(ensanche floorPlan architecture *)) sxml)))

    (pp-code-eval architecture)
    architecture))

;; Print graph
;;
(define (print-graph sxml)
  (pp-code-eval sxml))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get coordinate from point
;;
(define (point-coord coordinate point)
  (define (find-coordinate point)
    (if
      (equal? (caar point) coordinate)
      (cdar point)
      (find-coordinate (cdr point))))
  (find-coordinate point))

;; Get point n from point list
;;
(define (point-n n point-list)
  (cdr (list-ref point-list n)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get all wall points
;;
(define (wall-points wall)
  ((sxpath '(pt @)) wall))

;; Get wall point n
;;
(define (wall-point-n n wall)
  ((sxpath `((pt ,n) @ *)) wall))

;; Get windows in wall
