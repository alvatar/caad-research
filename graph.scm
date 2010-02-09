(import web/parse/ssax-sxml/sxml-tools/sxpath)
(import (std string/xml-to-sxml))
(import xml-macros)

(define (generate-graph-from-xml xml-string)
  (let*
    ((sxml (xml-string->sxml xml-string))
    (architecture ((sxpath '(ensanche floorPlan architecture *)) sxml)))

    (pp-code-eval architecture)
    architecture))
