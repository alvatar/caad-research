;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strategies define combinations of pluggable components for a generation
;;; algorithm
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        components/agents-hinted-evolutionary-distribution
        components/check-and-fix-input
        components/walls-from-agents-bath-distribution-block)

;-------------------------------------------------------------------------------
; Strategies list
;-------------------------------------------------------------------------------

(define bath-block
  (list
   check-and-fix-input
   agents-hinted-evolutionary-distribution
   walls-from-agents/distribution&bath-block))

;;; A-list of symbols and strategies

(define procedures-alist
  `((bath-block ,bath-block)))
