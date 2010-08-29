;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Strategies define combinations of pluggable components for a generation
;;; algorithm
;;; LPC: location/pattern/constraint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        components/agents-hinted-evolutionary-distribution
        components/walls-from-agents-bath-distribution-block)

(define-syntax define-strategy
  (syntax-rules ()
    ((_ (?strategy) ?components ...)
     (define ?strategy
       (list ?components ...)))))

;-------------------------------------------------------------------------------
; Strategies component lists
;-------------------------------------------------------------------------------

;;; A LPC algorithm with a set of predefined distribution&bath blocks

(define-strategy (bath-block)
  agents-hinted-evolutionary-distribution
  walls-from-agents/distribution&bath-block)

;;; A-list of symbols and strategies

(define procedures-alist
  `((bath-block ,bath-block)))
