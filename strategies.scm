;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1)
        components/agents-place-randomly
        components/agents-evolutionary-distribution
        components/agents-hinted-evolutionary-distribution
        components/check-and-fix-input
        components/walls-from-agents-bath-distribution-block)

(define fully-evolutionary
  (list
   agents-evolutionary-distribution))

(define bath-block
  (list
   check-and-fix-input
   agents-hinted-evolutionary-distribution
   walls-from-agents/distribution&bath-block))

;;; A-list of symbols and strategies

(define procedures-alist
  `((bath-block ,bath-block)))
