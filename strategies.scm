;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import components/agents-place-randomly)
(import components/agents-evolutionary-distribution)
(import components/agents-hinted-evolutionary-distribution)
(import components/walls-from-agents-bath-distribution-block)

(define fully-evolutionary
  (list
   agents-evolutionary-distribution))

(define hinted-evolutionary
  (list
   agents-hinted-evolutionary-distribution
   walls-from-agents/distribution&bath-block))