;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import components/agents-place-randomly)
(import components/agents-evolutionary-distribution)
;; (import components/agents-forces-system)
;; (import components/agents-forces-brownian-motion)
;; (import components/agents-to-rooms-positive-distribution)

;;; Algorithm steps

(define hinted-brownian-agents
  (list
   ;; agents-place-randomly
   agents-evolutionary-distribution
   ;; agents-forces-system
   ;; agents-forces-brownian-motion
   ;; agents-to-rooms-positive-distribution
   ))