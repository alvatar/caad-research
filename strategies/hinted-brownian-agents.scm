;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; A strategy based on agents with brownian motion but hinted initial
;;; positions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import ../core/debug)
(import ../core/syntax)
(import ../geometry/kernel)
(import ../geometry/generation)
(import ../math/exact-algebra)
(import ../math/inexact-algebra) ; TODO: Could be removed!

(import ../analysis)
(import ../context)
(import ../generation-elements)
(import ../graph)
(import ../graph-visualization)
(import ../operations)
(import ../output)
(import ../visualization)

(import components/agents-place-randomly)
(import components/agents-forces-system)
(import components/agents-forces-brownian-motion)
(import components/agents-to-rooms-positive-distribution)

(export hinted-brownian-agents)

;;; Algorithm steps

(define hinted-brownian-agents
  (list agents-place-randomly
        agents-forces-system
        agents-forces-brownian-motion
        agents-to-rooms-positive-distribution))