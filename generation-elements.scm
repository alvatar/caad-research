;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Elements used by generations algorithms and strategies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

;-------------------------------------------------------------------------------
; Agents
;-------------------------------------------------------------------------------

;;; Agent type

(define-structure agent label node-positions proc)

;;; Agent new state evaluation

(define (agent-new-state agent world)
  (if (agent? agent)
      ((agent-proc agent) world agent)
    (error "agent-new-state: argument #1 is not an agent")))

;;; Find an agent given a list

(define (find-agent agents label)
  (find
    (lambda (a) (equal? label (agent-label a)))
    agents))

;-------------------------------------------------------------------------------
; World
;-------------------------------------------------------------------------------

;;; World type

(define-structure world agents fields)
