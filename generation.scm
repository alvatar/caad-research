
;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms for graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import graph)
(import generation-elements)
(import strategies)

;-------------------------------------------------------------------------------
; General procedures
;-------------------------------------------------------------------------------

(define (generate-from-graph steps)
  (define (execute-step rest-steps graph world)
    (cond
     ((null? rest-steps)
      graph)
     (else
      (receive (g w)
        ((car rest-steps) graph world)
        (execute-step (cdr rest-steps) g w)))))
  (lambda (graph)
    (execute-step steps graph '())))

;;; Generator: creates a procedure for generating graphs
;;; generation-hints: gives information to help choosing generation algorithms
;;; seed-data: the data used for generating (genetic codes or model), used for checking
;;;   if the generator can be really used with the seed-data

(define (generator type #!optional seed-data generation-hints)
  (case type
    ((hinted-evolutionary)
     (generate-from-graph hinted-evolutionary))
    (else
     (error "generator strategy not implemented"))))
