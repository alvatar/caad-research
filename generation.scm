;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms defined as strategies. Strategies are combinations
;;; of pluggable components for a generation algorithm
;;; LPC: location/pattern/constraint-convergence
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        components/agents-hinted-evolutionary-distribution
        components/walls-from-agents-bath-distribution-block
        core/syntax
        graph
        graph-repairing)

;-------------------------------------------------------------------------------
; Generation general procedures
;-------------------------------------------------------------------------------

;;; Generate using a graph as input

(define (generate/graph steps)
  (letrec ((execute-step
            (lambda (steps-tail graph world)
              (cond
               ((null? steps-tail) graph)
               (else
                (receive (g w k)
                         (call/cc (lambda (k) ((car steps-tail) graph world k)))
                         (if g
                             (execute-step (cdr steps-tail) g w)
                             (execute-step steps graph world))))))))
    (lambda (input)
      (execute-step steps (graph:fix.everything input) #f))))

;;; Generate simply following the steps, no input

(define (generate steps)
  (error "re-implement like generate/graph")
  (letrec ((execute-step
            (lambda (steps graph world)
              (cond
               ((null? steps) graph)
               (else
                (receive (g w k)
                         (call/cc (lambda (k) ((car steps) graph world k)))
                         (execute-step (cdr steps) g w)))))))
    (lambda (input)
      (execute-step steps #f #f))))

;;; Generator: creates a procedure for generating graphs
;;; generation-hints: gives information to help choosing generation algorithms
;;; seed-data: the data used for generating (genetic codes or model), used for checking
;;;   if the generator can be really used with the seed-data

(define (generator type #!optional seed-data generation-hints)
  (if seed-data
      (cond
       ((graph? seed-data)
        (aif aelm (assq type procedures-alist)
             (generate/graph (cadr aelm))
             (error "unkown strategy asked to generator")))
       (else
        (error "unknown seed data type fed into generator")))
      (generate type)))

;-------------------------------------------------------------------------------
; Strategies
;-------------------------------------------------------------------------------

;;; Strategy components must have the following interface:
;;; (graph world exit)
;;; and return the same as (values new-graph new-world exit)
;;; or use the continuation (exit #f #f #f)

;;; Strategy definition

(define-syntax define-strategy
  (syntax-rules ()
    ((_ (?strategy) ?components ...)
     (define ?strategy
       (list ?components ...)))))

;;; A LPC algorithm with a set of predefined distribution&bath blocks

(define-strategy (bath-block)
  agents-hinted-evolutionary-distribution
  walls-from-agents/distribution&bath-block)

;;; A-list of symbols and strategies

(define procedures-alist
  `((bath-block ,bath-block)))
