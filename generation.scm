;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generation algorithms for graphs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))

(import core/syntax
        generation-elements
        graph
        strategies)

;;; Generate using a graph as input

(define (generate-from-graph steps)
  
  (letrec ((execute-step
            (lambda (steps graph world)
              (cond
               ((null? steps) graph)
               (else
                (receive (g w)
                         ((car steps) graph world)
                         (execute-step (cdr steps) g w)))))))
    (lambda (input)
      (execute-step steps input #f))))

;;; Generate simply following the steps, no input

(define (generate//input steps)
  (letrec ((execute-step
            (lambda (steps graph world)
              (cond
               ((null? steps) graph)
               (else
                (receive (g w)
                         ((car steps) graph world)
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
             (generate-from-graph (cadr aelm))
             (error "unkown strategy asked to generator")))
       (else
        (error "unknown seed data type fed into generator")))
      (generate//input type)))
