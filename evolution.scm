;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1
             srfi/26
             srfi/95))
(import core/command
        core/list
        core/syntax
        core/debugging
        generation
        selection)

;;; Main evolution cycle, pulling in all the algorithm parts

(define (evolution evolver-configuration
                   generator-type
                   selector-type
                   seed-data)
  (let ((evolver-type (car evolver-configuration))
        (evolver-args (cdr evolver-configuration)))
    ((case evolver-type
       ;; Stops when the 
       ((choose-bests)
        (let ((arg-max-iterations (get-arg evolver-args 'max-iterations))
              (arg-pool-size (get-arg evolver-args 'pool-size)))
         (let ((done? (cute >= <> arg-max-iterations)) ; TODO: consider case when the arg is not found
               (pool-updater (lambda (current-pool new-specimen)
                               (let ((sorted (sort
                                              (cons new-specimen current-pool)
                                              (lambda (s1 s2) (> (evaluated-graph-score s1)
                                                            (evaluated-graph-score s2))))))
                                 (if (> (length sorted) arg-pool-size)
                                     (take sorted arg-pool-size)
                                     sorted)))))
           (lambda (seed-data)               ; produce the proc
             (let ((generate (generator generator-type
                                        seed-data))
                   (select (selector selector-type)))
               (let loop ((pool '())
                          (n-iterations 0))
                 (if (done? n-iterations)
                     selected-pool
                     (loop (aif new-pool (select pool-updater
                                                 pool
                                                 (generate seed-data))
                                new-pool
                                pool)
                           (add1 n-iterations)))))))))
       ;; Stops when the results pool is full
       ((fill-pool)
        (error "todo"))
       (else
        (error "evolver type not implemented"))) seed-data)))