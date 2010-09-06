;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1
             srfi/26
             srfi/95))
(import core/tagged-list
        core/list
        core/syntax
        core/debugging
        generation
        graph-visualization
        selection
        visualization)

;;; Clean a pool, keeping only the graph, no extra info

(define (clean-pool pool)
  (map (lambda (eg) (evaluated-graph-graph eg)) pool))

;;; Main evolution cycle, pulling in all the algorithm parts

(define (evolution evolver-configuration
                   generator-type
                   selector-type
                   seed-data)
  ((case (@get evolver-type evolver-configuration)
     ;; Stops when the max iterations are reached, choosing the given amount
     ((choose-bests)
      (let ((arg-max-iterations (get-arg evolver-args 'max-iterations))
            (arg-pool-size (get-arg evolver-args 'pool-size)))
        (let ((done? (cute >= <> arg-max-iterations)) ; TODO: consider case when the arg is not found
              (pool-update (lambda (current-pool new-specimen)
                             (let ((sorted (sort
                                            (cons new-specimen current-pool)
                                            (lambda (s1 s2) (> (evaluated-graph-score s1)
                                                          (evaluated-graph-score s2))))))
                               (if (> (length sorted) arg-pool-size)
                                   (take sorted arg-pool-size)
                                   sorted)))))
          (lambda (seed-data)
            (let ((generate (generator generator-type
                                       seed-data))
                  (select (selector selector-type)))
              (let loop ((pool '())
                         (n-iterations 0))
                (if (done? n-iterations)
                    (clean-pool pool)
                    (loop (aif selected (select pool (generate seed-data))
                               (pool-update pool selected)
                               pool)
                          (add1 n-iterations)))))))))
     ;; Stops when the results pool is full
     ((fill-pool)
      (let ((arg-pool-size (@get pool-size evolver-configuration)))
        (lambda (seed-data)
          (let ((generate (generator generator-type
                                     seed-data))
                (select (selector selector-type)))
            (let loop ((pool '())
                       (pool-size arg-pool-size))
              (if (zero? pool-size)
                  (clean-pool pool)
                  (aif selected (select pool (generate seed-data))
                       (loop (cons selected pool)
                             (sub1 pool-size))
                       (loop pool
                             pool-size))))))))
     ;; Just show the graph
     ((only-show-graph)
      (lambda (graph)
        (visualize-graph graph)
        (visualization:do-loop)
        (list graph)))
     (else
      (error "evolver type not implemented"))) seed-data))