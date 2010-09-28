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
(import core/debugging
        core/functional
        core/list
        core/syntax
        core/tagged-list
        generation
        graph-visualization
        selection
        visualization)

;;; Exceptions?? (TODO: make it an argument maybe??)

(define handle-exceptions #t)

;;; Clean a pool, keeping only the graph, no extra info

(define (clean-out-pool pool)
  (map (lambda (eg) (evaluated-graph-graph eg)) pool))

;;; Main evolution cycle, pulling in all the algorithm parts

(define (evolution evolver-configuration
                   generator-type
                   seed-data
                   process-selected)
  ((case (get@ evolver-type evolver-configuration)
     ;; Stops when the max iterations are reached, choosing the best ones
     ((choose-bests)
      (let@ ((max-iterations pool-size) evolver-configuration)
            (let ((pool-update
                   (lambda (current-pool new-specimen)
                     (let ((sorted (sort
                                    (cons new-specimen current-pool)
                                    (lambda (s1 s2) (> (evaluated-graph-score s1)
                                                  (evaluated-graph-score s2))))))
                       (if (> (length sorted) pool-size)
                           (take sorted pool-size)
                           sorted)))))
              (lambda (seed-data)
                (let ((generate (generator generator-type
                                           seed-data))
                      (select selector))
                  (let recur ((pool '())
                              (n-iterations 0))
                    ;; if we are over the number of max iterations, just return the pool
                    (if (or (>= n-iterations max-iterations)
                            (>= (length pool) pool-size))
                        pool
                        (recur (let ((thunk (lambda ()
                                              (aif selected (select pool (generate seed-data))
                                                   (let ((selected-graph (evaluated-graph-graph selected)))
                                                     (display "Solution selected!\n")
                                                     (visualization:forget-all)
                                                     (visualize-graph selected-graph)
                                                     (visualize-room-uids selected-graph)
                                                     (visualization:do-now)
                                                     (thread-sleep! 5)
                                                     (process-selected selected-graph)
                                                     (pool-update pool selected))
                                                   pool))))
                                 (if handle-exceptions
                                     (with-exception-handler (lambda (e) (recur pool n-iterations))
                                                             thunk)
                                     (thunk)))
                               (add1 n-iterations)))))))))
     ;; Stops when the results pool is full
     ((fill-pool)
      (let@ ((pool-size) evolver-configuration)
            (lambda (seed-data)
              (let ((generate (generator generator-type
                                         seed-data))
                    (select selector))
                (let loop ((pool '())
                           (pool-size pool-size))
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