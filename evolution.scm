;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import core/list)
(import generation)
(import selection)

;;; Main evolution cycle, pulling in all the algorithm parts

(define (evolution evolver-type
                   generator-type
                   selector-type
                   seed-data)
  (let ((evolve
         (case evolver-type
           ((max-iterations)
            (let ((done? (lambda (n)
                           (>= n 1000)))) ; TODO!!!
              (lambda (seed-data) ; produce the proc
                (let ((generate (generator generator-type
                                           seed-data))
                      (select (selector selector-type)))
                  (let loop ((selected-list '())
                             (n-iterations 0))
                    (if (done? n-iterations)
                        selected-list
                        (loop (select
                               (generate seed-data) ; create the generator and execute it
                               selected-list)
                              (add1 n-iterations))))))))
           (else
            (error "evolver type not implemented")))))
    
    (evolve seed-data)))
