;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/26))
(import core/command
        core/list
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
       ((best)
        (let ((done? (cute >= <> (get-arg evolver-args 'max-iterations))))
          (lambda (seed-data)                ; produce the proc
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
        (error "evolver type not implemented"))) seed-data)))