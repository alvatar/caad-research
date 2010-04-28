;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Evolution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Describes how the evolution is performed

(define (evolver generator selector seed-data)
  (define (halter gen-list)
    (>= (length gen-list) 1))
  (let loop ((selected-list '()))
    (if (halter selected-list)
        selected-list
      (loop (selector (generator seed-data) selected-list)))))

;;; Main evolution cycle, pulling in all the algorithm parts

(define (evolution-cycle evolver generator selector seed-data)
  (evolver generator selector seed-data))
