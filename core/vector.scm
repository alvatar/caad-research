;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

;;; for-each for vectors
;;; TODO: try to optimize

(define (vector-for-each proc vec . vecs)
  (let ((lists (append (list (vector->list vec))
                       (map vector->list vecs))))
    (apply for-each (cons proc lists))))

;;; map for vectors
;;; TODO: try to optimize

(define (vector-map proc vec)
  (list->vector (map proc (vector->list vec))))
