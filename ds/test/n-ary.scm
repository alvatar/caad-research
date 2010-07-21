;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

(define test-tree (make-node 'a
                             (make-node 'b
                                        (make-leaf '1)
                                        (make-leaf '(5 5 5))
                                        (make-leaf '3))
                             (make-node 'c
                                        (make-leaf '4)
                                        (make-leaf '6))
                             (make-leaf 'd)
                             (make-leaf 'e)))

(define test-tree2 (make-node 'a
                             (make-node 'b
                                        (make-leaf 'ba)
                                        (make-leaf '(bb1 bb2 bb3))
                                        (make-leaf 'bc))
                             (make-node 'c
                                        (make-leaf 'ca)
                                        (make-leaf 'cb))
                             (make-leaf 'd)
                             (make-node 'e
                                        (make-node 'ea
                                                   (make-leaf 'eaa)
                                                   (make-leaf 'eab)))))
