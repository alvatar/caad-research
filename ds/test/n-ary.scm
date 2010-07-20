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
