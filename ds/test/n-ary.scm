;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/64))
(import ../n-ary)

;-------------------------------------------------------------------------------
(test-begin "n-ary")
;-------------------------------------------------------------------------------

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

(test-equal
 "skim-level, \"remove\" (default) treatment of shallow-leaves"
 (n-ary:skim-level test-tree 2)
 (make-node
  #f
  (make-node
   #f
   (make-leaf '1)
   (make-leaf '(5 5 5))
   (make-leaf '3))
  (make-node
   #f
   (make-leaf '4)
   (make-leaf '6))
  #f
  #f))

(test-equal
 "skim-level, \"accept\" treatment of shallow-leaves"
 (n-ary:skim-level test-tree 2 'accept)
 (make-node
  #f
  (make-node
   #f
   (make-leaf '1)
   (make-leaf '(5 5 5))
   (make-leaf '3))
  (make-node
   #f
   (make-leaf '4)
   (make-leaf '6))
  (make-leaf 'd)
  (make-leaf 'e)))

(test-equal
 "skim-level, \"strict\" treatment of shallow-leaves"
 (n-ary:skim-level test-tree 2 'strict)
 #f)

(test-equal
 "extract-level, default (\"remove\") treatment of shallow-leaves"
 (n-ary:extract-level test-tree 2)
 '(1 (5 5 5) 3 4 6))

(test-equal
 "extract-level, \"accept\" treatment of shallow-leaves"
 (n-ary:extract-level test-tree 2 'accept)
 '(1 (5 5 5) 3 4 6 d e))

(test-equal
 "extract-level, \"strict\" treatment of shallow-leaves"
 (n-ary:extract-level test-tree 2 'strict)
 #f)

(test-equal
 "depth"
 (n-ary:depth test-tree)
 2)

(test-equal
 "depth"
 (n-ary:depth test-tree2)
 3)

(test-equal
 "take-levels"
 (n-ary:take-levels test-tree2 2)
 (make-node 'a
            (make-node 'b
                       (make-leaf 'ba)
                       (make-leaf '(bb1 bb2 bb3))
                       (make-leaf 'bc))
            (make-node 'c
                       (make-leaf 'ca)
                       (make-leaf 'cb))
            (make-leaf 'd)
            (make-node 'e
                       (make-leaf 'ea))))

;-------------------------------------------------------------------------------
(test-end "n-ary")
;-------------------------------------------------------------------------------