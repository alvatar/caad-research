;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph context and subcontexts described as a tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import math)

(define-structure btree node sibling child)

;;; Create a context-tree

(define (make-context-tree tree)
  (if (and (not (null? tree)) (= (length tree) 3))
    (make-btree (car tree)
                (cadr tree)
                (caddr tree))
    'leaf))

;;; Get the context-tree root node

(define (context-tree:root context-tree)
  (context-tree:first-in-level context-tree 0))

;;; Extract a list of all nodes in the same level

(define (context-tree:level context-tree n)
  (define (extract-level tree-pos target current)
    (cond
     ((equal? tree-pos 'leaf)
      '())
     ((and (< current target) (not (null? (btree-child tree-pos))))
      (append
        (extract-level (make-context-tree (btree-child tree-pos)) target (add1 current))
        (extract-level (make-context-tree (btree-sibling tree-pos)) target current)))
     (else
      (cons (btree-node tree-pos)
            (extract-level (make-context-tree (btree-sibling tree-pos))
                           target
                           current)))))
  (extract-level context-tree n 0))

;;; Find the first node in a level

(define (context-tree:first-in-level context-tree n)
  (define (extract tree-pos target current)
    (cond
     ((and (< current target) (not (null? (btree-child tree-pos))))
      (extract (make-context-tree (btree-child tree-pos)) target (add1 current)))
     (else
      (btree-node tree-pos))))
  (extract context-tree n 0))
