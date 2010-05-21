;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic B-tree (unevolved)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-structure btree node sibling child)

;;; Create a b-tree from a list of three elements

(define (list->btree tree)
  (if (and (not (null? tree)) (= (length tree) 3))
    (make-btree (car tree)
                (cadr tree)
                (caddr tree))
    'leaf))

;;; Get the b-tree root node

(define (btree:root tree)
  (btree:first-in-level tree 0))

;;; Extract a list of all nodes in the same level

(define (btree:level tree n)
  (define (extract-level tree-pos target current)
    (cond
     ((equal? tree-pos 'leaf)
      '())
     ((and (< current target) (not (null? (btree-child tree-pos))))
      (append
       (extract-level (list->btree (btree-child tree-pos)) target (add1 current))
       (extract-level (list->btree (btree-sibling tree-pos)) target current)))
     (else
      (cons (btree-node tree-pos)
            (extract-level (list->btree (btree-sibling tree-pos))
                           target
                           current)))))
  (extract-level tree n 0))

;;; Find the first node in a level

(define (btree:first-in-level tree n)
  (define (extract tree-pos target current)
    (cond
     ((and (< current target) (not (null? (btree-child tree-pos))))
      (extract (list->btree (btree-child tree-pos)) target (add1 current)))
     (else
      (btree-node tree-pos))))
  (extract tree n 0))
