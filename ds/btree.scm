;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Binary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;; TODO: Argument checks

(import ../core/list)

(define (btree:make-node content left right)
  (list content left right))

(define (btree:make-leaf content)
  (list content))

(define (btree:node-content node)
  (car node))

(define (btree:node-left node)
  (cadr node))

(define (btree:node-right node)
  (caddr node))

(define (btree:node? e)
  (and (list? e)
       (= (length e) 3)))

(define (btree:leaf? e)
  (and (list? e)
       (= (length e) 1)))

;;; Create a btree node from a list of three elements

(define (list->btree-node l)
  (if (and (not (null? l)) (= (length l) 3))
    (btree:make-node (car l)
                     (cadr l)
                     (caddr l))
    (error "This list can't be converted to a btree-node")))

;;; Create a b-tree from a list
;; TODO: Consider these cases:
;; N-ary-tree
;; binary tree
;; Ordering?

(define (list->btree l)
  (error "unimplemented"))

;;; Is a member of the tree?

(define (btree:member? e node)
  (if (btree:leaf? node)
      (eq? (btree:content node) e)
      (or (eq? (btree:content node) e)
          (btree:member? e (btree:left node))
          (btree:member? e (btree:right node)))))

;;; Left-most element of the tree

(define (btree:leftmost node)
  (let recur ((current-node node))
    (if (btree:node? current-node)
        (recur (btree:left current-node))
        (btree:node-content current-node))))

;;; Find the left-most node in a level

(define (btree:leftmost-in-level node n)
  (let recur ((current-level 0)
              (current-node node))
    (if (and (< current-level n)
             (btree:node? current-node))
        (recur (add1 current-level)
               (btree:node-left current-node))
        (btree:node-content current-node))))

;;; Find the right-most node in a level

(define (btree:rightmost-in-level node n)
  (let recur ((current-level 0)
              (current-node node))
    (if (and (< current-level n)
             (btree:node? current-node))
        (recur (add1 current-level)
               (btree:node-right current-node))
        (btree:node-content current-node))))

;;; Right-most element of the tree

(define (btree:rightmost node)
  (let recur ((current-node node))
    (if (btree:node? current-node)
        (recur (btree:right current-node))
        (btree:node-content current-node))))

;;; Reverse tree

(define (btree:reverse node)
  (if (btree:leaf? node)
      node
      (btree:make-node (btree:node-content node)
                       (btree:reverse
                        (btree:node-right node))
                       (btree:reverse
                        (btree:node-left node)))))

(define (btree:preorder node)
  (let recur ((current node)
              (lis '()))
    (if (btree:leaf? current)
        (cons (btree:node-content current) lis)
        (cons (btree:node-content current)
              (recur (btree:node-left current)
                     (recur (btree:node-right current)
                            lis))))))

;;; Get the b-tree root node

(define (btree:root tree)
  (car tree))

;;; Extract a list of all nodes in the same level

(define (btree:level tree n)
  (define (extract-level tree-pos target current)
    (cond
     ((equal? tree-pos 'leaf)
      '())
     ((and (< current target) (not (null? (btree:node-left tree-pos))))
      (append
       (extract-level (list->btree (btree:node-left tree-pos)) target (add1 current))
       (extract-level (list->btree (btree:node-right tree-pos)) target current)))
     (else
      (cons (btree:node-content tree-pos)
            (extract-level (list->btree (btree-sibling tree-pos))
                           target
                           current)))))
  (extract-level tree n 0))

;;; Btree pretty printing

(define (btree-pp tree)
  (error "unimplemented"))