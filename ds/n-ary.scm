;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n-ary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; An internal node

(define (make-node data . children)
  (cons #f (cons data children)))

;;; A leaf

(define (make-leaf x) x)

;;; Get a node's data

(define (tree-data tree)
  (if (tree-node? tree)
      (cadr tree)
      tree))

;;; Get a node's children

(define (node-children tree) ;; TODO: Check node
  (cddr tree))

;;; An internal node must satisfy these, otherwise it is considered a leaf

(define (tree-node? obj)
  (and (list? obj)
       (not (car obj))
       (> (length obj) 2)))

(define (tree-leaf? obj)
  (not (tree-node? obj)))

;;; Build a tree taking only up the that depth

(define (tree:take-levels tree level)
  (cond
   ((null? tree)
    '())
   ((tree-leaf? tree)
    tree)
   ((zero? level)
    (make-leaf (tree-data tree)))
   (else
    (make-node (tree-data tree)
               (let ((new-level (- level 1)))
                 (let recur ((children (node-children tree)))
                   (if (null? children)
                       '()
                       (cons (tree:take-levels (car children) new-level)
                             (recur (cdr children))))))))))

;; Build a list with a given tree level

(define (tree:level tree level)
  (cond
   ((null? tree)
    '())
   ((zero? level)
    (tree-data tree))
   ((tree-leaf? tree)
    '())
   (else
    (make-node (tree-data tree)
               (let ((new-level (- level 1)))
                 (let recur ((children (node-children tree)))
                   (if (null? children)
                       '()
                       (cons (tree:level (car children) new-level)
                             (recur (cdr children))))))))))
