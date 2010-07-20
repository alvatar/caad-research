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

(define (node-data node) ; TODO check node
  (cadr tree))

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
  (let recur ((node tree)
              (level level))
   (cond
    ((null? node)
     '())
    ((tree-leaf? node)
     node)
    ((zero? level)
     (make-leaf (node-data node)))
    (else
     (make-node (node-data node)
                (let ((new-level (- level 1)))
                  (let recur-children ((children (node-children node)))
                    (if (null? children)
                        '()
                        (cons (recur (car children) new-level)
                              (recur-children (cdr children)))))))))))

;; Build a list with a given tree level. Takes an option to deal with shallow leaves

(define (tree:level tree level #!optional shallow-leaves)
  (let/cc
   abort
   (let ((leaf-process (case shallow-leaves
                         ((strict) ; Aborts if reaches a leaf shallower than target level
                          (lambda (leaf) (abort #f)))
                         ((accept) ; If they are leaves, add them even if not in target level
                          (lambda (leaf) leaf))
                         ((remove #f) ; Doesn't consider leaves that are not in target level
                          (lambda (leaf) #f)))))
     (let recur-down ((node tree)
                      (level level))
       (cond
        ((null? node)
         '())
        ((zero? level)
         (if (tree-leaf? node)
             node
             (node-data node)))
        ((tree-leaf? node)              ; leaf but not in target level
         (leaf-process node))
        (else
         (let ((new-level (- level 1)))
           (let recur-children ((children (node-children node)))
             (if (null? children)
                 '()
                 (let ((valid-head (recur-down (car children) new-level)))
                   (if (not valid-head)
                       (recur-children (cdr children))
                       (cons valid-head
                             (recur-children (cdr children)))))))))))))))