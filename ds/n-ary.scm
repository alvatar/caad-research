;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; n-ary tree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import ../core/syntax)

;;; An internal node

(define (make-node data . children)
  (cons #f (cons data children)))

;;; A leaf

(define (make-leaf x) x)

;;; Get a node's data

(define (node-data node) ; TODO check node
  (cadr node))

;;; Get a node's children

(define (node-children tree) ;; TODO: Check node
  (cddr tree))

;;; An internal node must satisfy these, otherwise it is considered a leaf

(define (n-ary-node? obj)
  (and (list? obj)
       (not (car obj))
       (> (length obj) 2)))

(define (n-ary-leaf? obj)
  (not (n-ary-node? obj)))

;;; Build a tree taking only up the that depth

(define (n-ary:take-levels tree level)
  ((letrec
       ((recur-down (lambda (node level)
                      (cond
                       ((null? node) '())
                       ((n-ary-leaf? node) node)
                       ((zero? level) (make-leaf (node-data node)))
                       (else (make-node
                              (node-data node)
                              (bifurcation (node-children node) level))))))
        (recur-right (lambda (nodes level)
                       (if (null? nodes)
                           '()
                           (bifurcation nodes level))))
        (bifurcation (lambda (nodes level)
                       (cons (recur-down (car nodes) (- level 1))
                             (recur-right (cdr nodes) level)))))
     recur-down) tree level))

;;; Build a list with a given tree level. Takes an option to deal with shallow leaves

(define (n-ary:level tree level #!optional shallow-leaves)
  (let/cc
   abort
   (let ((leaf-process (case shallow-leaves
                         ((strict) ; Aborts if reaches a leaf shallower than target level
                          (lambda (leaf) (abort #f)))
                         ((accept) ; If they are leaves, add them even if not in target level
                          (lambda (leaf) leaf))
                         ((remove #f) ; Doesn't consider leaves that are not in target level
                          (lambda (leaf) #f)))))
     ((letrec
          ((recur-down (lambda (node level)
                         (cond
                          ((null? node) '())
                          ((zero? level) (if (n-ary-leaf? node)
                                             node
                                             (node-data node)))
                          ((n-ary-leaf? node) ; leaf but not in target level
                           (leaf-process node))
                          (else
                           (recur-right (node-children node) level)))))
           (recur-right (lambda (nodes level)
                          (if (null? nodes)
                              '()
                              (let ((valid-head (recur-down (car nodes) (- level 1))))
                                (if (not valid-head)
                                    (recur-right (cdr nodes) level)
                                    (cons valid-head
                                          (recur-right (cdr nodes) level))))))))
        recur-down) tree level))))

;;; Calculate the depth of the deepest leaf

(define (n-ary:depth tree)
  ((letrec
       ((recur-down (lambda (node level)
                      (cond
                       ((null? node) '())
                       ((n-ary-leaf? node) level)
                       (else
                        (recur-right (node-children node) level)))))
        (recur-right (lambda (nodes level)
                       (if (null? nodes)
                           level
                           (max (recur-down (car nodes) (add1 level))
                                (recur-right (cdr nodes) level))))))
     recur-down) tree 0))