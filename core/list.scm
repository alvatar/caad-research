;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General list procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; atom?

(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

;;; notnull?

(define notnull?
  (lambda (x)
    (not (null? x))))

;;; snoc

(define snoc
  (lambda (ls x)
    (append ls (list x))))

;;; Recursive map

(define (map* f l)
  (cond
   ((null? l)
    '())
   ((atom? l)
    (f l))
   (else
    (cons (map* f (car l)) (map* f (cdr l))))))

;;; Map applying the function only to the elements satisfying predicate

(define (map-if pred func l) (map (lambda (e) (if (pred e) (func e) e)) l))

;;; Recursive substitution in a list

(define (subst* new old l)
  (xsubst* cons new old l))

;;; Recursive substitution with multiple 'news' in a list

(define (msubst* lnew old l)
  (xsubst* append lnew old l))

;;; Recursive substitution in a list

(define (xsubst* f new old l)
  ((letrec ((X (lambda (l)
    (cond
      ((null? l)
       '())
      ((atom? (car l)) ; Atoms level
       (cond
         ((eq? (car l) old)
          (f
            new
            (X (cdr l))))
         (else
           (cons
             (car l)
             (X (cdr l))))))
      ((equal? (car l) old) ; Sublist level
       (f new (X (cdr l))))
      (else
        (cons
          (X (car l))
          (X (cdr l)))))))) X) l))

;;; Rotates the list until the first one satisfies the predicate

(define (rotate-until-first pred lis)
  (define (iter lis-iter n)
    (let ((x (car lis-iter))
          (l (length lis)))
      (cond
       ((= n l)
        (error "Full list rotation done without satisfying predicate"))
       ((pred x)
          lis-iter)
       (else
        (iter (append (cdr lis-iter) (list x)) (+ n 1))))))
  (iter lis 0))
