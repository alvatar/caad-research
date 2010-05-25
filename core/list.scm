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

(define-syntax map-if
  (syntax-rules ()
    ((_ p f l)
     (map (lambda (e) (if (p e) (f e) e)) l))
    ((_ p ft ff l)
     (map (lambda (e) (if (p e) (ft e) (ff e))) l))))
		 
;;; Map and cond combined: maps applying a function to the elements that
;;; satisfy each predicate. It can contain an else clause

(define-syntax map-cond
  (syntax-rules (else)    
    ((_ ((?p ?f) ...) ?l . ?lt) ; init make-vars
     (map-cond "make-vars" () ((?p ?f) ...) () (?l . ?lt)))
    ((_ "make-vars" (?vars ...) ((?p ?f) ...) (?l ...) (?lh . ?lls)) ; recur make-vars
     (map-cond "make-vars" (?vars ... x) ((?p ?f) ...) (?l ... ?lh) ?lls))
    ((_ "make-vars" (?vars ...) ((?p ?f) ...) (?l ...) ()) ; finalize make-vars
     (map-cond "make-cond" (?vars ...) ((?p ?f) ...) (?l ...)))

    ((_ "make-cond" ?vars ((?p ?f) . ?ct) (?l ...)) ; init make-cond
     (map-cond "make-cond" ?vars ?ct (((?p . ?vars) (?f . ?vars))) (?l ...)))    
    ((_ "make-cond" ?vars ((else ?f) . ?ct) (?conds ...) (?l ...)) ; catch given 'else'
     (map-cond "make-cond-else" ?vars ?f (?conds ...) (?l ...)))
    ((_ "make-cond" ?vars ((?p ?f) . ?ct) (?conds ...) (?l ...)) ; recur
     (map-cond "make-cond" ?vars ?ct (?conds ... ((?p . ?vars) (?f . ?vars))) (?l ...)))
    ((_ "make-cond" ?vars () (?conds ...) (?l ...)) ; finalize with default 'else'
     (map (lambda ?vars (cond ?conds ... (else (list . ?vars)))) ?l ...))
    
    ((_ "make-cond-else" ?vars ?ef (?conds ...) (?l ...)) ; finalize with given else
     (map (lambda ?vars (cond ?conds ... (else (?ef . ?vars)))) ?l ...))
  
    ((_ ((any ...) ...) thing) ; detect wrong syntax
     (error "Syntax error: wrong number of arguments in condition"))))

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
