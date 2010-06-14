;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General list procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (mostly-generic))
(compile-options force-compile: #t)

(import (std srfi/1))
(import syntax)
(import)

;-------------------------------------------------------------------------------
; Basic
;-------------------------------------------------------------------------------

;;; add 1

(define (add1 x)
  (+ x 1))

;;; substract 1

(define (sub1 x)
  (- x 1))

;;; not null?

(define-syntax not-null?
  (syntax-rules ()
    ((_ l)
     (not (null? l)))))

;;; atom?

(define atom? (lambda (x) (and (not (pair? x)) (not-null? x))))

;;; XOR

(define (xor a b) (if a (not b) b))

;;; snoc (always prefer the use of cons before this)

(define snoc
  (lambda (ls x)
    (append ls (list x))))

;-------------------------------------------------------------------------------
; List/values
;-------------------------------------------------------------------------------

;;; All cars and all cdrs

(define (cars+cdrs ls)
  (call/cc
   (lambda (abort)
     (let recur ((ls ls))
       (if (pair? ls)
           (receive (hl tl) (car+cdr ls)
                    (if (null-list? hl) (abort '() '())
                        (receive (a d) (car+cdr hl)
                                 (receive (cars cdrs) (recur tl)
                                          (values (cons a cars) (cons d cdrs))))))
           (values '() '()))))))

;;; Values to list

(define-syntax values->list
  (syntax-rules ()
    ((_ producer)
     (call-with-values
         (lambda () producer)
       (lambda v (apply list v))))))

;;; List to values

(define-syntax list->values
  (syntax-rules ()
    ((_ l)
     (apply values l))))

;;; Number of values produced

(define-syntax values-length
  (syntax-rules ()
    ((_ producer)
     (call-with-values
         (lambda () producer)
       (lambda v (length v))))))

;;; Extract only the nth-value from a function returning multiple values

(define-syntax values-ref
  (syntax-rules ()
    ((_ n producer)
     (call-with-values
       (lambda () producer)
       (lambda v (list-ref v n))))))

;;; Demultiplex a list in 2

(define (demultiplex2 f l)
  (let recur ((l l))
    (if (null? l)
        (values l l)
        (let ((h (car l)))
          (call-with-values
              (lambda () (recur (cdr l)))
            (lambda (a b)
              (call-with-values
                  (lambda () (f h))
                (lambda (p1 p2) (values (cons p1 a)
                                   (cons p2 b))))))))))

;;; Demultiplex a list
;;; (demultiplex (lambda (x) (values (car x) (cadr x))) '((a 1) (b 2) (c 3)))
;;; => (a b c)
;;;    (1 2 3)

(define (demultiplex f lis)
  (if (null? lis)
      '()
      (let recur ((l lis))
        (if (null? l)
            (apply values
                   (make-list
                    (values-length (f (car lis)))
                    '()))
            (let ((h (car l)))
              (call-with-values
                  (lambda () (recur (cdr l)))
                (lambda tails
                  (call-with-values
                      (lambda () (f h))
                    (lambda produced-vals
                      (apply values
                             (map (lambda (p t) (cons p t))
                                  produced-vals
                                  tails)))))))))))

;-------------------------------------------------------------------------------
; Map variants
;-------------------------------------------------------------------------------

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
;;; TODO: Returning somthing NOT inside an s-expr doesn't work!!!
;;; TODO: default else should be #f!!!

(define-syntax map-cond
  (syntax-rules (else)
    ((_ (?vars ...) ((?p ?f ...) ...) ?l ...) ; entry for explicit vars case
     (map-cond "make-explicit-vars-init" (?vars ...) ((?p ?f ...) ...) ?l ... ))
    ((_ "make-explicit-vars-init" (?vars ...) ((else ?f ...) . ?ct) ?l ...) ; error: else is first
     (error "Syntax error: else clause can't be first"))
    ((_ "make-explicit-vars-init" (?vars ...) (((?p ...) ?f ...) . ?ct) ?l ...) ; init make-explicit-vars
     (map-cond "make-explicit-vars" (?vars ...) ?ct (((?p ...) ?f ...)) (?l ...)))

    ((_ "make-explicit-vars" (?vars ...) ((else ?f . ?ft)) (?conds ...) (?l ...)) ; catch given 'else'
     (map (lambda (?vars ...) (cond ?conds ... (else ?f . ?ft))) ?l ...))
    ((_ "make-explicit-vars" (?vars ...) ((else ?f . ?ft) . ?ct) (?conds ...) (?l ...)) ; error: else is not last
     (error "Syntax error: else clause must be last"))
    ((_ "make-explicit-vars" (?vars ...) (((?p ...) ?f ...) . ?ct) (?conds ...) (?l ...)) ; recur make-explicit-vars
     (map-cond "make-explicit-vars" (?vars ...) ?ct (?conds ... ((?p ...) ?f ...)) (?l ...)))
    ((_ "make-explicit-vars" (?vars ...) () (?conds ...) (?l ...)) ; finalize with default 'else'
     (map (lambda (?vars ...) (cond ?conds ... (else (list ?vars ...)))) ?l ...))

    ((_ ((?p ?f) ...) ?l . ?lt) ; entry for given vars case
     (map-cond "make-vars" () ((?p ?f) ...) () (?l . ?lt)))
    ((_ "make-vars" (?vars ...) ((?p ?f) ...) (?l ...) (?lh . ?lls)) ; recur make-vars
     (map-cond "make-vars" (?vars ... x) ((?p ?f) ...) (?l ... ?lh) ?lls))
    ((_ "make-vars" (?vars ...) ((?p ?f) ...) (?l ...) ()) ; finalize make-vars
     (map-cond "make-cond-init" (?vars ...) ((?p ?f) ...) (?l ...)))

    ((_ "make-cond-init" ?vars ((else ?f) . ?ct) (?l ...)) ; error: else is first
     (error "Syntax error: else clause can't be first"))
    ((_ "make-cond-init" ?vars ((?p ?f) . ?ct) (?l ...)) ; init make-cond
     (map-cond "make-cond" ?vars ?ct (((?p . ?vars) (?f . ?vars))) (?l ...)))    
    ((_ "make-cond" ?vars ((else ?f)) (?conds ...) (?l ...)) ; catch given 'else' in make-cond
     (map-cond "make-cond-else" ?vars ?f (?conds ...) (?l ...)))
    ((_ "make-cond" ?vars ((else ?f) . ?ct) (?conds ...) (?l ...)) ; error: else is not last
     (error "Syntax error: else clause must be last"))
    ((_ "make-cond" ?vars ((?p ?f) . ?ct) (?conds ...) (?l ...)) ; recur make-cond
     (map-cond "make-cond" ?vars ?ct (?conds ... ((?p . ?vars) (?f . ?vars))) (?l ...)))
    ((_ "make-cond" ?vars () (?conds ...) (?l ...)) ; finalize make-cond with default 'else'
     (map (lambda ?vars (cond ?conds ... (else (list . ?vars)))) ?l ...))
    ((_ "make-cond-else" ?vars ?ef (?conds ...) (?l ...)) ; finalize with given else
     (map (lambda ?vars (cond ?conds ... (else (?ef . ?vars)))) ?l ...))

    ((_ ((any ...) ...) thing) ; detect wrong syntax
     (error "Syntax error: wrong number of arguments in condition"))))

;;; Map that generates a value for each element
;;; (map/values (lambda (x y z) (values x y z)) '(a 1) '(b 2) '(c 3))
;;; => (a b c)
;;;    (1 2 3)

(define (map/values f . ls)
  (list->values
   (apply map (lambda args (values->list (apply f args))) ls)))

;;; map+fold combines them two, returning the map and the fold
;;; (map+fold (lambda (a b) (values (+ a b) (+ b 1))) 0 '(1 2 3 4))
;;; (1 3 5 7)
;;; 3

(define (map+fold kons knil lis1 . lists)
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists))
                  (fold-ans knil))
        (receive (cars cdrs) (cars+cdrs lists)
                 (if (null? cars)
                     (values '() fold-ans)
                     (receive (mapv foldv)
                              (apply kons (snoc cars fold-ans))
                              (receive (map-next fold-next)
                                       (recur cdrs foldv)
                                       (values (cons mapv map-next)
                                               fold-next))))))
      (let recur ((l lis1)
                  (fold-ans knil))
        (if (pair? l)
            (receive (lh lt) (car+cdr l)
                     (receive (mapv foldv)
                              (kons lh fold-ans)
                              (receive (map-next fold-next)
                                       (recur lt foldv)
                                       (values (cons mapv map-next)
                                               fold-next))))
            (values '() fold-ans)))))

;; TODO: WHY THIS DOESN'T WORK??
(define (leave-come-back)
  (let ((L (lambda (k) 
             (let recur ((n 0))
               (if (= n 5)
                   (begin (call/cc (lambda (back) (k n back)))
                          '())
                   (cons n (recur (+ n 1))))))))
    (receive (a b)
             (call/cc L)
             (values a (b)))))

;;; map-fold combines them two, maps values but also accumulates as fold, so that value can be
;;; used inside the map-fold computation
;;; (map-fold (lambda (a b) (values (+ a b) (+ b 1))) 0 '(1 2 3 4))
;;; (1 3 5 7)

(define (map-fold kons knil lis1 . lists) ; OPTIMIZE: meause if better than fold+map specialization
  (if (pair? lists)
      (let recur ((lists (cons lis1 lists))
                  (fold-ans knil))
        (receive (cars cdrs) (cars+cdrs lists)
                 (if (null? cars)
                     '()
                     (receive (mapv foldv)
                              (apply kons (snoc cars fold-ans))
                              (cons mapv (recur cdrs foldv))))))
      (let recur ((l lis1) ; Fast path for
                  (fold-ans knil))
        (if (null? l)
            '()
            (receive (lh lt) (car+cdr l)
                     (receive (mapv foldv)
                              (kons lh fold-ans)
                              (cons mapv (recur lt foldv))))))))

;-------------------------------------------------------------------------------
; Find, remove, substitute
;-------------------------------------------------------------------------------

;;; Remove first instance

(define (rember a l)
  ((letrec ((R (lambda (l)
                 (cond
                  ((null? l) '())
                  ((eq? (car l) a) (cdr l))
                  (else (cons (car l)
                              (R (cdr l)))))))) R) l))

;;; Try to find an element and remove it, yeilds #f if not found

(define (find-rember a l)
  (let/cc failed
   ((letrec ((R (lambda (l)
                  (cond
                   ((null? l) (failed #f))
                   ((eq? (car l) a) (cdr l))
                   (else (cons (car l)
                               (R (cdr l)))))))) R) l)))

;;; Rotates the list until the first one satisfies the predicate

(define (find-rotate pred lis)
  (define (iter lis-iter n)
    (let ((x (car lis-iter))
          (l (length lis)))
      (cond
       ((= n l) #f)
       ((pred x) lis-iter)
       (else
        (iter (append (cdr lis-iter) (list x)) (+ n 1))))))
  (iter lis 0))

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
          (f new
             (X (cdr l))))
         (else
           (cons (car l)
                 (X (cdr l))))))
      ((equal? (car l) old) ; Sublist level
       (f new (X (cdr l))))
      (else
        (cons
          (X (car l))
          (X (cdr l)))))))) X) l))

;-------------------------------------------------------------------------------
; Skeleton/shape
;-------------------------------------------------------------------------------

;;; Flatten a list (not-optimized)
;;; TODO: benchmark
;; (define (flatten x)
;;   (cond
;;    ((null? x) '())
;;    ((not (pair? x)) (list x))
;;    (else (append (flatten (car x))
;;                  (flatten (cdr x))))))

;;; Flatten a list (optimized)
;;; http://schemecookbook.org/Cookbook/ListFlatten

(define (flatten x:xs)
  (let* ((result (cons '() '())) (last-elt result))
    (define (f x:xs)
      (cond
       ((null? x:xs)
        result)
       ((pair? (car x:xs))
        (f (car x:xs)) (f (cdr x:xs)))
       (else
        (set-cdr! last-elt (cons (car x:xs) '()))
        (set! last-elt (cdr last-elt)) 
        (f (cdr x:xs)))))
    (f x:xs)
    (cdr result)))

;;; Fast flatten, that doesn't respect ordering

(define (flatten-unordered x:xs)
  (define (f x:xs result)
    (cond
     ((null? x:xs)
      result)
     ((pair? (car x:xs))
      (f (cdr x:xs) (f (car x:xs) result)))
     (else
      (f (cdr x:xs) (cons (car x:xs) result)))))
  (f x:xs '()))

;;; Make a structure analysis of a list

(define (list->skeleton l)
  ((letrec ((S (lambda (l n)
                 (cond
                  ((null? l)
                   (if (= n 0) '() (list n)))
                  ((list? (car l))
                   (if (= n 0)
                       (cons (S (car l) 0) (S (cdr l) 0))
                       (cons n (cons (S (car l) 0) (S (cdr l) 0)))))
                  (else
                   (S (cdr l) (+ 1 n)))))))
     S) l 0))

;;; Expand a skeleton into a list with stub positions
;; (define (expand-skeleton s) ; TODO Benchmark!
;;   ((letrec ((E (lambda (s)
;;                  (cond
;;                   ((null? s) '())
;;                   ((list? (car s))
;;                    (cons (E (car s)) (E (cdr s))))
;;                   (else
;;                    (append (make-list (car s) (car s))
;;                            (E (cdr s)))))))) E) s))

(define (expand-skeleton s)
  ((letrec ((E (lambda (s)
                 (cond
                  ((null? s) '())
                  ((list? (car s))
                   (cons (E (car s))
                         (E (cdr s))))
                  (else
                   (if (= (car s) 1)
                       (cons #t (E (cdr s)))
                       (cons #t (E (cons (- (car s) 1) (cdr s)))))))))) E) s))

;;; Apply a structure to make a flat list fit into a skeleton
;;; TODO! IMPORTANT: REMOVE TICKER

(define (apply-skeleton s l)
  ((letrec ((next (ticker! l))
            (E (lambda (s)
                 (cond
                  ((null? s) '())
                  ((list? (car s))
                   (cons (E (car s))
                         (E (cdr s))))
                  (else
                   (if (= (car s) 1)
                       (cons (next) (E (cdr s)))
                       (cons (next) (E (cons (- (car s) 1) (cdr s)))))))))) E) s))

;-------------------------------------------------------------------------------
; Distances
;-------------------------------------------------------------------------------

;;; Calculates the hamming distance (number of different positions) of two lists
;;; of equal length

(define (hamming-distance la lb)
  ((letrec ((H (lambda (a b d)
                 (cond
                  ((and (null? a) (null? b))
                   d)
                  ((xor (null? a) (null? b))
                   (error "lists have different length"))
                  ((equal? (car a) (car b))
                   (H (cdr a) (cdr b) d))
                  (else
                   (H (cdr a) (cdr b) (+ d 1))))))) H) la lb 0))

;;; Calculates the minimum hamming distance between a list and the permutations
;;; of the second

(define (min-hamming-distance la lb)
  ((letrec ((H (lambda (a b d)
                 (if (null? a)          ; a always decreases
                     (if (= d (length b)) ; the length must be equal to the times it wasn't cdr'd
                         d
                         (error "lists have different length"))
                     (let ((newb (find-rember (car a) b))) ; Removes the first instance if found
                       (if newb
                           (H (cdr a) newb d)
                           (H (cdr a) b (+ d 1)))))))) H) la lb 0))

;;; Takes a sublist from start to end positions

;-------------------------------------------------------------------------------
; Sublist operations
;-------------------------------------------------------------------------------

;;; Return a sublist from a start to an end positions

(define (slice l start end)
  (take (drop l start)
        (- end start)))

(define (slice! l start end)
  (take! (drop l start)
         (- end start)))
 
;;; Return two lists of lengths differing with at most one

(define (split-in-halves l) ; TODO: currently reverses first list
  (let loop ((front '())
             (slow  l)
             (fast  l))
    (cond
     ((null? fast)
      (values front
              slow))
     ((null? (cdr fast))
      (values (cons (car slow) front)
              (cdr slow)))
     (else
      (loop (cons (car slow) front)
            (cdr slow)
            (cddr fast))))))

(define (split-in-halves! l)
  (let loop ([slow (cons 'foo l)]
             [fast (cons 'bar l)])
    (cond
     [(or (null? fast)
          (null? (cdr fast))) (let ([back (cdr slow)])
          (set-cdr! slow '())
          (values l back))]
     [else                    (loop (cdr slow)
                                    (cddr fast))])))

;-------------------------------------------------------------------------------
; Random picking
;-------------------------------------------------------------------------------

;;; Pick a random element

(define (pick-random l)
  (list-ref l (random-integer (length l))))

;;; Pick a random element with known list length

(define (pick-random/length l len)
  (list-ref l (random-integer len)))

;;; Pick a number of random elements without repetition

(define (pick-random//repetition l n)
  (let recur ((l l)
              (n n)
              (picked '()))
    (if (or (null? l) (zero? n))
        picked
        (receive (fore aft)
                 (split-at l (random-integer (length l)))
                 (recur (append fore (cdr aft))
                        (sub1 n)
                        (cons (car aft) picked))))))

;;; Pick a random element and return also the list without that element

(define (pick-random+remove l)
  (receive (a b)
           (split-at l
                     (random-integer (length l)))
           (values
            (car b)
            (append a (cdr b)))))

;;; Pick a random element and return also the list without that element

(define (pick-random+remove/length l len)
  (receive (a b)
           (split-at l
                     (random-integer len))
           (values
            (car b)
            (append a (cdr b)))))

;-------------------------------------------------------------------------------
; Miscellaneous
;-------------------------------------------------------------------------------

;;; Creates a destructive function to read a list sequantially after each call

(define (ticker! l)
  (lambda ()
    (begin0 (car l)
            (set! l (cdr l)))))