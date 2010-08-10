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

(import (std srfi/1)
        functional
        syntax)

;-------------------------------------------------------------------------------
; Basic
;-------------------------------------------------------------------------------

;;; add 1

(define (add1 x) (+ x 1))

;;; substract 1

(define (sub1 x) (- x 1))

;;; not null?

(define-syntax not-null?
  (syntax-rules ()
    ((_ l)
     (not (null? l)))))

;;; atom?

(define atom? (lambda (x) (and (not (pair? x)) (not-null? x))))

;;; XOR

(define (xor a b) (if a (not b) b))

;;; snoc (always prefer the use of cons

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
                                          (values (cons a cars)
                                                  (cons d cdrs))))))
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

;-------------------------------------------------------------------------------
; Map/fold variants
;-------------------------------------------------------------------------------

;;; Recursive map

(define (map* f l)
  (cond
   ((null? l) '())
   ((atom? l) (f l))
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
    ;; Implicit with selector
    ((_ ((?letb <- ?s) ...) ((?p ?f) ...) ?l . ?lt) ; entry for implicit vars with selector
     (map-cond "expl/sel" ((?letb ?s) ...) () ((?p ?f) ...) () (?l . ?lt)))
    ((_ "expl/sel" ?let-block (?vars ...) ((?p ?f) ...) (?l ...) (?lh . ?lls)) ; recur
     (map-cond "expl/sel" ?let-block (?vars ... x) ((?p ?f) ...) (?l ... ?lh) ?lls))
    ((_ "expl/sel" ?let-block (?vars ...) ((?p ?f) ...) (?l ...) ()) ; finalize
     (map-cond "expl/sel-init-let" (?vars ...) ?let-block ((?p ?f) ...) (?l ...)))

    ((_ "expl/sel-init-let" ?vars ((?bind ?s) . ?ct) ?cond-block (?l ...)) ; init let
     (map-cond "expl/sel-let" ?vars ?ct ((?bind (?s . ?vars))) ?cond-block (?l ...)))
    ((_ "expl/sel-let" ?vars ((?bind ?s) . ?ct) (?bind-list ...) ?cond-block (?l ...)) ; recur let
     (map-cond "expl/sel-let" ?vars ?ct (?bind-list ... (?bind (?s . ?vars))) ?cond-block (?l ...)))
    ((_ "expl/sel-let" ?vars () (?bind-list ...) ?cond-block (?l ...)) ; finalize let
     (map-cond "expl/sel-init-cond" ?cond-block ?vars (?bind-list ...) (?l ...)))

    ((_ "expl/sel-init-cond" ((else ?f) . ?ct) ?vars ?let-block  (?l ...)) ; error: else is first
     (error "Syntax error: else clause can't be first"))
    ((_ "expl/sel-init-cond" ((?p ?f) . ?ct) ?vars ?let-block (?l ...)) ; init
     (map-cond "expl/sel-cond" ?ct ((?p ?f)) ?vars ?let-block (?l ...)))
    ((_ "expl/sel-cond" ((else ?f)) (?conds ...) ?vars ?let-block (?l ...)) ; catch given 'else'
     (map (lambda ?vars (let ?let-block (cond ?conds ... (else ?f)))) ?l ...))
    ((_ "expl/sel-cond" ((else ?f) . ?ct) (?conds ...) ?vars ?let-block (?l ...)) ; error: else is not last
     (error "Syntax error: else clause must be last"))
    ((_ "expl/sel-cond" ((?p ?f) . ?ct) (?conds ...) ?vars ?let-block (?l ...)) ; recur
     (map-cond "expl/sel-cond" ?ct (?conds ... (?p ?f)) ?vars ?let-block (?l ...)))
    ((_ "expl/sel-cond" () (?conds ...) ?vars ?let-block (?l ...)) ; finalize cond
     (map (lambda ?vars (let ?let-block (cond ?conds ... (else #f)))) ?l ...))

    ;; Explicit
    ((_ (?vars ...) ((?p ?f ...) ...) ?l ...) ; entry for explicit vars case
     (map-cond "expl-init" (?vars ...) ((?p ?f ...) ...) ?l ... ))

    ((_ "expl-init" (?vars ...) ((else ?f ...) . ?ct) ?l ...) ; error: else is first
     (error "Syntax error: else clause can't be first"))
    ((_ "expl-init" (?vars ...) (((?p ...) ?f ...) . ?ct) ?l ...) ; init explicit-vars
     (map-cond "expl-cond" (?vars ...) ?ct (((?p ...) ?f ...)) (?l ...)))

    ((_ "expl-cond" (?vars ...) ((else ?f . ?ft)) (?conds ...) (?l ...)) ; catch given 'else'
     (map (lambda (?vars ...) (cond ?conds ... (else ?f . ?ft))) ?l ...))
    ((_ "expl-cond" (?vars ...) ((else ?f . ?ft) . ?ct) (?conds ...) (?l ...)) ; error: else is not last
     (error "Syntax error: else clause must be last"))
    ((_ "expl-cond" (?vars ...) (((?p ...) ?f ...) . ?ct) (?conds ...) (?l ...)) ; recur explicit-vars
     (map-cond "expl-cond" (?vars ...) ?ct (?conds ... ((?p ...) ?f ...)) (?l ...)))
    ((_ "expl-cond" (?vars ...) () (?conds ...) (?l ...)) ; finalize with default 'else'
     (map (lambda (?vars ...) (cond ?conds ... (else #f))) ?l ...))

    ;; Implicit
    ((_ ((?p ?f) ...) ?l . ?lt) ; entry for given vars case
     (map-cond "impl" () ((?p ?f) ...) () (?l . ?lt)))
    ((_ "impl" (?vars ...) ((?p ?f) ...) (?l ...) (?lh . ?lls)) ; recur vars
     (map-cond "impl" (?vars ... x) ((?p ?f) ...) (?l ... ?lh) ?lls))
    ((_ "impl" (?vars ...) ((?p ?f) ...) (?l ...) ()) ; finalize vars
     (map-cond "impl-cond-init" (?vars ...) ((?p ?f) ...) (?l ...)))

    ((_ "impl-cond-init" ?vars ((else ?f) . ?ct) (?l ...)) ; error: else is first
     (error "Syntax error: else clause can't be first"))
    ((_ "impl-cond-init" ?vars ((?p ?f) . ?ct) (?l ...)) ; init cond
     (map-cond "impl-cond" ?vars ?ct (((?p . ?vars) (?f . ?vars))) (?l ...)))
    ((_ "impl-cond" ?vars ((else ?f)) (?conds ...) (?l ...)) ; catch given 'else' in cond
     (map (lambda ?vars (cond ?conds ... (else (?f . ?vars)))) ?l ...))
    ((_ "impl-cond" ?vars ((else ?f) . ?ct) (?conds ...) (?l ...)) ; error: else is not last
     (error "Syntax error: else clause must be last"))
    ((_ "impl-cond" ?vars ((?p ?f) . ?ct) (?conds ...) (?l ...)) ; recur cond
     (map-cond "impl-cond" ?vars ?ct (?conds ... ((?p . ?vars) (?f . ?vars))) (?l ...)))
    ((_ "impl-cond" ?vars () (?conds ...) (?l ...)) ; finalize cond with default 'else'
     (map (lambda ?vars (cond ?conds ... (else #f))) ?l ...))

    ;; Global wrong syntax cases
    ((_ ((any ...) ...) thing ...) ; detect wrong syntax
     (error "Syntax error: wrong number of arguments in condition"))))

;;; Map that generates a value for each element
;;; (map/values (lambda (x y z) (values x y z)) '(a 1) '(b 2) '(c 3))
;;; => (a b c)
;;;    (1 2 3)
;;;
;;;     A          B          C
;;;     +----+     +----+     +----+lists
;;;  ---+----+-----+----+-----+----+--------> (f A0 B0 C0) ----> val1
;;;     |0   |     |0   |     |0   |
;;;     +----+     +----+     +----+
;;;  ---+----+-----+----+-----+----+--------> (f A1 B1 C1) ----> val2
;;;     |1   |     |1   |     |1   |
;;;     +----+     +----+     +----+
;;;  ---+----+-----+----+-----+----+--------> (f A2 B2 C2) ----> val3
;;;     |2   |     |2   |     |2   |
;;;     +----+     +----+     +----+

(define (map/values f . ls)
  (list->values
   (apply map (lambda args (values->list (apply f args))) ls)))

;;; Fold that accumulates several values
;;; (fold/values (lambda (x a b) (values (cons (+ 1 x) a) (cons x b))) '(() ()) '(1 2 3 4 5))
;;; => (6 5 4 3 2)
;;;    (5 4 3 2 1)
;;; (fold/values (lambda (x a b) (values (cons (car x) a) (cons (cadr x) b))) '(() ()) '((a 1) (b 2) (c 3)))
;;; => (c b a)
;;;    (3 2 1)

(define (fold/values kons knil . ls)
  (list->values
   (apply fold
          (lambda args
            (let ((rev (reverse args))) ; (x . y (a . b)) -> (x . y . a . b)
              (values->list (apply kons (append (cdr rev) (car rev))))))
          knil
          (reverse ls))))

;;; Demultiplex a list
;;; (demux (lambda (x) (values (car x) (cadr x))) '((a 1) (b 2) (c 3)))
;;; => (a b c)
;;;    (1 2 3)
;;;
;;;                                 +---+---+---+---+---+list
;;;                                 |0A |1A |2A |3A |4A |------> val1
;;;    +---+list            A-->    +-o-+-o-+-o-+---+---+
;;;    |0  o------+        /         |   |   |
;;;    +---+      |---> f*  - - - - -+ - + - +
;;;    |1  o------+        \         |   |   |
;;;    +---+      |         B-->    +-o-+-o-+-o-+---+---+list
;;;    |2  o------+                 |0B |1B |2B |3B |4B |------> val2
;;;    +---+                        +---+---+---+---+---+
;;;    |3  | - - -+
;;;    +---+
;;;    |4  | - - -+ 
;;;    +---+

(define (demux f lis1 . lists)
  (if (pair? lists)
      (cons lis1 lists)
      (let recur ((l lis1))
        (if (null? l)
            (apply values
                   (make-list
                    (values-length (f (car lis1)))
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

;;; Apply a function to values, interleaving multiple sources
;;; (apply/values (lambda (x y) (cons x y)) (values 'a 'b 'c 'd) (values 1 2 3 4))
;;;
;;;              g1 -------+
;;;             /          |
;;;            /           |
;;;           /            |
;;;          /             +--------> (f g1 h1) --> val1
;;; g* -->  o--- g2 ----------+
;;;          \             |  |
;;;   |       \            |  |
;;;   |        \           |  |
;;;   |         \          |  |
;;;   |          g3 ----+  |  |
;;; values              |  |  +-----> (f g2 h2) --> val2
;;;   |          h1 -------+  |
;;;   |         /       |     |
;;;   |        /        |     |
;;;   |       /         |     |
;;;          /          |     |
;;; h* -->  o--- h2 ----------+
;;;          \          +-----------> (f g3 h3) --> val3
;;;           \         |
;;;            \        |                            ...
;;;             \       |
;;;              h3 ----+

(define-syntax apply/values
  (syntax-rules ()
    ((_ "init-transformation" ?l . ?ls)
     (apply/values "transformation" ((values->list ?l)) . ?ls))
    ((_ "transformation" (?tr ...))
     (list ?tr ...))
    ((_ "transformation" (?tr ...) ?l . ?ls)
     (apply/values "transformation" (?tr ... (values->list ?l)) . ?ls))
    ((_ ?f ?ls ...)
     (list->values
      (map (lambda (e) (apply ?f e)) (apply zip (apply/values "init-transformation" ?ls ...)))))))

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

;;; Try to find an element and remove it, yields #f if not found

(define (find-rember pred lis)
  (let/cc failed
   ((letrec ((R (lambda (l)
                  (if (null? l)
                      (failed #f)
                      (receive (h t) (car+cdr l)
                               (if (pred h)
                                   t
                                   (cons h (R t)))))))) R) lis)))

;;; Try to find an element, yielding #f if not found. It returns both the element
;;; and the list with that element removed

(define (find+rember pred lis)
  (let/cc failed
   ((letrec ((R (lambda (l)
                  (if (null? l)
                      (failed #f lis)
                      (receive (h t) (car+cdr l)
                               (if (pred h)
                                   (values h t)
                                   (receive (newhead newtail)
                                            (R t)
                                            (values newhead
                                                    (cons h newtail))))))))) R) lis)))

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

;;; Find the element that satisfies the predicate against all the other elements

(define (most pred lis)
  (reduce (lambda (a b) (if (pred b a) b a)) #f lis))

;;; Most, but return also the list with that element removed
;;; TODO: Benchmark!!!!!

(define (most+rember pred lis)
  (let recur ((l lis)
              (list-common '())
              (list-rembered '())
              (ans (car lis)))
    (if (null? l)
        (values ans
                (append! list-common (cdr list-rembered)))
        (receive (h t) (car+cdr l)
                 (if (pred h ans)
                     (recur t
                            (append! list-common list-rembered)
                            (list h)
                            h)
                     (recur t
                            list-common
                            (append! list-rembered (list h))
                            ans))))))
;; (define (most+rember pred lis)
;;   (let ((res (most pred lis)))
;;     (values res
;;             (rember res lis))))

;;; MOST using a generator instead of a comparator predicate

(define (most/generator generator comparator lis)
  (let iter ((ans (car lis))
             (current-max (generator (car lis)))
             (rest (cdr lis)))
    (if (null? rest)
        ans
        (receive (h t)
                 (car+cdr rest)
                 (let ((val (generator h)))
                   (if (comparator val current-max)
                       (iter h val t)
                       (iter ans current-max t)))))))

;;; MAX/MIN standard functions with a comparable number generator. Similar to MOST,
;;; but compares the numbers generated

(define (max/generator generator lis)
  (most/generator generator > lis))

(define (min/generator generator lis)
  (most/generator generator < lis))

;;; Recursive substitution in a list

(define (subst* new old l)
  (xsubst* cons new old l))

;;; Recursive substitution with multiple insertion

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

;;; Make a flat list fit into a skeleton
;;; TODO: remove ticker, do with receive/values

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
                     (let ((newb (find-rember (curry eq? (car a)) b))) ; Removes the first instance if found
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
  (let loop ((slow (cons 'foo l))
             (fast (cons 'bar l)))
    (cond
     ((or (null? fast)
          (null? (cdr fast)))
      (let ((back (cdr slow)))
        (set-cdr! slow '())
        (values l back)))
     (else
      (loop (cdr slow)
            (cddr fast))))))

;-------------------------------------------------------------------------------
; Miscellaneous
;-------------------------------------------------------------------------------

;;; Creates a destructive function to read a list sequantially after each call

(define (ticker! l)
  (lambda ()
    (begin0 (car l)
            (set! l (cdr l)))))

