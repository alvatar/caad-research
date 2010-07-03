;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monads
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Every monad consists of:
;;; 1. A type constructor M, which sends a type 'a' to a types 'M[a]'.
;;; 2. A function 'unit' 'a -> M[a]'
;;; 3. A function 'bind', of type '(M[a], a -> M[b]) -> M[b]'.
;;;
;;; (bind (unit x) f) == (f x)
;;; (bind M unit) == M
;;; (bind (bind M f) g) == (bind M (lambda (x) (bind (f x) g))) 

;;; Monads 'with zero' also consist of:
;;; 1. A value 'zero', of type 'M[a]'.
;;; 2. A function 'plus', of type '(M[a], M[a]) -> M[a]'.
;;;
;;; (plus m zero) == (plus zero m) == m
;;; (bind m (lambda (x) zero)) == zero
;;; (bind zero f) == zero 

;;; Do notation, like Haskell. From Oleg Kiselyov

(define-syntax doM
  (syntax-rules (def)
    ((_ (v <- expr) rest)
     (>>= expr (lambda (v) rest)))
    ((_ expr)
     expr)
    ((_ expr expr)
     (>>= (set expr) (lambda (dummy) expr)))
    ((_ expr expr* ...)
     (doM expr (doM expr* ...)))))

;;; LetM notation. Unhygienically refers to >>=

;; (define-syntax letM
;;   (sc-macro-transformer
;;    (lambda (form environment)
;;      (let ((binding (make-syntactic-closure environment '() (cadr form)))
;;            (expr (make-syntactic-closure environment '() (caddr form))))
;;        (apply
;;         (lambda (name-val)
;;           (apply (lambda (name initializer)
;;                    `(>>= ,initializer (lambda (,name) ,expr)))
;;                  name-val))
;;         binding)))))

(define-syntax letM
  (syntax-rules ()
    ((_ (( binding val)) expr)
     (>>= val (lambda (binding) expr)))))

;;; LetM*  notation. Unhygienically refers to >>=

;; (define-syntax letM*
;;   (sc-macro-transformer
;;    (lambda (form environment)
;;     (let ((bindings (make-syntactic-closure environment '() (cadr form)))
;;           (expr (make-syntactic-closure environment '() (caddr form))))
;;      (if (and (pair? bindings) (pair? (cdr bindings)))
;;          `(letM ,(list (car bindings))
;;                 (letM* ,(cdr bindings) ,expr))
;;          `(letM ,bindings ,expr))))))

(define-syntax letM*
  (syntax-rules ()
    ((_ (bindings) expr)
     (letM (bindings) expr))
    ((_ (binding . val) expr)
     (letM (binding)
           (letM* val expr)))))

;;; Run monad with init value

(define (runM m init)
  (m init))

;;; Example by Neelakantan Krishnaswami

;; (define (number-tree tree state)
;;   (if (pair? tree)
;;       (let-values ((lft state-1) (number-tree (car tree) state))
;;         (let-values ((rgt state-2) (number-tree (cdr tree) state-1))
;;           (values (cons lft rgt) state-2)))
;;       (values (+ state 1) (+ state 1))))
;;
;;
;; ; The type constructor (which could be a record also): M[a] = state -> (a, state)
;;
;; (define (unit x) (lambda (state) (values x state)))
;;
;; ; all bind does is thread the state through some calls.
;;
;; (define (bind m-a f)
;;    (lambda (state)
;;       (let-values ((a state*) (m-a state))
;;          ((f a) state*))))
;;
;; ; get takes a state and returns it as a value
;;
;; (define (get state) (values state state))
;;
;; ; set takes a state, and returns a monadic value that replaces
;; ; the old state with the new state
;;
;; (define (set new-state) (lambda (state) (values #f new-state))) 
;;
;; (define (number-tree tree)
;;   (if (pair? tree)
;;       (begin (bind (number-tree (car tree))
;;                    (lambda (left-subtree)
;;                      (bind (number-tree (cdr tree))
;;                            (lambda (right-subtree)
;;                              (unit (car left-subtree right-subtree)))))))
;;       (begin (bind get (lambda (n)
;;                          (bind (set (+ n 1)) (lambda (dont-care)
;;                                                (unit n)))))))) 
;;
;; (define (number-tree tree)
;;   (if (pair? tree)
;;       (do (left-subtree <- (number-tree (car tree)))
;;           (right-subtree <- (number-tree (cdr tree)))
;;           (unit (cons left-subtree right-subtree)))
;;       (do (n <- get)
;;           (set (+ n 1))
;;           (unit n))))
;;
;; (define (build-btree depth)
;;   (if (zero? depth) (make-node depth '())
;;       (letM* ((left-branch (build-btree (- depth 1)))
;;               (right-branch (build-btree (- depth 1))))
;;              (make-node depth (list left-branch right-branch)))))

;;; The 'option or maybe' monad

;; (define fail (vector 'fail))
;;
;; (define (unit x) x)
;;
;; (define (bind m f)
;;   (if (eq? m fail)
;;       fail
;;       (f m)))
;;
;; (define (foo-bar-baz x)
;;   (let ((foo-val (foo x)))
;;     (if (eq? foo-val fail)
;;         fail
;;         (let ((bar-val (bar foo-val)))
;;           (if (eq? bar-val fail)
;;               fail
;;               (baz bar-val))))))
;;
;; (define (foo-bar-baz x) (compose* baz (compose* bar (foo x))))
;; ; ...where compose* is the bind operation of the monad
