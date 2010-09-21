;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Basic contracts functionality
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Avoid contracts by default

(define-syntax define·i
  (syntax-rules ()
    ((_ (?def ...) (?i-contracts ...) ?exprs ...)
     (define (?def ...) ?exprs))))

(define-syntax define·o
  (syntax-rules ()
    ((_ (?def ...) (?o-contracts ...) ?exprs ...)
     (define (?def ...) ?exprs ...))))

(define-syntax define·io
  (syntax-rules ()
    ((_ (?def ...) ((?i-contracts ...) (?o-contracts ...)) ?exprs ...)
     (define (?def ...) ?exprs ...))))

;;; Input only contracts

(define-macro (%activate-contracts)
  '(begin
     (define-syntax define·i
       (syntax-rules ()
         ((_ (?proc ?args ...) (?i-contracts ...) . ?exprs)
          (define (?proc ?args ...)
            (if (and (?i-contracts ?args) ...)
                (begin . ?exprs)
                (raise
                 (list "input contracts not satisfied"
                       ?proc)))))))

;;; Output only contracts

     (define-syntax define·o
       (syntax-rules ()
         ((_ (?proc args ...) (?o-contracts ...) . ?exprs)
          (define (?proc args ...)
            (call-with-values
                (lambda () . ?exprs)
              (lambda vals
                (if (null? (cdr vals))
                    (let ((res (begin . ?exprs))) ; faster path
                      (if (?o-contracts ... res)
                          res
                          (raise
                           (list "output contracts not satisfied"
                                 ?proc))))
                    (let recur ((contr (list ?o-contracts ...))
                                (check-vals vals))
                      (cond
                       ((and (null? contr) (not (null? check-vals)))
                        (error "number of output contracts doesn't match the number of output values"))
                       ((null? check-vals)
                        (apply values vals))
                       (else
                        (if ((car contr) (car check-vals))
                            (recur (cdr contr) (cdr check-vals))
                            (raise
                             (list "output contracts not satisfied"
                                   ?proc)))))))))))))

;;; Input/output contracts (allows using -> as a separator as an alternate syntax)

     (define-syntax define·io
       (syntax-rules (->)
         ((_ (?proc ?args ...) ((?i-contracts ...) (?o-contracts ...)) . ?exprs)
          (define·io (?proc ?args ...) ((?i-contracts ...) -> (?o-contracts ...)) . ?exprs))
         ((_ (?proc ?args ...) ((?i-contracts ...) -> (?o-contracts ...)) . ?exprs)
          (define (?proc ?args ...)
            (if (and (?i-contracts ?args) ...)
                (let ((res (begin . ?exprs)))
                  (call-with-values
                      (lambda () . ?exprs)
                    (lambda vals
                      (if (null? (cdr vals))
                          (let ((res (begin . ?exprs))) ; faster path
                            (if (?o-contracts ... res)
                                res
                                (raise
                                 (list "output contracts not satisfied"
                                       ?proc))))
                          (let recur ((contr (list ?o-contracts ...))
                                      (check-vals vals))
                            (cond
                             ((and (null? contr) (not (null? check-vals)))
                              (error "number of output contracts doesn't match the number of output values"))
                             ((null? check-vals)
                              (apply values vals))
                             (else
                              (if ((car contr) (car check-vals))
                                  (recur (cdr contr) (cdr check-vals))
                                  (raise
                                   (list "output contracts not satisfied"
                                         ?proc))))))))))
                (raise
                 (list "input contracts not satisfied"
                       ?proc)))))))))