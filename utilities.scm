(import (std srfi/1))

;; Take the first one of a list, if is not a list, take the element itself
;;
(define (first-or-element list-or-element)
  (if
    (list? list-or-element)
    (car list-or-element)
    (list-or-element)))

;; SRFI-1's span and break modified to make deep comparisons
;;
(define (span-deep pred lis)
  ;(check-arg procedure? pred span)
  (let recur ((lis lis))
    (if (null-list? lis) (values '() '())
      (let ((x (car lis)))
        (if (pred lis)
          (receive (prefix suffix) (recur (cdr lis))
                   (values (cons x prefix) suffix))
          (values '() lis))))))

(define (break-deep  pred lis) (span-deep  (lambda (x) (not (pred x))) lis))

