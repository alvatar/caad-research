(import (std srfi/1))

;; Take the first one of a list, if is not a list, take the element itself
;;
(define (first-or-element list-or-element)
  (if (list? list-or-element)
      (car list-or-element)
    (list-or-element)))

;; Rotates the list until this element is the first one
;; Warning! Can yield infinite loop. Improve
;;
(define (rotate-until pred lis)
  (define (iter lis-iter)
    (let ((x (car lis-iter)))
      (if (pred x)
          lis-iter
        (iter (cons (cdr lis-iter) x)))))
  (iter lis))
