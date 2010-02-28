(import (std srfi/1))

;; Take the first one of a list, if is not a list, take the element itself
;;
(define (first-or-element list-or-element)
  (if
    (list? list-or-element)
    (car list-or-element)
    (list-or-element)))
