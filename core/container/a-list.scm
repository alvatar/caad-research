;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

(import (std srfi/1)
        ../list
        ../debugging)

;;; Make a new, fresh a-list cloning a given one

(define (a-list:clone al)
  (%accept (alist? al))
  (map (lambda (p) (receive (car cdr)
                       (car+cdr p)
                       (cons car cdr)))
       al))
  
;;; Drop-in destructive replacement for assq. If element is found, move it
;;; if is not in the "front" of the list.
;;; -> (symbol alist) -> element

;;; INEFFICIENT! FIND A SOLUTION

(define (a-list:make-assq-reorder! front-count) ; TODO! dummy-alist should be the actual input list
  ;; Don't move (name . method) if at front of name-method-alist
  ;; front-count determines "front"
  (lambda (sym al)
    (let count-loop ((count 0)
                     (alist al))
      (cond
       ((null? alist) #f)
       ((eq? sym (caar alist)) (car alist))
       ((< count front-count) (count-loop (+ count 1) (cdr alist)))
       (else
        (%accept (>= count front-count) "count is smaller than short-count!")
        (let move-loop ((left-reversed '())
                        (right alist))
          (cond
           ((null? right) #f)
           ((eq? sym (caar right))
            (set-cdr! alist (append! (reverse! left-reversed)
                                     (cdr right)))
            (set-car! alist (car right))
            (car right))
           (else (move-loop (cons (car right) left-reversed)
                            (cdr right))))))))))
