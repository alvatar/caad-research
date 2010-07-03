(define (make-numbered-value tag val) (cons tag val))
                                        ; accessors of the components of the value
(define (nvalue-tag tv) (car tv))
(define (nvalue-val tv) (cdr tv))

(define (return val)
  (lambda (curr_counter)
    (make-numbered-value curr_counter val)))


(define (>>= m f)
  (lambda (curr_counter)
    (let* ((m_result (m curr_counter))
           (n1 (nvalue-tag m_result))   ; result of the delayed computation
           (v  (nvalue-val m_result))   ; represented by m
           
           (m1 (f v))                   ; feed the result to f, get another m1
           )
      (m1 n1))))   

(define incr 
  (lambda (n)
    (make-numbered-value (+ 1 n) n)))

(define (make-node val kids)
  (>>=
   incr
   (lambda (counter)
     (return (cons (make-numbered-value counter val) kids)))))

(define (build-btree-r depth)
  (if (zero? depth) (make-node depth '())
      (>>=
       (build-btree-r (- depth 1))
       (lambda (left-branch)
         (>>=
          (build-btree-r (- depth 1))
          (lambda (right-branch)
            (make-node depth (list left-branch right-branch))))))))

(runM (build-btree 3) 100)

(define (build-btree depth)
  (if (zero? depth) (make-node depth '())
      (letM* ((left-branch (build-btree (- depth 1)))
              (right-branch (build-btree (- depth 1))))
             (make-node depth (list left-branch right-branch)))))
