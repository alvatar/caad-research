(import (std srfi/1))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Get coordinate from point
;;
(define (point-coord coordinate point)
  (define (find-coordinate point)
    (cond
     ((null-list? point)
      (raise "You sent me a null point. Seriously, what should I do with this?? Boy, I'm having a bad day thanks to you."))
     ((equal? (caar point) coordinate)
      (string->number (cadar point)))
     (else
      (find-coordinate (cdr point)))))
  (find-coordinate point))

;; Extract a list of point coordinates
;;
(define (extract-point-coords point)
  `(,(point-coord 'x point)
    ,(point-coord 'y point)))

;; Get point n from point list
;;
(define (point-n n point-list)
  (cdr (list-ref point-list n)))

;; Make point
;;
(define (make-point x y)
  (if (or (null? x) (null? y))
      (raise "Error making point: null arguments")
      (list (list 'y (number->string y))
            (list 'x (number->string x)))))

;; Calculate absolute point given segment and percentage
;;
(define (point-from-relative-in-segment point-a point-b percentage)
  (if (or (null-list? point-a) (null-list? point-b))
    (raise "Wrong points passed")
    (let* ((Ax (point-coord 'x point-a))
           (Ay (point-coord 'y point-a))
           (ABx (- (point-coord 'x point-b) Ax))
           (ABy (- (point-coord 'y point-b) Ay)))
      (make-point
        (+ Ax (* ABx percentage))
        (+ Ay (* ABy percentage))))))

;; Calculate the distance between two points
;;
(define (distance-point-point a b)
  (let ((p1 (extract-point-coords a))
        (p2 (extract-point-coords b)))
    (sqrt (+ (expt (- (car p1) (car p2)) 2)
             (expt (- (cadr p1) (cadr p2)) 2)))))
