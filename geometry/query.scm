;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Geometric queries: search and sort
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        ../core/functional
        ../core/list
        kernel)

;-------------------------------------------------------------------------------
; Extremes
;-------------------------------------------------------------------------------

;;; General extreme query

(define (find.extreme comparator selector
                      tie-comparator tie-selector
                      pseq)
  (reduce
   (lambda (p extreme)
     (let ((a (selector p))
           (b (selector extreme)))
      (cond
       ((comparator a b) extreme)
       ((= a b) (cond
                 ((tie-comparator (tie-selector p)
                                  (tie-selector extreme)) extreme)
                 (else p)))
       (else p))))
   (car pseq)
   pseq))

;;; pseq right-most point

(define pseq:extreme-right (curry find.extreme < point-x < point-y))

;;; pseq left-most point

(define pseq:extreme-left (curry find.extreme > point-x > point-y))

;;; pseq top-most point

(define pseq:extreme-top (curry find.extreme < point-y < point-x))

;;; pseq bottom-most point

(define pseq:extreme-bottom (curry find.extreme > point-y > point-x))

;;;
;;; TODO: these "longest" finding procedures should give information on unique results

;;; Find the direction that describes the longest possible diagonal

(define (find.longest-diagonal pseq)
  (max/generator segment:squaredlength
                 (pseq:diagonals pseq)))

;;; Find the direction that describes the longest possible segment connectig
;;; side mid-points

(define (find.longest-midsegment pseq)
  (max/generator segment:squaredlength
                 (pseq:midsegments pseq)))

;;; Find the direction that describes the longest possible segment produced by
;;; the intersection of a line passing through the center of the polygon

(define (find.longest-intersections pseq)
  (error "unimplemented"))

;-------------------------------------------------------------------------------
; Nearest
;-------------------------------------------------------------------------------

;;; Find in a pseq the segment that is nearest to a point

(define (find.nearest-segment-to-point/pseq p pseq) ; TODO: should return several in case they are at the same dist?
  (%accept (and (pseq? pseq) (> (length pseq) 1)) "not a proper pseq")
  (let ((first-segment (make-segment (car pseq) (cadr pseq))))
    (if (> (length pseq) 2)
        (pair-fold-2 (lambda (tail acc)
                       (let ((current-segment (make-segment (car tail)
                                                            (cadr tail))))
                         (if (< (squareddistance.point-segment p current-segment)
                                (squareddistance.point-segment p acc))
                             current-segment
                             acc)))
                     (make-segment (car pseq) (cadr pseq))
                     (cddr pseq))
        first-segment)))

;;; Find in a pseq the segment that is nearest to a point

(define (find.nearest-segment-to-point/segment-list p sl) ; TODO: should return several in case they are at the same dist?
  ;; TODO: not tested!
  (%accept (and (list? pseq) (segment? (car sl))) "not a proper segment list")
  (if (> (length sl) 1)
      (pair-fold-2 (lambda (tail acc)
                     (let ((current-segment (car tail)))
                       (if (< (squareddistance.point-segment p current-segment)
                              (squareddistance.point-segment p acc))
                           current-segment
                           acc)))
                   (car sl)
                   (cdr sl))
      (car sl)))

;-------------------------------------------------------------------------------
; Locus
;-------------------------------------------------------------------------------

;;; Direction of the line passing through a point and perpendicular to a pseq

(define (point&pseq-perpendicular->direction p pseq)
  (direction:perpendicular (segment->direction
                            (find.nearest-segment-to-point/pseq p pseq))))
