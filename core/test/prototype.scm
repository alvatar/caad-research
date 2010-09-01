;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;; Ported from Tiny Talk. Copyright (c) 2008 by Kenneth A Dicke

(import (std srfi/48
             srfi/64)
        ../prototype
        ../syntax)


(define-predicate point?)

(define (new-point #!key (x 0) (y 0))
  ;; 2d, exact-integer values for x,y
  ;; Intent is for pixel position graphics
  (unless (and (integer? x)
               (integer? y))
          (error 'new-point "x and y must be integers" x y))
  (object ([x x] [y y])
          ;;methods
          [(point? self) #t] ;; Yes, this is a point!
          [(name self) 'point]
          [(->string self) (format "(new-point x: ~a y: ~a)" [$ x self] [$ y self])]
          [(add self other)
           (cond
            ((point?  other) (new-point x: (+ [$ x self] [$ x other])
                                        y: (+ [$ y self] [$ y other])))
            ((number? other) (new-point x: (+ [$ x self] other)
                                        y: (+ [$ y self] other)))
            (else (error 'point:add "Can't add self to other" self other)))]
          [(=? self other)
           (unless (point? other)
                   (error 'point:=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (= [$ x self] [$ x other]) (= [$ y self] [$ y other]))]
          [(<? self other)
           (unless (point? other)
                   (error 'point:<?
                          "Don't know how compare point to non-point"
                          self other))
           (and (< [$ x self] [$ x other]) (< [$ y self] [$ y other]))]
          [(>? self other)
           (unless (point? other)
                   (error 'point:>?
                          "Don't know how compare point to non-point"
                          self other))
           (and (> [$ x self] [$ x other]) (> [$ y self] [$ y other]))]
          [(<=? self other)
           (unless (point? other)
                   (error 'point:<=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (<= [$ x self] [$ x other]) (<= [$ y self] [$ y other]))]
          [(>=? self other)
           (unless (point? other)
                   (error 'point:>=?
                          "Don't know how compare point to non-point"
                          self other))
           (and (>= [$ x self] [$ x other]) (>= [$ y self] [$ y other]))]
          [(min-point self other)
           (unless (point? other)
                   (error 'point:min-point
                          "Requires two points"
                          self other))
           (new-point x: (min [$ x self] [$ x other])
                      y: (min [$ y self] [$ y other]))]))

;-------------------------------------------------------------------------------
(test-begin "point")
;-------------------------------------------------------------------------------

(define p0 (new-point x:  0 y:   0))
(define p1 (new-point x: 23 y:  14))
(define p2 (new-point x:  7 y: 123))
(define p3 (new-point x: 17 y:   5))
(define p4 (new-point x: 20 y:  14))
(define (=? a b) [$ =? a b])

(test-equal "(point? 3)" (point? 3) #f)
(test-assert "point:=?" ($ =? p1 (new-point x: ($ x p1) y: ($ y p1))))
(test-equal "point:=? p1 p2" ($ =? p1 p2) #f)
(test-assert "(point? p1)" (point? p1))
(test-error "incorrect arguments with =?" ($ =? p1 3))
(test-assert "=?" ($ =? (new-point x: 30 y: 137) ($ add p1 p2)))
(test-assert "<=?" ($ <=? p0 p1))
(test-assert "<?" ($ <? p0 p1))
(test-equal "<?" ($ <? p4 p0) #f)
(test-assert ">=?" ($ >=? p1 p4))
(test-assert "min-point" ($ =? (new-point x: 7 y: 5) ($ min-point p2 p3)))



;-------------------------------------------------------------------------------
(test-end "point")
;-------------------------------------------------------------------------------

;; (define (vowel? c) (and (char? c) (memq c (string->list "aeiou")) #t))
;; (define (nv? x) (not (vowel? x)))
;; (define (id x) x) ;; identity

;; (add-test-suite 'tt-plus) ;; default-setup-thunk default-teardown-thunk)

;; (add-equal-test 'tt-plus "abcdef" [$ join "abc" "def"])
;; (add-equal-test 'tt-plus (vector 1 2 3 'a 'b 'c)
;;                 [$ join (vector 1 2 3) (vector 'a 'b 'c)])
;; (add-equal-test 'tt-plus (list 1 2 3 'a 'b 'c)
;;                 [$ join '(1 2 3) '(a b c)])
;; (add-eq-test 'tt-plus #t [$ every? "aeiou" vowel?])
;; (add-eq-test 'tt-plus #t [$ every? (string->list "aeiou") vowel?])
;; (add-eq-test 'tt-plus #t [$ every? (list->vector (string->list "aeiou")) vowel?])
;; (add-eq-test 'tt-plus #f [$ every? "aeixou" vowel?])
;; (add-eq-test 'tt-plus #f [$ every? (string->list "aeixou") vowel?])
;; (add-eq-test 'tt-plus #f [$ every? (list->vector (string->list "aeixou")) vowel?])
;; (add-eq-test 'tt-plus #t [$ any? (list->vector (string->list "aeiou")) vowel?])
;; (add-eq-test 'tt-plus #f [$ any? (list->vector (string->list "aeiou")) nv?])
;; (add-eq-test 'tt-plus #t [$ any? (string->list "aeixou") nv?])
;; (add-eq-test 'tt-plus #f [$ any? (string->list "aeiou" ) nv?])
;; (add-eq-test 'tt-plus #t [$ any? "aeixou" nv?])
;; (add-eq-test 'tt-plus #f [$ any? "aeiou"  nv?])
;; (add-eq-test 'tt-plus #t [$ any? "aeixou" nv?])
;; (add-test 'tt-plus "ths s  lttl strng"
;;           [$ reject  "this is a little string" vowel?] string=?)
;; (add-test 'tt-plus "ths s  lttl strng"
;;           [$ collect "this is a little string" nv?]
;;           =?)
;; (add-test 'tt-plus (string->list "ths s  lttl strng")
;;           [$ reject (string->list "this is a little string") vowel?]
;;           equal?)
;; (add-test 'tt-plus (string->list "ths s  lttl strng")
;;           [$ collect (string->list "this is a little string") nv?]
;;           =?)
;; (add-test 'tt-plus (list->vector (string->list "ths s  lttl strng"))
;;           [$ reject  (list->vector (string->list "this is a little string")) vowel?]
;;           =?)
;; (add-test 'tt-plus (list->vector (string->list "ths s  lttl strng"))
;;           [$ collect (list->vector (string->list "this is a little string")) nv?]
;;           =?)
;; (add-test 'tt-plus "123"          [$ map "123"          id] =?)
;; (add-test 'tt-plus (vector 1 2 3) [$ map (vector 1 2 3) id] =?)
;; (add-test 'tt-plus (list   1 2 3) [$ map (list   1 2 3) id] =?)
;; (add-test 'tt-plus "23"         [$ slice "012345"             2 4] =?)
;; (add-test 'tt-plus (vector 2 3) [$ slice (vector 0 1 2 3 4 5) 2 4] =?)
;; (add-test 'tt-plus (list   2 3) [$ slice (list   0 1 2 3 4 5) 2 4] =?)
;; (add-test 'tt-plus 6 [$ length "012345"] =?)
;; (add-test 'tt-plus 6 [$ length (vector 0 1 2 3 4 5)] =?)
;; (add-test 'tt-plus 6 [$ length (list   0 1 2 3 4 5)] =?)
;; (add-test 'tt-plus #\3 [$ iref "012345"             3] =?)
;; (add-test 'tt-plus   3 [$ iref (vector 0 1 2 3 4 5) 3] =?)
;; (add-test 'tt-plus   3 [$ iref (list   0 1 2 3 4 5) 3] =?)
;; (add-test 'tt-plus "123"          [$ shallow-clone "123"] =?)
;; (add-test 'tt-plus (vector 1 2 3) [$ shallow-clone (vector 1 2 3)] =?)
;; (add-test 'tt-plus (list   1 2 3) [$ shallow-clone (list   1 2 3)] =?)

