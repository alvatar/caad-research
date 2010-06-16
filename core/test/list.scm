;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block)
         (mostly-generic))
(compile-options force-compile: #t)

(import (std srfi/64))
(import ../list)

;-------------------------------------------------------------------------------
(test-begin "list" 11)
;-------------------------------------------------------------------------------

(test-equal
 "map-cond with only one list (explicit identifiers)"
 (map-cond (a)
           (((number? a)
             (number->string a))
            (else
             (string? a)))
           (list "a" 'b 0 1 2 'z 6))
 (list #t #f "0" "1" "2" #f "6"))

(test-equal
 "map-cond with default 'else' (explicit identifiers)"
 (map-cond (a b)
           (((number? b) (* a b))
            ((or (string? a) (string? b)) 'one-string))
           (list 'r 1 2 3 4 0)
           (list 'b 4 5 6 7 'r))
 (list #f 4 10 18 28 #f))

(test-equal
 "map-cond with default 'else' and returning simple values (explicit identifiers)"
 (map-cond (a b)
           (((number? b) a)
            ((or (string? a) (string? b)) 'one-string))
           (list 'r 1 2 3 4 0)
           (list 'b 4 5 6 7 'r))
 (list #f 1 2 3 4 #f))

(test-equal
 "map-cond with given 'else' (explicit identifiers)"
 (map-cond (a b)
           (((number? b) (* a b))
            ((or (string? a) (string? b)) 'one-string)
            (else a))
           (list 'r 1 2 3 4 0)
           (list 'b 4 5 6 7 "s"))
 (list 'r 4 10 18 28 'one-string))

(test-error
 "map-cond with else at the beginning (explicit identifiers)"
 (test-read-eval-string
  "(map-cond (a) ((else (string? a))
                    ((number? a) (number->string a)) )
            (list 'a 'b 0 1 2 'z 6))"))

(test-error
 "map-cond with else not at the end (explicit identifiers)"
 (test-read-eval-string
  "(map-cond (a) (((number? a) (number->string a))
                    (else (string? a))((number? a) (number->string a))
                    ((number? a) (number->string a)))
            (list 'a 'b 0 1 2 'z 6))"))

(test-equal
 "map-cond with only one list (explicit identifiers)"
 (map-cond (a) (((number? a) (number->string a))
                (else (string? a)))
           (list "a" 'b 0 1 2 'z 6))
 (list #t #f "0" "1" "2" #f "6"))

(test-equal
 "map-cond without else clause (implicit identifiers)"
 (map-cond (((lambda (a b) (number? a)) (lambda (a b) (+ a b)))
            ((lambda (a b) (and (string? a) (string? b))) (lambda (a b) 'was-string)))
           (list 'r 1 1 1 1 "s")
           (list 'b 1 2 3 4 "s"))
 (list #f 2 3 4 5 'was-string))

(test-equal
 "map-cond with else clause (implicit identifiers)"
 (map-cond (((lambda (a b) (number? a)) (lambda (a b) (+ a b)))
            ((lambda (a b) (and (symbol? a) (symbol? b))) (lambda (a b) 'was-symbol))
            (else (lambda (a b) 'was-other)))
           (list 'r 0 1 2 4 "s")
           (list 'b 5 6 7 8 "s"))
 (list 'was-symbol 5 7 9 12 'was-other))

(test-error
 "map-cond with else at the beginning (implicit identifiers)"
 (test-read-eval-string
  "(map-cond ((else string?)
                (number? number->string))
               (list 'a 'b 0 1 2 'z 6))"))

(test-error
 "map-cond with else not at the end (implicit identifiers)"
 (test-read-eval-string
  "(map-cond ((string? string->number)
                (else string?)
                (number? number->string))
               (list 'a 'b 0 1 2 'z 6))"))

;-------------------------------------------------------------------------------
(test-end "list")
;-------------------------------------------------------------------------------
