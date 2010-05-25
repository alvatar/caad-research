;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Tests for geometry package
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/64))
(import ../list)

;-------------------------------------------------------------------------------
(test-begin "list")
;-------------------------------------------------------------------------------

(test-equal
  "map-cond without else clause"
  (map-cond (((lambda (a b) (number? a)) (lambda (a b) (+ a b)))
             ((lambda (a b) (and (string? a) (string? b))) (lambda (a b) 'was-string)))
            (list 'r 1 1 1 1 "s")
            (list 'b 1 2 3 4 "s"))
  (list (list 'r 'b) 2 3 4 5 'was-string))

(test-equal
  "map-cond without else clause"
  (map-cond (((lambda (a b) (number? a)) (lambda (a b) (+ a b)))
             ((lambda (a b) (and (symbol? a) (symbol? b))) (lambda (a b) 'was-symbol))
             (else (lambda (a b) 'was-other)))
            (list 'r 0 1 2 4 "s")
            (list 'b 5 6 7 8 "s"))
  (list 'was-symbol 5 7 9 12 'was-other))

;-------------------------------------------------------------------------------
(test-end "list")
;-------------------------------------------------------------------------------
