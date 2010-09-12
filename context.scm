;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Procedures creating contexts and arguments for operators, serving as
;;; helpers between geometrical operations and high level-architectural
;;; operators
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        core/tagged-list
        core/container/n-ary
        core/debugging
        core/list
        graph
        graph-operations
        visualization)

(%activate-checks)

;-------------------------------------------------------------------------------
; Generic context & arguments builders
;-------------------------------------------------------------------------------

;;; Defines a context as a graph and a list of arbitrary elements

(define (many->context graph . many)
  (n-ary:make-node/children-list graph many))

;;; Define a context as a graph and a list of any element

(define (any->context graph any)
  (n-ary:make-node graph any))

;;; Define a context tree as a graph plus two layers of children

(define (2-layers->context graph layer-1 layer-2)
  (n-ary:make-node graph
                   (n-ary:make-node layer-1
                                    layer-2)))

;-------------------------------------------------------------------------------
; Specialized context & arguments builders
;-------------------------------------------------------------------------------

;;; Takes a graph and an infinite line and builds the context with the
;;; computation of their intersections

(define (line->context+arguments graph line)
  (receive (rooms walls intersections)
           (graph:relative-line-intersections graph line)
           (begin
             (%deny (or (null? walls)
                        (null? intersections))
                    "no intersections found, can't create context")
             (values
              `(,graph
                (,rooms ; TODO: DOESN'T WORK WITH MORE THAN ONE. IMPORTANT!
                 ,@walls))
              (list@ (split-points intersections))))))

;;; Takes a graph, a room and an infinite line and builds the context with
;;; the computation of the intersection between the room and the line

(define (room&line->context+arguments graph room line)
  (receive (walls intersections)
           (graph:room-relative-line-intersections graph room line)
           (begin
             (%deny (or (null? walls)
                        (null? intersections))
                    "no intersections found, can't create context")
             (values
              `(,graph
                (,room
                 ,@walls))
              (list@ (split-points intersections))))))

;-------------------------------------------------------------------------------
; Context checking
;-------------------------------------------------------------------------------
