;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/13
             srfi/48)
        core/debugging
        core/prototype
        geometry/kernel
        math/inexact-algebra)

;-------------------------------------------------------------------------------
; Types
;-------------------------------------------------------------------------------

;;; Object prototype: graph

(define-prototype-check graph?)
(define (new-graph uid environment architecture)
  (object ((uid uid) (environment environment) (architecture architecture))
          ((graph? self) #t)
          ((type self) 'graph)
          ((->string self) (string-append
                            "$graph: \n"
                            (string-concatenate
                             (map ->string architecture))))))
(define make-graph new-graph)
(define (graph-uid instance) ($ uid instance))
(define (graph-environment instance) ($ environment instance))
(define (graph-architecture instance) ($ architecture instance))

;;; Class prototype: wall

(define-prototype-check wall?)
(define new-wall
  (let ((prototype
         (object
          ()
          ((wall? self) #t)
          ((type self) 'wall)
          ((->string self)
           (string-append
            (let ((seg ($ segment self))) (format " $wall: a_~a b_~a ~%"
                                                  (segment-a seg)
                                                  (segment-b seg)))
            (let ((windows ($ windows self)))
              (if (null? windows)
                  ""
                  (string-concatenate (map ->string windows))))
            (let ((doors ($ doors self)))
              (if (null? doors)
                  ""
                  (string-concatenate (map ->string doors)))))))))
    (lambda (uid metadata segment windows doors)
      (object
       ((uid uid) (metadata metadata) (segment segment) (windows windows) (doors doors))
       ((delegate self) prototype)))))
(define make-wall new-wall)
(define (wall-uid instance) ($ uid instance))
(define (wall-metadata instance) ($ metadata instance))
(define (wall-segment instance) ($ segment instance))
(define (wall-windows instance) ($ windows instance))
(define (wall-doors instance) ($ doors instance))

;;; Class prototype: Window

(define-prototype-check window?)
(define new-window
  (let ((prototype
         (object ()
                 ((window? self) #t)
                 ((type self) 'window)
                 ((->string self) (format "  $window: a_~a b_~a ~%"
                                          (vect2:exact->inexact ($ point-a self))
                                          (vect2:exact->inexact ($ point-b self))))
                 ;; the segment is cached to avoid recalculation
                 ((segment self) (make-segment
                                  ($ point-a self)
                                  ($ point-b self)))
                 
                 ;; update keeping the segment as is
                 ((update/segment self parent) self) ; TODO
                 ;; update keeping the 2d absolute coords as they are
                 ((update/absolute-2d self parent) self) ; TODO
                 ;; update keeping the 1d absolute coords as they are
                 ((update/absolute-1d self parent) self) ; TODO
                 ;; update keeping the 1d relative coords as they are
                 ((update/relative-1d self parent) self))))
    (lambda (parent-wall kind . values)
      (case kind
        ((segment)
         (let ((segment (car values)))
           (object
            ((point-a (segment-a segment))
             (point-b (segment-b segment))
             (coord-1d-a #f)
             (coord-1d-b #f)
             (rel-a #f)
             (rel-b #f)
             (parent-wall parent-wall))
            ((delegate self) prototype))))
        ((absolute-2d)
         (object
          ((point-a (car values))
           (point-b (cadr values))
           (coord-1d-a #f)
           (coord-1d-b #f)
           (rel-a #f)
           (rel-b #f)
           (parent-wall parent-wall))
          ((delegate self) prototype)))
        ((absolute-1d) ; TODO
         '())
        ;; This is the main constructor, the one tha needs to be 
        ((relative-1d) ; TODO
         '())))))
(define (window-segment instance) ($ segment instance))

;;; Door

(define-prototype-check door?)
(define (new-door pseq from to)
  (object ((pseq pseq) (from from) (to to))
          ((door? self) #t)
          ((type self) 'door)
          ((->string self) (format "  $door: ~a ~%" pseq))))
(define make-door new-door)
(define (door-pseq instance) ($ pseq instance))
(define (door-from instance) ($ from instance))
(define (door-to instance) ($ to instance))

;;; Room

(define-prototype-check room?)
(define (new-room uid walls)
  (object ((uid uid) (walls walls))
          ((room? self) #t)
          ((type self) 'room)
          ((->string self) (format " $room: ~a ~%" walls))))
(define make-room new-room)
(define (room-uid instance) ($ uid instance))
(define (room-walls instance) ($ walls instance))

;;; Structural

(define-prototype-check structural?)
(define (new-structural uid pseq)
  (object ((uid uid) (pseq pseq))
          ((structural? self) #t)
          ((type self) 'structural)
          ((->string self) (format " $structural ~%"))))
(define make-structural new-structural)
(define (structural-uid instance) ($ uid instance))
(define (structural-pseq instance) ($ pseq instance))

;;; Entry

(define-prototype-check entry?)
(define (new-entry pseq wall-uid door-number wall-point)
  (object ((pseq pseq) (wall-uid wall-uid) (door-number door-number) (wall-point wall-point))
          ((entry? self) #t)
          ((type self) 'entry)
          ((->string self) (format " $entry ~%"))))
(define make-entry new-entry)
(define (entry-pseq instance) ($ pseq instance))
(define (entry-wall-uid instance) ($ wall-uid instance))
(define (entry-door-number instance) ($ door-number instance))
(define (entry-wall-point instance) ($ wall-point instance))

;;; Pipe

(define-prototype-check pipe?)
(define (new-pipe position)
  (object ((position position))
          ((pipe? self) #t)
          ((type self) 'pipe)
          ((->string self) (format " $pipe ~%"))))
(define make-pipe new-pipe)
(define (pipe-position instance) ($ position instance))