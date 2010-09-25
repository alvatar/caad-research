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
        math/inexact-algebra
        core/debugging
        core/prototype)

;-------------------------------------------------------------------------------
; Types
;-------------------------------------------------------------------------------

;; (define-list-record-type graph
;;   (make-graph uid environment architecture)
;;   graph?
;;   (uid graph-uid)
;;   (environment graph-environment)
;;   (architecture graph-architecture))
;; (define-list-record-type wall
;;   (make-wall uid metadata pseq windows doors)
;;   wall?
;;   (uid wall-uid)
;;   (metadata wall-metadata)
;;   (pseq wall-pseq)
;;   (windows wall-windows)
;;   (doors wall-doors))
;; (define-list-record-type window
;;   (make-window plan)
;;   window?
;;   (plan window-plan))
;; (define-list-record-type door
;;   (make-door pseq from to)
;;   door?
;;   (pseq door-pseq)
;;   (from door-from)
;;   (to door-to))
;; (define-list-record-type room
;;   (make-room uid walls)
;;   room?
;;   (uid room-uid)
;;   (walls room-walls))
;; (define-list-record-type structural
;;   (make-structural uid pseq)
;;   structural?
;;   (uid structural-uid)
;;   (pseq structural-pseq))
;; (define-list-record-type entry
;;   (make-entry pseq wall-uid door-number wall-point)
;;   entry?
;;   (pseq entry-pseq)
;;   (wall-uid entry-wall-uid)
;;   (door-number entry-door-number)
;;   (wall-point entry-wall-point)
;; (define-list-record-type pipe
;;   (make-pipe position)
;;   pipe?
;;   (position pipe-position))

;; (define-type graph uid environment architecture)
;; (define-type wall uid metadata segment windows doors)
;; (define-type window plan)
;; (define-type door pseq from to)
;; (define-type room uid walls)
;; (define-type structural uid pseq)
;; (define-type entry pseq wall-uid door-number wall-point)
;; (define-type pipe position)

;;; Graph

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

;;; Wall

(define-prototype-check wall?)
(define (new-wall uid metadata segment windows doors)
  (object ((uid uid) (metadata metadata) (segment segment) (windows windows) (doors doors))
          ((wall? self) #t)
          ((type self) 'wall)
          ((->string self) (string-append
                            (format " $wall: ~a ~%" (->string segment))
                            (if (null? windows) "" (string-concatenate (map ->string windows)))
                            (if (null? doors) "" (string-concatenate (map ->string doors)))))))
(define make-wall new-wall)
(define (wall-uid instance) ($ uid instance))
(define (wall-metadata instance) ($ metadata instance))
(define (wall-segment instance) ($ segment instance))
(define (wall-windows instance) ($ windows instance))
(define (wall-doors instance) ($ doors instance))

;;; Window

(define-prototype-check window?)
(define (new-window plan)
  (object ((plan plan))
          ((window? self) #t)
          ((type self) 'window)
          ((->string self) (format "  $window: ~a ~%" (map vect2:exact->inexact
                                                           plan)))))
(define make-window new-window)
(define (window-plan instance) ($ plan instance))

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