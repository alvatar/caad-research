;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

(import core/list-records
        web/parse/ssax-sxml/sxml-tools/sxpath)

;-------------------------------------------------------------------------------
; Types
;-------------------------------------------------------------------------------

(define-type graph uid environment architecture)
;; (define-list-record-type graph
;;   (make-graph uid environment architecture)
;;   graph?
;;   (uid graph-uid)
;;   (environment graph-environment)
;;   (architecture graph-architecture))

(define-type wall uid metadata pseq windows doors)
;; (define-list-record-type wall
;;   (make-wall uid metadata pseq windows doors)
;;   wall?
;;   (uid wall-uid)
;;   (metadata wall-metadata)
;;   (pseq wall-pseq)
;;   (windows wall-windows)
;;   (doors wall-doors))

;;; Make a wall without metadata, doors or windows

(define (make-wall-plain uid pseq)
  (make-wall uid '((type "new")) pseq '() '()))

(define-type window pseq from to)
;; (define-list-record-type window
;;   (make-window pseq from to)
;;   window?
;;   (pseq window-pseq)
;;   (from window-from)
;;   (to window-to))

(define-type door pseq from to)
;; (define-list-record-type door
;;   (make-door pseq from to)
;;   door?
;;   (pseq door-pseq)
;;   (from door-from)
;;   (to door-to))
    
(define-type room uid walls)
;; (define-list-record-type room
;;   (make-room uid walls)
;;   room?
;;   (uid room-uid)
;;   (walls room-walls))

(define-type structural uid pseq)
;; (define-list-record-type structural
;;   (make-structural uid pseq)
;;   structural?
;;   (uid structural-uid)
;;   (pseq structural-pseq))

(define-type entry pseq wall-uid door-number wall-point)
;; (define-list-record-type entry
;;   (make-entry pseq wall-uid door-number wall-point)
;;   entry?
;;   (pseq entry-pseq)
;;   (wall-uid entry-wall-uid)
;;   (door-number entry-door-number)
;;   (wall-point entry-wall-point)

(define-type pipe position)
;; (define-list-record-type pipe
;;   (make-pipe position)
;;   pipe?
;;   (position pipe-position))
