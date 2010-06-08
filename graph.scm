;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(compile-options force-compile: #t)

(import (std srfi/1))
(import (std string/xml-to-sxml))
(import (std misc/uuid))

(import web/parse/ssax-sxml/sxml-tools/sxpath)

(import core/syntax)
(import core/list-records)
(import geometry/kernel)
(import sxml-graph)

(import visualization)

;-------------------------------------------------------------------------------
; Types
;-------------------------------------------------------------------------------

;; (define-type graph uid environment architecture)
(define-list-record-type graph
  (make-graph uid environment architecture)
  graph?
  (uid graph-uid)
  (environment graph-environment)
  (architecture graph-architecture))

;; (define-type wall uid pseq windows doors)
(define-list-record-type wall
  (make-wall uid pseq windows doors)
  wall?
  (uid wall-uid)
  (pseq wall-pseq)
  (windows wall-windows)
  (doors wall-doors))

;; (define-type window pseq from to)
(define-list-record-type window
  (make-window pseq from to)
  window?
  (pseq window-pseq)
  (from window-from)
  (to window-to))

;; (define-type door pseq from to)
(define-list-record-type door
  (make-door pseq from to)
  door?
  (pseq door-pseq)
  (from door-from)
  (to door-to))
    
;; (define-type room uid walls)
(define-list-record-type room
  (make-room uid walls)
  room?
  (uid room-uid)
  (walls room-walls))

;; (define-type structural uid pseq)
(define-list-record-type structural
  (make-structural uid pseq)
  structural?
  (uid structural-uid)
  (pseq structural-pseq))

;; (define-type entry pseq wall-uid door-number)
(define-list-record-type entry
  (make-entry pseq wall-uid door-number)
  entry?
  (pseq entry-pseq)
  (wall-uid entry-wall-uid)
  (door-number entry-door-number))

;; (define-type pipe position)
(define-list-record-type pipe
  (make-pipe position)
  pipe?
  (position pipe-position))

;-------------------------------------------------------------------------------
; Sxml-grap / graph conversion
;-------------------------------------------------------------------------------

;;; SXML to graph conversion

(define (sxml-graph->graph sxmlgraph)
  (make-graph
   (sxml:element-uid sxmlgraph)
   (list (make-direction #e1 #e1))
   (map
    (lambda (e)
      (let ((type (sxml:element-type e)))
        (cond
         ((equal? type 'structural)
          (make-structural (sxml:element-uid e)
                           (sxml:structural->pseq sxmlgraph e)))
         ((equal? type 'entry)
          (make-entry (sxml:entry->pseq sxmlgraph e)
                      (sxml:entry-wall-uid e)
                      (sxml:entry-door-num e)))
         ((equal? type 'pipe)
          (make-pipe (sxml:pipe->center-position e)))
         ((equal? type 'wall)
          (make-wall (sxml:element-uid e)
                     (sxml:wall->pseq e)
                     (let ((windows (sxml:wall-windows e)))
                       (map (lambda (w)
                              (make-window
                               (sxml:wall-element->pseq w e)
                               (sxml:wall-element-relative-points 'from w)
                               (sxml:wall-element-relative-points 'to w))) windows))
                     (let ((doors (sxml:wall-doors e)))
                       (map (lambda (d)
                              (make-door
                               (sxml:wall-element->pseq d e)
                               (sxml:wall-element-relative-points 'from d)
                               (sxml:wall-element-relative-points 'to d))) doors))))
         ((equal? type 'room)
          (make-room (sxml:element-uid e)
                     (map (lambda (w) (sxml:element-uid w)) (sxml:room-wall-refs e)))))))
    (sxml:contents sxmlgraph))))

;;; Graph to SXML conversion

(define (graph->sxml-graph graph)
  graph)
