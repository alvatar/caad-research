;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std string/xml-to-sxml))
(import (std misc/uuid))

(import web/parse/ssax-sxml/sxml-tools/sxpath)

(import core/syntax)
(import geometry/kernel)
;(import math/exact-algebra)
;(import math/inexact-algebra)
(import sxml-graph)

(import visualization)

;-------------------------------------------------------------------------------
; Types
;-------------------------------------------------------------------------------

(define-structure graph uid architecture north)

(define-structure wall uid pseq windows doors)

(define-structure window pseq from to)

(define-structure door pseq from to)
    
(define-structure room uid walls)

(define-structure structural uid pseq)

(define-structure entry pseq wall-uid door-number)

(define-structure pipe position)

;-------------------------------------------------------------------------------
; Selectors
;-------------------------------------------------------------------------------

(define (graph:find-rooms g)
  (filter (lambda (e) (room? e)) (graph-architecture g)))

(define (graph:find-walls g)
  (filter (lambda (e) (wall? e)) (graph-architecture g)))

(define (graph:find-structurals g)
  (filter (lambda (e) (structural? e)) (graph-architecture g)))

(define (graph:find-entries g)
  (filter (lambda (e) (entry? e)) (graph-architecture g)))

(define (graph:find-pipes g)
  (filter (lambda (e) (pipe? e)) (graph-architecture g)))

(define (graph:find-wall/uid graph uid)
  (aif element (find
                (lambda (e) (equal? uid (wall-uid e)))
                (graph:find-walls graph))
       element
       (begin (display "UID: ")(display uid)(newline)
              (error "Wall with such UID not found"))))

;;; TODO: memoize?
(define (find-room-walls graph room)
  (map (lambda (r) (graph:find-wall/uid graph r)) (room-walls room)))

;-------------------------------------------------------------------------------
; Sxml-grap / graph conversion
;-------------------------------------------------------------------------------

(define (sxml-graph->graph sxmlgraph)
  (make-graph
   (sxml:element-uid sxmlgraph)
   (map
    (lambda (e)
      (let ((type (sxml:element-type e)))
        (cond
         ((equal? type 'structural)
          (make-structural (sxml:element-uid e)
                           (sxml:structural->pseq sxmlgraph e)))
         ((equal? type 'entry)
          (make-entry '(sxml:entry->pseq sxmlgraph e)
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
    (sxml:contents sxmlgraph))
   (make-direction #e1 #e1)))

(define (graph->sxml-graph graph)
  graph)
