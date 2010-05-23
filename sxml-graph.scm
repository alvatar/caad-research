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
(import core/debug)
(import geometry/kernel)
(import math/exact-algebra)

(import visualization)

;-------------------------------------------------------------------------------
; Constants
;-------------------------------------------------------------------------------

(define wall-thickness 0.1)
(define graph-space-size-x maxx)
(define graph-space-size-y maxy)

;-------------------------------------------------------------------------------
; General
;-------------------------------------------------------------------------------

;;; Subgraph type

(define (sxml:element-type g)
  (car g))

;;; Is this a graph

(define (sxml:graph? graph)
  (equal? (graph-type graph) 'architecture))

;;; Get everything inside the graph as-is

(define (sxml:contents graph)
  ;((sxpath '(*)) graph))
  (if (null? graph)
      (error "You sent me a null graph. What should I do with this?")
    (cddr graph)))

;;; Get all parts of a graph that are of a specific type

(define (sxml:parts graph type)
  ((sxpath `(,@type)) graph))

;;; Get all parts of a graph

(define (sxml:all-parts graph)
  ((sxpath '(*)) graph))

;;; Get all walls in the graph

(define (sxml:walls graph)
  ((sxpath '(wall)) graph))

;;; Get rooms in the graph

(define (sxml:rooms graph)
  ((sxpath '(room)) graph))

;;; Get the pipes in the graph

(define (sxml:pipes graph)
  ((sxpath '(pipe)) graph))

;;; Get the entrances

(define (sxml:entries graph)
  ((sxpath '(entry)) graph))

;;; Get all the structural elements in the graph

(define (sxml:structurals graph)
  ((sxpath '(structural)) graph))

;;; Get the north direction

(define (sxml:north graph)
  (make-vect2 1 1))

;-------------------------------------------------------------------------------
; Element references and UID
;-------------------------------------------------------------------------------

;;; Get element's uid

(define (sxml:element-uid elem)
  (if (null? elem)
      (error "element-uid: Element is null")
    (cadar ((sxpath '(@ uid)) elem))))

;;; Make a list of uids contained in this subgraph
;;; TODO: wall??
(define (sxml:make-uid-list subgraph)
  ((sxpath '(wall @ uid *text*)) subgraph))

;;; Get the element from a reference element (consisting only of its uid)

(define (sxml:reference->element graph ref)
  (sxml:find-wall/uid graph (sxml:element-uid ref)))

;;; Get the elements from a reference list

(define (sxml:lreferences->lelements graph rlis)
  (map (lambda (r) (sxml:reference->element graph r)) rlis))

;;; Make a reference from an UID

(define (sxml:uid->reference type element-uid)
  `(,type (@ (uid ,sxml:element-uid))))

;;; Make a reference from an element

(define (sxml:element->reference element)
  `(,(car element) (@ (uid ,(sxml:element-uid element)))))

;;; Make a reference list from an element list

(define (sxml:lelements->lreferences element-list)
  (map (lambda (e) (sxml:element->reference e)) element-list))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;;; Get coordinate from point

(define (sxml:archpoint-coord coordinate point)
  (inexact->exact
   (string->number
    (cadr (find (lambda (p) (equal? (car p) coordinate)) point)))))

;;; Get point n from point list

(define (sxml:archpoint-n n pseq)
  (cdr (list-ref pseq n)))

;;; Extract the basic list of point coordinates

(define (sxml:archpoint->point point)
  (make-vect2 (sxml:archpoint-coord 'x point)
              (sxml:archpoint-coord 'y point)))

;-------------------------------------------------------------------------------
; Wall
;-------------------------------------------------------------------------------

;;; Get all wall points

(define (sxml:wall-points wall)
  ((sxpath '(pt @)) wall))

;;; Get wall point n

(define (sxml:wall-point-n wall n)
  ((sxpath `((pt ,n) @ *)) wall))

;;; Get first wall point

(define (sxml:wall-first-point wall)
  ((sxpath '((pt 1) @ *)) wall))

;;; Get last wall point

(define (sxml:wall-last-point wall)
  ((sxpath '((pt 2) @ *)) wall))

;;; Convert a wall into a list of points

(define (sxml:wall->pseq wall)
  (map (lambda (p) (sxml:archpoint->point (cdr p))) (sxml:wall-points wall)))

;;; Convert a list of walls into a list of segments

(define (sxml:wall-list->pseq-list walls)
  (map (lambda (w) (sxml:wall->pseq w)) walls))

;;; Convert a point list into a wall

(define (sxml:pseq->wall p-list uuid)
  (let* ((pa (car p-list))
         (pb (cadr p-list)))
    `(wall (@ (uid ,uuid))
           (pt (@ (y ,(inexact->exact (number->string (vect2-y pa))))
                  (x ,(inexact->exact (number->string (vect2-x pa))))))
           (pt (@ (y ,(inexact->exact (number->string (vect2-y pb))))
                  (x ,(inexact->exact (number->string (vect2-x pb)))))))))

;;; Find the element with that specific uid

(define (sxml:find-wall/uid graph uid)
  (aif element (find
                (lambda (e) (equal? uid (sxml:element-uid e)))
                (sxml:walls graph))
       element
       (begin (display "UID: ")(display uid)(newline)
              (error "Element with such UID not found"))))

;-------------------------------------------------------------------------------
; Wall inner elements
;-------------------------------------------------------------------------------

;;; Get windows in wall

(define (sxml:wall-windows wall)
  ((sxpath '(window)) wall))

;;; Get doors in wall

(define (sxml:wall-doors wall)
  ((sxpath '(door)) wall))

;;; Get wall elements' relative points

(define (sxml:wall-element-relative-points label element)
  (inexact->exact
   (string->number
    (car ((sxpath `(@ ,label *text*)) element)))))

;;; Calculate wall element (door, wall...) points a list

(define (sxml:wall-element->pseq element wall)
  (let ((from (sxml:wall-element-relative-points 'from element))
        (to (sxml:wall-element-relative-points 'to element)))
    (if (= (length (sxml:wall-points wall)) 2)
        (let* ((Ax (sxml:archpoint-coord 'x (sxml:wall-point-n wall 1)))
               (Ay (sxml:archpoint-coord 'y (sxml:wall-point-n wall 1)))
               (ABx (- (sxml:archpoint-coord 'x (sxml:wall-point-n wall 2)) Ax))
               (ABy (- (sxml:archpoint-coord 'y (sxml:wall-point-n wall 2)) Ay)))
          (list (make-vect2 (+ Ax (* ABx from)) (+ Ay (* ABy from)))
                (make-vect2 (+ Ax (* ABx to)) (+ Ay (* ABy to)))))
        (error "Error - wall element has more than 2 relative points\n"))))
      ; Else:
        ; 1. Precalcular lista de puntos relativos
        ; 2. Hacer lista de puntos relativos menores que puerta
        ; 3. Dibujar trayectoria de puerta completa de los segmentos menores
        ; 4. Dibujar el porcentaje restante sobre el siguiente segmento

;;; Calculate all wall elements point lists of the same type

(define (sxml:all-wall-element-points->pseq type wall)
  (map (lambda (e) (sxml:wall-element->pseq e wall)) ((sxpath `(,type)) wall)))

;;; Calculate all wall elements point lists of the same type of all walls

(define (sxml:all-wall-element-points-all-walls->pseq type graph)
  (map (lambda (w) (sxml:all-wall-element-points->pseq type w)) (sxml:graph-walls graph)))

;-------------------------------------------------------------------------------
; Room
;-------------------------------------------------------------------------------

;;; Get a wall in the room by index

(define (sxml:room-wall graph room n)
  (sxml:find-wall/uid graph (cadr (list-ref ((sxpath '(wall @ uid)) room) n))))

;;; Get list of wall references in the room

(define (sxml:room-wall-refs room)
  (cddr room))

;;; Get list of walls that belong to a room, fully described
(define (sxml:room-walls graph room)
  (map (lambda (r) (sxml:find-wall/uid graph r)) (make-uid-list room)))

;-------------------------------------------------------------------------------
; Pipes
;-------------------------------------------------------------------------------

;;; Get the pipe's position

(define (sxml:pipe->center-position pipe)
  (sxml:archpoint->point ((sxpath '(@ *)) pipe)))

;;; Get all centers of a pipe list

(define (sxml:pipes-list->center-positions pipes-list)
  (map (lambda (p) (sxml:pipe->center-position p)) pipes-list))

;-------------------------------------------------------------------------------
; Entry
;-------------------------------------------------------------------------------

;;; Get the entry point as a list of points corresponding to the door

(define (sxml:entry->pseq graph entry)
  (let* ((doorNumber (sxml:entry-door-num entry))
         (wall (sxml:find-wall/uid graph (sxml:entry-wall-uid entry)))
         (doors (sxml:wall-doors wall)))
    (if (> doorNumber (length doors))
        (error "entry->pseq entry is assigned door number that doesn't exist in the referenced wall"))
    (sxml:wall-element->pseq
     (list-ref doors doorNumber)
     wall)))

;;; Get the wall uid where the entry is

(define (sxml:entry-wall-uid entry)
  (sxml:element-uid ((sxpath '(*)) entry)))

;;; Get the door number in the wall where the entry is

(define (sxml:entry-door-num entry)
  (inexact->exact
   (string->number (car ((sxpath '(@ doorNumber *text*)) entry)))))

;-------------------------------------------------------------------------------
; Structure
;-------------------------------------------------------------------------------

;;; Get the structural as a list of points

(define (sxml:structural->pseq graph structural)
  (let* ((center (sxml:archpoint->point ((sxpath '(center @ *)) structural)))
         (dimensions ((sxpath '(dim @ *)) structural))
         (a (inexact->exact (string->number (cadadr dimensions))))
         (b (inexact->exact (string->number (cadar dimensions))))
         (a/2 (/ a 2))
         (b/2 (/ b 2)))
    (list
     (make-vect2 (- (vect2-x center) a/2) (- (vect2-y center) b/2))
     (make-vect2 (+ (vect2-x center) a/2) (- (vect2-y center) b/2))
     (make-vect2 (+ (vect2-x center) a/2) (+ (vect2-y center) b/2))
     (make-vect2 (- (vect2-x center) a/2) (+ (vect2-y center) b/2)))))
