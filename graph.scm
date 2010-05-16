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
(import math/algebra)

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

(define (graph-type graph)
  (car graph))

;;; Is this a graph

(define (graph? graph)
  (equal? (graph-type graph) 'architecture))

;;; Get everything inside the graph as-is

(define (graph-contents graph)
  ;((sxpath '(*)) graph))
  (if (null-list? graph)
      (error "You sent me a null graph. What should I do with this?")
    (cddr graph)))

;;; Get all parts of a graph that are of a specific type

(define (graph-parts graph type)
  ((sxpath `(,@type)) graph))

;;; Get all parts of a graph

(define (graph-all-parts graph)
  ((sxpath '(*)) graph))

;;; Get all walls in the graph

(define (graph-walls graph)
  ((sxpath '(wall)) graph))

;;; Get rooms in the graph

(define (graph-rooms graph)
  ((sxpath '(room)) graph))

;;; Get the pipes in the graph

(define (graph-pipes graph)
  ((sxpath '(pipe)) graph))

;;; Get the entrances

(define (graph-entries graph)
  ((sxpath '(entry)) graph))

;;; Get all the structural elements in the graph

(define (graph-structurals graph)
  ((sxpath '(structural)) graph))

;;; Get the north direction

(define (graph-north graph)
  (vect2:normalize (make-vect2 1.0 1.0)))

;-------------------------------------------------------------------------------
; Element references and UID
;-------------------------------------------------------------------------------

;;; Get element's uid

(define (element-uid elem)
  (if (null? elem)
      (error "element-uid: Element is null")
    (cadar ((sxpath '(@ uid)) elem))))

;;; Find the element with that specific uid

(define (find-element/uid graph uid)
  (aif element (find
                 (lambda (e) (equal? uid (element-uid e)))
                 (graph-walls graph)) ; TODO: generalize!!
    element
    (begin (display "UID: ")(display uid)(newline)
           (error "Element with such UID not found"))))

;;; Get the element from a reference element (consisting only of its uid)

(define (reference->element graph ref)
  (find-element/uid graph (element-uid ref)))

;;; Get the elements from a reference list

(define (lreferences->lelements graph ref-lis)
  (define (iter elem-lis ref-lis)
    (if (null? ref-lis)
        elem-lis
      (iter (append elem-lis (list (reference->element graph (car ref-lis)))) (cdr ref-lis))))
  (iter '() ref-lis)) ; TODO: cons! no append!

;;; Make a reference from an UID

(define (uid->reference type element-uid)
  `(,type (@ (uid ,element-uid))))

;;; Make a reference from an element

(define (element->reference element)
  `(,(car element) (@ (uid ,(element-uid element)))))

;;; Make a reference list from an element list

(define (lelements->lreferences element-list)
  (map
    (lambda (e)
      (element->reference e))
    element-list))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;;; Get coordinate from point

(define (archpoint-coord coordinate point)
  (define (find-coordinate point)
    (cond
     ((null? point)
      (error "You sent me a null point"))
     ((equal? (caar point) coordinate)
      (string->number (cadar point)))
     (else
      (find-coordinate (cdr point)))))
  (find-coordinate point))

;;; Get point n from point list

(define (archpoint-n n pseq)
  (cdr (list-ref pseq n)))

;;; Extract the basic list of point coordinates

(define (archpoint->point point)
  (make-vect2 (archpoint-coord 'x point)
              (archpoint-coord 'y point)))

;-------------------------------------------------------------------------------
; Wall
;-------------------------------------------------------------------------------

;;; Get all wall points

(define (wall-points wall)
  ((sxpath '(pt @)) wall))

;;; Get wall point n

(define (wall-point-n wall n)
  ((sxpath `((pt ,n) @ *)) wall))

;;; Get first wall point

(define (wall-first-point wall)
  ((sxpath '((pt 1) @ *)) wall))

;;; Get last wall point

(define (wall-last-point wall)
  ((sxpath '((pt 2) @ *)) wall))

;;; Convert a wall into a list of points

(define (wall->pseq wall)
  (define (iter pseq to-process)
    (if (null? to-process)
        pseq
      (iter
        (cons (archpoint->point (cdar to-process)) pseq)
        (cdr to-process))))
  (iter '() (wall-points wall)))

;;; Convert a list of walls into a list of segments

(define (wall-list->pseq-list walls)
  (map
    (lambda (w)
      (wall->pseq w))
    walls))

;;; Convert a point list into a wall

(define (pseq->wall p-list uuid)
  (let* ((pa (car p-list))
         (pb (cadr p-list)))
    `(wall (@ (uid ,uuid))
           (pt (@ (y ,(number->string (vect2-y pa)))
                  (x ,(number->string (vect2-x pa)))))
           (pt (@ (y ,(number->string (vect2-y pb)))
                  (x ,(number->string (vect2-x pb))))))))

;-------------------------------------------------------------------------------
; Wall inner elements
;-------------------------------------------------------------------------------

;;; Get windows in wall

(define (wall-windows wall)
  ((sxpath '(window)) wall))

;;; Get doors in wall

(define (wall-doors wall)
  ((sxpath '(door)) wall))

;;; Get wall elements' relative points

(define (wall-element-relative-points label element)
  (string->number (car ((sxpath `(@ ,label *text*)) element))))

;;; Calculate wall element (door, wall...) points a list

(define (wall-element->pseq element wall)
  (let ((from (wall-element-relative-points 'from element))
        (to (wall-element-relative-points 'to element)))
    (if (= (length (wall-points wall)) 2)
        (let* ((Ax (archpoint-coord 'x (wall-point-n wall 1)))
               (Ay (archpoint-coord 'y (wall-point-n wall 1)))
               (ABx (- (archpoint-coord 'x (wall-point-n wall 2)) Ax))
               (ABy (- (archpoint-coord 'y (wall-point-n wall 2)) Ay)))
          (list (make-vect2 (+ Ax (* ABx from)) (+ Ay (* ABy from)))
                (make-vect2 (+ Ax (* ABx to)) (+ Ay (* ABy to)))))
        (error "Error - wall element has more than 2 relative points\n"))))
      ; Else:
        ; 1. Precalcular lista de puntos relativos
        ; 2. Hacer lista de puntos relativos menores que puerta
        ; 3. Dibujar trayectoria de puerta completa de los segmentos menores
        ; 4. Dibujar el porcentaje restante sobre el siguiente segmento

;;; Calculate all wall elements point lists of the same type

(define (all-wall-element-points->pseq type wall)
  (map
    (lambda (e)
      (wall-element->pseq e wall))
  ((sxpath `(,type)) wall)))

;;; Calculate all wall elements point lists of the same type of all walls

(define (all-wall-element-points-all-walls->pseq type graph)
  (define (iter lis walls)
    (cond
     ((null? walls)
      lis)
     (else
      (iter (append lis (all-wall-element-points->pseq type (car walls))) (cdr walls)))))
  (iter '() (graph-walls graph)))

;-------------------------------------------------------------------------------
; Room
;-------------------------------------------------------------------------------

;;; Get a wall in the room by index

(define (room-wall graph room n)
  (find-element/uid graph (cadr (list-ref ((sxpath '(wall @ uid)) room) n))))

;;; Get list of wall references in the room

(define (room-wall-refs room)
  (cddr room))

;;; Get list of walls that belong to a room, fully described

(define (room-walls graph room)
  (define (make-uid-list)
    ((sxpath '(wall @ uid *text*)) room))
  (define (collect-walls wall-lis uid-lis)
    (if (null-list? uid-lis)
        wall-lis
      (collect-walls
        (append wall-lis
                (list
                  (find-element/uid graph (car uid-lis))))
        (cdr uid-lis))))
  (let ((uids (make-uid-list)))
    (collect-walls '() uids)))

;-------------------------------------------------------------------------------
; Pipes
;-------------------------------------------------------------------------------

;;; Get the pipe's position

(define (pipe->center-position pipe)
  (archpoint->point ((sxpath '(@ *)) pipe)))

;;; Get all centers of a pipe list

(define (pipes-list->center-positions pipes-list) ; TODO: Maybe macrolize?
  (map
    (lambda (p)
      (pipe->center-position p))
    pipes-list))

;-------------------------------------------------------------------------------
; Entry
;-------------------------------------------------------------------------------

;;; Get the entry point as a list of points corresponding to the door

(define (entry->pseq graph entry)
  (let* ((doorNumber (string->number (car ((sxpath '(@ doorNumber *text*)) entry))))
         (wall (reference->element graph ((sxpath '(*)) entry)))
         (doors (wall-doors wall)))
    (if (> doorNumber (length doors))
        (error "entry->pseq entry is assigned door number that doesn't exist in the referenced wall"))
    (wall-element->pseq
      (list-ref doors doorNumber)
      wall)))

;-------------------------------------------------------------------------------
; Structure
;-------------------------------------------------------------------------------

;;; Get the structural as a list of points

(define (structural->pseq structural graph)
  (let* ((center (archpoint->point ((sxpath '(center @ *)) structural)))
         (dimensions ((sxpath '(dim @ *)) structural))
         (a (string->number (cadadr dimensions)))
         (b (string->number (cadar dimensions)))
         (a/2 (/ a 2))
         (b/2 (/ b 2)))
    (list
      (make-vect2 (- (vect2-x center) a/2) (- (vect2-y center) b/2))
      (make-vect2 (+ (vect2-x center) a/2) (- (vect2-y center) b/2))
      (make-vect2 (+ (vect2-x center) a/2) (+ (vect2-y center) b/2))
      (make-vect2 (- (vect2-x center) a/2) (+ (vect2-y center) b/2)))))
