;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std string/xml-to-sxml))
(import (std misc/uuid))

(import web/parse/ssax-sxml/sxml-tools/sxpath)

(import geometry)
(import math)
(import utils/misc)
(import visualization)

;-------------------------------------------------------------------------------
; Constants
;-------------------------------------------------------------------------------

(define wall-thickness 12.5)
(define graph-space-size-x maxx)
(define graph-space-size-y maxy)

;-------------------------------------------------------------------------------
; General
;-------------------------------------------------------------------------------

;;; Get everything inside the architecture tag as a list

(define (graph-parts graph)
  ;((sxpath '(*)) graph))
  (if (null-list? graph)
      (error "You sent me a null graph. What should I do with this?")
    (cdr graph)))

;;; Get all walls in the graph

(define (graph-walls graph)
  ((sxpath '(wall)) graph))

;;; Get rooms in the graph

(define (graph-rooms graph)
  ((sxpath '(room)) graph))

;;; Remove element from graph

(define (remove-element graph element)
  (remove
    (lambda (e)
      (equal? e element))
    graph))

;;; Remove element-list from graph

(define (remove-elements graph element-list)
  (remove
    (lambda (e)
      (any (lambda (e-in-element-list)
             (equal? e-in-element-list e))
           element-list))
    graph))

;;; Add element to graph

(define (add-element graph element)
  (append graph `(,element)))

;-------------------------------------------------------------------------------
; Element references and UID
;-------------------------------------------------------------------------------

;;; Get element's uid

(define (element-uid elem)
  (if (null-list? elem)
      (error "element-uid: Element is null")
    (cadar ((sxpath '(@ uid)) elem))))

;;; Find the element with that specific uid

(define (find-element-with-uid graph uid)
  (define (iter elem-list-tail)
    (cond
     ((null-list? elem-list-tail)
      (display "UID: ")(display uid)(newline)
      (error "Wall with such UID not found"))
     ((equal? (element-uid (car elem-list-tail)) uid)
      (car elem-list-tail))
     (else
      (iter (cdr elem-list-tail)))))
  (iter (graph-walls graph))) ; TODO!!!!!!!!!!!!!!!!!!!! GENERALIZE)

;;; Get the element from a reference element (consisting only of its uid)

(define (reference-to-element graph ref)
  (find-element-with-uid graph (element-uid ref)))

;;; Get the element from a reference element (consisting only of its uid)

(define (reference-list-to-elements graph ref-lis)
  (define (iter elem-lis ref-lis)
    (if (null-list? ref-lis)
        elem-lis
      (iter (append elem-lis (list (reference-to-element graph (car ref-lis)))) (cdr ref-lis))))
  (iter '() ref-lis))

;;; Make a reference from an UID

(define (make-ref-from-uid type element-uid)
  `(,type (@ (uid ,element-uid))))

;;; Make a reference from an element

(define (make-ref-from-element element)
  `(,(car element) (@ (uid ,(element-uid element)))))

;;; Make a reference list from an element list

(define (make-refs-from-elements element-list)
  (map
    (lambda (e)
      (make-ref-from-element e))
    element-list))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;;; Get coordinate from point

(define (archpoint-coord coordinate point)
  (define (find-coordinate point)
    (cond
     ((null-list? point)
      (error "You sent me a null point. Seriously, what should I do with this?? Boy, I'm having a bad day thanks to you."))
     ((equal? (caar point) coordinate)
      (string->number (cadar point)))
     (else
      (find-coordinate (cdr point)))))
  (find-coordinate point))

;;; Get point n from point list

(define (archpoint-n n point-list)
  (cdr (list-ref point-list n)))

;;; Make point

(define (make-archpoint p)
  (if (point? p)
      (error "Error making point: argument #1 is not a point")
      (list (list 'y (number->string (cadr p)))
            (list 'x (number->string (car p))))))

;-------------------------------------------------------------------------------
; Point-list conversion
;-------------------------------------------------------------------------------

;;; Extract the basic list of point coordinates

(define (archpoint->point point)
  (make-point (archpoint-coord 'x point)
              (archpoint-coord 'y point)))

;;; Convert a wall into a list of points

(define (wall->point-list wall) ; TODO: cons not append
  (define (iter point-list to-process)
    (if (null-list? to-process)
        point-list
      (iter
        (cons (archpoint->point (cdar to-process)) point-list)
        (cdr to-process))))
    (iter '() (wall-points wall)))

;;; Convert a point list into a wall

(define (point-list->wall p-list uuid)
  (let* ((pa (car p-list))
         (pb (cadr p-list)))
    `(wall (@ (uid ,uuid))
           (pt (@ (y ,(number->string (point-y pa)))
                  (x ,(number->string (point-x pa)))))
           (pt (@ (y ,(number->string (point-y pb)))
                  (x ,(number->string (point-x pb))))))))

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

(define (wall-element->point-list element wall)
  (let ((from (wall-element-relative-points 'from element))
        (to (wall-element-relative-points 'to element)))
    (if (= (length (wall-points wall)) 2)
        (let* ((Ax (archpoint-coord 'x (wall-point-n wall 1)))
               (Ay (archpoint-coord 'y (wall-point-n wall 1)))
               (ABx (- (archpoint-coord 'x (wall-point-n wall 2)) Ax))
               (ABy (- (archpoint-coord 'y (wall-point-n wall 2)) Ay)))
          (list (make-point (+ Ax (* ABx from)) (+ Ay (* ABy from)))
                (make-point (+ Ax (* ABx to)) (+ Ay (* ABy to)))))
        (error "Error - wall element has more than 2 relative points\n"))))
      ; Else:
        ; 1. Precalcular lista de puntos relativos
        ; 2. Hacer lista de puntos relativos menores que puerta
        ; 3. Dibujar trayectoria de puerta completa de los segmentos menores
        ; 4. Dibujar el porcentaje restante sobre el siguiente segmento

;;; Calculate all wall elements point lists of the same type

(define (all-wall-element-points->point-list type wall)
  (map
    (lambda (e)
      (wall-element->point-list e wall))
  ((sxpath `(,type)) wall)))

;;; Calculate all wall elements point lists of the same type of all walls

(define (all-wall-element-points-all-walls->point-list type graph)
  (define (iter lis walls)
    (cond
     ((null? walls)
      lis)
     (else
      (iter (append lis (all-wall-element-points->point-list type (car walls))) (cdr walls)))))
  (iter '() (graph-walls graph)))

;-------------------------------------------------------------------------------
; Room
;-------------------------------------------------------------------------------

;;; Get a wall in the room by index

(define (room-wall graph room n)
  (find-element-with-uid graph (cadr (list-ref ((sxpath '(wall @ uid)) room) n))))

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
                  (find-element-with-uid graph (car uid-lis))))
        (cdr uid-lis))))
  (let ((uids (make-uid-list)))
    (collect-walls '() uids)))
