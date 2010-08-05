;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1
             string/xml-to-sxml
             misc/uuid))

(import web/parse/ssax-sxml/sxml-tools/sxpath
        core/list
        core/syntax
        core/debugging
        geometry/kernel
        graph
        math/exact-algebra
        visualization)

(%activate-checks)

;-------------------------------------------------------------------------------
; Constants
;-------------------------------------------------------------------------------

(define wall-thickness 0.15)
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
  (%deny (null? graph) "you sent me a null graph. What should I do with this?")
  (cddr graph))

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
  (make-vect2 1 1)) ; TODO!!

;-------------------------------------------------------------------------------
; Element references and UID
;-------------------------------------------------------------------------------

;;; Get element's uid

(define (sxml:element-uid elem)
  (%deny (null? elem) "given element is null")
  (let ((raw ((sxpath '(@ uid)) elem)))
    (%deny (null? raw) "no element uid found")
    (cadar raw)))

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

;;; Make a SXML point

(define (sxml:make-archpoint x y)
  `(pt (@ (x ,(number->string
               (exact->inexact x)))
          (y ,(number->string
               (exact->inexact y))))))

;;; Trasnform a point into an archpoint (a point represented as SXML)

(define (sxml:point->archpoint p)
  `(pt (@ (x ,(number->string
               (exact->inexact (point-x p))))
          (y ,(number->string
               (exact->inexact (point-y p)))))))

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

;;; Make a SXML wall

(define (sxml:make-wall e)
  `(wall
    (@ (uid ,(wall-uid e))
       (type TODO))
    ,@(map (lambda (p)
             (%accept (point? p))
             (sxml:point->archpoint p))
           (wall-pseq e))
    ,@(map (lambda (w)
             (%accept (window? w))
             `(window (@ (from ,(number->string
                                 (exact->inexact (window-from w))))
                         (to ,(number->string
                               (exact->inexact (window-to w)))))))
           (wall-windows e))
    ,@(map (lambda (d)
             (%accept (door? d))
             `(door (@ (from ,(number->string
                               (exact->inexact (door-from d))))
                       (to ,(number->string
                             (exact->inexact (door-to d)))))))
           (wall-doors e))))

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

;;; Get wall metadata

(define (sxml:wall-metadata wall) ; TODO: change XML format for general metadata
  (let ((raw ((sxpath '(@ type)) wall)))
    (if (null? raw)
        raw
        (cadar raw))))

;;; Convert a wall into a list of points

(define (sxml:wall->pseq wall)
  (map (lambda (p) (sxml:archpoint->point (cdr p))) (sxml:wall-points wall)))

;;; Convert a list of walls into a list of segments

(define (sxml:wall-list->pseq-list walls)
  (map (lambda (w) (sxml:wall->pseq w)) walls))

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

;;; Make a SXML room

(define (sxml:make-room e)
  `(room
    (@ (uid ,(room-uid e)))
    ,@(map
       (lambda (w)
         `(wall (@ (uid ,w))))
       (room-walls e))))

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

;;; Make a SXML pipe

(define (sxml:make-pipe e)
  (let ((pos (pipe-position e)))
   `(pipe
     (@ (x ,(point-x pos))
        (y ,(point-y pos))))))

;;; Get the pipe's position

(define (sxml:pipe->center-position pipe)
  (sxml:archpoint->point ((sxpath '(@ *)) pipe)))

;;; Get all centers of a pipe list

(define (sxml:pipes-list->center-positions pipes-list)
  (map (lambda (p) (sxml:pipe->center-position p)) pipes-list))

;-------------------------------------------------------------------------------
; Entry
;-------------------------------------------------------------------------------

;;; Make a SXML entry

(define (sxml:make-entry e)
  `(entry
    (@ (wall-uid ,(entry-wall-uid e))
       (door-number ,(entry-door-number e))
       (pt TODO))))

;;; Get the entry point as a list of points corresponding to the door

(define (sxml:entry->pseq graph entry)
  (let* ((wall (sxml:find-wall/uid graph (sxml:entry-wall-uid entry)))
         (doors (sxml:wall-doors wall)))
    (let ((door-number (sxml:entry-door-num entry))
          (entry-point (sxml:entry-point entry)))
     (if (and (not-null? doors)
              (not-null? door-number) ; first we try with door reference
              (< door-number (length doors)))
         (sxml:wall-element->pseq (list-ref doors door-number)
                                  wall)
         (error "entry door creation with a point remains unimplemented")))))

;;; Get the wall uid where the entry is

(define (sxml:entry-wall-uid entry)
  (cadar ((sxpath '(@ (wall-uid 1))) entry)))

;;; Get the door number in the wall where the entry is

(define (sxml:entry-door-num entry)
  (inexact->exact
   (string->number (cadar ((sxpath '(@ (door-number 1))) entry)))))

;;; Get the door entry point in case where there is not door reference

(define (sxml:entry-point entry)
  (cadar ((sxpath '(@ (pt 1))) entry)))

;-------------------------------------------------------------------------------
; Structure
;-------------------------------------------------------------------------------

;;; Make a SXML structural

(define (sxml:make-structural e)
  `(structural
    (@ (uid ,(structural-uid e)))
    (center
     (@ (x TODO)
        (y TODO)))
    (dim
     (@ (a TODO)
        (b TODO)))))

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

;-------------------------------------------------------------------------------
; Sxml-grap / graph conversion
;-------------------------------------------------------------------------------

;;; SXML to graph conversion

(define (sxml-graph->graph sxmlgraph)
  (make-graph
   (sxml:element-uid sxmlgraph)
   (list (make-direction #e1 #e1))
   (map-cond ((type <- (lambda (el) (sxml:element-type el)))
              (e <- values))
             (((equal? type 'structural)
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
                          (sxml:wall-metadata e)
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
                          (map (lambda (w) (sxml:element-uid w)) (sxml:room-wall-refs e))))
              (else (error "sxml-graph contains an unknown type element")))
             (sxml:contents sxmlgraph))))

;;; Graph to SXML conversion

(define (graph->sxml-graph graph)
  `(architecture
    (@ (uid ,(graph-uid graph)))
    ,(map-cond
      (e)
      (((structural? e) (sxml:make-structural e))
       ((entry? e) (sxml:make-entry e))
       ((pipe? e) (sxml:make-pipe e))
       ((wall? e) (sxml:make-wall e))
       ((room? e) (sxml:make-room e))
       (else (error "graph contains an unknown type of element")))
      (graph-architecture graph))))
