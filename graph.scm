;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std string/xml-to-sxml))
(import (std misc/uuid))
(import web/parse/ssax-sxml/sxml-tools/sxpath)
(import utilities)
(import geometry)

;; Generate graph from XML
;;
(define (generate-graph-from-xml xml-string)
  (let* ((sxml (xml-string->sxml xml-string))
         (architecture
          (car
           ((sxpath '(ensanche floorPlan architecture)) sxml))))
    architecture))

;; Print graph
;;
(define (print-graph sxml)
  (define-macro (pp-code-eval . thunk) ; Pretty print the code as it is evatuated
    `(begin
       ,@(apply
          append  ; should better use `map-union' from "sxpathlib.scm"
          (map
           (lambda (s-expr)
             (cond
               ((string? s-expr)  ; string - just display it
                `((display ,s-expr)
                  (newline)))
               ((and (pair? s-expr) (eq? (car s-expr) 'define))
                ; definition - pp and eval it
                `((pp ',s-expr)
                  ,s-expr))
               ((and (pair? s-expr)
                     (memq (car s-expr) '(newline cond-expand)))
                ; just eval it
                `(,s-expr))
               (else  ; for anything else - pp it and pp result
                `((pp ',s-expr)
                  (display "==>")
                  (newline)
                  (pp ,s-expr)
                  (newline)))))
           thunk))))
  (pp-code-eval sxml))

;-------------------------------------------------------------------------------
; General
;-------------------------------------------------------------------------------

;; Get everything inside the architecture tag as a list
;;
(define (graph-parts graph)
  ;((sxpath '(*)) graph))
  (if (null-list? graph)
      (raise "You sent me a null graph. What should I do with this?")
    (cdr graph)))

;; Get element's uid
;;
(define (element-uid elem)
  (if (null-list? elem)
      (raise "element-uid: Element is null")
    (cadar ((sxpath '(@ uid)) elem))))

;; Get the element from a referene element (consisting only of its uid)
;;
(define (reference-to-element graph ref)
  (list (find-element-with-uid graph (element-uid ref))))

;; Get the element from a referene element (consisting only of its uid)
;;
(define (reference-list-to-elements graph ref-lis)
  (define (iter elem-lis ref-lis)
    (if (null-list? ref-lis)
        elem-lis
      (iter (append elem-lis (reference-to-element graph (car ref-lis))) (cdr ref-lis))))
  (iter '() ref-lis))

;; Find the element with that specific uid
;;
(define (find-element-with-uid graph uid)
  (define (iter elem-list-tail)
    (cond
     ((null-list? elem-list-tail)
      (raise "Wall with such UID not found"))
     ((equal? (element-uid (car elem-list-tail)) uid)
      (car elem-list-tail))
     (else
      (iter (cdr elem-list-tail)))))
  (iter (walls graph))) ; TODO!!!!!!!!!!!!!!!!!!!! GENERALIZE

;-------------------------------------------------------------------------------
; Wall
;-------------------------------------------------------------------------------

;; Get all wall points
;;
(define (wall-points wall)
  ((sxpath '(pt @)) wall))

;; Extract wall points as a list
;;
(define (extract-wall-points wall)
  (define (iter point-list to-process)
    (if (null-list? to-process)
        point-list
      (iter
        (append point-list (list (extract-point-coords (cdar to-process))))
        (cdr to-process))))
    (iter '() (wall-points wall)))

;; Extract poly wall points as a list
;;
(define (extract-polywall-points wall-lis)
  (define (iter pt-lis lis)
    (if (null-list? lis)
        pt-lis
      (iter (append pt-lis (extract-wall-points (car lis))) (cdr lis))))
  (iter '() wall-lis))

;; Get wall point n
;;
(define (wall-point-n wall n)
  ((sxpath `((pt ,n) @ *)) wall))

;; Get first wall point
;;
(define (wall-first-point wall)
  ((sxpath '((pt 1) @ *)) wall))

;; Get last wall point
;;
(define (wall-last-point wall)
  ((sxpath '((pt 2) @ *)) wall))

;; Is the wall described in a reverse order from a given reference?
;;
(define (wall-is-reversed? wall point)
  (> (distance-point-point point (wall-first-point wall))
     (distance-point-point point (wall-last-point wall))))

;; Create a wall
;;
(define (create-wall point-a point-b uuid)
  `(wall (@ (uid ,uuid))
         (pt (@ (y ,(number->string (point-coord 'y point-a)))
                (x ,(number->string (point-coord 'x point-a)))))
         (pt (@ (y ,(number->string (point-coord 'y point-b)))
                (x ,(number->string (point-coord 'x point-b)))))))

;; Create 2 walls splitting one in a point
;;
(define (create-splitted-wall wall split-point-relative uuid1 uuid2)
  (let ((split-point (point-from-relative-in-wall wall split-point-relative))
        (first-point (wall-first-point wall))
        (second-point (wall-last-point wall)))
  `((wall (@ (uid ,uuid1))
         (pt (@ (y ,(number->string (point-coord 'y first-point)))
                (x ,(number->string (point-coord 'x first-point)))))
         (pt (@ (y ,(number->string (point-coord 'y split-point)))
                (x ,(number->string (point-coord 'x split-point))))))
   (wall (@ (uid ,uuid2))
         (pt (@ (y ,(number->string (point-coord 'y split-point)))
                (x ,(number->string (point-coord 'x split-point)))))
         (pt (@ (y ,(number->string (point-coord 'y second-point)))
                (x ,(number->string (point-coord 'x second-point)))))))))

;; Create 2 walls splitting one in a point (inverting first and second)
;;
(define (create-splitted-wall-inverted wall split-point-relative uuid1 uuid2)
  (let ((split-point (point-from-relative-in-wall wall split-point-relative))
        (first-point (wall-first-point wall))
        (second-point (wall-last-point wall)))
  `((wall (@ (uid ,uuid1))
         (pt (@ (y ,(number->string (point-coord 'y second-point)))
                (x ,(number->string (point-coord 'x second-point)))))
         (pt (@ (y ,(number->string (point-coord 'y split-point)))
                (x ,(number->string (point-coord 'x split-point))))))
   (wall (@ (uid ,uuid2))
         (pt (@ (y ,(number->string (point-coord 'y split-point)))
                (x ,(number->string (point-coord 'x split-point)))))
         (pt (@ (y ,(number->string (point-coord 'y first-point)))
                (x ,(number->string (point-coord 'x first-point)))))))))

;; Get all walls in the graph
;;
(define (walls graph)
  ((sxpath '(wall)) graph))

;; Calculate point given wall and percentage
;;
(define (point-from-relative-in-wall wall percentage)
  (point-from-relative-in-segment
   (wall-point-n wall 1)
   (wall-point-n wall 2)
   percentage))
 
;; Make list of walls from uids
;;
(define (make-wall-list-from-uids uids graph)
  '()) ; TODO

;-------------------------------------------------------------------------------
; Wall elements
;-------------------------------------------------------------------------------

;; Get windows in wall
;;
(define (wall-windows wall)
  ((sxpath '(window)) wall))

;; Get doors in wall
;;
(define (wall-doors wall)
  ((sxpath '(door)) wall))

;; Get wall elements' relative points
;;
(define (wall-element-relative-points label element)
  (string->number (car ((sxpath `(@ ,label *text*)) element))))

;; Calculate wall element (door, wall...) points a list
;;
(define (extract-wall-element-points element wall)
  (let ((from (wall-element-relative-points 'from element))
        (to (wall-element-relative-points 'to element)))
    (if (= (length (wall-points wall)) 2)
        (let* ((Ax (point-coord 'x (wall-point-n wall 1)))
               (Ay (point-coord 'y (wall-point-n wall 1)))
               (ABx (- (point-coord 'x (wall-point-n wall 2)) Ax))
               (ABy (- (point-coord 'y (wall-point-n wall 2)) Ay)))
          (list `(,(+ Ax (* ABx from)) ,(+ Ay (* ABy from)))
                `(,(+ Ax (* ABx to)) ,(+ Ay (* ABy to)))))
        (raise "Error - wall element has more than 2 relative points\n"))))
      ; Else:
        ; 1. Precalcular lista de puntos relativos
        ; 2. Hacer lista de puntos relativos menores que puerta
        ; 3. Dibujar trayectoria de puerta completa de los segmentos menores
        ; 4. Dibujar el porcentaje restante sobre el siguiente segmento

;-------------------------------------------------------------------------------
; Room
;-------------------------------------------------------------------------------

;; Get rooms in the graph
;;
(define (rooms graph)
  ((sxpath '(room)) graph))

;; Get a wall in the room by index
;;
(define (room-wall graph room n)
  (find-element-with-uid graph (cadr (list-ref ((sxpath '(wall @ uid)) room) n))))

;; Get list of wall references in the room
;;
(define (room-wall-refs room)
  (cddr room))

;; Get list of walls that belong to a room, fully described
;;
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

;; Calculate the points that enclose a room polygon as a list
;;
(define (extract-room-points graph room)
  (define (get-next-points a-wall b-wall)
    (let ((a-wall-points (extract-wall-points a-wall))
          (b-wall-points (extract-wall-points b-wall)))
      (cond
       ((is-end-point? b-wall-points (car a-wall-points)) ; then the a-wall is reversed
        (reverse a-wall-points))
       ((is-end-point? b-wall-points (cadr a-wall-points)) ; then the a-wall is reversed
        a-wall-points)
       (else ; If neither the first or the last point of the wall 
         (display (element-uid a-wall))(newline)
         (display a-wall-points)(newline)
         (display (element-uid b-wall))(newline)
         (display b-wall-points)(newline)
         (raise "extract-room-points: Room must be a closed polygon. TODO: Polyline walls")))))
  (define (iter point-list walls)
    (if (< (length walls) 2)
        point-list
      (iter
        (append point-list (get-next-points (car walls) (cadr walls)))
        (cdr walls))))
  (let* ((walls (room-walls graph room)))
    (iter '() walls)))

;; Break in two lists from where a wall was found
;; Warning! This assumes that rooms contain topologically connected walls
;;
(define (room-break graph room first-wall-uid second-wall-uid)
  ; TODO: check if walls are ordered
  (break (lambda (wall) (equal? second-wall-uid (element-uid wall)))
         (rotate-until-first
           (lambda (wall) (equal? first-wall-uid (element-uid wall)))
           (room-wall-refs room))))

;; Calculate room area
;;
(define (room-area room)
  ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
  99.9) ; TODO
