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
(import utils/misc)
(import visualization)

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

;; Get graph limits (extreme points)
;;
(define (graph-limit-x graph)
  500.0) ; TODO: calculate
(define (graph-limit-y graph)
  500.0) ; TODO: calculate

;; Remove element from graph
;;
(define (remove-element graph element)
  (remove
    (lambda (e)
      (equal? e element))
    graph))

;; Remove element-list from graph
;;
(define (remove-elements graph element-list)
  (remove
    (lambda (e)
      (any (lambda (e-in-element-list)
             (equal? e-in-element-list e))
           element-list))
    graph))

;; Add element to graph
;;
(define (add-element graph element)
  (append graph `(,element)))

;-------------------------------------------------------------------------------
; Element references and UID
;-------------------------------------------------------------------------------

;; Get element's uid
;;
(define (element-uid elem)
  (if (null-list? elem)
      (raise "element-uid: Element is null")
    (cadar ((sxpath '(@ uid)) elem))))

;; Find the element with that specific uid
;;
(define (find-element-with-uid graph uid)
  (define (iter elem-list-tail)
    (cond
     ((null-list? elem-list-tail)
      (display "UID: ")(display uid)(newline)
      (raise "Wall with such UID not found"))
     ((equal? (element-uid (car elem-list-tail)) uid)
      (car elem-list-tail))
     (else
      (iter (cdr elem-list-tail)))))
  (iter (walls-in-graph graph))) ; TODO!!!!!!!!!!!!!!!!!!!! GENERALIZE)

;; Get the element from a reference element (consisting only of its uid)
;;
(define (reference-to-element graph ref)
  (find-element-with-uid graph (element-uid ref)))

;; Get the element from a reference element (consisting only of its uid)
;;
(define (reference-list-to-elements graph ref-lis)
  (define (iter elem-lis ref-lis)
    (if (null-list? ref-lis)
        elem-lis
      (iter (append elem-lis (list (reference-to-element graph (car ref-lis)))) (cdr ref-lis))))
  (iter '() ref-lis))

;; Make a reference from an UID
;;
(define (make-ref-from-uid type element-uid)
  `(,type (@ (uid ,element-uid))))

;; Make a reference from an element
;;
(define (make-ref-from-element element)
  `(,(car element) (@ (uid ,(element-uid element)))))

;; Make a reference list from an element list
;;
(define (make-refs-from-elements element-list)
  (map
    (lambda (e)
      (make-ref-from-element e))
    element-list))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Get coordinate from point
;;
(define (archpoint-coord coordinate point)
  (define (find-coordinate point)
    (cond
     ((null-list? point)
      (raise "You sent me a null point. Seriously, what should I do with this?? Boy, I'm having a bad day thanks to you."))
     ((equal? (caar point) coordinate)
      (string->number (cadar point)))
     (else
      (find-coordinate (cdr point)))))
  (find-coordinate point))

;; Get point n from point list
;;
(define (archpoint-n n point-list)
  (cdr (list-ref point-list n)))

;; Make point
;;
(define (make-archpoint p)
  (if (point? p)
      (raise "Error making point: argument #1 is not a point")
      (list (list 'y (number->string (cadr p)))
            (list 'x (number->string (car p))))))

;-------------------------------------------------------------------------------
; Point-list extraction
;-------------------------------------------------------------------------------

;; Extract the basic list of point coordinates
;;
(define (archpoint->point point)
  (make-point (archpoint-coord 'x point)
              (archpoint-coord 'y point)))

;; Extract wall points as a list
;;
(define (wall->point-list wall) ; TODO: cons not append
  (define (iter point-list to-process)
    (if (null-list? to-process)
        point-list
      (iter
        (append point-list (list (archpoint->point (cdar to-process))))
        (cdr to-process))))
    (iter '() (wall-points wall)))

;; Extract poly wall points as a list
;;
(define (polywall->point-list wall-lis)
  (define (iter pt-lis lis)
    (if (null-list? lis)
        pt-lis
      (iter (append pt-lis (wall->point-list (car lis))) (cdr lis))))
  (iter '() wall-lis))

;; Calculate the points that enclose a room polygon as a list
;;
(define (extract-room-points graph room)
  (define (get-next-points a-wall b-wall)
    (let ((a-wall-points (wall->point-list a-wall))
          (b-wall-points (wall->point-list b-wall)))
      (cond
       ((is-end-point? b-wall-points (car a-wall-points)) ; then the a-wall is reversed
        (reverse a-wall-points))
       ((is-end-point? b-wall-points (cadr a-wall-points)) ; then the a-wall is reversed
        a-wall-points)
       (else ; If neither the first or the last point of the wall 
         (display "Wall A:\n")
         (display (element-uid a-wall))(newline)
         (display a-wall-points)(newline)
         (display "Wall B:\n")
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

;-------------------------------------------------------------------------------
; Point-list conversion
;-------------------------------------------------------------------------------

;; Convert a point list into a wall
;;
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

;; Get all wall points
;;
(define (wall-points wall)
  ((sxpath '(pt @)) wall))

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
  (> (distance-point-point (wall->point-list point) (wall->point-list (wall-first-point wall)))
     (distance-point-point (wall->point-list point) (wall->point-list (wall-last-point wall)))))

;; Create 2 walls splitting one in a point
;;
(define (create-splitted-wall wall split-point-relative uuid1 uuid2)
  (let ((split-point (point-from-relative-in-wall wall split-point-relative))
        (first-point (wall-first-point wall))
        (second-point (wall-last-point wall)))
  `((wall (@ (uid ,uuid1))
         (pt (@ (y ,(number->string (archpoint-coord 'y first-point)))
                (x ,(number->string (archpoint-coord 'x first-point)))))
         (pt (@ (y ,(number->string (point-y split-point)))
                (x ,(number->string (point-x split-point))))))
   (wall (@ (uid ,uuid2))
         (pt (@ (y ,(number->string (point-y split-point)))
                (x ,(number->string (point-x split-point)))))
         (pt (@ (y ,(number->string (archpoint-coord 'y second-point)))
                (x ,(number->string (archpoint-coord 'x second-point)))))))))

;; Try to merge into one wall if the two given are parallel
;;
(define (try-to-merge-if-parallel-walls wall-list new-uid)
  (let ((wall-a-points (wall->point-list (car wall-list))) ; TODO: try to generalize
        (wall-b-points (wall->point-list (cadr wall-list))))
    (if (parallel? wall-a-points wall-b-points)
        (let ((first-point (if (is-end-point? wall-b-points (car wall-a-points))
                               (cadr wall-a-points)
                             (car wall-a-points)))
              (second-point (if (is-end-point? wall-a-points (car wall-b-points))
                                (cadr wall-b-points)
                              (car wall-b-points))))
          (list (point-list->wall
                (list first-point second-point)
                new-uid)))
        wall-list)))

;; Get all walls in the graph
;;
(define (walls-in-graph graph)
  ((sxpath '(wall)) graph))

;; Calculate point given wall and percentage
;;
(define (point-from-relative-in-wall wall percentage)
  (point-from-relative-in-segment
    (list
      (archpoint->point (wall-point-n wall 1))
      (archpoint->point (wall-point-n wall 2)))
    percentage))

;; Find walls connected to a given one
;;
(define (find-walls-connected-to graph uid)
  (let ((wall (find-element-with-uid graph uid)))
    (define (find-walls-with-point point)
      (define (iter wall-list connected-walls)
        (if (null-list? wall-list)
            connected-walls
          (iter
            (cdr wall-list)
            (if (is-end-point? (wall->point-list (car wall-list)) point)
                (append connected-walls (list (car wall-list)))
              connected-walls))))
      (iter (walls-in-graph graph) '()))
    (list
      (remove (lambda (elem)
                (equal? elem wall))
              (find-walls-with-point (archpoint->point (wall-first-point wall))))
      (remove (lambda (elem)
                (equal? elem wall))
              (find-walls-with-point (archpoint->point (wall-last-point wall)))))))
 
;; Are these walls connected?
;;
(define (walls-are-connected? wall1 wall2)
  (segments-are-connected?
    (wall->point-list wall1)
    (wall->point-list wall2)))

;-------------------------------------------------------------------------------
; Wall inner elements
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
        (let* ((Ax (archpoint-coord 'x (wall-point-n wall 1)))
               (Ay (archpoint-coord 'y (wall-point-n wall 1)))
               (ABx (- (archpoint-coord 'x (wall-point-n wall 2)) Ax))
               (ABy (- (archpoint-coord 'y (wall-point-n wall 2)) Ay)))
          (list (make-point (+ Ax (* ABx from)) (+ Ay (* ABy from)))
                (make-point (+ Ax (* ABx to)) (+ Ay (* ABy to)))))
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
(define (rooms-in-graph graph)
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

;; Break in two lists from where a wall was found
;; Warning! This assumes that rooms contain topologically connected walls
;;
(define (room-break graph room first-wall-uid second-wall-uid)
  ; TODO: check if walls are ordered
  (break (lambda (wall) (equal? second-wall-uid (element-uid wall)))
         (rotate-until-first
           (lambda (wall) (equal? first-wall-uid (element-uid wall)))
           (room-wall-refs room))))

;; Find common wall
;;
(define (room-find-common-wall rooms)
  (let ((walls-room-a (room-wall-refs (car rooms)))
        (walls-room-b (room-wall-refs (cadr rooms))))
    (define (iter lis1)
      (let ((first (car lis1)))
        (if (null-list? lis1)
            (raise "find-common-wall: No common wall found")
          (if (any (lambda (elem) (equal? elem first)) walls-room-b)
              (element-uid first)
            (iter (cdr lis1))))))
    (iter walls-room-b)))

;; Sort walls in a room, so they are connected
;;
(define (room-sort-walls graph room) ; TODO: check if the last and the first are really connected
  (let ((walls (room-wall-refs room)))
    (define (iter sorted remaining)
      (define (find-next first wall-list) ; (it sorts backwards)
        (cond
         ((null-list? wall-list)
          (display first)(newline)
          (raise "room-sort-walls: This wall cannot be connected to any other one"))
         ((walls-are-connected? (reference-to-element graph first) (reference-to-element graph (car wall-list)))
          (car wall-list))
         (else
          (find-next first (cdr wall-list)))))
      (if (null-list? remaining)
          sorted
        (let ((next (find-next (car sorted) remaining)))
          (iter (cons next sorted) (remove (lambda (e) (equal? e next)) remaining))))) ; (it sorts backwards)
    `(,(append `(room (@ (uid ,(element-uid room))))
                         (iter (list (car walls)) (cdr walls))))))

;; Calculate room area
;;
(define (room-area room)
  ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
  99.9) ; TODO

;-------------------------------------------------------------------------------
; Visualization
;-------------------------------------------------------------------------------

;; Draw graph
;;
(define (visualize-graph graph)
  (visualize-when-possible
    'graph
    (lambda (backend)
      ;; Paint wall
      (define (paint-wall wall)
        (paint-set-color backend 0.1 0.1 0.1 1.0)
        (paint-set-line-cap backend 'square)
        (paint-set-line-width backend 5.0)
        (paint-path backend (wall->point-list wall)))
      ;; Paint doors in the wall
      (define (paint-doors-in-wall wall)
        (for-each
          (lambda
            (door)
            (paint-set-line-cap backend 'butt)
            (paint-set-color backend 1.0 1.0 1.0 1.0)
            (paint-set-line-width backend 6.0)
            (paint-path backend (extract-wall-element-points door wall))
            (paint-set-color backend 1.0 0.1 0.1 1.0)
            (paint-set-line-width backend 3.0)
            (paint-path backend (extract-wall-element-points door wall)))
          (wall-doors wall)))
      ;; Paint windows in the wall
      (define (paint-windows-in-wall wall)
        (paint-set-color backend 1.0 1.0 0.1 1.0)
        (paint-set-line-cap backend 'butt)
        (paint-set-line-width backend 3.0)
        (for-each
          (lambda
            (window)
            (paint-path backend (extract-wall-element-points window wall)))
          (wall-windows wall)))
      ;; Paint pilar
      (define (paint-pilar pilar)
        '())
      ;; Paint room
      (define (paint-room graph room)
        ;(paint-set-color backend (random-real) (random-real) (random-real) 0.5)
        (paint-set-color backend 0.0 0.0 0.3 0.3)
        (paint-polygon backend (extract-room-points graph room)))
      ;; Paint entry
      (define (paint-entry wall)
        '())
      ;; Paint pipe
      (define (paint-pipe wall)
        '())

      (for-each
        (lambda
          (elem)
          (if (null-list? elem)
              (raise "Malformed SXML")
            (cond
              ((equal? (car elem) 'wall)
               (paint-wall
                 elem)
               (paint-windows-in-wall 
                 elem)
               (paint-doors-in-wall 
                 elem))
              ((equal? (car elem) 'pilar)
               (paint-pilar elem))
              ((equal? (car elem) 'room)
               (paint-room graph elem))
              ((equal? (car elem) 'entry)
               ;(paint-entry (make-wall-list-from-uids (make-uid-list elem) graph)))
               '())
              ((equal? (car elem) 'pipe)
               ;(paint-pipe (make-wall-list-from-uids (make-uid-list elem) graph))))))
               '()))))
        (graph-parts graph))))
  (visualization:layer-depth-set! 'graph 5))
