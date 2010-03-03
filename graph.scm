;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std srfi/1))
(import (std string/xml-to-sxml))
(import (std misc/uuid))
(import web/parse/ssax-sxml/sxml-tools/sxpath)
(import utilities)

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

;; Are the point lists equal? (with precision)
;;
(define (equal-point-lists? lis1 lis2 precision)
  (every
    (lambda (a b)
      (every (lambda (e) (< e precision)) (map abs (map - a b))))
    lis1
    lis2))

;-------------------------------------------------------------------------------
; Points
;-------------------------------------------------------------------------------

;; Get coordinate from point
;;
(define (point-coord coordinate point)
  (define (find-coordinate point)
    (cond
     ((null-list? point)
      (raise "You sent me a null point. Seriously, what should I do with this?? Boy, I'm having a bad day thanks to you."))
     ((equal? (caar point) coordinate)
      (string->number (cadar point)))
     (else
      (find-coordinate (cdr point)))))
  (find-coordinate point))

;; Extract a list of point coordinates
;;
(define (extract-point-coords point)
  `((,(point-coord 'x point)
     ,(point-coord 'y point))))

;; Get point n from point list
;;
(define (point-n n point-list)
  (cdr (list-ref point-list n)))

;; Make point
;;
(define (make-point x y)
  (if (or (null? x) (null? y))
      (raise "Error making point: null arguments")
      (list (list 'y (number->string y))
            (list 'x (number->string x)))))

;; Calculate absolute point given segment and percentage
;;
(define (point-from-relative-in-segment point-a point-b percentage)
  (if (or (null-list? point-a) (null-list? point-b))
    (raise "Wrong points passed")
    (let* ((Ax (point-coord 'x point-a))
           (Ay (point-coord 'y point-a))
           (ABx (- (point-coord 'x point-b) Ax))
           (ABy (- (point-coord 'y point-b) Ay)))
      (make-point
        (+ Ax (* ABx percentage))
        (+ Ay (* ABy percentage))))))

;-------------------------------------------------------------------------------
; Wall
;-------------------------------------------------------------------------------

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
(define (create-splitted-wall wall split-point uuid1 uuid2)
  `((wall (@ (uid ,uuid1))
         (pt (@ (y ,(number->string (point-coord 'y (wall-point-n 1 wall))))
                (x ,(number->string (point-coord 'x (wall-point-n 1 wall))))))
         (pt (@ (y ,(number->string (point-coord 'y (point-from-relative-in-wall wall split-point))))
                (x ,(number->string (point-coord 'x (point-from-relative-in-wall wall split-point)))))))
   (wall (@ (uid ,uuid2))
         (pt (@ (y ,(number->string (point-coord 'y (point-from-relative-in-wall wall split-point))))
                (x ,(number->string (point-coord 'x (point-from-relative-in-wall wall split-point))))))
         (pt (@ (y ,(number->string (point-coord 'y (wall-point-n 2 wall))))
                (x ,(number->string (point-coord 'x (wall-point-n 2 wall)))))))))

;; Get all walls in the graph
;;
(define (walls graph)
  ((sxpath '(wall)) graph))

;; Get wall's uid
;;
(define (wall-uid wall)
  (if (null-list? wall)
      (raise "wall-uid: Wall is null")
    (cadar ((sxpath '(@ uid)) wall))))

;; Calculate point given wall and percentage
;;
(define (point-from-relative-in-wall wall percentage)
  (point-from-relative-in-segment
   (wall-point-n 1 wall)
   (wall-point-n 2 wall)
   percentage))
 
;; Make list of walls from uids
;;
(define (make-wall-list-from-uids uids graph)
  '()) ; TODO

;; Find the wall with that specific uid
;;
(define (find-wall-with-uid graph uid)
  (define (iter wall-list-tail)
    (cond
     ((null-list? wall-list-tail)
      (raise "Wall with such UID not found"))
     ((equal? (wall-uid (car wall-list-tail)) uid)
      (car wall-list-tail))
     (else
      (iter (cdr wall-list-tail)))))
  (iter (walls graph)))

;-------------------------------------------------------------------------------
; Wall elements
;-------------------------------------------------------------------------------

;; Get all wall points
;;
(define (wall-points wall)
  ((sxpath '(pt @)) wall))

;; Get wall point n
;;
(define (wall-point-n n wall)
  ((sxpath `((pt ,n) @ *)) wall))

;; Get first wall point
;;
(define (wall-first-point wall)
  ((sxpath '((pt 1) @ *)) wall))

;; Get last wall point
;;
(define (wall-last-point wall)
  ((sxpath '((pt 2) @ *)) wall))

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
        (let* ((Ax (point-coord 'x (wall-point-n 1 wall)))
               (Ay (point-coord 'y (wall-point-n 1 wall)))
               (ABx (- (point-coord 'x (wall-point-n 2 wall)) Ax))
               (ABy (- (point-coord 'y (wall-point-n 2 wall)) Ay)))
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
  (find-wall-with-uid graph (cadr (list-ref ((sxpath '(wall @ uid)) room) n))))

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
      (collect-walls (append wall-lis
                             (list
                               (find-wall-with-uid graph (car uid-lis))))
                     (cdr uid-lis))))
  (let ((uids (make-uid-list)))
    (collect-walls '() uids)))

;; Calculate the points that enclose a room polygon as a list
;;
(define (extract-room-points graph room)
  (define (get-next-points wall last-point-coords)
    (let ((p1 (extract-point-coords (wall-first-point wall)))
          (p2 (extract-point-coords (wall-last-point wall))))
      (cond
       ((equal-point-lists? p1 last-point-coords 0.0001)
        ;`((,(point-coord 'x p2)
           ;,(point-coord 'y p2))))
           p2)
       ((equal-point-lists? p2 last-point-coords 0.0001)
        ; `((,(point-coord 'x p1)
           ; ,(point-coord 'y p1))))
           p1)
       (else ; If neither the first or the last point of the wall 
         (display "MIERDA\n")
         (display last-point-coords)(newline)
         (display p1)(newline)
         (display (equal-point-lists? p1 last-point-coords 0.001))(newline)
         (display p2)(newline)
         (display (equal-point-lists? p2 last-point-coords 0.001))(newline)
         (display wall)(newline)
        ;(raise "Room points extraction not implemented for polyline walls")
        p1
        ))))
  (define (iter point-list walls)
  (display point-list)(newline)
    (if (null-list? walls)
        point-list
      (iter
        (append point-list (get-next-points (car walls) (take-right point-list 1)))
        (cdr walls))))
  (let* ((walls (room-walls graph room))
         (first-wall (car walls)))
    (iter
      `((,(point-coord 'x (wall-last-point first-wall))
         ,(point-coord 'y (wall-last-point first-wall))))
      (cdr walls))))

;; Break in two lists from where a wall was found
;; Warning! This assumes that rooms contain topologically connected walls
;;
(define (room-break graph room first-wall-uid second-wall-uid)
  ; TODO: check if walls are ordered
  (break (lambda (wall) (equal? second-wall-uid (wall-uid wall)))
        (rotate-until-first
          (lambda (wall) (equal? first-wall-uid (wall-uid wall)))
          (room-wall-refs room))))

;; Calculate room area
;;
(define (room-area room)
  ;http://www.mathsisfun.com/geometry/area-irregular-polygons.html
  99.9) ; TODO
