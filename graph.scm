;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Graph definition and low-level operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import (std string/xml-to-sxml))
(import (std misc/uuid))
(import web/parse/ssax-sxml/sxml-tools/sxpath)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get everything inside the architecture tag as a list
;;
(define (graph-parts graph)
  ;((sxpath '(*)) graph))
  (if
    ;(and (not (null? graph)) (list? graph))
    (pair? graph)
    (cdr graph)
    '()))

;; Make uids list TODO
;;
(define (make-uid-list subgraph)
  ((sxpath '(wall @ uid *text*)) subgraph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modifiers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (add-wall graph point-a point-b uuid)
  (cons (car graph)
        (append (cdr graph)
                (list `(wall (@ (uid ,uuid))
                         (pt (@ (y ,(number->string (point-coord 'y point-a)))
                                (x ,(number->string (point-coord 'x point-a)))))
                         (pt (@ (y ,(number->string (point-coord 'y point-b)))
                                (x ,(number->string (point-coord 'x point-b))))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Points
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get coordinate from point
;;
(define (point-coord coordinate point)
  (define (find-coordinate point)
    (if (equal? (caar point) coordinate)
        (string->number (cadar point))
        (find-coordinate (cdr point))))
  (find-coordinate point))

;; Get point n from point list
;;
(define (point-n n point-list)
  (cdr (list-ref point-list n)))

;; Make point
;;
(define (make-point x y)
  (if (or (null? x) (null? y))
      (display "Error making point\n")
      (list (list 'y (number->string y))
            (list 'x (number->string x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Wall
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get all wall points
;;
(define (wall-points-structured wall)
  ((sxpath '(pt @)) wall))

;; Get wall point n
;;
(define (wall-point-n n wall)
  ((sxpath `((pt ,n) @ *)) wall))

;; Get windows in wall
;;
(define (wall-windows wall)
  ((sxpath '(window)) wall))

;; Get doors in wall
;;
(define (wall-doors wall)
  ((sxpath '(door)) wall))

;; Calculate wall element (door, wall...) points
;;
(define (wall-element-points element wall)
  (let ((from (string->number (car ((sxpath '(@ from *text*)) element))))
        (to (string->number (car ((sxpath '(@ to *text*)) element)))))
    (if (= (length (wall-points-structured wall)) 2)
        (let* ((Ax (point-coord 'x (wall-point-n 1 wall)))
               (Ay (point-coord 'y (wall-point-n 1 wall)))
               (ABx (- (point-coord 'x (wall-point-n 2 wall)) Ax))
               (ABy (- (point-coord 'y (wall-point-n 2 wall)) Ay)))
          (list `(,(+ Ax (* ABx from)) ,(+ Ay (* ABy from)))
                `(,(+ Ax (* ABx to)) ,(+ Ay (* ABy to)))))
          (display "Error - wall element has more than 2 relative points\n"))))
      ; Else:
        ; 1. Precalcular lista de puntos relativos
        ; 2. Hacer lista de puntos relativos menores que puerta
        ; 3. Dibujar trayectoria de puerta completa de los segmentos menores
        ; 4. Dibujar el porcentaje restante sobre el siguiente segmento
 
;; Make list of walls from uids
;;
(define (make-wall-list-from-uids uids graph)
  '()) ; TODO


;; Make list of walls from uids
;;
(define (make-wall-list-from-uids uids graph)
  '()) ; TODO

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Room
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Get rooms in the graph
;;
(define (rooms graph)
  ((sxpath '(room)) graph))

;; Calculate room area
;;
(define (room-area room)
  99.9) ; TODO
