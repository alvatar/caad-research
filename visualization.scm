(import opengl/gl)
(import sdl/sdl)
(import cairo/cairo)

(import graph)
(import xml-macros)

(export visualize-graph)
(export visualize-graph-list)

(define maxx 500)
(define maxy 500)

(define (representation-init)
  (SDL::initialize SDL::init-everything)
  (SDL::set-window-caption "ensanche-core" "ensanche-core")
  (display "--> Representation init\n"))

(define (representation-cleanup)
  (SDL::exit))

;; Visualization loop, controlled with continuations
;;
(define visualize-loop-with-continuation
  (letrec
    ((graph #f)
     (control-state
       (lambda (return)
         (let*
           ((cairo-surface (SDL::set-video-mode maxx maxy 0 (+ SDL::hwsurface
                                                               SDL::hwpalette
                                                               SDL::doublebuf)))
            (image-surface (cairo-image-surface-create-for-data
                             (SDL::surface-pixels cairo-surface)
                             CAIRO_FORMAT_RGB24
                             maxx
                             maxy
                             (SDL::screen-pitch cairo-surface)))
            (cairo (cairo-create image-surface)))
           ;(display "FIRST TIME\n")
           (set! return (call/cc
                          (lambda (resume-here)
                            (set! control-state resume-here)
                            (return))))
         (let loop ()
           (SDL::delay 4)
           ;(display "LOOP\n")
           (let
             ((event (SDL::event-exit)))
             (cond
               ((= event 27) ; 27 = escape TODO!
                (begin 
                  (SDL::exit)
                  (exit 0)))
               ((= event 32) ; 32 = space TODO!
                (return))))

           (cairo-set-source-rgba cairo 1.0 1.0 1.0 1.0)
           (cairo-rectangle cairo 0.0 0.0 (* maxx 1.0) (* maxy 1.0))
           (cairo-fill cairo)
           (graph-to-cairo graph cairo)
           (SDL::flip cairo-surface)

           (loop)))

         (return))))

    (lambda (g)
      (set! graph g)
      (call/cc control-state))))

;; Visualization entry points
;;
(define (visualize-graph graph)
  (visualize-loop-with-continuation graph))

(define (visualize-graph-list graph-list)
  (for-each
    (lambda
      (graph)
      (visualize-graph graph))
    graph-list))

;; Draw graph
;;
(define (graph-to-cairo graph cairo)
  ;; Paint wall
  (define (paint-wall wall)
    (cairo-set-source-rgba cairo 0.1 0.1 0.1 1.0)
    (cairo-set-line-cap cairo CAIRO_LINE_CAP_SQUARE)
    (cairo-set-line-width cairo 5.0)
    (cairo-new-path cairo)
    (cairo-move-to cairo
                   (point-coord 'x (wall-point-n 1 wall))
                   (point-coord 'y (wall-point-n 1 wall)))
    (cairo-line-to cairo
                   (point-coord 'x (wall-point-n 2 wall))
                   (point-coord 'y (wall-point-n 2 wall)))
    (cairo-stroke cairo))
  ;; Paint doors in the wall
  (define (paint-doors-in-wall wall)
    (for-each
      (lambda
        (door)
        (cairo-set-line-cap cairo CAIRO_LINE_CAP_BUTT)
        (cairo-set-source-rgba cairo 1.0 1.0 1.0 1.0)
        (cairo-set-line-width cairo 6.0)
        (paint-path cairo (wall-element-points door wall))
        (cairo-set-source-rgba cairo 1.0 0.1 0.1 1.0)
        (cairo-set-line-width cairo 3.0)
        (paint-path cairo (wall-element-points door wall)))
      (wall-doors wall)))
  ;; Paint windows in the wall
  (define (paint-windows-in-wall wall)
    (cairo-set-source-rgba cairo 1.0 1.0 0.1 1.0)
    (cairo-set-line-cap cairo CAIRO_LINE_CAP_BUTT)
    (cairo-set-line-width cairo 3.0)
    (for-each
      (lambda
        (window)
        (paint-path cairo (wall-element-points window wall)))
      (wall-windows wall)))
  ;; Paint pilar
  (define (paint-pilar pilar)
    (cairo-new-path cairo)
    '())
  ;; Paint room
  (define (paint-room wall-list)
    (cairo-new-path cairo)
    '())
  ;; Paint entry
  (define (paint-entry wall)
    (cairo-new-path cairo)
    '())
  ;; Paint pipe
  (define (paint-pipe wall)
    (cairo-new-path cairo)
    '())

  (for-each
    (lambda
      (elem)
      (cond
        ((equal? elem (void))
          (display "Malformed SXML\n"))
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
         (paint-room (make-wall-list-from-uids (make-uid-list elem) graph)))
        ((equal? (car elem) 'entry)
         (paint-entry (make-wall-list-from-uids (make-uid-list elem) graph)))
        ((equal? (car elem) 'pipe)
         (paint-pipe (make-wall-list-from-uids (make-uid-list elem) graph)))))
    (graph-parts graph)))

;; Paint a path given a list of 2d points
;;
(define (paint-path cairo points)
  (if
    (and (not (null? points)) (list? points))
    (begin
      (cairo-new-path cairo)
      (cairo-move-to cairo
                     (caar points)
                     (cadar points))
      (for-each
        (lambda
          (point)
          (cairo-line-to cairo
                         (car point)
                         (cadr point)))
        (cdr points))
      (cairo-stroke cairo))))
