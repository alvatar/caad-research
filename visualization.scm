(import opengl/gl)
(import sdl/sdl)
(import cairo/cairo)

(import web/parse/ssax-sxml/sxml-tools/sxpath)
(import graph)
(import xml-macros)

(export visualize-graph)

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

(define (visualize-graph graph)
  (visualize-loop-with-continuation graph))

;; Draw graph
;;
(define (graph-to-cairo graph cairo)
  ;; Paint wall from a list of points
  (define (paint-wall-from-points wall)
    (cairo-new-path cairo)
    '())
  ;; Paint doors in the wall
  (define (paint-doors-in-wall door wall)
    (cairo-new-path cairo)
    '())
  ;; Paint windows in the wall
  (define (paint-windows-in-wall window wall)
    (cairo-new-path cairo)
    '())

  (cairo-set-source-rgba cairo 1.0 1.0 0.0 1.0)
  (cairo-set-line-width cairo 15.0)
  (for-each
    (lambda
      (elem1)
      (cond
        ((equal? (car elem1) 'wall)
         ;(pp-code-eval elem1)
         (for-each
           (lambda
             (elem2)
             ;(display elem2)
             (let
               ((this-wall ((sxpath '(@ *)) elem1)))

               (paint-wall-from-points
                 this-wall)
               (paint-windows-in-wall 
                 ((sxpath '(window @ *)) elem1)
                 this-wall)
               (paint-doors-in-wall 
                 ((sxpath '(window @ *)) elem1)
                 this-wall))
             #|
             (cond
               ((equal? (car elem2) '@)
  (cairo-move-to cairo 10.0 10.0)
  (cairo-line-to cairo 10.0 80.0)
  ;; Recursive solution?
                '())
               ((equal? (car elem2) 'pt)
                '())
               ((equal? (car elem2) 'door)
                '())
               ((equal? (car elem2) 'window)
                '()))
                |#
                )
         (cdr elem1))
         (cairo-stroke cairo))
        ((equal? (car elem1) 'pilar)
         (for-each
           (lambda
             (elem2)
             (cond
               ((equal? (car elem2) 'center)
                '())
               ((equal? (car elem2) 'dim)
                '())))
         (cdr elem1)))
        ((equal? (car elem1) 'room)
         (for-each
           (lambda
             (elem2)
             (if
               (equal? (car elem2) 'wall)
               '()))
         (cdr elem1)))
        ((equal? (car elem1) 'entry)
         '())))
    graph)

  #|
  (cairo-set-source-rgba cairo 1.0 1.0 0.0 1.0)
  (cairo-set-line-width cairo 15.0)

  (cairo-new-path cairo)
  (cairo-set-line-cap cairo CAIRO_LINE_CAP_BUTT)
  (cairo-move-to cairo 10.0 10.0)
  (cairo-line-to cairo 10.0 80.0)
  (cairo-stroke cairo)

  (cairo-new-path cairo)
  (cairo-set-line-cap cairo CAIRO_LINE_CAP_ROUND)
  (cairo-move-to cairo 50.0 10.0)
  (cairo-line-to cairo 50.0 80.0)
  (cairo-stroke cairo)

  (cairo-new-path cairo)
  (cairo-set-line-cap cairo CAIRO_LINE_CAP_SQUARE)
  (cairo-move-to cairo 90.0 10.0)
  (cairo-line-to cairo 90.0 80.0)
  (cairo-stroke cairo)
  |#
  )
