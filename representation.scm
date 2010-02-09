(import opengl/gl)
(import sdl/sdl)
(import cairo/cairo)

(import graph)

(define (representation-init)
  (SDL::initialize SDL::init-everything)
  (SDL::set-window-caption "ensanche-core" "ensanche-core")
  (display "--> Representation init\n"))

(define (representation-cleanup)
  (SDL::exit))

(define (represent-loop graph)
  (define (control-state)
    (let*
      ((maxx 500)
       (maxy 500)
       #|(cairo-surface (SDL::set-video-mode maxx maxy 0 (+ SDL::hwsurface
                                                          SDL::hwpalette
                                                          SDL::doublebuf)))
       (image-surface (cairo-image-surface-create-for-data
                        (SDL::surface-pixels cairo-surface)
                        CAIRO_FORMAT_RGB24
                        maxx
                        maxy
                        (SDL::screen-pitch cairo-surface)))
       (cairo (cairo-create image-surface))
       (paint-loop (lambda ()
                     (cairo-set-source-rgba cairo 1.0 1.0 1.0 1.0)
                     (cairo-rectangle cairo 0.0 0.0 (* maxx 1.0) (* maxy 1.0))
                     (cairo-fill cairo)

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

                     (SDL::flip cairo-surface)

                     ))
                     |#
                     )
      (display "FIRST TIME ONLY\n")

      ;
      (define represent-loop
  (letrec ((control-state
            (lambda ()
              (display "FIRST TIME ONLY\n")
              (call/cc
               (lambda (resume-here)
                 (set! control-state resume-here)))
              (let loop ()
                (display "WILL BE LOOP\n")))))
    (lambda (graph)
      control-state)))

(define (represent graph)
  ((represent-loop graph)))
;


      (call-with-current-continuation
        (lambda (resume-here)
          ;; Grab the current continuation
          (set! control-state resume-here)
          (let loop ()
            #|
            (SDL::delay 4)
            (if (SDL::event-exit)
              (begin (SDL::exit)
                     (exit 0)))
                     |#
            (display "WILL BE LOOP\n")
            ;(paint-loop)
            ;(loop)
            )))))
  
  control-state)

(define (represent graph)
  ((represent-loop graph)))
