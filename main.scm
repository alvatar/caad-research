(import opengl/gl)
(import sdl/sdl)
(import cairo/cairo)
(import generate-graph-from-xml)

(define (main)

  (SDL::initialize SDL::init-everything)
  (SDL::set-window-caption "ensanche-core" "ensanche-core")
  (let*
    ((maxx 500)
     (maxy 500)
     (cairo-surface (SDL::set-video-mode maxx maxy 0 (+ SDL::hwsurface
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
     (xml-file (open-input-file "arch.xml"))
     (xml-string (read-line xml-file #f))
     (close-port xml-file))

    ;(pp-code-eval sxml)
    ;((sxpath "ensanche/floorPlan") sxml)
    ;(display (sxpath '(sxml)))
    (generate-graph-from-xml xml-string)

    (let loop ()
      (SDL::delay 4)
      (if (SDL::event-exit)
        (begin (SDL::exit)
               (exit 0)))
      (paint-loop)
      (loop)))

  (SDL::exit)
  (exit 0))

(main)
