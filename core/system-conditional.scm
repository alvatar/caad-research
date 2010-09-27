;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditional system macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; TODO: This shouldn't use arguments but a common system discovery method



;;; Conditional compilation
;;; Example:
;;; (compile-cond ("Linux" ("-w -I/usr/include/cairo"
;;;                         "-lcairo"))
;;;               ("Darwin" ("-w -I/opt/local/include/cairo"
;;;                          "-L/opt/local/lib -lobjc -lcairo")))

(define-macro (%compile-cond . opts)
  (let ((kernel
         (let find ((args (cdr (command-line))))
           (cond
            ((null? args)
             (error "Please pass an argument of type System:<> to be able to compile platform-specific code"))
            ((equal? (car args) "@System:")
             (if (null? (cdr args))
                 (error "Please supply the System: argument")
                 (cadr args)))
            (else
             (find (cdr args)))))))
    (let ((opt (cadr (assoc kernel opts))))
      `(compile-options cc-options: ,(car opt) ld-options: ,(cadr opt) force-compile: #t))))

;;; Static if that checks the arguments for a system

(define-macro (%if-sys kernel if-true . if-false)
  (let ((cli-arg-kernel
         (let find ((args (cdr (command-line))))
           (cond
            ((null? args) #f)
            ((equal? (car args) "@System:")
             (if (null? (cdr args))
                 (error "Please supply the System: argument")
                 (cadr args)))
            (else
             (find (cdr args)))))))
    (if (equal? cli-arg-kernel kernel)
        if-true
        (if (pair? if-false) (car if-false) #f))))