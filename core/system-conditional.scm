;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Conditional system macros
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Conditional compilation
;;; Example:
;;; (compile-cond ("Linux" ("-w -I/usr/include/cairo"
;;;                         "-lcairo"))
;;;               ("Darwin" ("-w -I/opt/local/include/cairo"
;;;                          "-L/opt/local/lib -lobjc -lcairo")))

(define-macro %compile-cond
  (let ((kernel
         (with-input-from-process (list path: "uname"
                                        arguments: '("-s"))
                                  read-line)))
    (lambda opts
      (let ((opt (cadr (assoc kernel opts))))
        `(compile-options cc-options: ,(car opt) ld-options: ,(cadr opt) force-compile: #t)))))

;;; Static if that checks the arguments for a system

(define-macro %if-sys
  (let ((kernel
         (with-input-from-process (list path: "uname"
                                        arguments: '("-s"))
                                  read-line)))
    (lambda (kernel-cond if-true . if-false)
      (if (equal? kernel kernel-cond)
          if-true
          (if (pair? if-false) (car if-false) #f)))))