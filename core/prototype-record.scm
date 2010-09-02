;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Records implemented as prototypes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;; WARNING! ALPHA CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(declare (standard-bindings)
         (extended-bindings)
         (block))
(compile-options force-compile: #t)

(import prototype)

(define-syntax %?define-prototype-record
  (syntax-rules ()
    ((_ ?maker                       ; for seamless record integration
        (?constructor ?arg ...)
        ?type-checker
        (?field ?accessor) ...)
     (begin
       (define-prototype-check ?type-checker)
       (define (?constructor ?arg ...)
         (object ((?arg ?arg) ...)
                 ((?type-checker self) #t)))
       (define ?maker ?constructor)
       ;(define (?type-checker instance) ($ ?type-checker instance))
       (define (?accessor instance) ($ ?field instance))
       ...))))

(define-macro (define-prototype-record name field . fields)
  (let ((name (symbol->string name)))
    `(%?define-prototype-record ,(string->symbol (string-append "make-" name))
       (,(string->symbol (string-append "new-" name)) ,field ,@fields)
       ,(string->symbol (string-append name "?"))
       ,@(map (lambda (f) `(,f ,(string->symbol
                            (string-append name "-" (symbol->string f)))))
              (cons field fields)))))
