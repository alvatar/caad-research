;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;; Based on Tiny Talk. Copyright (c) 2008 by Kenneth A Dicke

(import (std srfi/1
             srfi/48)
        container/a-list
        debugging
        syntax)

(export $ ->string define-prototype-check
        object object?
        custom-method-finder)

(%activate-checks)

;-------------------------------------------------------------------------------
; Basic prototype-based OO system
;-------------------------------------------------------------------------------

;;; Object prototype

(define-structure object dispatcher)

;;; Short syntax for sending messages to objects

(define-syntax $
  (syntax-rules ()
    ((_ ?selector ?obj . ?args)
     ((-> '?selector ?obj) ?obj . ?args))))

;;; Define a type-checking predicate

(define-syntax define-prototype-check
  (syntax-rules ()
    ((_ ?name)
     (define (?name obj)
       (cond ((not (object? obj)) #f)
             ((find-method '?name obj) => (lambda (m) (m obj)))
             (else #f))))))

;;; Getter/setter for fields

(define-syntax %?field-spec->accessor
  (syntax-rules ()
    ((field-spec->accessor (?name ?val))
     (cons '?name (make-setter-getter ?val)))
    ((field-spec->accessor ?name)
     (cons '?name (make-setter-getter ':uninitialized)))))

;;; Object creation

(define-syntax object
  (syntax-rules ()
    ((_ (?field-spec ...) ((?method-name ?method-self ?method-args ...) ?expr1 ?expr2 ...) ...)
     (let* ((fields (list (%?field-spec->accessor ?field-spec) ...))
            (field-names (map car fields)))
       (make-object
        (make-dispatch-table
         (append
          fields
          (list (cons '?method-name
                      (lambda (?method-self ?method-args ...) ?expr1 ?expr2 ...))
                ...
                ;; default behaviors          
                (cons 'field-names (lambda (obj) field-names))
                (cons 'shallow-clone shallow-clone-method)
                (cons 'deep-clone deep-clone-method)))))))))

;;; Find the right message handler

(define (-> sym obj)
  (cond ((find-method sym obj))
        (((custom-method-finder) sym obj))
        (else
         ;; it would be nice to use (->string obj) but if there is a
         ;; bug in obj's ->string then an infinite error loop might occur
         (lambda args (error sym "message not recognized by prototype" args)))))

(define custom-method-finder
  (let ((sim+obj->method (lambda (sym obj) #f)))
    (lambda rest
      (if (null? rest)
          sim+obj->method
          (let ((proc (car rest)))
            (unless (procedure? proc)
                    (error 'custom-method-finder
                           "requires a procedure (lambda (sym obj) ...)"
                           proc))
            (set! sim+obj->method proc)
            proc)))))

;;; Search methods, look for delegate(s) if required
;;; -> method or #f
;;; TODO: Could be optimized with a cache

(define (find-method selector obj)
  (when (object? obj)                   ; return #f if not an object
        (let ((dispatcher (object-dispatcher obj)))
          (cond ((dispatcher selector)) ; find method
                ((dispatcher 'delegate) ; find delgate
                 => (lambda (m)
                      (let ((delegate (m obj)))
                        (if (list? delegate)
                            ;; multiple inheritance (First Found Rule)
                            (let loop ((delegates delegate))
                              (cond ((null? delegates) #f)
                                    ((find-method selector (car delegates)))
                                    (else (loop (cdr delegates)))))
                            ;; single inheritance
                            (find-method selector delegate)))))
                (else #f)))))

;;; Make an accessor procedure
;;; -> (lambda) (self) or (self val)

(define (make-setter-getter val)
  (lambda self&args (if (null? (cdr self&args))
                   val
                   (begin (set! val (cadr self&args)) val))))

;;; Make a procedure which maps a symbol (selector) to a method
;;; -> (lambda (self args ...)) or #f

(define (make-dispatch-table name-method-alist)
  (letrec
      ((find-method
        (lambda (sym)
          (let ((assq! (a-list:make-assq-reorder! 5)))
            (cond ((eq? sym 'lookup) (lambda (obj) find-method))
                  ((assq! sym name-method-alist) => cdr)
                  ;; Default built-in behaviors
                  ((eq? sym 'methods) (lambda (obj) name-method-alist))
                  ((eq? sym 'add-method!) add-method!)
                  ((eq? sym 'remove-method!) remove-method!)
                  (else #f)))))
       (add-method!
        (lambda (obj name method)
          (cond
           ((assq name name-method-alist) => (lambda (p) (set-cdr! p method)))
           (else
            (set! name-method-alist
                  (cons (cons name method) name-method-alist))))
          name))
       (remove-method!
        (lambda (obj to-remove)
          (%accept (symbol? to-remove) "bad method selector (not a symbol)")
          (%deny (memq to-remove '(field-names shallow-clone deep-clone))
                 "this method is required: can't be removed")
          (set! name-method-alist
                (remp (lambda (pair)
                        (eq? to-remove (car pair)))
                      name-method-alist))
          (let* ((field-names-bucket
                  (assq 'field-names name-method-alist))
                 (field-names-method (cdr field-names-bucket))
                 (field-names-list (field-names-method obj)))
            (when (memq to-remove field-names-list)
                  (let ([new-field-names
                         (remq to-remove field-names-list)])
                    (set-cdr! field-names-bucket
                              (lambda (self) new-field-names))))))))
    find-method))

;;; Clone an object without recursively cloning its delegates

(define (shallow-clone obj)
  (define (clone-accessors al)
    (map (lambda (p) (cons (car p)
                      (make-setter-getter ((cdr p) obj))))
         al))
  (%accept (object? obj) "can't clone because this isn't an object")
  (let ((field-names ($ field-names obj)))
    (receive (fields methods)
             (partition (lambda (pair) (memq (car pair) field-names))
                        ($ methods obj))
             (make-object
              (make-dispatch-table
               (append
                (clone-accessors fields)
                ;; make a new a-list because add-method! uses set-cdr!
                (map (lambda (pair) (cons (car pair) (cdr pair))) methods)))))))
(define shallow-clone-method (lambda (self) (shallow-clone self)))

;;; Recursively clones delegates

(define (deep-clone obj)
  (let* ((cloned (shallow-clone obj))
         (del (assq 'delegate [$ methods cloned])))
    (when del
          (let* ((delegate ((cdr del) cloned))
                 (cloned-delegate
                  (when (list? delegate)
                        (map deep-clone delegate)
                        (deep-clone delegate))))
            (set-cdr! del (lambda (obj) cloned-delegate))))
    cloned))
(define deep-clone-method (lambda (self) (deep-clone self)))

;;; Return a string describing any object

(define (->string thing)
  (let ((obj->string
         (lambda (obj)
           (let ((outp (open-output-string))
                 (field-names ($ field-names  obj))
                 (lookup (object-dispatcher obj)))
             (display "#[instance" outp)
             (for-each (lambda (name)
                         (display " " outp)
                         (display name outp)
                         (display ": " outp)
                         (display (->string ((lookup name) obj))
                                  outp))
                       field-names)
             (display "]" outp)
             (get-output-string outp))))
        (scheme->string
         (lambda (e) (format #f "~a" e))))
    (cond
     ((find-method '->string thing) => (lambda (m) (m thing)))
     ((object? thing) (obj->string thing))
     (else (scheme->string thing)))))