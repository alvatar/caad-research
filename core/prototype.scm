;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;; Based on Tiny Talk. Copyright (c) 2008 by Kenneth A Dicke

(import (std srfi/1
             srfi/48)
        syntax)

;-------------------------------------------------------------------------------
; Basic prototype-based OO system
;-------------------------------------------------------------------------------

(define-structure object dispatcher)

(define-macro ($ <selector> <obj> . <args>)
  `((-> ',<selector> ,<obj>) ,<obj> ,@<args>))

(define-macro (define-predicate <name>)
  `(define (,<name> obj)
     (cond ((not (object? obj)) #f)
           ((find-method  ',<name> obj)
            => (lambda (m) (m obj)))
           (else #f))))

(define-macro (object <fields> . <methods>)
  `(let* ( (fields ,(cons 'list
                          (map (lambda (f)
                                 (if (pair? f)
                                     `(cons ',(car f) (make-setter-getter ,(cadr f)))
                                     `(cons ',f (make-setter-getter ':uninitialized))))
                               `,<fields>)))
           (field-names (map car fields)))
     (make-object
      (make-dispatch-table
       (append
        fields
        ;; method procs
        ,(cons 'list 
               (map
                (lambda (m) ;; ((name arg...) body)
                  (let ((name (caar m))
                        (args (cdar m))
                        (body (cdr  m)))
                    `(cons ',name (lambda ,args ,@body))))
                `,<methods>))
        (list
         ;; Default behaviors not shortcut
         (cons 'field-names  (lambda (obj) field-names))
         (cons 'shallow-clone shallow-clone-method)
         (cons 'deep-clone    deep-clone-method)))))))

;;; [$ <selector-sym> <obj> <arg> ...]
; (define-syntax $  ;; send [user syntax]
;   (syntax-rules ()
;     [($ <selector> <obj> <arg> ...)
;      ;;=>
;      ((-> '<selector> <obj>) <obj> <arg> ...)
;     ]
; ) )

(define (-> sym obj) ;; '-> send sym to obj to get a method
  (cond
   [(find-method sym obj)]            ;; Answer method or #f
   [((custom-method-finder) sym obj)] ;; user addition
   [else (error-not-applicable sym obj)]))

(define always-false (lambda (sym obj) #f))

(define custom-method-finder
  (let ([sim+obj->method always-false])
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


;;; (find-method obj selector) -> method or #f
;;;      Search delegate(s) as required
(define (find-method selector obj)
  (cond
   [(not (object? obj)) #f]             ; failed
   [((object-dispatcher obj) selector)] ;; method or #f
   [((object-dispatcher obj) 'delegate)
    => ;; method which returns delegate(s)
    (lambda (m)
      (let ( (delegate (m obj)) )
        (if (list? delegate)
            ;; multiple inheritance [First Found Rule]
            (let loop ( (delegates delegate) )
              (cond
               [(null? delegates) #f]
               [(find-method selector (car delegates))]
               [else (loop (cdr delegates))]))
            ;; single inheritance
            (find-method selector delegate))))]
   [else #f]))



(define (error-not-applicable sym obj)
  (lambda args
    (error sym "not applicable to object" args)))
  ;; Note: it would be nice to use (->string obj)
  ;; here, but if there is a bug in obj's ->string
  ;; then we can get an infinite error loop.


; (define-syntax field-spec->accessor
;   (syntax-rules ()
;     ([field-spec->accessor (<name> <val>)]
;      ;;=>
;      (cons '<name> (make-setter-getter <val>))
;      )
;     ([field-spec->accessor <name>]
;      ;;=>
;      (cons '<name> (make-setter-getter ':uninitialized)))
;     )
; )


;;; (object (<field-spec> ...)  <method-spec>... )
; (define-syntax object
;   (syntax-rules ()
;     ([object (<field-spec> ...)
;              ((<name> <self> <arg> ...) <exp1> <exp2> ...) ...]
;      ;;=>
;      [let* ( [fields
;               (list (field-spec->accessor <field-spec>) ...)]
;              [field-names (map car fields)]
;            )
;      (make-obj
;       (make-dispatch-table
;        (append
;         fields
;         (list ;; method procs
;          (cons '<name>
;                (lambda (<self> <arg> ...) <exp1> <exp2> ...))
;          ...
;          ;; Default behaviors not shortcut
;          (cons 'field-names  (lambda (obj) field-names))
;          (cons 'shallow-clone shallow-clone-method)
;          (cons 'deep-clone    deep-clone-method)
;         ))))]
;      )
; ) )


(define (make-setter-getter val) ;; return a method
  (lambda rest                        ;; (self) or (self new-val)
    (if (null? (cdr rest))
        val
        (begin
          (set! val (cadr rest))
          val))))

(define (make-dispatch-table name-method-alist)
  ;; Return a function which maps a symbol [selector]
  ;;  to a method: (lambda (self <arg>...) ...) or #f
  (define (find-method sym) ;; NB: does NOT follow delegate
    (cond
     [(eq? sym 'lookup)
      (lambda (obj) find-method)]
     [(smart-assq sym)
      => ;; return method
      cdr]
     ;; Default built-in behaviors [keep these few in number]
     [(eq? sym 'method-alist) ;; introspection
      (lambda (obj) name-method-alist)]
     [(eq? sym 'add-method!)    add-method!   ]
     [(eq? sym 'remove-method!) remove-method!]
     [else #f]))
  ;; Don't move (name . meth) if at front of name-method-alist
  ;; short-count determines "front"
  (define short-count 2)
  ;; If found method not near front, move it to front.
  ;; This acts as a cache does to shorten searches
  ;; for commonly used methods but does NOT require
  ;; hashing or cache flushing.
  (define (smart-assq sym) 
    (let count-loop ([count 0][alist name-method-alist])
      (cond
       [(null? alist) #f]                   ;; failed
       [(eq? sym (caar alist)) (car alist)] ; success
       [(< count short-count)
        (count-loop (+ count 1) (cdr alist))]
       [else ;; ASSERT: (>= count short-count)
        (let move-loop ([last alist][current (cdr alist)])
          (cond
           [(null? current) #f]      ;; failed
           [(eq? sym (caar current)) ; success
            ;; splice out found
            (set-cdr! last (cdr current))
            ;; move found to front
            (set-cdr! current name-method-alist)
            (set! name-method-alist current)
            ;; return found (name . method) pair
            (car current)]
           [else (move-loop (cdr last) (cdr current))]))])))
  (define (add-method! obj name method)
    (cond
     [(assq name name-method-alist)
      => (lambda (pair) (set-cdr! pair method))]
     [else
      (set! name-method-alist
            (cons (cons name method) name-method-alist))])
    name)
  (define required-methods
    '(field-names shallow-clone deep-clone))
  (define (remove-method! obj to-remove)
    (unless (symbol? to-remove)
            (error 'remove-method!
                   "bad method selector [not a symbol]"
                   to-remove))
    (when (memq to-remove required-methods)
          (error 'remove-method!
                 "cowardly refuses to break object system"
                 to-remove))
    (set! name-method-alist
          (remp (lambda (pair)
                  (eq? to-remove (car pair)))
                name-method-alist))
    ;; If method was an accessor,
    ;;   remove it from the field-names list.
    (let* ([field-names-bucket
            (assq 'field-names name-method-alist)]
           [field-names-method (cdr field-names-bucket)]
           [field-names-list   (field-names-method obj)])
      (when (memq to-remove field-names-list)
            (let ([new-field-names
                   (remq to-remove field-names-list)])
              (set-cdr! field-names-bucket
                        (lambda (self) new-field-names)))))
    to-remove)

  ;; Return the dispatcher
  find-method)


(define (fresh-alist alist)
  (unless (list? alist)
    (error 'copy-list "requires a list argument" alist))
  (map (lambda (pair) (cons (car pair) (cdr pair))) alist))


;;; (shallow-clone obj)
(define (shallow-clone obj)
  ;; don't recursively clone delegate
  (define (clone-accessors alist)
    (map (lambda (pair)
           (cons (car pair)
                 (make-setter-getter ((cdr pair) obj))))
         alist))
  (unless (object? obj)
    (error 'clone "can't clone non-object" obj))
  (let ( (field-names [$ field-names obj]) )
    (call-with-values
      (lambda ()
          (partition
           (lambda (pair) (memq (car pair) field-names))
           [$ method-alist obj]))
      (lambda (fields methods)
        
        (make-object
         (make-dispatch-table
          (append
           (clone-accessors fields)
           (fresh-alist methods)))))))) ; fresh-alist used because add-method! uses set-cdr!

;;; (deep-clone obj)
(define (deep-clone obj)
  ;; like shallow-clone, but DO recursively clone delegate
  (let* ((cloned (shallow-clone obj))
         (del (assq 'delegate [$ method-alist cloned])))
    (when del ;; (delegate . proc)
          (let* ((delegate ((cdr del) cloned))
                 (cloned-delegate
                  (if (list? delegate)
                      (map deep-clone delegate)
                      (deep-clone delegate))))
            (set-cdr! del (lambda (obj) cloned-delegate))))
    cloned))


;;; Useful helpers
;;
;;; (define-predicate <pred?>) -- syntax
; (define-syntax define-predicate
;   (syntax-rules ()
;     ((define-predicate <pred?>)
;      ;;=>
;      (define (<pred?> obj)
;        (cond
;         [(not (object? obj)) #f]
;         [(find-method '<pred?> obj)
;          =>
;          (lambda (meth) (meth obj))
;          ]
;         [else #f]))
;      )
; ) )

;;; (->string thing)  Return a string describing any thing.

(define (->string thing)
  (define (default:obj->string obj)
    ;; assert (obj ?obj)
    (let ((outp (open-output-string))
          (field-names ($ field-names  thing))
          (lookup      (object-dispatcher thing)))
      ;; NB does not follow delegates.
      (display "#[instance" outp)
      (for-each (lambda (name)
                  (display " " outp)
                  (display name outp)
                  (display ": " outp)
                  (display (->string ((lookup name) thing))
                           outp))
                field-names)
      (display "]" outp)
      (get-output-string outp)))
  
  (define (default:scheme->string non-object)
    (format #f "~a" thing))
  
  ;; ->string main code
  (cond
   [(find-method '->string thing)
    => ;; local object override
    (lambda (m) (m thing))]
   [(object? thing) (default:obj->string    thing)]
   [else            (default:scheme->string thing)]))



(define shallow-clone-method (lambda (self) (shallow-clone self)))
(define deep-clone-method    (lambda (self) (deep-clone self)))

;; (let ( [sim+obj->method always-false] )
;;   (set! custom-method-finder
;;         (lambda rest
;;           (if (null? rest)
;;               sim+obj->method
;;               (let ( (proc (car rest)) )
;;                 (unless (procedure? proc)
;;                         (error 'custom-method-finder
;;                                "requires a procedure (lambda (sym obj) ...)"
;;                                proc))
;;                 (set! sim+obj->method proc)
;;                 proc)))))

;;; Optimizaion Note: both the find-method procs (the 2nd is in 
;;; make-dispatch-table) are where method caches would improve 
;;; performance.  This exactly corresponds to the local and
;;; global caches in some Smalltalk implementations.


;-------------------------------------------------------------------------------
; Value types
;-------------------------------------------------------------------------------

(define add-deputy!   'defined-below)
(define deputy-object 'defined-below)

(define (every? pred? list)
  (let loop ( (list list) )
    (cond
     ((null? list) #t)
     ((pred? (car list)) (loop (cdr list)))
     (else #f))))

(define (any? pred? list)
  (let loop ( (list list) )
    (cond
     ((null? list)       #f)
     ((pred? (car list)) #t)
     (else (loop (cdr list))))))


(define deputy-alist '()) ;; A globle table


(define (deputy-method-finder sym obj)
  ;; Answer a method or #f
  (cond
   [(deputy-object obj)
    => ;; object representing a Scheme native type
    (lambda (deputy) ([$ lookup deputy] sym))] ; NB: NO delegation
   [else #f]))


(define (start-tiny-talk) ;; ensures this lib is "initialized"
  ;; Hook into tiny-talk -> (method lookup) function.
  (custom-method-finder deputy-method-finder)
  'OK)


;; =============================================================
;; Helpers

(define (slice indexed-coll start end repackage)
  ;; start-inclusive end-exclusive
  ;; repackage is list-><whatever>
  (let loop ( [elts '()] [index (- end 1)] )
    (if (< index start)
        (repackage elts)
        (loop (cons [$ iref indexed-coll index] elts) (- index 1)))))

(define (id x) x)

(define (every? pred? list)
  (let loop ( (list list) )
    (cond
     ((null? list) #t)
     ((pred? (car list)) (loop (cdr list)))
     (else #f))))

(define (any? pred? list)
  (let loop ( (list list) )
    (cond
     ((null? list)       #f)
     ((pred? (car list)) #t)
     (else (loop (cdr list))))))

(define (vector-for-each proc vec . vecs)
  (let ( (lists (append (list (vector->list vec)) (map vector->list vecs))) )
    (apply for-each (cons proc lists))))


(define (vector-map proc vec)
  (list->vector (map proc (vector->list vec))))

;; =============================================================
;; EXPORTS

(set! add-deputy!
      (lambda (predicate object)
        (unless (and (procedure? predicate)
                     (object? object))
                (error 'add-deputy!
                       "requires a predicate and an object"
                       predicate object))
        (cond
         [(assq predicate deputy-alist)
          =>
          (lambda (bucket) (set-cdr! bucket object))]
         [else (set! deputy-alist
                     (cons (cons predicate object) deputy-alist))])))
                             
(set! deputy-object
      (lambda (thing)
        (let loop ([deps deputy-alist])
          (cond
           [(null? deps) #f]
           [((caar deps) thing) (cdar deps)]
           [else (loop (cdr deps))]))))


;; =============================================================
;; "SIMPLE" (non-collection) OBJECTS

;; name =? shallow-clone deep-clone

(add-deputy! boolean?
             (object ()
                     [(name self) 'boolean]
                     [(=? self other)
                      (or (and self other)
                          (and (not self) (not other)))]
                     [(shallow-clone self) (if self #t #f)]
                     [(deep-clone    self) (if self #t #f)]))

(add-deputy! symbol?
             (object ()
                     [(name self) 'symbol]
                     [(=? self other) (eq? self other)]
                     [(shallow-clone self) self]
                     [(deep-clone    self) self]))

(add-deputy! char?
             (object ()
                     [(name self) 'character]
                     [(=? self other) (and (char? other)(char=? self other))]
                     [(shallow-clone self) self]
                     [(deep-clone    self) self]))

(add-deputy! procedure?
             (object ()
                     [(name self) 'procedure]
                     [(=? self other) (eq? self other)]
                     [(shallow-clone self) self]
                     [(deep-clone    self) self]))

(add-deputy! number?
             (object ()
                     [(name self) 'number]
                     [(join self other) (+ self other)]
                     [(negate self) (- self)]
                     [(=? self other) (= self other)]
                     [(shallow-clone self) self]
                     [(deep-clone    self) self]))

;; =============================================================
;; COLLECTIONS

;; length map for-each every? any? collect reject join
;; iref slice [for indexed collections only]

(add-deputy! string?
             (object ()
                     [(name self) 'string]
                     [(join self other) (string-append self other)]
                     [(length self) (string-length self)]
                     [(iref self index) (string-ref self index)]
                     [(slice self start end)
                      ;;(slice self start end list->string)
                      (substring self start end)]
                     [(=? self other) (and (string? other) (string=? self other))]
                     [(for-each self proc) ;; for-each-elt
                      (let ( [limit (string-length self)] )
                        (let loop ( [index 0] )
                          (if (>= index limit)
                              'OK
                              (begin (proc (string-ref self index))
                                     (loop (+ index 1))))))]
                     [(map self proc) ;; NB: returns a string
                      (let ( [limit (string-length self)] )
                        (let loop ( [index 0] [results '()] )
                          (if (>= index limit)
                              (list->string (reverse results))
                              (loop (+ index 1)
                                    (cons (proc (string-ref self index))
                                          results)))))]
                     [(every?   self proc) [$ every? (string->list self) proc]]
                     [(any?     self proc) [$ any?   (string->list self) proc]]
                     [(shallow-clone self) (string-copy self)]
                     [(deep-clone    self) (string-copy self)]
                     [(collect self proc)
                      (call-with-values (lambda () (partition proc (string->list self)))
                        (lambda (yes no) (list->string yes)))]
                     [(reject self proc)
                      (call-with-values (lambda () (partition proc (string->list self)))
                        (lambda (yes no) (list->string no)))]))


(add-deputy! vector?
             (object ()
                     [(name self) 'vector]
                     [(join self other) ;; (vector-append self other)
                      (unless (vector? other)
                              (error 'vector:join
                                     "requires two vectors to append together"
                                     self other))
                      (list->vector
                       (append (vector->list self)
                               (vector->list other)))]
                     [(=? self other)
                      (and (vector? other)
                           (or (eq? self other)
                               (and (= (vector-length self) (vector-length other))
                                    (call-with-current-continuation
                                     (lambda (return)
                                       (vector-for-each
                                        (lambda (a b)
                                          (unless [$ =? a b] (return #f)))
                                        self other)
                                       (return #t))))))]
                     [(length self) (vector-length self)]
                     [(iref self index) (vector-ref self index)]
                     [(slice self start end)
                      (slice self start end list->vector)]
                     [(for-each self proc) (vector-for-each proc self)]
                     [(map      self proc) (vector-map      proc self)]
                     [(every?   self proc) [$ every? (vector->list self) proc]]
                     [(any?     self proc) [$ any?   (vector->list self) proc]]
                     [(shallow-clone self) (vector-map id self)]
                     [(deep-clone    self) (vector-map id self)]
                     [(collect self proc)
                      (call-with-values (lambda () (partition proc (vector->list self)))
                        (lambda (yes no) (list->vector yes)))]
                     [(reject self proc)
                      (call-with-values (lambda () (partition proc (vector->list self)))
                        (lambda (yes no) (list->vector no)))]))

(add-deputy! list?
             (object ()
                     [(name self) 'list]
                     [(length self) (length self)]
                     [(join self other) (append self other)]
                     [(=? self other) (equal? self other)
                      ;; #|      (or (eq? self other)
                      ;; (and (list? other)
                      ;; (let loop ( [left self] [right other] )
                      ;; (cond 
                      ;; [(null? left ) (null? right)]
                      ;; [(null? right) #f]
                      ;; [[$ =? (car left) (car right)]
                      ;; (loop (cdr left) (cdr right))]
                      ;; [else #f])))) |#
                      ]
                     [(iref  self index) (list-ref self index)]
                     [(slice self start end)
                      ;; start-inclusive end-exclusive
                      (let start-loop ( [list self] [index 0] )
                        (if (< index start)
                            (start-loop (cdr list) (+ 1 index))
                            ;; ASSERT: (= index start)
                            (let collect-loop ( [elts '()] [list list] [index index] )
                              (if (< index end)
                                  (collect-loop (cons (car list) elts)
                                                (cdr list)
                                                (+ index 1))
                                  (reverse elts)))))]
                     [(every?   self proc) (every?  proc self)]
                     [(any?     self proc) (any?    proc self)]
                     [(shallow-clone self) (map id self)]
                     [(deep-clone    self) (map id self)] ;; bogus
                     [(for-each self proc) (for-each proc self)]
                     [(map      self proc) (map      proc self)]
                     [(collect self proc)
                      (call-with-values (lambda () (partition proc self))
                        (lambda (yes no) yes))]
                     [(reject self proc)
                      (call-with-values (lambda () (partition proc self))
                        (lambda (yes no) no))]))


(start-tiny-talk)