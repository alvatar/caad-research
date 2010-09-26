;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Low-level and auxiliary operations on a graph
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (declare (standard-bindings)
;;          (extended-bindings)
;;          (block))
;; (compile-options force-compile: #t)

(import (std srfi/1)
        core/list
        core/functional
        core/syntax
        core/debugging
        geometry/kernel
        geometry/bounding-box
        math/exact-algebra
        math/inexact-algebra
        graph)

(%activate-checks)

;-------------------------------------------------------------------------------
; Selectors
;-------------------------------------------------------------------------------

(define (graph:filter.rooms g)
  (filter (lambda (e) (room? e)) (graph-architecture g)))

(define (graph:filter.walls g)
  (filter (lambda (e) (wall? e)) (graph-architecture g)))

(define (graph:filter.structurals g)
  (filter (lambda (e) (structural? e)) (graph-architecture g)))

(define (graph:filter.entries g)
  (filter (lambda (e) (entry? e)) (graph-architecture g)))

(define (graph:filter.pipes g)
  (filter (lambda (e) (pipe? e)) (graph-architecture g)))

(define (graph:filter.windows g)
  (reduce append '() (map (lambda (w) (wall-windows w)) (graph:filter.walls g))))

(define (graph:filter.doors g)
  (reduce append '() (map (lambda (w) (wall-doors w)) (graph:filter.walls g))))

(define (graph:find.wall/uid graph uid)
  (aif element (find
                (lambda (e) (and (wall? e) (equal? uid (wall-uid e))))
                (graph-architecture graph))
       element
       (begin (display "UID: ")(display uid)(newline)
              (error "Wall with such UID not found"))))

(define (graph:find.room/uid graph uid)
  (aif element (find
                (lambda (r) (equal? uid (room-uid r)))
                (graph:filter.rooms graph))
       element
       (begin (display "uid: ")(display uid)(newline)
              (error "Room with such UID not found"))))

(define (graph:filter.room-walls graph room)
  (map (lambda (r) (graph:find.wall/uid graph r)) (room-walls room)))

;-------------------------------------------------------------------------------
; Predicates
;-------------------------------------------------------------------------------

;;; Are these walls connected?

(define (graph:walls-are-connected? wall1 wall2)
  (segment:connected-segment?
   (wall-segment wall1)
   (wall-segment wall2)))

;;; Is this wall exterior?

(define (graph:exterior-wall? wall graph)
  (define (point-in-any-room? p)
    (any (lambda (room) (graph:point-in-room? graph room p))
         (graph:filter.rooms graph)))
  (let* ((wall-seg (wall-segment wall))
         (mid-p (segment:normalized-1d->point wall-seg 1/2))
         (tangent-p (pseq:tangent-in-relative (segment->pseq ; TODO: ??
                                               wall-seg)
                                              1/2)) ; TODO: Could this be improved with normalized?
         (p1 (rotate.point/ref mid-p
                               (vect2+ mid-p
                                       (vect2:*scalar tangent-p equal-accuracy))
                               ~pi/2))
         (p2 (rotate.point/ref mid-p
                               (vect2+ mid-p
                                       (vect2:*scalar tangent-p equal-accuracy))
                               ~-pi/2)))
    (not (and (point-in-any-room? p1)
              (point-in-any-room? p2)))))

;;; Is point in room?

(define (graph:point-in-room? graph room point)
  (pseq:point-inside? (graph:room->pseq graph room) point))

;;; Is point inside the graph limits?

(define (graph:point-inside? graph point)
  (pseq:point-inside? (graph:limits graph) point))

;;; Is the wall of this room?

(define (graph:room-wall? room wall) (%accept (and (room? room) (wall? wall)))
  (find (lambda (wuid) (equal? wuid (wall-uid wall))) (room-walls room)))

;-------------------------------------------------------------------------------
; Finders/Filters
; Find returns #f is no object is found
;-------------------------------------------------------------------------------

;;; Filter the exterior walls

(define (graph:filter.exterior-walls graph)
  (define (iter exterior-walls rest-walls)
    (cond
     ((null? rest-walls)
      exterior-walls) ; TODO: check if closed and do something about it, TODO: multiple contours
     ((graph:exterior-wall? (car rest-walls) graph)
      (iter (cons (car rest-walls) exterior-walls) (cdr rest-walls)))
     (else
      (iter exterior-walls (cdr rest-walls)))))
  (graph:sort.wall-list-connected graph (iter '() (graph:filter.walls graph))))

;;; Filter common walls

(define (graph:filter.common-room-walls room-a room-b)
  (filter (lambda (a) (any (lambda (b) (equal? a b)) (room-walls room-b)))
          (room-walls room-a)))

;;; Find walls connected to a given one

(define (graph:filter.walls-connected/wall graph wall)
  (let ((wall-seg (wall-segment wall))
        (inspected-walls (remove
                          (lambda (w) (equal? wall w))
                          (graph:filter.walls graph))))
    (values
     (aif connected-walls
          null?
          (filter (lambda (w) (segment:end-point? (wall-segment w) (segment-a wall-seg))) inspected-walls)
          (error "can't find any wall connected to this one: inconsistency")
          connected-walls)
     (aif connected-walls
          null?
          (filter (lambda (w) (segment:end-point? (wall-segment w) (segment-b wall-seg))) inspected-walls)
          (error "can't find any wall connected to this one: inconsistency")
          connected-walls))))

;;; Find walls connected to a given one in a room

(define (graph:filter.walls-connected/wall/room graph wall room)
  (let ((lists (values->list
                (apply/values (curry filter (lambda (w) (graph:room-wall? room w)))
                              (graph:filter.walls-connected/wall graph wall)))))
    (if (any null? lists)
        '()
        (map car lists))))

;;; Find longest wall in room

(define (graph:find.longest-wall-in-room graph room)
  (aif walls (graph:filter.room-walls graph room) null?
       #f
       (fold
        (lambda (w maxw)
          (if (< (segment:squaredlength (wall-segment maxw))
                 (segment:squaredlength (wall-segment w)))
              w
              maxw))
        (car walls)
        (cdr walls))))

;-------------------------------------------------------------------------------
; Geometrical calculations
;-------------------------------------------------------------------------------

;;; Calculate bounding box

(define-memoized/key-gen graph:bounding-box 
  (lambda (graph) (graph-uid graph))
  (lambda (graph)
    (pseq:bbox (graph:wall-list->pseq (graph:filter.exterior-walls graph)))))

;;; External polygon extraction

(define (graph:limits graph)
  (graph:wall-list->pseq (graph:filter.exterior-walls graph)))

;;; Total area of the graph

(define (graph:total-area graph)
  (pseq:area (graph:limits graph)))

;;; Calculate the pseq that describes a list of walls

(define (graph:wall-list->pseq wlis)
  (cond
   ((null? (cdr wlis))
    (segment->pseq ; TODO: MAKE An append-segment
     (wall-segment (car wlis))))
   (else
    (pseq:append
      (segment->pseq
       (wall-segment (car wlis)))
      (graph:wall-list->pseq (cdr wlis))))))

;;; Walls common point

(define (graph:walls-common-point w1 w2)
  (aif cp (segment:connected-segment?
           (wall-segment w1)
           (wall-segment w2))
       cp
       (begin (pp (wall-segment w1))
              (pp (wall-segment w2))
              (error "Given walls don't have any common point"))))

;;; Wall distances from end points

(define (graph:~walls-distance/endpoints w1 w2)
  (~distance.segment-segment/endpoints (wall-segment w1) (wall-segment w2)))

;;; Calculate the closest wall to a point

(define (graph:nearest-wall graph point)
  (min/generator (lambda (w)
                   (squareddistance.point-segment point (wall-segment w)))
                 (graph:filter.walls graph)))

;;; Calculate the pseq that describes a room

(define (graph:room->pseq graph room)
  (graph:wall-list->pseq (graph:filter.room-walls graph room)))

;;; Calculate room area

(define (graph:room-area graph room)
  (pseq:area (graph:room->pseq graph room)))

;;; Intersection of room and line returns a list of intersected walls and
;;; intersection points
;;; Output: 2 values of the same size (walls and intersections)

(define (graph:room-relative-line-intersections graph room line)
  (let* ((walls (graph:filter.room-walls graph room))
         (intersections (map
                         (lambda (w)
                           (intersect.line-segment
                            line
                            (wall-segment w)))
                         walls)))
    (unzip2
     (filter-map (lambda (e)
                   (let ((wall (car e))
                         (intersection (cadr e)))
                     (and
                      (point? intersection)
                      (list wall
                            (segment:point->normalized-1d
                             (wall-segment wall)
                             intersection)))))
                 (zip walls intersections)))))

;;; Returns all the intersections of a line with the graph
;;; Output: 3 values of the same size (rooms, walls and intersections)

;;; TODO: this functions should be rethought, it's output is not well-structured

(define (graph:relative-line-intersections graph line)
  (fold/values
   (lambda (room rooms walls intersections)
     (receive (new-wall new-intr)
              (graph:room-relative-line-intersections graph room line)
              (values (if (not-null? new-wall) (append rooms room) rooms)
                      (append walls new-wall)
                      (append intersections new-intr))))
   '(() () ())
   (graph:filter.rooms graph)))

;;; Calculate room aspect ratio

(define (graph:room-aspect-ratio graph room)
  (let ((bbxsg (bbox:size-segment
                (pseq:bbox (graph:room->pseq graph room)))))
    (max (vect2:x/y bbxsg)
         (vect2:y/x bbxsg))))

;;; Calculate south from north direction

(define (graph:north->south vec)
  (rotate.point/O vec (inexact->exact ~pi)))

;;; Calculate north-east from north direction

(define (graph:north->north-east vec)
  (rotate.point/O vec (inexact->exact ~-pi/4)))

;;; Calculate east from north direction

(define (graph:north->east vec)
  (rotate.point/O vec (inexact->exact ~-pi/2)))        ; TODO: perpendicular

;-------------------------------------------------------------------------------
; Low-level operations
;-------------------------------------------------------------------------------

;;; Add a new element to the graph

(define (graph:add graph arguments) (%accept (graph? graph))
  ;; TODO: A context with the graph and a room would allow adding the element and reference
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (cons arguments (graph-architecture graph))))

;;; Remove any element from the graph

(define (graph:remove graph element) (%accept (graph? graph) "context is not a graph")
  ;; TODO: remove references, too. IMPORTANT
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (remove (lambda-equal? element) (graph-architecture graph))))

;;; Remove element-list from graph

(define (graph:remove-multiple graph le) (%accept (graph? graph))
  ;; TODO: remove references, too. IMPORTANT
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (remove (lambda (e) (any (lambda-equal? e) le)) (graph-architecture graph))))

;;; Change an element property or the whole element
;;; Multiple properties can be changed if a list is along with multiple tail arguments
;;; (_ graph element new-element)
;;; (_ graph element property (new-values))
;;; (_ graph element (properties) (new-values))

(define (graph:update-element graph element new-element/properties . values)
  ;; TODO: use case-lambda!
  (let ((update-properties
         (lambda (props&vals)
           (make-graph
            (graph-uid graph)
            (graph-environment graph)
            (substitute (lambda-equal? element)
                        ;; <-- TODO: this *really* needs the object system
                        (cond
                         ((wall? element)
                          (make-wall (uif (assq 'uid props&vals) (cadr ?it) (wall-uid element))
                                     (uif (assq 'metadata props&vals) (cadr ?it) (wall-metadata element))
                                     (uif (assq 'segment props&vals) (cadr ?it) (wall-segment element))
                                     (uif (assq 'windows props&vals) (cadr ?it) (wall-windows element))
                                     (uif (assq 'doors props&vals) (cadr ?it) (wall-doors element))))
                         (else (error "doesn't allow changing any other element than walls")))
                        ;; <-- until here
                        (graph-architecture graph)))))
        (update-whole-element
         (lambda ()
           (make-graph
            (graph-uid graph)
            (graph-environment graph)
            (substitute (lambda-equal? element) new-element/properties (graph-architecture graph))))))
    (if (null? new-element/properties)
        ;; whole element update
        (update-whole-element)
        ;; properties update
        (if (list? new-element/properties)
            (begin
              (%accept (and (list? values) (= (length new-element/properties) (length values)))
                       "not a proper combination of properties/values")
              (update-properties (zip new-element/properties values)))
            (update-properties (list (cons new-element/properties values)))))))

;;; Update refs to walls in rooms

(define (graph:update-wall-refs-in-rooms graph uid new-uids)
  (make-graph
   (graph-uid graph)
   (graph-environment graph)
   (map (lambda (e)
          (if (room? e)
              (make-room
               (room-uid e)
               (multiple-substitute (lambda-equal? uid) new-uids (room-walls e)))
              e))
        (graph-architecture graph))))

;;; Classify the windows of a wall in three groups depending on their position
;;; respect a reference relative point:
;;; @returns: one side, the other, in-between

(define (graph:partition-windows/point wall split-x)
  (let/cc exit
   (let ((wall-seg (wall-segment wall)))
     (fold/values (lambda (w a b c)
                    (let ((window-segment (window-segment w)))
                      (let ((window-from
                             (segment:point->normalized-1d wall-seg
                                                           (segment-a window-segment)))
                            (window-to
                             (segment:point->normalized-1d wall-seg
                                                           (segment-b window-segment))))
                        (cond
                         ;; TODO: HORRIBLE HACK DUE TO SOME NON-COLLINEARITY OF WINDOWS
                         ((not (and (number? window-from)
                                    (number? window-to)))
                          (values #f #f #f))
                         ;; both points fall into the first side
                         ((and (< window-from split-x)
                               (< window-to split-x))
                          (values (cons w a) b c))
                         ;; both points fall into the second side
                         ((and (> window-from split-x)
                               (> window-to split-x))
                          (values a (cons w b) c))
                         ;; the window is in between
                         (else
                          (values a b (cons w c)))))))
                  '(() () ())
                  (wall-windows wall)))))

;;; Create two new walls where one was before, given a splitting point
;;; @returns 3 vals: 2 new walls + status

(define (graph:split-wall wall split-x uuid1 uuid2) ; TODO: make segment-based walls
  (let ((wall-seg (wall-segment wall)))
    (let ((split-point (segment:normalized-1d->point wall-seg split-x))
          (first-point (segment-a wall-seg))
          (second-point (segment-b wall-seg)))
      (receive (first-side-windows second-side-windows splitted-windows)
               (graph:partition-windows/point wall split-x)
               (values
                (make-wall uuid1
                           '((type "new"))
                           (make-segment first-point split-point)
                           first-side-windows
                           '())
                (make-wall uuid2
                           '((type "new"))
                           (make-segment split-point second-point)
                           second-side-windows
                           '())
                (if (null? splitted-windows)
                    'ok
                    'lost-window))))))

;;; Try to merge into one wall if the two given are parallel

(define (graph:try-to-merge-if-parallel-walls walls new-uid) (%accept (and (list? walls)))
  ;; TODO: input as a list of arguments
  (let ((wall-a (car walls))
        (wall-b (cadr walls)))
    (let ((wall-a-seg (wall-segment wall-a))
          (wall-b-seg (wall-segment wall-b)))
      (if (segment:parallel-segment? wall-a-seg wall-b-seg)
          (let ((first-point (if (segment:end-point? wall-b-seg (segment-a wall-a-seg))
                                 (segment-b wall-a-seg)
                                 (segment-a wall-a-seg)))
                (second-point (if (segment:end-point? wall-a-seg (segment-a wall-b-seg))
                                  (segment-b wall-b-seg)
                                  (segment-a wall-b-seg))))
            (list (make-wall
                   new-uid
                   '((type "new"))
                   (make-segment first-point second-point)
                   (append (wall-windows wall-a)
                           (wall-windows wall-b))
                   '()))) ; TODO: we drop doors, because in the future everything will be holes
          walls))))

;;; Break in two lists from where a wall was found
;;; Warning! This assumes that rooms contain topologically connected walls

(define (graph:room-break graph room first-wall-uid second-wall-uid)
                                        ; TODO: check if walls are ordered
  (break (lambda (wall) (equal? second-wall-uid wall))
         (find-rotate
          (lambda (wall) (equal? first-wall-uid wall))
          (room-walls room))))

;;; Fix order of walls in a room

(define (graph:sort.room-walls graph room)
  (make-room
   (room-uid room)
   (map (lambda (w)
          (wall-uid w))
        (graph:sort.wall-list-connected graph (graph:filter.room-walls graph room)))))

;;; Sort walls in a wall list so they are connected properly

(define (graph:sort.wall-list-connected graph wall-list) ; TODO: check if the last and the first are really connected
  (define (iter sorted remaining)
    (define (find-next first wall-list) ; (it sorts backwards)
      (cond
       ((null? wall-list)
        #f)
       ((graph:walls-are-connected? first (car wall-list))
        (car wall-list))
       (else
        (find-next first (cdr wall-list)))))
    (if (null? remaining)
        sorted
      (aif next (find-next (car sorted) remaining)
        (iter (cons next sorted) (remove (lambda (e) (equal? e next)) remaining))
        (begin
          (display "----------\n")
          (pp sorted)
          (display "----------\n")
          (pp remaining)
          (error "graph:sort.walls-connected -- This wall cannot be connected to any other")))))
  
  (if (null? wall-list)
      (error "Argument #2 (wall-list) is null")
    (iter (list (car wall-list)) (cdr wall-list))))

;;; Snap room walls, in case they are close enough, so the room can be a closed shape

(define (graph:snap.room-walls graph room delta)
  (pair-fold-2
   (lambda (e a)
     (if (> (graph:~walls-distance/endpoints (graph:find.wall/uid graph (car e))
                                             (graph:find.wall/uid graph (cadr e)))
            delta)
         (error "implement wall snapping fix") ; will need PAIR-MAP
         a))
   (void)
   (room-walls room))
  room) ; TODO: obiously this is not doing anything now