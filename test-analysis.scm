(define graph
'(architecture
 (@ (uid "43CD6949-15FD-401C-BB95-D1D691254283"))
 (room (@ (uid "2603EFE9-7584-457E-95CD-674B54A2BB36"))
       (wall (@ (uid "5181D521-B984-41DA-A54D-D90978E6AD51")))
       (wall (@ (uid "8A179250-2725-4E56-9E83-ED5B859E1ED7")))
       (wall (@ (uid "77296B36-50AD-4CEF-9CF2-37B4D440B736")))
       (wall (@ (uid "2F1077C0-2D27-4EDF-8FA7-6135446F20AC"))))
 (room (@ (uid "49544DDC-E3C8-4CE1-9F00-48B6B99AB8C5"))
       (wall (@ (uid "11C8385B-D8F1-41A3-8208-301E4DAE10B9")))
       (wall (@ (uid "8A179250-2725-4E56-9E83-ED5B859E1ED7")))
       (wall (@ (uid "B02EACA4-84F2-4266-A754-76A52500FD87")))
       (wall (@ (uid "6F5847AB-FAC4-4A64-9D1E-F6135699A605"))))
 (room (@ (uid "62A1800A-BAA3-4857-9154-5BB69A9E2C64"))
       (wall (@ (uid "DB7F49A3-BBF6-40F3-95A1-038CCACE1D88")))
       (wall (@ (uid "E1CFEC6B-46E4-48B2-A41D-D0BBD51A54AC")))
       (wall (@ (uid "11C8385B-D8F1-41A3-8208-301E4DAE10B9")))
       (wall (@ (uid "77296B36-50AD-4CEF-9CF2-37B4D440B736")))
       (wall (@ (uid "BF9D73F0-20E8-4815-829B-641F6597DF80"))))
 (structural (@ (uid "")) (center (@ (y "4.0") (x "6.0"))) (dim (@ (b "0.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "4.0") (x "12.0"))) (dim (@ (b "0.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "10.0") (x "6.0"))) (dim (@ (b "0.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "10.0") (x "12.0"))) (dim (@ (b "0.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "16.0") (x "6.0"))) (dim (@ (b "2.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "16.0") (x "12.0"))) (dim (@ (b "2.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "22.0") (x "6.0"))) (dim (@ (b "0.4") (a "0.3"))))
 (structural (@ (uid "")) (center (@ (y "22.0") (x "12.0"))) (dim (@ (b "0.4") (a "0.3"))))
 (wall (@ (uid "DB7F49A3-BBF6-40F3-95A1-038CCACE1D88")) (pt (@ (y "0.0") (x "0.0"))) (pt (@ (y "0.0") (x "18.0"))))
 (entry (@ (doorNumber "0")) (wall (@ (uid "2F1077C0-2D27-4EDF-8FA7-6135446F20AC"))) (wall (@ (uid "BF9D73F0-20E8-4815-829B-641F6597DF80"))))
 (pipe (@ (y "13.0") (x "0.3")))
 (wall (@ (uid "BF9D73F0-20E8-4815-829B-641F6597DF80")) (pt (@ (y "0.") (x "18."))) (pt (@ (y "10.479678089956993") (x "18."))))
 (wall (@ (uid "2F1077C0-2D27-4EDF-8FA7-6135446F20AC")) (pt (@ (y "10.479678089956993") (x "18."))) (pt (@ (y "26.") (x "18."))))
 (wall (@ (uid "6F5847AB-FAC4-4A64-9D1E-F6135699A605")) (pt (@ (y "26.") (x "0."))) (pt (@ (y "10.479678089956996") (x "0."))))
 (wall (@ (uid "E1CFEC6B-46E4-48B2-A41D-D0BBD51A54AC")) (pt (@ (y "10.479678089956996") (x "0."))) (pt (@ (y "0.") (x "0."))))
 (wall (@ (uid "8A179250-2725-4E56-9E83-ED5B859E1ED7")) (pt (@ (y "10.479678089956995") (x "7.167100884103444"))) (pt (@ (y "26.") (x "7.16710088410345"))))
 (wall (@ (uid "77296B36-50AD-4CEF-9CF2-37B4D440B736")) (pt (@ (y "10.479678089956993") (x "18."))) (pt (@ (y "10.479678089956995") (x "7.167100884103444"))))
 (wall (@ (uid "11C8385B-D8F1-41A3-8208-301E4DAE10B9")) (pt (@ (y "10.479678089956995") (x "7.167100884103444"))) (pt (@ (y "10.479678089956996") (x "0."))))
 (wall (@ (uid "5181D521-B984-41DA-A54D-D90978E6AD51")) (pt (@ (y "26.") (x "18."))) (pt (@ (y "26.") (x "7.16710088410345"))))
 (wall (@ (uid "B02EACA4-84F2-4266-A754-76A52500FD87")) (pt (@ (y "26.") (x "7.16710088410345"))) (pt (@ (y "26.") (x "0."))))))

(define room
'(room (@ (uid "2603EFE9-7584-457E-95CD-674B54A2BB36"))
       (wall (@ (uid "5181D521-B984-41DA-A54D-D90978E6AD51")))
       (wall (@ (uid "8A179250-2725-4E56-9E83-ED5B859E1ED7")))
       (wall (@ (uid "77296B36-50AD-4CEF-9CF2-37B4D440B736")))
       (wall (@ (uid "2F1077C0-2D27-4EDF-8FA7-6135446F20AC")))))


(import ../analysis)
(import ../geometry/kernel)
(import ../core/debug)
(import ../math/algebra)
(import ../graph)

(receive (points walls)
         (room-line-intersection
          graph
          room
          (p (point+direction->line (vect2+
                                     (vect2:random)
                                     (pseq:centroid (room->pseq graph room))) ; TODO: limit random bias
                                    (direction:perpendicular
                                     (segment->direction
                                      (pseq->segment
                                       (wall->pseq
                                        (find-longest-wall-in-room graph room))))))))
         (pp (wall-list->pseq-list walls))
         (pp points)
         (if (or (not (= 2 (length walls)))
                 (not (= 2 (length points))))
             (error "NO BIEN"))
         (make-context-tree `[,graph
                              ()
                              (,room
                               ()
                               (,(car walls)
                                (,(cadr walls)
                                 ()
                                 (,(cadr points)
                                  ()
                                  ()))
                                (,(car points)
                                 ()
                                 ())))]))

(define line     (make-line 0.0 -15.520321910043007 307.95765765058894))

(pp (line->segment line))

(define wall
  '(wall (@ (uid "8A179250-2725-4E56-9E83-ED5B859E1ED7"))
         (pt (@ (y "10.479678089956995") (x "7.167100884103444")))
         (pt (@ (y "26.") (x "7.16710088410345")))))
(pp (wall->pseq wall))

(define wallpoints
  (make-segment (make-point 7.16710088410345 26.0)
                (make-point 7.16710088410345 10.479678089956995)))

(pp (intersection:line-segment
     line
     wallpoints))

(pp (intersection:line-segment
     line
     (pseq->segment (wall->pseq wall))))


(define (line->segment line)
  (cond
   ((~zero? (line-a line))
    (make-segment
     (make-point -100.0 (/ (- (line-c line)) (line-b line)))
     (make-point 100.0 (/ (- (line-c line)) (line-b line)))))
   ((~zero? (line-b line))
    (make-segment
     (make-point (/ (- (line-c line)) (line-a line)) -100.0)
     (make-point (/ (- (line-c line)) (line-a line)) 100.0)))
   (else
    (let ((any-origin (make-point 0.0 (/ (- (line-c line)) (line-b line))))
          (dirmult (vect2:*scalar (line->direction line) 100.0)))
      (make-segment (vect2- any-origin dirmult) (vect2+ any-origin dirmult))))))

(define (<e a b)
  (< (+ a 0.01) b))

(define (>e a b)
  (> a (+ b 0.01)))

(define (collinear-ordered-points? p q r)
  (cond
   ((<e (point-x p) (point-x q))
    (>e (point-x r) (point-x q)))
   ((<e (point-x q) (point-x p))
    (>e (point-x q) (point-x r)))
   ((<e (point-y p) (point-y q))
    (>e (point-y r) (point-y q)))
   ((<e (point-y q) (point-y p))
    (>e (point-y q) (point-y r)))
   (else #t)))




(collinear-ordered-points?
 (make-point 1.0 1.0)
 (make-point 1.0 0.999)
 (make-point 1.0 3.0))

(line->segment (make-line 0.0 15.293721656290371 -289.788262214386))

(collinear-ordered-points?
 (make-point 7.700050882811329 26.0)
 (make-point 7.700050882811329 18.948184668654207)
 (make-point 7.700050882811329 20.619845244616226))
