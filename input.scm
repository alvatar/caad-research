(define (input)
  (let*
    ((xml-file (open-input-file "arch.xml"))
     (xml-string (read-line xml-file #f))
     (close-port xml-file))
     xml-string))
