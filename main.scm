;;; Copyright (c) 2010 by Álvaro Castro-Castilla, All Rights Reserved.
;;; Licensed under the GPLv3 license, see LICENSE file for full description.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import core/tagged-list
        evolution
        input
        output
        visualization)

(let ((input-dir "xml-input/")
      (output-dir "xml-output/")
      (pool-size-target 5))
  (random-source-randomize! default-random-source)
  (evolution (list@ (evolver-type 'choose-bests)
                    (max-iterations 100)
                    (pool-size (if (file-exists? output-dir)
                                   (let* ((files (directory-files output-dir))
                                          (num-files (length files)))
                                     (if (>= num-files pool-size-target)
                                         (begin
                                           (for-each
                                            (lambda (f)
                                              (delete-file
                                               (string-append output-dir f)))
                                            files)
                                           pool-size-target)
                                         (- pool-size-target num-files)))
                                   (begin
                                     (create-directory output-dir)
                                     pool-size-target))))
             'bath-block
             (input-from-xml
              (string-append
               input-dir
               (let ((files (directory-files input-dir)))
                 (if (> (length files) 1)
                     (begin
                       (display
                        "warning: there are more than one xml in the input directory\n")
                       (car files))
                     (car files)))))
             output)
  (visualization:exit)
  (exit 0))
