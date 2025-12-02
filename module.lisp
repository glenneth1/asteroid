(in-package #:rad-user)

(define-module #:asteroid
  (:use #:cl #:radiance #:lass #:r-clip #:asteroid.app-utils)
  (:domain "asteroid")
  (:export #:-main))

;; Generate Parenscript files at compile time
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :asteroid)
    (let ((pkg (find-package :asteroid)))
      (when (fboundp (find-symbol "WRITE-SPECTRUM-ANALYZER-JS" pkg))
        (funcall (find-symbol "WRITE-SPECTRUM-ANALYZER-JS" pkg))))))
