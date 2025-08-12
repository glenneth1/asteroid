;; -*-lisp-*-
(defpackage :asteroid
            (:use :cl)
            (:use :asteroid.app-utils)
            (:export :-main))

(in-package :asteroid)

(defun -main (&optional args)
  (format t "~a~%" "I don't do much yet"))

