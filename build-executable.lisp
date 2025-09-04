#!/usr/local/bin/sbcl --script
;; -*-lisp-*-

(load "~/quicklisp/setup.lisp")

;; Build script for creating asteroid executable using save-lisp-and-die
;; ASDF will automatically find the project via source-registry.conf

;; Load the system
(ql:quickload :asteroid)

;; Define the main function for the executable
(defun main ()
  (asteroid:-main))

;; Save the executable
(sb-ext:save-lisp-and-die "asteroid" 
                          :toplevel #'main
                          :executable t
                          :compression 22)
