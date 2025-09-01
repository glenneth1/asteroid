#!/usr/local/bin/sbcl --script 
;; -*-lisp-*-

(load "~/quicklisp/setup.lisp")

;; Build script for creating asteroid executable using save-lisp-and-die
(require :asdf)
;; Add project directory to ASDF
;; (push #P"/home/fade/SourceCode/lisp/asteroid/" asdf:*central-registry*)

;; Load the system
(ql:quickload "asteroid")
;; (asdf:load-system :asteroid)

;; Define the main function for the executable
(defun main ()
  (asteroid:-main))

;; Save the executable
(sb-ext:save-lisp-and-die "asteroid" 
                          :toplevel #'main
                          :executable t
                          :compression 22)
