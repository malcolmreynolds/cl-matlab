;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cl-matlab.asd - ASDF system definition for matlab bindings

(asdf:defsystem cl-matlab
  :description "MATLAB engine bindings for Common Lisp using CFFI."
  :author "Malcolm Reynolds <malcolm.reynolds@gmail.com>"
  :version "0.0.1"
  :licence "BSD"
  :depends-on (:cffi)
  :components ((:file "package")
               (:file "globals" :depends-on ("package"))
               (:file "macros" :depends-on ("package" "globals"))
               (:file "matlab-bindings" :depends-on ("package" "macros" "globals"))
               ))