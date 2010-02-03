(in-package :cl-matlab)

;; There are two versions of some functions in the lib files. One with _700 and
;; one with _730. I'm assuming we want the latest versions but change this if necessary.
(defvar *matlab-version* :matlab-730
  "Change this to :matlab-700 to link to the _700 functions in the libs.")
