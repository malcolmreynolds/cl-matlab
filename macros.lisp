(in-package :cl-matlab)

;; This is to work around the crazy ass matlab libraries


(defmacro defcfun-mlab-vers (name return-type &body body)
  "For when the library contains _730 and _700 versions,
   for the API change between 730 and 700. Well, you know,
   I assume there was a point to why they did that. Anyway
   this means I can swap the value of *matlab-version* to
   bind to either the _730 or _700 version."
  (let* ((func-base-name (first name))
	 (suffix (ecase *matlab-version*
		   (:matlab-730 "_730")
		   (:matlab-700 "_700")))
	 (new-name (list (concatenate 'string func-base-name suffix)
			 (second name))))
    ;; new-name now is either funcName_700 or funcName_730
    `(defcfun ,new-name ,return-type ,@body)))