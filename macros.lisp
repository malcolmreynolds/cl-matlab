(in-package :cl-matlab)

#|
(defun split-string-at-caps (string)
  "Splits a string at the capitals so mxGetIr
   becomes (\"mx\" \"get\" \"ir\")"
  (let (words
	(all-lower (string-downcase mlab-name))
	(all-upper (string-upcase mlab-name))
	(last-capital 0))
    (dotimes (x (length mlab-name))
      ;; if we find a capital letter
      (when (eq (elt mlab-name x)
		(elt all-upper x))
	(push (subseq all-lower last-capital x) words)
	(setf last-capital x)))
    ;; last word
    (push (subseq all-lower last-capital) words)
    (nreverse words)))

;; TODO: make this a bit less shitty. surely possible to split with cl-ppcre?
(defun matlab-to-lisp-name (mlab-name)
  "Converts a matlab C name like mxSetIr to lispy
   name like mx-set-ir."
  (let ((split (split-string-at-caps mlab-name)))
    (if (equal (first split) "mx")
	(make-symbol-with-hyphens (rest split))
	(make-symbol-with-hyphens split))))

(defun make-symbol-with-hypens (strings)
  "Converts a list of strings to a symbol of them interleaved with hyphens."
  )

|#

;; 99% of functions have their first argument as an array-pointer to an mx-array-p so
;; encapsulate this here rather than writing it out every time
(defmacro defcfun-mlab (name return-type docstring &body additional-args)
  `(defcfun ,name ,return-type ,docstring (array-pointer mx-array-p) ,@additional-args))

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
	 (new-name (cons (concatenate 'string func-base-name suffix)
			 (rest name))))
    ;; new-name now is either funcName_700 or funcName_730
    `(defcfun-mlab ,new-name ,return-type ,@body)))