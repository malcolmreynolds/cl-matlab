(in-package :cl-matlab)
#|
  TODO: the functions which return data are listed as returning doubles, but
  we need abstractions 
|#



;;; Import the libraries
(define-foreign-library matlab-engine
  (:darwin "libeng.dylib"))

(define-foreign-library matlab-mx
  (:darwin "libmx.dylib"))

(use-foreign-library matlab-engine)
(use-foreign-library matlab-mx)



;;; Types
(defctype array-p :pointer
  "MATLAB mxArray pointer. 'Defined' in matrix.h")

(defctype engine-p :pointer
  "MATLAB Engine pointer. 'Defined' in engine.h")

(defctype logical-t :boolean
  "MATLAB Logical type")

(defctype char-t :short
  "Unicode - 16 bit characters")

;; FIXME this stuff wants to be changed for 64 bit!!!
(defctype size-t :unsigned-int
  "Size type - 32 bit for now.")
(defctype ptrdiff-t :int
  "Pointer offset type")

(defctype mw-size size-t
  "'Unsigned pointer-width integer' according to tmwtypes.h")
(defctype mw-index size-t
  "'Unsigned pointer-width integer' according to tmwtypes.h")
(defctype ptrdiff-t ptrdiff-t
  "'Signed pointer-width integer' according to tmwtypes.h")


;; Enums
(defcenum class-id
  "Enumeration corresponding to all the valid MATLAB mxArray types"
  (:unknown-class 0)
  :cell-class
  :struct-class
  :logical-class
  :char-class
  :void-class
  :double-class
  :single-class
  :int8-class
  :uint8-class
  :int16-class
  :uint16-class
  :int32-class
  :uint32-class
  :int64-class
  :uint64-class
  :function-class
  :opaque-class
  :object-class
  ;;FIXME this should have some stuff round it to use uint64-class if
  ;; we are on a 64 bit system
  (:index-class 13)
  ;; temporary and nasty hack (according to matrix.h)
  ;;.. so I'm going to comment this out for now
  ;;(sparse-class void-class)
  )

(defcenum complexity
  "Indicates whether floating-point mxArrays are real or complex"
  :real
  :complex)



;;; Functions from engine.h
(defcfun ("engOpen" eng-open) engine-p
  "Opens a connection to the MATLAB engine."
  (startcmd :string))

(defcfun ("engClose" eng-close) :int
  "Closes the connection to the MATLAB engine."
  (ep engine-p))

(defcfun ("engEvalString" eng-eval-string) :int
  "Evaluate a string in the MATLAB engine."
  (ep engine-p)
  (string :string))

(defcfun ("engGetVariable" eng-get-variable) array-p
  "Get a variable from the MATLAB engine."
  (ep engine-p)
  (name :string))

(defcfun ("engPutVariable" eng-put-variable) :int
  "Put a variable into MATLAB's workspace with the specified name."
  (ep engine-p)
  (var-name :string)
  (ap array-p))

;;; Functions from matrix.h
(defcfun ("mxMalloc" malloc) (:pointer :void)
  "Allocate memory, notifying registered listener."
  (n size-t))

(defcfun ("mxCalloc" calloc) (:pointer :void)
  "Allocate cleared memory, notifying registered listener."
  (n size-t) ;; number of objects
  (size size-t)) ;; size of objects

(defcfun ("mxFree" free) :void
  "Free memory, notifying registered listener."
  (ptr (:pointer :void)))

(defcfun ("mxRealloc" realloc) (:pointer :void)
  "Reallocate memory, notifying registered listener."
  (ptr (:pointer :void))
  (size size-t))

(defcfun ("mxGetClassID" get-class-id) class-id
  "Returns the class (category) of data that the array holds."
  (pa array-p))

(defcfun ("mxGetData" get-data) (:pointer :void)
  "Get pointer to data."
  (pa array-p))

(defcfun ("mxSetData" set-data) :void
  "Set pointer to data."
  (pa array-p)
  (newdata (:pointer :void)))

(defcfun ("mxIsNumeric" is-numeric) :boolean
  "Determine whether the specified array contains numeric
   (as opposed to cell or struct) data."
  (pa array-p))

(defcfun ("mxIsCell" is-cell) :boolean
  "Determine whether the given array is a cell array."
  (pa array-p))

(defcfun ("mxIsLogical" is-logical) :boolean
  "Determine whether the given array's logical flag is on."
  (pa array-p))

(defcfun ("mxIsChar" is-char) :boolean
  "Determine whether the given array contains character data."
  (pa array-p))

(defcfun ("mxIsStruct" is-struct) :boolean
  "Determine whether the given array is a structure array."
  (pa array-p))

(defcfun ("mxIsOpaque" is-opaque) :boolean
  "Determine whether the given array is an opaque array."
  (pa array-p))

(defcfun ("mxIsFunctionHandle" is-function-handle) :boolean
  "Returns true if specified array is a function object."
  (pa array-p))

;; There is a function mxIsObject but that's deprecated so I'm not going to support
;; it right now.

(defcfun ("mxGetImagData" get-imag-data) (:pointer :void)
  "Get imaginary Data pointer for numeric array."
  (pa array-p))

(defcfun ("mxSetImagData" set-imag-data) :void
  "Set imaginary data pointer for numeric array."
  (pa array-p)
  (newdata (:pointer :void)))

(defcfun ("mxIsComplex" is-complex) :boolean
  "Determine whether the given array contains complex values."
  (pa array-p))

(defcfun ("mxIsSparse" is-sparse) :boolean
  "Determine whether the given array is sparse (as opposed to full)."
  (pa array-p))

(defcfun ("mxIsDouble" is-double) :boolean
  "Determine whether the data in the array contains double values."
  (pa array-p))

(defcfun ("mxIsSingle" is-single) :boolean
  "Determine whether the data in the array contains single values."
  (pa array-p))

(defcfun ("mxIsInt8" is-int8) :boolean
  "Determine whether the data in the array contains signed 8-bit values."
  (pa array-p))

(defcfun ("mxIsUint8" is-uint8) :boolean
  "Determine whether the data in the array contains unsigned 8-bit values."
  (pa array-p))

(defcfun ("mxIsInt16" is-int16) :boolean
  "Determine whether the data in the array contains signed 16-bit values."
  (pa array-p))

(defcfun ("mxIsUint16" is-uint16) :boolean
  "Determine whether the data in the array contains unsigned 16-bit values."
  (pa array-p))

(defcfun ("mxIsInt32" is-int32) :boolean
  "Determine whether the data in the array contains signed 32-bit values."
  (pa array-p))

(defcfun ("mxIsUint32" is-uint32) :boolean
  "Determine whether the data in the array contains unsigned 32-bit values."
  (pa array-p))

(defcfun ("mxIsInt64" is-int64) :boolean
  "Determine whether the data in the array contains signed 64-bit values."
  (pa array-p))

(defcfun ("mxIsUint64" is-uint64) :boolean
  "Determine whether the data in the array contains unsigned 64-bit values."
  (pa array-p))

(defcfun-mlab-vers ("mxGetNumberOfDimensions" get-num-dimensions) mw-size
  "Get number of dimensions in array."
  (pa array-p))

(defcfun-mlab-vers ("mxGetDimensions" get-dimensions) (:pointer mw-size)
  "Get pointer to dimension array."
  (pa array-p))

(defcfun ("mxGetNumberOfElements" get-num-elements) size-t
  "Get number of elements in array."
  (pa array-p))

(defcfun ("mxGetPr" get-pr) (:pointer :double)
  "Get real data pointer for numeric array."
  (pa array-p))

(defcfun ("mxSetPr" set-pr) :void
  "Set real data pointer for numeric array."
  (pa array-p)
  (pr (:pointer :double)))

(defcfun ("mxGetPi" get-pi) (:pointer :double)
  "Get imaginary data pointer for numeric array."
  (pa array-p))

(defcfun ("mxSetPi" set-pi) :void
  "Set imaginary data pointer for numeric array."
  (pa array-p)
  (p-imag (:pointer :double)))

(defcfun ("mxGetChars" get-chars) (:pointer char-t)
  "Get string array data."
  (pa array-p))

(defcfun ("mxGetUserBits" get-user-bits) :int
  "Get 8 bits of user data stored in mxArray header. State of these
   bits is not guaranteed to be preserved after API function calls."
  (pa array-p))

(defcfun ("mxSetUserBits" set-user-bits) :void
  "Set 8 bits of user data stored in mxArray header. State of these
   bits is not guaranteed to be preserved after API function calls."
  (pa array-p)
  (value :int))

(defcfun ("mxGetScalar" get-scalar) :double
  "Get the real component of the specified array's first data element."
  (pa array-p))

(defcfun ("mxIsFromGlobalWS" is-from-global-ws) :boolean
  "Is the isFromGlobalWorkspace bit set?"
  (pa array-p))

(defcfun ("mxSetFromGlobalWS" set-from-global-ws) :void
  "Set the isFromGlobalWS bit."
  (pa array-p)
  (global :boolean))

(defcfun ("mxGetM" get-row-dim) size-t
  "Get row dimension."
  (pa array-p))

(defcfun ("mxSetM_730" set-row-dim) :void
  "Set row dimension."
  (pa array-p)
  (m mw-size))

(defcfun ("mxGetN" get-col-dim) size-t
  "Get column dimension."
  (pa array-p))

(defcfun-mlab-vers ("mxSetN" set-col-dim) size-t
  "Set column dimension."
  (pa array-p))

(defcfun ("mxIsEmpty" is-empty) :boolean
  "Is array empty?"
  (pa array-p))

(defcfun-mlab-vers ("mxGetIr" get-ir) mw-index
  "Get row data pointer for sparse numeric array."
  (pa array-p))

