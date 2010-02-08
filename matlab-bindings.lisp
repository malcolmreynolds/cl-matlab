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
(defctype mx-array-p :pointer "MATLAB mxArray pointer. 'Defined' in matrix.h")
(defctype engine-p :pointer "MATLAB Engine pointer. 'Defined' in engine.h")
(defctype logical-t :boolean "MATLAB Logical type")
(defctype char-t :short "Unicode - 16 bit characters")

;; FIXME this stuff wants to be changed for 64 bit!!!
(defctype size-t :unsigned-int "Size type - 32 bit for now.")
(defctype ptrdiff-t :int "Pointer offset type")

(defctype mw-size size-t "'Unsigned pointer-width integer' according to tmwtypes.h")
(defctype mw-index size-t "'Unsigned pointer-width integer' according to tmwtypes.h")
(defctype ptrdiff-t ptrdiff-t "'Signed pointer-width integer' according to tmwtypes.h")

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
  (engine-pointer engine-p))

(defcfun ("engEvalString" eng-eval-string) :int
  "Evaluate a string in the MATLAB engine."
  (engine-pointer engine-p)
  (string-to-eval :string))

(defcfun ("engGetVariable" eng-get-variable) mx-array-p
  "Get a variable from the MATLAB engine."
  (engine-pointer engine-p)
  (variable-name :string))

(defcfun ("engPutVariable" eng-put-variable) :int
  "Put a variable into MATLAB's workspace with the specified name."
  (engine-pointer engine-p)
  (variable-name :string)
  (array-pointer mx-array-p))

;;; Functions from matrix.h
(defcfun ("mxMalloc" malloc) (:pointer :void)
  "Allocate memory, notifying registered listener."
  (num-bytes size-t))

(defcfun ("mxCalloc" calloc) (:pointer :void)
  "Allocate cleared memory, notifying registered listener."
  (num-objects size-t) ;; number of objects
  (object-size size-t)) ;; size of objects

(defcfun ("mxFree" free) :void
  "Free memory, notifying registered listener."
  (pointer (:pointer :void)))

(defcfun ("mxRealloc" realloc) (:pointer :void)
  "Reallocate memory, notifying registered listener."
  (pointer (:pointer :void))
  (new-memory-size size-t))

(defcfun-mlab ("mxGetClassID" get-class-id) class-id
    "Returns the class (category) of data that the array holds.")

(defcfun-mlab ("mxGetData" get-data) (:pointer :void)
    "Get pointer to data.")

(defcfun-mlab ("mxSetData" set-data) :void
  "Set pointer to data."
  (new-data-pointer (:pointer :void)))

(defcfun-mlab ("mxIsNumeric" is-numeric) :boolean
  "Determine whether the specified array contains numeric
   (as opposed to cell or struct) data.")

(defcfun-mlab ("mxIsCell" is-cell) :boolean
  "Determine whether the given array is a cell array.")

(defcfun-mlab ("mxIsLogical" is-logical) :boolean
  "Determine whether the given array's logical flag is on.")

(defcfun-mlab ("mxIsChar" is-char) :boolean
  "Determine whether the given array contains character data.")

(defcfun-mlab ("mxIsStruct" is-struct) :boolean
  "Determine whether the given array is a structure array.")

(defcfun-mlab ("mxIsOpaque" is-opaque) :boolean
  "Determine whether the given array is an opaque array.")

(defcfun-mlab ("mxIsFunctionHandle" is-function-handle) :boolean
  "Returns true if specified array is a function object.")

;; There is a function mxIsObject but that's deprecated so I'm not going to support
;; it right now.

(defcfun-mlab ("mxGetImagData" get-imag-data) (:pointer :void)
  "Get imaginary Data pointer for numeric array.")

(defcfun-mlab ("mxSetImagData" set-imag-data) :void
  "Set imaginary data pointer for numeric array."
  (new-imag-data-pointer (:pointer :void)))

(defcfun-mlab ("mxIsComplex" is-complex) :boolean
  "Determine whether the given array contains complex values.")

(defcfun-mlab ("mxIsSparse" is-sparse) :boolean
  "Determine whether the given array is sparse (as opposed to full).")

(defcfun-mlab ("mxIsDouble" is-double) :boolean
  "Determine whether the data in the array contains double values.")

(defcfun-mlab ("mxIsSingle" is-single) :boolean
  "Determine whether the data in the array contains single values.")

(defcfun-mlab ("mxIsInt8" is-int8) :boolean
  "Determine whether the data in the array contains signed 8-bit values.")

(defcfun-mlab ("mxIsUint8" is-uint8) :boolean
  "Determine whether the data in the array contains unsigned 8-bit values.")

(defcfun-mlab ("mxIsInt16" is-int16) :boolean
  "Determine whether the data in the array contains signed 16-bit values.")

(defcfun-mlab ("mxIsUint16" is-uint16) :boolean
  "Determine whether the data in the array contains unsigned 16-bit values.")

(defcfun-mlab ("mxIsInt32" is-int32) :boolean
  "Determine whether the data in the array contains signed 32-bit values.")

(defcfun-mlab ("mxIsUint32" is-uint32) :boolean
  "Determine whether the data in the array contains unsigned 32-bit values.")

(defcfun-mlab ("mxIsInt64" is-int64) :boolean
  "Determine whether the data in the array contains signed 64-bit values.")

(defcfun-mlab ("mxIsUint64" is-uint64) :boolean
  "Determine whether the data in the array contains unsigned 64-bit values.")

(defcfun-mlab-vers ("mxGetNumberOfDimensions" get-num-dimensions) mw-size
  "Get number of dimensions in array.")

(defcfun-mlab-vers ("mxGetDimensions" get-dimensions) (:pointer mw-size)
  "Get pointer to dimension array.")

(defcfun-mlab ("mxGetNumberOfElements" get-num-elements) size-t
  "Get number of elements in array.")

(defcfun-mlab ("mxGetPr" get-pr) (:pointer :double)
  "Get real data pointer for numeric array.")

(defcfun-mlab ("mxSetPr" set-pr) :void
  "Set real data pointer for numeric array."
  (new-real-data-pointer (:pointer :double)))

(defcfun-mlab ("mxGetPi" get-pi) (:pointer :double)
  "Get imaginary data pointer for numeric array.")

(defcfun-mlab ("mxSetPi" set-pi) :void
  "Set imaginary data pointer for numeric array."
  (new-image-data-pointer (:pointer :double)))

(defcfun-mlab ("mxGetChars" get-chars) (:pointer char-t)
  "Get string array data.")

(defcfun-mlab ("mxGetUserBits" get-user-bits) :int
  "Get 8 bits of user data stored in mxArray header. State of these
   bits is not guaranteed to be preserved after API function calls.")

(defcfun-mlab ("mxSetUserBits" set-user-bits) :void
  "Set 8 bits of user data stored in mxArray header. State of these
   bits is not guaranteed to be preserved after API function calls."
  (new-user-bits :int))

(defcfun-mlab ("mxGetScalar" get-scalar) :double
  "Get the real component of the specified array's first data element.")

(defcfun-mlab ("mxIsFromGlobalWS" is-from-global-ws) :boolean
  "Is the isFromGlobalWorkspace bit set?")

(defcfun-mlab ("mxSetFromGlobalWS" set-from-global-ws) :void
  "Set the isFromGlobalWS bit."
  (new-is-global-value :boolean))

(defcfun-mlab ("mxGetM" get-row-dim) size-t
  "Get row dimension.")

(defcfun-mlab ("mxSetM_730" set-row-dim) :void
  "Set row dimension."
  (new-row-dimension mw-size))

(defcfun-mlab ("mxGetN" get-col-dim) size-t
  "Get column dimension.")

(defcfun-mlab-vers ("mxSetN" set-col-dim) size-t
  "Set column dimension."
  (new-column-dimension mw-size))

(defcfun-mlab ("mxIsEmpty" is-empty) :boolean
  "Is array empty?")

(defcfun-mlab-vers ("mxGetIr" get-ir) mw-index
  "Get row data pointer for sparse numeric array.")

(defcfun-mlab ("mxSetIr_730" set-ir) :void
  "Set row data pointer for sparse numeric array."
  (new-row-data-pointer (:pointer mw-index)))

(defcfun-mlab ("mxGetJc_730" get-jc) (:pointer mw-index)
  "Get column data pointer for sparse numeric array.")

(defcfun-mlab ("mxSetJc_730" set-jc) :void
  "Set column data pointer for sparse numeric array."
  (new-column-data-pointer (:pointer mw-index)))

(defcfun-mlab ("mxGetNzmax_730" get-nz-max) mw-size
  "Get maximum nonzero elements for sparse numeric array.")

(defcfun-mlab ("mxSetNzmax_730" set-nz-max) :void
  "Set maximum nonzero elements for numeric array."
  (new-nonzero-max mw-size))

(defcfun-mlab ("mxGetElementSize" get-element-size) size-t
  "Get array data element size.")

(defcfun-mlab-vers ("mxCalcSingleSubscript" calc-single-subscript) mw-index
  "Return the offset (in number of elements from the
   beginning of the array to a given subscript"
  (num-subscripts mw-size)
  (subscripts-pointer (:pointer mw-size)))

(defcfun-mlab ("mxGetNumberOfFields" get-number-of-fields) :int
    "Get number of structure fields in array.")

(defcfun-mlab-vers ("mxGetCell" get-cell) mx-array-p
    "Get a pointer to the specified cell element."
  (index mw-index))

(defcfun-mlab-vers ("mxSetCell" set-cell) mx-array-p
    "Set an element in a cell array to the specified value."
  (index mw-index)
  (new-value mx-array-p))

(defcfun-mlab ("mxGetFieldNameByNumber" get-field-name-by-number) (:pointer :char)
    "Return pointer to the nth field name."
  (n :int))

(defcfun-mlab ("mxGetFieldNumber" get-field-number) :int
    "Get the index to the named field."
  (field-name :string))

(defcfun-mlab-vers ("mxGetFieldByNumber" get-field-by-number) mx-array-p
    "Return a pointer to the contents of the named field for the
     ith element (zero based)."
  (index mw-index)
  (field-number :int))

(defcfun-mlab-vers ("mxSetFieldByNumber" set-field-by-number) :void
    "Set pa[i][fieldnum] = value."
  (index mw-index)
  (field-number :int)
  (new-value mx-array-p))

(defcfun-mlab-vers ("mxGetField" get-field) mx-array-p
    "Return a pointer to the contents of the named field for the i'th
     element (Zero based). Returns NULL on no such field or if the
     field itself is NULL."
  (index mw-index)
  (field-name :string))

(defcfun-mlab-vers ("mxSetField" set-field) :void
    "Sets the contents of the named field for the i'th element (zero based).
     The input 'value' is stored in the input array 'pa' - no copy is made."
  (index mw-index)
  (field-name :string)
  (new-value mx-array-p))

(defcfun-mlab-vers ("mxGetProperty" get-property) mx-array-p
  "Returns the value of a property for a given object and index. The property
   must be public. If the given property name doesn't exist, isn't public, or the object
   isn't the right type, then mxGetProperty returns NULL."
  (index mw-index)
  (property-name :string))

(defcfun-mlab-vers ("mxSetProperty" set-property) :void
  "Sets the value of a proerty for a given object and index. The property must
   be public."
  (index mw-index)
  (property-name :string)
  (new-value mx-array-p))

(defcfun-mlab ("mxIsClass" is-class) :boolean
    "Determine whether an array is a member of the specified class."
  (class-name :string))

;;; Now we are onto stuff that doesn't necessarily take a mx-array-p as the first argument,
;;; so we stop using defcfun-mlab

(defcfun ("mxCreateNumericMatrix" create-numeric-matrix) mx-array-p
    "Create a numeric matrix and initialise all the data elements to 0."
  (num-rows mw-size)
  (num-cols mw-size)
  (class-id class-id)
  (real-or-complex-flag complexity))

(defcfun-mlab ("mxSetDimensions" set-dimensions) :int
    "Set dimension array and number of dimensions. Returns 0 on success and 1
     if there was not enough memory available to reallocate the dimensions array."
  (new-size-array (:pointer mw-size))
  (new-num-dimensions mw-size))

(defcfun-mlab ("mxDestroyArray" destroy-array) :void
    "mxArray Destructor")

(defcfun ("mxCreateNumericArray" create-numeric-array) mx-array-p
  "Create a numeric array and initialize all the data elements to 0.

   Returns NULL when not enough memory is available."
  (num-dimensions mw-size)
  (dimensions-array (:pointer mw-size))
  (class-id class-id)
  (real-or-complex-flag complexity))

(defcfun ("mxCreateCharArray" create-char-array) mx-array-p
  "Create an n-dimensional array to hold string data. Initialise
   all elements to zero."
  (num-dimensions mw-size)
  (dimensions-array (:pointer mw-size)))