(in-package :h5cl)

;;
;; Type management is bit messy because there are three levels to deal with:
;;  - Common Lisp types
;;  - C types (as defined by cffi)
;;  - HDF5 types (as defined by hdf5-cffi)
;;
;; For now, h5cl supports only data in the form of specialized arrays whose
;; element types are
;;  - short-float
;;  - double-float
;;  - integer subtypes other than bignum
;;
;; A note about floats: the Common Lisp standard does not guarantee
;; that short-float and double-float correspond to IEEE 754
;; single/double precision binary floats, but this is the case in all
;; of today's implementations I am aware of. With an implementation that
;; uses different representations, writing floats to HDF5 and then reading
;; them back will not yield identical values.
;;

(defun lisp-type->cffi-type (type-spec)
  "Find the corresponding cffi type for the elements of a specialized
array whose element-type is TYPE-SPEC. Supports floats and integers
execpt for bignums."
  (let ((el-type (upgraded-array-element-type type-spec)))
    (cond ((subtypep el-type 'single-float)       :float)
          ((subtypep el-type 'double-float)       :double)
          ((subtypep el-type '(unsigned-byte  8)) :uint8)
          ((subtypep el-type '(unsigned-byte 16)) :uint16)
          ((subtypep el-type '(unsigned-byte 32)) :uint32)
          ((subtypep el-type '(unsigned-byte 64)) :uint64)
          ((subtypep el-type '(signed-byte  8))   :int8)
          ((subtypep el-type '(signed-byte 16))   :int16)
          ((subtypep el-type '(signed-byte 32))   :int32)
          ((subtypep el-type '(signed-byte 64))   :int64)  
          (t (error "Type ~s not supported." el-type)))))

(defun cffi-type->hdf5-type (cffi-type)
  "Find the HDF5 type corresponding to CFFI-TYPE."
  (case cffi-type
    (:uint8 hdf5::+H5T-NATIVE-UINT8+)
    (:uint16 hdf5::+H5T-NATIVE-UINT16+)
    (:uint32 hdf5::+H5T-NATIVE-UINT32+)
    (:uint64 hdf5::+H5T-NATIVE-UINT64+)
    (:int8 hdf5::+H5T-NATIVE-INT8+)
    (:int16 hdf5::+H5T-NATIVE-INT16+)
    (:int32 hdf5::+H5T-NATIVE-INT32+)
    (:int64 hdf5::+H5T-NATIVE-INT64+)
    (:float hdf5:+H5T-NATIVE-FLOAT+)
    (:double hdf5:+H5T-NATIVE-DOUBLE+)))

;;
;; A class for named datatypes
;;

(defclass hdf5-datatype (hdf5-named-object)
  ())

