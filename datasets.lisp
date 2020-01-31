(in-package :h5cl)

(defclass hdf5-dataset (hdf5-named-object)
  ())

;;
;; Return information about datasets
;;

(defun hdf5-dataset-element-type (dataset)
  "Return a type specifier for the elements of DATASET. For non-numeric
types, the return value is not a valid Common Lisp type specifier."
  (let* ((datatype-id (hdf5:h5dget-type (hdf5-object-id dataset)))
         (native-type-id (hdf5:h5tget-native-type datatype-id :H5T-DIR-DEFAULT)))
    (unless (and (< 0 (hdf5:h5iis-valid datatype-id))
                 (< 0 (hdf5:h5iis-valid native-type-id)))
      (error "Invalid datatype id"))
    (close-id datatype-id)
    (let ((type
           (let ((class (hdf5:h5tget-class native-type-id))
                 (precision (hdf5:h5tget-precision native-type-id))
                 (sign (hdf5:h5tget-sign native-type-id)))
             (case class
               (:H5T-INTEGER (case sign
                               (:H5T-SGN-NONE (list 'unsigned-byte precision))
                               (:H5T-SGN-2 (list 'signed-byte precision))
                               (otherwise (error "Unknown sign type: ~s" sign))) )
               (:H5T-FLOAT (case precision
                             (32 'single-float)
                             (64 'double-float)
                             (otherwise (error "Unknown float precision: ~s" precision))))
               ;; Note: this is not a valid Common Lisp type specifier!
               (otherwise (list 'hdf5-type class precision))))))
      (close-id native-type-id)
      type)))

(defun hdf5-dataset-rank (dataset)
  (let ((dataspace-id (hdf5:h5dget-space (hdf5-object-id dataset))))
    (unless (< 0 (hdf5:h5iis-valid dataspace-id))
      (error "Invalid dataspace id"))
    (let ((rank (hdf5:h5sget-simple-extent-ndims dataspace-id)))
      (close-id dataspace-id)
      rank)))

(defun hdf5-dataset-dimensions (dataset)
  (let ((dataspace-id (hdf5:h5dget-space (hdf5-object-id dataset))))
    (unless (< 0 (hdf5:h5iis-valid dataspace-id))
      (error "Invalid dataspace id"))
    (let ((rank (hdf5:h5sget-simple-extent-ndims dataspace-id))
          dimensions max-dimensions)
      (cffi:with-foreign-objects ((dims 'hdf5:hsize-t rank)
                                  (maxdims 'hdf5:hsize-t rank))
        (hdf5:h5sget-simple-extent-dims dataspace-id dims maxdims)
        (setf dimensions
              (loop for i below rank
                    collect (cffi:mem-aref dims 'hdf5:hsize-t i)))
        (setf max-dimensions
              (loop for i below rank
                    collect (cffi:mem-aref maxdims 'hdf5:hsize-t i))))
      (close-id dataspace-id)
      (values dimensions max-dimensions))))

;;
;; Create datasets
;;
(defun create-dataspace (dimensions &optional max-dimensions)
  (let* ((rank (list-length dimensions))
         (dimensions-ptr (cffi:foreign-alloc 'hdf5:hsize-t :count rank
                                             :initial-contents dimensions))
         (max-dimensions-ptr (if max-dimensions
                                 (cffi:foreign-alloc 'hdf5:hsize-t :count rank
                                                     :initial-contents max-dimensions)
                                 hdf5:+NULL+))
         (space (hdf5:h5screate-simple rank dimensions-ptr max-dimensions-ptr)))
    (cffi:foreign-free dimensions-ptr)
    (when max-dimensions (cffi:foreign-free max-dimensions-ptr))
    space))

(defun create-dataset (location path dimensions max-dimensions element-type)
  (let* ((location-id (hdf5-object-id location))
         (dspace-id (create-dataspace dimensions max-dimensions)))
    (unless (< 0 (hdf5:h5iis-valid dspace-id))
      (error "Invalid dataspace id"))
    (let ((dataset-id
           (hdf5:h5dcreate2 location-id path element-type dspace-id
                            hdf5:+H5P-DEFAULT+
                            hdf5:+H5P-DEFAULT+
                            hdf5:+H5P-DEFAULT+)))
      (close-id dspace-id)
      (unless (< 0 (hdf5:h5iis-valid dataset-id))
        (error "Invalid dataset id")))
    (hdf5-ref location path)))

(defun write-dataset (location path dimensions max-dimensions element-type data)
  (let* ((cffi-type (lisp-type->cffi-type element-type))
         (hdf5-type (cffi-type->hdf5-type cffi-type))
         (size (apply #'* dimensions))
         (dataset (create-dataset location path dimensions max-dimensions hdf5-type)))
    (cffi:with-foreign-object (f-array cffi-type size)
      (if (arrayp data)
          (dotimes (i size)
            (setf (cffi:mem-aref f-array cffi-type i) (row-major-aref data i)))
          (dotimes (i size)
            (setf (cffi:mem-aref f-array cffi-type i) data)))
      (hdf5:h5dwrite (hdf5-object-id dataset)
                     hdf5-type
                     hdf5:+H5S-ALL+ hdf5:+H5S-ALL+ hdf5:+H5P-DEFAULT+
                     f-array))
    dataset))

(defun make-hdf5-dataset (location path dimensions
                          &key element-type
                            initial-element
                            initial-contents
                            max-dimensions)
  ;; Check initial contents
  (when (and initial-element initial-contents)
    (error "Can't specify both :INITIAL-ELEMENT and :INITIAL-CONTENTS"))

  ;; Check dimensions and max-dimensions
  (unless (and (consp dimensions) (listp max-dimensions))
    (error "DIMENSIONS and MAX-DIMENSIONS must tbe lists."))
  (when (and max-dimensions
             (not (= (list-length dimensions) (list-length max-dimensions))))
    (error "MAX-DIMENSIONS must have the same length as DIMENSIONS."))
  (when (or (< (list-length dimensions) 1)
            (> (list-length dimensions) 32))
    (error "The length of DIMENSIONS must be between 1 and 32."))
  (when (some #'(lambda (x) (or (not (integerp x)) (< x 1))) dimensions)
    (error "The elements of DIMENSIONS must be positive integers."))
  (when (and max-dimensions
             (some #'(lambda (x) (and x (or (not (integerp x)) (< x 1))))
                   max-dimensions))
    (error "The elements of MAX-DIMENSIONS must be positive integers or NIL."))
  (when (and max-dimensions
             (some #'null
                   (mapcar #'(lambda (x y) (or (null y) (<= x y)))
                           dimensions max-dimensions)))
    (error "A dimension must not exceed it's corresponding maximum."))
  (when (and max-dimensions
             (some #'identity
                   (mapcar #'(lambda (x y) (and y (> x y)))
                           dimensions max-dimensions)))
    (error "A dimension must not exceed it's corresponding maximum."))

  ;; Check element type
  (unless element-type
    (when initial-element
      (setf element-type (type-of initial-element)))
    (when (and initial-contents
               (arrayp initial-contents))
      (setf element-type (array-element-type initial-contents)))
    (unless element-type
      (error "Element type cannot be deduced from initial contents.")))

  ;; Convert initial contents to array if necessary
  (when (and initial-contents
             (not (arrayp initial-contents)))
    (setf initial-contents (make-array dimensions
                                       :element-type element-type
                                       :initial-contents initial-contents)))

  ;; Create the dataset and write the initial contents
  (write-dataset location path dimensions max-dimensions
                 element-type (or initial-element initial-contents)))
