(in-package :h5cl)

(defclass hdf5-dataset (hdf5-named-object)
  ())

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
  (let ((location-id (hdf5-object-id location))
        (dspace (create-dataspace dimensions max-dimensions)))
    (unwind-protect
         (hdf5:h5dcreate2 location-id path element-type dspace
                          hdf5:+H5P-DEFAULT+
                          hdf5:+H5P-DEFAULT+
                          hdf5:+H5P-DEFAULT+)
      (close-id dspace))
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
    (error "DIMENSIONS and MAX-DIMENSIONS mus tbe lists."))
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
