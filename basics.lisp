(in-package :h5cl)

;; A hdf5-object is anything that has an HDF5 id. This includes
;; files, datasets, groups, and more.

(defclass hdf5-object ()
  ((id
    :initarg :id
    :reader hdf5-object-id
    :documentation "C-level id (type hid_t)")))

;; All HDF5 objects must be closed after use, at least to free memory.

(defun close-id (id)
  "Close an HDF5 id"
  (when id
    (unless (< 0 (hdf5:h5iis-valid id))
      (error "Invalid id"))
    (let ((type (hdf5:h5iget-type id)))
      (cond ((eql type :H5I-ATTR) (hdf5:h5aclose id))
            ((eql type :H5I-DATASET) (hdf5:h5dclose id))
            ((eql type :H5I-DATASPACE) (hdf5:h5sclose id))
            ((eql type :H5I-DATATYPE) (hdf5:h5tclose id))
            ((eql type :H5I-FILE) (hdf5:h5fclose id))
            ((eql type :H5I-GENPROP-LST) (hdf5:h5pclose id))
            ((eql type :H5I-GROUP) (hdf5:h5gclose id))
            (t (error (format nil "Can't close id ~a" type)))))))

;; Some HDF5 obects refer to named entities within a file. This function
;; retrieves the name of such an object from its id.

(defun get-name-for-id (id)
  "Return the name corresponding to a HDF5 object id"
  (let* ((size (1+ (hdf5:h5iget-name id hdf5:+NULL+ 0)))
         (name (cffi:foreign-alloc :char :count size)))
    (prog2
        (hdf5:h5iget-name id name size)
        (cffi:foreign-string-to-lisp name)
      (cffi:foreign-free name))))

;;
;; A base class for the HDF5 objects accessible via
;; a path in a file: groups, datasets, and named datatypes.
;;

(defclass hdf5-named-object (hdf5-object)
  ((file
    :initarg :file
    :documentation "File in which the group resides")
   (name
    :initarg :name
    :documentation "Name of the object")))

(defmethod print-object ((object hdf5-named-object) out)
  (with-slots (file name) object
    (print-unreadable-object (object out :type t)
      (format out "~s in file ~s" name (hdf5-file-filename file)))))

;;
;; A base class for HDF5 locations: files and groups
;;

(defclass hdf5-location (hdf5-object) ())
