(in-package :h5cl)

(defgeneric hdf5-ref (hdf5-object item-spec &rest more-item-specs)
  (:documentation "Access items inside a HDF5 object"))

(defmethod hdf5-ref ((location hdf5-location) path &rest more-item-specs)
    (let* ((location-id (hdf5-object-id location))
           (object-id (hdf5:h5oopen location-id path hdf5:+H5P-DEFAULT+)))
      (unless (< 0 (hdf5:h5iis-valid object-id))
        (error "Path could not be opened ~s." path))
      (let ((object
             (case (hdf5:h5iget-type object-id)
               (:H5I-GROUP
                (make-instance 'hdf5-group
                               :id object-id
                               :file (hdf5-containing-file location)
                               :name (get-name-for-id object-id)))
               (:H5I-DATASET
                (make-instance 'hdf5-dataset
                               :id object-id
                               :file (hdf5-containing-file location)
                               :name (get-name-for-id object-id)))
               (:H5I-DATATYPE
                (make-instance 'hdf5-datatype
                               :id object-id
                               :file (hdf5-containing-file location)
                               :name (get-name-for-id object-id)))
               (t (error "Not a group, dataset, or datatype: ~s" path)))))
        (trivial-garbage:finalize object
                                  #'(lambda ()
                                      (close-id object-id)))
        object)))
