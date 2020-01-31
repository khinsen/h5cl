(in-package :h5cl)

(defclass hdf5-group (hdf5-named-object hdf5-location)
  ())

;;
;; Create a new group
;;

(defun make-hdf5-group (location path)
  (let* ((location-id (hdf5-object-id location))
         (group-id (hdf5:h5gcreate2 location-id
                                    path
                                    hdf5:+H5P-DEFAULT+
                                    hdf5:+H5P-DEFAULT+
                                    hdf5:+H5P-DEFAULT+)))
    (unless (< 0 (hdf5:h5iis-valid group-id))
      (error "Group ~s could not be created in ~s." path location))
    (close-id group-id))
  (hdf5-ref location path))
