(in-package :h5cl)

(defclass hdf5-dataset (hdf5-named-object)
  ())

(defun hdf5-dataset-type (dataset)
  (let* ((h5type (hdf5:h5dget-type (hdf5-object-id dataset)))
         (h5class (hdf5:h5tget-class h5type))
         (h5native (hdf5:h5tget-native-type h5type :H5T-DIR-DEFAULT)))
    (close-id h5type)
    (list h5class h5native)))
