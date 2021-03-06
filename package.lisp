(defpackage :h5cl
  (:use :cl)
  (:export :open-hdf5
           :close-hdf5
           :hdf5-ref
           :hdf5-containing-file
           :hdf5-path
           :make-hdf5-group
           :make-hdf5-dataset
           :hdf5-dataset-element-type
           :hdf5-dataset-rank
           :hdf5-dataset-dimensions))
