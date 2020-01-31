(defpackage :h5cl
  (:use :cl)
  (:export :open-hdf5
           :close-hdf5
           :hdf5-ref
           :make-hdf5-group
           :make-hdf5-dataset))
