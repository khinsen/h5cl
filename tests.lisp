(in-package :h5cl-test)

(def-suite :h5cl)
(in-suite :h5cl)

(defun make-temporary-file-name ()
  (uiop:with-temporary-file (:pathname pn :prefix "h5cl-test" :type "h5")
    pn))

(defmacro with-temporary-filename (symbol &body body)
  `(let ((,symbol (make-temporary-file-name)))
     (unwind-protect
          ,@body
       (uiop:delete-file-if-exists ,symbol))))

(test create-new-file
  ;; Use open-hdf5
  (finishes
    (with-temporary-filename fn
      (let ((h5file (open-hdf5 fn :direction :io)))
        (close-hdf5 h5file))))
  ;; Use the non-exported low-level function h5create
  (finishes
    (with-temporary-filename fn
      (let ((h5file (h5cl::h5create (namestring fn))))
        (close-hdf5 h5file))))
  ;; TODO add tests for failures
  )

(test open-existing-file
  (signals simple-error
    (with-temporary-filename fn
      (let ((h5file (open-hdf5 fn :direction :input)))
        (close-hdf5 h5file))))
  (finishes
    (with-temporary-filename fn
      (let ((h5file (open-hdf5 fn :direction :io)))
        (close-hdf5 h5file)
        (let ((h5file (open-hdf5 fn :direction :input)))
          (close-hdf5 h5file))))))
