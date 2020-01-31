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

(defmacro with-temporary-file (symbol &body body)
  (let ((fn-symbol (gensym)))
    `(with-temporary-filename ,fn-symbol
       (let ((,symbol (open-hdf5 ,fn-symbol :direction :io)))
         (unwind-protect
              ,@body
           (close-hdf5 ,symbol))))))

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

(test make-group
  (with-temporary-file h5file
    (let ((group (make-hdf5-group h5file "test")))
      (is (eq (hdf5-containing-file group) h5file))
      (is (equal (hdf5-path group) "/test"))
      (let ((subgroup (make-hdf5-group group "subtest")))
        (is (eq (hdf5-containing-file subgroup) h5file))
        (is (equal (hdf5-path subgroup) "/test/subtest"))))))

