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

(defmacro with-temporary-file-write-then-read (symbol write read)
  (let ((fn-symbol (gensym)))
    `(with-temporary-filename ,fn-symbol
       (let ((,symbol (open-hdf5 ,fn-symbol :direction :io)))
         (unwind-protect
              ,write
           (close-hdf5 ,symbol)))
       (let ((,symbol (open-hdf5 ,fn-symbol :direction :input)))
         (unwind-protect
              ,read
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

(test make-dataset
  (with-temporary-file h5file
    (let* ((data (make-array '(2 2)
                             :element-type '(unsigned-byte 16)
                             :initial-contents '((1 2) (3 4))))
           (dataset (make-hdf5-dataset h5file "test"
                                       (array-dimensions data)
                                       :initial-contents data)))
      (is (= (hdf5-dataset-rank dataset) (array-rank data)))
      (is (equal (hdf5-dataset-dimensions dataset) (array-dimensions data)))
      (is (equal (hdf5-dataset-element-type dataset) '(unsigned-byte 16)))
      (is (eq (hdf5-containing-file dataset) h5file))
      (is (equal (hdf5-path dataset) "/test")))))

(test write-and-read-dataset
  (dolist (type '((unsigned-byte 8)
                  (unsigned-byte 16)
                  (unsigned-byte 32)
                  (unsigned-byte 64)
                  (signed-byte 8)
                  (signed-byte 16)
                  (signed-byte 32)
                  (signed-byte 64)))
    (with-temporary-file h5file
      (let* ((data (make-array '(2 2)
                               :element-type type
                               :initial-contents '((1 2) (3 4))))
             (dataset (make-hdf5-dataset h5file "test"
                                         (array-dimensions data)
                                         :initial-contents data))
             (read-data (hdf5-ref dataset 't)))
        (is (equal (array-element-type data) (array-element-type read-data)))
        (is (equalp data read-data))))))


(test write-and-read-file
  (dolist (type '((unsigned-byte 8)
                  (unsigned-byte 16)
                  (unsigned-byte 32)
                  (unsigned-byte 64)
                  (signed-byte 8)
                  (signed-byte 16)
                  (signed-byte 32)
                  (signed-byte 64)))
    (let ((data (make-array '(2 2)
                            :element-type type
                            :initial-contents '((1 2) (3 4)))))
      (with-temporary-file-write-then-read h5file
        (make-hdf5-dataset h5file "test"
                           (array-dimensions data)
                           :initial-contents data)
        (progn
          (let* ((dataset (hdf5-ref h5file "test"))
                 (read-data (hdf5-ref dataset 't)))
            (is (equal (array-element-type data) (array-element-type read-data)))
            (is (equalp data read-data))))))))
