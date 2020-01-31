(in-package :h5cl)

(defclass hdf5-file (hdf5-location)
  ((filename
    :initarg :filename
    :reader hdf5-file-filename
    :documentation "Filename")
   (fapl-id
    :initarg :fapl-id
    :documentation "File access property list id")))

(defmethod print-object ((object hdf5-file) out)
  (with-slots (filename) object
    (print-unreadable-object (object out :type t)
      (format out "filename = ~s" filename))))

(defun h5open (filename mode)
  (let* ((fapl-id (hdf5:h5pcreate hdf5:+H5P-FILE-ACCESS+))
	 (file-id (prog2
                      (hdf5:h5pset-fclose-degree fapl-id :H5F-CLOSE-STRONG)
                      (hdf5:h5fopen filename mode fapl-id))))
    (unless (< 0 (hdf5:h5iis-valid file-id))
      (error "File could not be opened: ~s." filename))
    (let ((file-object (make-instance 'hdf5-file
                                      :id file-id :filename filename :fapl-id fapl-id)))
      (trivial-garbage:finalize file-object
                                #'(lambda ()
                                    (close-id file-id)
                                    (close-id fapl-id)))
      file-object)))

(defun h5create (filename)
  (let* ((fapl-id (hdf5:h5pcreate hdf5:+H5P-FILE-ACCESS+))
	 (file-id (prog2
                      (hdf5:h5pset-fclose-degree fapl-id :H5F-CLOSE-STRONG)
                      (hdf5:h5fcreate filename
                                      hdf5:+H5F-ACC-TRUNC+
                                      hdf5:+H5P-DEFAULT+
                                      fapl-id))))
    (unless (< 0 (hdf5:h5iis-valid file-id))
      (error "File could not be created: ~s." filename))
    (let ((file-object (make-instance 'hdf5-file
                                      :id file-id :filename filename :fapl-id fapl-id)))
      (trivial-garbage:finalize file-object
                                #'(lambda ()
                                    (close-id file-id)
                                    (close-id fapl-id)))
      file-object)))

(defun open-hdf5 (filename
                  &key
                    direction
                    (if-exists nil if-exists-supplied-p)
                    (if-does-not-exist nil if-does-not-exist-supplied-p))
  "Open the HDF5 file FILENAME."
  ;; HDF5 files can always be read, so :output is interpreted as :io.
  (when (eql direction :output)
    (setf direction :io))
  (case direction
    (:input
     (unless if-does-not-exist-supplied-p
       (setf if-does-not-exist :error))
     (if (probe-file filename)
         (h5open (namestring filename) hdf5:+H5F-ACC-RDONLY+)
         (case if-does-not-exist
           (:error (error "File does not exist: ~s" filename))
           (nil nil))))
    (:io
     (unless if-exists-supplied-p
       (setf if-exists :error))
     (unless if-does-not-exist-supplied-p
       (setf if-does-not-exist :create))
     (if (probe-file filename)
         (case if-exists
           (:error (error "File exists: ~s" filename))
           (nil nil)
           (:supersede
            (delete-file filename)
            (h5create (namestring filename)))
           (:overwrite
            (h5create (namestring filename)))
           (:append
            (h5open (namestring filename) hdf5:+H5F-ACC-RDWR+)))
         ;; TODO
         (case if-does-not-exist
           (:error (error "file does not exist"))
           (nil nil)
           (:create
            (h5create (namestring filename))))))))

(defun close-hdf5 (hdf5-file)
  "Close HDF5-FILE"
  (with-slots (id fapl-id) hdf5-file
    (close-id id)
    (setf id nil)
    (close-id fapl-id)
    (setf fapl-id nil)))
