(asdf:defsystem h5cl
  :description "High-level HDF5 interface"
  :version "0.0.1"
  :author "Konrad Hinsen"
  :license "GPL3"
  :homepage "https://github.com/khinsen/h5cl"
  :depends-on (:hdf5-cffi
               :trivial-garbage
               :uiop)
  :serial t
  :components ((:file "package")
               (:file "basics")
               (:file "files")
               (:file "groups")
               (:file "datasets")
               (:file "datatypes")
               (:file "accessors"))
  :in-order-to ((test-op (test-op :h5cl.test))))
