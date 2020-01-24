(defsystem h5cl.test
  :description "Tests for high-level HDF5 interface"
  :version "0.0.1"
  :author "Konrad Hinsen"
  :license "GPL3"
  :homepage "https://github.com/khinsen/h5cl"
  :depends-on (:h5cl
               :fiveam
               :uiop)
  :serial t
  :components ((:file "package.test")
               (:file "tests"))
  :perform (test-op :after (op c)
                    (eval (read-from-string "(5am:run! :h5cl)"))))
