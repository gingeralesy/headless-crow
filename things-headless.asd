
(defsystem "things-headless"
  :name "Experiments without X"
  :author "Janne"
  :long-description "This is experiments in Lisp"
  :description "Experiments with CL"
  :components ((:file "utils")
               (:file "queens")
               (:file "twist")
               (:file "my-cffi"))
  :depends-on (:cffi))

