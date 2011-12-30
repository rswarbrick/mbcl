(defpackage mbcl-asd
  (:use :cl :asdf))
(in-package :mbcl-asd)

(defsystem mbcl
    :depends-on (:drakma :alexandria :xmls :split-sequence)
    :components
    ((:file "package")
     (:file "util" :depends-on ("package"))
     (:file "mbws" :depends-on ("package"))
     (:file "mb-cache" :depends-on ("package" "mb-classes"))
     (:file "mb-classes" :depends-on ("package"))
     (:file "relations" :depends-on ("package"))
     (:file "xml-trans" :depends-on ("package" "mb-classes" "mbws" "util"))
     (:file "hilevel-webservice"
            :depends-on ("package"
                         "mbws" "mb-classes" "xml-trans" "mb-cache"))))
