(defpackage mb-getrecs-asd
  (:use :cl :asdf))
(in-package :mb-getrecs-asd)

(defsystem mb-getrecs
    :depends-on (:drakma :alexandria :xmls)
    :components
    ((:file "package")
     (:file "util" :depends-on ("package"))
     (:file "xml-trans" :depends-on ("package" "util"))
     (:file "search" :depends-on ("package" "xml-trans"))))
