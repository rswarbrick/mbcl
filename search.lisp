(in-package :mb-getrecs)

(defun search-request (search-string &key (type "recording") (limit 100) (offset 0))
  "Make an http request for a search and return the XML, parsed by XMLS."
  (unless (member type *mb-entities* :test #'string=)
    (error "Invalid entity type: ~A." type))
  (unless (typep limit '(integer 1 100))
    (error "Invalid limit (~A not integer in [1,100])." limit))
  (unless (typep offset '(integer 0))
    (error "Invalid offset (~A not integer >= 0)." offset))
  (parse-search-results
   (mbws-call type (list (cons "query" search-string)
                         (cons "limit" (format nil "~D" limit))
                         (cons "offset" (format nil "~D" offset))))))
