(in-package :mb-getrecs)

(alexandria:define-constant +mb-search-base+ "http://musicbrainz.org/ws/2/"
  :test #'string=)
(alexandria:define-constant +mb-user-agent+ "MB-Search Lambda 0.1"
  :test #'string=)

(defparameter *mb-entities*
  '("artist" "label" "recording" "release" "release-group" "work"))

(defun mb-body-format (headers external-format)
  (multiple-value-bind (type subtype parameters)
      (drakma:get-content-type headers)
    (unless (and (string= "application" type)
                 (string= "xml" subtype))
      (error "Unrecognised content-type: ~A/~A." type subtype))
    (let* ((charset (drakma:parameter-value "charset" parameters))
           (name (cond (charset (chunga:as-keyword charset))
                       (t external-format))))
      (flexi-streams:make-external-format name :eol-style :lf))))

(defun search-request (search-string &key (type "recording") (limit 100) (offset 0))
  "Make an http request for a search and return the XML, parsed by XMLS."
  (unless (member type *mb-entities* :test #'string=)
    (error "Invalid entity type: ~A." type))
  (unless (typep limit '(integer 1 100))
    (error "Invalid limit (~A not integer in [1,100])." limit))
  (unless (typep offset '(integer 0))
    (error "Invalid offset (~A not integer >= 0)." offset))

  (let ((drakma:*body-format-function* #'mb-body-format))
    (parse-search-results
     (xmls:parse
      (drakma:http-request
       (format nil "~A~A" +mb-search-base+ type)
       :parameters (list (cons "query" search-string)
                         (cons "limit" (format nil "~D" limit))
                         (cons "offset" (format nil "~D" offset)))
       :user-agent +mb-user-agent+)))))
