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

(defun mbws-call (method parameters)
  "Call a method from the MusicBrainz web service. Returns the parsed XML."
  (let ((drakma:*body-format-function* #'mb-body-format))
    (xmls:parse
     (drakma:http-request
      (format nil "~A~A" +mb-search-base+ method)
      :parameters parameters
      :user-agent +mb-user-agent+))))
