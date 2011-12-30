(in-package :mbcl)

(alexandria:define-constant +mb-search-base+ "http://musicbrainz.org/ws/2/"
  :test #'string=)
(alexandria:define-constant +mb-user-agent+ "MB-Search Lambda 0.1"
  :test #'string=)

(defparameter *mb-entities*
  '("artist" "label" "recording" "release" "release-group" "work"))

(defvar *last-call* nil
  "The UT of the last call to the web service. Used to ensure that we don't make
more than one query per second.")

(defvar *debug-mbws-calls* t
  "Print something to the console on each web service call.")

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

(defun mbws-call (method mbid parameters)
  "Call a method from the MusicBrainz web service. If you don't want to give an
MBID (ie for search calls, just pass NIL). Returns the parsed XML."
  (unless (and (or (null mbid) (stringp mbid)) (stringp method))
    (error "METHOD must be a string and MBID must be a string or NIL."))
  (when *debug-mbws-calls*
    (format t "MBWS-CALL: ~A / ~A / ~A~%" method mbid parameters)
    (force-output))

  (when *last-call*
    (when (= 0 (- (get-universal-time) *last-call*))
      (sleep 1)))
  (prog1
      (let ((drakma:*body-format-function* #'mb-body-format))
        (xmls:parse
         (drakma:http-request
          (format nil "~A~A~@[/~A~]" +mb-search-base+ method mbid)
          :parameters parameters
          :user-agent +mb-user-agent+)))
    (setf *last-call* (get-universal-time))
    (when *debug-mbws-calls*
      (format t "END MBWS-CALL~%")
      (force-output))))
