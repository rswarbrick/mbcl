(in-package :mbcl)

(alexandria:define-constant +mb-search-base+ "http://musicbrainz.org/ws/2/"
  :test #'string=)
(alexandria:define-constant +homepage+ "https://github.com/rswarbrick/mbcl"
  :test #'string=)
(alexandria:define-constant +version+ "0.1" :test #'string=)
(alexandria:define-constant +mb-user-agent+
    (format nil "MBCL/~A ~A" +version+ +homepage+) :test #'string=)

(defparameter *mb-entities*
  '("artist" "label" "recording" "release" "release-group" "work"))

(defvar *last-call* nil
  "The UT of the last call to the web service. Used to ensure that we don't make
more than one query per second.")

(defvar *debug-mbws-calls* nil
  "Print something to the console on each web service call.")

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
      (let ((drakma:*text-content-types*
             (cons (cons "application" "xml") drakma:*text-content-types*))
            (drakma:*drakma-default-external-format* :utf-8))
        (xmls:parse
         (drakma:http-request
          (format nil "~A~A~@[/~A~]" +mb-search-base+ method mbid)
          :parameters parameters
          :user-agent +mb-user-agent+)))
    (setf *last-call* (get-universal-time))
    (when *debug-mbws-calls*
      (format t "END MBWS-CALL~%")
      (force-output))))

(defun object-browse-request (mb-object table-name &key (offset 0) (limit 100))
  (mbws-call table-name nil
             (list (cons (table-name mb-object) (id mb-object))
                   (cons "offset" (format nil "~A" offset))
                   (cons "limit" (format nil "~A" limit)))))
