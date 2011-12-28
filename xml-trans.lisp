(in-package :mb-getrecs)

(defclass mb-object ()
  ((id :initarg :id :reader id :initform nil))
  (:documentation
   "The base class for all MB objects."))

(defclass list-segment ()
  ((type :reader ls-type)
   (count :reader ls-count)
   (offset :reader ls-offset)
   (contents :reader contents)))

(defun parse-list-segment (xml type-sym object-parser)
  (let* ((attributes (second xml))
         (children (cddr xml))
         (count (get-attribute "count" attributes))
         (offset (get-attribute "offset" attributes))
         (ls (make-instance 'list-segment)))
    (unless (or (and count offset) (not (or count offset)))
      (error "List segment only had one of count & offset in its attributes."))
    (setf (slot-value ls 'type) type-sym
          (slot-value ls 'count)
          (if count (parse-integer count) (length children))
          (slot-value ls 'offset)
          (if offset (parse-integer offset) 0)
          (slot-value ls 'contents) (mapcar object-parser children))
    ls))

(defmethod print-object ((ls list-segment) stream)
  (format stream "#<LIST-SEGMENT(~A) [~D,~D] of ~D>"
          (ls-type ls) (ls-offset ls)
          (1- (+ (ls-offset ls) (length (contents ls)))) (ls-count ls)))

(defclass time-period ()
  ((begin :reader begin :initform nil)
   (end :reader end :initform nil)))

(defun parse-time-period (xml)
  (simple-xml-parse (make-instance 'time-period) xml t
    () ("begin" "end")))

(defclass alias ()
  ((alias :reader alias)
   (locale :reader locale)))

(defun parse-alias (xml)
  (let ((locale (get-attribute "locale" (second xml)))
        (name (third xml))
        (a (make-instance 'alias)))
    (setf (slot-value a 'alias) name
          (slot-value a 'locale) locale)
    a))

(defmethod print-object ((alias alias) stream)
  (format stream "#<ALIAS '~A'~@[ (locale: ~A)~]>"
          (alias alias) (locale alias)))

(defun parse-alias-list (xml)
  (parse-list-segment xml 'alias 'parse-alias))

(defclass artist (mb-object)
  ((name :reader name)
   (sort-name :reader sort-name)
   (disambiguation :reader disambiguation)
   (type :reader artist-type :initform nil)
   (gender :reader gender :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (aliases :reader aliases :initform nil)))

(defun parse-artist (xml)
  (simple-xml-parse (make-instance 'artist) xml t
    ("id" "type" ("score" . nil))
    ("name"
     "sort-name" "disambiguation" "gender" "country"
     (("life-span" 'parse-time-period) . life-span)
     (("alias-list" 'parse-alias-list) . aliases)
     ("tag-list" . nil))))

(defmethod print-object ((artist artist) stream)
  (format stream "#<ARTIST '~A'>" (name artist)))

(defclass name-credit ()
  ((artist :reader artist)
   (join-phrase :reader join-phrase :initform nil)))

(defun name-credit-string (nc)
  "Return the string that NAME-CREDIT should be rendered to."
  (format nil "~A~@[ ~A ~]" (name (artist nc)) (join-phrase nc)))

(defmethod print-object ((nc name-credit) stream)
  (format stream "#<NAME-CREDIT '~A'>" (name-credit-string nc)))

(defun parse-name-credit (xml)
  "Return a NAME-CREDIT object from a <name-credit> tag."
  (simple-xml-parse (make-instance 'name-credit) xml t
    (("joinphrase" . join-phrase))
    ((("artist" 'parse-artist) . artist))))

(defclass artist-credit ()
  ((name-credits :reader name-credits)))

(defun parse-artist-credit (xml)
  (let ((ac (make-instance 'artist-credit)))
    (setf (slot-value ac 'name-credits)
          (mapcar #'parse-name-credit (cddr xml)))
    ac))

(defun artist-credit-string (ac)
  (format nil "~{~A~}" (mapcar #'name-credit-string (name-credits ac))))

(defmethod print-object ((ac artist-credit) stream)
  (format stream "#<ARTIST-CREDIT '~A'>" (artist-credit-string ac)))

(defclass release-group (mb-object)
  ((type :reader rg-type)))

(defun parse-release-group (xml)
  ;; This won't work for stuff that returns more release group info!
  (simple-xml-parse (make-instance 'release-group) xml t
    ("type") ()))

(defclass track ()
  ((title :reader title)))

(defun parse-track (xml)
  (simple-xml-parse (make-instance 'track) xml t () ("title")))

(defmethod print-object ((track track) stream)
  (format stream "#<TRACK '~A'>" (title track)))

(defclass medium ()
  ((position :reader pos)
   (format :reader fmt)
   (track-list :reader track-list)))

(defun parse-track-list (xml)
  (parse-list-segment xml 'track 'parse-track))

(defun parse-medium (xml)
  (simple-xml-parse (make-instance 'medium) xml t
    ()
    ("format"
     (("position" :int) . position)
     (("track-list" 'parse-track-list) . track-list))))

(defmethod print-object ((medium medium) stream)
  (format stream "#<MEDIUM: ~A~@[ ~A~]>"
          (fmt medium)
          (unless (= 1 (pos medium)) (pos medium))))

(defclass medium-list (list-segment)
  ((track-count :reader track-count :initform nil)))

(defun parse-medium-list (xml)
  (let* ((ml (make-instance 'medium-list))
         (children (cddr xml)))
    (when (matches-name (first (first children)) "track-count")
      (setf (slot-value ml 'track-count)
            (parse-integer (third (first children))))
      (setf children (cdr children)))
    (setf (slot-value ml 'type) 'medium
          (slot-value ml 'count) (length children)
          (slot-value ml 'offset) 0
          (slot-value ml 'contents)
          (mapcar 'parse-medium children))
    ml))

(defclass release (mb-object)
  ((title :reader title)
   (status :reader status)
   (artist-credit :reader artist-credit :initform nil)
   (release-group :reader release-group)
   (date :reader date)
   (country :reader country)
   (medium-list :reader medium-list)))

(defmethod print-object ((release release) stream)
  (format stream "#<RELEASE '~A'~@[ BY '~A'~]>"
          (shortened-string (title release))
          (when (artist-credit release)
            (artist-credit-string (artist-credit release)))))

(defun parse-release (xml)
  (simple-xml-parse (make-instance 'release) xml t
    ("id")
    ("title"
     "status"
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-group" 'parse-release-group) . release-group)
     "date"
     "country"
     (("medium-list" 'parse-medium-list) . medium-list))))

(defun parse-release-list (xml)
  (parse-list-segment xml 'release 'parse-release))

(defclass recording (mb-object)
  ((title :reader title)
   (length :reader recording-length)
   (artist-credit :reader artist-credit)
   (release-list :reader release-list)))

(defun format-time-period (milliseconds)
  (multiple-value-bind (mins secs)
      (floor (floor milliseconds 1000) 60)
    (format nil "~A:~2,'0D" mins secs)))

(defmethod print-object ((r recording) stream)
  (format stream "#<RECORDING '~A' BY '~A' (~A)>"
          (shortened-string (title r))
          (artist-credit-string (artist-credit r))
          (format-time-period (recording-length r))))

(defun parse-recording (xml)
  (simple-xml-parse (make-instance 'recording) xml nil
    ("id")
    ("title"
     (("length" :int) . length)
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-list" 'parse-release-list) . release-list))))

(defun parse-recording-list (xml)
  (parse-list-segment xml 'recording 'parse-recording))

(defun parse-artist-list (xml)
  (parse-list-segment xml 'artist 'parse-artist))

(defun parse-search-results (xml)
  (let ((key (first (third xml))))
    (cond
      ((matches-name key "recording-list")
       (parse-recording-list (third xml)))
      ((matches-name key "artist-list")
       (parse-artist-list (third xml)))
      (t
       (error "Unknown search results type (~A)" key)))))
