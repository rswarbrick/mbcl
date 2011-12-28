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
    (unless (or count (not (or count offset)))
      (error "List segment had offset but not count in its attributes."))
    (setf (slot-value ls 'type) type-sym
          (slot-value ls 'count)
          (if count (parse-integer count) (length children))
          (slot-value ls 'offset)
          (if offset (parse-integer offset) 0)
          (slot-value ls 'contents) (mapcar object-parser children))
    ls))

(defmethod print-object ((ls list-segment) stream)
  (format stream "#<LIST-SEGMENT(~A) ~:[None~2*~;[~D,~D]~] of ~D>"
          (ls-type ls)
          (> (length (contents ls)) 0)
          (ls-offset ls) (1- (+ (ls-offset ls) (length (contents ls))))
          (ls-count ls)))

(defclass time-period ()
  ((begin :reader begin :initform nil)
   (end :reader end :initform nil)))

(defun parse-time-period (xml)
  (simple-xml-parse (make-instance 'time-period) xml t
    () ("begin" "end")))

(defmethod print-object ((tp time-period) stream)
  (with-slots (begin end) tp
    (cond
      ((not (or begin end))
       (call-next-method))
      ((string= begin end)
       (format stream "#<TIME-PERIOD ~A>" begin))
      (t
       (format stream "#<TIME-PERIOD ~A TO ~A>" (or begin "-") (or end "NOW"))))))

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
   (name :reader name :initform nil)
   (join-phrase :reader join-phrase :initform nil)))

(defun name-credit-string (nc)
  "Return the string that NAME-CREDIT should be rendered to."
  (format nil "~A~@[~A~]"
          (or (name nc) (name (artist nc)))
          (join-phrase nc)))

(defmethod print-object ((nc name-credit) stream)
  (format stream "#<NAME-CREDIT '~A'>" (name-credit-string nc)))

(defun parse-name-credit (xml)
  "Return a NAME-CREDIT object from a <name-credit> tag."
  (simple-xml-parse (make-instance 'name-credit) xml t
    (("joinphrase" . join-phrase))
    ((("artist" 'parse-artist) . artist)
     "name")))

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
  ((type :reader rg-type :initform nil)
   (title :reader title :initform nil)
   (artist-credit :reader artist-credit :initform nil)
   (release-list :reader release-list :initform nil)))

(defun parse-release-group (xml)
  (simple-xml-parse (make-instance 'release-group) xml t
    ("type" "id" ("score" . nil))
    ("title"
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-list" 'parse-release-list) . release-list)
     ("tag-list" . nil))))

(defmethod print-object ((rg release-group) stream)
  (format stream "#<RELEASE-GROUP '~A'~@[ BY '~A'~]>"
          (shortened-string (title rg))
          (when (artist-credit rg)
            (artist-credit-string (artist-credit rg)))))

(defclass track ()
  ((title :reader title)))

(defun parse-track (xml)
  (simple-xml-parse (make-instance 'track) xml t () ("title")))

(defmethod print-object ((track track) stream)
  (format stream "#<TRACK '~A'>" (title track)))

(defun parse-track-list (xml)
  (parse-list-segment xml 'track 'parse-track))

(defclass disc ()
  ((id :reader id)
   (sectors :reader sectors)
   (release-list :reader release-list)))

(defun parse-disc (xml)
  (declare (ignore xml))
  (error "Not yet written."))

(defun parse-disc-list (xml)
  (parse-list-segment xml 'disc 'parse-disc))

(defclass medium ()
  ((position :reader pos :initform nil)
   (format :reader fmt :initform nil)
   (disc-list :reader disc-list :initform nil)
   (track-list :reader track-list :initform nil)))

(defun parse-medium (xml)
  (simple-xml-parse (make-instance 'medium) xml t
    ()
    ("format"
     (("position" :int) . position)
     (("disc-list" 'parse-disc-list) . disc-list)
     (("track-list" 'parse-track-list) . track-list))))

(defmethod print-object ((medium medium) stream)
  (format stream "#<MEDIUM~@[ ~A~]~@[ (POS: ~A)~]>"
          (fmt medium)
          (unless (and (integerp medium) (= 1 (pos medium)))
            (pos medium))))

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

(defclass text-representation ()
  ((language :reader language :initform nil)
   (script :reader script :initform nil)))

(defun parse-text-representation (xml)
  (simple-xml-parse (make-instance 'text-representation) xml t
    () ("language" "script")))

(defmethod print-object ((tr text-representation) stream)
  (format stream "#<TEXT-REPRESENTATION ~A / ~A>"
          (language tr) (script tr)))

(defclass label-info ()
  ((catno :reader catno :initform nil)
   (label :reader label :initform nil)))

(defun parse-label-info (xml)
  (simple-xml-parse (make-instance 'label-info) xml t
    ()
    (("catalog-number" . catno)
     (("label" 'parse-label) . label))))

(defmethod print-object ((li label-info) stream)
  (format stream "#<LABEL-INFO~@[ '~A'~]~@[ ('~A')~]>"
          (catno li)
          (when (label li) (name (label li)))))

(defun parse-label-info-list (xml)
  (parse-list-segment xml 'label-info 'parse-label-info))

(defclass release (mb-object)
  ((title :reader title)
   (status :reader status :initform nil)
   (text-representation :reader text-representation :initform nil)
   (artist-credit :reader artist-credit :initform nil)
   (release-group :reader release-group :initform nil)
   (date :reader date :initform nil)
   (country :reader country :initform nil)
   (asin :reader release-asin :initform nil)
   (barcode :reader barcode :initform nil)
   (label-info :reader label-info :initform nil)
   (medium-list :reader medium-list :initform nil)))

(defmethod print-object ((release release) stream)
  (format stream "#<RELEASE '~A'~@[ BY '~A'~]>"
          (shortened-string (title release))
          (when (artist-credit release)
            (artist-credit-string (artist-credit release)))))

(defun parse-release (xml)
  (simple-xml-parse (make-instance 'release) xml t
    ("id" ("score" . nil))
    ("title"
     "status"
     (("text-representation" 'parse-text-representation) .
      text-representation)
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-group" 'parse-release-group) . release-group)
     "date" "asin" "country" "barcode"
     (("label-info-list" 'parse-label-info-list) . label-info)
     (("medium-list" 'parse-medium-list) . medium-list))))

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
  (simple-xml-parse (make-instance 'recording) xml t
    ("id")
    ("title"
     (("length" :int) . length)
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-list" 'parse-release-list) . release-list))))

(defclass label (mb-object)
  ((name :reader name)
   (sort-name :reader sort-name)
   (disambiguation :reader disambiguation)
   (label-code :reader label-code :initform nil)
   (type :reader label-type :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span)
   (aliases :reader aliases)))

(defun parse-label (xml)
  (simple-xml-parse (make-instance 'label) xml t
    ("id" "type" ("score" . nil))
    ("name"
     "sort-name" "label-code" "country" "disambiguation"
     (("life-span" 'parse-time-period) . life-span)
     (("alias-list" 'parse-alias-list) . aliases)
     (("tag-list" . NIL)))))

(defmethod print-object ((label label) stream)
  (format stream "#<LABEL '~A'>" (name label)))

(defmacro declare-list-parsers (&body symbols)
  "Make a function for each symbol called PARSE-<SYMBOL>-LIST, which parses a
list of that type into a LIST-SEGMENT using PARSE-<SYMBOL> (which you've
hopefully defined). Also defines PARSE-SEARCH-RESULTS, which dispatches on the
type of the result so it can be called simply by a search function."
  (flet ((parse-list-name (sym)
           (intern (format nil "PARSE-~A-LIST" (symbol-name sym))))
         (parse-name (sym)
           (intern (format nil "PARSE-~A" (symbol-name sym))))
         (tag-name (sym)
           (format nil "~A-list" (string-downcase (symbol-name sym)))))
    `(PROGN
       ,@(mapcar
          (lambda (sym)
            `(DEFUN ,(parse-list-name sym) (XML)
               (PARSE-LIST-SEGMENT XML ',sym ',(parse-name sym))))
          symbols)

       (DEFUN PARSE-SEARCH-RESULTS (XML)
         (LET ((KEY (FIRST (THIRD XML))))
           (COND
             ,@(mapcar
                (lambda (sym)
                  `((MATCHES-NAME KEY ,(tag-name sym))
                    (,(parse-list-name sym) (THIRD XML))))
                symbols)
             (T
              (ERROR "Unknown search results type (~A)" KEY))))))))

(declare-list-parsers recording artist label release release-group)
