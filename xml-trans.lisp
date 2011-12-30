(in-package :mbcl)

(defparameter *xml-parsers*
  '(("artist" parse-artist)
    ("release-group" parse-release-group)
    ("release" parse-release)
    ("recording" parse-recording)
    ("label" parse-label)
    ("work" parse-work))
  "A list of XML tags and the relevant parsers for them: needed to make
MB-REQUEST work.")

(defun find-parser (xml)
  "Returns a valid parser for a tag representing a subclass of MB-OBJECT."
  (second (find (caar xml) *xml-parsers* :key #'car :test #'string=)))

(defgeneric refresh-object (mb-object inc)
  (:documentation "Grab further info about the object by calling through to the
web service with the given inc parameters."))

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

(defmacro declare-list-parser (sym)
  "Declare PARSE-<SYM>-LIST, which parses a list segment of the given type."
  (let ((parse-list-sym (intern (format nil "PARSE-~A-LIST" (symbol-name sym))))
        (parse-sym (intern (format nil "PARSE-~A" (symbol-name sym)))))
    `(DEFUN ,parse-list-sym (XML)
       (PARSE-LIST-SEGMENT XML ',sym ',parse-sym))))

(defun parse-time-period (xml)
  (simple-xml-parse (make-instance 'time-period) xml t
    () ("begin" "end")))

(defun parse-alias (xml)
  (let ((locale (get-attribute "locale" (second xml)))
        (name (third xml))
        (a (make-instance 'alias)))
    (setf (slot-value a 'alias) name
          (slot-value a 'locale) locale)
    a))

(declare-list-parser alias)

(defmacro mb-xml-parse (class xml (&body attribute-defns) (&body child-defns))
  "Like SIMPLE-XML-PARSE, but also deals with the relationship list
bootstrapping procedure. CLASS should be the symbol naming the class of object
to make."
  `(LET ((PARSED (SIMPLE-XML-PARSE (MAKE-INSTANCE ',class) ,xml t
                   ,attribute-defns ,child-defns)))
     ;; Note the use of STD-SLOT-VALUE to avoid the infinite recursion.
     (AIF (STD-SLOT-VALUE PARSED 'RELATIONS)
          (SETF (SLOT-VALUE IT 'PARENT) PARSED)
          (SETF (SLOT-VALUE PARSED 'RELATIONS) (MAKE-RELATIONS-LIST PARSED)))
     (MERGE-CACHED-OBJECT PARSED)))

(defun parse-artist (xml) 
  (mb-xml-parse artist xml
    ("id" "type" ("score" . nil))
    ("name"
     "sort-name" "disambiguation" "gender" "country" "ipi"
     (("life-span" 'parse-time-period) . life-span)
     (("alias-list" 'parse-alias-list) . aliases)
     (("release-list" 'parse-release-list) . releases)
     (("release-group-list" 'parse-release-group-list) . release-groups)
     (("relation-list" 'parse-relation-list) . relations)
     ("tag-list" . nil))))

(defun parse-name-credit (xml)
  "Return a NAME-CREDIT object from a <name-credit> tag."
  (simple-xml-parse (make-instance 'name-credit) xml t
    (("joinphrase" . join-phrase))
    ((("artist" 'parse-artist) . artist)
     "name")))

(defun parse-artist-credit (xml)
  (let ((ac (make-instance 'artist-credit)))
    (setf (slot-value ac 'name-credits)
          (mapcar #'parse-name-credit (cddr xml)))
    ac))

(defun parse-release-group (xml)
  (mb-xml-parse release-group xml
    ("type" "id" ("score" . nil))
    ("title" "first-release-date"
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-list" 'parse-release-list) . release-list)
     ("tag-list" . nil))))

(defun parse-track (xml)
  (simple-xml-parse (make-instance 'track) xml t () ("title")))

(declare-list-parser track)

(defun parse-disc (xml)
  (declare (ignore xml))
  (error "Not yet written."))

(declare-list-parser disc)

(defun parse-medium (xml)
  (simple-xml-parse (make-instance 'medium) xml t
    ()
    ("format"
     (("position" :int) . position)
     (("disc-list" 'parse-disc-list) . disc-list)
     (("track-list" 'parse-track-list) . track-list))))

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

(defun parse-text-representation (xml)
  (simple-xml-parse (make-instance 'text-representation) xml t
    () ("language" "script")))

(defun parse-label-info (xml)
  (simple-xml-parse (make-instance 'label-info) xml t
    ()
    (("catalog-number" . catno)
     (("label" 'parse-label) . label))))

(declare-list-parser label-info)

(defun parse-release (xml)
  (mb-xml-parse release xml
    ("id" ("score" . nil))
    ("title"
     "status" "packaging" "quality" "disambiguation"
     (("text-representation" 'parse-text-representation) .
      text-representation)
     (("artist-credit" 'parse-artist-credit) . artist-credit)
     (("release-group" 'parse-release-group) . release-group)
     "date" "asin" "country" "barcode"
     (("label-info-list" 'parse-label-info-list) . label-info)
     (("medium-list" 'parse-medium-list) . medium-list))))

(defun parse-recording (xml)
  (mb-xml-parse recording xml
   ("id" ("score" . nil))
   ("title"
    (("length" :int) . length)
    (("artist-credit" 'parse-artist-credit) . artist-credit)
    (("release-list" 'parse-release-list) . release-list))))

(defun parse-label (xml)
  (mb-xml-parse label xml
    ("id" "type" ("score" . nil))
    ("name"
     "sort-name" "label-code" "country" "disambiguation" "ipi"
     (("life-span" 'parse-time-period) . life-span)
     (("alias-list" 'parse-alias-list) . aliases)
     (("tag-list" . NIL)))))

(defun parse-attribute-list (xml)
  (let ((al (make-instance 'attribute-list)))
    (setf (slot-value al 'attributes)
          (mapcar (lambda (form)
                    (unless (matches-name (first form) "attribute")
                      (error "Unexpected tag in attribute list: ~A." form))
                    (third form))
                  (cddr xml)))
    al))

(defun parse-relation (xml)
  (simple-xml-parse (make-instance 'relation) xml t
    ("type")
    ("direction"
     (("attribute-list" 'parse-attribute-list) . attributes)
     "begin" "end"
     ("target" . target-id)
     (("artist" 'parse-artist) . target)
     (("release" 'parse-release) . target)
     (("release-group" 'parse-release-group) . target)
     (("recording" 'parse-recording) . target)
     (("label" 'parse-label) . target)
     (("work" 'parse-work) . target))))

(defun parse-relation-list (xml)
  (let ((lst (make-relations-list nil)))
    (store-relation-segment
     lst (parse-list-segment xml 'relation 'parse-relation))
    lst))

(defun parse-work (xml)
  (mb-xml-parse work xml
    ("id" "type" ("score" . nil))
    ("title"
     "disambiguation"
     (("alias-list" 'parse-alias-list) . aliases)
     (("relation-list" 'parse-relation-list) . relations))))

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
       ,@(mapcar (lambda (sym) `(declare-list-parser ,sym)) symbols)
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

(declare-list-parsers recording artist label release release-group work)
