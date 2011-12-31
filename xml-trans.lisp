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

(defun parse-integer-or-zero (str-or-nil)
  "Return a parsed version of this integer, or zero if STR-OR-NIL is NULL."
  (if str-or-nil (parse-integer str-or-nil) 0))

(defun parse-partial-list (xml object-parser)
  (let* ((attributes (second xml))
         (children (cddr xml))
         (count (get-attribute "count" attributes))
         (offset (parse-integer-or-zero
                  (get-attribute "offset" attributes)))
         (parsed-count
          (if (and (not count) (> (length children) 0))
              (length children)
              (parse-integer count)))
         (logical-length
          (if (= 0 (length children)) 100 (length children)))
         (psize (if (> offset 0) (hcf offset logical-length) logical-length)))
    (when (or count (> parsed-count 0))
      (let ((pl (make-instance 'partial-list
                               :page-size psize :size parsed-count)))
        (when children
          (pl-store-segment pl offset (mapcar object-parser children)))
        pl))))

(defmacro declare-list-parser (sym)
  "Declare PARSE-<SYM>-LIST, which parses a list segment of the given type."
  (let ((parse-list-sym (intern (format nil "PARSE-~A-LIST" (symbol-name sym))))
        (parse-sym (intern (format nil "PARSE-~A" (symbol-name sym)))))
    `(DEFUN ,parse-list-sym (XML)
       (PARSE-PARTIAL-LIST XML ',parse-sym))))

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

(defun object-browse-request (mb-object table-name &key (offset 0) (limit 100))
  (mbws-call table-name nil
             (list (cons (table-name mb-object) (id mb-object))
                   (cons "offset" (format nil "~A" offset))
                   (cons "limit" (format nil "~A" limit)))))

(defmacro mb-xml-parse (class xml
                        (&body attribute-defns) (&body child-defns)
                        (&body partial-list-defns))
  "Like SIMPLE-XML-PARSE, but also deals with the relationship list and
partial-list bootstrapping procedures. CLASS should be the symbol naming the
class of object to make. PARTIAL-LIST-DEFNS should consist of pairs whose first
element is the slot name and second element is the table name for the browse
request. Use NIL if this isn't actually a partial list, and the value in the
slot will be replaced by the contents."
  `(let ((parsed (simple-xml-parse (make-instance ',class) ,xml t
                   ,attribute-defns ,child-defns)))
     ;; Deal with relationship list. Note the use of std-slot-value to avoid an
     ;; infinite recursion.
     (aif+ (std-slot-value parsed 'relations)
         (setf (slot-value it 'parent) parsed)
       (setf (slot-value parsed 'relations) (make-relations-list parsed)))
     ;; Deal with the partial lists.
     ,@(mapcar (lambda (pld)
                 (destructuring-bind (slot-name table-name) pld
                   `(awhen (std-slot-value parsed ',slot-name)
                      ,(if table-name
                          `(setf (updater it)
                                 (lambda (offset page-size)
                                   (declare (ignore page-size))
                                   (parse-search-results
                                    (object-browse-request parsed ,table-name
                                                           :offset offset))))
                          `(setf (slot-value parsed ',slot-name)
                                 (pl-as-list (std-slot-value parsed ',slot-name)))))))
              partial-list-defns)
     (merge-cached-object parsed)))

(defun parse-artist (xml) 
  (mb-xml-parse artist xml
    ("id" "type" ("score" . nil))
    ("name"
     "sort-name" "disambiguation" "gender" "country" "ipi"
     (("life-span" 'parse-time-period) . life-span)
     (("alias-list" 'parse-alias-list) . aliases)
     (("release-list" 'parse-release-list) . releases)
     (("release-group-list" 'parse-release-group-list) . release-groups)
     (("relation-list" 'parse-relations) . relations)
     ("tag-list" . nil))
    ((aliases nil) (releases "release") (release-groups "release-groups"))))

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
     ("tag-list" . nil))
    ((release-list "release"))))

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
  (let* ((children (cddr xml))
         (track-count
          (when (matches-name (first (first children)) "track-count")
            (parse-integer (third (first children)))))
         (count (- (length children) (if track-count 1 0)))
         (ml (make-instance 'medium-list :page-size count :size count)))
    (pl-store-segment ml 0
                      (mapcar 'parse-medium
                              (if track-count (cdr children) children)))
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
     (("medium-list" 'parse-medium-list) . medium-list))
    ((label-info "label"))))

(defun parse-recording (xml)
  (mb-xml-parse recording xml
   ("id" ("score" . nil))
   ("title"
    (("length" :int) . length)
    (("artist-credit" 'parse-artist-credit) . artist-credit)
    (("release-list" 'parse-release-list) . release-list)
    ("puid-list" . nil) ("tag-list" . nil) ("isrc-list" . nil))
   ((release-list "release"))))

(defun parse-label (xml)
  (mb-xml-parse label xml
    ("id" "type" ("score" . nil))
    ("name"
     "sort-name" "label-code" "country" "disambiguation" "ipi"
     (("life-span" 'parse-time-period) . life-span)
     (("alias-list" 'parse-alias-list) . aliases)
     (("release-list" 'parse-release-list) . releases)
     (("tag-list" . NIL)))
    ((aliases nil) (releases "release"))))

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

(defun parse-relations (xml)
  (let ((lst (make-relations-list nil)))
    (store-relation-segment
     lst (parse-partial-list xml 'parse-relation))
    lst))

(defun parse-work (xml)
  (mb-xml-parse work xml
    ("id" "type" ("score" . nil))
    ("title"
     "disambiguation"
     (("alias-list" 'parse-alias-list) . aliases)
     (("relation-list" 'parse-relations) . relations))
    ((aliases nil))))

(defmacro declare-list-parsers (&body symbols)
  "Make a function for each symbol called PARSE-<SYMBOL>-LIST, which parses a
list of that type into a PARTIAL-LIST using PARSE-<SYMBOL> (which you've
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
