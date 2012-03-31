(in-package :mbcl)

(defclass mb-class-slot (sb-mop:standard-slot-definition)
  ((inc :initform ""
        :initarg :inc
        :reader mb-class-slot-inc
        :documentation
        "Set to a string if it's needed as inc parameter for updating slot. If
set to NIL, then we should never try to update it via the webservice (for
example, the MBID). The slot-value code will throw an error if we try.

The other possibility is to set it to '(:BROWSE . STR). Then updating it goes via
a browse request with STR as the tablename.")))

(defclass mb-class-direct-slot
    (mb-class-slot sb-mop:standard-direct-slot-definition) ())

(defclass mb-class-effective-slot
    (mb-class-slot sb-mop:standard-effective-slot-definition) ())

(defclass mb-class (standard-class) ())

(defmethod sb-mop:direct-slot-definition-class
    ((cls mb-class) &key name allocation &allow-other-keys)
  (if (or (eq name 'updated-list) (eq allocation :class))
      (find-class 'sb-mop:standard-direct-slot-definition)
      (find-class 'mb-class-direct-slot)))

(defmethod sb-mop:effective-slot-definition-class
    ((cls mb-class) &key name allocation &allow-other-keys)
  (if (or (eq name 'updated-list) (eq allocation :class))
      (find-class 'sb-mop:standard-effective-slot-definition)
      (find-class 'mb-class-effective-slot)))

(defmethod sb-mop:validate-superclass ((cls mb-class) (super standard-class))
  t)

(defun find-slot-definition (object slot-name)
  (find slot-name (sb-mop:class-slots (class-of object))
        :key #'sb-mop:slot-definition-name))

(defun std-slot-value (object slot-name)
  "A slot-value call that doesn't do the clever caching that we define with
SLOT-VALUE-USING-CLASS for MB-OBJECT."
  (unless (typep object 'mb-object)
    (slot-value object slot-name))
  (let ((location (sb-mop:slot-definition-location
                   (find-slot-definition object slot-name))))
    (if (integerp location)
      (sb-mop:standard-instance-access object location)
      :class-allocated)))

(defclass mb-object ()
  ((id :initarg :id :reader id :inc nil :initform nil)
   (table-name :initform nil :reader table-name :allocation :class)
   (parser :initform nil :reader parser :allocation :class)
   (relations :reader relations :initform nil :inc "artist-rels")
   (updated-list
    :documentation
    "Set in initialize-instance: an alist of slot names and NIL/T depending on
whether the slot should have been updated. If it is NIL and we want to read the
slot, then update it from the web service."))
  (:documentation "The base class for all MB objects.")
  (:metaclass mb-class))

(defgeneric page (mb-object)
  (:documentation "Return string of the URL for MB-OBJECT's MusicBrainz page."))

(defmethod page ((mbo mb-object))
  (format nil "http://musicbrainz.org/~A/~A" (table-name mbo) (id mbo)))

(defgeneric moniker (mb-object)
  (:documentation
   "Return the name by which this object is referred. This is either called
TITLE or NAME elsewhere, which is somewhat irritating when trying to do generic
stuff."))

(defun combine-incs (dsds)
  "Take a list of direct slot definitions and find the correct INC value. If
there are two different non-empty ones we just throw an error to keep life
simple."
  (let ((incs (delete-duplicates
               (remove "" (mapcar (lambda (dsd) (slot-value dsd 'inc)) dsds)
                       :test #'equalp)
               :test #'equalp)))
    (cond
      ((null incs)
       "")
      ((= (length incs) 1)
       (car incs))
      (t
       (error
         "Don't know how to combine the INC values of the given DSDs.")))))

(defmethod sb-mop:compute-effective-slot-definition :around
    ((class mb-class) slot-name direct-slot-definitions)
  (let ((slot (call-next-method)))
    (when (find 'inc (sb-mop:class-slots (class-of slot))
                :key #'sb-mop:slot-definition-name)
      (setf (slot-value slot 'inc)
            (combine-incs direct-slot-definitions)))
    slot))

(defun slot-defined-p (ob slot-name)
  (cdr (assoc slot-name (std-slot-value ob 'updated-list))))

(defun updatable-slot-p (slot)
  (typep slot 'mb-class-slot))

(defun updatable-slots (mb-class)
  (remove-if-not #'updatable-slot-p (sb-mop:class-slots mb-class)))

(defun mark-slot-updated (ob slot-name)
  (setf (cdr (assoc slot-name (std-slot-value ob 'updated-list))) t))

(defun maybe-slot-value (ob slot-name)
  (or (std-slot-value ob slot-name) "<not defined>"))

(defmethod initialize-instance :after ((ob mb-object) &rest initargs)
  (declare (ignore initargs))
  ;; Set updated-list
  (setf (slot-value ob 'updated-list)
        (mapcar (lambda (slot) (cons (sb-mop:slot-definition-name slot) nil))
                (updatable-slots (class-of ob)))))

(defmacro def-mb-class (name direct-superclasses direct-slots &rest options)
  `(defclass ,name ,(cons 'mb-object direct-superclasses)
     ,direct-slots
     ,@options
     (:metaclass mb-class)))

(defclass date ()
  ((year :initform nil :initarg :year :accessor year)
   (month :initform nil :initarg :month :accessor month)
   (day :initform nil :initarg :day :accessor day)))

(defun date-string (date)
  "Format a date in the form Y-M-D (possibly missing D or M and D)"
  (with-output-to-string (str)
    (with-slots (year month day) date
      (princ year str)
      (when month
        (format str "-~2,1,0,'0@A" month)
        (when day
          (format str "-~2,1,0,'0@A" day))))))

(defun parse-date-string (string)
  "Parse a string of the form nnn-nn-nn or nnnn-nn or nnnn to a valid date
object."
  (let ((pieces (mapcar #'parse-integer (split-sequence #\- string))))
    (unless (and (<= 1 (length pieces) 3)
                 (or (not (second pieces)) (<= 1 (second pieces) 12))
                 (or (not (third pieces)) (<= 1 (third pieces) 31)))
      (error "Invalid date string."))
    (make-instance 'date
                   :year (first pieces)
                   :month (second pieces)
                   :day (third pieces))))

(defun date= (date1 date2)
  "Return true if the dates are equal (have the same number of defined
components, each of which is equal)"
  (and (equal (day date1) (day date2))
       (equal (month date1) (month date2))
       (equal (year date1) (year date2))))

(defmethod print-object ((date date) stream)
  (print-unreadable-object (date stream :type t)
    (princ (date-string date) stream)))

(defclass time-period ()
  ((begin :reader begin :initform nil)
   (end :reader end :initform nil)))

(defmethod print-object ((tp time-period) stream)
  (print-unreadable-object (tp stream :type t :identity t)
    (with-slots (begin end) tp
      (cond
        ((not (or begin end)) nil)
        ((date= begin end) (princ (date-string begin) stream))
        (t
         (format stream "~A TO ~A"
                 (or (when begin (date-string begin)) "-")
                 (or (when end (date-string end)) "NOW")))))))

(defclass alias ()
  ((alias :reader alias)
   (locale :reader locale)))

(defmethod print-object ((alias alias) stream)
  (print-unreadable-object (alias stream :type t :identity t)
    (format stream "'~A'~@[ (locale: ~A)~]"
            (alias alias) (locale alias))))

(def-mb-class artist ()
  ((table-name :initform "artist" :allocation :class)
   (parser :initform 'parse-artist :allocation :class)
   (name :reader name :initform nil)
   (sort-name :reader sort-name :initform nil)
   (disambiguation :reader disambiguation :initform nil)
   (type :reader artist-type :initform nil)
   (ipi :reader ipi :initform nil)
   (gender :reader gender :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (aliases :reader aliases :initform nil :inc "aliases")
   (releases :reader releases :initform nil :inc "releases")
   (release-groups :reader release-groups :initform nil :inc "release-groups")))

(defmethod print-object ((artist artist) stream)
  (print-unreadable-object (artist stream :type t :identity t)
    (princ (maybe-slot-value artist 'name) stream)))

(defmethod moniker ((x artist)) (name x))

(defclass name-credit ()
  ((artist :reader artist)
   (name :reader name :initform nil)
   (join-phrase :reader join-phrase :initform nil)))

(defun name-credit-string (nc)
  "Return the string that NAME-CREDIT should be rendered to."
  (format nil "~A~@[~A~]"
          (or (name nc) (maybe-slot-value (artist nc) 'name))
          (join-phrase nc)))

(defmethod print-object ((nc name-credit) stream)
  (print-unreadable-object (nc stream :type t)
    (princ #\' stream)
    (princ (name-credit-string nc) stream)
    (princ #\' stream)))

(defun make-name-credit (artist &key title join-phrase)
  (unless (typep artist 'artist)
    (error "ARTIST must be a MusicBrainz ARTIST object."))
  (let ((nc (make-instance 'name-credit)))
    (setf (slot-value nc 'artist) artist)
    (awhen title (setf (slot-value nc 'name) it))
    (awhen join-phrase (setf (slot-value nc 'join-phrase) it))
    nc))

(defclass artist-credit ()
  ((name-credits :reader name-credits :initform nil)))

(defun artist-credit-string (ac)
  (format nil "~{~A~}" (mapcar #'name-credit-string (name-credits ac))))

(defmethod print-object ((ac artist-credit) stream)
  (print-unreadable-object (ac stream :type t)
    (princ #\' stream)
    (princ (artist-credit-string ac) stream)
    (princ #\' stream)))

(defun make-artist-credit (artist &key name)
  "Create a simple artist credit for just a single artist, possibly given a
different NAME."
  (let ((ac (make-instance 'artist-credit)))
    (setf (slot-value ac 'name-credits)
          (list (make-name-credit artist :title name)))
    ac))

(def-mb-class release-group ()
  ((table-name :initform "release-group" :allocation :class)
   (parser :initform 'parse-release-group :allocation :class)
   (type :reader rg-type :initform nil)
   (title :reader title :initform nil)
   (first-release-date :reader first-release-date :initform nil)
   (artist-credit :reader artist-credit :initform nil :inc "artists")
   (release-list :reader release-list :initform nil :inc "releases")))

(defmethod moniker ((x release-group)) (title x))

(defmethod print-object ((rg release-group) stream)
  (print-unreadable-object (rg stream :type t :identity t)
    (format stream "'~A'~@[ BY '~A'~]"
            (shortened-string (maybe-slot-value rg 'title))
            (when (std-slot-value rg 'artist-credit)
              (artist-credit-string (artist-credit rg))))))

(defclass track ()
  ((title :reader title :initform nil)
   (position :reader pos :initform nil)
   (length :reader track-length :initform nil)
   (recording :reader recording :initform nil)
   (artist-credit :reader artist-credit :initform nil))
  (:documentation
   "A track on a medium in a release. TITLE is NIL when the track title agrees
   with that of the recording, it seems."))

(defmethod print-object ((track track) stream)
  (print-unreadable-object (track stream :type t :identity t)
    (princ (shortened-string (maybe-slot-value track 'title)) stream)))

(defclass disc ()
  ((id :reader id)
   (sectors :reader sectors)
   (release-list :reader release-list)))

(defclass medium ()
  ((position :reader pos :initform nil)
   (format :reader fmt :initform nil)
   (title :reader title :initform nil)
   (disc-list :reader disc-list :initform nil)
   (track-list :reader track-list :initform nil)))

(defgeneric tracks (object)
  (:documentation
   "Return a list of the tracks on OBJECT. Throw an error if they can't be
  properly retrieved."))

(defmethod tracks ((m medium))
  (let ((tl (track-list m)))
    (unless tl (error "No track list for medium ~A" m))
    (let ((tracks (pl-as-list tl)))
      (unless (recording (first tracks))
        (error "Track list incomplete (no recording for first track)."))
      tracks)))

(defmethod print-object ((medium medium) stream)
  (print-unreadable-object (medium stream :type t :identity t)
    (format stream "~@[~A~]~@[ (POS: ~A)~]"
            (fmt medium)
            (unless (and (integerp medium) (= 1 (pos medium)))
              (pos medium)))))

(defclass medium-list (partial-list)
  ((track-count :reader track-count :initform nil)))

(defmethod tracks ((ml medium-list))
  (mapcan #'tracks (pl-as-list ml)))

(defclass text-representation ()
  ((language :reader language :initform nil)
   (script :reader script :initform nil)))

(defmethod print-object ((tr text-representation) stream)
  (print-unreadable-object (tr stream :type t :identity t)
    (format stream "~A / ~A" (language tr) (script tr))))

(defclass label-info ()
  ((catno :reader catno :initform nil)
   (label :reader label :initform nil)))

(defmethod print-object ((li label-info) stream)
  (print-unreadable-object (li stream :type t :identity t)
    (format stream "~@[ '~A'~]~@[ ('~A')~]"
            (catno li) (when (label li) (name (label li))))))

(def-mb-class release ()
  ((table-name :initform "release" :allocation :class)
   (parser :initform 'parse-release :allocation :class)
   (title :reader title :initform nil)
   (disambiguation :reader disambiguation :initform nil)
   (status :reader status :initform nil)
   (quality :reader quality :initform nil)
   (packaging :reader packaging :initform nil)
   (text-representation :reader text-representation :initform nil)
   (artist-credit :reader artist-credit :initform nil :inc "artists")
   (release-group :reader release-group :initform nil
                  :inc "release-groups")
   (date :reader date :initform nil)
   (country :reader country :initform nil)
   (asin :reader release-asin :initform nil)
   (barcode :reader barcode :initform nil)
   (label-info :reader label-info :initform nil :inc "labels")
   ;; Ask for recordings and artist-credits in medium list since then I get the
   ;; track list as well, along with the artists that performed on the
   ;; tracks. As far as I can tell, this is the only web call that'll get the
   ;; relevant information.
   (medium-list :reader medium-list :initform nil
                :inc "media+recordings+artist-credits")
   (recordings :reader recordings :initform nil
               :inc (:browse . "recording"))))

(defmethod moniker ((x release)) (title x))

(defmethod tracks ((rel release))
  (tracks (medium-list rel)))

(defmethod print-object ((release release) stream)
  (print-unreadable-object (release stream :type t :identity t)
    (format stream "'~A'~@[ BY '~A'~]"
            (shortened-string (maybe-slot-value release 'title))
            (when (std-slot-value release 'artist-credit)
              (artist-credit-string (artist-credit release))))))

(def-mb-class recording ()
  ((table-name :initform "recording" :allocation :class)
   (parser :initform 'parse-recording :allocation :class)
   (title :reader title :initform nil)
   (disambiguation :reader disambiguation :initform nil)
   (length :reader recording-length :initform nil)
   (artist-credit :reader artist-credit :initform nil :inc "artists")
   (release-list :reader release-list :initform nil :inc "releases")))

(defmethod moniker ((x recording)) (title x))

(defun format-time-period (milliseconds)
  (multiple-value-bind (mins secs)
      (floor (round milliseconds 1000) 60)
    (format nil "~A:~2,'0D" mins secs)))

(defmethod print-object ((r recording) stream)
  (print-unreadable-object (r stream :type t :identity t)
    (format stream "'~A' BY '~A' (~A)"
            (shortened-string (maybe-slot-value r 'title))
            (when (std-slot-value r 'artist-credit)
              (artist-credit-string (artist-credit r)))
            (awhen (std-slot-value r 'length)
              (format-time-period (maybe-slot-value r 'length))))))

(def-mb-class label ()
  ((table-name :initform "label" :allocation :class)
   (parser :initform 'parse-label :allocation :class)
   (name :reader name)
   (sort-name :reader sort-name :initform nil)
   (disambiguation :reader disambiguation :initform nil)
   (label-code :reader label-code :initform nil)
   (type :reader label-type :initform nil)
   (ipi :reader ipi :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (releases :reader releases :initform nil :inc "labels")
   (aliases :reader aliases :initform nil :inc "aliases")))

(defmethod moniker ((x label)) (name x))

(defmethod print-object ((label label) stream)
  (print-unreadable-object (label stream :type t :identity t)
    (princ (maybe-slot-value label 'name) stream)))

(defclass relation ()
  ((type :reader relation-type)
   (direction :reader direction :initform nil)
   (attributes :reader attributes :initform nil)
   (begin :reader begin :initform nil)
   (end :reader end :initform nil)
   (target-id :reader target-id :initform nil)
   (target :reader target :initform nil)))

(defclass attribute-list ()
  ((attributes :reader attributes)))

(defmethod print-object ((relation relation) stream)
  (print-unreadable-object (relation stream :type t :identity t)
    (format stream "~A ~A ~A"
            (relation-type relation)
            (cond
              ((not (direction relation)) "<->")
              ((string= "backward" (direction relation)) "<-")
              ((string= "forward" (direction relation)) "->")
              (t
               (error "Weird relation direction: ~A"
                      (direction relation))))
            (target relation))))

(def-mb-class work ()
  ((table-name :initform "work" :allocation :class)
   (parser :initform 'parse-work :allocation :class)
   (type :reader work-type :initform nil)
   (title :reader title :initform nil)
   (disambiguation :reader disambiguation :initform nil)
   (aliases :reader aliases :initform nil :inc "aliases")
   (relations :reader relations :initform nil)))

(defmethod moniker ((x work)) (title x))

(defmethod print-object ((work work) stream)
  (print-unreadable-object (work stream :type t :identity t)
    (format stream "~@[ '~A'~]~@[ (TYPE: '~A')~]"
            (shortened-string (maybe-slot-value work 'title))
            (maybe-slot-value work 'type))))

(defgeneric merge-slot-values (target-object sv-one sv-two)
  (:documentation
   "Called when merging two different slot values. There is a default
implementation that takes SV-ONE if it's non-null and SV-TWO otherwise."))

(defmethod merge-slot-values ((target-object mb-object) sv-one sv-two)
  (or sv-one sv-two))

(defmethod merge-objects ((a mb-object) (b mb-object) &optional in-place?)
  "Merge two objects of the same class, replacing any NILs in A with non-NILS in
B. If IN-PLACE? is true, instead of making a new object, we make changes to A."

  ;; Use STD-SLOT-VALUE lots here because this gets called from the caching
  ;; code, so shouldn't invoke it circularly!
  (unless (eq (class-of b) (class-of a))
    (error "A and B must be of the same class (got ~A, ~A)."
           (class-of a) (class-of b)))

  (let* ((cls (class-of a))
         (slot-names (mapcar #'sb-mop:slot-definition-name (updatable-slots cls)))
         (ulist-a (std-slot-value a 'updated-list))
         (ulist-b (std-slot-value b 'updated-list))
         (c (if in-place? a (make-instance cls))))
    ;; Slots that should be updated if set.
    (dolist (name slot-names)
      (setf (slot-value c name)
            (merge-slot-values
             c (std-slot-value a name) (std-slot-value b name))))
    ;; Merge ulists.
    (setf (slot-value c 'updated-list)
          (mapcar (lambda (name)
                    (cons name
                          (or (cdr (assoc name ulist-a))
                              (cdr (assoc name ulist-b)))))
                  slot-names))
    c))
