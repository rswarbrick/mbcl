(in-package :mbcl)

(defclass mb-class-slot (sb-mop:standard-slot-definition)
  ((inc :initform ""
        :initarg :inc
        :reader mb-class-slot-inc
        :documentation
        "Set to a string if it's needed as inc parameter for updating slot. If
set to NIL, then we should never try to update it via the webservice (for
example, the MBID). The slot-value code will throw an error if we try.")))

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

(defun combine-incs (dsds)
  "Take a list of direct slot definitions. For each, grab the INC slot and
combine to get the correct resulting INC argument (with plusses)"
  (format nil "~{~A~^+~}"
          (remove-duplicates
           (mapcan (lambda (dsd)
                     (let ((val (slot-value dsd 'inc)))
                       (when val (split-sequence:split-sequence #\+ val))))
                   dsds)
           :test #'string=)))

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

(defclass list-segment ()
  ((type :reader ls-type)
   (count :reader ls-count)
   (offset :reader ls-offset)
   (contents :reader contents)))

(defmethod print-object ((ls list-segment) stream)
  (print-unreadable-object (ls stream :type t :identity t)
    (format stream "[~A] ~:[None~2*~;[~D,~D]~] of ~D"
            (ls-type ls)
            (> (length (contents ls)) 0)
            (ls-offset ls) (1- (+ (ls-offset ls) (length (contents ls))))
            (ls-count ls))))

(defclass time-period ()
  ((begin :reader begin :initform nil)
   (end :reader end :initform nil)))

(defmethod print-object ((tp time-period) stream)
  (print-unreadable-object (tp stream :type t :identity t)
    (with-slots (begin end) tp
      (cond
        ((not (or begin end)) nil)
        ((string= begin end) (princ begin stream))
        (t
         (format stream "~A TO ~A" (or begin "-") (or end "NOW")))))))

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
   (gender :reader gender :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (aliases :reader aliases :initform nil :inc "aliases")
   (releases :reader releases :initform nil :inc "releases")
   (release-groups :reader release-groups :initform nil :inc "release-groups")))

(defmethod print-object ((artist artist) stream)
  (print-unreadable-object (artist stream :type t :identity t)
    (princ (maybe-slot-value artist 'name) stream)))

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
  (print-unreadable-object (nc stream :type t :identity t)
    (princ (name-credit-string nc) stream)))

(defclass artist-credit ()
  ((name-credits :reader name-credits)))

(defun artist-credit-string (ac)
  (format nil "~{~A~}" (mapcar #'name-credit-string (name-credits ac))))

(defmethod print-object ((ac artist-credit) stream)
  (print-unreadable-object (ac stream :type t :identity t)
    (princ (artist-credit-string ac) stream)))

(def-mb-class release-group ()
  ((table-name :initform "release-group" :allocation :class)
   (parser :initform 'parse-release-group :allocation :class)
   (type :reader rg-type :initform nil)
   (title :reader title :initform nil)
   (first-release-date :reader first-release-date :initform nil)
   (artist-credit :reader artist-credit :initform nil :inc "artists")
   (release-list :reader release-list :initform nil :inc "releases")))

(defmethod print-object ((rg release-group) stream)
  (print-unreadable-object (rg stream :type t :identity t)
    (format stream "'~A'~@[ BY '~A'~]"
            (shortened-string (maybe-slot-value rg 'title))
            (when (std-slot-value rg 'artist-credit)
              (artist-credit-string (artist-credit rg))))))

(defclass track ()
  ((title :reader title)))

(defmethod print-object ((track track) stream)
  (print-unreadable-object (track stream :type t :identity t)
    (princ (shortened-string (title track)) stream)))

(defclass disc ()
  ((id :reader id)
   (sectors :reader sectors)
   (release-list :reader release-list)))

(defclass medium ()
  ((position :reader pos :initform nil)
   (format :reader fmt :initform nil)
   (disc-list :reader disc-list :initform nil)
   (track-list :reader track-list :initform nil)))

(defmethod print-object ((medium medium) stream)
  (print-unreadable-object (medium stream :type t :identity t)
    (format stream "~@[ ~A~]~@[ (POS: ~A)~]"
            (fmt medium)
            (unless (and (integerp medium) (= 1 (pos medium)))
              (pos medium)))))

(defclass medium-list (list-segment)
  ((track-count :reader track-count :initform nil)))

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
   (title :reader title)
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
   (medium-list :reader medium-list :initform nil)))

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
   (length :reader recording-length :initform nil)
   (artist-credit :reader artist-credit :initform nil :inc "artists")
   (release-list :reader release-list :initform nil :inc "releases")))

(defun format-time-period (milliseconds)
  (multiple-value-bind (mins secs)
      (floor (floor milliseconds 1000) 60)
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
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (aliases :reader aliases :initform nil :inc "aliases")))

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
