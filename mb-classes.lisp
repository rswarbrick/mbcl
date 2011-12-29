(in-package :mb-getrecs)

(defclass mb-class-slot (sb-mop:standard-slot-definition)
  ((inc :initform nil
        :initarg :inc
        :reader mb-class-slot-inc
        :documentation
        "Set to a string if it's needed as inc parameter for updating slot.")))

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
  ((id :initarg :id :reader id :initform nil)
   (table-name :initform nil :reader table-name :allocation :class)
   (parser :initform nil :reader parser :allocation :class)
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
  (format stream "#<LIST-SEGMENT(~A) ~:[None~2*~;[~D,~D]~] of ~D>"
          (ls-type ls)
          (> (length (contents ls)) 0)
          (ls-offset ls) (1- (+ (ls-offset ls) (length (contents ls))))
          (ls-count ls)))

(defclass time-period ()
  ((begin :reader begin :initform nil)
   (end :reader end :initform nil)))

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

(defmethod print-object ((alias alias) stream)
  (format stream "#<ALIAS '~A'~@[ (locale: ~A)~]>"
          (alias alias) (locale alias)))

(def-mb-class artist ()
  ((table-name :initform "artist" :allocation :class)
   (parser :initform 'parse-artist :allocation :class)
   (name :reader name)
   (sort-name :reader sort-name)
   (disambiguation :reader disambiguation :initform nil)
   (type :reader artist-type :initform nil)
   (gender :reader gender :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (aliases :reader aliases :initform nil :inc "aliases")))

(defmethod print-object ((artist artist) stream)
  (format stream "#<ARTIST '~A'>" (maybe-slot-value artist 'name)))

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
  (format stream "#<NAME-CREDIT '~A'>" (name-credit-string nc)))

(defclass artist-credit ()
  ((name-credits :reader name-credits)))

(defun artist-credit-string (ac)
  (format nil "~{~A~}" (mapcar #'name-credit-string (name-credits ac))))

(defmethod print-object ((ac artist-credit) stream)
  (format stream "#<ARTIST-CREDIT '~A'>" (artist-credit-string ac)))

(def-mb-class release-group ()
  ((table-name :initform "release-group" :allocation :class)
   (parser :initform 'parse-release-group :allocation :class)
   (type :reader rg-type :initform nil)
   (title :reader title :initform nil)
   (artist-credit :reader artist-credit :initform nil)
   (release-list :reader release-list :initform nil)))

(defmethod print-object ((rg release-group) stream)
  (format stream "#<RELEASE-GROUP '~A'~@[ BY '~A'~]>"
          (shortened-string (maybe-slot-value rg 'title))
          (when (std-slot-value rg 'artist-credit)
            (artist-credit-string (artist-credit rg)))))

(defclass track ()
  ((title :reader title)))

(defmethod print-object ((track track) stream)
  (format stream "#<TRACK '~A'>" (title track)))

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
  (format stream "#<MEDIUM~@[ ~A~]~@[ (POS: ~A)~]>"
          (fmt medium)
          (unless (and (integerp medium) (= 1 (pos medium)))
            (pos medium))))

(defclass medium-list (list-segment)
  ((track-count :reader track-count :initform nil)))

(defclass text-representation ()
  ((language :reader language :initform nil)
   (script :reader script :initform nil)))

(defmethod print-object ((tr text-representation) stream)
  (format stream "#<TEXT-REPRESENTATION ~A / ~A>"
          (language tr) (script tr)))

(defclass label-info ()
  ((catno :reader catno :initform nil)
   (label :reader label :initform nil)))

(defmethod print-object ((li label-info) stream)
  (format stream "#<LABEL-INFO~@[ '~A'~]~@[ ('~A')~]>"
          (catno li)
          (when (label li) (name (label li)))))

(def-mb-class release ()
  ((table-name :initform "release" :allocation :class)
   (parser :initform 'parse-release :allocation :class)
   (title :reader title)
   (disambiguation :reader disambiguation :initform nil)
   (status :reader status :initform nil)
   (quality :reader quality :initform nil)
   (packaging :reader packaging :initform nil)
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
          (shortened-string (maybe-slot-value release 'title))
          (when (std-slot-value release 'artist-credit)
            (artist-credit-string (artist-credit release)))))

(def-mb-class recording ()
  ((table-name :initform "recording" :allocation :class)
   (parser :initform 'parse-recording :allocation :class)
   (title :reader title)
   (length :reader recording-length)
   (artist-credit :reader artist-credit)
   (release-list :reader release-list)))

(defun format-time-period (milliseconds)
  (multiple-value-bind (mins secs)
      (floor (floor milliseconds 1000) 60)
    (format nil "~A:~2,'0D" mins secs)))

(defmethod print-object ((r recording) stream)
  (format stream "#<RECORDING '~A' BY '~A' (~A)>"
          (shortened-string (maybe-slot-value r 'title))
          (when (std-slot-value r 'artist-credit)
            (artist-credit-string (artist-credit r)))
          (format-time-period (maybe-slot-value r 'length))))

(def-mb-class label ()
  ((table-name :initform "label" :allocation :class)
   (parser :initform 'parse-label :allocation :class)
   (name :reader name)
   (sort-name :reader sort-name)
   (disambiguation :reader disambiguation)
   (label-code :reader label-code :initform nil)
   (type :reader label-type :initform nil)
   (country :reader country :initform nil)
   (life-span :reader life-span :initform nil)
   (aliases :reader aliases :initform nil)))

(defmethod print-object ((label label) stream)
  (format stream "#<LABEL '~A'>" (maybe-slot-value label 'name)))

(defclass relation ()
  ((type :reader relation-type)
   (direction :reader direction :initform nil)
   (attributes :reader attributes :initform nil)
   (beginning :reader beginning :initform nil)
   (end :reader end :initform nil)
   (target :reader target :initform nil)))

(defclass attribute-list ()
  ((attributes :reader attributes)))

(defmethod print-object ((relation relation) stream)
  (format stream "#<RELATION ~A~@[ ~A ~]~A>"
          (relation-type relation)
          (when (direction relation)
            (cond
              ((string= "backward" (direction relation)) "<-")
              ((string= "forward" (direction relation)) "->")
              (t
               (direction relation))))
          (target relation)))

(def-mb-class work ()
  ((table-name :initform "work" :allocation :class)
   (parser :initform 'parse-work :allocation :class)
   (type :reader work-type :initform nil)
   (title :reader title :initform nil)
   (disambiguation :reader disambiguation :initform nil)
   (aliases :reader aliases :initform nil)
   (relations :reader relations :initform nil)))

(defmethod print-object ((work work) stream)
  (format stream "#<WORK~@[ '~A'~]~@[ (TYPE: '~A')~]>"
          (shortened-string (maybe-slot-value work 'title))
          (maybe-slot-value work 'work-type)))

(defmethod merge-objects ((a mb-object) (b mb-object))
  "Merge two objects of the same class, replacing any NILs in A with non-NILS in
B."
  ;; Use STD-SLOT-VALUE lots here because this gets called from the caching
  ;; code, so shouldn't invoke it circularly!
  (unless (eq (class-of b) (class-of a))
    (error "A and B must be of the same class (got ~A, ~A)."
           (class-of a) (class-of b)))

  (let* ((cls (class-of a))
         (slot-names (mapcar #'sb-mop:slot-definition-name (updatable-slots cls)))
         (ulist-a (std-slot-value a 'updated-list))
         (ulist-b (std-slot-value b 'updated-list))
         (c (make-instance cls)))
    ;; Slots that should be updated if set.
    (dolist (name slot-names)
      (setf (slot-value c name)
            (if (cdr (assoc name ulist-a))
                (std-slot-value a name)
                (or (std-slot-value a name) (std-slot-value b name)))))
    ;; Merge ulists.
    (setf (slot-value c 'updated-list)
          (mapcar (lambda (name)
                    (cons name
                          (or (assoc name ulist-a) (assoc name ulist-b))))
                  slot-names))
    c))
