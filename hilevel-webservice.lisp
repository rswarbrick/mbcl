(in-package :mbcl)

(defun search-request (search-string
                       &key (type "recording") (limit 100) (offset 0))
  "Make an http request for a search and return a partial list containing the
matches."
  (unless (member type *mb-entities* :test #'string=)
    (error "Invalid entity type: ~A." type))
  (unless (typep limit '(integer 1 100))
    (error "Invalid limit (~A not integer in [1,100])." limit))
  (unless (typep offset '(integer 0))
    (error "Invalid offset (~A not integer >= 0)." offset))

  (let ((pl (parse-search-results
             (mbws-call type nil
                        (list (cons "query" search-string)
                              (cons "limit" (format nil "~D" limit))
                              (cons "offset" (format nil "~D" offset)))))))
    (setf (updater pl)
          (lambda (offset page-size)
            (search-request search-string :type type
                            :limit page-size :offset offset)))
    pl))

(defun inc-should-be-marked-updated? (object list-pair inc)
  "See SET-INC-UPDATED!"
  (destructuring-bind (key &rest updated?) list-pair
    (cons key
          (or updated?
              (let ((slot-inc (mb-class-slot-inc
                               (find-slot-definition object key))))
                (or (null slot-inc)
                    (and (stringp slot-inc)
                         (or (string= "" slot-inc)
                             (string= inc slot-inc)))))))))

(defun set-inc-updated! (object inc)
  "Modifies OBJECT by updating the ULIST such that any slot that gets picked up
with inc equal to NIL or INC or with is marked as updated. This stops multiple
calls to the webservice when an object doesn't have a property set: it's just
going to be the same call (and presumably the same answer!) each time
otherwise.

Returns (the modified version of) OBJECT."
  (setf (slot-value object 'updated-list)
        (mapcar (lambda (list-pair)
                  (inc-should-be-marked-updated? object list-pair inc))
                (slot-value object 'updated-list)))
  object)

(defun mb-request (type mb-object &key inc browse)
  "Make a MusicBrainz standard request for MB-OBJECT in the table TYPE. Pass INC
as inc=... parameters if given: it can be a list of strings, which this function
can join together, or just a single string. If BROWSE is true, we do something
slightly different and calls OBJECT-BROWSE-REQUEST, using TYPE as the table name
for the output data."
  (let ((xml
         (if browse
             (object-browse-request mb-object type)
             (mbws-call type (id mb-object)
                        (cond
                          ((or (not inc) (and (stringp inc) (string= inc "")))
                           nil)
                          ((stringp inc) (list (cons "inc" inc)))
                          ((consp inc)
                           (list (cons "inc" (format nil "~{~A~^+~}" inc))))
                          (t
                           (error "Invalid format for INC: ~A" inc)))))))
    (cond
      ((matches-name (first xml) "metadata")
       (let ((result (parse-metadata xml)))
         (unless browse
           (set-inc-updated! result inc))
         result))

      ((matches-name (first xml) "error" :namespace nil)
       (error 'mb-error :text (third (third xml))))

      (t
       (error "Unknown response format: ~A" xml)))))

(define-condition mb-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (c s)
             (format s "MB Error: ~A" (text c)))))

(defmethod refresh-object ((mb mb-object) inc)
  (unless (std-slot-value mb 'id)
    (error "Cannot refresh an object with no ID! (~A)" mb))
  (merge-cached-object (mb-request (table-name mb) mb :inc inc)))

(defvar *mb-slot-value-debug* nil
  "Set this to T to see each call to SB-MOP:SLOT-VALUE-USING-CLASS specialised
as below. I'm a bit paranoid about circular calls etc. and this should make it
easier to see that happening.")

;; Defined here rather than in mb-classes.lisp because it uses the cache and
;; webservice functions
(defmethod sb-mop:slot-value-using-class
    ((cls mb-class) (ob mb-object) (slot mb-class-slot))
  (let* ((cached)
         (slot-name (sb-mop:slot-definition-name slot))
         (val (std-slot-value ob slot-name))
         (inc (mb-class-slot-inc slot))
         (slow-path nil))

    (let ((ans
           (cond
             ;; If we *think* the slot is defined, great!
             ((slot-defined-p ob slot-name) val)
             ;; If we don't think we've defined it, but we've got a nonzero value, use
             ;; it anyway and correct updated-list.
             (val
              (mark-slot-updated ob slot-name)
              val)
             ;; Next, try the cached instance. Maybe that'll be more help. Note
             ;; that we get the ID via STD-SLOT-VALUE. If, for some bizarre
             ;; reason, there's no ID set, we blow the control stack otherwise!
             ((and (setf slow-path t)
                   (setf cached (gethash (std-slot-value ob 'id) *mb-cache*))
                   (slot-defined-p cached slot-name))
              (mark-slot-updated ob slot-name)
              (setf (slot-value ob slot-name)
                    (std-slot-value cached slot-name)))
             ;; Rubbish. Looks like we have to make a webservice call.
             (t
              (unless cached
                (setf (gethash (std-slot-value ob 'id) *mb-cache*) ob
                      cached ob))
              (cond
                ((not inc)
                 (error "Trying to update slot ~A with NIL INC." slot-name))
                ;; Standard request (see doc for MB-CLASS-SLOT)
                ((stringp inc)
                 (setf cached (refresh-object ob inc)))
                ;; Browse request
                ((and (consp inc) (eq (car inc) :browse))
                 (setf (slot-value cached slot-name)
                       (mb-request (cdr inc) cached :browse t)))
                (t
                 (error "Unrecognised INC value: ~A" inc)))

              (mark-slot-updated cached slot-name)
              (mark-slot-updated ob slot-name)
              (setf (slot-value ob slot-name)
                    (std-slot-value cached slot-name))))))
      (when (and *mb-slot-value-debug* slow-path)
        (fresh-line)
        (format t "SLOT-VALUE-USING-CLASS on slot ~A~%" slot)
        (format t "  VAL was ~A~%" val)
        (format t "  ANS was ~A~%END~%" ans))
      ans)))

(defun parse-mb-object-url (url)
  (destructuring-bind (type id) (last (split-sequence #\/ url) 2)
    (values type id)))

(defun url-to-object (url)
  "Take a URL for the web page of some mb-object and return its object."
  (multiple-value-bind (type id) (parse-mb-object-url url)
    (let ((sym (find-symbol (string-upcase type) :mbedit)))
      (unless sym (error "Unrecognised table name: ~A" type))
      (make-instance sym :id id))))
