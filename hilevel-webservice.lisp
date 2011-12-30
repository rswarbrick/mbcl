(in-package :mb-getrecs)

(defun search-request (search-string &key (type "recording") (limit 100) (offset 0))
  "Make an http request for a search and return the XML, parsed by XMLS."
  (unless (member type *mb-entities* :test #'string=)
    (error "Invalid entity type: ~A." type))
  (unless (typep limit '(integer 1 100))
    (error "Invalid limit (~A not integer in [1,100])." limit))
  (unless (typep offset '(integer 0))
    (error "Invalid offset (~A not integer >= 0)." offset))
  (parse-search-results
   (mbws-call type nil
              (list (cons "query" search-string)
                    (cons "limit" (format nil "~D" limit))
                    (cons "offset" (format nil "~D" offset))))))

(defun mb-request (type mbid &key inc)
  "Make a MusicBrainz standard request for the entity referred to by MBID in the
TYPE table. Pass INC as inc=... parameters if given: it can be a list of
strings, which this function can join together, or just a single string."
  (let ((xml (mbws-call type mbid
                        (cond
                          ((not inc) nil)
                          ((stringp inc) (list (cons "inc" inc)))
                          ((consp inc)
                           (list (cons "inc" (format nil "~{~A~^+~}" inc))))
                          (t
                           (error "Invalid format for INC: ~A" inc))))))
    (cond
      ((matches-name (first xml) "metadata")
       (funcall (find-parser (third xml)) (third xml)))

      ((matches-name (first xml) "error" :namespace nil)
       (error 'mb-error :text (third (third xml))))

      (t
       (error "Unknown response format: ~A" xml)))))

(define-condition mb-error (error)
  ((text :initarg :text :reader text))
  (:report (lambda (c s)
             (format s "MB Error: ~A" (text c)))))

(defmethod refresh-object ((mb mb-object) inc)
  (merge-cached-object (mb-request (table-name mb) (id mb) :inc inc)))

(defun cached-get-object (mbid &key table)
  "Get an object with id equal to MBID from the hash table, if one
exists. Otherwise, call the webservice, using the given table. This works fine
with a nonsense table (eg NIL) as long as there's a cache hit."
  (or (gethash mbid *mb-cache*)
      (setf (gethash mbid *mb-cache*) (mb-request table mbid))))

(defvar *mb-slot-value-debug* t
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
             ;; Next, try the cached instance. Maybe that'll be more help.
             ((and (setf slow-path t)
                   (setf cached (gethash (id ob) *mb-cache*))
                   (slot-defined-p cached slot-name))
              (mark-slot-updated ob slot-name)
              (setf (slot-value ob slot-name)
                    (std-slot-value cached slot-name)))
             ;; Rubbish. Looks like we have to make a webservice call.
             (t
              (setf cached (refresh-object ob (mb-class-slot-inc slot)))
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