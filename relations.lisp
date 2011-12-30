(in-package :mbcl)

(defclass relations-list ()
  ((parent
    :reader parent :initarg :parent :documentation
    "The parent object: needed in order to grab relations of types we hadn't yet
    seen.")
   (relations
    :reader relations :initform nil :initarg :relations :documentation
    "An assoc of lists of relations, indexed by class of target (eg ARTIST)")))

(defparameter *relation-types*
  '((artist . "artist-rels")
    (label . "label-rels")
    (recording . "recording-rels")
    (release . "release-rels")
    (release-group . "release-group-rels")
    (nil . "url-rels")
    (work . "work-rels")))

(defun make-relations-list (parent &key assoc)
  (make-instance 'relations-list :parent parent :relations assoc))

(defmethod print-object ((rl relations-list) stream)
  (print-unreadable-object (rl stream :type t :identity t)
    (format stream "(~{~{(~A: ~A)~}~^, ~})"
            (mapcar (lambda (pair) (list (car pair) (length (cdr pair))))
                    (relations rl)))))

(defmethod merge-slot-values ((target-object mb-object)
                              (new relations-list) (old relations-list))
  (unless (string= (id (parent new)) (id (parent old)))
    (error "Cannot merge relations for two objects that aren't equal (~A, ~A)"
           (id (parent new)) (id (parent old))))
  (make-relations-list (parent new)
                       :assoc (delete-duplicates
                               (append (relations old) (relations new))
                               :key #'car)))

(defun store-relation-segment (relations-list list-segment)
  "LIST-SEGMENT should be a list of relations, got from some web service
call. Store its contents into RELATIONS-LIST under the correct heading."
  ;; The web service irritatingly gives the same container XML for each
  ;; relationship target type (artist, work, ...) but clearly stores them
  ;; separately. On the plus side, it doesn't page the results so we don't have
  ;; to worry about storing LIST-SEGMENT objects or merging them.
  (let ((contents (contents list-segment)))
    (when contents
      (let* ((cls (class-name (class-of (target (first contents)))))
             (hit (assoc cls (relations relations-list))))
        (if hit
            (setf (cdr hit) contents)
            (setf (slot-value relations-list 'relations)
                  (push (cons cls contents)
                        (slot-value relations-list 'relations)))))))
  (values))

(defgeneric relations-of-type (object &key class)
  (:documentation
   "Return the relations between the given OBJECT whose targets are of type
CLASS. Set CLASS to T (the default) to get all relations (NIL means URLs)."))

(defmethod relations-of-type ((mb mb-object) &key (class t))
  (if (eq class t)
      (reduce #'append *relation-types*
              :key (lambda (pair)
                     (relations-of-type (relations mb) :class (car pair)))
              :initial-value nil)
      (relations-of-type (relations mb) :class class)))

(defmethod relations-of-type ((relations relations-list) &key (class t))
  "Return a list of all relations of type CLASS from the given set of
relations. Performs a call to the web service if we don't have them yet."
  (let ((str (cdr (assoc class *relation-types*)))
        (hit (assoc class (relations relations)))
        cached cached-relations)
    (when (eq class t)
      (error "Can't deal with CLASS = T at the moment: call the parent."))
    (unless str
      (error "No such type: ~A" class))
    (unless (id (parent relations))
      (error "Relationship list has no ID, so can't update."))

    (cond
      ;; If this has the relevant relations, goody good.
      (hit (cdr hit))
      ;; Otherwise, get our cached version. If it's got them, update us too and
      ;; return them.
      ((and (setf cached (cached-version (parent relations)))
            (setf cached-relations (relations cached))
            (setf hit (assoc class (relations cached-relations))))
       (push hit (slot-value relations 'relations))
       (cdr hit))
      ;; Huh, rubbish. Well, we'd better do the web thang.
      (t
       (setf cached-relations
             (relations
              (merge-cached-object (mb-request (table-name (parent relations))
                                               (id (parent relations))
                                               :inc str))))
       (setf hit (assoc class (relations cached-relations)))
       (push (or hit (cons class nil))
             (slot-value relations 'relations))
       (cdr hit)))))
