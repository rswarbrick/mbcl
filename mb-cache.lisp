(in-package :mbcl)

(defvar *mb-cache* (make-hash-table :test 'equalp)
  "Cache of MB objects, keyed by ID.")

(defun merge-cached-object (mb-object)
  "This takes an object and merges its data with any stored already, saving and
returning the result. If the object has no ID, just return NIL, since there's
not a huge amount we can sensibly do!"
  (awhen (std-slot-value mb-object 'id)
    (let ((hit (gethash it *mb-cache*)))
      (setf (gethash it *mb-cache*)
            (if hit (merge-objects mb-object hit t) mb-object)))))

(defun cached-by-id (mbid) (gethash mbid *mb-cache*))
(defun cached-version (mb-object)
  (awhen (std-slot-value mb-object 'id) (cached-by-id it)))

(defun clear-cache () (clrhash *mb-cache*))
