(in-package :mbcl)

(defvar *mb-cache* (make-hash-table :test 'equalp)
  "Cache of MB objects, keyed by ID.")

(defun merge-cached-object (mb-object)
  "This takes an object and merges its data with any stored already, saving and
returning the result."
  (let ((hit (gethash (id mb-object) *mb-cache*)))
    (setf (gethash (id mb-object) *mb-cache*)
          (if hit (merge-objects mb-object hit t) mb-object))))
