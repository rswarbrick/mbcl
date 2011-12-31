(in-package :mbcl)

(defclass partial-list ()
  ((size :reader size :initarg :size)
   (segments :reader segments :initform nil)
   (updater :accessor updater :initarg :updater :initform nil)
   (page-size :accessor page-size :initarg :page-size))
  (:documentation
   "SEGMENTS should be a list of things like '(OFF . ARR) where OFF is the
beginning of the indices of the whole list that this represents and where ARR is
an array holding that segment of the list. All segments but the last will be of
length PAGE-SIZE, and the last may be shorter.

UPDATER should be a function taking (OFFSET PAGE-SIZE) and returning the page
starting at OFFSET. It's fine if the result is longer than expected (in fact, we
update the current value), but the result should be at least PAGE-SIZE long (or
should be the last page, if that's shorter)."))

(defun make-partial-list (size &key updater (page-size 25))
  (make-instance 'partial-list
                 :size size :updater updater :page-size page-size))

(defmethod pl-present-count ((pl partial-list))
  "Returns the number of elements currently stored."
  (let ((seg-count (length (segments pl))))
    (if (> seg-count 0)
        (+ (* (1- seg-count) (page-size pl))
           (length (cdar (last (segments pl)))))
        0)))

(defmethod print-object ((pl partial-list) stream)
  (print-unreadable-object (pl stream :type t :identity t)
    (format stream "~A in ~A pages of ~A objects"
            (pl-present-count pl) (length (segments pl)) (size pl))))

(defun pl-page-number (pl offset)
  "Return the page in which the element at OFFSET can be found in PL."
  (floor offset (page-size pl)))

(defun pl-pages-required (pl)
  "Return the number of pages required for the full list."
  (1+ (pl-page-number pl (1- (size pl)))))

(defun pagify-list (start lst page-size)
  "Return pages suitable for a partial-list by chopping up LST into pieces of
max length PAGE-SIZE. We also add the offset to the start as a cons, starting
with START."
  (let ((acc))
    (do ((rest lst (nthcdr page-size rest))
         (lenrest (length lst) (- lenrest page-size))
         (start start (+ start page-size)))
        ((null rest) (nreverse acc))
      (push
       (cons start (replace (make-array (min page-size lenrest)) rest)) acc))))

(defun split-segments (pl newsize)
  "Split the given segment list into pages of size newsize (which must divide
the current page size)."
  (unless (= (mod (page-size pl) newsize) 0)
    (error "NEWSIZE (~A) isn't a factor of the current page size (~A)."
           newsize (page-size pl)))
  (mapcan (lambda (segment)
            (loop
               for internal from 0 below (page-size pl) by newsize
               for off = (+ (car segment) internal)
               collecting (cons off
                                (subseq (cdr segment)
                                        internal (+ internal newsize)))))
          (segments pl)))

(defun merge-segments (segs1 segs2)
  (delete-duplicates (merge 'list segs1 segs2 #'< :key #'car) :key #'car))

(defun merge-partial-lists (pl1 pl2)
  "Merge two partial lists, which both should represent the same sequence. Take
whichever updater is non-nil, or the one for PL1 if both are."
  (unless (= (size pl1) (size pl2))
    (error "PL1 and PL2 have different sizes."))
  (let* ((newsize (hcf (page-size pl1) (page-size pl2)))
         (pl (make-partial-list
              (size pl1)
              :updater (or (updater pl1) (updater pl2))
              :page-size newsize)))
    (setf (slot-value pl 'segments)
          (merge-segments (split-segments pl1 newsize)
                          (split-segments pl2 newsize)))
    pl))

(defun pl-store-segment (pl offset lst)
  "Store the given list in PL at the given offset in pages that are of
length (PAGE-SIZE PL). If OFFSET is not a multiple of the page size, it throws
an error.

No error is thrown if LST is not a multiple to allow a list with arbitrary total
length. However the code does check that, if a segment is passed which only
partially fills a page then this should be the last page of PL."
  (unless (= 0 (mod offset (page-size pl)))
    (error "OFFSET (~A) not a multiple of PAGE-SIZE (~A)."
           offset (page-size pl)))
  (let* ((new-pages (pagify-list offset lst (page-size pl)))
         (new-page-count (length new-pages))
         (last-new-page (car (last new-pages))))

    (unless (or (< (size pl) (+ offset (* new-page-count (page-size pl))))
                (= (page-size pl) (length (cdr last-new-page))))
      (error "Internal partial page requested."))
    (setf (slot-value pl 'segments)
          (merge-segments (segments pl) new-pages))
    (values)))

(defun pl-update-page (pl page)
  (unless (updater pl) (error "PL needs an UPDATER to find new pages."))
  (let* ((offset (* page (page-size pl)))
         (result (funcall (updater pl) offset (page-size pl))))
    (cond
      ((listp result)
       (pl-store-segment pl offset result))
      ((typep result 'partial-list)
       (let ((tmp (merge-partial-lists pl result)))
         (setf (slot-value pl 'segments) (segments tmp)
               (slot-value pl 'page-size) (page-size tmp)))
       (values))
      (t
       (error "Don't know how to make sense of result ~A~%" result)))))

(defun pl-get-page (pl page)
  (find (* page (page-size pl)) (segments pl) :key #'car))

(defun pl-ensure-page (pl page)
  (unless (pl-get-page pl page)
    (pl-update-page pl page)
    (unless (pl-get-page pl page)
      (error "Failed to ensure page ~A for ~A" page pl)))
  (values))

(defun pl-nth (pl n)
  "Get the nth element of the partial list, updating if necessary."
  (unless (< -1 n (size pl))
    (error "Invalid index: ~A (PL has length ~A)." n (size pl)))
  (multiple-value-bind (page offset) (floor n (page-size pl))
    (pl-ensure-page pl page)
    (aif+ (pl-get-page pl page)
        (aref (cdr it) offset)
      (error "PL-ENSURE-PAGE didn't :-("))))

(defun pl-ensure-all (pl)
  (dotimes (n (pl-pages-required pl))
    (pl-ensure-page pl n)))

(defun pl-as-array (pl)
  (pl-ensure-all pl)
  (let ((arr (make-array (size pl))))
    (loop
       for segment in (segments pl)
       for page-no from 0
       doing (replace arr (cdr segment) :start1 (* page-no (page-size pl))))
    arr))

(defun pl-as-list (pl)
  (coerce (pl-as-array pl) 'list))

;; A simple test of the above. The double pages returned check that we deal
;; correctly with doubled pages. (Call at 13, then at 12).
;;
;; (defparameter *test*
;;   (let ((len 1000) (ps 13))
;;     (make-partial-list
;;      len
;;      :updater (lambda (offset page-size)
;;                 (format t "Update at offset: ~A~%" offset)
;;                 (loop for i from offset
;;                    below (min (+ offset (* 2 page-size)) len)
;;                    collecting i))
;;      :page-size ps)))
;;
;; More impressive/relevant:
;;
;; MBCL> (defparameter *mozart*
;;         (make-instance 'artist :id "b972f589-fb0e-474e-b64a-803b0364fa75"))
;; *MOZART*
;; MBCL> (releases *mozart*)
;; #<PARTIAL-LIST 25 in 1 pages of 2038 objects {BA63CD1}>
;; MBCL> (pl-nth (releases *mozart*) 1046)
;; #<RELEASE 'The Essential Mozart' {BB95F81}>
;; MBCL> (releases *mozart*)
;; #<PARTIAL-LIST 125 in 5 pages of 2038 objects {BA63CD1}>

