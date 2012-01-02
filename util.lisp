(in-package :mbcl)

(alexandria:define-constant +mb-namespace+
    "http://musicbrainz.org/ns/mmd-2.0#"
  :test #'string=)

(defun parse-comma-string (str)
  "Split STR into sub-strings separated by commas."
  (let ((acc) (start 0))
    (dotimes (n (length str))
      (when (eq (aref str n) #\,)
        (push (subseq str start n) acc)
        (setf start (1+ n))))
    (push (subseq str start) acc)
    (nreverse acc)))

(defun matches-name (tag-key name &key (namespace +mb-namespace+))
  "Return T iff TAG-KEY matches NAME in the given NAMESPACE."
  (if (not namespace)
      (string= (if (consp tag-key) (car tag-key) tag-key) name)
      (and (consp tag-key)
           (string= (car tag-key) name)
           (string= (cdr tag-key) namespace))))

(defun string-to-slot-name (string)
  "Find the slot-name symbol for STRING (used in PARSE-DEFN)."
  (or (find-symbol (string-upcase string))
      (error "No such symbol to use as slot name: ~A" string)))

(defun get-defn-parser (key value-form whole-tag)
  "Return the form that, when evaluated, should give the parsed version of
VALUE-FORM and also return the name of the tag to match."
  (if (stringp key)
      (values value-form key)
      (values
       (cond
         ((eq (second key) :int)
          `(and ,value-form (parse-integer ,value-form)))
         ((eq (second key) :float)
          `(and ,value-form (float (read-from-string ,value-form))))
         ((eq (second key) :comma)
          `(parse-comma-string ,value-form))
         (t
          `(funcall ,(second key) ,whole-tag)))
       (first key))))

(defun parse-defn (defn value-form whole-tag)
  "Reads DEFN and described in SIMPLE-XML-PARSE-TAG and returns values
SLOT-NAME, KEY-NAME, PARSE-FORM, MANUAL-SETTER for use in READ-DEFN."
  (cond
    ((stringp defn)
     (values (string-to-slot-name defn) defn value-form nil))
    (t
     (destructuring-bind (key &body value) defn
       (multiple-value-bind (parser match-name)
           (get-defn-parser key value-form whole-tag)
         (if (symbolp value)
             (values value match-name parser NIL)
             (values NIL match-name parser value)))))))

(defun read-defn (defn key-form value-form whole-tag attribute?)
  "See documentation for SIMPLE-XML-PARSE-TAG."
  (multiple-value-bind (slot-name key-name parse-form manual-setter)
      (parse-defn defn value-form whole-tag)
    `((MATCHES-NAME ,key-form ,key-name
                    ,@(if attribute? '(:namespace nil) '()))
      ;; If VALUE is NIL, don't do anything: just sticking a NIL in the cond
      ;; branch will be fine.
      ,(when (or slot-name manual-setter)
             `(LET ((VAL ,parse-form))
                ,(if manual-setter
                     `(PROGN ,@manual-setter)
                     `(SETF (SLOT-VALUE IT ',slot-name) VAL)))))))

(defmacro simple-xml-parse-tag (object tag strict? attribute? defns)
  "Parse the xml tag TAG into OBJECT with definitions given by DEFNS. A general
definition consists of a list (KEY . VALUE). KEY should either be a string or a
pair, (NAME TYPE) where TYPE is :comma, :float or :int. If all we should do is
set a string slot with the same name, you can give just the string KEY.

If TYPE is given and is :int or :float, the string is first parsed to the
relevant numerical type, if it isn't NIL (this occurs when you get tags
<blah></blah> for unknown quantities). If TYPE is :comma, we return a list of
substrings separated by commas. If TYPE is anything else, it's FUNCALL'ed to act
as a parser.

Now, if VALUE is just a symbol, we set the slot with that name to whatever we
read (possibly parsed to a number). If VALUE is NIL, the tag gets
ignored (useful to allow STRICT? to be true but not store something). Otherwise,
the rest of the list is treated as a hunk of code to be run. The code runs with
IT bound to the object (a bit like aif, awhen etc.) and VAL bound to the
possibly parsed value. If STRICT? is true and we find a tag we don't recognise,
throw an error."
  (let ((xml (gensym)) (val (gensym)) (att (gensym)))
    `(LET* ((,xml ,tag)
            (,att ,attribute?)
            (,val (if ,att (second ,xml) (third ,xml)))
            (IT ,object))

       ;; Ignoring VAL silences warnings if each given tag uses the full
       ;; XML. Ignoring IT silences warnings when there aren't actually any
       ;; tags.
       (DECLARE (IGNORABLE ,VAL IT))

       (COND
         ,@(mapcar (lambda (defn)
                     (read-defn defn `(first ,xml) `,val xml att))
                   defns)
         ,@(when strict?
                 `((T
                    (ERROR "No matching definition for XML tag: ~A"
                           (FIRST ,xml)))))))))

(defmacro simple-xml-parse (object xml strict?
                            (&body attribute-defns) (&body child-defns))
  "Parse each tag in XML using SIMPLE-XML-PARSE-TAG. Returns OBJECT. The format
of the two &BODY parameters is described in SIMPLE-XML-PARSE-TAG."
  (let ((ob (gensym)) (x (gensym)))
    `(LET ((,ob ,object) (,x ,xml))
       (DOLIST (TAG (SECOND ,x))
         (SIMPLE-XML-PARSE-TAG ,ob tag ,strict? T ,attribute-defns))
       (DOLIST (TAG (CDDR ,x))
         (SIMPLE-XML-PARSE-TAG ,ob tag ,strict? NIL ,child-defns))
       ,ob)))

(defun get-attribute (key attributes)
  "Get the value of an attribute from the parsed XMLS stuff or NIL if it's not
there."
  (second (find key attributes :key #'car :test #'string=)))

(defun shortened-string (str &key (max-length 30))
  (if (> (length str) max-length)
      (concatenate 'string (subseq str 0 (- max-length 3)) "...")
      str))

(defmacro awhen (test &body body)
  "Evaluate TEST. If true, bind the result to IT and run BODY with the
binding."
  `(let ((it ,test)) (when it ,@body)))

(defmacro aif+ (test then &body else)
  `(let ((it ,test)) (if it ,then (progn ,@else))))

(defun hcf (a b)
  "Euclid's algorithm for HCF."
  (if (< a b)
      (hcf b a)
      (loop
         (multiple-value-bind (q r) (floor a b)
           (declare (ignore q))
           (when (= r 0) (return b))
           (setf a b b r)))))

(defun debug-print (x &optional str)
  (format t "~@[~A: ~]~S~%" str x)
  (force-output)
  x)
