;;;; objects.lisp --- Pictures, annotations, forms, layers, pages across saves, actions, embedded files, encryption.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:palimpsest)

;;; Pictures

(defun sorted-position-after (numbers limit &key below)
  "How many of NUMBERS, a vector sorted from the least, are no more than
LIMIT; with BELOW, how many are less than LIMIT."
  (let ((low 0) (high (length numbers)))
    (loop while (< low high)
          do (let ((middle (floor (+ low high) 2)))
               (if (if below (< (aref numbers middle) limit) (<= (aref numbers middle) limit))
                   (setf low (1+ middle))
                   (setf high middle))))
    low))

(defun picture-jpeg-sections (pdf revision mark)
  "What the JPEG picture MARK of REVISION draws says of itself, as the
sections JPEG-METADATA gives (its EXIF and comments), read as data and
never as a picture; NIL when it is no JPEG or says nothing."
  (let ((filters (mapcar #'car (pdf-stream-filters (mark-actual mark)))))
    (when (and filters (mark-object mark) (jpeg-filter-p (car (last filters))))
      (let* ((object (revision-object pdf revision (mark-object mark)))
             (data (and object (if (rest filters) (stream-data pdf object) (raw-stream pdf object)))))
        (and data (remove "" (ignore-errors (jpeg-metadata data)) :test #'string=))))))

(defun check-images (pdf revision)
  "Pictures unlike the others, pictures added later, and what JPEG pictures say of the programs that made them."
  (let ((images (pictures-of pdf revision)) (out '()))
    (flet ((props (m) (let ((d (mark-actual m)))
                        (list (format nil "~{~a~^ ~}" (mapcar #'car (pdf-stream-filters d)))
                              (let ((cs (pdf-get d "/ColorSpace")))
                                (cond
                                  ((stringp cs) cs)
                                  ((pdf-ref-p cs)
                                   (let ((v (deref pdf revision cs)))
                                     (if (stringp v) v (princ-to-string (first (pdf-array-items v))))))
                                  (t (princ-to-string (first (pdf-array-items cs))))))
                              (pdf-get d "/BitsPerComponent"))))
           (dpi (m) (let ((w (pdf-get (mark-actual m) "/Width")) (placed (mark-width m)))
                      (and (realp w) (> placed 1) (/ w (/ placed 72))))))
      (let ((ways '()))
        ;; each way a picture is kept unlike the others, (PICTURE WORDS RULE)
        (when (>= (length images) 3)
          (loop with all = (mapcar (lambda (i) (props (first i))) images)
                for k below 3 for what in '("compression" "colour space" "bit depth")
                do (let ((values (map 'vector (lambda (p) (nth k p)) all))
                         (counts (make-hash-table :test 'equal)))
                     (loop for v across values do (incf (gethash v counts 0)))
                     (loop for i in images for v across values
                           ;; a value no other picture has, while nearly all the others share one
                           for other = (if (equal (aref values 0) v) (aref values 1) (aref values 0))
                           when (and (= 1 (gethash v counts)) (>= (gethash other counts) (* 2/3 (1- (length values)))))
                             do (push (list i (format nil "a ~a (~a)" what v)
                                            (concatenate 'string "pictures made by one program for one document "
                                                         "share their compression, colour space and bit depth"))
                                      ways))))
          (let* ((each (mapcar (lambda (i) (dpi (first i))) images))
                 (resolutions (sort (coerce (remove nil each) 'vector) #'<)))
            (when (plusp (length resolutions))
              (let ((middle (aref resolutions (floor (length resolutions) 2))))
                (loop for i in images for r in each
                      ;; no other picture's resolution from half its own to twice it
                      when (and r
                                (= 1
                                   (- (sorted-position-after resolutions (* 2 r))
                                      (sorted-position-after resolutions (/ r 2) :below t))))
                        do (push (list i (format nil "a resolution of about ~d pixels to the inch (the others about ~
                                                      ~d)"
                                                 (round r) (round middle))
                                       "pictures of one scan or one program share a resolution")
                                 ways))))))
        (dolist (i (remove-duplicates (mapcar #'first ways) :from-end t))
          (let ((own (reverse (remove i ways :key #'first :test-not #'eq))) (m (first i)))
            (push (found :record (format nil "The picture ~a (object ~d) on page ~d has ~{~a~^, ~} unlike the other ~
                                              pictures: kept otherwise, as a scanner's layers or a grey page among ~
                                              colour ones are, or put in from elsewhere, as a signature, stamp or ~
                                              logo is; for the record."
                                         (mark-text m) (mark-object m) (1+ (page-index (second i)))
                                         (mapcar #'second own))
                         :rule (format nil "~@(~{~a~^; ~}~). Honest scans keep their pictures so too, a scanner's ~
                                            layers or grey pages beside colour ones, so a picture kept otherwise is ~
                                            for the record; one added in a later save is worth a look."
                                       (remove-duplicates (mapcar #'third own) :test #'string=))
                         :revision (revision-index revision)
                         :page (1+ (page-index (second i)))
                         :objects (list (mark-object m))
                         :evidence (append (nth-value 0 (object-evidence pdf revision (mark-object m)))
                                           (mark-evidence pdf revision m "the picture"))
                         :method :picture-pasted)
                  out))))
      (dolist (i images)
        (destructuring-bind (m page) i
          (let ((written (written-in pdf revision (mark-object m))))
            (when (and written (plusp (revision-index written)))
              (push (found :look (format nil "The picture ~a (object ~d) on page ~d was added in ~a."
                                         (mark-text m)
                                         (mark-object m)
                                         (1+ (page-index page))
                                         (revision-name written))
                           :rule (concatenate 'string "A picture whose object was written in a later save was put on "
                                                      "the page after it was made.")
                           :revision (revision-index written)
                           :page (1+ (page-index page))
                           :objects (list (mark-object m))
                           :evidence (nth-value 0 (object-evidence pdf revision (mark-object m)))
                           :method :picture-pasted :kind :picture-added)
                    out)))
          (let ((sections (picture-jpeg-sections pdf revision m)))
            (when sections
              (let ((later (picture-dated-later pdf revision m page sections)))
                (when later (push later out)))
              (let* ((text (format nil "~{~a~}" sections))
                     (tools (tools-in text)))
                (push (found (if tools :look :record)
                             (format nil "The JPEG picture ~a (object ~d) on page ~d keeps metadata of its own~@[, ~
                                          naming ~{~a~^, ~}~]: it was made or edited elsewhere."
                                     (mark-text m) (mark-object m) (1+ (page-index page)) (mapcar #'second tools))
                             :kind :picture-metadata
                             :rule (concatenate 'string "A JPEG made by a camera or a picture editor keeps EXIF "
                                                        "and comments; read as data, never as a picture.")
                             :revision (revision-index revision)
                             :page (1+ (page-index page))
                             :objects (list (mark-object m))
                             :details (loop for line in (uiop:split-string text :separator '(#\Newline))
                                            when (plusp (length line))
                                            collect line)
                             :evidence (nth-value 0 (object-evidence pdf revision (mark-object m)))
                             :method :picture-pasted)
                      out))))))
      out)))

;;; Annotations, forms, layers

(defparameter *content-like-annotations*
  '("/FreeText" "/Stamp" "/Square" "/Circle" "/Ink" "/Polygon" "/PolyLine" "/Line" "/Redact" "/Caret")
  "The kinds of annotation that can look like part of the page.")

(defun annotation-lines (pdf revision page)
  "What is laid over PAGE in REVISION, a line for each annotation but a popup:
its kind, what it says (a form field its name and value), its colours for a
shape, where it lies, and whether it is hidden; so that two revisions' can be
set side by side, as their text is."
  (loop for value in (page-annots page)
        for annot = (deref pdf revision value)
        for subtype = (and (pdf-dictionary-p annot) (pdf-get annot "/Subtype"))
        when (and (stringp subtype) (not (string= subtype "/Popup")))
          collect (let* ((box (box-of (deref pdf revision (pdf-get annot "/Rect"))))
                         (contents (let ((c (dref pdf revision annot "/Contents")))
                                     (and (stringp c) (plusp (length c)) c)))
                         (field (let ((name (dref pdf revision annot "/T"))) (and (stringp name) name)))
                         (stamp (let ((name (pdf-get annot "/Name"))) (and (stringp name) (subseq name 1))))
                         (fill (let ((c (deref pdf revision (pdf-get annot "/IC"))))
                                 (and (consp c) (every #'realp c) c)))
                         (flags (let ((f (pdf-get annot "/F"))) (if (integerp f) f 0))))
                    (format nil "~a~@[ ~a~]~@[ field ~s~]~@[ = ~s~]~@[ ~s~]~@[, filled ~a~]~@[ at ~{~d,~d to ~
                                 ~d,~d~}~]~:[~;, hidden~]"
                            (subseq subtype 1) stamp field (and field (field-value pdf revision annot))
                            (and contents (shorten-string contents 100 :one-line t))
                            (and fill (= (length fill) 3) (colour-words fill))
                            (and box (mapcar #'round box))
                            (plusp (logand flags (logior 1 2 32)))))))

(defun check-annotations (pdf revision)
  "Annotations that look like the page, added later, hidden, or marked for redaction and never applied."
  (let ((out '()))
    (dolist (page (pages-of pdf revision))
      (dolist (value (page-annots page))
        (guarding-item (out "an annotation of page ~d" (1+ (page-index page)))
          (let* ((annot (deref pdf revision value)) (number (pdf-ref-p value))
                 (subtype (pdf-get annot "/Subtype")) (flags (let ((f (pdf-get annot "/F"))) (if (integerp f) f 0)))
                 (p (1+ (page-index page)))
                 (written (and number (written-in pdf revision number)))
                 (later (and written (plusp (revision-index written))))
                 (evidence (and number (nth-value 0 (object-evidence pdf revision number))))
                 (contents (let ((c (dref pdf revision annot "/Contents")))
                             (and (stringp c) (shorten-string c 80 :one-line t)))))
            (when (and (stringp subtype) (member subtype *content-like-annotations* :test #'string=))
              (push (found (if later :look :record)
                           (format nil "A ~a annotation~@[ (object ~d)~] lies on page ~d~:[~;~:*, added in ~a~]~@[, ~
                                        saying ~s~]: it can look like part of the page, but is laid over it."
                                   (subseq subtype 1) number p (and later (revision-name written)) contents)
                           :rule (concatenate 'string "Annotations are drawn over a page, not part of its content; "
                                                      "text boxes, stamps, shapes and ink can pass for the page "
                                                      "itself.")
                           :revision (if written (revision-index written) (revision-index revision))
                           :page p
                           :objects (and number (list number))
                           :evidence evidence :method :annotation-overlay)
                    out))
            (when (name-p subtype "/Redact")
              (push (found :look (format nil "A redaction on page ~d was marked but never applied: what it covers is ~
                                              still in the page." p)
                           :rule (concatenate 'string "A Redact annotation marks what is to be removed; until it is "
                                                      "applied, the content beneath stays.")
                           :revision (revision-index revision)
                           :page p
                           :objects (and number (list number))
                           :evidence evidence)
                    out))
            (when (plusp (logand flags (logior 1 2 32)))
              (push (found :look (format nil "A ~a annotation on page ~d is hidden from view (its flags are ~d)."
                                         (if (stringp subtype) (subseq subtype 1) "")
                                         p
                                         flags)
                           :rule "An annotation with its Invisible, Hidden or NoView flag set is not shown."
                           :revision (revision-index revision) :page p :objects (and number (list number))
                           :evidence (and number (key-evidence pdf revision number "/F")) :method :hidden-text)
                    out))))))
    out))

(defun field-value (pdf revision dict)
  "The value of the form field DICT, as text."
  (let ((v (dref pdf revision dict "/V")))
    (cond
      ((null v) nil)
      ((stringp v) v)
      ((pdf-dictionary-p v) (if (pdf-get v "/ByteRange") "(a signature)" "(a dictionary)"))
      (t (princ-to-string v)))))

(defun check-forms (pdf)
  "Form fields whose values changed from one save to the next, and XFA forms."
  (let ((out '()))
    (loop for r across (pdf-revisions pdf)
          for before = nil then previous
          for previous = r
          do (when before
               (let ((old (make-hash-table :test 'equal)))
                 (dolist (field (reverse (form-fields pdf before))) (setf (gethash (first field) old) field))
                 (dolist (field (form-fields pdf r))
                   (destructuring-bind (name number dict) field
                     (let* ((then (gethash name old))
                            (a (and then (field-value pdf before (third then)))) (b (field-value pdf r dict)))
                       (when (and then (not (equal a b)))
                         (push (found :look (format nil "The form field ~s changed in ~a: ~s, before ~s."
                                                    name
                                                    (revision-name r)
                                                    b
                                                    a)
                                      :rule (concatenate 'string "A field's /V is its value; a later save with another "
                                                                 "value changed what the form says.")
                                      :revision (revision-index r) :objects (and number (list number))
                                      :evidence (and number (append (key-evidence pdf r number "/V")
                                                                    (key-evidence pdf before (second then) "/V"))))
                               out)))))))
             (let ((form (dref pdf r (catalogue pdf r) "/AcroForm")))
               (when (and (pdf-get form "/XFA") (= (revision-index r) (1- (length (pdf-revisions pdf)))))
                 (push (found :look (format nil "The form is an XFA form: what it shows depends on the program reading ~
                                                 it, and can differ from the PDF's own pages.")
                              :rule (concatenate 'string "An /XFA entry in the form holds an XML form that some "
                                                         "readers draw in place of the pages.")
                              :revision (revision-index r))
                       out))))
    out))

(defun check-layers (pdf revision)
  "Layers (optional content groups) off as the file opens."
  (let ((hidden (hidden-layers pdf revision)))
    (when hidden
      (list (found :look (format nil "~d layer~:p ~:*~[~;is~:;are~] off as the file opens (~{~a~^, ~}): content there ~
                                      is not shown."
                                 (length hidden)
                                 (mapcar (lambda (n) (let ((name (dref pdf
                                                                       revision
                                                                       (deref pdf revision (list :ref n 0))
                                                                       "/Name")))
                                                       (if (stringp name) name (format nil "object ~d" n)))) hidden))
                   :rule (concatenate 'string "The catalogue's /OCProperties /D /OFF (or /BaseState /OFF) lists "
                                              "optional content that is hidden as standard.")
                   :revision (revision-index revision) :objects hidden
                   :evidence (loop for n in hidden repeat 6 append (nth-value 0 (object-evidence pdf revision n))))))))

;;; Pages across saves

(defun check-pages (pdf)
  "Pages added, removed or put in another order between saves; pages unlike the others; page labels changed."
  (let ((out '()))
    (loop for r across (pdf-revisions pdf)
          for before = nil then previous
          for previous = r
          do (job-check-stop)
             (when before
               (let ((old (remove nil (mapcar #'page-number (pages-of pdf before))))
                     (new (remove nil (mapcar #'page-number (pages-of pdf r)))))
                 (let ((added (numbers-not-in new old)) (removed (numbers-not-in old new)))
                   (when (or added removed)
                     (push (found :look (format nil "~@(~a~) ~@[adds ~d page~:p~]~:[~; and ~]~@[removes ~d page~:p~]: ~
                                                     the document now has ~d."
                                                (revision-name r)
                                                (and added (length added))
                                                (and added removed)
                                                (and removed (length removed))
                                                (length new))
                                  :rule (concatenate 'string "Each save's page tree lists its pages; set beside the "
                                                             "save before, pages appear and go.")
                                  :revision (revision-index r)
                                  :objects (sort (append added removed) #'<)
                                  :method :pages-changed)
                           out))
                   (when (and (null added) (null removed) (not (equal old new)))
                     (push (found :look (format nil "~@(~a~) puts the pages in another order." (revision-name r))
                                  :rule "The order of a page tree's kids is the order of the pages."
                                  :revision (revision-index r) :objects new :method :pages-changed)
                           out))))
               (let ((a (princ-to-string (dref pdf before (catalogue pdf before) "/PageLabels")))
                     (b (princ-to-string (dref pdf r (catalogue pdf r) "/PageLabels"))))
                 (unless (string= a b)
                   (push (found :look (format nil "~@(~a~) changes the page labels (the page numbers the reader shows)."
                                              (revision-name r))
                                :rule "The catalogue's /PageLabels names the numbers pages are shown with."
                                :revision (revision-index r))
                         out)))))
    (let* ((last (last-revision pdf)) (pages (pages-of pdf last)))
      (when (>= (length pages) 3)
        (flet ((odd (key words)
                 ;; a page is odd when its value is its own and every other page shares one value
                 (let ((counts (make-hash-table :test 'equal)) (values (mapcar key pages)))
                   (dolist (v values) (incf (gethash v counts 0)))
                   (job-check-stop)
                   (loop for page in pages for v in values
                         when (and (= 2 (hash-table-count counts)) (= 1 (gethash v counts)))
                         do (push (found :look (format nil "Page ~d has a ~a (~a) unlike every other page: put in from ~
                                                            another document." (1+ (page-index page)) words v)
                                         :rule "Pages of one document share their size and rotation."
                                         :revision (revision-index last)
                                         :page (1+ (page-index page))
                                         :objects (and (page-number page) (list (page-number page)))
                                         :evidence (and (page-number page)
                                                        (nth-value 0 (object-evidence pdf last (page-number page))))
                                         :method :pages-changed :kind :page-size)
                                  out)))))
          (odd
            (lambda (p)
              (format nil "~{~d~^ by ~}"
                      (list (round (- (third (page-media p)) (first (page-media p))))
                            (round (- (fourth (page-media p)) (second (page-media p)))))))
            "size")
          (odd (lambda (p) (format nil "~d°" (page-rotate p))) "rotation"))
        (let ((faces (mapcar (lambda (page)
                               (job-check-stop)
                               (remove-duplicates (loop for (nil . value)
                                                          in (page-font-entries pdf last page)
                                                        collect (font-base (font-of pdf last value)))
                                                  :test #'string=))
                             pages)))
          ;; a page is put in when it alone shares no font with any other page
          (when (every #'identity faces)
            (let ((pages-of-font (make-hash-table :test 'equal)))
              (dolist (f faces)
                (dolist (font f) (incf (gethash font pages-of-font 0))))
              (let ((alone (loop for page in pages for f in faces
                                 when (every (lambda (font) (= 1 (gethash font pages-of-font))) f)
                                   collect (cons page f))))
                (when (= 1 (length alone))
                  (destructuring-bind (page . f) (first alone)
                    (push (found :look (format nil "Page ~d shares no font with any other page (~{~a~^, ~}): put in ~
                                                    from another document." (1+ (page-index page)) f)
                                 :rule "Pages of one document share their fonts."
                                 :revision (revision-index last) :page (1+ (page-index page)) :method :pages-changed)
                          out)))))))))
    out))

(defun numbers-not-in (numbers others)
  "Those of NUMBERS not among OTHERS, in their order."
  (let ((there (make-hash-table)))
    (dolist (n others) (setf (gethash n there) t))
    (remove-if (lambda (n) (gethash n there)) numbers)))

;;; Actions, embedded files, encryption

(defun dictionaries-within (value)
  "VALUE, when it is a dictionary, and every dictionary written inside it, in
its dictionaries and arrays, outermost first; not those it refers to, which
are objects of their own. Followed no deeper than 32, as a file may nest
without end."
  (let ((found '()))
    (labels ((walk (v depth)
               (when (< depth 32)
                 (cond ((pdf-dictionary-p v)
                        (push v found)
                        (loop for (nil . inner) in v do (walk inner (1+ depth))))
                       ((and (consp v) (not (pdf-ref-p v)))
                        (dolist (inner (pdf-array-items v)) (walk inner (1+ depth))))))))
      (walk value 0))
    (nreverse found)))

(defun check-actions (pdf revision)
  "JavaScript, actions on opening, programs launched, addresses, forms sent
and files embedded: named, never run or opened. An action is found as an
object of its own and written inside another, as a link's /A is."
  (let ((acts '()) (files '()) (failed '()))
    (dolist (n (live-numbers pdf revision))
      (guarding-item (failed "the actions of object ~d" n)
        (let* ((object (revision-object pdf revision n)) (value (and object (pdf-indirect-get object :value))))
          (when (pdf-dictionary-p value)
            (dolist (action (dictionaries-within value))
              (let ((kind (pdf-get action "/S")))
                (when (and (stringp kind) (member kind '("/JavaScript"
                                                        "/Launch"
                                                        "/URI"
                                                        "/SubmitForm"
                                                        "/ImportData"
                                                        "/GoToR"
                                                        "/GoToE"
                                                        "/RichMediaExecute") :test #'string=))
                  (push (list
                          n
                          (subseq kind 1)
                          (let ((u (or (pdf-get action "/URI") (pdf-get action "/F") (pdf-get action "/JS"))))
                            (and (stringp u) (shorten-string u 100 :one-line t))))
                    acts))))
            (when (pdf-get value "/OpenAction") (push (list n "OpenAction" nil) acts))
            (when (and (pdf-get value "/AA") (not (pdf-get value "/FT")))
              (push (list n "AA (actions on events)" nil) acts))
            (when (or (name-p (pdf-get value "/Type") "/EmbeddedFile") (pdf-get value "/EF"))
              (unless (name-p (pdf-get value "/Type") "/EmbeddedFile")
                (let* ((ef (dref pdf revision value "/EF"))
                       (stream-ref (or (pdf-get ef "/UF") (pdf-get ef "/F")))
                       (stream (and (pdf-ref-p stream-ref) (revision-object pdf revision (second stream-ref))))
                       (size (or (pdf-get (dref pdf revision (and stream (pdf-indirect-get stream :value)) "/Params")
                                          "/Size")
                                 (and stream
                                      (pdf-indirect-get stream :stream-start)
                                      (- (pdf-indirect-get stream :stream-end)
                                         (pdf-indirect-get stream :stream-start))))))
                  (push (list n (let ((name (or (pdf-get value "/UF") (pdf-get value "/F"))))
                                  (if (stringp name) name "(no name)")) size
                              (and stream (written-in pdf revision (pdf-indirect-get stream :number))))
                        files))))))))
    (append failed
     (loop for (strength kinds one many) in '((:look ("JavaScript"
                                                      "Launch"
                                                      "SubmitForm"
                                                      "ImportData"
                                                      "RichMediaExecute"
                                                      "AA (actions on events)")
                                               "that runs code, starts programs or sends data"
                                               "that run code, start programs or send data")
                                              (:record ("URI" "GoToR" "GoToE" "OpenAction")
                                               "that goes to places, in the file or outside it"
                                               "that go to places, in the file or outside it"))
           for some = (reverse (remove-if-not (lambda (a) (member (second a) kinds :test #'string=)) acts))
           when some
             collect (found strength
                            (format nil "The file holds ~d action~:p ~a (~{~a~^, ~}), named here and never run."
                                    (length some)
                                    (if (= 1 (length some)) one many)
                                    (remove-duplicates (mapcar #'second some) :test #'string= :from-end t))
                            :rule (concatenate 'string "Actions and JavaScript in a PDF act when it is opened or "
                                                       "clicked; they are named here, never run.")
                            :revision (revision-index revision) :objects (mapcar #'first some)
                            :details (loop for (n kind what) in some
                                           repeat 30
                                           collect (format nil "object ~d: ~a~@[ ~s~]" n kind what))
                            :evidence (loop for (n) in some
                                            repeat 10
                                            append (nth-value 0 (object-evidence pdf revision n)))))
     (when files
       (setf files (nreverse files))
       (list (found (if (some (lambda (f) (and (fourth f) (plusp (revision-index (fourth f))))) files) :look :record)
                    (format nil "The file holds ~d embedded file~:p, never opened here: ~{~a~^, ~}."
                            (length files)
                            (mapcar #'second files))
                    :rule "A file specification with /EF keeps another file inside the PDF."
                    :revision (revision-index revision) :objects (mapcar #'first files)
                    :details (loop for (n name size written) in files
                                   collect (format nil "object ~d: ~a~@[, ~:d octets~]~@[, added in revision ~d~]"
                                                   n
                                                   name
                                                   size
                                                   (and written
                                                        (plusp (revision-index written))
                                                        (1+ (revision-index written)))))
                    :evidence (loop for (n) in files
                                    repeat 10
                                    append (nth-value 0 (object-evidence pdf revision n)))))))))

(defun permission-words (p)
  "The permissions P, an encryption's /P, in words: what it does not allow."
  (let ((denied (loop for (bit words) in '((3 "printing")
                                          (4 "changing")
                                          (5 "copying")
                                          (6 "annotating")
                                          (9 "filling forms")
                                           (10 "extracting for accessibility") (11 "assembling") (12 "printing well"))
                      unless (logbitp (1- bit) p) collect words)))
    (if denied (format nil "it does not allow ~{~a~^, ~}" denied) "it allows everything")))

(defun check-encryption (pdf)
  "Encryption and its permissions, and encryption added in a later save."
  (let ((out '()))
    (loop for r across (pdf-revisions pdf)
          for before = nil then previous
          for previous = r
          do (let ((encrypt (dref pdf r (revision-trailer r) "/Encrypt")))
               (when (and encrypt (or (null before) (not (pdf-get (revision-trailer before) "/Encrypt"))))
                 (push (found (if before :look :record)
                              (format nil "~:[The file is encrypted~;~:*~@(~a~) encrypts the file~]~@[ (~{~a~^, ~})~]~
                                           ~@[; ~a~]: its streams and strings are not read here."
                                      (and before (revision-name r))
                                      (let ((filter (pdf-get encrypt "/Filter")) (version (pdf-get encrypt "/V"))
                                            (revision (pdf-get encrypt "/R")))
                                        (remove nil (list (and (stringp filter) (subseq filter 1))
                                                          (and version (format nil "version ~a" version))
                                                          (and revision (format nil "revision ~a" revision)))))
                                      (let ((p (pdf-get encrypt "/P"))) (and (integerp p) (permission-words p))))
                              :rule "A trailer with /Encrypt names how the file's strings and streams are encrypted."
                              :revision (revision-index r)
                              :evidence (list (format nil "/Encrypt in the trailer at ~a"
                                                      (offset-words (or (revision-trailer-at r) 0)))))
                       out))))
    out))

(defun check-unended-streams (pdf)
  "A %%EOF lying within a stream whose end the file does not confirm: worth a
look, as a stream left open can hide a save from a reader that takes it
whole, or a save can be cut into the middle of a stream."
  (loop for (eof . number) in (getf (pdf-cache pdf) :eofs-in-unended-streams)
        for revision = (revision-of-offset pdf eof)
        collect (found :look (format nil "A %%EOF at ~a lies within the stream of object ~d, whose end the file ~
                                          does not confirm: the save it ends is read as a save, but a reader taking ~
                                          the stream to its endstream would not see it."
                                     (offset-words eof) number)
                       :rule "A stream's /Length reaches its endstream; one that does not leaves where it ends unknown."
                       :revision (and revision (revision-index revision))
                       :evidence (list (format nil "%%EOF at ~a, within object ~d's stream"
                                               (offset-words eof) number)))))

(defun check-lying-streams (pdf)
  "A stream whose /Length reaches an endstream, so that its end looks
confirmed, while a cross-reference section of the file itself, or the
%%EOF ending its save, lies within it (PDF-EOF-LINES): a strong sign, as
the length was written to hide a save from a reader that takes the stream
whole."
  (loop for (at . number) in (getf (pdf-cache pdf) :streams-lying)
        for revision = (revision-of-offset pdf at)
        collect (found :strong (format nil "The stream of object ~d runs, by its /Length, over ~a, where a save of ~
                                            the file itself is: a length written to hide that save from a reader ~
                                            taking the stream whole."
                                       number (offset-words at))
                       :rule (concatenate 'string
                                          "A stream's /Length ends it; a save of the file, one its trailers name "
                                          "or whose startxref and entries name the file's own places, lies "
                                          "outside every stream.")
                       :revision (and revision (revision-index revision))
                       :evidence (list (format nil "~a, within object ~d's stream" (offset-words at) number)))))

(defparameter *most-inferred-saves* 5
  "The most saves inferred within streams (CHECK-SAVE-SIGNS) read and set
beside the save they lie in, for one PDF.")

(defun inferred-save-changes (pdf number eof)
  "The pages that read otherwise in the save inferred within the stream of
object NUMBER of PDF, its octets up to the %%EOF at EOF read as a PDF of
their own, than in the save of PDF holding them: findings worth a look,
never more, as nothing of the file names that save. What reading it decodes
and runs is counted against PDF's own limits (SPENT-COUNTS)."
  (let* ((start (spent-counts pdf))
         (earlier (read-pdf (subseq (pdf-octets pdf) 0 (+ eof 5)) :name (pdf-name pdf) :spent start)))
    (unwind-protect
         (let* ((before (last-revision earlier))
                (revision (or (revision-of-offset pdf eof) (last-revision pdf)))
                (old-pages (pages-of earlier before)))
           (loop for page in (pages-of pdf revision) for k from 0
                 for old-rest = old-pages then (rest old-rest)
                 for old = (first old-rest)
                 for old-marks = (if old (marks-of earlier before old) #())
                 for new-marks = (marks-of pdf revision page)
                 for text = (line-diff (text-lines old-marks) (text-lines new-marks))
                 when (or text (null old))
                   collect (found :look (format nil "Page ~d reads otherwise in ~a than in a save inferred within ~
                                                     the data of stream ~d (its octets up to the %%EOF at ~a): ~:d ~
                                                     line~:p of its text differ. That earlier save is inferred from ~
                                                     octets the stream's /Length covers; nothing of the file names it."
                                                  (1+ k) (revision-name revision) number (offset-words eof) (length text))
                                  :rule (concatenate 'string "A save hidden within a stream is read as the octets up to "
                                                     "its %%EOF, and its pages are set beside the save that holds it.")
                                  :revision (revision-index revision) :page (1+ k)
                                  :details (loop for (kind line) in text repeat 12
                                                 collect (format nil "~:[-~;+~] ~a" (eq kind :added)
                                                                 (shorten-string line 100 :one-line t))))))
      (add-spent-counts pdf earlier start))))

(defun check-save-signs (pdf)
  "Streams, no file kept within the PDF, whose data holds a %%EOF or object
headers: worth a look, as a save may be hidden in them; the save inferred
up to that %%EOF set beside the save holding it (INFERRED-SAVE-CHANGES)."
  (loop for (number . eof) in (getf (pdf-cache pdf) :streams-with-save-signs)
        for k from 0
        append (cons (found :look (format nil "~:[Object headers lie~;%%EOF and object headers lie~] within the data ~
                                               of stream ~d, which is no embedded file: a save may be hidden in it."
                                          eof number)
                            :rule (concatenate 'string "Only a file kept within a PDF holds a %%EOF or object headers "
                                               "within a stream's data.")
                            :evidence (and eof (list (format nil "%%EOF at ~a, within object ~d's stream"
                                                             (offset-words eof) number))))
                     (and eof (< k *most-inferred-saves*)
                          (let ((failed '()))
                            (guarding-item (failed "the save inferred within stream ~d" number)
                              (inferred-save-changes pdf number eof)))))))
