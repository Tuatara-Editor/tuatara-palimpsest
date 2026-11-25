;;;; structure.lisp --- The saves of a PDF compared, and how each is put together.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:palimpsest)

;;; The revisions compared

(defstruct (page-change (:copier nil))
  "A page a revision changes: PAGE, as it is in that revision, OLD, as it was
in the one before (NIL for a new page); TEXT, the lines that differ
\(LINE-DIFF); ADDED and REMOVED, the marks it draws now and did not, and
drew and does not; ANNOTATIONS, the lines of what is laid over it that
differ (ANNOTATION-LINES, LINE-DIFF)."
  page old text added removed annotations)

(setf (documentation 'page-change-p 'function) "True when OBJECT is a PAGE-CHANGE.")

(defstruct (account (:copier nil))
  "What one revision did: REVISION; CHANGES, the pages it changed (each a
PAGE-CHANGE); GONE, the pages of the revision before it no longer has."
  revision (changes '()) (gone '()))

(setf (documentation 'account-p 'function) "True when OBJECT is an ACCOUNT.")

(defun page-matcher (pages)
  "A function giving, of a page, the page among PAGES that is it in another
revision: the first of its object number, else of its place; NIL for none."
  (let ((by-number (make-hash-table)) (by-index (make-hash-table)))
    (dolist (page (reverse pages))
      (setf (gethash (page-number page) by-number) page
            (gethash (page-index page) by-index) page))
    (lambda (page)
      (or (and (page-number page) (gethash (page-number page) by-number))
          (gethash (page-index page) by-index)))))

(defun compare-revision (pdf revision)
  "The ACCOUNT of REVISION: each page it touched (one drawing with an
object it adds or changes, or a new page) set beside what it was before.
A page that cannot be compared is left out, and noted in PDF (NOTE)."
  (let ((account (make-account :revision revision)))
    (when (plusp (revision-index revision))
      (let* ((before (aref (pdf-revisions pdf) (1- (revision-index revision))))
             (touched (let ((h (make-hash-table)))
                        (dolist (n (append (revision-added revision) (revision-changed revision)) h)
                          (setf (gethash n h) t))))
             (old-pages (pages-of pdf before))
             (matching-page (page-matcher old-pages)))
        (dolist (page (pages-of pdf revision))
          (job-check-stop)
          (handler-case
              (let ((old (funcall matching-page page)))
                (when (or (null old) (and (page-number page) (not (eql (page-number page) (page-number old))))
                          (and (plusp (hash-table-count touched))
                               (nth-value 1 (page-reach pdf revision page :until (lambda (n) (gethash n touched))))))
                  (let ((new-marks (marks-of pdf revision page))
                        (old-marks (if old (marks-of pdf before old) #())))
                    (multiple-value-bind (added removed) (drawing-diff old-marks new-marks)
                      (push (make-page-change :page page :old old
                                              :text (line-diff (text-lines old-marks) (text-lines new-marks))
                                              :added added :removed removed
                                              :annotations (line-diff (if old (annotation-lines pdf before old) '())
                                                                      (annotation-lines pdf revision page)))
                            (account-changes account))))))
            (error (e)
              (note pdf "In revision ~d, page ~d could not be set beside the revision before: ~a"
                    (1+ (revision-index revision))
                    (1+ (page-index page))
                    (shorten-string (princ-to-string e) 200 :one-line t)))))
        (setf (account-changes account) (nreverse (account-changes account))
              (account-gone account)
              (let ((numbers (make-hash-table)))
                (dolist (page (pages-of pdf revision)) (setf (gethash (page-number page) numbers) t))
                (remove-if (lambda (old) (gethash (page-number old) numbers)) old-pages)))))
    account))

;;; Structure

(defun check-saves (pdf accounts)
  "The file's later saves, and each page a later save changed."
  (let ((revisions (pdf-revisions pdf)) (out '()))
    (when (> (length revisions) 1)
      (push (found :record (format nil "The file was changed after it was first saved: ~d later save~:p ~
                                        ~:*~[~;is~:;are~] kept in it, beside what ~:*~[~;it~:;they~] changed."
                                   (1- (length revisions)))
                   :rule (concatenate 'string "A PDF saved again incrementally keeps each save's objects, "
                                              "cross-reference section and %%EOF one after the other.")
                   :revision (1- (length revisions))
                   :details (loop for r across revisions
                                  collect (format nil "~@(~a~): ~a to ~a"
                                                  (revision-name r)
                                                  (hex-offset (revision-start r))
                                                  (hex-offset (revision-end r))))
                   :evidence (loop for r across revisions
                                   collect (format nil "%%EOF of ~a at ~a"
                                                   (revision-name r)
                                                   (offset-words (revision-eof r))))
                   :steps (list "Search the file for %%EOF: each one that is not inside a stream ends a save.")
                   :method :saved-again)
            out))
    (dolist (account accounts)
      (let ((revision (account-revision account)))
        (dolist (change (account-changes account))
          (let* ((page (page-change-page change)) (p (1+ (page-index page))))
            (when (or (page-change-text change)
                      (page-change-added change)
                      (page-change-removed change)
                      (null (page-change-old change)))
              (push (found :look (if (page-change-old change)
                                     (format nil "Page ~d was changed in ~a: ~:d line~:p of its text and ~:d thing~:p ~
                                                  it draws differ from the save before."
                                             p (revision-name revision) (length (page-change-text change))
                                             (+ (length (page-change-added change))
                                                (length (page-change-removed change))))
                                     (format nil "Page ~d was added in ~a." p (revision-name revision)))
                           :kind :page-changed
                           :rule (concatenate 'string "A page drawing with an object a later save adds or changes is "
                                                      "compared with the same page in the save before: its text line "
                                                      "by line, its drawing item by item.")
                           :revision (revision-index revision) :page p
                           :details (append (loop for (kind line) in (page-change-text change) repeat 12
                                                  collect (format nil "~:[-~;+~] ~a"
                                                                  (eq kind :added)
                                                                  (shorten-string line 100 :one-line t)))
                                            (loop for m in (page-change-added change)
                                                  repeat 6
                                                  collect (format nil "drawn now: ~a" (mark-words page m)))
                                            (loop for m in (page-change-removed change)
                                                  repeat 6
                                                  collect (format nil "drawn before: ~a"
                                                                  (mark-words (or (page-change-old change) page) m))))
                           :evidence (loop for m in (page-change-added change) repeat 4
                                           append (mark-evidence pdf revision m (mark-words page m)))
                           :objects (page-contents page))
                    out))))))
    out))

(defun check-revision-problems (pdf)
  "What does not make sense in each revision's cross-reference section, trailer and page tree, each PROBLEM a
finding of its own strength by its own rule."
  (loop for revision across (pdf-revisions pdf)
        append (loop for problem in (remove-duplicates (revision-problems revision) :test #'string=
                                                       :key #'problem-words)
                     collect (found (problem-strength problem)
                                    (format nil "In ~a, ~a: the file was edited by hand, repaired, or made by a ~
                                                 careless program." (revision-name revision) (problem-words problem))
                                    :rule (or (problem-rule problem)
                                              (format nil "A save's startxref names where its cross-reference section ~
                                                           begins, and the section lists each object once."))
                                    :revision (revision-index revision)
                                    :evidence (remove nil
                                                      (list (and (revision-startxref-at revision)
                                                                 (format nil "startxref at ~a names ~a"
                                                                         (offset-words (revision-startxref-at revision))
                                                                         (let ((named (revision-startxref revision)))
                                                                           (and named (offset-words named)))))
                                                            (and (revision-xref-at revision)
                                                                 (format nil "the section read is at ~a"
                                                                         (offset-words (revision-xref-at revision))))))
                                    :steps (list (if (revision-startxref-at revision)
                                                     (format nil "Go to the startxref before the %%EOF at ~a and read ~
                                                                  the offset after it; go to that offset and see what ~
                                                                  begins there."
                                                             (offset-words (revision-eof revision)))
                                                     (format nil "Read back from the %%EOF at ~a: no startxref stands ~
                                                                  before it, so nothing names where this save's ~
                                                                  cross-reference section begins."
                                                             (offset-words (revision-eof revision)))))
                                    :method :structure-by-hand))))

(defun check-offsets (pdf)
  "Cross-reference entries that do not point at the object they name. Strong when a named object is written
elsewhere in the file (an entry moved off it) or in a later save; worth a look when, in the first save, every
named object is written nowhere, as the empty slots some programs (cairo, Quartz) leave in their index."
  (let ((out '()))
    (loop for revision across (pdf-revisions pdf)
          do (let ((wrong '()))
               (maphash (lambda (number entry)
                          (when (and (plusp number) (eq (entry-kind entry) :used))
                            (let ((object (object-at-offset pdf (entry-place entry))))
                              (unless (and object
                                           (eql (pdf-indirect-get object :number) number)
                                           (eql (pdf-indirect-get object :generation) (entry-generation entry)))
                                (push (list number entry object
                                            (found-object pdf number (revision-end revision)))
                                      wrong)))))
                        (revision-entries revision))
               (setf wrong (sort wrong #'< :key #'first))
               (when wrong
                 (push (found (if (or (plusp (revision-index revision)) (some #'fourth wrong)) :strong :record)
                              (if (or (plusp (revision-index revision)) (some #'fourth wrong))
                                  (format nil "In ~a, ~d cross-reference entr~:@p ~:*~[~;does~:;do~] not point at ~
                                               the object ~:*~[~;it names~:;they name~]: the file was edited by ~
                                               hand or repaired."
                                          (revision-name revision) (length wrong))
                                  (format nil "In ~a, ~d cross-reference entr~:@p ~:*~[~;does~:;do~] not point at ~
                                               the object ~:*~[~;it names~:;they name~], which ~:*~[~;is~:;are~] ~
                                               written nowhere in the file: slots the program left in its index, ~
                                               as some programs do."
                                          (revision-name revision) (length wrong)))
                              :rule (concatenate 'string "Each entry of a cross-reference section gives the offset "
                                                         "where its object's NUMBER GENERATION obj begins. An "
                                                         "entry whose object is written nowhere in the file is an "
                                                         "empty slot, which some programs leave in the index of "
                                                         "their first save; an entry that misses an object written "
                                                         "elsewhere in the file, or in a later save, is an edit or a "
                                                         "repair.")
                              :revision (revision-index revision) :objects (mapcar #'first wrong)
                              :evidence (loop for (number entry object real) in wrong repeat 20
                                              append (cons (format nil "Object ~d ~d is listed at ~a, where ~:[no ~
                                                                        object begins~;~:*object ~a begins~]~@[; ~
                                                                        object ~d is really at ~a~]."
                                                                   number
                                                                   (entry-generation entry)
                                                                   (offset-words (entry-place entry))
                                                                   (and object
                                                                        (format nil "~d ~d"
                                                                                (pdf-indirect-get object :number)
                                                                                (pdf-indirect-get object :generation)))
                                                                   (and real number)
                                                                   (and real
                                                                        (offset-words (pdf-indirect-get real :at))))
                                                           (octets-evidence
                                                             "  the octets there"
                                                             (pdf-octets pdf)
                                                             (entry-place entry)
                                                             (min (length (pdf-octets pdf))
                                                                  (+ (entry-place entry) 16)))))
                              :steps (list (format nil "Read the cross-reference section at ~a; for each object named ~
                                                        here go to the offset its entry gives."
                                                   (offset-words (or (revision-xref-at revision) 0))))
                              :method :structure-by-hand)
                       out))))
    out))

(defun check-generations (pdf)
  "Objects freed, and numbers used again without their generation going up."
  (let ((out '()) (last-generation (make-hash-table)) (freed (make-hash-table)))
    (loop for revision across (pdf-revisions pdf)
          do (let ((before (and (plusp (revision-index revision))
                                (aref (pdf-revisions pdf) (1- (revision-index revision))))))
               (when (revision-freed revision)
                 (push (found :record (format nil "~@(~a~) frees ~d object~:p: ~{~d~^, ~}." (revision-name revision)
                                              (length (revision-freed revision))
                                              (subseq
                                                (revision-freed revision)
                                                0
                                                (min 30 (length (revision-freed revision)))))
                              :rule (concatenate 'string "An object a save frees is marked f in its cross-reference "
                                                         "section; what it was is still in the file.")
                              :revision (revision-index revision) :objects (revision-freed revision)
                              :details (loop for n in (revision-freed revision) repeat 20
                                             collect (format nil "object ~d~@[, ~a~]"
                                                             n
                                                             (and before (role-words pdf before n))))
                              :evidence (loop for n in (revision-freed revision) repeat 20
                                              append (nth-value 0 (and before (object-evidence pdf before n)))))
                       out))
               (dolist (number (sort (loop for n being the hash-keys of (revision-entries revision) collect n) #'<))
                 (let ((entry (gethash number (revision-entries revision))))
                   (cond ((eq (entry-kind entry) :free) (setf (gethash number freed) t))
                         ((eq (entry-kind entry) :used)
                          (let ((previous (gethash number last-generation)))
                            (when (and previous (gethash number freed) (<= (entry-generation entry) previous))
                              (push (found :look (format nil "Object ~d was freed and used again in ~a without its ~
                                                              generation number going up, as the format asks."
                                                         number (revision-name revision))
                                           :rule (concatenate 'string "An object number freed and used again must take "
                                                                      "a higher generation number.")
                                           :revision (revision-index revision) :objects (list number)
                                           :evidence (object-evidence pdf revision number) :method :structure-by-hand)
                                    out))
                            (remhash number freed)
                            (setf (gethash number last-generation) (entry-generation entry)))))))))
    out))

(defun hint-stream-p (pdf object)
  "True when OBJECT (a PDF-INDIRECT) is a hint stream of PDF, a file read as
prepared for the web, its two parts agreeing (READ-PDF): a stream with /S,
where its shared objects' hints begin, as hint streams have, listed in the
first page's cross-reference section where it is written, beginning where
the linearisation dictionary's /H says a hint stream does."
  (let ((first (getf (pdf-cache pdf) :first-page-entries))
        (hints (pdf-get (linearisation pdf) "/H")))
    (and first (consp hints) (pdf-indirect-get object :stream-start) (not (pdf-indirect-get object :packed-in))
         ;; a hint stream says where its shared objects' hints begin
         (integerp (pdf-get (pdf-indirect-get object :value) "/S"))
         (let ((entry (gethash (pdf-indirect-get object :number) first)))
           (and entry (eq (entry-kind entry) :used) (eql (entry-place entry) (pdf-indirect-get object :at))))
         (loop for (at) on (pdf-array-items hints) by #'cddr
               thereis (and (integerp at) (eql (+ at (pdf-base pdf)) (pdf-indirect-get object :at)))))))

(defun structural-p (pdf revision number)
  "True when the object NUMBER of REVISION is part of how the file is put
together: an object or cross-reference stream, a linearisation dictionary."
  (let* ((object (revision-object pdf revision number)) (value (and object (pdf-indirect-get object :value))))
    (or (null object)
        (name-p (pdf-get value "/Type") "/ObjStm")
        (name-p (pdf-get value "/Type") "/XRef")
        (pdf-get value "/Linearized"))))

(defun check-leftovers (pdf)
  "Objects the document no longer uses, and objects written that no cross-reference section lists."
  (let* ((last (last-revision pdf))
         (reach (reachable pdf last))
         ;; a hint stream is reached by no reference, only by where /H says it is
         (unused (remove-if (lambda (n) (or (gethash n reach) (structural-p pdf last n)
                                            (let ((object (revision-object pdf last n)))
                                              (and object (hint-stream-p pdf object)))))
                            (live-numbers pdf last)))
         (listed (let ((h (make-hash-table :test 'equal)))
                   (flet ((note (entries)
                            (maphash
                              (lambda (n e)
                                (when (eq (entry-kind e) :used) (setf (gethash (cons n (entry-place e)) h) t)))
                              entries)))
                     (loop for r across (pdf-revisions pdf) do (note (revision-entries r)))
                     ;; every section read lists what it lists, though a save's own may take another's place
                     (let ((sections (getf (pdf-cache pdf) :sections)))
                       (when sections
                         (maphash
                           (lambda (at read) (declare (ignore at)) (when (first read) (note (first read))))
                           sections))))
                   h))
         (unlisted (unless (some (lambda (r) (eq (revision-xref-kind r) :none)) (pdf-revisions pdf))
                     (remove-if (lambda (o) (or (gethash (cons (pdf-indirect-get o :number) (pdf-indirect-get o :at))
                                                         listed)
                                                (name-p (pdf-get (pdf-indirect-get o :value) "/Type") "/XRef")
                                                (pdf-get (pdf-indirect-get o :value) "/Linearized")))
                                (coerce (pdf-found pdf) 'list))))
         (out '()))
    (when unused
      (push (found :record (format nil "~d object~:p in the file ~:*~[~;is~:;are~] no longer used by the ~
                                        document: leftovers of earlier content, or kept to be used later."
                                   (length unused))
                   :rule (concatenate 'string "Every object a document uses is reached from its trailer by following "
                                              "references; the others are not shown.")
                   :revision (revision-index last) :objects unused
                   :details (loop for n in unused repeat 30
                                  collect (format nil "object ~d~@[, ~a~]~@[, written in revision ~d~]" n
                                                  (loop for r from (revision-index last) downto 0
                                                        thereis (gethash n (roles-of pdf (aref (pdf-revisions pdf) r))))
                                                  (let ((w (written-in pdf last n))) (and w (1+ (revision-index w))))))
                   :evidence (loop for n in unused repeat 20 append (nth-value 0 (object-evidence pdf last n)))
                   :steps (list (format nil "Follow the references from the last trailer's /Root and /Info: these ~
                                             objects are never reached.")))
            out))
    (when unlisted
      (push (found :look (format nil "~d object~:p ~:*~[~;is~:;are~] written in the file but listed by no ~
                                      cross-reference section: hidden from every reader, or left by an edit."
                                 (length unlisted))
                   :rule "Each object written in a save is listed in that save's cross-reference section."
                   :revision (revision-index last) :objects (mapcar (lambda (o) (pdf-indirect-get o :number)) unlisted)
                   :evidence (loop for o in unlisted repeat 20
                                   collect (format nil "object ~d ~d at ~a"
                                                   (pdf-indirect-get o :number)
                                                   (pdf-indirect-get o :generation)
                                                   (offset-words (pdf-indirect-get o :at))))
                   :steps (list "Go to each offset: an object begins there that no cross-reference entry names.")
                   :method :structure-by-hand)
            out))
    out))

(defun check-twice (pdf)
  "Objects written twice within one save."
  (loop with found = (pdf-found pdf)
        for revision across (pdf-revisions pdf)
        append (let ((seen (make-hash-table)) (twice '())
                     ;; the objects are in the order of their offsets: the first of this save's
                     (first (let ((low 0) (high (length found)))
                              (loop while (< low high)
                                    do (let ((middle (floor (+ low high) 2)))
                                         (if (< (pdf-indirect-get (aref found middle) :at) (revision-start revision))
                                             (setf low (1+ middle))
                                             (setf high middle))))
                              low)))
                 (job-check-stop)
                 (loop for k from first below (length found)
                       for o = (aref found k)
                       while (< (pdf-indirect-get o :at) (revision-end revision))
                       do (let ((earlier (gethash (pdf-indirect-get o :number) seen)))
                            (if earlier
                                (push (list (pdf-indirect-get o :number) earlier o) twice)
                                (setf (gethash (pdf-indirect-get o :number) seen) o))))
                 (when twice
                   (list (found :look (format nil "~@(~a~) writes ~d object~:p twice: the later copy is the one shown; ~
                                                   written by hand or by a careless program."
                                              (revision-name revision) (length twice))
                                :rule "One save writes each object once."
                                :revision (revision-index revision) :objects (mapcar #'first (reverse twice))
                                :evidence (loop for (n a b) in (reverse twice) repeat 20
                                                collect (format nil "object ~d at ~a and again at ~a"
                                                                n
                                                                (offset-words (pdf-indirect-get a :at))
                                                                (offset-words (pdf-indirect-get b :at))))
                                :method :structure-by-hand))))))

(defun tail-lines (octets start)
  "The lines of OCTETS from START to the end, as strings, blank lines left
out; NIL when they are more than 1 KB, or an octet is other than printable
ASCII, a space, a tab, a line end or the escape that begins a printer's
universal exit."
  (when (<= (- (length octets) start) 1024)
    (let ((lines '()) (line (make-string-output-stream)))
      (loop for k from start below (length octets)
            for c = (aref octets k)
            do (cond ((or (= c 10) (= c 13))
                      (let ((text (get-output-stream-string line)))
                        (when (plusp (length text)) (push text lines))))
                     ((not (or (<= 32 c 126) (= c 9) (= c 27)))
                      (return-from tail-lines nil))
                     (t (write-char (code-char c) line))))
      (let ((text (get-output-stream-string line)))
        (when (plusp (length text)) (push text lines)))
      (nreverse lines))))

(defparameter *banner-attributes*
  '("printer-name" "printer-info" "printer-location" "printer-make-and-model" "printer-driver-name"
    "printer-driver-version" "paper-size" "imageable-area" "job-id" "options" "time-at-creation"
    "time-at-processing" "job-originating-user-name" "job-name" "job-originating-host-name" "job-billing"
    "job-uuid")
  "The attributes a CUPS banner page's %%Show line names, as CUPS's banner
pages list them; a %%Show line naming anything else is not taken for a
banner's.")

(defun plain-words-p (text &key (most 128))
  "True when TEXT is at most MOST characters of plain words: none longer than
24 characters, none turning from a small letter to a capital more than once,
none turning between letters and digits more than twice. Encoded data (hex,
base64) breaks those at once; names such as McDonald, v2.1 or x86_64 keep
them."
  (flet ((plain-word-p (word)
           (and (<= (length word) 24)
                (<= (loop for (a b) on (coerce word 'list) while b count (and (lower-case-p a) (upper-case-p b))) 1)
                (<= (loop for (a b) on (coerce word 'list)
                          while b count (or (and (alpha-char-p a) (digit-char-p b))
                                            (and (digit-char-p a) (alpha-char-p b))))
                    2))))
    (and (<= (length text) most) (every #'plain-word-p (uiop:split-string text :separator '(#\Space #\Tab))))))

(defun honest-tail-kind (octets start)
  "What OCTETS from START to the end are, when they have the shape of what
honest programs write after a PDF's last %%EOF, in 1 KB at most of lines of
printable text (TAIL-LINES). :BANNER stands for the instructions of a
printing system's banner page as CUPS writes them, a line %%#PDF-BANNER and
lines of %% and a key, each once, with a value of the kind CUPS reads, Show
with names of *BANNER-ATTRIBUTES*, Font with a font's name, Font-size with a
number, Template or Image with a file's name, Header, Footer or Notice with
plain words (PLAIN-WORDS-P). :PRINTER-JOB stands for printer job language
(PJL) lines as a driver ending a job writes them, six at most, the universal
exit (escape and %-12345X), then lines of @PJL and one of the commands EOJ,
JOB, RESET, SET, ENTER, USTATUSOFF or COMMENT, the exit again allowed before
any; a COMMENT carries plain words, the others plain words and quoted names
of plain words, 40 characters at most. NIL is anything else."
  (let ((lines (tail-lines octets start))
        (exit (format nil "~c%-12345X" (code-char 27))))
    (labels ((split (line)
               (let ((space (position-if (lambda (c) (member c '(#\Space #\Tab))) line)))
                 (values (subseq line 0 space) (if space (string-trim '(#\Space #\Tab) (subseq line space)) ""))))
             (one-word-p (value test)
               (and (plusp (length value)) (<= (length value) 255) (every test value)))
             (banner-line-p (line)
               (and (uiop:string-prefix-p "%%" line)
                    (multiple-value-bind (key value) (split (subseq line 2))
                      (cond ((string-equal key "Show")
                             (every (lambda (name) (member name *banner-attributes* :test #'string=))
                                    (remove "" (uiop:split-string value :separator '(#\Space #\Tab))
                                            :test #'string=)))
                            ((string-equal key "Font")
                             (one-word-p value (lambda (c) (or (alphanumericp c) (find c "-_")))))
                            ((string-equal key "Font-size")
                             (one-word-p value (lambda (c) (or (digit-char-p c) (char= c #\.)))))
                            ((member key '("Template" "Image") :test #'string-equal)
                             (one-word-p value (lambda (c) (or (alphanumericp c) (find c "-_./")))))
                            ((member key '("Header" "Footer" "Notice") :test #'string-equal)
                             (plain-words-p value))))))
             (job-line-p (line)
               (let ((rest (if (uiop:string-prefix-p exit line) (subseq line (length exit)) line)))
                 (or (zerop (length rest))
                     (and (uiop:string-prefix-p "@PJL" rest)
                          (multiple-value-bind (command value) (split (string-trim " " (subseq rest 4)))
                            (cond ((zerop (length command)) t)
                                  ((string-equal command "COMMENT") (plain-words-p value :most 80))
                                  ((member command '("EOJ" "JOB" "RESET" "SET" "ENTER" "USTATUSOFF")
                                           :test #'string-equal)
                                   (job-value-p value))))))))
             (job-value-p (value)
               (let ((quote (position #\" value)))
                 (if quote
                     (let ((close (position #\" value :start (1+ quote))))
                       (and close (plain-words-p (subseq value (1+ quote) close) :most 40)
                            (plain-words-p (subseq value 0 quote) :most 80)
                            (job-value-p (subseq value (1+ close)))))
                     (plain-words-p value :most 80)))))
      (cond ((null lines) nil)
            ((and (string= (first lines) "%%#PDF-BANNER") (every #'banner-line-p (rest lines))
                  (let ((keys (mapcar (lambda (line) (string-upcase (split (subseq line 2)))) (rest lines))))
                    (= (length keys) (length (remove-duplicates keys :test #'string=)))))
             :banner)
            ((and (<= (length lines) 6) (uiop:string-prefix-p exit (first lines)) (every #'job-line-p lines))
             :printer-job)))))

(defparameter *tail-white-names*
  '((0 "NUL" "NULs") (9 "tab" "tabs") (10 "line feed" "line feeds") (12 "form feed" "form feeds")
    (13 "carriage return" "carriage returns") (32 "space" "spaces"))
  "The kinds of white space a PDF may hold, each octet with its name and its
name for more than one.")

(defun tail-white-counts (octets start &optional (end (length octets)))
  "How many of each kind of white space of *TAIL-WHITE-NAMES* OCTETS hold
from START below END (the end when not given), as a list of (OCTET COUNT),
the most first, those of none left out."
  (let ((counts (make-array 256 :element-type 'fixnum :initial-element 0)))
    (loop for i from start below end do (incf (aref counts (aref octets i))))
    (sort (loop for (octet) in *tail-white-names*
                when (plusp (aref counts octet)) collect (list octet (aref counts octet)))
          #'> :key #'second)))

(defun mixed-white-p (white other &key line-ends-apart)
  "True when WHITE, the white space of some octets counted by its kinds
\(TAIL-WHITE-COUNTS), mixes them as a code hiding bits in white space writes
them: two kinds or more, and, with OTHER octets among them besides white
space, eight or more of the second kind, so that NULs to a block's end with
a line end and a Ctrl-Z are not mixed. With LINE-ENDS-APART, as within a
file whose lines writers end and indent as they please, line ends are left
out (carriage returns and line feeds alike) and two other kinds are needed,
eight or more of the second."
  (if line-ends-apart
      (let ((kinds (remove-if (lambda (w) (member (first w) '(10 13))) white)))
        (and (rest kinds) (>= (second (second kinds)) 8)))
      (and (rest white) (or (zerop other) (>= (second (second white)) 8)))))

(defun white-details (white other)
  "Lines of details counting the white space WHITE (TAIL-WHITE-COUNTS) kind by
kind, and the OTHER octets among it when there are any."
  (append (loop for (octet count) in white
                collect (format nil "~:d ~a (0x~2,'0x)" count
                                (if (= count 1)
                                    (second (assoc octet *tail-white-names*))
                                    (third (assoc octet *tail-white-names*)))
                                octet))
          (and (plusp other) (list (format nil "~:d other octet~:p" other)))))

(defun white-kind-names (white)
  "The kinds of white space WHITE counts (TAIL-WHITE-COUNTS), each named for
more than one: NULs, line feeds."
  (mapcar (lambda (w) (third (assoc (first w) *tail-white-names*))) white))

(defun padding-tail-p (octets start)
  "True when OCTETS from START to the end are padding: one line end at most,
as a mail program adds, then more than four of one octet repeated to the
end, as a copy padded to a block's size or the Ctrl-Z of an old transfer
leaves."
  (let* ((n (length octets))
         (first (cond ((and (< (1+ start) n) (= (aref octets start) 13) (= (aref octets (1+ start)) 10)) (+ start 2))
                      ((and (< start n) (member (aref octets start) '(10 13))) (1+ start))
                      (t start))))
    (and (> (- n first) 4) (not (position (aref octets first) octets :start first :test-not #'=)))))

(defun judge-tail (octets start)
  "How strong a sign the octets of OCTETS from START to the end are, when they
follow a PDF's last %%EOF line, and why, as two values; NIL for nothing worth
naming. Of 15,475 honest PDFs read, none had white space after the line end
of the last %%EOF, and only the banner pages of CUPS had data there, in 47
to 289 octets; of the 113 honest scans and samples kept for testing, none
had any octet there. The reasons, in the order they are tried:
:PDF-WORDS, PDF objects or another file among them, strong;
:PADDING, one octet repeated (PADDING-TAIL-P), which can hold nothing, worth
a look at any length;
:BANNER and :PRINTER-JOB, the shapes of HONEST-TAIL-KIND, worth a look;
:DATA, 16 octets or more besides white space, strong;
:MIXED, 64 octets or more of white space mixing its kinds (spaces, tabs,
NULs, form feeds, carriage returns, line feeds), as a code hiding bits in
white space writes them, strong; with a few other octets among them, only
when a second kind has eight octets or more, so that NULs to a block's end
with a line end and a Ctrl-Z are short data;
:DATA again for fewer than 16 octets besides white space, too few to hold a
sentence, worth a look, as one octet repeated to a block and ending in a
Ctrl-Z or two is;
for white space alone, nothing for four octets or fewer of line ends and
spaces, as a mail program adding a line end leaves; :LONG for 1 KB or more,
more than Adobe's readers look past for %%EOF, strong; :WHITE for anything
between, worth a look."
  (let* ((size (- (length octets) start))
         (rest-count (count-if-not #'pdf-white-p octets :start start)))
    (cond ((zerop size) nil)
          ((and (plusp rest-count)
                (some (lambda (word) (octets-search (string-octets word) octets :start start))
                      '("obj" "%PDF" "xref" "stream" "trailer")))
           (values :strong :pdf-words))
          ((padding-tail-p octets start) (values :look :padding))
          ((and (plusp rest-count) (honest-tail-kind octets start))
           (let ((kind (honest-tail-kind octets start))) (values (if (eq kind :banner) :record :look) kind)))
          ((>= rest-count 16) (values :strong :data))
          ((and (>= (- size rest-count) 64) (mixed-white-p (tail-white-counts octets start) rest-count))
           (values :strong :mixed))
          ((plusp rest-count) (values :look :data))
          ((and (<= size 4) (every (lambda (c) (member c '(10 13 32))) (subseq octets start))) nil)
          ((>= size 1024) (values :strong :long))
          (t (values :look :white)))))

(defun check-ends (pdf)
  "Data after the last save, and before the PDF begins: what follows the last
%%EOF line, judged by JUDGE-TAIL, and octets before %PDF-. White space mixed
there is named by its kinds, and each kind counted in the details."
  (let* ((octets (pdf-octets pdf)) (last (last-revision pdf)) (end (revision-end last)) (out '())
         (size (- (length octets) end)))
    (multiple-value-bind (strength why) (judge-tail octets end)
      (when strength
        (let* ((white (and (member why '(:long :mixed :white)) (tail-white-counts octets end)))
               (other (count-if-not #'pdf-white-p octets :start end))
               (kinds (white-kind-names white)))
          (push (found strength
                       (format nil "~:d octet~:p~:[~; of white space~] follow~:[s~;~] the last %%EOF~?"
                               size (and white (zerop other)) (/= size 1)
                               (ecase why
                                 (:pdf-words ", among them PDF objects or another file: data hidden after the end, ~
                                              or a save cut short.")
                                 (:padding ": one octet repeated, padding from a copy made to a block's size or ~
                                            from a file carved out of a disk, which holds no data.")
                                 (:banner ": the instructions of a printing system's banner page, as CUPS writes ~
                                           them, or data made to look so.")
                                 (:printer-job ": printer job language (PJL) lines, as a driver printing to a file ~
                                                writes them, or data made to look so.")
                                 (:data ": data hidden after the end, or a save cut short.")
                                 (:long ": more than a reader looks past for the end, so data hidden after it.")
                                 (:mixed ": ~{~a~#[~; and ~:;, ~]~} mixed, as a code hiding bits in white space ~
                                          writes them: data hidden after the end.")
                                 (:white ": white space after the end, which can hold data."))
                               (list kinds))
                       :rule "A PDF ends with %%EOF and a line end; readers pass over anything after it."
                       :revision (revision-index last)
                       :details (when (eq why :mixed) (white-details white other))
                       :evidence (octets-evidence "After the last %%EOF" octets end (length octets))
                       :steps (list (format nil "Go to offset ~a, just after the last %%EOF line, and read to the ~
                                                 end of the file." (offset-words end))) :method :structure-by-hand)
                out))))
    (when (plusp (pdf-header pdf))
      (push (found :look (format nil "~:d octet~:p come before %PDF-: another file's data, or a file made to be read ~
                                      as two kinds." (pdf-header pdf))
                   :rule "A PDF begins with %PDF-."
                   :revision 0
                   :evidence (octets-evidence "Before %PDF-" octets 0 (pdf-header pdf))
                   :steps (list "Read the file from offset 0 to where %PDF- begins.") :method :structure-by-hand)
            out))
    out))

(defun stream-data-spans (pdf)
  "Where the data of each stream written in PDF lies, a list of (START . END)
in the order written. A stream ends where its /Length says, written there
or in an object of its own, when that falls before its endstream with only
white space after it; else just before its endstream. What white space lies
between that end and endstream is padding, not data."
  (let ((last (last-revision pdf)))
    (loop for object across (pdf-found pdf)
          for start = (pdf-indirect-get object :stream-start)
          for end = (pdf-indirect-get object :stream-end)
          when start
            collect (let ((length (deref pdf last (pdf-indirect-get object :length)))
                          (octets (pdf-octets pdf)))
                      (cons start (if (and (integerp length) (<= 0 length) (<= (+ start length) end)
                                           (loop for i from (+ start length) below end
                                                 always (pdf-white-p (aref octets i))))
                                      (+ start length)
                                      end))))))

(defun white-runs (pdf least)
  "The runs of white space LEAST octets long or longer in PDF's saves, from
%PDF- to the last save's %%EOF line, between the words of the file: between
objects, within them, in a stream's padding, about the cross-reference
section and the trailer. Each is (START END), in the order written. The
data of its streams (STREAM-DATA-SPANS) is passed over, and so is that of a
stream whose end was never found, from its stream keyword to the endstream
after it, and what strings and comments hold."
  (let* ((octets (pdf-octets pdf)) (end (revision-end (last-revision pdf)))
         (spans (stream-data-spans pdf)) (runs '()) (from nil) (i (pdf-header pdf))
         (endstream (string-octets "endstream")))
    (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum end i))
    (labels ((close-run (at)
               (when (and from (>= (- at from) least)) (push (list from at) runs))
               (setf from nil))
             (literal-end (at)
               ;; past the ) closing the string whose ( is at AT, its own brackets and escapes kept
               (let ((depth 0))
                 (loop for k from at below end
                       for c = (aref octets k)
                       do (case c
                            (92 (incf k))
                            (40 (incf depth))
                            (41 (when (zerop (decf depth)) (return (1+ k)))))
                       finally (return end))))
             (after (at mark)
               ;; past the first octet MARK from AT, or the end
               (let ((k (position mark octets :start at :end end))) (if k (1+ k) end))))
      (loop while (< i end)
            do (let ((c (aref octets i)))
                 (cond ((and spans (>= i (car (first spans))))
                        (let ((span (pop spans)))
                          (close-run (car span))
                          (setf i (max i (cdr span)))))
                       ((pdf-white-p c) (unless from (setf from i)) (incf i))
                       (t
                        (close-run i)
                        (setf i (cond ((= c 40) (literal-end i))
                                      ((and (= c 60) (< (1+ i) end) (/= (aref octets (1+ i)) 60)) (after i 62))
                                      ((= c 60) (+ i 2))
                                      ((= c 37) (1- (min end (after i 10) (after i 13))))
                                      ;; the stream keyword of a stream whose data was not found
                                      ((and (= c 115) (octets-at-p octets "stream" i)
                                            (not (and spans (<= (car (first spans)) (+ i 8))))
                                            (> i 2) (not (octets-at-p octets "end" (- i 3)))
                                            (< (+ i 6) end) (member (aref octets (+ i 6)) '(10 13)))
                                       (let ((close (octets-search endstream octets :start i :end end)))
                                         (if close (+ close 9) end)))
                                      (t (1+ i))))))))
      (close-run end))
    (nreverse runs)))

(defun white-run-place (pdf start)
  "Where in PDF a run of white space beginning at START lies, in words: in
the padding of a stream, within an object, about a save's cross-reference
section, trailer and startxref, or between objects."
  (let* ((found (pdf-found pdf))
         (object (find-if (lambda (o) (and (<= (pdf-indirect-get o :at) start) (< start (pdf-indirect-get o :end))))
                          found))
         (revision (revision-of-offset pdf start)))
    (cond ((and object (pdf-indirect-get object :stream-start) (>= start (pdf-indirect-get object :stream-start)))
           (format nil "after the data of object ~d's stream" (pdf-indirect-get object :number)))
          (object (format nil "within object ~d" (pdf-indirect-get object :number)))
          ((and revision (revision-startxref-at revision) (> start (revision-startxref-at revision)))
           (format nil "between startxref and %%EOF in ~a" (revision-name revision)))
          ((and revision (eq (revision-xref-kind revision) :table) (revision-trailer-at revision)
                (> start (revision-trailer-at revision)))
           (format nil "between the trailer and startxref in ~a" (revision-name revision)))
          ((and revision (revision-xref-at revision) (> start (revision-xref-at revision)))
           (format nil "in the cross-reference section of ~a" (revision-name revision)))
          (t (format nil "between objects~@[ in ~a~]" (and revision (revision-name revision)))))))

(defparameter *hidden-white-least* 64
  "The fewest octets a run of white space within a PDF's saves must have to
be named when it mixes its kinds (CHECK-HIDDEN-WHITE).")

(defun check-hidden-white (pdf)
  "Runs of white space within a PDF's saves, outside the data of its streams
\(WHITE-RUNS), *HIDDEN-WHITE-LEAST* octets or more, that mix their kinds as
a code hiding bits in white space writes them (MIXED-WHITE-P), eight octets
or more of a second kind: a run of spaces between two line ends, as writers
pad with, is not mixed. Strong, as after the last %%EOF (CHECK-ENDS)."
  (let* ((octets (pdf-octets pdf))
         (runs (loop for (start end) in (white-runs pdf *hidden-white-least*)
                     for white = (tail-white-counts octets start end)
                     when (mixed-white-p white 0 :line-ends-apart t)
                       collect (list start end white)))
         (longest (first (sort (copy-list runs) #'> :key (lambda (run) (- (second run) (first run)))))))
    (when runs
      (destructuring-bind (start end white) longest
        (list (found :strong
                     (format nil "~:d octets of white space mixing ~{~a~#[~; and ~:;, ~]~} lie ~a~:[~*~; (the ~
                                  longest of ~d such runs)~]: as a code hiding bits in white space writes them, data ~
                                  hidden within the file."
                             (- end start) (white-kind-names white) (white-run-place pdf start) (rest runs)
                             (length runs))
                     :rule (format nil "Writers part a PDF's words with a few octets of white space, or pad with ~
                                        one kind; none writes long runs mixing its kinds.")
                     :revision (let ((revision (revision-of-offset pdf start)))
                                 (revision-index (or revision (last-revision pdf))))
                     :details (loop for (from to counts) in runs repeat 20
                                    collect (format nil "~a: ~:d octets ~a: ~{~a~^, ~}" (offset-words from) (- to from)
                                                    (white-run-place pdf from) (white-details counts 0)))
                     :evidence (octets-evidence "The longest run" octets start (min end (+ start 256)))
                     :steps (list (format nil "Go to offset ~a and read ~:d octets."
                                          (offset-words start) (- end start)))
                     :method :structure-by-hand))))))

(defun check-index-kinds (pdf)
  "Saves whose cross-reference is of another kind than the first's, hybrid
files, and files prepared for the web then changed."
  (let* ((revisions (coerce (pdf-revisions pdf) 'list)) (first (first revisions)) (out '()))
    (dolist (r (rest revisions))
      (when (and (member (revision-xref-kind first) '(:table :stream)) (member (revision-xref-kind r) '(:table :stream))
                 (not (eq (revision-xref-kind first) (revision-xref-kind r))))
        (push (found :look (format nil "~@(~a~) keeps its cross-reference as a ~(~a~), the original as a ~(~a~): saved ~
                                        by another program than made it."
                                   (revision-name r) (revision-xref-kind r) (revision-xref-kind first))
                     :rule "A program writes its cross-reference sections one way: a table, or a stream."
                     :revision (revision-index r)
                     :evidence (list (format nil "~a's section at ~a; the original's at ~a" (revision-name r)
                                             (offset-words (or (revision-xref-at r) 0))
                                             (offset-words (or (revision-xref-at first) 0)))) :method :other-program)
              out)))
    (dolist (r revisions)
      (when (eq (revision-xref-kind r) :hybrid)
        (push (found :record (format nil "~@(~a~) has both a cross-reference table and a stream (a hybrid file): older ~
                                          readers see only part of it." (revision-name r))
                     :rule "A trailer with /XRefStm names a cross-reference stream beside the table."
                     :revision (revision-index r)
                     :evidence (list (format nil "/XRefStm ~a in the trailer at ~a"
                                             (pdf-get (revision-trailer r) "/XRefStm")
                                             (offset-words (or (revision-trailer-at r) 0)))))
              out)))
    (when (and (revision-linearised first) (rest revisions))
      (push (found :record (format nil "The file was prepared for fast web viewing, then changed by a later save, ~
                                        which undoes the linearisation.")
                   :rule (concatenate 'string "A linearised file is written whole in one save; a save appended after "
                                              "it, by the program that made it (signing, say) or by another, undoes "
                                              "the linearisation. The later saves show what they changed.")
                   :revision 1
                   :evidence (list (format nil "the first object, at ~a, says /Linearized"
                                           (offset-words (pdf-indirect-get (aref (pdf-found pdf) 0) :at)))))
            out))
    out))

(defun file-comments (pdf)
  "The comments written between PDF's objects, up to 50, each (AT END TEXT):
lines beginning with %, but for the header and %%EOF, and binary marks."
  (let* ((octets (pdf-octets pdf)) (n (length octets)) (found (pdf-found pdf)) (comments '()) (k 0))
    (loop for at = (position 37 octets) then (position 37 octets :start (1+ at))
          while (and at (< (length comments) 50))
          do (loop while (and (< k (length found)) (<= (pdf-indirect-get (aref found k) :end) at)) do (incf k))
             (unless (or (and (< k (length found)) (<= (pdf-indirect-get (aref found k) :at) at))
                         (not (or (zerop at) (member (aref octets (1- at)) '(10 13)))))
               (let* ((end (or (position-if (lambda (c) (member c '(10 13))) octets :start at) n))
                      (text (octets-latin-1 octets :start at :end (min end (+ at 200)))))
                 (unless (or (uiop:string-prefix-p "%PDF-" text) (uiop:string-prefix-p "%%EOF" text)
                             (and (> (length text) 1) (> (count-if (lambda (c) (> (char-code c) 127)) text) 2)))
                   (push (list at end text) comments)))))
    (nreverse comments)))

(defun check-comments (pdf)
  "Comments written between the objects (FILE-COMMENTS), and the programs they name."
  (let ((octets (pdf-octets pdf)) (comments (file-comments pdf)))
    (when comments
      (let ((tools (tools-in (format nil "~{~a~^ ~}" (mapcar #'third comments)))))
        (list (found (if tools :look :record)
                     (format nil "~d comment~:p ~:*~[~;is~:;are~] written between the objects~@[, naming ~{~a~^, ~}~]: ~
                                  notes left by the programs that wrote the file."
                             (length comments) (mapcar #'second tools))
                     :rule (concatenate 'string "Lines beginning with % between objects are comments, passed over by "
                                                "readers; programs sometimes sign their work in them.")
                     :revision (let ((r (revision-of-offset pdf (first (first comments))))) (if r (revision-index r) 0))
                     :details (loop for (nil nil text) in comments repeat 20 collect text)
                     :evidence (loop for (at end) in comments
                                     repeat 20
                                     append (octets-evidence "A comment" octets at end))
                     :method :pdf-editor))))))

(defun check-packed-changes (pdf)
  "Saves whose changes are kept in object streams."
  (loop for r across (pdf-revisions pdf)
        when (and (plusp (revision-index r))
                  (some (lambda (n) (let ((e (gethash n (revision-entries r)))) (and e (eq (entry-kind e) :packed))))
                        (append (revision-added r) (revision-changed r))))
          collect (found :record (format nil "~@(~a~) keeps the objects it changes in compressed object streams: they ~
                                              cannot be read in the file without decoding them." (revision-name r))
                         :rule "Objects of type 2 in a cross-reference stream are kept inside an object stream."
                         :revision (revision-index r))))

(defun length-reaches-endstream-p (octets start length)
  "True when LENGTH octets of a stream's data from START in OCTETS end where
endstream begins, or one line end before it (a line feed, a carriage
return, or the two). A reader takes the line end before endstream as no
part of the data, but a writer may end the data with a carriage return or
a line feed of its own, counted in its /Length: then the data and the line
end read as one line end, and the /Length is one octet past where the data
seems to end, and right."
  (let ((at (+ start length)))
    (and (<= 0 length) (<= at (length octets))
         (or (octets-at-p octets "endstream" at)
             (and (< at (length octets)) (member (aref octets at) '(10 13))
                  (octets-at-p octets "endstream" (1+ at)))
             (and (< (1+ at) (length octets)) (= (aref octets at) 13) (= (aref octets (1+ at)) 10)
                  (octets-at-p octets "endstream" (+ at 2)))))))

(defun check-lengths (pdf)
  "Streams whose /Length disagrees with where endstream is: one that does
not reach endstream, nor a line end before it (LENGTH-REACHES-ENDSTREAM-P)."
  (let ((wrong '()) (last (last-revision pdf)) (octets (pdf-octets pdf)))
    (loop for o across (pdf-found pdf)
          when (pdf-indirect-get o :stream-start)
            do (let ((length (deref pdf last (pdf-indirect-get o :length))))
                 (when (and (integerp length)
                            (/= length (- (pdf-indirect-get o :stream-end) (pdf-indirect-get o :stream-start)))
                            (not (length-reaches-endstream-p octets (pdf-indirect-get o :stream-start) length)))
                   (push (list o length) wrong))))
    (when wrong
      (setf wrong (nreverse wrong))
      (list (found :look (format nil "~d stream~:p ~:*~[~;has~:;have~] a /Length that disagrees with where endstream ~
                                      is: changed by hand or by a program that did not write the length again."
                                 (length wrong))
                   :rule "A stream's /Length gives how many octets lie between stream and endstream."
                   :revision (revision-index last)
                   :objects (mapcar (lambda (w) (pdf-indirect-get (first w) :number)) wrong)
                   :evidence (loop for (o length) in wrong repeat 20
                                   collect (format nil "object ~d at ~a: /Length ~:d, but endstream comes after ~:d ~
                                                        octets (data from ~a)"
                                                   (pdf-indirect-get o :number)
                                                   (offset-words (pdf-indirect-get o :at))
                                                   length
                                                   (- (pdf-indirect-get o :stream-end)
                                                      (pdf-indirect-get o :stream-start))
                                                   (offset-words (pdf-indirect-get o :stream-start))))
                   :method :structure-by-hand)))))
