;;;; reference.lisp --- A questioned PDF set beside known-genuine files of the same issuer.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A forger who writes a document again with a tool of his own leaves a
;;;; clean file: nothing in it disagrees with itself. It disagrees with the
;;;; issuer. A bank, an employer or a billing system writes every document
;;;; with one program in one way: the same producer, the same kind of
;;;; cross-references, the same fonts kept the same way, the same page and
;;;; the same operators drawing its text. Each such habit is read here from
;;;; the questioned file and from the genuine files given, and a habit in
;;;; which the questioned file stands apart from all of them is a finding
;;;; that says what they do and what it does instead.

(in-package #:palimpsest)

(defparameter *references-most* 12
  "The most known-genuine files a questioned file is set beside; those given
past it are left out, and the report says so.")

(defvar *references* '()
  "The known-genuine files the file being examined is set beside, each a PDF
read (READ-PDF); NIL when none was given.")

(defvar *references-left-out* '()
  "The known-genuine files given that could not be set beside the file being
examined, each (NAME WHY).")

(defparameter *habits*
  '((:producer :program :same "the producer")
    (:creator :program :same "the creator")
    (:header :build :same "the PDF version and the comment under the first line")
    (:index :build :same "the cross-reference section")
    (:saves :dress :same "the saves")
    (:order :build :same "the order of the objects")
    (:trailer :build :same "the trailer's keys")
    (:id :build :same "the two parts of the /ID")
    (:date-form :metadata :same "the form of the creation date")
    (:xmp :metadata :same "the XMP metadata")
    (:streams :build :among "how the pages' content is packed")
    (:fonts-kept :build :among "how fonts are kept")
    (:operators :build :among "the operators drawing text")
    (:fields :metadata :fields "the fields of the document information")
    (:page-sizes :dress :among "the page size")
    (:font-names :dress :among "the fonts")
    (:sizes :dress :among "the sizes of text")
    (:pictures :dress :among "how pictures are kept")
    (:edges :dress :edges "where text begins and ends across the page"))
  "The habits of an issuer's system read from a file (FILE-HABITS), each (KEY
CLASS MODE NAME). CLASS is :PROGRAM for what names the program that wrote
the file, :BUILD for how that program puts a file together whatever the
document says, :METADATA for what the file says of itself beyond that
name, which a program upstream of the issuer's may set, and :DRESS for how
the document looks, which a new template changes honestly. MODE says how a
questioned file's value is set beside the references': :SAME, the one value
every reference has; :AMONG, each of a list of values found in some
reference; :FIELDS, as :AMONG and no value that every reference has left
out; :EDGES, the edges of runs of text, each run beginning or ending where
a run of a reference does. NAME is the habit in words. The habits of class
:BUILD and mode :SAME are the strict ones: no honest file measured stood
apart from the others of its program in one, as doc/measures.org says under
Reference comparison.")

(defun habit-entry (key)
  "The entry of *HABITS* for KEY."
  (assoc key *habits*))

(defun first-save-objects (pdf)
  "The objects PDF's first save writes, in the order written, as far as the
first 2,000."
  (let ((end (revision-end (aref (pdf-revisions pdf) 0))))
    (loop for object across (pdf-found pdf)
          repeat 2000
          while (< (pdf-indirect-get object :at) end)
          collect object)))

(defun header-habit (pdf)
  "PDF's version as its first line says it, and the octets of the comment on
its second line in hex, which each program writes its own way."
  (let* ((octets (pdf-octets pdf))
         (line (position 10 octets :start (pdf-header pdf)))
         (start (and line (position-if-not (lambda (o) (member o '(10 13))) octets :start line)))
         (end (and start (or (position-if (lambda (o) (member o '(10 13))) octets :start start) (length octets)))))
    (format nil "PDF ~a, ~:[no comment under it~;~:*the comment ~a under it~]" (or (pdf-version pdf) "of no version")
            (and start (= (aref octets start) 37) (< (- end start) 12)
                 (format nil "~{~2,'0x~}" (coerce (subseq octets (1+ start) end) 'list))))))

(defun index-habit (pdf)
  "How PDF's first save keeps its cross-references, in words, and whether it
packs objects in object streams."
  (let* ((first (aref (pdf-revisions pdf) 0))
         (packed (loop for entry being the hash-values of (revision-entries first)
                       thereis (eq (entry-kind entry) :packed))))
    (format nil "~a~:[~;, objects packed in streams~]~:[~;, prepared for the web~]"
            (ecase (revision-xref-kind first)
              (:table "a table") (:stream "a stream") (:hybrid "a table and a stream") (:none "none found"))
            packed (revision-linearised first))))

(defun order-habit (pdf)
  "The order PDF's first save writes its objects in, in words: whether their
numbers rise through the file, and where among them the catalogue and the
document information stand."
  (let* ((first (aref (pdf-revisions pdf) 0))
         (numbers (mapcar (lambda (object) (pdf-indirect-get object :number)) (first-save-objects pdf))))
    (flet ((place (key)
             (let* ((value (pdf-get (revision-trailer first) key))
                    (at (and (pdf-ref-p value) (position (second value) numbers))))
               (cond ((not (pdf-ref-p value)) "not there")
                     ((null at) "packed in a stream")
                     ((zerop at) "written first")
                     ((= at (1- (length numbers))) "written last")
                     ((>= at (- (length numbers) 3)) "among the last three")
                     ((< at 3) "among the first three")
                     (t "in between")))))
      (format nil "numbers ~:[not in rising order~;rising~], the catalogue ~a, the document information ~a"
              (and numbers (apply #'< numbers)) (place "/Root") (place "/Info")))))

(defun date-form (text)
  "TEXT, a date as a PDF writes it, with every digit made a 9: its form."
  (substitute-if #\9 #'digit-char-p text))

(defparameter *text-operators*
  '("BT" "ET" "Tf" "Td" "TD" "Tm" "T*" "Tj" "TJ" "'" "\"" "Tc" "Tw" "Tz" "TL" "Tr" "Ts" "BDC" "BMC" "EMC")
  "The operators that draw and place text and mark its content: those a
program uses the same way whatever a document says. The operators that
draw lines, boxes and pictures depend on what the document holds, and are
no habit.")

(defun content-habits (pdf revision)
  "How the content streams of REVISION's first pages are packed and what
operators draw their text, as two values: the filters' names, and the
operators of *TEXT-OPERATORS* met, each once. The first 3 pages, 20,000
operators of each at most."
  (let ((filters '()) (operators '()))
    (loop for page in (pages-of pdf revision) repeat 3
          do (dolist (number (page-contents page))
               (let ((object (revision-object pdf revision number)))
                 (when object
                   (let ((names (mapcar #'car (pdf-stream-filters (pdf-indirect-get object :value)))))
                     (pushnew (if names (format nil "~{~a~^ ~}" names) "not packed") filters :test #'string=))
                   (let ((data (ignore-errors (stream-data pdf object))))
                     (when data
                       (map-pdf-content (lambda (operator operands from end)
                                          (declare (ignore operands from end))
                                          (when (member operator *text-operators* :test #'equal)
                                            (pushnew operator operators :test #'string=)))
                                        data :limit 20000)))))))
    (values (nreverse filters) (nreverse operators))))

(defun font-kept-words (font)
  "How FONT is kept in its file, in words: its kind, whether its program is
embedded and how a subset of it is tagged, and whether it maps its codes to
text."
  (format nil "~a, ~a~:[~;, with a map to text~]"
          (string-left-trim "/" (or (font-subtype font) "of no kind"))
          (cond ((and (font-subset font) (numbered-tag-p (font-subset font))) "a subset tagged by number")
                ((font-subset font) "a subset tagged with letters")
                ((font-embedded font) "embedded whole or untagged")
                (t "not embedded"))
          (font-unicode font)))

(defun picture-kept-words (pdf revision mark)
  "How the picture MARK of REVISION is kept, in words: its filters, its colour
space and bits, and for a JPEG the tables it was saved with."
  (let* ((dict (mark-actual mark))
         (filters (mapcar #'car (pdf-stream-filters dict)))
         (space (let ((value (deref pdf revision (pdf-get dict "/ColorSpace"))))
                  (cond ((stringp value) value) ((consp value) (first (pdf-array-items value))))))
         (object (revision-object pdf revision (mark-object mark)))
         (tables (and object (= 1 (length filters)) (jpeg-filter-p (first filters))
                      (multiple-value-list (ignore-errors (jpeg-tables (raw-stream pdf object 65536)))))))
    (format nil "~:[not packed~;~:*~{~a~^ ~}~]~@[, ~a~]~@[, ~d bits~]~@[, JPEG tables ~a~]~@[, about quality ~d~]"
            filters (and (stringp space) space) (pdf-get dict "/BitsPerComponent")
            (and (first tables) (fnv-hex (first tables))) (second tables))))

(defun text-runs (pdf revision &optional (pages 3))
  "The runs of text the first PAGES pages of REVISION draw that show, each
\(MARK PAGE): invisible text, a scan's text recognition, is left out."
  (loop for page in (pages-of pdf revision) repeat pages
        append (loop for m across (marks-of pdf revision page)
                     when (and (eq (mark-kind m) :text) (not (member (mark-mode m) '(3 7)))
                               (find-if-not (lambda (c) (char= c #\Space)) (mark-text m)))
                       collect (list m page))))

(defun size-words (mark)
  "The size of MARK's text as habits name it: to a tenth of a point."
  (format nil "~,1f pt" (mark-size mark)))

(defun file-habits (pdf)
  "The habits of the system that wrote PDF, each (KEY VALUE) for the keys of
*HABITS*: VALUE a string, a list of strings for a habit of several values,
or for :EDGES the left and right edge of each run of text (TEXT-RUNS) in
points, a cons for each. Read from its first save where the habit is of how
the file was made, from the file as it is now where it is of what the pages
show. Worked out once."
  (kept-for-pdf (pdf :habits)
    (let* ((first (aref (pdf-revisions pdf) 0))
           (last (last-revision pdf))
           (info (info-of pdf last))
           (id (pdf-array-items (pdf-get (revision-trailer first) "/ID")))
           (runs (text-runs pdf last))
           (fonts (font-uses pdf last))
           (strings (lambda (list) (sort (remove-duplicates list :test #'string=) #'string<))))
      (multiple-value-bind (filters operators) (content-habits pdf last)
        (list
         (list :producer (or (info-text pdf last "/Producer") "none named"))
         (list :creator (or (info-text pdf last "/Creator") "none named"))
         (list :header (header-habit pdf))
         (list :index (index-habit pdf))
         (list :saves (if (= 1 (length (pdf-revisions pdf))) "saved once" "saved more than once"))
         (list :order (order-habit pdf))
         (list :trailer (format nil "~{~a~^ ~}" (funcall strings (and (pdf-dictionary-p (revision-trailer first))
                                                                      (mapcar #'car (revision-trailer first))))))
         (list :id (cond ((/= (length id) 2) "none")
                         ((equal (first id) (second id)) "alike")
                         (t "unlike")))
         (list :date-form (let ((date (dref pdf last info "/CreationDate")))
                            (if (stringp date) (date-form date) "no creation date")))
         (list :xmp (let ((fields (xmp-of pdf last)))
                      (if fields
                          (format nil "XMP of ~:d field~:p" (length fields))
                          "none")))
         (list :streams (funcall strings filters))
         (list :fonts-kept (funcall strings (mapcar (lambda (use) (font-kept-words (first use))) fonts)))
         (list :operators (funcall strings operators))
         (list :fields (funcall strings (mapcar #'car info)))
         (list :page-sizes (funcall strings (loop for page in (pages-of pdf last)
                                                  for box = (or (page-crop page) (page-media page))
                                                  when box
                                                    collect (format nil "~d by ~d pt"
                                                                    (round (- (third box) (first box)))
                                                                    (round (- (fourth box) (second box)))))))
         (list :font-names (funcall strings (remove nil (mapcar (lambda (use) (font-base (first use))) fonts))))
         (list :sizes (funcall strings (mapcar (lambda (run) (size-words (first run))) runs)))
         (list :pictures (funcall strings (loop for (m) in (pictures-of pdf last) repeat 40
                                                collect (picture-kept-words pdf last m))))
         (list :edges (loop for (m) in runs collect (cons (mark-x0 m) (mark-x1 m)))))))))

;;; A file set beside the references

(defun habit-value (habits key)
  "The value of the habit KEY among HABITS, as FILE-HABITS gives them."
  (second (assoc key habits)))

(defun habit-words (value)
  "VALUE, a habit's value, in words: a list's values with commas between,
\"none\" for an empty one."
  (cond ((stringp value) value)
        ((null value) "none")
        (t (format nil "~{~a~^, ~}" value))))

(defun producer-name (producer)
  "PRODUCER, the name of a program as a file gives it, without its version:
what stands before its first digit."
  (string-right-trim " -_(vV" (subseq producer 0 (or (position-if #'digit-char-p producer) (length producer)))))

(defun same-program-p (a b)
  "True when the habits A and B name the same producer and creator, and name
a producer at all."
  (and (string/= (habit-value a :producer) "none named")
       (string= (habit-value a :producer) (habit-value b :producer))
       (string= (habit-value a :creator) (habit-value b :creator))))

(defun habit-departure (key ours theirs)
  "How the questioned file's habits OURS stand apart in the habit KEY from
THEIRS, the habits of each reference, as (THEY IT) in words; NIL when they
do not, or the references do not agree among themselves. For a habit of
several values a second value gives those of the file that no reference has
\(and, of mode :FIELDS, a third those every reference has and it lacks)."
  (let ((value (habit-value ours key))
        (others (mapcar (lambda (habits) (habit-value habits key)) theirs)))
    (ecase (third (habit-entry key))
      (:same
       (and (every (lambda (other) (equal other (first others))) others)
            (not (equal value (first others)))
            (list (first others) value)))
      ((:among :fields)
       (let* ((all (remove-duplicates (apply #'append others) :test #'string=))
              (extra (remove-if (lambda (v) (member v all :test #'string=)) value))
              (missing (and (eq (third (habit-entry key)) :fields)
                            (remove-if (lambda (v) (member v value :test #'string=))
                                       (reduce (lambda (a b) (intersection a b :test #'string=)) others)))))
         (and (or extra missing)
              (values (list (habit-words (sort (copy-list all) #'string<))
                            (cond ((and extra missing)
                                   (format nil "~{~a~^, ~}, and lacks ~{~a~^, ~}, which each of them has"
                                           extra missing))
                                  (extra (habit-words extra))
                                  (t (format nil "none of ~{~a~^, ~}, which each of them has" missing))))
                      extra missing)))))))

(defparameter *edge-slack* 0.75
  "How far, in points, an edge of a run of text may lie from an edge of a
reference's run and still be where that one is.")

(defun off-grid (edges others)
  "The indexes of the runs of EDGES (a file's :EDGES habit) that neither
begin where a run of OTHERS, a list of such habits, begins nor end where one
ends, within *EDGE-SLACK*."
  (let ((lefts (sort (coerce (loop for other in others append (mapcar #'car other)) 'vector) #'<))
        (rights (sort (coerce (loop for other in others append (mapcar #'cdr other)) 'vector) #'<)))
    (flet ((near-p (x sorted)
             (let ((low 0) (high (length sorted)))
               (loop while (< low high)
                     do (let ((middle (floor (+ low high) 2)))
                          (if (< (aref sorted middle) (- x *edge-slack*)) (setf low (1+ middle)) (setf high middle))))
               (and (< low (length sorted)) (<= (aref sorted low) (+ x *edge-slack*))))))
      (loop for (left . right) in edges for index from 0
            unless (or (near-p left lefts) (near-p right rights))
              collect index))))

(defun grid-departure (ours theirs)
  "The indexes of the questioned file's runs of text (TEXT-RUNS) that sit off
the grid of the references: OURS and THEIRS are their habits. The share of
each reference's runs off the grid of the other references says how much an
honest document of this issuer strays; the questioned file's runs off the
grid are given when their share is more than twice the largest of those and
a fiftieth more. With one reference there is nothing to judge that by, and
they are given when more than half its runs are off it."
  (let* ((edges (habit-value ours :edges))
         (others (mapcar (lambda (habits) (habit-value habits :edges)) theirs))
         (off (and edges (some #'identity others) (off-grid edges others)))
         (share (and off (/ (length off) (length edges))))
         (honest (and (rest others)
                      (loop for other in others
                            when other
                              maximize (/ (length (off-grid other (remove other others :test #'eq)))
                                          (length other))))))
    (and off
         (if honest
             (> share (+ 1/50 (* 2 honest)))
             (> share 1/2))
         off)))

(defparameter *habit-causes*
  '((:saves #.(concatenate 'string "A signature, a note or a form filled in and saved does this honestly; so does "
                                   "an edit kept as a later save."))
    (:page-sizes #.(concatenate 'string "Another office or a new template uses another paper honestly; a page "
                                        "printed anew has the printer's."))
    (:font-names #.(concatenate 'string "A new template or another office's letterhead brings a font honestly; so "
                                        "does text typed in by another program."))
    (:sizes #.(concatenate 'string "A new template or a heading the other documents lack is honest; so is text typed "
                                   "in at a size of the editor's own."))
    (:pictures #.(concatenate 'string "A new logo or a scan from another machine is honest; so is a picture put in "
                                      "or saved again by an editor."))
    (:edges #.(concatenate 'string "A centred line of another length or a new template is honest; text set by hand, "
                                   "or a page printed or made again, sits off the issuer's grid too."))
    (:streams "")
    (:fonts-kept "")
    (:operators ""))
  "For each habit of how a document looks, and each of several values, what
explains honestly a file standing apart in it and what else does, as a
sentence ending its finding.")

(defun reference-names (references)
  "The names of the files REFERENCES, read PDFs, as a finding's evidence lists them."
  (format nil "set beside: ~{~a~^, ~}" (mapcar #'pdf-name references)))

(defun run-lines (pdf runs)
  "RUNS, each (MARK PAGE) of PDF's pages, as a finding's details name them:
the first four, each with its page and place."
  (declare (ignore pdf))
  (loop for (m page) in runs repeat 4
        collect (format nil "page ~d: ~a" (1+ (page-index page)) (mark-words page m))))

(defun program-finding (ours theirs references departures strict-p)
  "The finding of the questioned file naming another program than every
reference: OURS and THEIRS are their habits, REFERENCES the files, and
DEPARTURES the other habits it stands apart in, each (NAME THEY IT), which
another program explains. NIL when it names what they name, or they do not
agree. Worth a look; for the record when only the version differs and,
STRICT-P being NIL, the file is put together as theirs are."
  (let ((producer (habit-departure :producer ours theirs))
        (creator (habit-departure :creator ours theirs)))
    (when (or producer creator)
      (let ((update (and producer (not creator)
                         (> (length (producer-name (first producer))) 2)
                         (string= (producer-name (first producer)) (producer-name (second producer))))))
        (found (if (and update (not strict-p)) :record :look)
               (format nil "The ~:d known-genuine file~:p ~:*~[~;names~:;all name~] ~{~a~^ and ~}; this file names ~
                            ~{~a~^ and ~}. ~:[Another program wrote this file. The issuer changing its system, or ~
                            another office with a system of its own, explains that honestly; so does the file being ~
                            printed to PDF, scanned or saved again by whoever passed it on; so does a document made ~
                            again to look like the issuer's.~;The same program in another version: a software update ~
                            at the issuer between the documents explains it.~]~@[ As a file of another program ~
                            would, it also differs in ~{~a~^, ~}.~]~:[~; An update seldom changes how a program puts ~
                            its files together: this file may have been written again by another program.~]"
                       (length theirs)
                       (append (and producer (list (format nil "~s as producer" (first producer))))
                               (and creator (list (format nil "~s as creator" (first creator)))))
                       (append (and producer (list (format nil "~s" (second producer))))
                               (and creator (list (format nil "~s" (second creator)))))
                       update (mapcar #'first departures) (and update strict-p))
               :rule #.(concatenate 'string "An issuer's system names itself the same way in every document it "
                                            "writes. The producer and creator of the known-genuine files are set "
                                            "beside this file's.")
               :details (loop for (name they it) in departures
                              collect (format nil "~a: the references ~a; this file ~a" name they it))
               :evidence (list (reference-names references))
               :method (and (not update) :other-program))))))

(defun build-finding (ours same references departures soft)
  "The finding of the questioned file being put together otherwise than the
references that name its own program: OURS its habits, SAME those
references, REFERENCES all of them, DEPARTURES the strict habits it stands
apart in from all of SAME, each (NAME THEY IT), and SOFT the same for the
habits of several values and of what the file says of itself. Worth a look,
never strong: a file written again with nothing changed, by a tool that tidies
files or a portal, stands apart in the same way as a changed one. The words
say more for two such references or more that agree, the producer naming its
version: the program named did not write the file as it stands. For one
reference, and for a producer that names no version, whose update would not
show in its name, they say why less can be told."
  (let* ((versioned (find-if #'digit-char-p (habit-value ours :producer)))
         (agreed (and (rest same) versioned)))
    (found :look
           (format nil "This file names the same producer and creator as ~:d of the known-genuine files, and is not ~
                        put together as ~:*~[~;that one is~:;they are~]: ~{~a~^; ~}. ~a"
                   (length same)
                   (loop for (name they it) in departures
                         collect (format nil "~a (theirs: ~a; this file's: ~a)" name they it))
                   (cond (agreed
                          #.(concatenate 'string "The program named did not write this file as it stands: another "
                                                 "program wrote it again and kept the name. A software update or "
                                                 "another office would change the name or the version. An archive, "
                                                 "a portal or a tool that tidies files writes them again honestly, "
                                                 "with nothing changed, so ask how the file reached you; an editor "
                                                 "that keeps the issuer's name does it too. This says the file was "
                                                 "written again, not that anything in it was changed."))
                         ((not versioned)
                          #.(concatenate 'string "The producer names no version, so a software update at the issuer "
                                                 "would not show in its name and cannot be ruled out; else another "
                                                 "program wrote the file again and kept the name."))
                         (t #.(concatenate 'string "One genuine file is little to judge by: two or more that agree "
                                                   "would tell the issuer's habit from a chance."))))
           :rule #.(concatenate 'string "A program puts every file together in one way, whatever the document says: "
                                        "its first lines, its cross-reference section, the order of its objects, its "
                                        "trailer and /ID. Where the known-genuine files written by the program this "
                                        "file names all agree and this file differs, that program did not write it.")
           :details (loop for (name they it) in soft
                          collect (format nil "~a: the references ~a; this file also ~a" name they it))
           :evidence (list (reference-names references)
                           (format nil "naming the same program: ~{~a~^, ~}" (mapcar #'pdf-name same)))
           :revision 0
           :method :other-program)))

(defun dress-finding (pdf key departure runs references)
  "The finding, worth a look, of the questioned file PDF standing apart from
every one of REFERENCES in the habit KEY: DEPARTURE as HABIT-DEPARTURE gives
it, RUNS the runs of its text that show it, each (MARK PAGE), or NIL."
  (let ((last (last-revision pdf)))
    (found :look
           (format nil "~@(~a~): in the ~:d known-genuine file~:p, ~a; in this file, ~a~@[, in ~:d run~:p of ~
                        text~]. ~a"
                   (fourth (habit-entry key)) (length references) (first departure) (second departure)
                   (and runs (length runs)) (second (assoc key *habit-causes*)))
           :rule #.(concatenate 'string "What an issuer's documents share from one to the next is read from the "
                                        "known-genuine files; this file stands apart from every one of them here.")
           :details (run-lines pdf runs)
           :evidence (append (list (reference-names references))
                             (loop for (m) in runs repeat 2
                                   append (mark-evidence pdf last m)))
           :page (and runs (1+ (page-index (second (first runs)))))
           :objects (remove-duplicates (remove nil (loop for (m) in runs collect (mark-source m))))
           :method (case key
                     ((:font-names :sizes :edges) :retyped)
                     (:saves :saved-again)
                     (:page-sizes :pages-changed)))))

(defun check-references (pdf &optional (references *references*))
  "The findings of PDF set beside REFERENCES, known-genuine files of the same
issuer (read PDFs): each habit of *HABITS* in which it stands apart from all
of them, saying what they do and what it does instead. Another program
named is worth a look (PROGRAM-FINDING), with what else differs in how the
file is put together and what it says of itself told there.
The same program named and the file put together otherwise is worth a look
too (BUILD-FINDING), since a file only written again stands apart in the same
way. A habit
of how the document looks is worth a look, with the runs of text that show
it (DRESS-FINDING). NIL without references."
  (when references
    (let* ((ours (file-habits pdf))
           (theirs (mapcar #'file-habits references))
           (same (loop for reference in references for habits in theirs
                       when (same-program-p ours habits) collect reference))
           (same-habits (mapcar #'file-habits same))
           (runs (text-runs pdf (last-revision pdf)))
           (out '()))
      (flet ((departures (class modes habits)
               (loop for (key habit-class mode name) in *habits*
                     for departure = (and (eq habit-class class) (member mode modes) habits
                                          (habit-departure key ours habits))
                     when departure collect (list name (first departure) (second departure)))))
        (let* ((metadata (departures :metadata '(:same :fields) theirs))
               (program (program-finding ours theirs references
                                         (append (departures :build '(:same) theirs)
                                                 (departures :build '(:among) theirs)
                                                 metadata)
                                         (departures :build '(:same) theirs))))
          (cond (program (push program out))
                (t (let ((strict (departures :build '(:same) (or same-habits theirs)))
                         (soft (departures :build '(:among) (or same-habits theirs))))
                     (cond ((and strict same)
                            (push (build-finding ours same references strict (append soft metadata)) out)
                            (setf metadata '()))
                           ((or strict soft)
                            (push (found :look
                                         (format nil "This file is not put together as the ~:d known-genuine ~
                                                      file~:p ~:*~[~;is~:;are~]: ~{~a~^; ~}. Another program wrote ~
                                                      it or wrote it again: an archive or a portal that writes ~
                                                      every file again does so honestly, and so does an editor.~
                                                      ~:[~; What its pages hold beyond theirs may be a part the ~
                                                      other documents lack, a picture or a ruled line.~]"
                                                 (length references)
                                                 (loop for (name they it) in (append strict soft)
                                                       collect (format nil "~a (theirs: ~a; this file's: ~a)"
                                                                       name they it))
                                                 (null strict))
                                         :rule #.(concatenate 'string "A program puts every file together in one "
                                                                      "way, whatever the document says; the "
                                                                      "known-genuine files show the issuer's.")
                                         :evidence (list (reference-names references))
                                         :revision 0 :method :other-program)
                                  out)))
                     (when metadata
                       (push (found :look
                                    (format nil "What this file says of itself is not as in the ~:d known-genuine ~
                                                 file~:p: ~{~a~^; ~}. A program upstream of the issuer's own, or a ~
                                                 title or an author filled in for one document and not another, ~
                                                 explains it honestly; so does a tool that writes metadata."
                                            (length references)
                                            (loop for (name they it) in metadata
                                                  collect (format nil "~a (theirs: ~a; this file's: ~a)" name they it)))
                                    :rule #.(concatenate 'string "An issuer's system fills in the same fields of "
                                                                 "the document information and the XMP, in the "
                                                                 "same form, in every document it writes.")
                                    :evidence (list (reference-names references))
                                    :method :metadata-edited)
                             out))))))
        (loop for (key class mode) in *habits*
              when (eq class :dress)
                do (if (eq mode :edges)
                       (let ((off (grid-departure ours theirs)))
                         (when off
                           (push (dress-finding pdf key
                                                (list "every run of text begins or ends on one grid"
                                                      (format nil "~:d of ~:d runs of text off that grid"
                                                              (length off) (length runs)))
                                                (mapcar (lambda (index) (nth index runs)) off) references)
                                 out)))
                       (multiple-value-bind (departure extra) (habit-departure key ours theirs)
                         (when departure
                           (push (dress-finding
                                  pdf key departure
                                  (case key
                                    (:font-names
                                     (remove-if-not (lambda (run)
                                                      (let ((font (mark-font (first run))))
                                                        (and font (member (font-base font) extra :test #'equal))))
                                                    runs))
                                    (:sizes
                                     (remove-if-not (lambda (run)
                                                      (member (size-words (first run)) extra :test #'string=))
                                                    runs)))
                                  references)
                                 out)))))
        (nreverse out)))))

;;; Reading the references, and asking for them

(defun read-references (paths &optional sum)
  "The known-genuine files at PATHS read, as two values: those read, each a
PDF (READ-PDF), and those left out, each (NAME WHY). At most
*REFERENCES-MOST* are read. Left out: a file that is no PDF, one that cannot
be read, and one whose SHA-256 is SUM, the questioned file's own, which
would agree with itself in everything."
  (let ((read '()) (left '()))
    (dolist (path paths (values (nreverse read) (nreverse left)))
      (let ((name (native-file-name path)))
        (if (>= (length read) *references-most*)
            (push (list name (format nil "more than ~:d files given" *references-most*)) left)
            (handler-case
                (let ((octets (source-octets path)))
                  (cond ((not (pdf-header-at octets)) (push (list name "not a PDF") left))
                        ((and sum (equal (octets-sha256 octets) sum))
                         (push (list name "the file examined itself") left))
                        (t (push (read-pdf octets :path path :name name) read))))
              (error (condition)
                (push (list name (shorten-string (princ-to-string condition) 120 :one-line t)) left))))))))

(defun reference-paths (path)
  "The known-genuine files PATH, a native name, stands for: the PDFs of a
directory (DIRECTORY-PDFS), or the file itself."
  (if (eq :directory (ignore-errors (path-kind path)))
      (directory-pdfs path)
      (list path)))

(defun reference-pdf-p (path)
  "True when the file at PATH is a PDF by its first octets; NIL too when it cannot be read."
  (ignore-errors (pdf-file-p path)))

(define-command palimpsest-compare-genuine ()
  "Examine the report's PDF again beside known-genuine files of the same
issuer: other documents the same bank, employer or billing system wrote,
which you know to be genuine. A file asked for, or a directory, whose PDFs
are all taken (the first 12). What the issuer's system keeps the same from
one document to the next is read from them, and the report gains a check,
Beside known-genuine files, with a finding for each habit in which the
examined file stands apart from every one of them: what they do, and what
it does instead. Its findings are worth a look at most: they tell a file
the issuer's system did not write as it stands, which a file only written
again by a portal or a tidying tool is too. Two genuine files or more that
agree say more than one. A software update or
another office of the issuer changes some habits honestly, and each finding
names such causes. Run again to set it beside other files; with
\\[universal-argument], the file is examined alone again. The genuine files
are only read, shut in as the examined file is."
  (let* ((buffer (current-buffer))
         (analysis (buffer-local buffer 'analysis))
         (path (buffer-local buffer 'palimpsest-path)))
    (cond ((or (null analysis) (null path)) (message "The report is not ready yet"))
          ((picture-refused-p analysis "set beside genuine files"))
          ((analysis-busy-p analysis))
          (t (let* ((given (and (not (prefix-argument))
                                (read-file-path "Known-genuine PDF, or a directory of them: " :note nil)))
                    (references (and given (reference-paths (native (expand-path given))))))
               (cond ((and (not (prefix-argument)) (null given)))
                     ((and given (null references))
                      (message "No PDF in ~a" (abbreviated-path (native (expand-path given)))))
                     ((find-if-not #'reference-pdf-p references)
                      (message "~a is not a PDF, by its first octets"
                               (abbreviated-path (find-if-not #'reference-pdf-p references))))
                     (t (if references
                            (message "Examining ~a beside ~:d known-genuine file~:p" (analysis-name analysis)
                                     (length references))
                            (message "Examining ~a alone again" (analysis-name analysis)))
                        (start-analysis-job analysis (make-palimpsest-job buffer path (buffer-private-directory buffer)
                                                                          references)
                                            #'examine "tuatara palimpsest"))))))))
