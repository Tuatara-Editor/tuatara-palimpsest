;;;; methods.lisp --- The ways of editing the findings tell of, how practised they are, and the file's fingerprint.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each finding names the way of editing it plausibly comes from. The ways
;;;; found are listed with how much craft each takes, from a PDF editor's
;;;; text tool to characters composed out of a scan and the scan saved
;;;; again to blur them; with the programs, the cross-references and the
;;;; JPEG tables of the file they make its fingerprint. Fingerprints saved
;;;; with reports are set beside one another to group the files that share
;;;; ways and tools: the same hand, or the same kit, for the examiner to judge.

(in-package #:palimpsest)

(defparameter *methods*
  '((:saved-again 1 "Saved again after it was made"
     "The changes were kept as a later save, after the original: what any program that edits PDFs does.")
    (:other-program 1 "Changed in another program than made it"
     "The later saves name another program, or write the file another way, than the one that made it.")
    (:pdf-editor 1 "Changed in a PDF editor"
     "The file names a program made for editing PDFs.")
    (:laid-over 1 "Something laid over a page in a later save"
     "A content stream added in a later save draws over the page as it was.")
    (:text-covered 1 "Text covered by a shape, new text over it"
     "A shape the colour of the page hides text: the PDF editor's way of changing what it says.")
    (:annotation-overlay 1 "Annotations laid over the page as if they were its text"
     "Text boxes, stamps or squares put over the page, which a reader shows as if printed.")
    (:typed-on-scan 1 "Text typed onto a scan"
     "Text drawn over a page that is one scanned picture.")
    (:picture-pasted 1 "A picture put in from elsewhere"
     "A signature, stamp or logo pasted in: a picture unlike the others, or added later.")
    (:pages-changed 1 "Pages put in, taken out or replaced"
     "Pages added, removed, reordered, or unlike the rest: put in from another document.")
    (:retyped 2 "Text retyped to look like the rest"
     "Figures or words set again in another font or ink, or drawn a character at a time: changed by someone who matched what showed.")
    (:fitted 2 "Text fitted into the gap it replaced"
     "Text set in another size, squeezed or spaced so that new words fill the old ones' place.")
    (:placed-by-hand 2 "Text placed by hand"
     "Text set a little above or below its line, or off the edge the lines keep: moved into place by hand.")
    (:hidden-text 2 "Text hidden"
     "Text clipped away, off the page, in a layer that is off, invisible, the page's colour or too small to read.")
    (:font-added 2 "A font put in later"
     "A font changed after the document was made, in several subsets, or embedded on some pages only: text set at another time.")
    (:metadata-edited 2 "Dates or metadata written by hand"
     "Dates that disagree, run backwards or lie ahead, or a time zone no place uses: what the file says of itself was changed.")
    (:metadata-removed 2 "What the file says of itself taken out"
     "The document information or the XMP metadata removed in a later save: traces cleared.")
    (:pasted-on-scan 2 "Words pasted into a scan"
     "Words of another ink, edge or paper than the scan's: printed or pasted onto it.")
    (:structure-by-hand 3 "The file's structure written by hand"
     "Cross-references, lengths or objects that do not agree with the file: its octets edited directly, or by a careless program.")
    (:moved-in-scan 3 "Words of a scan moved or turned"
     "Words of the scan's own ink set off their line or turned: cut out and put back, the scan's look kept.")
    (:characters-composed 3 "Characters cut out of the scan and composed"
     "Characters copied from the scan itself, pixel for pixel, to make new figures or words that match it.")
    (:signature-bypassed 3 "Changes made past a signature"
     "Changes saved after a signature, outside what it covers, or objects hidden in what it covers and used later.")
    (:characters-composed-resaved 4 "Characters composed, then the scan saved again"
     "Copies within the scan that are no longer alike pixel for pixel: saved again as a JPEG, which blurs the traces of the cutting."))
  "The ways of editing a finding may tell of, each (KEY LEVEL NAME WHY):
LEVEL the craft it takes (*CRAFT-LEVELS*), NAME what it is in a few words,
WHY what in the file shows it.")

(defparameter *craft-levels*
  '((1 "plain" "what anyone does with a PDF editor or a picture editor's text tool")
    (2 "practised" "what someone does who matches what shows: fonts, sizes, places, the file's own account of itself")
    (3 "skilled" "work in the scan's pixels, or round the file's structure and its signatures")
    (4 "expert" "skilled work with its traces covered as well"))
  "The craft a way of editing takes, each (LEVEL NAME WHAT).")

(defun method-of (key)
  "The entry of *METHODS* for KEY."
  (assoc key *methods*))

(defun methods-found (findings)
  "The ways of editing FINDINGS tell of, each (KEY LEVEL NAME WHY FINDINGS),
the most practised first, then by how many findings show them. The findings
for the record tell of none."
  (let ((ways '()))
    (dolist (f findings)
      (when (and (finding-method f) (not (eq (finding-strength f) :record)))
        (let ((entry (assoc (finding-method f) ways)))
          (if entry (push f (cdr entry)) (push (list (finding-method f) f) ways)))))
    (sort (loop for (key . some) in ways
                collect (append (method-of key) (list (reverse some))))
          (lambda (a b) (or (> (second a) (second b))
                            (and (= (second a) (second b)) (> (length (fifth a)) (length (fifth b)))))))))

(defun strong-way-p (way)
  "True when WAY (METHODS-FOUND) is shown by a strong sign, not only by signs worth a look."
  (find :strong (fifth way) :key #'finding-strength))

(defun craft-words (ways)
  "What WAYS (METHODS-FOUND) say of the craft of the editing, in words: the
highest level found and how many ways at each; and, when only signs worth
a look show that level, so, with the level the strong signs show."
  (if (null ways)
      "No way of editing shown."
      (let* ((top (reduce #'max ways :key #'second))
             (level (assoc top *craft-levels*))
             (strong (remove-if-not #'strong-way-p ways))
             (top-strong (and strong (reduce #'max strong :key #'second))))
        (format nil "~@(~a~), level ~d of 4: ~a. ~{~a~^, ~}.~@[ ~a~]"
                (second level) top (third level)
                (loop for (n name) in *craft-levels*
                      for some = (count n ways :key #'second)
                      when (plusp some) collect (format nil "~d way~:p ~a" some name))
                (cond ((eql top-strong top) nil)
                      (top-strong (format nil "Only signs worth a look show level ~d; the strong signs show level ~d." top top-strong))
                      (t (format nil "None of the signs is strong: an honest file can show as much.")))))))

;;; The fingerprint

(defun fnv-hex (octets)
  "A short name for OCTETS, the same for the same octets: their 64-bit FNV-1a hash, in 16 hex digits."
  (let ((hash #xcbf29ce484222325))
    (loop for o across octets
          do (setf hash (ldb (byte 64 0) (* (logxor hash o) #x100000001b3))))
    (format nil "~(~16,'0x~)" hash)))

(defparameter *jpeg-luminance-table*
  #(16 11 12 14 12 10 16 14 13 14 18 17 16 19 24 40 26 24 22 22 24 49 35 37 29 40 58 51 61 60 57 51
    56 55 64 72 92 78 64 68 87 69 55 56 80 109 81 87 95 98 103 104 103 62 77 113 121 112 100 120 92 101 103 99)
  "The luminance table of the JPEG standard's example, in the order a JPEG
keeps it, that the usual encoders scale for each quality.")

(defun jpeg-tables (octets)
  "The quantisation tables a JPEG's OCTETS hold (its DQT segments), as two
values: their octets together, and the quality from 1 to 100 its first
table stands for when made by scaling the standard's (NIL for none). A
segment that says it is longer than the octets hold is read as far as they
go; its table, cut short, is not read for the quality."
  (let ((tables (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (first-table nil) (i 2) (n (length octets)))
    (loop while (< (+ i 4) n)
          do (if (/= (aref octets i) #xff)
                 (return)
                 (let ((marker (aref octets (1+ i)))
                       (length (+ (* 256 (aref octets (+ i 2))) (aref octets (+ i 3)))))
                   (when (= marker #xdb)
                     (loop for k from (+ i 4) below (min n (+ i 2 length)) do (vector-push-extend (aref octets k) tables))
                     ;; a first table read only when the segment and the octets hold all of it
                     (when (and (null first-table) (< (+ i 4) (min n (+ i 2 length)))
                                (<= (+ i 5 (if (plusp (ldb (byte 4 4) (aref octets (+ i 4)))) 128 64)) (min n (+ i 2 length))))
                       (let ((wide (plusp (ldb (byte 4 4) (aref octets (+ i 4))))))
                         (setf first-table (loop for k below 64
                                                 collect (if wide
                                                             (+ (* 256 (aref octets (+ i 5 (* 2 k)))) (aref octets (+ i 6 (* 2 k))))
                                                             (aref octets (+ i 5 k))))))))
                   ;; the picture's data follows the start of scan
                   (when (= marker #xda) (return))
                   (incf i (+ 2 length)))))
    (values (and (plusp (length tables)) (coerce tables '(simple-array (unsigned-byte 8) (*))))
            (and first-table (= 64 (length first-table))
                 (let ((scale (/ (loop for q in first-table for s across *jpeg-luminance-table* sum (/ (* 100 q) s)) 64)))
                   (max 1 (min 100 (round (if (<= scale 100) (/ (- 200 scale) 2) (/ 5000 scale))))))))))

(defun analysis-traits (analysis)
  "ANALYSIS's fingerprint (FINGERPRINT), worked out once and kept: while it
is examined, before what was read of its pages is let go."
  (or (analysis-fingerprint analysis) (setf (analysis-fingerprint analysis) (fingerprint analysis))))

(defun fingerprint (analysis)
  "The fingerprint of ANALYSIS's PDF: what is told of who edited it and with
what, each (KIND VALUE), KIND :method (a way of editing, METHODS-FOUND),
:editor (a program a later save names, not the original), :made-by (a
program the original names), :index (how a later save keeps its
cross-references) or :jpeg (the tables a JPEG picture was saved with: the
encoder and its quality, often the forger's own when a scan was changed)."
  (let* ((pdf (analysis-pdf analysis)) (last (last-revision pdf)) (traits '()))
    (flet ((add (kind value) (when value (pushnew (list kind value) traits :test #'equal))))
      (dolist (way (methods-found (analysis-findings analysis))) (add :method (string-downcase (first way))))
      (loop for r across (pdf-revisions pdf)
            do (dolist (key '("/Producer" "/Creator"))
                 (let ((program (info-text pdf r key)))
                   (when program
                     ;; the original's programs made the document; a later save's own edited it
                     (if (or (zerop (revision-index r))
                             (equal program (info-text pdf (aref (pdf-revisions pdf) 0) key)))
                         (add :made-by program)
                         (add :editor program)))))
               (when (plusp (revision-index r))
                 (add :index (case (revision-xref-kind r) (:table "a table in a later save") (:stream "a stream in a later save")
                               (:hybrid "a hybrid in a later save")))))
      (loop for (m) in (pictures-of pdf last)
            do (let ((filters (mapcar #'car (pdf-stream-filters (mark-actual m)))))
                 (when (and (= 1 (length filters)) (member (first filters) '("/DCTDecode" "/DCT") :test #'equal))
                   (let ((object (revision-object pdf last (mark-object m))))
                     ;; the tables come before the picture's data: its first 64 KB hold them
                     (multiple-value-bind (tables quality) (and object (ignore-errors (jpeg-tables (raw-stream pdf object 65536))))
                       (when tables
                         (add :jpeg (format nil "tables ~a~@[, about quality ~d~]" (fnv-hex tables) quality))))))))
      (nreverse traits))))

(defparameter *trait-weights* '((:method . 1) (:editor . 3) (:made-by . 1/4) (:index . 1/2) (:jpeg . 2))
  "How much each kind of trait of a fingerprint counts when two are set
beside each other: the programs a forger saved with and the JPEG tables of
the pictures tell more of who edited a file than a way of editing many use;
the program that made the original tells of the document's source, not of
its editor, and counts little.")

(defun fingerprint-text (analysis)
  "ANALYSIS's fingerprint as the text of its file: a line naming what it is,
the PDF and its SHA-256, then one line for each trait, KIND: VALUE; each
control character shown as the editor shows one (ESCAPE-CONTROLS)."
  (with-output-to-string (s)
    ;; what the PDF says, a program's name, and the file's own name shown as the editor shows control
    ;; characters: a line of it never acts on a terminal, nor splits into a trait of its own
    (format s "palimpsest fingerprint 1~%file: ~a~%sha-256: ~a~%"
            (escape-controls (analysis-name analysis)) (or (analysis-sha256 analysis) "not summed"))
    (loop for (kind value) in (analysis-traits analysis)
          do (format s "~(~a~): ~a~%" kind (escape-controls value)))))

(defun fingerprint-file-name (analysis)
  "The name the fingerprint of ANALYSIS's PDF is written under: NAME-fingerprint.txt."
  (format nil "~a-fingerprint.txt" (native-file-stem (analysis-name analysis))))

(defun read-fingerprint (path)
  "The fingerprint written at PATH (FINGERPRINT-TEXT) as two values: the name
of the PDF it is of, and its traits, each (KIND VALUE); NIL when it is no
fingerprint."
  (let* ((text (ignore-errors (octets-string (file-octets path :limit (* 1024 1024)))))
         (lines (and text (mapcar (lambda (line) (string-right-trim '(#\Return) line))
                                  (uiop:split-string text :separator '(#\Newline))))))
    (when (and lines (string= (first lines) "palimpsest fingerprint 1"))
      (let ((name nil) (traits '()))
        (dolist (line (rest lines))
          (let ((colon (search ": " line)))
            (when colon
              (let ((kind (subseq line 0 colon)) (value (subseq line (+ colon 2))))
                (cond ((string= kind "file") (setf name value))
                      ((find kind '("method" "editor" "made-by" "index" "jpeg") :test #'string=)
                       (push (list (intern (string-upcase kind) :keyword) value) traits)))))))
        (values (or name (native-file-name path)) (nreverse traits))))))

(defun fingerprint-likeness (a b)
  "How alike the traits A and B of two fingerprints are, from 0 to 1: the
weight (*TRAIT-WEIGHTS*) of the traits they share over that of all either
has."
  (flet ((weight (traits) (loop for (kind) in traits sum (cdr (assoc kind *trait-weights*)))))
    (let ((both (intersection a b :test #'equal)) (either (union a b :test #'equal)))
      (if either (/ (weight both) (weight either)) 0))))

(defparameter *cases-alike* 1/2
  "How alike (FINGERPRINT-LIKENESS) two fingerprints must be for their files
to be grouped as one case.")

(defun cases-alike-p (a b)
  "True when the files of the fingerprints A and B (their traits) are one
case: they share a way of editing (two files of one source, edited by no one,
are no case) and one thing more, another way or a tool (a program a later
save names, the JPEG tables of a picture), a way alone being one many use;
and are *CASES-ALIKE* alike (FINGERPRINT-LIKENESS)."
  (let ((shared (intersection a b :test #'equal)))
    (and (find :method shared :key #'first)
         (or (> (count :method shared :key #'first) 1)
             (find-if (lambda (trait) (member (first trait) '(:editor :jpeg))) shared))
         (>= (fingerprint-likeness a b) *cases-alike*))))

(defun group-fingerprints (prints)
  "PRINTS, each (PATH NAME TRAITS), in groups: two that are one case
\(CASES-ALIKE-P) in one group, and every one alike to one in it too.
Each group (MEMBERS SHARED), SHARED the traits all its members have; the
largest groups first, a file alike to none a group of its own last."
  (let* ((n (length prints)) (parent (make-array n)) (items (coerce prints 'vector)))
    (dotimes (k n) (setf (aref parent k) k))
    (labels ((root (k) (if (= (aref parent k) k) k (setf (aref parent k) (root (aref parent k))))))
      (dotimes (i n)
        (job-check-stop)
        (loop for j from (1+ i) below n
              do (when (cases-alike-p (third (aref items i)) (third (aref items j)))
                   (setf (aref parent (root j)) (root i)))))
      (let ((groups (make-hash-table)))
        (dotimes (k n) (push (aref items k) (gethash (root k) groups)))
        (sort (loop for members being the hash-values of groups
                    collect (let ((members (reverse members)))
                              (list members (reduce (lambda (a b) (intersection a b :test #'equal)) (mapcar #'third members)))))
              #'> :key (lambda (g) (length (first g))))))))

(defun fingerprints-under (folder &key (cancel (list nil)))
  "The fingerprints in FOLDER and the folders under it, each (PATH NAME TRAITS)."
  (let ((prints '()))
    (walk-directory (slash-ended folder)
                    (lambda (dir path entry)
                      (declare (ignore path))
                      (when entry
                        (destructuring-bind (name kind &rest rest) entry
                          (declare (ignore rest))
                          (cond ((eq kind :directory) t)
                                ((and (eq kind :regular) (> (length name) 16) (string= "-fingerprint.txt" name :start2 (- (length name) 16)))
                                 (let ((path (path-join dir name)))
                                   ;; a file of its own, never a link leading out of the folder
                                   (multiple-value-bind (pdf traits) (and (eq (path-kind path :follow nil) :regular) (read-fingerprint path))
                                     (when pdf (push (list path pdf traits) prints))))
                                 nil)))))
                    :cancel cancel)
    (sort prints #'string< :key #'first)))

(define-major-mode palimpsest-cases-mode special-mode
  "The files whose fingerprints palimpsest set beside one another
\(\\[palimpsest-group-cases]), grouped as one case when they share enough of
their ways of editing and their tools, with what each group shares.
\\<palimpsest-cases-mode>\\[push-button] on a fingerprint opens it; \\[quit-window] leaves."
  :lighter "Palimpsest cases" :interactive nil)

(defun trait-words (trait)
  "TRAIT, (KIND VALUE), in words."
  (destructuring-bind (kind value) trait
    (case kind
      (:method (let ((way (find value *methods* :key (lambda (m) (string-downcase (first m))) :test #'string=)))
                 (if way (format nil "way: ~a (level ~d)" (third way) (second way)) (format nil "way: ~a" value))))
      (:editor (format nil "edited with: ~a" value))
      (:made-by (format nil "made with: ~a" value))
      (:index (format nil "cross-references: ~a" value))
      (:jpeg (format nil "JPEG ~a" value))
      (t value))))

(defparameter *cases-pairs-shown* 30
  "The most pairs of one case whose likeness the list of cases shows.")

(defun cases-pieces (folder groups)
  "The pieces of the list of GROUPS (GROUP-FINGERPRINTS) of the fingerprints
found under FOLDER: the groups of two or more, then the files alike to none."
  (let ((cases (remove-if-not #'rest groups :key #'first))
        (alone (loop for (members) in groups unless (rest members) collect (first members))))
    (collecting-pieces (put)
      (flet ((file (m)
               (destructuring-bind (path name traits) m
                 (put "  ")
                 (put (escape-controls name) :link (let ((path path)) (lambda () (visit-path path))))
                 (put (format nil "  ~:d way~:p of editing, in ~a~%" (count :method traits :key #'first)
                              (escape-controls (abbreviated-path (parent-path path))))
                      :list-note))))
        (put (format nil "Palimpsest: cases under ~a~%" (escape-controls (abbreviated-path folder))) :heading)
        (put (format nil "~:d fingerprint~:p: ~:d case~:p of more than one file, ~:d file~:p alike to no other. Files of one case share ways of editing and tools, as one hand or one kit leaves them: a sign for you to judge, not proof.~%"
                     (loop for (members) in groups sum (length members)) (length cases) (length alone)))
        (loop for (members shared) in cases for k from 1
              do (put (format nil "~%* Case ~d: ~:d files~%" k (length members)) :heading)
                 (mapc #'file members)
                 (put (format nil "  Shared by all:~%") :list-label)
                 (dolist (trait shared) (put (format nil "    ~a~%" (escape-controls (trait-words trait)))))
                 (put (format nil "  Alike, two by two:~%") :list-label)
                 (let ((pairs (loop for (a . more) on members append (loop for b in more collect (cons a b)))))
                   (loop for (a . b) in pairs repeat *cases-pairs-shown*
                         do (put (format nil "    ~a and ~a: ~d%~%" (escape-controls (second a)) (escape-controls (second b))
                                         (round (* 100 (fingerprint-likeness (third a) (third b)))))
                                 :list-note))
                   (when (> (length pairs) *cases-pairs-shown*)
                     (put (format nil "    and ~:d pairs more~%" (- (length pairs) *cases-pairs-shown*)) :list-note))))
        (when alone
          (put (format nil "~%* Alike to no other: ~:d file~:p~%" (length alone)) :heading)
          (mapc #'file alone))))))

(defstruct (palimpsest-cases-job (:include job) (:constructor make-palimpsest-cases-job (buffer folder)) (:copier nil))
  "Setting beside one another the fingerprints under FOLDER, for BUFFER, a job;
GROUPS, what was found (GROUP-FINGERPRINTS); SHOWN, the list's text, faces
and buttons, made in the background (PIECES-TEXT)."
  folder groups shown)

(setf (documentation 'palimpsest-cases-job-p 'function) "True when OBJECT is a PALIMPSEST-CASES-JOB.")

(define-job-words palimpsest-cases-job
  :doing "Grouping the cases"
  :description (format nil "Setting beside one another the fingerprints under ~a" (palimpsest-cases-job-folder job))
  :verb "group the cases"
  :left-words "nothing shown")

(defmethod job-take-in ((job palimpsest-cases-job))
  "Show the groups of the fingerprints found, or say there were none."
  (let ((groups (palimpsest-cases-job-groups job)) (folder (palimpsest-cases-job-folder job)))
    (if (null groups)
        (message (substitute-command-keys "No fingerprints of palimpsest under ~a: \\<palimpsest-mode>\\[palimpsest-save] in a report writes one")
                 (abbreviated-path folder))
        (let ((buffer (find-or-make-buffer "*Palimpsest: cases*")))
          (set-major-mode buffer 'palimpsest-cases-mode)
          (destructuring-bind (text spans buttons) (palimpsest-cases-job-shown job)
            (set-made-text buffer text :spans spans :buttons buttons))
          (setf (buffer-point buffer) 0)
          (show-buffer buffer)
          (message "~:d file~:p: ~:d case~:p of more than one, ~:d alike to no other"
                   (loop for (members) in groups sum (length members)) (count-if #'rest groups :key #'first)
                   (count-if-not #'rest groups :key #'first))))))

(define-command palimpsest-group-cases ()
  "Group the PDFs examined so far as cases: the fingerprints palimpsest saved
with their reports (\\<palimpsest-mode>\\[palimpsest-save]), in a folder asked for and the folders under
it, set beside one another, and the files that share enough of their ways of
editing and their tools (the programs named, the JPEG tables a scan was
saved with, how later saves keep their cross-references) put in one group,
with what they share and how alike each two are. A sign that one hand or one
kit made them, for you to judge. Read in the background."
  (let ((folder (read-native-path "Group the cases whose fingerprints are under folder: ")))
    (when folder
      (if (not (eq (path-kind folder) :directory))
          (message "~a is not a folder" (abbreviated-path folder))
          (start-job-thread (make-palimpsest-cases-job (current-buffer) folder)
                            (lambda (job)
                              (let* ((prints (fingerprints-under folder :cancel (job-cancel job)))
                                     (groups (and prints (group-fingerprints prints))))
                                (setf (palimpsest-cases-job-groups job) groups
                                      (palimpsest-cases-job-shown job)
                                      (and groups (multiple-value-list (pieces-text (cases-pieces folder groups)))))))
                            :name "tuatara palimpsest cases")))))
