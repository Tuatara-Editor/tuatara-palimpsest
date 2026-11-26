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
    (:typed-into-scan 1 "Text typed into a scan's pixels"
     #.(concatenate 'string "Words or marks in the scanned picture itself without the grain every print of a scanner "
                            "has: drawn into it with a picture editor's text tool, or by a program."))
    (:picture-pasted 1 "A picture put in from elsewhere"
     "A signature, stamp or logo pasted in: a picture unlike the others, added later, or the same in two places.")
    (:page-made-picture 1 "A page made one picture"
     #.(concatenate 'string "A page whose text a program would have kept as text drawn as one picture: a PDF "
                            "opened in a picture editor and saved again, or printed as a picture."))
    (:painted-over 1 "Paper painted over in a scan"
     #.(concatenate 'string "A part of the scanned paper without the grain the scanner left on the rest: words hidden "
                            "under a box or a brush of one colour in a picture editor."))
    (:picture-editor 1 "Saved in a picture editor"
     #.(concatenate 'string "The picture names a picture editor, says it was changed after it was made, or was "
                            "saved as a JPEG twice: what any picture editor does, an honest crop or turn too."))
    (:generated 1 "Made by an image generator"
     #.(concatenate 'string "The file says an image generator made it or a part of it: in its content credentials "
                            "(C2PA), its digital source type or a generator's own text; or its pixels carry the "
                            "pattern a generator's decoder leaves. It tells how it was made, not that it is false."))
    (:chatbot-text 1 "Text written with a chatbot"
     #.(concatenate 'string "The page keeps words a chatbot writes around its answers, its citation marks, or the "
                            "places it leaves to fill: text pasted from one and not read through. It tells how the "
                            "text was made, not that the document is false."))
    (:figure-unchecked 1 "A figure changed alone"
     #.(concatenate 'string "A number whose check digit fails, or a net and VAT that do not make the total: one "
                            "figure changed without those that hang on it, or a slip where it was written."))
    (:text-dated-later 1 "A date in the text later than the file"
     #.(concatenate 'string "The document's own date (an invoice's, a letter's) later than every date the file gives "
                            "of itself: the date changed by hand, or the file's dates set back."))
    (:pages-changed 1 "Pages put in, taken out or replaced"
     "Pages added, removed, reordered, or unlike the rest: put in from another document.")
    (:retyped 2 "Text retyped to look like the rest"
     #.(concatenate 'string "Figures or words set again in another font or ink, or drawn a character at a time: "
                            "changed by someone who matched what showed."))
    (:fitted 2 "Text fitted into the gap it replaced"
     "Text set in another size, squeezed or spaced so that new words fill the old ones' place.")
    (:placed-by-hand 2 "Text placed by hand"
     "Text set a little above or below its line, or off the edge the lines keep: moved into place by hand.")
    (:hidden-text 2 "Text hidden"
     "Text clipped away, off the page, in a layer that is off, invisible, the page's colour or too small to read.")
    (:font-added 2 "A font put in later"
     #.(concatenate 'string "A font changed after the document was made, in several subsets, or embedded on some pages "
                            "only: text set at another time."))
    (:metadata-edited 2 "Dates or metadata written by hand"
     #.(concatenate 'string "Dates that disagree, run backwards or lie ahead, or a time zone no place uses: what the "
                            "file says of itself was changed."))
    (:metadata-removed 2 "What the file says of itself taken out"
     "The document information or the XMP metadata removed in a later save: traces cleared.")
    (:pasted-on-scan 2 "Words pasted into a scan, or printed apart"
     #.(concatenate 'string "Words of another ink, edge or paper than the words beside them: pasted onto the scan, or "
                            "printed in another pass, as honest pages have too."))
    (:structure-by-hand 3 "The file's structure written by hand"
     #.(concatenate 'string "Cross-references, lengths or objects that do not agree with the file: its octets edited "
                            "directly, or by a careless program."))
    (:moved-in-scan 2 "Words of a scan moved or turned"
     #.(concatenate 'string "Words of the scan's own ink set off their line or turned: cut out and put back, the "
                            "scan's look kept, or set so by the paper's curl or the print."))
    (:characters-composed 3 "Characters cut out of the scan and composed"
     #.(concatenate 'string "Characters copied from the scan itself, the grain of their ink and all, to make new "
                            "figures or words that match it."))
    (:piece-laid-in 3 "A piece of the scan saved apart and laid back"
     #.(concatenate 'string "A region of the scan whose JPEG traces are unlike the rest of it: cut out and saved as a "
                            "JPEG on its own, or laid in after the scan was saved as a JPEG."))
    (:paper-copied 3 "Paper copied over in a scan"
     #.(concatenate 'string "A patch of the scanned paper alike pixel for pixel with paper elsewhere on the page: "
                            "a clone stamp or a content-aware fill laid over what was there, words taken away."))
    (:scan-changed-after-ocr 3 "A scan changed after its text was recognised"
     #.(concatenate 'string "Words of a scan's invisible text layer over blank paper, or words of ink between words "
                            "the layer holds with none of their own: its pixels changed after text recognition read "
                            "them."))
    (:text-remapped 3 "Text made to copy otherwise than it shows"
     #.(concatenate 'string "A font whose map from codes to text gives other figures or letters than its glyphs draw: "
                            "the page shows one amount, and copying, searching and reading programs get another."))
    (:signature-bypassed 3 "Changes made past a signature"
     "Changes saved after a signature, outside what it covers, or objects hidden in what it covers and used later.")
    (:characters-composed-resaved 4 "Characters composed, then the scan saved again"
     #.(concatenate 'string "Copies within the scan that are no longer alike pixel for pixel: saved again as a JPEG, "
                            "which blurs the traces of the cutting.")))
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
highest level a strong sign shows and how many ways at each level; when
signs worth a look alone show a higher level, so, with that level. When
none of the signs is strong, no level is given as shown: the line says so
and gives the highest the signs worth a look would take, for an honest file
can show as much."
  (if (null ways)
      "No way of editing shown."
      (let* ((top (reduce #'max ways :key #'second))
             (strong (remove-if-not #'strong-way-p ways))
             (top-strong (and strong (reduce #'max strong :key #'second)))
             (counts (loop for (n name) in *craft-levels*
                           for some = (count n ways :key #'second)
                           when (plusp some) collect (format nil "~d way~:p ~a" some name))))
        (if top-strong
            (let ((level (assoc top-strong *craft-levels*)))
              (format nil "~@(~a~), level ~d of 4: ~a. ~{~a~^, ~}.~@[ ~a~]"
                      (second level) top-strong (third level) counts
                      (and (> top top-strong)
                           (format nil "Only signs worth a look show level ~d; the strong signs show level ~d."
                                   top top-strong))))
            (format nil "No strong sign, so no level of craft shown: the signs worth a look would take up to level ~
                         ~d of 4, ~a. ~{~a~^, ~}. An honest file can show as much."
                    top (second (assoc top *craft-levels*)) counts)))))

;;; The fingerprint

(defun fnv-hex (octets)
  "A short name for OCTETS, the same for the same octets: their 64-bit FNV-1a hash, in 16 hex digits."
  (format nil "~(~16,'0x~)" (octets-hash (coerce octets '(simple-array (unsigned-byte 8) (*))))))

(defun analysis-traits (analysis)
  "ANALYSIS's fingerprint (FINGERPRINT), worked out once and kept: while it
is examined, before what was read of its pages is let go. The editor's own
ANALYSIS, which has no PDF, has the fingerprint examining sent back."
  (if (or (analysis-fingerprint analysis) (null (analysis-pdf analysis)))
      (analysis-fingerprint analysis)
      (setf (analysis-fingerprint analysis) (fingerprint analysis))))

(defun fingerprint (analysis)
  "The fingerprint of ANALYSIS's PDF: what is told of who edited it and with
what, each (KIND VALUE). KIND is :method (a way of editing, METHODS-FOUND),
:editor (a program a later save names, not the original) or :made-by (a
program the original names). KIND is also :index (how a later save keeps
its cross-references) or :jpeg (the tables a JPEG picture was saved with:
the encoder and its quality, often the forger's own when a scan was
changed). For a picture file, the programs it names and its own JPEG
tables (PICTURE-TRAITS)."
  (let* ((pdf (analysis-pdf analysis)) (last (last-revision pdf)) (traits '()))
    (flet ((add (kind value) (when value (pushnew (list kind value) traits :test #'equal))))
      (dolist (way (methods-found (analysis-findings analysis))) (add :method (string-downcase (first way))))
      (loop with names = (program-names pdf)
            for r across (pdf-revisions pdf)
            do (loop while (and names (eq (second (first names)) r))
                     do (destructuring-bind (key revision program) (pop names)
                          (declare (ignore revision))
                          ;; the original's programs made the document; a later save's own edited it
                          (when (char= #\/ (char key 0))
                            (if (or (zerop (revision-index r))
                                    (equal program (info-text pdf (aref (pdf-revisions pdf) 0) key)))
                                (add :made-by program)
                                (add :editor program)))))
               (when (plusp (revision-index r))
                 (add :index (case (revision-xref-kind r)
                               (:table "a table in a later save")
                               (:stream "a stream in a later save")
                               (:hybrid "a hybrid in a later save")))))
      (loop for (m) in (pictures-of pdf last)
            do (let* ((layout (picture-head-layout pdf last m))
                      (words (and layout (jpeg-tables-words (jpeg-layout-tables layout)))))
                 (when words (add :jpeg words))))
      (let ((picture (analysis-picture analysis)))
        (when picture
          (loop for (kind value) in (picture-traits picture) do (add kind value))))
      (nreverse traits))))

(defparameter *trait-weights* '((:method . 1) (:editor . 3) (:made-by . 1/4) (:index . 1/2) (:jpeg . 2))
  "How much each kind of trait of a fingerprint counts when two are set beside
each other. The programs a forger saved with and the JPEG tables of the
pictures tell more of who edited a file than a way of editing many use. The
program that made the original tells of the document's source, not of its
editor, and counts little.")

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
case. They share a way of editing (two files of one source, edited by no
one, are no case) and one thing more, another way or a tool (a program a
later save names, the JPEG tables of a picture), a way alone being one many
use. They are also *CASES-ALIKE* alike (FINGERPRINT-LIKENESS)."
  (let ((shared (intersection a b :test #'equal)))
    (and (find :method shared :key #'first)
         (or (> (count :method shared :key #'first) 1)
             (find-if (lambda (trait) (member (first trait) '(:editor :jpeg))) shared))
         (>= (fingerprint-likeness a b) *cases-alike*))))

(defun group-fingerprints (prints)
  "PRINTS, each (PATH NAME TRAITS), in groups: two that are one case
\(CASES-ALIKE-P) in one group, and every one alike to one in it too.
Each group (MEMBERS SHARED), SHARED the traits all its members have; the
largest groups first, a file alike to none a group of its own last. As one
case shares a way of editing, only the prints of each way are set beside
one another, and each pair once, under the first way of the one before it
that the other has."
  (let* ((items (coerce prints 'simple-vector)) (sets (make-disjoint-sets (length items)))
         (ways (make-hash-table :test 'equal)))
    (dotimes (k (length items))
      (dolist (trait (third (svref items k)))
        (when (eq (first trait) :method) (push k (gethash trait ways)))))
    (loop for way being the hash-keys of ways using (hash-value members)
          do (loop for (j . rest) on members
                   do (job-check-stop)
                      (dolist (i rest)
                        ;; MEMBERS run from the last print down: I comes before J
                        (let ((a (third (svref items i))) (b (third (svref items j))))
                          (when (and (equal way (find-if (lambda (trait) (and (eq (first trait) :method)
                                                                              (member trait b :test #'equal)))
                                                         a))
                                     (/= (set-root sets i) (set-root sets j))
                                     (cases-alike-p a b))
                            (join-sets sets i j))))))
    (stable-sort (loop for members in (set-groups sets items)
                       collect (list members
                                     (reduce (lambda (a b) (intersection a b :test #'equal)) (mapcar #'third members))))
                 #'> :key (lambda (g) (length (first g))))))

(defparameter *cases-prints-most* 2000
  "The most fingerprints read to be grouped as cases (FINGERPRINTS-UNDER);
those found after are counted, not read.")

(defun fingerprints-under (directory &key (cancel (list nil)))
  "The fingerprints in DIRECTORY and the directories under it, each (PATH NAME
TRAITS), as many as *CASES-PRINTS-MOST* of them; as a second value, how many
more were found and not read."
  (let ((prints '()) (read 0) (left 0))
    (walk-directory (slash-ended directory)
                    (lambda (dir path entry)
                      (declare (ignore path))
                      (when entry
                        (destructuring-bind (name kind &rest rest) entry
                          (declare (ignore rest))
                          (cond ((eq kind :directory) t)
                                ((and (eq kind :regular) (> (length name) 16)
                                      (uiop:string-suffix-p name "-fingerprint.txt"))
                                 (let ((path (path-join dir name)))
                                   ;; a file of its own, never a link leading out of the directory
                                   (cond ((>= read *cases-prints-most*) (incf left))
                                         ((eq (path-kind path :follow nil) :regular)
                                          (incf read)
                                          (multiple-value-bind (pdf traits) (read-fingerprint path)
                                            (when pdf (push (list path pdf traits) prints))))))
                                 nil)))))
                    :cancel cancel)
    (values (sort prints #'string< :key #'first) left)))

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

(defun cases-pieces (directory groups &optional (left 0))
  "The pieces of the list of GROUPS (GROUP-FINGERPRINTS) of the fingerprints
found under DIRECTORY: the groups of two or more, then the files alike to
none. LEFT, how many more fingerprints were found and not read, is said."
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
        (put (format nil "Palimpsest: cases under ~a~%" (escape-controls (abbreviated-path directory))) :heading)
        (put (format nil "~:d fingerprint~:p: ~:d case~:p of more than one file, ~:d file~:p alike to no other. Files ~
                          of one case share ways of editing and tools, as one hand or one kit leaves them: a sign for ~
                          you to judge, not proof.~%"
                     (loop for (members) in groups sum (length members)) (length cases) (length alone)))
        (when (plusp left)
          (put (format nil "Only the first ~:d fingerprints were read and grouped (*cases-prints-most*); ~:d more ~
                            found were not.~%"
                       *cases-prints-most* left)
               :list-note))
        (loop for (members shared) in cases for k from 1
              do (put (format nil "~%* Case ~d: ~:d files~%" k (length members)) :heading)
                 (mapc #'file members)
                 (put (format nil "  Shared by all:~%") :list-label)
                 (dolist (trait shared) (put (format nil "    ~a~%" (escape-controls (trait-words trait)))))
                 (put (format nil "  Alike, two by two:~%") :list-label)
                 (let ((pairs (/ (* (length members) (1- (length members))) 2)) (shown 0))
                   (loop for (a . more) on members
                         while (< shown *cases-pairs-shown*)
                         do (loop for b in more
                                  while (< shown *cases-pairs-shown*)
                                  do (incf shown)
                                     (put (format nil "    ~a and ~a: ~d%~%"
                                                  (escape-controls (second a))
                                                  (escape-controls (second b))
                                                  (round (* 100 (fingerprint-likeness (third a) (third b)))))
                                          :list-note)))
                   (when (> pairs shown)
                     (put (format nil "    and ~:d pairs more~%" (- pairs shown)) :list-note))))
        (when alone
          (put (format nil "~%* Alike to no other: ~:d file~:p~%" (length alone)) :heading)
          (mapc #'file alone))))))

(defstruct (palimpsest-cases-job (:include job) (:constructor make-palimpsest-cases-job (buffer directory))
                                 (:copier nil))
  "Setting beside one another the fingerprints under DIRECTORY, for BUFFER, a job;
GROUPS, what was found (GROUP-FINGERPRINTS); SHOWN, the list's text, faces
and buttons, made in the background (PIECES-TEXT)."
  directory groups shown)

(setf (documentation 'palimpsest-cases-job-p 'function) "True when OBJECT is a PALIMPSEST-CASES-JOB.")

(define-job-words palimpsest-cases-job
  :doing "Grouping the cases"
  :description (format nil "Setting beside one another the fingerprints under ~a" (palimpsest-cases-job-directory job))
  :verb "group the cases"
  :left-words "nothing shown")

(defun show-made-list (name mode shown)
  "Show the list SHOWN, its text, faces and buttons made in the background
\(PIECES-TEXT), in the buffer NAME in MODE, from its top."
  (let ((buffer (buffer-in-mode name mode)))
    (destructuring-bind (text spans buttons) shown
      (set-made-text buffer text :spans spans :buttons buttons))
    (setf (buffer-point buffer) 0)
    (show-buffer buffer)))

(defun start-directory-job (prompt make-job work name)
  "Ask for a directory by PROMPT and work on it in the background: the job
MAKE-JOB, called with the directory, makes, its work WORK, called with the
job and the directory, in a thread named NAME. Nothing for no answer; a
path that is no directory is said."
  (let ((directory (read-directory-path prompt :note nil)))
    (when directory
      (if (not (eq (path-kind directory) :directory))
          (message "~a is not a directory" (abbreviated-path directory))
          (start-job-thread (funcall make-job directory) (lambda (job) (funcall work job directory)) :name name)))))

(defmethod job-take-in ((job palimpsest-cases-job))
  "Show the groups of the fingerprints found, or say there were none."
  (let ((groups (palimpsest-cases-job-groups job)) (directory (palimpsest-cases-job-directory job)))
    (if (null groups)
        (message (substitute-command-keys #.(concatenate 'string "No fingerprints of palimpsest under ~a: "
                                                                 "\\<palimpsest-mode>\\[palimpsest-save] in a report "
                                                                 "writes one"))
                 (abbreviated-path directory))
        (progn
          (show-made-list "*Palimpsest: cases*" 'palimpsest-cases-mode (palimpsest-cases-job-shown job))
          (message "~:d file~:p: ~:d case~:p of more than one, ~:d alike to no other"
                   (loop for (members) in groups sum (length members)) (count-if #'rest groups :key #'first)
                   (count-if-not #'rest groups :key #'first))))))

(defun cases-work (job directory)
  "What a job grouping the fingerprints under DIRECTORY as cases does
\(PALIMPSEST-CASES-JOB): the fingerprints read and grouped, and the list of
the groups made."
  (multiple-value-bind (prints left) (fingerprints-under directory :cancel (job-cancel job))
    (let ((groups (and prints (group-fingerprints prints))))
      (setf (palimpsest-cases-job-groups job) groups
            (palimpsest-cases-job-shown job)
            (and groups (multiple-value-list (pieces-text (cases-pieces directory groups left))))))))

(define-command palimpsest-group-cases ()
  "Group the PDFs examined so far as cases. The fingerprints palimpsest saved
with their reports (\\<palimpsest-mode>\\[palimpsest-save]), in a directory
asked for and the directories under it, are set beside one another. The files
that share enough of their ways of editing and their tools (the programs
named, the JPEG tables a scan was saved with, how later saves keep their
cross-references) are put in one group, with what they share and how alike
each two are. A sign that one hand or one kit made them, for you to judge.
Read in the background."
  (let ((buffer (current-buffer)))
    (start-directory-job "Group the cases whose fingerprints are under directory: "
                         (lambda (directory) (make-palimpsest-cases-job buffer directory))
                         #'cases-work "tuatara palimpsest cases")))
