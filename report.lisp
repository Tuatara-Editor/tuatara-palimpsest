;;;; report.lisp --- The report on a PDF: its summary, findings, methods, fingerprint and revisions, each revision written out with its log.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x palimpsest examines a PDF in the background and shows what it found
;;;; in a buffer of its own, read only: a summary, the findings strongest
;;;; first, then each revision from the original to the latest with what it
;;;; added, changed and freed and how its pages' text and drawing changed.
;;;; Each revision is written as a PDF of its own (the file's octets up to
;;;; its %%EOF line) beside a log saying how it was found and every sign in
;;;; it, with the offsets and octets that show it, so that the same can be
;;;; done again by hand with a hex editor. They are written to a folder of
;;;; the package's own in the cache directory, for you alone; the PDF itself
;;;; is only read.

(in-package #:palimpsest)

(define-package-setting *palimpsest-largest-written* (* 2 1024 1024 1024)
  "The most octets the revisions of one PDF, each written out as a PDF of its
own, may take together: 2 GB as standard. Revisions past it are listed as not
written, as they are when the drive would keep less than 1 GB free.")

(define-package-setting *palimpsest-largest-shown* (* 1024 1024)
  "The most octets of an object's stream shown by \\<palimpsest-mode>\\[palimpsest-show-object]: 1 MB as standard.")

(defparameter *palimpsest-version* "1.0"
  "The version of the palimpsest package, as its logs name it.")

(define-major-mode palimpsest-mode special-mode
  "The report on a PDF examined for signs of editing (\\[palimpsest]), read
only: a summary, the findings (strong signs of editing, then those worth a
look, then those for the record), the ways of editing they tell of and the
craft those take, the file's fingerprint, and each revision from the
original to the latest. \\<palimpsest-mode>\\[push-button] on a revision's PDF opens it in Tuatara's PDF view, on its
log opens the log, on an object's number shows the object; \\[forward-button] and
\\[backward-button] go from one of these to the next. \\[palimpsest-show-object] shows an object's dictionary and
stream (as text, or in hex in a buffer of its own when it is not text;
in hex too with \\[universal-argument]); \\[palimpsest-save] saves the revisions, their logs and the report to a
folder of your choosing; \\[palimpsest-write-revision] writes the revision whose part point is in
as a PDF to show, its log beside it; \\[palimpsest-write-report] writes the report alone as
text; \\[quit-window] leaves it, \\[kill-buffer] closes it. Closing the report
closes the revisions and logs opened from it and deletes its own copies of
the revisions, asking first when they were not saved."
  :lighter "Palimpsest" :interactive nil)

(defvar *palimpsest-reports* '()
  "The buffers showing a report on a PDF.")

(defstruct (analysis (:copier nil))
  "What examining a PDF found: PATH and NAME of the file, SIZE, SHA-256; PDF,
its structure (PDF); ACCOUNTS, each revision compared with the one before;
FINDINGS; FOLDER, where its revisions were written; FILES, each revision's
\(PDF-PATH LOG-PATH SHA-256), or (NIL NIL WHY) when not written; SHOWN, the
report's text, faces and buttons (as PIECES-TEXT gives them), made in the
background; SAVED, the folder its revisions, logs and report were saved to,
or NIL while they are not; FINGERPRINT, its fingerprint once worked out
\(ANALYSIS-TRAITS)."
  path name size sha256 pdf (accounts '()) (findings '()) folder (files '()) shown saved fingerprint)

(setf (documentation 'analysis-p 'function) "True when OBJECT is an ANALYSIS.")

;;; The work in the background

(defstruct (palimpsest-job (:include job) (:constructor make-palimpsest-job (buffer path folder)) (:copier nil))
  "The PDF at PATH being examined for BUFFER, its revisions written to
FOLDER, a job: PHASE, what it does now, PROGRESS, how far it has got from 0
to 1; ANALYSIS, what it found."
  path folder (phase "Reading the PDF") (progress 0) analysis)

(setf (documentation 'palimpsest-job-p 'function) "True when OBJECT is a PALIMPSEST-JOB.")

(define-job-words palimpsest-job
  :doing (palimpsest-job-phase job)
  :description (format nil "Examining ~a for signs of editing" (native-file-name (palimpsest-job-path job)))
  :verb "examine the PDF"
  :left-words "nothing found")

(defmethod job-percent ((job palimpsest-job))
  "How far examining the PDF has got."
  (running-percent (round (* 1000 (palimpsest-job-progress job))) 1000))

(defun stage (job phase from to)
  "A function of a fraction, moving JOB's progress from FROM to TO, its phase PHASE."
  (setf (palimpsest-job-phase job) phase (palimpsest-job-progress job) from)
  (lambda (fraction) (setf (palimpsest-job-progress job) (+ from (* (- to from) (min 1 (max 0 fraction)))))))

(defun examine (job)
  "What a job examining a PDF does: the file read whole and summed, its
structure read, each revision compared with the one before, every sign
found, and each revision written out with its log."
  (let* ((path (palimpsest-job-path job))
         (octets (file-octets path :limit *palimpsest-largest-file*))
         (analysis (make-analysis :path path :name (native-file-name path) :size (length octets) :folder (palimpsest-job-folder job))))
    (funcall (stage job "Summing the PDF" 0.02 0.05) 0)
    (setf (analysis-sha256 analysis) (file-sha256 path :cancel (job-cancel job)))
    (job-check-stop)
    (let* ((pdf (read-pdf octets :path path :name (analysis-name analysis) :progress (stage job "Reading its saves" 0.05 0.35))))
      (setf (analysis-pdf analysis) pdf)
      (let ((compare (stage job "Comparing its saves" 0.35 0.6)) (revisions (pdf-revisions pdf)))
        (setf (analysis-accounts analysis)
              (loop for revision across revisions for k from 1
                    collect (prog1 (compare-revision pdf revision) (funcall compare (/ k (length revisions)))))))
      (setf (analysis-findings analysis) (all-findings pdf (analysis-accounts analysis) (stage job "Looking for signs of editing" 0.6 0.85)))
      (write-revisions job analysis)
      (setf (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis)))
            (palimpsest-job-analysis job) analysis)
      (forget-workings pdf))))

(defun write-revisions (job analysis)
  "Write each revision of ANALYSIS's PDF to its folder as revision-N.pdf,
with revision-N.log beside it, as far as *PALIMPSEST-LARGEST-WRITTEN* and the
free space allow."
  (let* ((pdf (analysis-pdf analysis)) (folder (analysis-folder analysis)) (written 0)
         (progress (stage job "Writing its revisions" 0.85 0.99))
         (revisions (pdf-revisions pdf)))
    (setf (analysis-files analysis)
          (loop for revision across revisions for k from 1
                collect (progn
                          (job-check-stop)
                          (funcall progress (/ k (length revisions)))
                          (let ((end (revision-end revision)) (free (ignore-errors (free-bytes folder))))
                            (cond ((> (+ written end) *palimpsest-largest-written*)
                                   (list nil nil "not written: the revisions together would pass *palimpsest-largest-written*"))
                                  ((and free (< free (+ end (expt 2 30))))
                                   (list nil nil "not written: the drive of the cache directory would keep less than 1 GB free"))
                                  (t (incf written end)
                                     (let ((file (path-join folder (format nil "revision-~d.pdf" k)))
                                           (log (path-join folder (format nil "revision-~d.log" k))))
                                       (write-new-file file (pdf-octets pdf) :end end)
                                       (let ((sum (file-sha256 file :cancel (job-cancel job))))
                                         (write-new-file log (revision-log analysis revision sum))
                                         (list file log sum)))))))))))

;;; The log of a revision

(defun strength-words (strength)
  "STRENGTH, a finding's, in words."
  (ecase strength (:strong "Strong sign of editing") (:look "Worth a look") (:record "For the record")))

(defun show-log (buffer)
  "Show BUFFER, a revision's log, read as the report is: moving, selecting and
copying, nothing that edits, \\`q' closing it."
  (view-made-text buffer)
  (show-buffer buffer))

(defun kept-text (text)
  "TEXT as it is written to a log or report kept: its lines as they are, each
control character within them shown as the editor shows one (^[ for an
escape), so that what a file holds cannot act on a terminal it is read in."
  (with-output-to-string (out)
    (loop for start = 0 then (1+ end)
          for end = (position #\Newline text :start start)
          do (write-string (escape-controls (subseq text start end)) out)
             (when end (terpri out))
          while end)))

(defun report-text (analysis)
  "The report on ANALYSIS as text to keep (KEPT-TEXT)."
  (kept-text (first (analysis-shown analysis))))

(defun revision-log (analysis revision sum)
  "The log of REVISION of ANALYSIS's PDF, its own file's SHA-256 being SUM:
how it was found, every sign in it with its evidence and the steps to see it
by hand, what it adds, changes and frees, and how it differs from the
revision before, as text to keep (KEPT-TEXT). The same file gives the same
log but for its date line."
  (let* ((pdf (analysis-pdf analysis)) (octets (pdf-octets pdf)) (k (1+ (revision-index revision)))
         (before (and (> k 1) (aref (pdf-revisions pdf) (- k 2))))
         (account (nth (revision-index revision) (analysis-accounts analysis))))
    (kept-text
     (with-output-to-string (s)
       (format s "Palimpsest log of ~a of ~a~%~a~%~%" (revision-name revision) (analysis-name analysis) (make-string 60 :initial-element #\=))
       (format s "Source file:  ~a~%Size:         ~:d octets (~a)~%SHA-256:      ~a~%" (analysis-name analysis) (analysis-size analysis)
               (hex-offset (analysis-size analysis)) (or (analysis-sha256 analysis) "(could not be summed)"))
       (format s "Written:      ~a~%" (format-time "%F %T %z" (get-universal-time)))
       (format s "Written by:   palimpsest ~a, one of the Tuatara project's packages~%~%" *palimpsest-version*)
       ;; how it was found
       (format s "How this revision was found~%---------------------------~%")
       (format s "Its %%EOF is at ~a; its line ends at ~a.~%" (offset-words (revision-eof revision)) (offset-words (revision-end revision)))
       (if (revision-startxref-at revision)
           (format s "Its startxref keyword is at ~a and names ~:[nothing that can be read~;~:*offset ~a~].~%"
                   (offset-words (revision-startxref-at revision)) (and (revision-startxref revision) (offset-words (revision-startxref revision))))
           (format s "It has no startxref keyword.~%"))
       (format s "Its cross-reference section ~:[could not be found: its objects were found by scanning its octets~;~:*is ~a at ~a~].~%"
               (case (revision-xref-kind revision)
                 (:table "a table (the keyword xref)") (:stream "a stream") (:hybrid "a table with a stream beside it (/XRefStm)") (t nil))
               (and (revision-xref-at revision) (offset-words (revision-xref-at revision))))
       (when (revision-trailer-at revision)
         (format s "Its trailer is at ~a:~%" (offset-words (revision-trailer-at revision)))
         (dolist (line (octets-evidence "  the trailer" octets (revision-trailer-at revision)
                                        (min (revision-eof revision) (+ (revision-trailer-at revision) 240))))
           (format s "  ~a~%" line)))
       (let ((prev (revision-prev revision)))
         (if prev
             (let ((named (find prev (pdf-revisions pdf) :key #'revision-xref-at)))
               (format s "Its trailer's /Prev is ~d (~a), ~:[which is the cross-reference section of no revision~;~:*the cross-reference section of ~a~].~%"
                       prev (hex-offset prev) (and named (revision-name named))))
             (format s "Its trailer names no /Prev~:[~; (it is the original)~].~%" (= k 1))))
       (dolist (problem (revision-problems revision)) (format s "Not as it should be: ~a.~%" problem))
       (format s "~%This revision is the source file's octets from 0x00000000 to ~a, the end of its %%EOF line included: ~:d octets.~%"
               (hex-offset (1- (revision-end revision))) (revision-end revision))
       (format s "revision-~d.pdf SHA-256: ~a~%" k (or sum "(could not be summed)"))
       (format s "To check: copy the source file's first ~:d octets into a new file; its SHA-256 is the one above.~%~%" (revision-end revision))
       ;; the signs
       (let ((signs (remove-if-not (lambda (f) (eql (finding-revision f) (revision-index revision))) (analysis-findings analysis))))
         (format s "Signs found in this revision: ~d~%------------------------------~%" (length signs))
         (loop for f in signs for n from 1
               do (format s "~%[~d] ~a: ~a~%" n (strength-words (finding-strength f)) (finding-words f))
                  (when (finding-page f) (format s "    Page: ~d~%" (finding-page f)))
                  (when (finding-objects f) (format s "    Objects: ~{~d~^, ~}~%" (subseq (finding-objects f) 0 (min 50 (length (finding-objects f))))))
                  (when (finding-rule f) (format s "    Rule: ~a~%" (finding-rule f)))
                  (when (finding-details f)
                    (format s "    Details:~%")
                    (dolist (d (finding-details f)) (format s "      ~a~%" d)))
                  (when (finding-evidence f)
                    (format s "    Evidence:~%")
                    (dolist (e (finding-evidence f)) (format s "      ~a~%" e)))
                  (when (finding-steps f)
                    (format s "    Steps to see it by hand:~%")
                    (loop for step in (finding-steps f) for m from 1 do (format s "      ~d. ~a~%" m step)))))
       ;; the objects
       (format s "~%Objects~%-------~%")
       (flet ((place (r n)
                (let ((o (revision-object pdf r n)))
                  (cond ((null o) "not in the file")
                        ((pdf-indirect-packed-in o) (format nil "in object stream ~d at ~a of its decoded octets" (pdf-indirect-packed-in o) (offset-words (pdf-indirect-at o))))
                        (t (format nil "at ~a" (offset-words (pdf-indirect-at o))))))))
         (format s "Added (~d):~%" (length (revision-added revision)))
         (dolist (n (revision-added revision))
           (format s "  object ~d ~a~@[, ~a~]~%" n (place revision n) (role-words pdf revision n)))
         (format s "Changed (~d):~%" (length (revision-changed revision)))
         (dolist (n (revision-changed revision))
           (format s "  object ~d now ~a, before ~a~@[, ~a~]~%" n (place revision n) (place before n) (role-words pdf revision n)))
         (when (revision-same revision)
           (format s "Written again unchanged (~d):~%" (length (revision-same revision)))
           (dolist (n (revision-same revision)) (format s "  object ~d now ~a, before ~a~%" n (place revision n) (place before n))))
         (format s "Freed (~d):~%" (length (revision-freed revision)))
         (dolist (n (revision-freed revision))
           (format s "  object ~d, before ~a~@[, ~a~]~%" n (place before n) (role-words pdf before n))))
       ;; against the revision before
       (when before
         (format s "~%Changes against revision ~d~%--------------------------~%" (1- k))
         (if (null (account-changes account))
             (format s "No page draws anything different.~%")
             (dolist (change (account-changes account))
               (let ((page (page-change-page change)))
                 (format s "~%Page ~d~:[ (new in this revision)~;~]:~%" (1+ (page-index page)) (page-change-old change))
                 (dolist (line (page-change-text change))
                   (format s "  text ~:[-~;+~] ~a~%" (eq (first line) :added) (second line)))
                 (dolist (line (page-change-annotations change))
                   (format s "  laid over ~:[-~;+~] ~a~%" (eq (first line) :added) (second line)))
                 (dolist (m (page-change-added change))
                   (format s "  drawn now: ~a~%" (mark-words page m))
                   (dolist (e (mark-evidence pdf revision m)) (format s "    ~a~%" e)))
                 (dolist (m (page-change-removed change))
                   (format s "  drawn before: ~a~%" (mark-words (or (page-change-old change) page) m))
                   (dolist (e (mark-evidence pdf before m)) (format s "    ~a~%" e))))))
         (dolist (page (account-gone account))
           (format s "~%Page ~d of revision ~d is gone.~%" (1+ (page-index page)) (1- k))))))))

;;; The report

(defun summary-verdict (findings)
  "The verdict of FINDINGS, in words."
  (cond ((find :strong findings :key #'finding-strength) "Strong signs that the file was edited after it was made.")
        ((find :look findings :key #'finding-strength) "Signs worth a look, none of them strong.")
        (t "No sign of editing found.")))

(defun object-pieces (analysis revision numbers &optional (most 60))
  "Pieces naming the objects NUMBERS of REVISION, each a button showing it, at most MOST of them."
  (collecting-pieces (put)
    (loop for n in numbers for k from 0
          while (< k most)
          do (when (plusp k) (put ", "))
             (put (princ-to-string n) :link (let ((n n)) (lambda () (show-object analysis revision n)))))
    (when (> (length numbers) most) (put (format nil " and ~:d more" (- (length numbers) most))))))

(defun detail-pieces (detail analysis revision put)
  "Put DETAIL, a line of a finding's details, by PUT: each place of text in
it (4 @539, PLACE-LABEL) a link showing that object with the cursor where
its operators begin, and each picture named (picture 7) one showing it, as
REVISION has it."
  (let ((at 0))
    (cl-ppcre:do-scans (s e starts ends "(\\d+) @(\\d+)|picture (\\d+)" detail)
      (funcall put (subseq detail at s) :list-note)
      (if (aref starts 0)
          (let ((number (parse-integer detail :start (aref starts 0) :end (aref ends 0)))
                (from (parse-integer detail :start (aref starts 1) :end (aref ends 1))))
            (funcall put (subseq detail s e) :link (lambda () (show-object analysis revision number :at from))))
          (let ((number (parse-integer detail :start (aref starts 2) :end (aref ends 2))))
            (funcall put (subseq detail s e) :link (lambda () (show-object analysis revision number)))))
      (setf at e))
    (funcall put (subseq detail at) :list-note)))

(defun report-pieces (analysis)
  "The pieces of the report on ANALYSIS: its summary, findings, the ways of
editing they tell of (METHODS-FOUND), its fingerprint (FINGERPRINT) and
its revisions."
  (let* ((pdf (analysis-pdf analysis)) (last (last-revision pdf)) (findings (analysis-findings analysis)))
    (collecting-pieces (put)
      (flet ((line (label value &optional face)
               (when value (put (format nil "  ~12a" label) :list-label) (put (format nil "~a~%" value) face)))
             (heading (text) (put (format nil "~%~a~%" text) :heading)))
        (put (format nil "Palimpsest: ~a~%" (analysis-name analysis)) :heading)
        (put (format nil "Read from the file's octets alone: nothing in it was run, and the file was not changed.~%"))
        (put (format nil "~a~%" (substitute-command-keys "\\<palimpsest-mode>\\[push-button] on a link opens it; \\[palimpsest-show-object] shows an object.")) :list-dim)
        (heading "* Summary")
        (line "File" (abbreviated-path (analysis-path analysis)))
        (line "Size" (format nil "~:d octets (~a), ~a" (analysis-size analysis) (hex-offset (analysis-size analysis)) (octets-words (analysis-size analysis))))
        (line "SHA-256" (or (analysis-sha256 analysis) "could not be summed"))
        (line "PDF version" (or (pdf-version pdf) "not given"))
        (line "Revisions" (format nil "~d: the original~[~; and 1 later save~:;~:* and ~d later saves~]" (length (pdf-revisions pdf)) (1- (length (pdf-revisions pdf)))))
        (line "Pages" (princ-to-string (length (pages-of pdf last))))
        (line "Verdict" (summary-verdict findings) (if (find :strong findings :key #'finding-strength) :warning :strong))
        (line "Findings" (format nil "~d strong sign~:p of editing, ~d worth a look, ~d for the record"
                                 (count :strong findings :key #'finding-strength) (count :look findings :key #'finding-strength)
                                 (count :record findings :key #'finding-strength)))
        (flet ((date (key) (let ((d (info-text pdf last key))) (and d (pdf-date-words d)))))
          (line "Made" (date "/CreationDate"))
          (line "Changed" (date "/ModDate")))
        (line "Saved to" (and (analysis-saved analysis) (abbreviated-path (analysis-saved analysis))))
        (line "Creator" (info-text pdf last "/Creator"))
        (line "Producer" (info-text pdf last "/Producer"))
        (dolist (note (reverse (pdf-notes pdf))) (line "Not read" note :warning))
        ;; findings
        (heading "* Findings")
        (when (null findings) (put (format nil "  None.~%")))
        (dolist (strength *strengths*)
          (let ((some (remove strength findings :key #'finding-strength :test-not #'eq)))
            (when some
              (heading (format nil "** ~a (~d)" (ecase strength (:strong "Strong signs of editing") (:look "Worth a look") (:record "For the record")) (length some)))
              (dolist (f some)
                (put "- ")
                (put (format nil "~a~%" (finding-words f)) (if (eq strength :strong) :warning nil))
                (let ((revision (aref (pdf-revisions pdf) (finding-revision f))))
                  (put (format nil "    ~a~@[, page ~d~]" (revision-name revision) (finding-page f)) :list-note)
                  (when (finding-objects f)
                    (put ", objects " :list-note)
                    (mapc #'put (object-pieces analysis revision (finding-objects f) 20)))
                  (put (string #\Newline)))
                (let ((revision (aref (pdf-revisions pdf) (finding-revision f))))
                  (dolist (d (finding-details f))
                    (put "    " :list-note)
                    (detail-pieces d analysis revision #'put)
                    (put (string #\Newline))))))))
        ;; the ways of editing the findings tell of, and the craft they take
        (let ((ways (methods-found findings)))
          (heading "* Methods")
          (put (format nil "  ~a~%" (craft-words ways)) (and ways (if (>= (reduce #'max ways :key #'second) 3) :warning :strong)))
          (when ways
            (put (format nil "  Ways of editing the findings plausibly tell of, the most practised first; a careful hand leaves fewer signs, so what is not found is not ruled out.~%") :list-dim))
          (loop for way in ways
                for (nil level name why some) = way
                do (put (format nil "- Level ~d, ~a~:[ (signs worth a look only)~;~]~%" level name (strong-way-p way))
                        (and (>= level 3) (strong-way-p way) :warning))
                   (put (format nil "    ~a~%" why) :list-note)
                   (dolist (f some)
                     (put (format nil "    ~a~%" (shorten-string (finding-words f) 110 :one-line t)) :list-note))))
        (let ((traits (analysis-traits analysis)))
          (heading "* Fingerprint")
          (put (format nil "  ~a~%" (substitute-command-keys "What tells of who edited it and with what; \\<palimpsest-mode>\\[palimpsest-save] writes it beside the report, and \\[palimpsest-group-cases] groups the files whose fingerprints share it.")) :list-dim)
          (if traits
              (dolist (trait traits) (put (format nil "  ~a~%" (trait-words trait))))
              (put (format nil "  Nothing.~%"))))
        ;; revisions
        (heading "* Revisions")
        (loop for revision across (pdf-revisions pdf)
              for account in (analysis-accounts analysis)
              for (file log why) in (analysis-files analysis)
              do (heading (format nil "** ~@(~a~): ~a to ~a, ~:d octets" (revision-name revision)
                                  (hex-offset (revision-start revision)) (hex-offset (1- (revision-end revision)))
                                  (- (revision-end revision) (revision-start revision))))
                 (put (format nil "  ~12a" "Written as") :list-label)
                 (if file
                     (progn (put (native-file-name file) :link (let ((file file)) (lambda () (visit-path file))))
                            (put "  its log ")
                            (put (native-file-name log) :link (let ((log log)) (lambda () (visit-path log :show #'show-log))))
                            (put (string #\Newline)))
                     (put (format nil "~a~%" why) :warning))
                 (line "Index" (format nil "~a~@[ at ~a~]~@[, startxref at ~a~]"
                                       (case (revision-xref-kind revision) (:table "a cross-reference table") (:stream "a cross-reference stream")
                                         (:hybrid "a table and a stream (hybrid)") (t "none: objects found by scanning"))
                                       (and (revision-xref-at revision) (hex-offset (revision-xref-at revision)))
                                       (and (revision-startxref-at revision) (hex-offset (revision-startxref-at revision)))))
                 (multiple-value-bind (info number) (info-of pdf revision)
                   (declare (ignore info))
                   (line "Trailer" (format nil "~{~a~^, ~}"
                                           (remove nil (list (let ((root (pdf-ref-p (pdf-get (revision-trailer revision) "/Root"))))
                                                               (if root (format nil "/Root object ~d" root) "no /Root"))
                                                             (and number (format nil "/Info object ~d" number))
                                                             (let ((id (pdf-array-items (pdf-get (revision-trailer revision) "/ID"))))
                                                               (and id (every #'stringp id) (format nil "/ID <~{~a~^> <~}>" (mapcar #'id-hex id))))
                                                             (and (revision-prev revision) (format nil "/Prev ~a" (hex-offset (revision-prev revision)))))))))
                 (line "Claims" (let ((made (info-text pdf revision "/CreationDate")) (changed (info-text pdf revision "/ModDate"))
                                      (producer (info-text pdf revision "/Producer")))
                                  (and (or made changed producer)
                                       (format nil "~@[made ~a~]~@[, changed ~a~]~@[, by ~a~]" (and made (pdf-date-words made)) (and changed (pdf-date-words changed)) producer))))
                 (dolist (part (list (list "Adds" (revision-added revision) revision) (list "Changes" (revision-changed revision) revision)
                                     (list "Frees" (revision-freed revision) (if (plusp (revision-index revision)) (aref (pdf-revisions pdf) (1- (revision-index revision))) revision))))
                   (destructuring-bind (label numbers in) part
                     (when numbers
                       (put (format nil "  ~12a" label) :list-label)
                       (put (format nil "~:d object~:p: " (length numbers)))
                       (mapc #'put (object-pieces analysis in numbers))
                       (put (string #\Newline)))))
                 (dolist (problem (revision-problems revision)) (line "Not right" problem :warning))
                 (when (account-changes account)
                   (line "Pages" (format nil "changed: ~{~d~^, ~}" (mapcar (lambda (c) (1+ (page-index (page-change-page c)))) (account-changes account)))))
                 (dolist (change (account-changes account))
                   (let ((page (page-change-page change)))
                     (when (page-change-text change)
                       (put (format nil "  Page ~d, its text:~%" (1+ (page-index page))) :strong)
                       (loop for (kind text) in (page-change-text change) repeat 60
                             do (put (format nil "    ~:[-~;+~] ~a~%" (eq kind :added) (shorten-string text 120 :one-line t)) (if (eq kind :added) :diff.plus :diff.minus))))
                     (when (page-change-annotations change)
                       (put (format nil "  Page ~d, what is laid over it (annotations):~%" (1+ (page-index page))) :strong)
                       (loop for (kind text) in (page-change-annotations change) repeat 40
                             do (put (format nil "    ~:[-~;+~] ~a~%" (eq kind :added) (shorten-string text 120 :one-line t)) (if (eq kind :added) :diff.plus :diff.minus))))
                     (when (or (page-change-added change) (page-change-removed change))
                       (put (format nil "  Page ~d, what it draws:~%" (1+ (page-index page))) :strong)
                       (loop for m in (page-change-added change) repeat 40
                             do (put (format nil "    + ~a~%" (mark-words page m)) :diff.plus))
                       (loop for m in (page-change-removed change) repeat 40
                             do (put (format nil "    - ~a~%" (mark-words (or (page-change-old change) page) m)) :diff.minus)))))
                 (dolist (page (account-gone account))
                   (put (format nil "  Page ~d of the revision before is gone.~%" (1+ (page-index page))) :warning)))))))

(defun report-render (buffer analysis)
  "Show ANALYSIS in BUFFER, its report: its text and buttons, point at its start."
  (setf (buffer-local buffer 'analysis) analysis)
  (destructuring-bind (text spans buttons) (or (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis))))
    (set-made-text buffer text :spans spans :buttons buttons)
    (setf (buffer-point buffer) 0)))

(defmethod job-take-in ((job palimpsest-job))
  "Show what examining the PDF found, or say why it could not be examined."
  (show-reading-result job (palimpsest-job-path job)
                       (lambda (buffer) (report-render buffer (palimpsest-job-analysis job)))
                       "~a could not be examined: ~a~%"))

(defun palimpsest-buffer (native)
  "The buffer of the report on the PDF at NATIVE: the one there is, else a
new one, the PDF examined for it in the background (EXAMINE), its revisions
written to a folder of its own, deleted when it is closed."
  (let ((name (native-file-name native)))
    (reading-buffer '*palimpsest-reports* (lambda (b) (buffer-local b 'palimpsest-path)) native
                    (format nil "*Palimpsest: ~a*" name) (format nil "Examining ~a for signs of editing...~%" name) 'palimpsest-mode
                    (lambda (buffer)
                      (setf (buffer-local buffer 'palimpsest-path) native)
                      (start-job-thread (make-palimpsest-job buffer native (buffer-private-folder buffer)) #'examine :name "tuatara palimpsest"))
                    :private-folder "palimpsest/")))

(defun palimpsest-close-query (buffer)
  "Kill-buffer query: true to close BUFFER, a report on a PDF, at once when
its revisions and logs were saved (\\<palimpsest-mode>\\[palimpsest-save]) or none were written;
else asked first, as closing deletes them."
  (let ((analysis (buffer-local buffer 'analysis)))
    (or (null analysis) (analysis-saved analysis) (null (buffer-private-folder buffer))
        (notany #'first (analysis-files analysis))
        (yes-or-no-p* "Revisions and logs not saved: close and delete them?"))))

(defun palimpsest-forget (buffer)
  "Kill-buffer function: when BUFFER is a report on a PDF, close the buffers
showing the revisions and logs it wrote, as its folder is deleted with it."
  (let ((folder (and (buffer-local buffer 'palimpsest-path) (buffer-private-folder buffer))))
    (when folder
      (dolist (other (buffer-list))
        (let ((file (shown-file other)))
          (when (and file (not (eq other buffer)) (path-within-p (native file) folder))
            (kill-buffer other)))))))

;;; Saving the revisions, their logs and the report

(defun report-file-name (analysis)
  "The name the report on ANALYSIS's PDF is written under: NAME-revision-report.txt."
  (format nil "~a-revision-report.txt" (native-file-stem (analysis-name analysis))))

(defun saved-names (analysis)
  "The files saving ANALYSIS makes, each (SOURCE NAME SUM): each revision's PDF
and log written (SUM, a PDF's SHA-256, or NIL to be worked out), then the
report and the fingerprint (SOURCE NIL)."
  (append (loop for (file log sum) in (analysis-files analysis)
                when file append (list (list file (native-file-name file) sum) (list log (native-file-name log) nil)))
          (list (list nil (report-file-name analysis) nil) (list nil (fingerprint-file-name analysis) nil))))

(defun folder-refusal (analysis folder)
  "Why nothing of ANALYSIS's may be written in FOLDER, in words: it is the
folder of the PDF examined or a folder within it, or within palimpsest's own
folder in the cache, either told by where FOLDER really leads, through links
too; NIL when it may."
  (flet ((real (path) (slash-ended (or (real-path-or-parent (string-right-trim "/" path)) path))))
    (let ((evidence (parent-path (analysis-path analysis)))
          (cache (private-cache-directory "palimpsest/")))
      (cond ((or (string= (slash-ended folder) (slash-ended evidence)) (string= (real folder) (real evidence)))
             "it is the folder of the PDF examined: nothing is written beside the evidence")
            ((or (path-within-p folder cache) (path-within-p (real folder) (real cache)))
             "it is within palimpsest's own folder in the cache, which is cleared")
            ((or (path-within-p folder evidence) (path-within-p (real folder) (real evidence)))
             "it is within the folder of the PDF examined: nothing is written there, nor a folder made")))))

(defun offered-path (analysis name)
  "Where NAME is offered to be written for ANALYSIS: in the folder its work
was last saved to, else in the home folder, never beside the PDF examined."
  (concatenate 'string (if (analysis-saved analysis) (abbreviated-path (slash-ended (analysis-saved analysis))) "~/") name))

(defun save-refusal (analysis folder)
  "Why the revisions, logs and report of ANALYSIS cannot be saved to FOLDER,
in words; NIL when they can."
  (cond ((notany #'first (analysis-files analysis)) "no revision was written to save")
        ((folder-refusal analysis folder))
        ((and (path-kind folder) (not (eq :directory (path-kind folder)))) "it is not a folder")
        (t (let ((there (loop for (nil name) in (saved-names analysis)
                              when (path-kind (path-join folder name) :follow nil) collect name)))
             (and there (format nil "~{~a~^, ~} ~:[is~;are~] there already: nothing is written over" there (rest there)))))))

(defun copy-checked (source target &key sum (cancel (list nil)))
  "Copy the file SOURCE to TARGET, then check the copy's SHA-256 against SUM,
the original's (worked out when not given). Returns the copy's SHA-256; an
error naming TARGET when the two differ."
  (unless (copy-regular-file source target :cancel cancel) (job-check-stop))
  (let ((original (or sum (file-sha256 source :cancel cancel)))
        (copy (file-sha256 target :cancel cancel)))
    (unless (and original (equal original copy))
      (error "the copy at ~a does not have its original's SHA-256" target))
    copy))

(defun save-copies (analysis folder &key (cancel (list nil)))
  "Copy ANALYSIS's revisions and logs to FOLDER (made when it is not there),
and write the report there (REPORT-FILE-NAME), ending with where each copy is and
its SHA-256, each checked against its original, and the fingerprint beside it
\(FINGERPRINT-FILE-NAME), for \\[palimpsest-group-cases]. Returns what was saved, each
\(NAME SHA-256); an error naming the first copy whose sum differs."
  (let ((folder (slash-ended folder)) (saved '()))
    (ensure-directories folder)
    (loop for (source name sum) in (saved-names analysis)
          when source
            do (push (list name (copy-checked source (path-join folder name) :sum sum :cancel cancel)) saved))
    (let ((report (path-join folder (report-file-name analysis))))
      (write-new-file report
                       (with-output-to-string (s)
                         (write-string (report-text analysis) s)
                         (format s "~%* Saved~%~%Saved to ~a on ~a.~%Each copy's SHA-256, checked against its original:~%~%"
                                 folder (format-time "%F %T %z" (get-universal-time)))
                         (loop for (name sum) in (reverse saved) do (format s "  ~a  ~a~a~%" sum folder name))))
      (push (list (report-file-name analysis) (file-sha256 report :cancel cancel)) saved)
      (let ((print (path-join folder (fingerprint-file-name analysis))))
        (write-new-file print (fingerprint-text analysis))
        (push (list (fingerprint-file-name analysis) (file-sha256 print :cancel cancel)) saved)))
    (setf (analysis-saved analysis) folder)
    (nreverse saved)))

(defstruct (palimpsest-save-job (:include job) (:constructor make-palimpsest-save-job (buffer analysis folder)) (:copier nil))
  "Saving the revisions, logs and report of ANALYSIS to FOLDER for BUFFER, a job; SAVED, what was saved (SAVE-COPIES)."
  analysis folder saved)

(setf (documentation 'palimpsest-save-job-p 'function) "True when OBJECT is a PALIMPSEST-SAVE-JOB.")

(define-job-words palimpsest-save-job
  :doing "Saving the revisions"
  :description (format nil "Saving the revisions and logs to ~a" (palimpsest-save-job-folder job))
  :verb "save the revisions"
  :left-words "some copies may have been made")

(defmethod job-take-in ((job palimpsest-save-job))
  "Say what was saved, its sums checked, and show the report again with where; or why it was not."
  (let ((analysis (palimpsest-save-job-analysis job)) (buffer (job-buffer job)))
    (if (palimpsest-save-job-saved job)
        (progn
          (message "Saved ~d files to ~a, each copy's SHA-256 checked against its original"
                   (length (palimpsest-save-job-saved job)) (abbreviated-path (palimpsest-save-job-folder job)))
          (when (live-buffer-p buffer)
            (let ((point (buffer-point buffer)))
              (report-render buffer analysis)
              (setf (buffer-point buffer) (min point (buffer-size buffer))))))
        (message "Not saved: ~a" (job-ending-words job)))))

(define-command palimpsest-save ()
  "Save the revisions of the PDF the report is on, each with its log, and
the report itself as NAME-revision-report.txt, to a folder asked for (made when it is not
there): the work to keep, as the report's own copies go when it is closed.
Each copy's SHA-256 is checked against its original, and the report ends
with where each copy is and its sum; beside it NAME-fingerprint.txt, the
file's fingerprint, which \\[palimpsest-group-cases] sets beside others'. Never the PDF's own folder, nor the
package's own folder in the cache; a file there already is never written over: nothing is saved,
and which are there is said. Copied in the background."
  (let* ((buffer (current-buffer))
         (analysis (buffer-local buffer 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let ((folder (read-native-path "Save the revisions and logs to folder: ")))
          (when folder
            (let ((refusal (save-refusal analysis folder)))
              (if refusal
                  (message "Not saved to ~a: ~a" (abbreviated-path folder) refusal)
                  (start-job-thread (make-palimpsest-save-job buffer analysis folder)
                                    (lambda (job)
                                      (setf (palimpsest-save-job-saved job)
                                            (save-copies analysis folder :cancel (job-cancel job)))
                                      ;; the report again, naming where: made here, not where the editor waits
                                      (setf (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis)))))
                                    :name "tuatara palimpsest saving"))))))))

(defun write-report (analysis target)
  "Write the report on ANALYSIS's PDF as text to TARGET, a native name not
there yet; returns its SHA-256."
  (write-new-file target (report-text analysis))
  (file-sha256 target))

(define-command palimpsest-write-report ()
  "Write the report as text to a file asked for, NAME-revision-report.txt
offered in the folder last saved to, else in the home folder; never beside
the PDF examined nor in the package's own folder, nothing written over. Says
its SHA-256."
  (let ((analysis (buffer-local (current-buffer) 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let ((target (read-new-native-path "Write the report to: "
                                            :initial (offered-path analysis (report-file-name analysis)))))
          (when target
            (let ((refusal (or (folder-refusal analysis (parent-path target))
                               (and (path-kind target :follow nil) "it is there already, and nothing is written over"))))
              (if refusal
                  (message "Not written to ~a: ~a" (abbreviated-path target) refusal)
                  (message "Wrote the report to ~a, SHA-256 ~a" (abbreviated-path target) (write-report analysis target)))))))))

;;; One revision, to show

(defun revision-files (analysis number)
  "Revision NUMBER (from 1, the original) of ANALYSIS's PDF as it was written,
as three values: its PDF, its log and its SHA-256; NIL when there is no such
revision, or it could not be written."
  (let ((files (analysis-files analysis)))
    (when (<= 1 number (length files))
      (destructuring-bind (file log sum) (nth (1- number) files)
        (and file (values file log sum))))))

(defun log-beside (target)
  "The name of the log written beside TARGET, a PDF's native name: its name with .log for .pdf."
  (path-join (parent-path target) (concatenate 'string (native-file-stem target) ".log")))

(defun revision-refusal (analysis target)
  "Why a revision of ANALYSIS's PDF cannot be written as TARGET, a PDF's
native name, in words; NIL when it can."
  (let ((log (log-beside target)))
    (cond ((folder-refusal analysis (parent-path target)))
          ((path-kind target :follow nil) (format nil "~a is there already: nothing is written over" (native-file-name target)))
          ((path-kind log :follow nil) (format nil "~a is there already: nothing is written over" (native-file-name log))))))

(defstruct (palimpsest-revision-job (:include job) (:constructor make-palimpsest-revision-job (buffer analysis number target)) (:copier nil))
  "Writing revision NUMBER of ANALYSIS's PDF as TARGET, with its log beside it, for BUFFER, a job; SUM, the copy's checked SHA-256."
  analysis number target sum)

(setf (documentation 'palimpsest-revision-job-p 'function) "True when OBJECT is a PALIMPSEST-REVISION-JOB.")

(define-job-words palimpsest-revision-job
  :doing "Writing a revision"
  :description (format nil "Writing revision ~d to ~a" (palimpsest-revision-job-number job) (palimpsest-revision-job-target job))
  :verb "write the revision"
  :left-words "a part of it may have been written")

(defun write-revision (analysis number target &key (cancel (list nil)))
  "Copy revision NUMBER of ANALYSIS's PDF to TARGET, and its log beside it
\(LOG-BESIDE), each copy's SHA-256 checked against its original. Returns the
PDF copy's SHA-256; an error when a sum differs."
  (multiple-value-bind (file log sum) (revision-files analysis number)
    (ensure-directories target)
    (prog1 (copy-checked file target :sum sum :cancel cancel)
      (copy-checked log (log-beside target) :cancel cancel))))

(defmethod job-take-in ((job palimpsest-revision-job))
  "Open the revision written in the PDF view and say what it is; or why it was not written."
  (let* ((analysis (palimpsest-revision-job-analysis job))
         (target (palimpsest-revision-job-target job))
         (number (palimpsest-revision-job-number job)))
    (if (palimpsest-revision-job-sum job)
        (progn
          (visit-path target)
          (message "Wrote ~a: ~a as its revision ~d of ~d was, SHA-256 ~a, its log beside it~:[~;; ~a on it, then ~a, goes back further~]"
                   (abbreviated-path target) (analysis-name analysis) number (length (analysis-files analysis))
                   (palimpsest-revision-job-sum job) (> number 1)
                   (substitute-command-keys "\\[palimpsest]") (substitute-command-keys "\\<palimpsest-mode>\\[palimpsest-write-revision]")))
        (message "Revision ~d was not written: ~a" number (job-ending-words job)))))

(defun revision-meant ()
  "The report and the revision the current buffer points at, as two values:
in a report, the report and the revision whose part point is in (or NIL);
in a revision opened from a report, that report and that revision's number."
  (let ((buffer (current-buffer)))
    (if (buffer-local buffer 'analysis)
        (let ((revision (report-revision-at (buffer-local buffer 'analysis))))
          (values buffer (and revision (1+ (revision-index revision)))))
        (let ((file (and (shown-file buffer) (native (shown-file buffer)))))
          (dolist (report (buffer-list) (values nil nil))
            (let ((analysis (buffer-local report 'analysis)))
              (when analysis
                (let ((k (position file (analysis-files analysis) :key #'first :test #'equal)))
                  (when k (return (values report (1+ k))))))))))))

(define-command palimpsest-write-revision ()
  "Write a revision of the PDF the report is on as a PDF of its own, to show
to others, its log beside it: the revision whose part of the report point is
in, or the revision shown, when this is a revision opened from the report;
else asked for, the one before the latest offered. Asked where, offered in
the folder last saved to, else in the home folder; never beside the PDF
examined nor in the package's own folder, nothing written over; each copy's SHA-256 checked, and the PDF
opened when it is written. That PDF is the
file as it stood after that save: \\[palimpsest] on it finds the saves before."
  (multiple-value-bind (report meant) (revision-meant)
    (let* ((analysis (and report (buffer-local report 'analysis)))
           (count (and analysis (length (analysis-files analysis)))))
      (cond ((null analysis) (message "Not a report of palimpsest, nor a revision opened from one: ~a there first" (substitute-command-keys "\\[palimpsest]")))
            (t (let ((number (or meant (read-whole-number (format nil "Revision to write (1-~d)~~@[ (~~d)~~]: " count) (max 1 (1- count))
                                                          :valid (lambda (n) (<= 1 n count)) :refusal (format nil "Not a revision: 1 to ~d." count)))))
                 (cond ((null number))
                       ((null (revision-files analysis number))
                        (message "Revision ~d of ~a could not be written out, so it cannot be copied" number (analysis-name analysis)))
                       (t (let* ((offered (offered-path analysis (format nil "~a-revision-~d.pdf" (native-file-stem (analysis-name analysis)) number)))
                                 (target (read-new-native-path (format nil "Write revision ~d to: " number) :initial offered)))
                            (when target
                              (let ((refusal (revision-refusal analysis target)))
                                (if refusal
                                    (message "Not written to ~a: ~a" (abbreviated-path target) refusal)
                                    (start-job-thread (make-palimpsest-revision-job report analysis number target)
                                                      (lambda (job)
                                                        (setf (palimpsest-revision-job-sum job)
                                                              (write-revision analysis number target :cancel (job-cancel job))))
                                                      :name "tuatara palimpsest revision")))))))))))))

;;; Objects shown

(defun object-stream-name (analysis revision number)
  "The name of the buffer showing in hex the stream of the object NUMBER as REVISION of ANALYSIS's PDF has it."
  (format nil "*Palimpsest: ~a object ~d stream, revision ~d*" (analysis-name analysis) number (1+ (revision-index revision))))

(defun object-stream-shown (analysis revision number)
  "The decoded octets of the stream of the object NUMBER as REVISION of
ANALYSIS's PDF has it, as many as are shown (*PALIMPSEST-LARGEST-SHOWN*);
NIL when it has none, or it cannot be decoded."
  (let* ((pdf (analysis-pdf analysis))
         (object (revision-object pdf revision number))
         (data (and object (stream-data pdf object))))
    (and data (subseq data 0 (min (length data) *palimpsest-largest-shown*)))))

(defun object-text (analysis revision number &key hex)
  "The text showing the object NUMBER as REVISION of ANALYSIS's PDF has it:
where it is, what it is, its dictionary as written and its stream decoded:
as text, or (with HEX, or when it is not text) named as shown in hex in a
buffer of its own (OBJECT-STREAM-NAME)."
  (let* ((pdf (analysis-pdf analysis)) (octets (pdf-octets pdf))
         (object (revision-object pdf revision number)))
    (with-output-to-string (s)
      (format s "Object ~d of ~a, as ~a has it~%~%" number (analysis-name analysis) (revision-name revision))
      (if (null object)
          (format s "It is not in this revision.~%")
          (progn
            (dolist (line (object-evidence pdf revision number)) (format s "~a~%" line))
            (let ((role (role-words pdf revision number))) (when role (format s "It is ~a.~%" role)))
            (format s "~%Its dictionary, as written:~%~%")
            (if (pdf-indirect-packed-in object)
                (multiple-value-bind (pairs data) (packed-objects pdf (revision-object pdf revision (pdf-indirect-packed-in object)))
                  (declare (ignore pairs))
                  (format s "~a~%" (octets-latin-1 data :start (pdf-indirect-at object) :end (min (length data) (pdf-indirect-end object) (+ (pdf-indirect-at object) 65536)))))
                (format s "~a~%" (octets-latin-1 octets :start (pdf-indirect-at object)
                                                        :end (min (or (pdf-indirect-stream-start object) (pdf-indirect-end object)) (+ (pdf-indirect-at object) 65536)))))
            (when (pdf-indirect-stream-start object)
              (multiple-value-bind (data why) (stream-data pdf object)
                (format s "~%Its stream: ~:d octets in the file, ~a to ~a; filters: ~a; ~:[~:d octets decoded~;~:*~a~*~].~%~%"
                        (- (pdf-indirect-stream-end object) (pdf-indirect-stream-start object))
                        (hex-offset (pdf-indirect-stream-start object)) (hex-offset (pdf-indirect-stream-end object)) (filter-names object)
                        (and why (if (member why *pdf-picture-filters* :test #'equal) (format nil "picture data (~a), not decoded" why) why))
                        (length (or data #())))
                (when data
                  (let ((end (min (length data) *palimpsest-largest-shown*)))
                    (if (or hex (binary-head-p data end))
                        (format s "In hex: ~a~%" (object-stream-name analysis revision number))
                        (format s "~a~%" (octets-latin-1 data :end end)))
                    (when (< end (length data))
                      (format s "~%... ~:d octets more (*palimpsest-largest-shown*)~%" (- (length data) end))))))))))))

(defun show-object (analysis revision number &key hex at)
  "Show the object NUMBER as REVISION of ANALYSIS's PDF has it in a buffer of
its own, read only, as help is shown (OBJECT-TEXT); its stream, when it is
shown in hex, in a buffer of its own too, the object's naming it as a link.
With AT, a place in its decoded stream shown as text, the cursor there."
  (let* ((text (object-text analysis revision number :hex hex))
         (name (object-stream-name analysis revision number))
         (hex-at (search (format nil "In hex: ~a" name) text))
         (show-stream (lambda () (show-octets-in-hex name (object-stream-shown analysis revision number))))
         (buffer-name (format nil "*Palimpsest: ~a object ~d, revision ~d*" (analysis-name analysis) number (1+ (revision-index revision)))))
    (let ((window (show-help-pieces (if hex-at
                                        (list (subseq text 0 (+ hex-at 8))
                                              (list name :link show-stream)
                                              (subseq text (+ hex-at 8 (length name))))
                                        (list text))
                                    :name buffer-name)))
      (if hex-at
          (funcall show-stream)
          ;; the decoded stream begins after its line, an octet a character
          (let* ((line (search "Its stream: " text))
                 (start (and line at (search (format nil "~%~%") text :start2 line))))
            (when (and start window)
              (setf (window-point window)
                    (string-octet-length (subseq text 0 (min (length text) (+ start 2 at)))))))))))

(defun number-at-point (buffer)
  "The whole number written at point in BUFFER, or ending just before it, on
point's line; NIL when there is none."
  (let* ((point (buffer-point buffer))
         (start (buffer-line-start buffer point))
         (line (buffer-line-text buffer start))
         (column (length (buffer-string buffer :start start :end point))))
    (multiple-value-bind (s e) (cl-ppcre:scan "\\d+" line :start (max 0 (min (length line) (- column 6))))
      (and s (<= s column (1+ e)) (parse-integer line :start s :end e)))))

(define-command palimpsest-show-object ()
  "Show an object of the PDF the report is on, read only: its place in the
file, its dictionary as written and its stream decoded (at most
*palimpsest-largest-shown*), as text, or in hex in a buffer of its own when it
is not text or with \\[universal-argument]. Asks for its number, offering the object whose number is at
point, and shows it as the latest revision has it (the revision at point, on
an object named under a revision)."
  (let ((analysis (buffer-local (current-buffer) 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let* ((pdf (analysis-pdf analysis))
               (number (read-whole-number "Object number~@[ (~d)~]: " (number-at-point (current-buffer)) :valid (lambda (n) (>= n 0)) :refusal "Not an object number."))
               (revision (or (report-revision-at analysis) (last-revision pdf))))
          (when number
            (show-object analysis revision number :hex (prefix-argument)))))))

(defun report-revision-at (analysis)
  "The revision whose part of the report point is in, or NIL when it is in
none: told by the report's own headings before point, never by what the PDF
says, which the report quotes."
  (let* ((buffer (current-buffer))
         (point (buffer-point buffer))
         (revisions (pdf-revisions (analysis-pdf analysis)))
         (in-revisions nil) (found nil))
    (loop for (start end face) in (second (analysis-shown analysis))
          while (< start point)
          when (eq face :heading)
            do (let ((heading (string-trim '(#\Newline) (buffer-string buffer :start start :end end))))
                 (cond ((string= heading "* Revisions") (setf in-revisions t found nil))
                       ((uiop:string-prefix-p "** " heading)
                        (let ((k (and in-revisions (uiop:string-prefix-p "** Revision " heading)
                                      (parse-integer heading :start 12 :junk-allowed t))))
                          (setf found (and k (<= 1 k (length revisions)) (aref revisions (1- k))))))
                       (t (setf in-revisions nil found nil)))))
    found))

;;; Asking for a PDF

(define-command palimpsest ()
  "Examine a PDF for signs of editing, from its octets alone: every revision
from the original to the latest, what each added, changed and freed and how
its pages changed, and every sign of editing or forgery that examiners know,
each with its evidence. The file the current buffer shows when it is a PDF
\(a PDF's view, a visited PDF, a PDF on rimu's line), else, or with
\\[universal-argument], one asked for. The report is made in the background. Nothing in
the PDF is run and the file is never changed; each revision is written, with
a log of how to find its signs by hand, to a folder for you alone."
  (let* ((shown (and (not (prefix-argument)) (shown-file (current-buffer))))
         (path (if (and shown (pdf-file-p (native (expand-path shown)))) shown (read-file-path "PDF to examine: "))))
    (when path
      (let ((native (native (expand-path path))))
        (cond ((not (eq :regular (ignore-errors (path-kind native)))) (message "~a is not a file" (abbreviated-path native)))
              ((not (pdf-file-p native)) (message "~a is not a PDF: it does not begin with %PDF-" (abbreviated-path native)))
              (t (show-buffer (palimpsest-buffer native))))))))
