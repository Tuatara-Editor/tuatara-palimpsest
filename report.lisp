;;;; report.lisp --- The report on a PDF, its revisions written out with their logs.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; M-x palimpsest examines a PDF in the background and shows what it found
;;;; in a buffer of its own, read only: a summary, the findings strongest
;;;; first, then each revision from the original to the latest with what it
;;;; added, changed and freed and how its pages' text and drawing changed.
;;;; Each revision is written as a PDF of its own (the file's octets up to
;;;; its %%EOF line) beside a log saying how it was found and every sign in
;;;; it, with the offsets and octets that show it, so that the same can be
;;;; done again by hand with a hex editor. They are written to a directory of
;;;; the package's own in the cache directory, for you alone; the PDF itself
;;;; is only read.

(in-package #:palimpsest)

(define-package-setting *palimpsest-largest-written* (* 2 1024 1024 1024)
  "The most octets the revisions of one PDF, each written out as a PDF of its
own, may take together.
Revisions past it are listed as not written, as they are when the drive would
keep less than 1 GB free.
- a size, 1 octet or more: a whole number of octets, or words such as \"4G\";
  2 GB by default."
  :values (octets 1 *))

(define-package-setting *palimpsest-largest-shown* (* 1024 1024)
  "The most octets of an object's stream shown by
\\<palimpsest-mode>\\[palimpsest-show-object].
- a size, 1 octet or more: a whole number of octets, or words such as \"2M\";
  1 MB by default."
  :values (octets 1 *))

(defparameter *palimpsest-version* "1.0"
  "The version of the palimpsest package, as its logs name it.")

(define-major-mode palimpsest-mode special-mode
  "The report on a PDF examined for signs of editing (\\[palimpsest]), read
only: a summary, the findings (strong signs of editing, then those worth a
look, then those for the record), the ways of editing they tell of and the
craft those take, the file's fingerprint, and each revision from the
original to the latest. \\<palimpsest-mode>\\[push-button] on a revision's
PDF opens it in Tuatara's PDF view, on its log opens the log, on an
object's number shows the object. \\[forward-button] and
\\[backward-button] go from one of these to the next.
\\[palimpsest-show-object] shows an object's dictionary and stream (as
text, or in hex in a buffer of its own when it is not text; in hex too with
\\[universal-argument]). \\[palimpsest-save] saves the revisions, their
logs and the report to a directory of your choosing.
\\[palimpsest-write-revision] writes the revision whose part point is in as
a PDF to show, its log beside it. \\[palimpsest-write-report] writes the
report alone as text. \\[quit-window] leaves it, \\[kill-buffer] closes it.
Closing the report closes the revisions and logs opened from it and deletes
its own copies of the revisions, asking first when they were not saved."
  :lighter "Palimpsest" :interactive nil)

(defvar *palimpsest-reports* '()
  "The buffers showing a report on a PDF.")

(defstruct (analysis (:copier nil))
  "What examining a PDF found: PATH and NAME of the file, SIZE, SHA-256. PDF,
its structure (PDF). ACCOUNTS, each revision compared with the one before.
FINDINGS. DIRECTORY, where its revisions were written. FILES, each revision's
\(PDF-PATH LOG-PATH SHA-256), or (NIL NIL WHY) when not written. SHOWN, the
report's text, faces and buttons (as PIECES-TEXT gives them), made in the
background. SAVED, the directory its revisions, logs and report were saved to,
or NIL while they are not. FINGERPRINT, its fingerprint once worked out
\(ANALYSIS-TRAITS). JOB, the last job started on its PDF (START-ANALYSIS-JOB).
PICTURE, for a picture file examined as one scanned page, its PICTURE-FILE,
its pixels let go once examined; its PDF is then the page that stands for
it (PICTURE-PAGE-OCTETS), and it has no revisions written. CHECKS, each
check's outcome in the order run, (KEY . OUTCOME) as ALL-FINDINGS gives
them, for the report's Checks. PLACE, the file's path as the report shows
it.
The editor's own, made from what examining the file shut in sent back
\(TAKE-EXAMINED), has no PDF, accounts, findings or checks: its PICTURE is
the picture's kind, a key of *PICTURE-KINDS*; REVISIONS, the name of each
revision; PIECES, the report's pieces; STOPPED, why examining it stopped,
when it ran out of memory or time."
  path name size sha256 pdf (accounts '()) (findings '()) directory (files '()) shown saved fingerprint job picture
  (checks '()) place (revisions '()) (pieces '()) stopped)

(setf (documentation 'analysis-p 'function) "True when OBJECT is an ANALYSIS.")

(defun picture-refused-p (analysis what)
  "True, and why said, when ANALYSIS is of a picture file, which has no
revisions or objects for WHAT, a command's work in words, to act on."
  (when (and analysis (analysis-picture analysis))
    (message "~a is a picture, not a PDF: it has no revisions or objects to ~a" (analysis-name analysis) what)
    t))

(defun analysis-busy-p (analysis)
  "True, and why said, while a job started on ANALYSIS's PDF runs: its tables
are then the job's alone, and nothing else reads the PDF."
  (let ((job (analysis-job analysis)))
    (when (and job (job-running-p job))
      (message "~a is still being read for this report: try again when that ends" (analysis-name analysis))
      t)))

(defun start-analysis-job (analysis job work name)
  "Start JOB, which reads ANALYSIS's PDF, running WORK on a thread called
NAME (START-JOB-THREAD); NIL, with why said, when one runs already."
  (unless (analysis-busy-p analysis)
    (start-job-thread (setf (analysis-job analysis) job) work :name name)))

;;; The work in the background

(defstruct (palimpsest-job (:include job) (:constructor make-palimpsest-job (buffer path directory)) (:copier nil))
  "The PDF at PATH being examined for BUFFER, its revisions written to
DIRECTORY, a job: PHASE, what it does now, PROGRESS, how far it has got from 0
to 1; ANALYSIS, what it found."
  path directory (phase "Reading the PDF") (progress 0) analysis)

(setf (documentation 'palimpsest-job-p 'function) "True when OBJECT is a PALIMPSEST-JOB.")

(define-job-words palimpsest-job
  :doing (palimpsest-job-phase job)
  :description (format nil "Examining ~a for signs of editing" (native-file-name (palimpsest-job-path job)))
  :verb "examine the PDF"
  :left-words "nothing found")

(defmethod job-percent ((job palimpsest-job))
  "How far examining the PDF has got."
  (running-percent (round (* 1000 (palimpsest-job-progress job))) 1000))

(defun stage (phase from to)
  "A function of a fraction, telling how far examining a file has got from
FROM to TO, its phase PHASE (*SHUT-IN-PROGRESS*)."
  (funcall *shut-in-progress* (list phase from))
  (lambda (fraction)
    (funcall *shut-in-progress* (list phase (float (+ from (* (- to from) (min 1 (max 0 fraction)))))))))

(defun examine (job)
  "What a job examining a file does: the file examined in a Tuatara of its own
shut in (EXAMINE-SHUT-IN), and the report made from what that sent back
\(TAKE-EXAMINED); when it ran out of memory or time, a report saying so."
  (let* ((path (palimpsest-job-path job))
         (directory (palimpsest-job-directory job))
         (analysis (make-analysis :path path :name (native-file-name path) :directory directory
                                  :place (abbreviated-path path))))
    (handler-case
        (take-examined analysis (shut-in 'examine-shut-in (list path) directory
                                         (format nil "The check of ~a" (analysis-name analysis))
                                         :arguments (list :place (analysis-place analysis))
                                         :progress (lambda (value)
                                                     (when (and (consp value) (stringp (first value))
                                                                (realp (second value)))
                                                       (setf (palimpsest-job-phase job)
                                                             (shorten-string (first value) 60 :one-line t)
                                                             (palimpsest-job-progress job)
                                                             (max 0 (min 1 (second value))))))))
      (check-stopped (condition) (setf (analysis-stopped analysis) (check-stopped-words condition))))
    (setf (analysis-shown analysis) (multiple-value-list (pieces-text (shown-pieces analysis)))
          (palimpsest-job-analysis job) analysis)))

(defun octets-picture-kind (octets)
  "The kind of picture file OCTETS begin as, a key of *PICTURE-KINDS*, or NIL
for any other file."
  (car (assoc (nth-value 1 (octets-kind (subseq octets 0 (min (length octets) +binary-sample+)))) *picture-kinds*)))

(defun examine-file (path directory &key place)
  "Examine the file at PATH as the work shut in does: read whole and those
octets summed, its structure read, each revision compared with the one
before, every sign found, and each revision written out with its log into
DIRECTORY. A picture file is examined as one scanned page instead
\(EXAMINE-PICTURE). Returns the ANALYSIS, PLACE its path as the report
shows it."
  (let* ((octets (source-octets path))
         (analysis (make-analysis :path path :name (native-file-name path) :size (length octets)
                                  :directory directory :place place)))
    (funcall (stage "Summing the PDF" 0.02 0.05) 0)
    (setf (analysis-sha256 analysis) (octets-sha256 octets))
    (let ((kind (octets-picture-kind octets)))
      (when kind
        (return-from examine-file (examine-picture analysis octets kind))))
    (let* ((pdf (read-pdf octets
                          :path path
                          :name (analysis-name analysis)
                          :progress (stage "Reading its saves" 0.05 0.35))))
      (setf (analysis-pdf analysis) pdf)
      (let ((compare (stage "Comparing its saves" 0.35 0.6)) (revisions (pdf-revisions pdf)))
        (setf (analysis-accounts analysis)
              (loop for revision across revisions for k from 1
                    collect (prog1 (compare-revision pdf revision) (funcall compare (/ k (length revisions)))))))
      (setf (values (analysis-findings analysis) (analysis-checks analysis))
            (all-findings pdf (analysis-accounts analysis)
                          :progress (stage "Looking for signs of editing" 0.6 0.85)))
      (analysis-traits analysis)
      (write-revisions analysis)
      analysis)))

(defun examine-shut-in (paths directory &key settings place)
  "Examine the file, the first of PATHS, as the work shut in (EXAMINE-FILE),
its revisions and logs written into DIRECTORY, and say what was found as
data: its size and SHA-256; its kind of picture, or NIL for a PDF; the name
of each revision; for each revision written its SHA-256, else NIL and why
not; its fingerprint, each (KIND VALUE); and the report's pieces
\(PIECES-DATA). SETTINGS are those it is examined by (SHUT-IN)."
  (call-with-settings
   settings
   (lambda ()
     (let* ((analysis (examine-file (first paths) directory :place place))
            (pdf (analysis-pdf analysis))
            (picture (analysis-picture analysis)))
       (list (analysis-size analysis)
             (analysis-sha256 analysis)
             (and picture (string-downcase (symbol-name (picture-file-kind picture))))
             (map 'list #'revision-name (pdf-revisions pdf))
             (loop for (file nil sum) in (analysis-files analysis)
                   collect (if file (list sum nil) (list nil sum)))
             (loop for (kind value) in (analysis-traits analysis)
                   collect (list (string-downcase (symbol-name kind)) value))
             (pieces-data (report-pieces analysis) directory))))))

(defun take-examined (analysis data)
  "Fill ANALYSIS, the editor's, with DATA, what examining its file shut in
sent back (EXAMINE-SHUT-IN), each part checked: what does not fit is let go,
words are shown as the editor shows control characters, and a revision
counts as written only when its PDF and log are plain files in ANALYSIS's
directory."
  (flet ((words (text) (and (stringp text) (escape-controls (shorten-string text 400 :one-line t)))))
    (destructuring-bind (&optional size sum kind revisions files traits pieces &rest more) data
      (declare (ignore more))
      (let ((directory (analysis-directory analysis)))
        (setf (analysis-size analysis) (and (integerp size) (<= 0 size) size)
              (analysis-sha256 analysis) (and (stringp sum) (= 64 (length sum))
                                              (every (lambda (c) (digit-char-p c 16)) sum) sum)
              (analysis-picture analysis) (and (stringp kind)
                                               (car (find kind *picture-kinds*
                                                          :key (lambda (entry) (symbol-name (car entry)))
                                                          :test #'string-equal)))
              (analysis-revisions analysis) (loop for name in (and (listp revisions) revisions)
                                                  repeat *palimpsest-most-revisions*
                                                  collect (or (words name) "a revision"))
              (analysis-files analysis)
              (loop for entry in (and (listp files) files) for k from 1
                    repeat (length (analysis-revisions analysis))
                    collect (destructuring-bind (&optional sum why &rest more) (and (listp entry) entry)
                              (declare (ignore more))
                              (let ((file (path-join directory (format nil "revision-~d.pdf" k)))
                                    (log (path-join directory (format nil "revision-~d.log" k))))
                                (if (and (stringp sum) (eq :regular (path-kind file :follow nil))
                                         (eq :regular (path-kind log :follow nil)))
                                    (list file log sum)
                                    (list nil nil (or (words why) "not written"))))))
              (analysis-fingerprint analysis)
              (loop for entry in (and (listp traits) traits)
                    for (name value) = (and (listp entry) entry)
                    for kind = (and (stringp name)
                                    (car (find name *trait-weights* :key (lambda (weight) (symbol-name (car weight)))
                                                                    :test #'string-equal)))
                    when (and kind (stringp value)) collect (list kind (words value)))
              (analysis-pieces analysis)
              (data-pieces (and (listp pieces) pieces) directory (lambda (action) (report-action analysis action))))
        (ignore-errors (remove-file (path-join directory "pieces.txt")))
        analysis))))

(defun report-action (analysis action)
  "What a button of the report on ANALYSIS does, made from ACTION, its data as
REPORT-PIECES gives it, a list of words and numbers: (\"object\" REVISION
NUMBER AT) shows an object, (\"revision\" K) and (\"log\" K) open a
revision's PDF and log, (\"check\" KEY) shows what a check looks for, and
\(\"saved\") stands where the line saying where the report was saved goes,
as :SAVED. NIL for any other, or one that names what is not there."
  (destructuring-bind (kind &optional a b c &rest more) action
    (declare (ignore more))
    (let ((count (length (analysis-revisions analysis))))
      (flet ((whole (n) (and (integerp n) (<= 0 n) n)))
        (cond ((string= kind "saved") :saved)
              ((string= kind "check")
               (let ((check (and (stringp a) (find a *checks* :key (lambda (c) (symbol-name (check-key c)))
                                                              :test #'string-equal))))
                 (and check (let ((key (check-key check))) (lambda () (show-check-explanation key))))))
              ((string= kind "object")
               (let ((revision (whole a)) (number (whole b)) (at (whole c)))
                 (and revision number (< revision count)
                      (lambda () (show-object analysis revision number :at at)))))
              ((member kind '("revision" "log") :test #'string=)
               (let ((entry (and (whole a) (<= 1 a (length (analysis-files analysis)))
                                 (nth (1- a) (analysis-files analysis)))))
                 (and entry (first entry)
                      (if (string= kind "log")
                          (let ((log (second entry))) (lambda () (visit-path log :show #'show-log)))
                          (let ((file (first entry))) (lambda () (visit-path file))))))))))))

(defun summary-line (label value &optional face)
  "The pieces of a line of a report's summary saying VALUE of LABEL, VALUE in
FACE; none when VALUE is NIL."
  (and value (list (cons (format nil "  ~12a" label) :list-label)
                   (if face (cons (format nil "~a~%" value) face) (format nil "~a~%" value)))))

(defun shown-pieces (analysis)
  "The pieces of the report the editor shows on ANALYSIS, its own: those
examining sent back, with where it was saved, once it was; or, when examining
stopped, a report saying why."
  (if (analysis-stopped analysis)
      (append (list (cons (format nil "Palimpsest: ~a~%" (analysis-name analysis)) :heading)
                    (format nil "Read from the file's octets alone: nothing in it was run, and the file was not ~
                                 changed.~%")
                    (cons (format nil "~%* Summary~%") :heading))
              (summary-line "File" (analysis-place analysis))
              (summary-line "Verdict" "Not examined: what was found before it stopped is not kept." :warning)
              (summary-line "Stopped" (analysis-stopped analysis) :warning))
      (loop for piece in (analysis-pieces analysis)
            append (if (and (consp piece) (consp (cdr piece)) (eq (third piece) :saved))
                       (summary-line "Saved to" (and (analysis-saved analysis)
                                                     (abbreviated-path (analysis-saved analysis))))
                       (list piece)))))

(defun write-revisions (analysis)
  "Write each revision of ANALYSIS's PDF to its directory as revision-N.pdf,
with revision-N.log beside it, as far as *PALIMPSEST-LARGEST-WRITTEN* and the
free space allow."
  (let* ((pdf (analysis-pdf analysis)) (directory (analysis-directory analysis)) (written 0)
         (progress (stage "Writing its revisions" 0.85 0.99))
         (revisions (pdf-revisions pdf)))
    (setf (analysis-files analysis)
          (loop for revision across revisions for k from 1
                collect (progn
                          (job-check-stop)
                          (funcall progress (/ k (length revisions)))
                          (let ((end (revision-end revision)) (free (ignore-errors (free-octets directory))))
                            (cond ((> (+ written end) *palimpsest-largest-written*)
                                   (list
                                     nil
                                     nil
                                     "not written: the revisions together would pass *palimpsest-largest-written*"))
                                  ((and free (< free (+ end (expt 2 30))))
                                   (list
                                     nil
                                     nil
                                     "not written: the drive of the cache directory would keep less than 1 GB free"))
                                  (t (incf written end)
                                     (let ((file (path-join directory (format nil "revision-~d.pdf" k)))
                                           (log (path-join directory (format nil "revision-~d.log" k))))
                                       (write-new-file file (pdf-octets pdf) :end end)
                                       (let ((sum (file-sha256 file)))
                                         (write-new-file log (revision-log analysis revision sum))
                                         (list file log sum)))))))))))

;;; The log of a revision

(defun xref-kind-words (kind)
  "KIND, of a save's cross-reference section, in words; NIL for none."
  (case kind
    (:table "a table (the keyword xref)")
    (:stream "a stream")
    (:hybrid "a table with a stream beside it (/XRefStm), a hybrid")))

(defun strength-words (strength &optional (count 1) short)
  "STRENGTH, a finding's, in words, for COUNT findings of it; with SHORT, in
one word, as the report's Checks gives it: Strong, Look or Record."
  (ecase strength
    (:strong (cond (short "Strong") ((= count 1) "Strong sign of editing") (t "Strong signs of editing")))
    (:look (if short "Look" "Worth a look"))
    (:record (if short "Record" "For the record"))))

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
       (format s "Palimpsest log of ~a of ~a~%~a~%~%"
               (revision-name revision)
               (analysis-name analysis)
               (make-string 60 :initial-element #\=))
       (format s "Source file:  ~a~%Size:         ~:d octets (~a)~%SHA-256:      ~a~%"
               (analysis-name analysis)
               (analysis-size analysis)
               (hex-offset (analysis-size analysis)) (or (analysis-sha256 analysis) "(could not be summed)"))
       (format s "Written:      ~a~%" (format-time "%F %T %z" (get-universal-time)))
       (format s "Written by:   palimpsest ~a, one of the Tuatara project's packages~%~%" *palimpsest-version*)
       ;; how it was found
       (format s "How this revision was found~%---------------------------~%")
       (format s "Its %%EOF is at ~a; its line ends at ~a.~%"
               (offset-words (revision-eof revision))
               (offset-words (revision-end revision)))
       (if (revision-startxref-at revision)
           (format s "Its startxref keyword is at ~a and names ~:[nothing that can be read~;~:*offset ~a~].~%"
                   (offset-words (revision-startxref-at revision))
                   (and (revision-startxref revision) (offset-words (revision-startxref revision))))
           (format s "It has no startxref keyword.~%"))
       (when (revision-linearised revision)
         (multiple-value-bind (named last-at)
             (pdf-startxref octets :start (revision-start revision) :end (revision-eof revision))
           (format s "It is a file prepared for the web (linearised), saved once in two parts: first the section for ~
                      its first page~:[~;, ending in a %%EOF of its own whose startxref readers pass over and its ~
                      makers write as 0~], then the rest of the file, whose section the first page's trailer names by ~
                      /Prev; the startxref before its last %%EOF, at ~a, names ~:[nothing that can be read~;~:*offset ~
                      ~a~].~%"
                   (and (revision-startxref-at revision)
                        (revision-prev revision)
                        (< (revision-startxref-at revision) (revision-prev revision)))
                   (offset-words last-at)
                   (and named (offset-words named)))))
       (format s "Its cross-reference section ~:[could not be found: its objects were found by scanning its ~
                  octets~;~:*is ~a at ~a~].~%"
               (xref-kind-words (revision-xref-kind revision))
               (and (revision-xref-at revision) (offset-words (revision-xref-at revision))))
       (when (revision-trailer-at revision)
         (format s "Its trailer is at ~a:~%" (offset-words (revision-trailer-at revision)))
         (dolist (line (octets-evidence "  the trailer" octets (revision-trailer-at revision)
                                        (min (revision-eof revision) (+ (revision-trailer-at revision) 240))))
           (format s "  ~a~%" line)))
       (let ((prev (revision-prev revision)))
         (if prev
             (let ((named (find prev (pdf-revisions pdf) :key #'revision-xref-at)))
               (if (and (revision-linearised revision) (< (revision-start revision) prev (revision-eof revision)))
                   (format s "Its trailer's /Prev is ~d (~a), the section of the rest of the file, in this same save.~%"
                           prev
                           (hex-offset prev))
                   (format s "Its trailer's /Prev is ~d (~a), ~:[which is the cross-reference section of no ~
                              revision~;~:*the cross-reference section of ~a~].~%"
                           prev (hex-offset prev) (and named (revision-name named)))))
             (format s "Its trailer names no /Prev~:[~; (it is the original)~].~%" (= k 1))))
       (dolist (problem (revision-problems revision)) (format s "Not as it should be: ~a.~%" problem))
       (format s "~%This revision is the source file's octets from 0x00000000 to ~a, the end of its %%EOF line ~
                  included: ~:d octets.~%"
               (hex-offset (1- (revision-end revision))) (revision-end revision))
       (format s "revision-~d.pdf SHA-256: ~a~%" k (or sum "(could not be summed)"))
       (format s "To check: copy the source file's first ~:d octets into a new file; its SHA-256 is the one above.~%~%"
               (revision-end revision))
       ;; the signs
       (let ((signs (remove-if-not (lambda (f) (eql (finding-revision f) (revision-index revision)))
                                   (analysis-findings analysis))))
         (format s "Signs found in this revision: ~d~%------------------------------~%" (length signs))
         (loop for f in signs for n from 1
               do (format s "~%[~d] ~a: ~a~%" n (strength-words (finding-strength f)) (finding-words f))
                  (when (finding-page f) (format s "    Page: ~d~%" (finding-page f)))
                  (let ((check (find-check (finding-check f))))
                    (when check (format s "    Check: ~a~%" (check-label check))))
                  (when (finding-objects f)
                    (format s "    Objects: ~{~d~^, ~}~%"
                            (subseq (finding-objects f) 0 (min 50 (length (finding-objects f))))))
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
                        ((pdf-indirect-get o :packed-in)
                         (format nil "in object stream ~d at ~a of its decoded octets"
                                 (pdf-indirect-get o :packed-in)
                                 (offset-words (pdf-indirect-get o :at))))
                        (t (format nil "at ~a" (offset-words (pdf-indirect-get o :at))))))))
         (format s "Added (~d):~%" (length (revision-added revision)))
         (dolist (n (revision-added revision))
           (format s "  object ~d ~a~@[, ~a~]~%" n (place revision n) (role-words pdf revision n)))
         (format s "Changed (~d):~%" (length (revision-changed revision)))
         (dolist (n (revision-changed revision))
           (format s "  object ~d now ~a, before ~a~@[, ~a~]~%"
                   n
                   (place revision n)
                   (place before n)
                   (role-words pdf revision n)))
         (when (revision-same revision)
           (format s "Written again unchanged (~d):~%" (length (revision-same revision)))
           (dolist (n (revision-same revision))
             (format s "  object ~d now ~a, before ~a~%" n (place revision n) (place before n))))
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

(defun unit-words (number unit)
  "NUMBER of UNIT, a noun, in words: 1 region, 3 patches, 1,204 characters."
  (format nil "~:d ~a~:[~;~:[s~;es~]~]" number unit (/= number 1)
          (some (lambda (end) (let ((at (- (length unit) (length end))))
                                (and (>= at 0) (string= end unit :start2 at))))
                '("s" "x" "ch" "sh"))))

(defun check-summary (findings outcome)
  "The cell of the report's Checks for a check whose FINDINGS are these and
whose outcome is OUTCOME (RUN-CHECK): could not finish, not run and why,
none, or the strongest strength of its findings, what they count (their
tallies, else how many findings) and the pages they hold for; and as a
second value that strength, NIL when it has no findings."
  (cond ((eq outcome :unfinished) "could not finish")
        (outcome (format nil "not run: ~a" outcome))
        ((null findings) "none")
        (t
         (flet ((pages-of (some)
                  (sort (remove-duplicates (loop for f in some
                                                 append (or (third (finding-tally f))
                                                            (and (finding-page f) (list (finding-page f))))))
                        #'<)))
           (let* ((strength (find-if (lambda (s) (find s findings :key #'finding-strength)) *strengths*))
                  (these (remove strength findings :key #'finding-strength :test-not #'eq))
                  (measured (remove :dpi these :key (lambda (f) (second (finding-tally f))) :test-not #'eq))
                  (counted (remove :dpi these :key (lambda (f) (second (finding-tally f)))))
                  (units '()) (others 0))
             (dolist (f counted)
               (destructuring-bind (&optional number unit &rest pages) (finding-tally f)
                 (declare (ignore pages))
                 (if (and number unit)
                     (let ((entry (or (assoc unit units :test #'string=)
                                      (first (push (cons unit 0) units)))))
                       (incf (cdr entry) number))
                     (incf others))))
             (values
              (format nil "~:[~*~;~a: ~]~{~a~^; ~}"
                      (or counted (not (eq strength :record)))
                      (strength-words strength 1 t)
                      (append (loop for f in measured
                                    for pages = (pages-of (list f))
                                    collect (format nil "about ~d dpi~@[, ~a~]" (first (finding-tally f))
                                                    (and pages (pages-words pages :ranges t))))
                              (and counted
                                   (list (format nil "~{~a~^, ~}~@[, ~a~]"
                                                 (append (loop for (unit . number) in (reverse units)
                                                               collect (unit-words number unit))
                                                         (and (plusp others) (list (unit-words others "finding"))))
                                                 (let ((pages (pages-of counted)))
                                                   (and pages (pages-words pages :ranges t))))))))
              strength))))))

(defun show-check-explanation (key)
  "Show the explanation of the check of KEY in the *Help* buffer, filled as help is."
  (let ((check (find-check key)))
    (show-help (format nil "~a~%~%~a" (check-label check) (check-explanation check)) :fill t)))

(defun checks-pieces (analysis)
  "Pieces of the report's Checks for ANALYSIS: a row for each check run on
its kind of file, its label a link to its explanation
\(SHOW-CHECK-EXPLANATION), then what it found (CHECK-SUMMARY); none when
ANALYSIS holds no outcomes."
  (let* ((outcomes (analysis-checks analysis))
         (width (+ 2 (reduce #'max outcomes :key (lambda (o) (length (check-label (find-check (car o)))))
                                            :initial-value (length "Check"))))
         (findings (analysis-findings analysis)))
    (collecting-pieces (put)
      (when outcomes
        (put (format nil "~%* Checks~%") :heading)
        (put (format nil "  ~va~a~%" width "Check" "Summary") :list-label)
        (loop for (key . outcome) in outcomes
              for label = (check-label (find-check key))
              do (multiple-value-bind (summary strength)
                     (check-summary (remove key findings :key #'finding-check :test-not #'eq) outcome)
                   (put "  ")
                   (put label :link (list :check key))
                   (put (make-string (- width (length label)) :initial-element #\Space))
                   (put (format nil "~a~%" summary)
                        (cond ((or (eq outcome :unfinished) (eq strength :strong)) :warning)
                              ((or outcome (null strength)) :list-dim)))))))))

(defun summary-verdict (findings)
  "The verdict of FINDINGS, in words."
  (cond ((find :strong findings :key #'finding-strength) "Strong signs that the file was edited after it was made.")
        ((find :look findings :key #'finding-strength) "Signs worth a look, none of them strong.")
        (t "No sign of editing found.")))

(defun object-pieces (analysis revision numbers &optional (most 60))
  "Pieces naming the objects NUMBERS of REVISION of ANALYSIS's PDF, each a
button showing it (REPORT-ACTION), at most MOST of them."
  (declare (ignore analysis))
  (collecting-pieces (put)
    (loop for n in numbers for k from 0
          while (< k most)
          do (when (plusp k) (put ", "))
             (put (princ-to-string n) :link (list :object (revision-index revision) n)))
    (when (> (length numbers) most) (put (format nil " and ~:d more" (- (length numbers) most))))))

(defun detail-pieces (detail analysis revision put)
  "Put DETAIL, a line of a finding's details, by PUT: each place of text in
it (4 @539, PLACE-LABEL) a link showing that object with the cursor where
its operators begin, and each picture named (picture 7) one showing it, as
REVISION has it (REPORT-ACTION). ANALYSIS is the report's."
  (declare (ignore analysis))
  (let ((at 0))
    (cl-ppcre:do-scans (s e starts ends "(\\d+) @(\\d+)|picture (\\d+)" detail)
      (funcall put (subseq detail at s) :list-note)
      (if (aref starts 0)
          (let ((number (parse-integer detail :start (aref starts 0) :end (aref ends 0)))
                (from (parse-integer detail :start (aref starts 1) :end (aref ends 1))))
            (funcall put (subseq detail s e) :link (list :object (revision-index revision) number from)))
          (let ((number (parse-integer detail :start (aref starts 2) :end (aref ends 2))))
            (funcall put (subseq detail s e) :link (list :object (revision-index revision) number))))
      (setf at e))
    (funcall put (subseq detail at) :list-note)))

(defun report-pieces (analysis)
  "The pieces of the report on ANALYSIS: its summary, findings, the ways of
editing they tell of (METHODS-FOUND), its fingerprint (FINGERPRINT) and
its revisions; for a picture file, which has none, the picture's kind and
size in their place. Made where the file is read: its buttons are data, a
list of a keyword and numbers (REPORT-ACTION), and where it was saved is
left to the editor (SHOWN-PIECES)."
  (let* ((pdf (analysis-pdf analysis)) (last (last-revision pdf)) (findings (analysis-findings analysis))
         (picture (analysis-picture analysis)))
    (collecting-pieces (put)
      (flet ((line (label value &optional face) (mapc #'put (summary-line label value face)))
             (heading (text) (put (format nil "~%~a~%" text) :heading)))
        (put (format nil "Palimpsest: ~a~%" (analysis-name analysis)) :heading)
        (put (format nil "Read from the file's octets alone: nothing in it was run, and the file was not changed.~%"))
        (unless picture
          (put (format nil "~a~%"
                       (substitute-command-keys (format nil "\\<palimpsest-mode>\\[push-button] on a link opens it; ~
                                                             \\[palimpsest-show-object] shows an object."))) :list-dim))
        (heading "* Summary")
        (line "File" (or (analysis-place analysis) (abbreviated-path (analysis-path analysis))))
        (line "Size"
              (format nil "~:d octets (~a), ~a"
                      (analysis-size analysis)
                      (hex-offset (analysis-size analysis))
                      (octets-words (analysis-size analysis))))
        (line "SHA-256" (or (analysis-sha256 analysis) "could not be summed"))
        (if picture
            (line "Picture" (format nil "~a, ~:[its pixels not read~;~:*~:d by ~:d pixels~], examined as one ~
                                         scanned page"
                                    (cdr (assoc (picture-file-kind picture) *picture-kinds*))
                                    (and (plusp (picture-file-width picture)) (picture-file-width picture))
                                    (picture-file-height picture)))
            (progn
              (line "PDF version" (or (pdf-version pdf) "not given"))
              (line "Revisions"
                    (format nil "~d: the original~[~; and 1 later save~:;~:* and ~d later saves~]"
                            (length (pdf-revisions pdf))
                            (1- (length (pdf-revisions pdf)))))
              (line "Pages" (princ-to-string (length (pages-of pdf last))))))
        (line "Verdict"
              (summary-verdict findings)
              (if (find :strong findings :key #'finding-strength) :warning :strong))
        (line "Findings" (format nil "~d strong sign~:p of editing, ~d worth a look, ~d for the record"
                                 (count :strong findings :key #'finding-strength)
                                 (count :look findings :key #'finding-strength)
                                 (count :record findings :key #'finding-strength)))
        (flet ((date (key) (let ((d (info-text pdf last key))) (and d (pdf-date-words d)))))
          (line "Made" (date "/CreationDate"))
          (line "Changed" (date "/ModDate")))
        ;; where it was saved, said by the editor once it is (SHOWN-PIECES)
        (put "" nil (list :saved))
        (line "Creator" (info-text pdf last "/Creator"))
        (line "Producer" (info-text pdf last "/Producer"))
        (dolist (note (reverse (pdf-notes pdf))) (line "Not read" note :warning))
                (mapc #'put (checks-pieces analysis))
        ;; findings
        (heading "* Findings")
        (when (null findings) (put (format nil "  None.~%")))
        (dolist (strength *strengths*)
          (let ((some (remove strength findings :key #'finding-strength :test-not #'eq)))
            (when some
              (heading (format nil "** ~a (~d)" (strength-words strength 2) (length some)))
              (dolist (f some)
                (put "- ")
                (put (format nil "~a~%" (finding-words f)) (if (eq strength :strong) :warning nil))
                (let ((revision (aref (pdf-revisions pdf) (finding-revision f))))
                  (unless picture
                    (put (format nil "    ~a~@[, page ~d~]" (revision-name revision) (finding-page f)) :list-note)
                    (when (finding-objects f)
                      (put ", objects " :list-note)
                      (mapc #'put (object-pieces analysis revision (finding-objects f) 20)))
                    (put (string #\Newline)))
                  (dolist (d (finding-details f))
                    (put "    " :list-note)
                    (detail-pieces d analysis revision #'put)
                    (put (string #\Newline))))))))
        ;; the ways of editing the findings tell of, and the craft they take
        (let ((ways (methods-found findings)))
          (heading "* Methods")
          (put (format nil "  ~a~%" (craft-words ways))
               (let ((strong (remove-if-not #'strong-way-p ways)))
                 (and strong (if (>= (reduce #'max strong :key #'second) 3) :warning :strong))))
          (when ways
            (put (format nil "  Ways of editing the findings plausibly tell of, the most practised first; a careful ~
                              hand leaves fewer signs, so what is not found is not ruled out.~%") :list-dim))
          (loop for way in ways
                for (nil level name why some) = way
                do (put (format nil "- Level ~d, ~a~:[ (signs worth a look only)~;~]~%" level name (strong-way-p way))
                        (and (>= level 3) (strong-way-p way) :warning))
                   (put (format nil "    ~a~%" why) :list-note)
                   (dolist (f some)
                     (put (format nil "    ~a~%" (shorten-string (finding-words f) 110 :one-line t)) :list-note))))
        (let ((traits (analysis-traits analysis)))
          (heading "* Fingerprint")
          (put (format nil "  ~a~%"
                       (substitute-command-keys (format nil "What tells of who edited it and with what; ~
                                                             \\<palimpsest-mode>\\[palimpsest-save] writes it beside ~
                                                             the report, and \\[palimpsest-group-cases] groups the ~
                                                             files whose fingerprints share it."))) :list-dim)
          (if traits
              (dolist (trait traits) (put (format nil "  ~a~%" (trait-words trait))))
              (put (format nil "  Nothing.~%"))))
        ;; revisions
        (unless picture (heading "* Revisions"))
        (loop for revision across (if picture #() (pdf-revisions pdf))
              for account in (analysis-accounts analysis)
              for (file log why) in (analysis-files analysis)
              for k from 1
              do (heading (format nil "** ~@(~a~): ~a to ~a, ~:d octets" (revision-name revision)
                                  (hex-offset (revision-start revision)) (hex-offset (1- (revision-end revision)))
                                  (- (revision-end revision) (revision-start revision))))
                 (put (format nil "  ~12a" "Written as") :list-label)
                 (if file
                     (progn (put (native-file-name file) :link (list :revision k))
                            (put "  its log ")
                            (put (native-file-name log) :link (list :log k))
                            (put (string #\Newline)))
                     (put (format nil "~a~%" why) :warning))
                 (line "Index" (format nil "~a~@[ at ~a~]~@[, startxref at ~a~]"
                                       (or (xref-kind-words (revision-xref-kind revision))
                                           "none: objects found by scanning")
                                       (and (revision-xref-at revision) (hex-offset (revision-xref-at revision)))
                                       (and (revision-startxref-at revision)
                                            (hex-offset (revision-startxref-at revision)))))
                 (multiple-value-bind (info number) (info-of pdf revision)
                   (declare (ignore info))
                   (line "Trailer" (format nil "~{~a~^, ~}"
                                           (remove nil (list (let ((root (pdf-ref-p (pdf-get (revision-trailer revision)
                                                                                             "/Root"))))
                                                               (if root (format nil "/Root object ~d" root) "no /Root"))
                                                             (and number (format nil "/Info object ~d" number))
                                                             (let ((id (pdf-array-items
                                                                         (pdf-get (revision-trailer revision) "/ID"))))
                                                               (and id
                                                                    (every #'stringp id)
                                                                    (format nil "/ID <~{~a~^> <~}>"
                                                                            (mapcar #'id-hex id))))
                                                             (and (revision-prev revision)
                                                                  (format nil "/Prev ~a"
                                                                          (hex-offset (revision-prev revision)))))))))
                 (line "Claims"
                       (let ((made (info-text pdf revision "/CreationDate"))
                             (changed (info-text pdf revision "/ModDate"))
                             (producer (info-text pdf revision "/Producer")))
                         (and (or made changed producer)
                              (format nil "~@[made ~a~]~@[, changed ~a~]~@[, by ~a~]"
                                      (and made (pdf-date-words made))
                                      (and changed (pdf-date-words changed))
                                      producer))))
                 (dolist (part (list (list "Adds" (revision-added revision) revision)
                                     (list "Changes" (revision-changed revision) revision)
                                     (list "Frees"
                                           (revision-freed revision)
                                           (if
                                             (plusp (revision-index revision))
                                             (aref (pdf-revisions pdf) (1- (revision-index revision)))
                                             revision))))
                   (destructuring-bind (label numbers in) part
                     (when numbers
                       (put (format nil "  ~12a" label) :list-label)
                       (put (format nil "~:d object~:p: " (length numbers)))
                       (mapc #'put (object-pieces analysis in numbers))
                       (put (string #\Newline)))))
                 (dolist (problem (revision-problems revision)) (line "Not right" problem :warning))
                 (when (account-changes account)
                   (line "Pages"
                         (format nil "changed: ~{~d~^, ~}"
                                 (mapcar (lambda (c) (1+ (page-index (page-change-page c))))
                                         (account-changes account)))))
                 (dolist (change (account-changes account))
                   (let ((page (page-change-page change)))
                     (when (page-change-text change)
                       (put (format nil "  Page ~d, its text:~%" (1+ (page-index page))) :strong)
                       (loop for (kind text) in (page-change-text change) repeat 60
                             do (put (format nil "    ~:[-~;+~] ~a~%"
                                             (eq kind :added)
                                             (shorten-string text 120 :one-line t))
                                     (if (eq kind :added) :diff.plus :diff.minus))))
                     (when (page-change-annotations change)
                       (put
                         (format nil "  Page ~d, what is laid over it (annotations):~%" (1+ (page-index page)))
                         :strong)
                       (loop for (kind text) in (page-change-annotations change) repeat 40
                             do (put (format nil "    ~:[-~;+~] ~a~%"
                                             (eq kind :added)
                                             (shorten-string text 120 :one-line t))
                                     (if (eq kind :added) :diff.plus :diff.minus))))
                     (when (or (page-change-added change) (page-change-removed change))
                       (put (format nil "  Page ~d, what it draws:~%" (1+ (page-index page))) :strong)
                       (loop for m in (page-change-added change) repeat 40
                             do (put (format nil "    + ~a~%" (mark-words page m)) :diff.plus))
                       (loop for m in (page-change-removed change) repeat 40
                             do (put
                                  (format nil "    - ~a~%" (mark-words (or (page-change-old change) page) m))
                                  :diff.minus)))))
                 (dolist (page (account-gone account))
                   (put (format nil "  Page ~d of the revision before is gone.~%" (1+ (page-index page)))
                        :warning)))))))

(defun report-render (buffer analysis)
  "Show ANALYSIS in BUFFER, its report: its text and buttons, point at its start."
  (setf (buffer-local buffer 'analysis) analysis)
  (destructuring-bind (text spans buttons) (or (analysis-shown analysis)
                                               (multiple-value-list (pieces-text (shown-pieces analysis))))
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
written to a directory of its own, deleted when it is closed."
  (let ((name (native-file-name native)))
    (reading-buffer '*palimpsest-reports* (lambda (b) (buffer-local b 'palimpsest-path)) native
                    (format nil "*Palimpsest: ~a*" name)
                    (format nil "Examining ~a for signs of editing...~%" name)
                    'palimpsest-mode
                    (lambda (buffer)
                      (setf (buffer-local buffer 'palimpsest-path) native)
                      (start-job-thread
                        (make-palimpsest-job buffer native (buffer-private-directory buffer))
                        #'examine
                        :name "tuatara palimpsest"))
                    :private-directory "palimpsest/")))

(defun palimpsest-close-query (buffer)
  "Kill-buffer query: true to close BUFFER, a report on a PDF, at once when
its revisions and logs were saved (\\<palimpsest-mode>\\[palimpsest-save]) or none were written;
else asked first, as closing deletes them."
  (let ((analysis (buffer-local buffer 'analysis)))
    (or (null analysis) (analysis-saved analysis) (null (buffer-private-directory buffer))
        (notany #'first (analysis-files analysis))
        (yes-or-no-p* "Revisions and logs not saved: close and delete them?"))))

(defun palimpsest-forget (buffer)
  "Kill-buffer function: when BUFFER is a report on a PDF, close the buffers
showing the revisions and logs it wrote, as its directory is deleted with it."
  (let ((directory (and (buffer-local buffer 'palimpsest-path) (buffer-private-directory buffer))))
    (when directory
      (dolist (other (buffer-list))
        (let ((file (shown-file other)))
          (when (and file (not (eq other buffer)) (path-within-p (native file) directory))
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

(defun directory-refusal (analysis directory)
  "Why nothing of ANALYSIS's may be written in DIRECTORY, in words: it is the
directory of the PDF examined or a directory within it, or within palimpsest's own
directory in the cache, either told by where DIRECTORY really leads, through links
too, or a directory Tuatara writes nothing in, an evidence directory
\(WRITING-REFUSAL); NIL when it may."
  (flet ((real (path) (slash-ended (or (real-path-or-parent (string-right-trim "/" path)) path))))
    (let ((evidence (parent-path (analysis-path analysis)))
          (cache (private-cache-directory "palimpsest/")))
      (cond ((or (string= (slash-ended directory) (slash-ended evidence)) (string= (real directory) (real evidence)))
             "it is the directory of the PDF examined: nothing is written beside the evidence")
            ((or (path-within-p directory cache) (path-within-p (real directory) (real cache)))
             "it is within palimpsest's own directory in the cache, which is cleared")
            ((or (path-within-p directory evidence) (path-within-p (real directory) (real evidence)))
             "it is within the directory of the PDF examined: nothing is written there, nor a directory made")
            ((writing-refusal (string-right-trim "/" directory)))))))

(defun offered-path (analysis name)
  "Where NAME is offered to be written for ANALYSIS: in the directory its work
was last saved to, else in the home directory, never beside the PDF examined."
  (concatenate
    'string
    (if (analysis-saved analysis) (abbreviated-path (slash-ended (analysis-saved analysis))) "~/")
    name))

(defun target-refusal (analysis target &rest also)
  "Why nothing of ANALYSIS's may be written as TARGET, a native name, nor as
the files ALSO beside it, in words: TARGET's directory may not be written in
\(DIRECTORY-REFUSAL), or some of them are there already; NIL when they may."
  (or (directory-refusal analysis (parent-path target))
      (let ((there (loop for path in (cons target also)
                         when (path-kind path :follow nil) collect (native-file-name path))))
        (and there
             (format nil "~{~a~^, ~} ~:[is~;are~] there already: nothing is written over" there (rest there))))))

(defun save-refusal (analysis directory)
  "Why the revisions, logs and report of ANALYSIS cannot be saved to DIRECTORY,
in words; NIL when they can."
  (cond ((notany #'first (analysis-files analysis)) "no revision was written to save")
        ((and (path-kind directory) (not (eq :directory (path-kind directory)))) "it is not a directory")
        (t (apply #'target-refusal analysis
                  (loop for (nil name) in (saved-names analysis) collect (path-join (slash-ended directory) name))))))

(defun ask-write-target (analysis prompt name &key also)
  "Ask, with PROMPT, where to write NAME for ANALYSIS, offered where
OFFERED-PATH offers it: the native name given, or NIL when none is given or
nothing may be written there (TARGET-REFUSAL), why said. ALSO is a function
of the name giving the other files written beside it."
  (let ((target (read-new-native-path prompt :initial (offered-path analysis name) :writing t)))
    (when target
      (let ((refusal (apply #'target-refusal analysis target (and also (funcall also target)))))
        (if refusal
            (progn (message "Not written to ~a: ~a" (abbreviated-path target) refusal) nil)
            target)))))

(defun save-copies (analysis directory &key (cancel (list nil)))
  "Copy ANALYSIS's revisions and logs to DIRECTORY (made when it is not there),
and write the report there (REPORT-FILE-NAME), ending with where each copy is and
its SHA-256, each checked against its original, and the fingerprint beside it
\(FINGERPRINT-FILE-NAME), for \\[palimpsest-group-cases]. Returns what was saved, each
\(NAME SHA-256); an error naming the first copy whose sum differs."
  (let ((directory (slash-ended directory)) (saved '()))
    (ensure-directories directory)
    (loop for (source name sum) in (saved-names analysis)
          when source
            do (push (list name (copy-file-checked source (path-join directory name) :sum sum :cancel cancel)) saved))
    (let ((report (path-join directory (report-file-name analysis))))
      (write-new-file report
                       (with-output-to-string (s)
                         (write-string (report-text analysis) s)
                         (format s "~%* Saved~%~%Saved to ~a on ~a.~%Each copy's SHA-256, checked against its ~
                                    original:~%~%"
                                 directory (format-time "%F %T %z" (get-universal-time)))
                         (loop for (name sum) in (reverse saved) do (format s "  ~a  ~a~a~%" sum directory name))))
      (push (list (report-file-name analysis) (file-sha256 report :cancel cancel)) saved)
      (let ((print (path-join directory (fingerprint-file-name analysis))))
        (write-new-file print (fingerprint-text analysis))
        (push (list (fingerprint-file-name analysis) (file-sha256 print :cancel cancel)) saved)))
    (setf (analysis-saved analysis) directory)
    (nreverse saved)))

(defstruct (palimpsest-write-job
             (:include job)
             (:constructor make-palimpsest-write-job (buffer doing description verb left-words not-done finish))
             (:copier nil))
  "Writing files of a report for BUFFER, a job. DOING, DESCRIPTION, VERB and
LEFT-WORDS say what it does, as for any job (DEFINE-JOB-WORDS); RESULT is
what the work gave, NIL when it gave nothing. FINISH, a function of the job,
is called once it is done with a RESULT; else the words NOT-DONE are said,
with why."
  doing description verb left-words not-done finish result)

(setf (documentation 'palimpsest-write-job-p 'function) "True when OBJECT is a PALIMPSEST-WRITE-JOB.")

(define-job-words palimpsest-write-job
  :doing (palimpsest-write-job-doing job)
  :description (palimpsest-write-job-description job)
  :verb (palimpsest-write-job-verb job)
  :left-words (palimpsest-write-job-left-words job))

(defmethod job-take-in ((job palimpsest-write-job))
  "Say what was written, or why it was not."
  (if (palimpsest-write-job-result job)
      (funcall (palimpsest-write-job-finish job) job)
      (message "~a: ~a" (palimpsest-write-job-not-done job) (job-ending-words job))))

(defun save-job (buffer analysis directory)
  "A job saving the revisions, logs and report of ANALYSIS to DIRECTORY for
BUFFER (SAVE-COPIES), which says what was saved and shows the report again
with where."
  (make-palimpsest-write-job
   buffer "Saving the revisions" (format nil "Saving the revisions and logs to ~a" directory) "save the revisions"
   "some copies may have been made" "Not saved"
   (lambda (job)
     (message "Saved ~d files to ~a, each copy's SHA-256 checked against its original"
              (length (palimpsest-write-job-result job)) (abbreviated-path directory))
     (when (live-buffer-p buffer)
       (let ((point (buffer-point buffer)))
         (report-render buffer analysis)
         (setf (buffer-point buffer) (min point (buffer-size buffer))))))))

(define-command palimpsest-save ()
  "Save the revisions of the PDF the report is on, each with its log, and
the report itself as NAME-revision-report.txt, to a directory asked for (made when it is not
there): the work to keep, as the report's own copies go when it is closed.
Each copy's SHA-256 is checked against its original, and the report ends
with where each copy is and its sum; beside it NAME-fingerprint.txt, the
file's fingerprint, which \\[palimpsest-group-cases] sets beside others'. Never the PDF's own directory, nor the
package's own directory in the cache; a file there already is never written over: nothing is saved,
and which are there is said. Copied in the background."
  (let* ((buffer (current-buffer))
         (analysis (buffer-local buffer 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let ((directory (read-native-path "Save the revisions and logs to directory: " :writing t)))
          (when directory
            (let ((refusal (save-refusal analysis directory)))
              (if refusal
                  (message "Not saved to ~a: ~a" (abbreviated-path directory) refusal)
                  (start-analysis-job analysis (save-job buffer analysis directory)
                                      (lambda (job)
                                        (setf (palimpsest-write-job-result job)
                                              (save-copies analysis directory :cancel (job-cancel job)))
                                        ;; the report again, naming where: made here, not where the editor waits
                                        (setf (analysis-shown analysis)
                                              (multiple-value-list (pieces-text (shown-pieces analysis)))))
                                      "tuatara palimpsest saving"))))))))

(defun write-report (analysis target)
  "Write the report on ANALYSIS's PDF as text to TARGET, a native name not
there yet; returns its SHA-256."
  (write-new-file target (report-text analysis))
  (file-sha256 target))

(define-command palimpsest-write-report ()
  "Write the report as text to a file asked for, NAME-revision-report.txt
offered in the directory last saved to, else in the home directory; never beside
the PDF examined nor in the package's own directory, nothing written over. Says
its SHA-256."
  (let ((analysis (buffer-local (current-buffer) 'analysis)))
    (if (null analysis)
        (message "The report is not ready yet")
        (let ((target (ask-write-target analysis "Write the report to: " (report-file-name analysis))))
          (when target
            (message "Wrote the report to ~a, SHA-256 ~a" (abbreviated-path target) (write-report analysis target)))))))

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

(defun write-revision (analysis number target &key (cancel (list nil)))
  "Copy revision NUMBER of ANALYSIS's PDF to TARGET, and its log beside it
\(LOG-BESIDE), each copy's SHA-256 checked against its original. Returns the
PDF copy's SHA-256; an error when a sum differs."
  (multiple-value-bind (file log sum) (revision-files analysis number)
    (ensure-directories target)
    (prog1 (copy-file-checked file target :sum sum :cancel cancel)
      (copy-file-checked log (log-beside target) :cancel cancel))))

(defun revision-job (report analysis number target)
  "A job writing revision NUMBER of ANALYSIS's PDF as TARGET with its log
beside it, for the buffer REPORT (WRITE-REVISION), which opens the revision
written in the PDF view and says what it is."
  (make-palimpsest-write-job
   report "Writing a revision" (format nil "Writing revision ~d to ~a" number target) "write the revision"
   "a part of it may have been written" (format nil "Revision ~d was not written" number)
   (lambda (job)
     (visit-path target)
     (message "Wrote ~a: ~a as its revision ~d of ~d was, SHA-256 ~a, its log beside it~:[~;; ~a on it, then ~a, goes ~
               back further~]"
              (abbreviated-path target) (analysis-name analysis) number (length (analysis-files analysis))
              (palimpsest-write-job-result job) (> number 1)
              (substitute-command-keys "\\[palimpsest]")
              (substitute-command-keys "\\<palimpsest-mode>\\[palimpsest-write-revision]")))))

(defun revision-meant ()
  "The report and the revision the current buffer points at, as two values:
in a report, the report and the revision whose part point is in (or NIL);
in a revision opened from a report, that report and that revision's number."
  (let ((buffer (current-buffer)))
    (if (buffer-local buffer 'analysis)
        (let ((revision (report-revision-at (buffer-local buffer 'analysis))))
          (values buffer (and revision (1+ revision))))
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
the directory last saved to, else in the home directory; never beside the PDF
examined nor in the package's own directory, nothing written over; each copy's SHA-256 checked, and the PDF
opened when it is written. That PDF is the
file as it stood after that save: \\[palimpsest] on it finds the saves before."
  (multiple-value-bind (report meant) (revision-meant)
    (let* ((analysis (and report (buffer-local report 'analysis)))
           (count (and analysis (length (analysis-files analysis)))))
      (cond ((null analysis) (message "Not a report of palimpsest, nor a revision opened from one: ~a there first"
                                      (substitute-command-keys "\\[palimpsest]")))
            ((picture-refused-p analysis "write"))
            (t (let ((number (or meant
                                 (read-whole-number (format nil "Revision to write (1-~d)~~@[ (~~d)~~]: " count)
                                                    (max 1 (1- count))
                                                    :valid (lambda (n) (<= 1 n count))
                                                    :refusal (format nil "Not a revision: 1 to ~d." count)))))
                 (cond ((null number))
                       ((null (revision-files analysis number))
                        (message
                          "Revision ~d of ~a could not be written out, so it cannot be copied"
                          number
                          (analysis-name analysis)))
                       (t (let ((target (ask-write-target analysis (format nil "Write revision ~d to: " number)
                                                          (format nil "~a-revision-~d.pdf"
                                                                  (native-file-stem (analysis-name analysis)) number)
                                                          :also (lambda (target) (list (log-beside target))))))
                            (when target
                              (start-job-thread (revision-job report analysis number target)
                                                (lambda (job)
                                                  (setf (palimpsest-write-job-result job)
                                                        (write-revision analysis number target
                                                                        :cancel (job-cancel job))))
                                                :name "tuatara palimpsest revision")))))))))))

;;; Objects shown

(defun object-stream-name (analysis index number)
  "The name of the buffer showing in hex the stream of the object NUMBER as
the revision INDEX (from 0) of ANALYSIS's PDF has it."
  (format nil "*Palimpsest: ~a object ~d stream, revision ~d*" (analysis-name analysis) number (1+ index)))

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
where it is, what it is, its dictionary as written and its stream decoded,
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
            (if (pdf-indirect-get object :packed-in)
                (multiple-value-bind (pairs data) (packed-objects pdf
                                                                  (revision-object
                                                                    pdf
                                                                    revision
                                                                    (pdf-indirect-get object :packed-in)))
                  (declare (ignore pairs))
                  (format s "~a~%"
                          (octets-latin-1 data
                                          :start (pdf-indirect-get object :at)
                                          :end (min (length data)
                                                    (pdf-indirect-get object :end)
                                                    (+ (pdf-indirect-get object :at) 65536)))))
                (format s "~a~%" (octets-latin-1 octets :start (pdf-indirect-get object :at)
                                                        :end (min (or (pdf-indirect-get object :stream-start)
                                                                      (pdf-indirect-get object :end))
                                                                  (+ (pdf-indirect-get object :at) 65536)))))
            (when (pdf-indirect-get object :stream-start)
              (multiple-value-bind (data why) (stream-data pdf object)
                (format s "~%Its stream: ~:d octets in the file, ~a to ~a; filters: ~a; ~:[~:d octets ~
                           decoded~;~:*~a~*~].~%~%"
                        (- (pdf-indirect-get object :stream-end) (pdf-indirect-get object :stream-start))
                        (hex-offset (pdf-indirect-get object :stream-start))
                        (hex-offset (pdf-indirect-get object :stream-end))
                        (filter-names object)
                        (and why (if
                                   (member why *pdf-picture-filters* :test #'equal)
                                   (format nil "picture data (~a), not decoded" why)
                                   why))
                        (length (or data #())))
                (when data
                  (let ((end (min (length data) *palimpsest-largest-shown*)))
                    (if (or hex (binary-head-p data end))
                        (format s "In hex: ~a~%" (object-stream-name analysis (revision-index revision) number))
                        (format s "~a~%" (octets-latin-1 data :end end)))
                    (when (< end (length data))
                      (format s "~%... ~:d octets more (*palimpsest-largest-shown*)~%" (- (length data) end))))))))))))

(defun reread-analysis (path sum)
  "The file at PATH read again for work on it after it was examined, as an
ANALYSIS with its PDF read (READ-PDF), and for a picture file its picture
\(READ-PICTURE-FILE, its pixels kept) and the page that stands for it; its
findings are not looked for again. An error when its SHA-256 is no longer
SUM (SOURCE-OCTETS)."
  (let* ((octets (source-octets path sum))
         (name (native-file-name path))
         (analysis (make-analysis :path path :name name :size (length octets) :sha256 sum))
         (kind (octets-picture-kind octets)))
    (if kind
        (let ((file (read-picture-file octets kind)))
          (setf (analysis-picture analysis) file
                (analysis-pdf analysis) (read-pdf (picture-page-octets (max 1 (picture-file-width file))
                                                                       (max 1 (picture-file-height file)))
                                                  :path path :name name)))
        (setf (analysis-pdf analysis) (read-pdf octets :path path :name name)))
    analysis))

(defun revision-at (analysis index)
  "The revision INDEX (from 0) of ANALYSIS's PDF; an error when it has none."
  (let ((revisions (pdf-revisions (analysis-pdf analysis))))
    (unless (and (integerp index) (< -1 index (length revisions)))
      (error "~a has no revision ~a" (analysis-name analysis) index))
    (aref revisions index)))

(defun revision-words (analysis index)
  "The revision INDEX (from 0) of the editor's ANALYSIS in words, as examining
named it: revision 1 (the original) and so on."
  (or (nth index (analysis-revisions analysis)) (format nil "revision ~d" (1+ index))))

(defun object-shut-in (paths directory &key settings sha256 revision number hex)
  "Read the object NUMBER of the file, the first of PATHS, examined with the
SHA-256 SHA256, as its revision REVISION (from 0) has it, as the work shut in
does: the text showing it (OBJECT-TEXT, HEX as there) written to object.txt
in DIRECTORY, and when that names its stream shown in hex, the stream as far
as shown to the file stream. SETTINGS as SHUT-IN carries them."
  (call-with-settings
   settings
   (lambda ()
     (let* ((analysis (reread-analysis (first paths) sha256))
            (at (revision-at analysis revision))
            (text (object-text analysis at number :hex hex)))
       (write-new-file (path-join directory "object.txt") text)
       (when (search (format nil "In hex: ~a" (object-stream-name analysis revision number)) text)
         (let ((stream (object-stream-shown analysis at number)))
           (when stream (write-new-file (path-join directory "stream") stream))))
       t))))

(defstruct (palimpsest-object-job (:include job)
                                  (:constructor make-palimpsest-object-job (buffer analysis revision number hex at))
                                  (:copier nil))
  "Reading the object NUMBER as the revision REVISION (from 0) of ANALYSIS's
PDF has it, for BUFFER, the report, a job: TEXT, the text showing it
\(OBJECT-TEXT), and STREAM, its stream decoded as far as shown when that is
shown in hex, once read. HEX and AT as SHOW-OBJECT takes them."
  analysis revision number hex at text stream)

(setf (documentation 'palimpsest-object-job-p 'function) "True when OBJECT is a PALIMPSEST-OBJECT-JOB.")

(define-job-words palimpsest-object-job
  :doing "Reading the object"
  :description (format nil "Reading object ~d of ~a" (palimpsest-object-job-number job)
                       (analysis-name (palimpsest-object-job-analysis job)))
  :verb "read the object"
  :left-words "no object shown")

(defun object-work (job)
  "What a job reading an object does (PALIMPSEST-OBJECT-JOB): the object read
in a Tuatara of its own shut in (OBJECT-SHUT-IN), and its text, and its
stream when that is shown in hex, read back."
  (let* ((analysis (palimpsest-object-job-analysis job)) (number (palimpsest-object-job-number job))
         (largest *palimpsest-largest-shown*))
    (with-work-directory (directory)
      (shut-in 'object-shut-in (list (analysis-path analysis)) directory
               (format nil "Reading object ~d of ~a" number (analysis-name analysis))
               :arguments (list :sha256 (analysis-sha256 analysis) :revision (palimpsest-object-job-revision job)
                                :number number :hex (palimpsest-object-job-hex job)))
      (let ((text (shut-in-octets directory "object.txt" (+ largest (* 1024 1024)))))
        (setf (palimpsest-object-job-stream job) (shut-in-octets directory "stream" largest)
              (palimpsest-object-job-text job) (and text (octets-string text)))))))

(defmethod job-take-in ((job palimpsest-object-job))
  "Show the object read (SHOW-OBJECT-TEXT)."
  (when (palimpsest-object-job-text job)
    (show-object-text (palimpsest-object-job-analysis job) (palimpsest-object-job-revision job)
                      (palimpsest-object-job-number job) (palimpsest-object-job-text job)
                      (palimpsest-object-job-stream job) (palimpsest-object-job-at job))))

(defun show-object (analysis revision number &key hex at)
  "Show the object NUMBER as the revision REVISION (from 0) of ANALYSIS's PDF
has it in a buffer of its own, read only, as help is shown (OBJECT-TEXT),
read in the background first; its stream, when it is shown in hex, in a
buffer of its own too, the object's naming it as a link. With AT, a place
in its decoded stream shown as text, the cursor there. Not while a job reads
the PDF (ANALYSIS-BUSY-P)."
  (start-analysis-job analysis
                      (make-palimpsest-object-job (current-buffer) analysis revision number hex at)
                      #'object-work "tuatara palimpsest object"))

(defun show-object-text (analysis revision number text stream at)
  "Show TEXT, the object NUMBER as the revision REVISION (from 0) of
ANALYSIS's PDF has it (OBJECT-TEXT), as SHOW-OBJECT does: STREAM, its stream
decoded as far as shown when TEXT names it shown in hex, AT a place in it
shown as text."
  (let* ((name (object-stream-name analysis revision number))
         (hex-at (search (format nil "In hex: ~a" name) text))
         (show-stream (lambda () (show-octets-in-hex name stream)))
         (buffer-name (format nil "*Palimpsest: ~a object ~d, revision ~d*" (analysis-name analysis) number
                              (1+ revision))))
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
    (cond
      ((null analysis) (message "The report is not ready yet"))
      ((picture-refused-p analysis "show"))
      (t
        (let* ((number (read-whole-number "Object number~@[ (~d)~]: "
                                          (number-at-point (current-buffer))
                                          :valid (lambda (n) (>= n 0))
                                          :refusal "Not an object number."))
               (revision (or (report-revision-at analysis) (1- (length (analysis-revisions analysis))))))
          (when number
            (show-object analysis revision number :hex (prefix-argument))))))))

(defun report-revision-at (analysis)
  "The index (from 0) of the revision whose part of the report point is in,
or NIL when it is in none: told by the report's own headings before point,
never by what the PDF says, which the report quotes."
  (let* ((buffer (current-buffer))
         (point (buffer-point buffer))
         (count (length (analysis-revisions analysis)))
         (in-revisions nil) (found nil))
    (loop for (start end face) in (second (analysis-shown analysis))
          while (< start point)
          when (eq face :heading)
            do (let ((heading (string-trim '(#\Newline) (buffer-string buffer :start start :end end))))
                 (cond ((string= heading "* Revisions") (setf in-revisions t found nil))
                       ((uiop:string-prefix-p "** " heading)
                        (let ((k (and in-revisions (uiop:string-prefix-p "** Revision " heading)
                                      (parse-integer heading :start 12 :junk-allowed t))))
                          (setf found (and k (<= 1 k count) (1- k)))))
                       (t (setf in-revisions nil found nil)))))
    found))

;;; Asking for a PDF

(defun examinable-p (path)
  "True when the file at PATH, a native name, is one palimpsest examines: a
PDF, or a PNG, JPEG or TIFF picture (*PICTURE-KINDS*), told by its first
octets whatever its name."
  (or (pdf-file-p path) (picture-kind-of path)))

(define-command palimpsest ()
  "Examine a PDF, or a picture (PNG, JPEG or TIFF), for signs of editing,
from its octets alone. For a PDF: every revision from the original to the
latest, what each added, changed and freed and how its pages changed, and
every sign of editing or forgery that examiners know, each with its
evidence. A picture is examined as one scanned page, as a PDF's are, with
what it says of itself (EXIF, XMP, its comments and text) and, for a JPEG,
whether it was saved twice. The file the current buffer shows when it is
one of these (a PDF's or picture's view, a visited file, a file on rimu's
line), else, or with \\[universal-argument], one asked for. The report is made in the
background. Nothing in the file is run and the file is never changed; each
revision of a PDF is written, with a log of how to find its signs by hand,
to a directory for you alone."
  (let* ((shown (and (not (prefix-argument)) (shown-file (current-buffer))))
         (path (if
                 (and shown (examinable-p (native (expand-path shown))))
                 shown
                 (read-file-path "PDF or picture to examine: " :note nil))))
    (when path
      (let ((native (native (expand-path path))))
        (cond
          ((not (eq :regular (ignore-errors (path-kind native))))
           (message "~a is not a file" (abbreviated-path native)))
          ((not (examinable-p native))
           (message "~a is neither a PDF nor a PNG, JPEG or TIFF picture, by its first octets"
                    (abbreviated-path native)))
          (t (show-buffer (palimpsest-buffer native))))))))
