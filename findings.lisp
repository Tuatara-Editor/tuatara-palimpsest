;;;; findings.lisp --- The signs of a PDF's editing, each with its evidence and how to see it by hand.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each check looks for one kind of sign that examiners of documents know:
;;;; in how the file is put together, in what it says of itself, in what its
;;;; pages draw, in its pictures, annotations, form and signatures. A sign
;;;; found is a FINDING: how strong a sign it is, one plain sentence of what
;;;; it means, the revision, page and objects it is in, the rule that found
;;;; it, the evidence (offsets and the octets themselves, in hex and as text)
;;;; and the steps that show it again with a hex editor alone. A check that
;;;; cannot finish on a malformed file says so as a finding of its own.

(in-package #:palimpsest)

(defstruct (finding (:constructor found (strength words &key rule details evidence steps revision page objects method
                                                     tally check kind place))
                    (:copier nil))
  "A sign found: STRENGTH :strong (a strong sign of editing), :look (worth a
look) or :record (for the record). WORDS, one plain sentence of what it means.
RULE, the rule that found it, in one sentence. DETAILS, more lines for the
report. EVIDENCE, lines naming the offsets and octets it rests on. STEPS,
lines saying how to see it by hand. REVISION, the index of the revision it is
in. PAGE, the page's number from 1, or NIL. OBJECTS, the object numbers it
concerns. METHOD, the key in *METHODS* of the way of editing it plausibly
comes from, or NIL for one that tells of none. TALLY, what the report's
Checks table counts of it, (NUMBER UNIT PAGES): NUMBER of UNIT, a noun
such as character or region, or for UNIT :dpi, the resolution in dots
an inch (dpi) it found; PAGES, every page it holds for, numbers from 1;
each part NIL when not known. CHECK, the key in *CHECKS* of the check that
found it, set by RUN-CHECK. KIND, the kind of sign it is when honest files
show that kind too (*HONEST-KINDS*), or a list of such kinds; NIL for a
kind honest files are not known to show. PLACE, :FIELD when the sign lies
on a place worth changing, :PROSE when it lies in running text or
boilerplate (PLACE-WORTH-CHANGING), NIL when not known."
  strength words rule details evidence steps revision page objects method tally check kind place)

(setf (documentation 'finding-p 'function) "True when OBJECT is a FINDING.")
(setf (documentation 'found 'function) "A new FINDING of STRENGTH saying WORDS, its other parts given by their names.")

(defun unfinished (name condition)
  "The finding that the check of NAME could not finish on this file, failing with CONDITION."
  (found :record (format nil "The check of ~a could not finish on this file: ~a"
                         name (shorten-string (princ-to-string condition) 200 :one-line t))
         :rule "A malformed file is read as far as it makes sense."))

(defmacro guarding-item ((findings format &rest arguments) &body body)
  "Run BODY, the check of one item of a file; when it fails on what the file
holds, push onto the place FINDINGS the finding that the check of the item
FORMAT and ARGUMENTS name could not finish (UNFINISHED), so that one
malformed item leaves the findings of the others standing. Returns BODY's
value, or FINDINGS with that finding pushed."
  (let ((condition (gensym "CONDITION")))
    `(handler-case (progn ,@body)
       ((or error storage-condition) (,condition)
         (push (unfinished (format nil ,format ,@arguments) ,condition) ,findings)))))

(defparameter *strengths* '(:strong :look :record)
  "The strengths of findings, the strongest first.")

(defparameter *honest-files-measured* 1445
  "How many honest files the counts of *HONEST-KINDS* were measured on.")

(defparameter *honest-kinds*
  '((:words-ink 314 "a word in another ink"
     "a form filled in or a stamp in a second ink, bold type, a line printed in another pass")
    (:words-edges 168 "a word with sharper or softer edges"
     "bold or thin type, a line printed in another pass, focus falling off across a photograph")
    (:words-paper 54 "a word on cleaner or noisier paper" "a shaded row, a stain or a fold, light falling unevenly")
    (:words-baseline 485 "a word off its line by its baseline"
     "a heading, a superscript or a figure in another size, a curled or tilted page")
    (:words-character 248 "a character off its line" "a curled or tilted page, a second JPEG save, italic type")
    (:words-turn 41 "a word turned against its line" "a curled page, a stamp or handwriting at a slant")
    (:words-both 17 "a word unlike its line both in how it looks and where it sits"
     "a stamp or a form's second ink printed off the line")
    (:near-pairs 203 "characters nearer one another than prints"
     "clean prints on a page kept as a JPEG, a second JPEG save, a scanner's software smoothing its pages")
    (:no-grain 59 "words or marks without grain of their own"
     "text a scanner's software stamped, a page whitened all over, a picture drawn by a program")
    (:layout 83 "text set apart from its line or its edge"
     "a heading or a footnote, a table, a program setting text so")
    (:hidden 11 "text never seen" "specks of a text recognition layer, a program's leftovers off the page")
    (:piece-off-grid 25 "a region lacking the page's JPEG blocks"
     "a scanner's software stamping a date or a number after the page was saved")
    (:piece-own 1 "a region carrying JPEG blocks of its own" "a stamp, a date or a logo laid on the page by software")
    (:font-subsets 1 "plain text of a line in a second subset of its font"
     "a form put together from parts made apart, pages joined")
    (:figure-passing 2 "a figure in another font at its line's size"
     "a year, a section number or a count a program sets in another font")
    (:dates-before-made 5 "a file changed before it was made" "a clock set wrong, a time zone written wrong")
    (:metadata-removed 2 "what the file says of itself taken out" "a program writing its own account anew on saving")
    (:info-xmp 2 "the document information and the XMP disagreeing"
     "a program that brings only one of the two up to date")
    (:typed-on-scan 5 "visible text over a scanned page"
     "a scanner's software or a court's filing system writing a stamp, a page number or a header")
    (:page-changed 4 "a page changed in a later save"
     "a form filled in, notes or a signature added and saved, a publisher correcting pages")
    (:page-made-picture 2 "a page drawn as one picture by a program" "a page printed to a picture, a screenshot")
    (:page-size 7 "a page of another size" "an attachment or a form of another paper size joined in")
    (:picture-added 1 "a picture added in a later save" "a signature or a stamp added honestly and saved")
    (:picture-metadata 4 "a picture keeping metadata of its own" "a logo or a photograph made elsewhere and placed")
    (:marks-twice 1 "a mark repeated on the page" "a stamp or a logo printed twice"))
  "Each kind of sign worth a look that honest files show too, (KIND FILES
NAME CAUSES): FILES, how many of the *HONEST-FILES-MEASURED* honest files
of the run of 2026-11-23 show it at all, worth a look or noted; NAME, the
sign in a few words; CAUSES, its known honest causes.
doc/measures.org (Honest files worth a look) says how they were counted. A
finding of such a kind says so in a sentence of its own (HONEST-SENTENCE);
a kind not here, which no honest file measured showed, says nothing more.")

(defun honest-kind (kind)
  "The entry of *HONEST-KINDS* for KIND, or NIL."
  (assoc kind *honest-kinds*))

(defun honest-sentence (kinds)
  "The sentence saying how often the honest files measured show the KINDS
of a finding (a kind of *HONEST-KINDS*, or a list of them) and their known
honest causes; NIL when none is a kind honest files show."
  (let ((entries (remove nil (mapcar #'honest-kind (if (listp kinds) kinds (list kinds))))))
    (when entries
      (format nil "Honest files show this too: of ~:d measured, ~{~{~:d show ~a~}~^, ~}; known honest causes: ~
                   ~{~a~^; ~}."
              *honest-files-measured*
              (mapcar (lambda (e) (list (second e) (third e))) entries)
              (remove-duplicates (mapcar #'fourth entries) :test #'string= :from-end t)))))

(defun weigh-finding (finding)
  "FINDING, its words given the sentence of how often honest files show its
kind (HONEST-SENTENCE) when it is worth a look, or was noted for where it
lies, and its kind is one honest files show. Returns FINDING."
  (let ((sentence (and (or (eq (finding-strength finding) :look) (eq (finding-place finding) :prose))
                       (honest-sentence (finding-kind finding)))))
    (when (and sentence (not (search sentence (finding-words finding))))
      (setf (finding-words finding) (format nil "~a ~a" (finding-words finding) sentence)))
    finding))

(defun look-rank (finding)
  "Where FINDING, worth a look, stands among the others: 0 for a kind honest
files are not known to show, 1 for a sign on a place worth changing, 2 for
the rest."
  (cond ((notany #'honest-kind (let ((kinds (finding-kind finding))) (if (listp kinds) kinds (list kinds)))) 0)
        ((eq (finding-place finding) :field) 1)
        (t 2)))

;;; Evidence

(defun offset-words (n)
  "N, an offset, in hex and in decimal: 0x0000012A (298)."
  (format nil "~a (~:d)" (hex-offset n) n))

(defun hex-words (octets start end &optional (most 96))
  "The octets of OCTETS from START below END in hex, at most MOST of them, ... after more."
  (let ((end (min end (length octets))))
    (format nil "~a~:[~; ...~]"
            (octets-hex octets :start start :end (min end (+ start most)) :upper-case t :separator " ")
            (> (- end start) most))))

(defun text-words (octets start end &optional (most 200))
  "The octets of OCTETS from START below END as text, a character each, those
not printable as a full stop, at most MOST of them."
  (let ((end (min end (length octets))))
    (format nil "~a~:[~;...~]"
            (map 'string
                 (lambda (o) (if (<= 32 o 126) (code-char o) #\.))
                 (subseq octets start (min end (+ start most))))
            (> (- end start) most))))

(defun octets-evidence (label octets start end &key decoded)
  "Lines of evidence for the octets of OCTETS from START below END, called
LABEL: where they are (in the file, or with DECODED, in the decoded octets of
the stream it names), in hex and as text."
  (list (format nil "~a: ~:[at~;~:*in ~a, at~] ~a to ~a" label decoded (offset-words start) (offset-words end))
        (format nil "  hex:  ~a" (hex-words octets start end))
        (format nil "  text: ~a" (text-words octets start end))))

(defun filter-names (object)
  "The filters of OBJECT's stream, in words."
  (let ((filters (mapcar #'car (pdf-stream-filters (pdf-indirect-get object :value)))))
    (if filters (format nil "~{~a~^ then ~}" filters) "none")))

(defun stream-steps (pdf object)
  "The steps to reach the decoded octets of OBJECT's stream by hand."
  (let ((octets (pdf-octets pdf)))
    (if (pdf-indirect-get object :packed-in)
        (list (format nil "Object ~d is kept in object stream ~d: decode that first."
                      (pdf-indirect-get object :number)
                      (pdf-indirect-get object :packed-in)))
        (append
         (list (format nil "Go to offset ~a: the object begins ~s." (offset-words (pdf-indirect-get object :at))
                       (text-words octets
                                   (pdf-indirect-get object :at)
                                   (min (length octets) (+ (pdf-indirect-get object :at) 24 ))
                                   24)))
         (when (pdf-indirect-get object :stream-start)
           (list (format nil "Its stream's data runs from ~a to ~a, ~:d octets (its dictionary gives /Length ~a); its ~
                              filters: ~a."
                         (offset-words (pdf-indirect-get object :stream-start))
                         (offset-words (pdf-indirect-get object :stream-end))
                         (- (pdf-indirect-get object :stream-end) (pdf-indirect-get object :stream-start))
                         (let ((l (pdf-indirect-get object :length)))
                           (if (pdf-ref-p l) (format nil "~d ~d R" (second l) (third l)) l))
                         (filter-names object))
                 (if (pdf-stream-filters (pdf-indirect-get object :value))
                     (format nil "Undo the filters on those octets (for /FlateDecode, inflate them as zlib data) to ~
                                  have the decoded stream.")
                     "The data is not filtered: it is read as it stands.")))))))

(defun object-evidence (pdf revision number)
  "Lines of evidence for where the object NUMBER, as REVISION has it, sits,
and the steps to find it, as two values."
  (let ((object (revision-object pdf revision number)))
    (cond
      ((null object)
       (values (list (format nil "Object ~d is not in revision ~d." number (1+ (revision-index revision)))) '()))
      ((pdf-indirect-get object :packed-in)
       (let ((stream (revision-object pdf revision (pdf-indirect-get object :packed-in))))
         (values (list (format nil "Object ~d ~d is kept in object stream ~d~@[ (at ~a)~], at offset ~a of its decoded ~
                                    octets."
                               number (pdf-indirect-get object :generation) (pdf-indirect-get object :packed-in)
                               (and stream (offset-words (pdf-indirect-get stream :at)))
                               (offset-words (pdf-indirect-get object :at))))
                 (append (and stream (stream-steps pdf stream))
                         (list (format nil "In the decoded object stream, go to offset ~a: object ~d's value begins ~
                                            there."
                                       (offset-words (pdf-indirect-get object :at)) number))))))
      (t (let ((written (revision-of-offset pdf (pdf-indirect-get object :at))))
           (values (list (format nil "Object ~d ~d begins at ~a and ends at ~a~@[, written in revision ~d~]."
                                 number
                                 (pdf-indirect-get object :generation)
                                 (offset-words (pdf-indirect-get object :at))
                                 (offset-words (pdf-indirect-get object :end))
                                 (and written (1+ (revision-index written)))))
                   (stream-steps pdf object)))))))

(defun key-evidence (pdf revision number key)
  "Lines of evidence for the entry KEY of the object NUMBER's dictionary, as
REVISION has it: where the key and its value are, in hex and as text; and
the steps to find them, as two values. NIL when it has no such entry."
  (let ((object (revision-object pdf revision number)))
    (when object
      (multiple-value-bind (octets start end label)
          (if (pdf-indirect-get object :packed-in)
              (let ((stream (revision-object pdf revision (pdf-indirect-get object :packed-in))))
                (multiple-value-bind (pairs data) (and stream (packed-objects pdf stream))
                  (declare (ignore pairs))
                  (values data (pdf-indirect-get object :at) (min (length (or data #())) (pdf-indirect-get object :end))
                          (format nil "the decoded object stream ~d" (pdf-indirect-get object :packed-in)))))
              (values (pdf-octets pdf)
                      (pdf-indirect-get object :at)
                      (or (pdf-indirect-get object :stream-start) (pdf-indirect-get object :end))
                      nil))
        (when octets
          (let ((at (octets-search (string-octets key) octets :start start :end end)))
            (when (and at (< (+ at (length key)) (length octets))
                       (= (nth-value 1 (pdf-object-at octets at)) (+ at (length key))))
              (let ((value-end (nth-value 1 (pdf-object-at octets (+ at (length key))))))
                (multiple-value-bind (place steps) (object-evidence pdf revision number)
                  (values (append place
                                  (octets-evidence (format nil "~a of object ~d" key number)
                                                   octets
                                                   at
                                                   value-end
                                                   :decoded label))
                          (append steps (list (format nil "~:[In the file~;~:*In ~a~], at offset ~a, read ~a and its ~
                                                           value: ~a."
                                                      label
                                                      (offset-words at)
                                                      key
                                                      (text-words octets at value-end))))))))))))))

(defun mark-evidence (pdf revision mark &optional (label "it"))
  "Lines of evidence for MARK, a thing a page draws: the operators that draw
it, where they are in the decoded stream, in hex and as text; and the
steps to find them, as two values."
  (let* ((source (mark-source mark))
         (object (and source (revision-object pdf revision source)))
         (data (and object (stream-data pdf object))))
    (if (not (and data (mark-from mark) (mark-to mark)))
        (values (list (format nil "~@(~a~) is drawn by ~:[an inline part~;~:*object ~d's stream~]." label source)) '())
        (multiple-value-bind (place steps) (object-evidence pdf revision source)
          (values (append place
                          (octets-evidence (format nil "The operators drawing ~a" label)
                                           data
                                           (mark-from mark)
                                           (mark-to mark)
                                           :decoded (format nil "the decoded stream of object ~d" source))
                          (let ((colour (mark-colour-at mark)))
                            (and colour
                                 (octets-evidence (format nil "The operator setting its colour")
                                                  data
                                                  (car colour)
                                                  (cdr colour)
                                                  :decoded (format nil "the decoded stream of object ~d" source)))))
                  (append steps
                          (list (format nil "In the decoded stream, go to offset ~a: ~s."
                                        (offset-words (mark-from mark))
                                        (text-words data (mark-from mark) (mark-to mark))))))))))

;;; Words of a revision

(defun revision-name (revision)
  "REVISION in words: revision 1 (the original), revision 2, revision 3 (the
current file), the last save being the file as it is now."
  (format nil "revision ~d~a" (1+ (revision-index revision))
          (let ((original (zerop (revision-index revision))) (latest (revision-latest revision)))
            (cond ((and original latest) " (the original, the current file)")
                  (original " (the original)")
                  (latest " (the current file)")
                  (t "")))))

(defun info-of (pdf revision)
  "REVISION's document information, as two values: its dictionary (or NIL) and its object number."
  (let* ((value (pdf-get (revision-trailer revision) "/Info"))
         (info (deref pdf revision value)))
    (values (and (pdf-dictionary-p info) info) (pdf-ref-p value))))

(defun info-text (pdf revision key)
  "The text of KEY in REVISION's document information, or NIL."
  (let ((v (dref pdf revision (info-of pdf revision) key)))
    (and (stringp v) (plusp (length v)) v)))

;;; All of them

(defun sorted-findings (findings last)
  "FINDINGS, each placed in a revision (LAST, the latest, when it is of the
file as a whole) and weighed (WEIGH-FINDING), the strongest first; those
worth a look by LOOK-RANK, kinds honest files are not known to show first,
then signs on places worth changing; then by revision and page."
  (dolist (f findings)
    (unless (finding-revision f) (setf (finding-revision f) (revision-index last)))
    (weigh-finding f))
  (stable-sort findings
               (lambda (a b)
                 (let ((sa (position (finding-strength a) *strengths*))
                       (sb (position (finding-strength b) *strengths*)))
                   (cond ((/= sa sb) (< sa sb))
                         ((and (eq (finding-strength a) :look) (/= (look-rank a) (look-rank b)))
                          (< (look-rank a) (look-rank b)))
                         ((/= (finding-revision a) (finding-revision b))
                          (< (finding-revision a) (finding-revision b)))
                         (t (< (or (finding-page a) 0) (or (finding-page b) 0))))))))
