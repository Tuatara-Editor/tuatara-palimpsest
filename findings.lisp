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

(defstruct (finding (:constructor found (strength words &key rule details evidence steps revision page objects method))
                    (:copier nil))
  "A sign found: STRENGTH :strong (a strong sign of editing), :look (worth a
look) or :record (for the record). WORDS, one plain sentence of what it means.
RULE, the rule that found it, in one sentence. DETAILS, more lines for the
report. EVIDENCE, lines naming the offsets and octets it rests on. STEPS,
lines saying how to see it by hand. REVISION, the index of the revision it is
in. PAGE, the page's number from 1, or NIL. OBJECTS, the object numbers it
concerns. METHOD, the key in *METHODS* of the way of editing it plausibly
comes from, or NIL for one that tells of none."
  strength words rule details evidence steps revision page objects method)

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

;;; XMP

(defun xmp-of (pdf revision)
  "REVISION's XMP metadata, as three values: its fields (each (NAME . VALUE)),
its tree, and its object number; NIL when it has none or it cannot be read.
Read once for each metadata object, however many revisions share it."
  (let* ((value (pdf-get (catalogue pdf revision) "/Metadata"))
         (object (and (pdf-ref-p value) (revision-object pdf revision (second value)))))
    (when object
      (let ((kept (or (getf (pdf-cache pdf) :xmp) (setf (getf (pdf-cache pdf) :xmp) (make-hash-table :test 'eq)))))
        (values-list
         (multiple-value-bind (read there) (gethash object kept)
           (if there
               read
               (setf (gethash object kept)
                     (multiple-value-bind (data why) (stream-data pdf object :limit (* 16 1024 1024))
                       (let ((tree (and data (null why) (ignore-errors (xml-parse (octets-string data) :most 100000)))))
                         (and tree (list (ignore-errors (xmp-fields tree)) tree (second value)))))))))))))

(defun xmp-get (fields local)
  "The value of the XMP field whose name without its prefix is LOCAL among FIELDS, or NIL."
  (let ((pair (find-if (lambda (f) (string= (xml-unprefixed (car f)) local)) fields)))
    (and pair (stringp (cdr pair)) (plusp (length (cdr pair))) (cdr pair))))

(defun xmp-history (tree)
  "The saves an XMP TREE's edit history (xmpMM:History) lists, each an alist
of (LOCAL-NAME . VALUE): action, when, softwareAgent, instanceID, changed."
  (let ((steps '()))
    (labels ((elements (node) (remove-if-not #'consp (xml-children node)))
             (text (node)
               (string-trim '(#\Space #\Newline #\Tab #\Return)
                            (format nil "~{~a~}" (remove-if-not #'stringp (xml-children node)))))
             (item (li)
               (let ((fields (loop for (name . value) in (second li)
                                   unless (search "rdf:" name) collect (cons (xml-unprefixed name) value))))
                 (dolist (child (elements li))
                   (if (string= (xml-local-name child) "Description")
                       (setf fields (append fields (cdr (item child))))
                       (push (cons (xml-local-name child) (text child)) fields)))
                 (cons :step fields))))
      (dolist (history (xml-find-all tree "History" :local t))
        (dolist (list (elements history))
          (dolist (li (xml-local-children list "li"))
            (push (cdr (item li)) steps)))))
    (nreverse steps)))

(defun date-of (text)
  "TEXT, a date as a PDF or XMP writes it, as two values: the universal time and its zone in minutes, or NIL."
  (multiple-value-bind (time zone) (iso-date-time text)
    (if time
        (values time zone)
        (multiple-value-bind (words time zone) (pdf-date-words text)
          (declare (ignore words))
          (values time zone)))))

(defun date-local-fields (text)
  "The parts TEXT, a date as a PDF (D:20130328210040-06'00') or XMP
\(2013-03-28T21:00:40-06:00) writes it, gives of the time where it was
written, as far as it gives them: a list of the year, then month, day, hour,
minute and second; NIL when it begins with no year."
  (let* ((text (string-trim " " (or text "")))
         (text (if (and (>= (length text) 2) (string= (subseq text 0 2) "D:")) (subseq text 2) text)))
    (flet ((number-at (start end)
             (and (<= end (length text))
                  (every #'digit-char-p (subseq text start end))
                  (parse-integer text :start start :end end))))
      (if (and (> (length text) 4) (char= (char text 4) #\-))
          ;; XMP's form: each part after its separator, as far as they go
          (loop for (start end separator) in '((0 4 nil) (5 7 #\-) (8 10 #\-) (11 13 #\T) (14 16 #\:) (17 19 #\:))
                for value = (and (or (null separator)
                                     (and (> start 0)
                                          (< (1- start) (length text))
                                          (char-equal (char text (1- start)) separator)))
                                 (number-at start end))
                while value collect value)
          ;; the PDF's: two digits a part after the year's four
          (loop for (start end) in '((0 4) (4 6) (6 8) (8 10) (10 12) (12 14))
                for value = (number-at start end)
                while value collect value)))))

(defun dates-apart (a b)
  "How far apart the dates A and B are, in seconds (each as a PDF or XMP
writes it), compared as finely as the coarser of the two is written. Two
given to the minute or finer are compared by their universal times, their
zones allowed for. One given only to the day (or more coarsely) is compared
by the parts both give, the finer read where it was written or in universal
time, whichever is nearer. So 2013-03-28 and D:20130328210040-06'00' are
the same day, 0 apart. NIL when either cannot be read. As a second value,
how many parts were compared when the coarser gives fewer than five (the
year alone 1, and the month 2, the day 3), else NIL."
  (let ((fa (date-local-fields a)) (fb (date-local-fields b)))
    (cond ((or (null fa) (null fb)) nil)
          ((and (>= (length fa) 5) (>= (length fb) 5))
           (let ((ta (date-of a)) (tb (date-of b))) (and ta tb (abs (- ta tb)))))
          (t (let ((n (min (length fa) (length fb))))
               (labels ((moment (fields)
                          (destructuring-bind (year &optional (month 1) (day 1) (hour 0) (minute 0) (second 0))
                              (subseq fields 0 n)
                            (ignore-errors
                              (encode-universal-time second
                                                     minute
                                                     hour
                                                     (max 1 (min day 31))
                                                     (max 1 (min month 12))
                                                     year
                                                     0))))
                        (universal-fields (text)
                          ;; the finer one's parts in universal time, as some programs write the coarser
                          (let ((time (date-of text)))
                            (and time
                                 (multiple-value-bind (second minute hour day month year) (decode-universal-time time 0)
                                   (list year month day hour minute second)))))
                        (apart (x y)
                          (let ((mx (and x (moment x))) (my (and y (moment y)))) (and mx my (abs (- mx my))))))
                 (let ((local (apart fa fb))
                       (universal (if (> (length fa) n)
                                      (apart (universal-fields a) fb)
                                      (apart fa (universal-fields b)))))
                   (values (if (and local universal) (min local universal) (or local universal)) n))))))))

;;; Tools that make and edit PDFs

(defparameter *pdf-tools*
  '(("ilovepdf" "iLovePDF" :editor "an online service for editing PDFs")
    ("smallpdf" "Smallpdf" :editor "an online service for editing PDFs")
    ("sejda" "Sejda" :editor "a PDF editor, online and on the desktop")
    ("pdfescape" "PDFescape" :editor "an online PDF editor")
    ("pdf-xchange" "PDF-XChange" :editor "a PDF editor")
    ("foxit" "Foxit" :editor "a PDF reader and editor")
    ("nitro" "Nitro" :editor "a PDF editor")
    ("pdfelement" "PDFelement" :editor "a PDF editor (Wondershare)")
    ("wondershare" "Wondershare" :editor "the maker of the PDFelement editor")
    ("soda pdf" "Soda PDF" :editor "a PDF editor")
    ("pdftk" "pdftk" :editor "a program that joins, splits and changes PDFs")
    ("qpdf" "qpdf" :editor "a program that rewrites PDFs")
    ("pypdf" "PyPDF" :editor "a library for changing PDFs from Python")
    ("pdfsam" "PDFsam" :editor "a program that joins and splits PDFs")
    ("pdf24" "PDF24" :editor "a set of PDF tools, online and on the desktop")
    ("xodo" "Xodo" :editor "a PDF editor")
    ("pdf expert" "PDF Expert" :editor "a PDF editor")
    ("canva" "Canva" :editor "an online design service")
    ("inkscape" "Inkscape" :editor "a drawing program that opens and saves PDFs")
    ("photoshop" "Adobe Photoshop" :editor "a picture editor")
    ("gimp" "GIMP" :editor "a picture editor")
    ("mutool" "MuPDF tools" :editor "a program that rewrites PDFs")
    ("bluebeam" "Bluebeam" :editor "a PDF editor")
    ("power pdf" "Kofax Power PDF" :editor "a PDF editor")
    ("acrobat" "Adobe Acrobat" :editor "Adobe's PDF editor")
    ("ghostscript" "Ghostscript" :maker "a program that makes and rewrites PDFs")
    ("libreoffice" "LibreOffice" :maker "an office suite")
    ("openoffice" "OpenOffice" :maker "an office suite")
    ("microsoft: print to pdf" "Microsoft Print to PDF" :maker "Windows' printing to PDF")
    ("quartz" "Quartz" :maker "macOS's own PDF writer, used by Preview and printing")
    ("preview" "Preview" :editor "macOS's viewer, which can change PDFs")
    ("itext" "iText" :maker "a library that makes and changes PDFs")
    ("reportlab" "ReportLab" :maker "a library that makes PDFs")
    ("aspose" "Aspose" :maker "a library that makes and changes PDFs")
    ("pdfium" "PDFium" :maker "the PDF library of Chrome")
    ("skia" "Skia" :maker "the drawing library of Chrome, which prints to PDF")
    ("distiller" "Acrobat Distiller" :maker "Adobe's program that makes PDFs from print files")
    ("pdfmaker" "Acrobat PDFMaker" :maker "Adobe's add-in that makes PDFs from office documents")
    ("microsoft" "Microsoft Office" :maker "an office program")
    ("docusign" "DocuSign" :maker "a signing service"))
  "Programs that make or change PDFs, as they name themselves: each (TEXT NAME
KIND WORDS), TEXT what they write (in lower case), KIND :editor for one used
to change PDFs, :maker for one that makes them.")

(defun tools-in (text)
  "The entries of *PDF-TOOLS* TEXT names, the first for each name; none for
NIL. An entry named only within another named entry's name is that one's:
\"Acrobat Distiller\" names Distiller, not Adobe Acrobat; \"Acrobat
Distiller, then Adobe Acrobat Pro\" names both."
  (when text
    (let* ((text (string-downcase text))
           (named (remove-duplicates (remove-if-not (lambda (tool) (search (first tool) text)) *pdf-tools*)
                                     :key #'second :test #'string= :from-end t)))
      (flet ((places (words)
               (loop for at = (search words text) then (search words text :start2 (1+ at))
                     while at collect at)))
        (remove-if (lambda (tool)
                     (let ((own (first tool)))
                       (every (lambda (at)
                                (some (lambda (other)
                                        (let ((name (string-downcase (second other))))
                                          (and (not (eq other tool))
                                               (some (lambda (from)
                                                       (<= from at (+ at (length own)) (+ from (length name))))
                                                     (places name)))))
                                      named))
                              (places own))))
                   named)))))

(defun program-names (pdf)
  "The programs each save of PDF names, from the first save, each (PLACE
REVISION TEXT): PLACE is where, \"/Producer\" or \"/Creator\" of its document
information, or \"XMP Producer\" or \"XMP CreatorTool\" of its XMP."
  (kept-for-pdf (pdf :program-names)
    (loop for r across (pdf-revisions pdf)
          append (let ((fields (xmp-of pdf r)))
                   (loop for (place text) in (list (list "/Producer" (info-text pdf r "/Producer"))
                                                   (list "/Creator" (info-text pdf r "/Creator"))
                                                   (list "XMP Producer" (and fields (xmp-get fields "Producer")))
                                                   (list "XMP CreatorTool" (and fields (xmp-get fields "CreatorTool"))))
                         when (stringp text) collect (list place r text))))))

(defun file-tools (pdf)
  "The entries of *PDF-TOOLS* that any save of PDF names in its document
information's producer or creator, or its XMP's producer or creator tool."
  (kept-for-pdf (pdf :file-tools)
    (remove-duplicates (loop for (nil nil text) in (program-names pdf) append (tools-in text))
                       :key #'second :test #'string=)))

(defun ids-alike-p (pdf revision)
  "True when REVISION's trailer gives no /ID, or gives its two parts alike,
as a program writes them when it makes a file; a program writing the file
again changes the second."
  (let ((id (pdf-array-items (pdf-get (revision-trailer revision) "/ID"))))
    (or (/= (length id) 2) (not (stringp (first id))) (not (stringp (second id)))
        (string= (first id) (second id)))))

(defparameter *pdf-rewriters*
  '("mupdf" "mutool" "qpdf" "pikepdf" "pypdf" "itext" "pdfbox" "hexapdf" "pdf-lib" "pdfsharp" "pdftk" "ghostscript"
    "libharu" "podofo" "reportlab" "fpdf" "tcpdf" "pdfkit" "pdfcpu" "unipdf" "aspose" "spire.pdf" "apryse" "pdftron")
  "Words naming a program or library that can write a PDF anew from another,
as found in lower case in a file's producer, creator or comments. Any of
them named takes away that a shape over text was drawn as the file was made
\(DRAWN-AS-MADE-P). A file rewritten by one of them may have had a shape
added before it was written.")

(defun rewriter-named-p (pdf)
  "True when a save of PDF names, in its producer or creator, its XMP's
producer or creator tool, or a comment between its objects
\(FILE-COMMENTS), a program of *PDF-REWRITERS*."
  (kept-for-pdf (pdf :rewriter-named)
    (let ((texts (append (mapcar #'third (program-names pdf)) (mapcar #'third (file-comments pdf)))))
      (some (lambda (text)
              (and (stringp text)
                   (let ((text (string-downcase text))) (some (lambda (w) (search w text)) *pdf-rewriters*))))
            texts))))

(defparameter *maker-trailer-marks*
  '(("libreoffice" "/DocChecksum") ("openoffice" "/DocChecksum"))
  "Marks a program that makes PDFs writes in the trailer of every file it
makes, each (WORDS KEY): a file whose first save names WORDS as its
producer and whose trailer lacks KEY, or whose KEY does not hold
\(MAKER-MARK-HOLDS-P), was written again by another program or changed
after. LibreOffice and OpenOffice write /DocChecksum, the MD5 digest of the
file's octets up to its trailer.")

(defun maker-mark-holds-p (pdf key)
  "True when the mark KEY in the trailer of PDF's first save holds: for
/DocChecksum, the MD5 digest (OCTETS-MD5) of the file's octets from its start
up to the keyword trailer after the first save's cross-reference table."
  (let* ((first (aref (pdf-revisions pdf) 0))
         (value (pdf-get (revision-trailer first) key)))
    (cond ((null value) nil)
          ((string= key "/DocChecksum")
           (let* ((octets (pdf-octets pdf))
                  (from (or (revision-xref-at first) 0))
                  (at (and (integerp from) (<= 0 from (length octets))
                           (octets-search (load-time-value (string-octets "trailer")) octets :start from))))
             (and at (stringp value) (= (length value) 33)
                  (string-equal (subseq value 1)
                                (let ((cache (pdf-cache pdf)))
                                  (or (getf cache :doc-checksum)
                                      (setf (getf (pdf-cache pdf) :doc-checksum) (octets-md5 octets :end at))))))))
          (t t))))

(defun maker-marks-missing-p (pdf)
  "True when PDF's first save names a program of *MAKER-TRAILER-MARKS* as
its producer and its trailer lacks that program's mark, or the mark does
not hold (MAKER-MARK-HOLDS-P)."
  (kept-for-pdf (pdf :maker-marks-missing)
    (let* ((first (aref (pdf-revisions pdf) 0))
           (producer (string-downcase (or (info-text pdf first "/Producer") ""))))
      (loop for (words key) in *maker-trailer-marks*
            thereis (and (search words producer) (not (maker-mark-holds-p pdf key)))))))

(defun structure-mcids (pdf revision page)
  "Whether REVISION is a tagged PDF (its catalogue has a structure tree or
says it is marked), and the numbers of PAGE's marked content its structure
tree names (/MCID, directly or as a marked-content reference), in a table;
the tree followed 100,000 entries at most. Worked out once for each
revision and page."
  (let ((kept (kept-for-pdf (pdf :structure-mcids) (make-hash-table :test 'equal)))
        (key (cons (revision-index revision) (page-index page))))
    (values-list
     (or (gethash key kept)
         (setf (gethash key kept)
               (multiple-value-list
                 (let* ((catalogue (catalogue pdf revision))
                        (root (and catalogue (deref pdf revision (pdf-get catalogue "/StructTreeRoot"))))
                        (marked (let ((m (deref pdf revision (pdf-get catalogue "/MarkInfo"))))
                                  (pdf-true-p (pdf-get m "/Marked"))))
                        (out (make-hash-table)) (seen (make-hash-table)) (budget 100000)
                        (number (page-number page)))
                   (labels ((walk (value pg)
                              (when (plusp (decf budget))
                                (cond ((pdf-ref-p value)
                                       (unless (gethash (second value) seen)
                                         (setf (gethash (second value) seen) t)
                                         (walk (deref pdf revision value) pg)))
                                      ((integerp value) (when (and pg number (eql pg number))
                                                          (setf (gethash value out) t)))
                                      ((pdf-dictionary-p value)
                                       (let ((pg (or (pdf-ref-p (pdf-get value "/Pg")) pg)))
                                         (let ((mcid (pdf-get value "/MCID")))
                                           (when (and (integerp mcid) pg number (eql pg number))
                                             (setf (gethash mcid out) t)))
                                         (walk (pdf-get value "/K") pg)))
                                      ((consp value) (dolist (item (pdf-array-items value)) (walk item pg)))))))
                     (when root (walk (pdf-get root "/K") nil)))
                   (values (or (and root t) marked) out))))))))

(defparameter *replacement-size* 0.05
  "How near in size, as a share of the hidden text's, the own text of a box
over text is taken to be set as the hidden text is (REPLACEMENT-P), room for
a size rounded where it is written.")

(defparameter *replacement-baseline* 1.0
  "How near, in points, the baseline of a box's own text is taken to lie to
the hidden text's line (REPLACEMENT-P): new text set over old lies on the old
line, where an office program sets a text box's own text inside its
margins, further off.")

(defun replacement-p (own hidden)
  "True when the run of text OWN is set as the text HIDDEN is, each (RUN FROM
TO), a run with the part of it, as fractions along its baseline, that is
OWN's or hidden. It has the same font family, a size within
*REPLACEMENT-SIZE* of it, its baseline within *REPLACEMENT-BASELINE* points
of its line, and the middle of OWN within the hidden part along it. This is
new text set in place of old, as an editor changing a figure does."
  (destructuring-bind (run from to) hidden
    (let ((frame (mark-frame run)) (own-frame (mark-frame own)))
      (and frame own-frame (mark-font own) (mark-font run)
           (equal (font-family (mark-font own)) (font-family (mark-font run)))
           (< (abs (- (mark-size own) (mark-size run))) (* *replacement-size* (max (mark-size run) 1)))
           (let* ((ax (aref frame 2)) (ay (aref frame 3)) (length (sqrt (+ (* ax ax) (* ay ay)))))
             (and (plusp length)
                  (let ((nx (- (/ ay length))) (ny (/ ax length))
                        (dx (- (aref own-frame 0) (aref frame 0))) (dy (- (aref own-frame 1) (aref frame 1))))
                    (<= (abs (+ (* dx nx) (* dy ny))) *replacement-baseline*))
                  (multiple-value-bind (mx my) (run-middle own)
                    (let ((along (/ (+ (* (- mx (aref frame 0)) ax) (* (- my (aref frame 1)) ay)) (* length length))))
                      (<= from along to)))))))))

(defun file-as-written-p (pdf revision)
  "True when REVISION of PDF reads as the program that wrote it left it: its
/ID as its maker wrote it (IDS-ALIKE-P), no program named that writes PDFs
anew, its comments too (REWRITER-NAMED-P), its maker's trailer marks there
and holding (MAKER-MARKS-MISSING-P), and no program named that edits PDFs
\(FILE-TOOLS)."
  (and (ids-alike-p pdf revision)
       (not (rewriter-named-p pdf))
       (not (maker-marks-missing-p pdf))
       (not (find :editor (file-tools pdf) :key #'third))))

(defun box-tagging (pdf revision page box own)
  "How the structure tree of PAGE in REVISION of PDF names the box BOX and
its own text OWN: :TAGGED when the page is tagged and the tree names the
marked content of each (STRUCTURE-MCIDS), :UNTAGGED for an untagged page or
none given, NIL when the page is tagged and one of them is outside it, as a
box appended to a page is."
  (if page
      (multiple-value-bind (tagged mcids) (structure-mcids pdf revision page)
        (cond ((not tagged) :untagged)
              ((every (lambda (m) (and (mark-mcid m) (gethash (mark-mcid m) mcids))) (cons box own)) :tagged)))
      :untagged))

(defun block-alone-p (mark marks keep-p)
  "True when MARK is drawn in an outermost q ... Q block (MARK-BLOCK) holding
no other of MARKS drawn by its content stream but those KEEP-P is true of."
  (and (mark-block mark)
       (every (lambda (other)
                (or (eq other mark)
                    (funcall keep-p other)
                    (not (eql (mark-source other) (mark-source mark)))
                    (not (eql (mark-block other) (mark-block mark)))))
              marks)))

(defun cells-alone-p (box own covered marks)
  "True when the box BOX and its own text OWN are drawn in an outermost
q ... Q block of their own, over the text COVERED each in a block of its own
too, among the page's MARKS, as LibreOffice Calc draws each cell and each
shape."
  (and (every (lambda (m) (eql (mark-block m) (mark-block box))) own)
       (block-alone-p box marks (lambda (m) (member m own)))
       (every (lambda (c) (block-alone-p c marks (lambda (m) (not (eq (mark-kind m) :text))))) covered)))

(defun own-text-as-made-p (own covered hidden alone)
  "True when OWN, a box's own text, is set in fonts kept in the file of the
family of the text COVERED (FONT-FAMILY), in any font kept in the file when
ALONE (CELLS-ALONE-P), and none of it is set as the hidden text is over the
part hidden (REPLACEMENT-P, HIDDEN giving each covered run with the part of
it hidden, (RUN FROM TO)): that is new text in place of old, not a text
box's own."
  (let ((families (loop for m in covered when (mark-font m) collect (font-family (mark-font m)))))
    (and (every (lambda (m) (and (mark-font m) (font-embedded (mark-font m))
                                 (or alone (member (font-family (mark-font m)) families :test #'equal))))
                own)
         (notany (lambda (m) (some (lambda (h) (replacement-p m h)) hidden)) own))))

(defun drawn-as-made-p (pdf revision box covered within &optional page hidden)
  "True when the filled shape BOX was drawn over the text COVERED as the PDF
was made, not laid on it later: a text box, as an office program prints one
with a white fill over a spreadsheet's cell. It has text of its own drawn on
it after it, of WITHIN, the texts whose middle it covers, set as a text box's
own is (OWN-TEXT-AS-MADE-P, HIDDEN giving the parts of COVERED hidden). BOX,
all of COVERED and its own text are drawn by one content stream, written in
the original save, within the outermost q ... Q block the hidden text is
drawn in (MARK-BLOCK). The file reads as its maker wrote it
\(FILE-AS-WRITTEN-P) and names a program that makes PDFs. On a tagged PAGE
the structure tree names the box and its text (BOX-TAGGING); there they may
be drawn in a block of their own over covered text in blocks of its own
\(CELLS-ALONE-P), and set in another font kept in the file. A box laid over
text by a PDF editor fails one of these: it comes in a later save, in a
content stream of its own, with an editor named, the /ID written again, or
its text in a font of the editor's own. A forger who passes them all still
leaves the finding naming the text hidden under the box, worth a look."
  (let* ((own (remove-if-not
                (lambda (m) (and (> (mark-order m) (mark-order box)) (eql (mark-source m) (mark-source box))))
                within))
         (hidden (or hidden (mapcar (lambda (run) (list run 0 1)) covered)))
         (tagging (box-tagging pdf revision page box own))
         (alone (and (eq tagging :tagged) (cells-alone-p box own covered (marks-of pdf revision page))))
         (blocks (remove nil (mapcar #'mark-block covered))))
    (and own (mark-source box) tagging (not (later-source-p pdf revision box))
         (own-text-as-made-p own covered hidden alone)
         (file-as-written-p pdf revision)
         (find :maker (file-tools pdf) :key #'third)
         (or (null blocks) alone
             (every (lambda (m) (and (mark-block m) (member (mark-block m) blocks))) (cons box own)))
         (every (lambda (m) (eql (mark-source m) (mark-source box))) covered)
         t)))

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
      (push (found :look (format nil "The file was changed after it was first saved: ~d later save~:p ~
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
  "What does not make sense in each revision's cross-reference section, trailer and page tree."
  (loop for revision across (pdf-revisions pdf)
        append (loop for problem in (remove-duplicates (revision-problems revision) :test #'string=)
                     for web = (search "prepared for the web disagree" problem)
                     collect (found (if (or web (search "startxref" problem) (search "no section" problem))
                                        :strong
                                        :look)
                                    (format nil "In ~a, ~a: the file was edited by hand, repaired, or made by a ~
                                                 careless program." (revision-name revision) problem)
                                    :rule (if web
                                              (format nil "A file prepared for the web (linearised) is saved once in ~
                                                           two parts: the first page's cross-reference section, whose ~
                                                           trailer names the rest's by /Prev, and the rest's. Its ~
                                                           linearisation dictionary's /L gives the length of the file, ~
                                                           no object is listed in both, and the startxref before the ~
                                                           last %%EOF names the first page's section, as every program ~
                                                           that writes such files writes them. Parts that disagree let ~
                                                           readers show another document than the two read together: ~
                                                           one part's object in place of the other's.")
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
  "Cross-reference entries that do not point at the object they name."
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
                 (push (found :strong (format nil "In ~a, ~d cross-reference entr~:@p ~:*~[~;does~:;do~] not point at ~
                                                   the object ~:*~[~;it names~:;they name~]: the file was edited by ~
                                                   hand or repaired."
                                      (revision-name revision) (length wrong))
                              :rule (concatenate 'string "Each entry of a cross-reference section gives the offset "
                                                         "where its object's NUMBER GENERATION obj begins.")
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
      (push (found :look (format nil "~d object~:p in the file ~:*~[~;is~:;are~] no longer used by the document: ~
                                      leftovers of earlier content, or kept to be used later."
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

(defun check-ends (pdf)
  "Data after the last save, and before the PDF begins. Data after the last
%%EOF is strong at 64 octets or more besides spaces and line ends, or with
PDF words among it, else worth a look: fewer than 64 octets of no PDF words
hold no hidden file, but any is named."
  (let* ((octets (pdf-octets pdf)) (last (last-revision pdf)) (end (revision-end last)) (out '())
         (rest-count (count-if-not #'pdf-white-p octets :start end)))
    (when (plusp rest-count)
      (let ((suspect (some (lambda (word) (octets-search (string-octets word) octets :start end))
                           '("obj" "%PDF" "xref" "stream" "trailer"))))
        (push (found (if (or suspect (>= rest-count 64)) :strong :look)
                     (format nil "~:d octet~:p follow~:[s~;~] the last %%EOF~:[~;, among them PDF objects or another ~
                                  file~]: data hidden after the end, or a save cut short."
                             (- (length octets) end) (/= (- (length octets) end) 1) suspect)
                     :rule "A PDF ends with %%EOF and a line end; readers pass over anything after it."
                     :revision (revision-index last)
                     :evidence (octets-evidence "After the last %%EOF" octets end (length octets))
                     :steps (list (format nil "Go to offset ~a, just after the last %%EOF line, and read to the end of ~
                                               the file." (offset-words end))) :method :structure-by-hand)
              out)))
    (when (plusp (pdf-header pdf))
      (push (found :look (format nil "~:d octet~:p come before %PDF-: another file's data, or a file made to be read ~
                                      as two kinds." (pdf-header pdf))
                   :rule "A PDF begins with %PDF-."
                   :revision 0
                   :evidence (octets-evidence "Before %PDF-" octets 0 (pdf-header pdf))
                   :steps (list "Read the file from offset 0 to where %PDF- begins.") :method :structure-by-hand)
            out))
    out))

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
      (push (found :look (format nil "The file was prepared for fast web viewing, then changed by a later save, which ~
                                      undoes the linearisation.")
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

(defun check-lengths (pdf)
  "Streams whose /Length disagrees with where endstream is."
  (let ((wrong '()) (last (last-revision pdf)))
    (loop for o across (pdf-found pdf)
          when (pdf-indirect-get o :stream-start)
            do (let ((length (deref pdf last (pdf-indirect-get o :length))))
                 (when (and (integerp length)
                            (/= length (- (pdf-indirect-get o :stream-end) (pdf-indirect-get o :stream-start))))
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

;;; What the file says of itself

(defstruct (metadata-reading (:constructor make-metadata-reading (pdf last info info-number fields tree xmp-number))
                             (:copier nil) (:predicate nil))
  "What the checks of a PDF's metadata read from its LAST revision: its
document information INFO, the object INFO-NUMBER holding it, the XMP
FIELDS and TREE, and the object XMP-NUMBER holding them."
  pdf last info info-number fields tree xmp-number)

(defun info-evidence (reading key)
  "The evidence of KEY in the document information of READING."
  (let ((number (metadata-reading-info-number reading)))
    (and number (key-evidence (metadata-reading-pdf reading) (metadata-reading-last reading) number key))))

(defun xmp-evidence (reading local)
  "The evidence of the XMP field LOCAL of READING: its object, and the
decoded octets about where the name first stands."
  (let* ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
         (number (metadata-reading-xmp-number reading))
         (object (and number (revision-object pdf last number)))
         (data (and object (stream-data pdf object)))
         (at (and data (octets-search (string-octets local) data))))
    (and at (append (object-evidence pdf last number)
                    (octets-evidence (format nil "~a in the XMP" local) data (max 0 (- at 8))
                                     (min (length data) (+ at 80))
                                     :decoded (format nil "the decoded stream of object ~d" number))))))

(defun metadata-disagreements (reading)
  "The findings of READING's document information and XMP giving other
dates, programs or titles: dates more than a minute apart, strongly when
more than a day; dates alike only as far as the coarser is written, for the
record."
  (let* ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
         (fields (metadata-reading-fields reading))
         (objects (remove nil (list (metadata-reading-info-number reading) (metadata-reading-xmp-number reading))))
         (out '()))
    (loop for (key local words) in '(("/CreationDate" "CreateDate" "creation dates")
                                    ("/ModDate" "ModifyDate" "dates last changed")
                                    ("/Producer" "Producer" "producing programs")
                                    ("/Creator" "CreatorTool" "creating programs")
                                    ("/Title" "title" "titles"))
          for a = (info-text pdf last key)
          for b = (xmp-get fields local)
          when (and a b)
            do (multiple-value-bind (apart parts) (and (search "Date" key) (dates-apart a b))
                 ;; compared only to the month or the year: said, for the examiner to weigh
                 (when (and apart parts (<= parts 2) (<= apart 60))
                   (push (found :record (format nil "The document information and the XMP metadata give their ~a only ~
                                                     as alike as the coarser is written, to the ~:[year~;month~]: ~s ~
                                                     and ~s."
                                                words (= parts 2) a b)
                                :rule (format nil "Dates are compared as finely as the coarser of the two is written; ~
                                                   one given only to the month or the year agrees with any day of it.")
                                :revision (revision-index last) :objects objects
                                :evidence (append (info-evidence reading key) (xmp-evidence reading local)))
                         out))
                 (when (if (search "Date" key) (and apart (> apart 60))
                           (string/= (string-trim " " a) (string-trim " " b)))
                   (push (found (if (and apart (> apart 86400)) :strong :look)
                                (format nil "The document information and the XMP metadata give different ~a: ~s and ~
                                             ~s; one was changed without the other." words a b)
                                :rule (format nil "A program saving a PDF writes the same dates, programs and title in ~
                                                   its document information and its XMP metadata.")
                                :revision (revision-index last) :objects objects
                                :evidence (append (info-evidence reading key) (xmp-evidence reading local))
                                :method :metadata-edited)
                         out))))
    (nreverse out)))

(defun metadata-date-findings (reading where created changed created-evidence changed-evidence)
  "The findings of the dates CREATED and CHANGED that WHERE gives, with
their evidence: changed more than a minute before made, a date more than a
day in the future, a time zone no place uses, made and changed in two
zones."
  (let ((revision (revision-index (metadata-reading-last reading))) (now (get-universal-time)) (out '()))
    (multiple-value-bind (ct cz) (date-of created)
      (multiple-value-bind (mt mz) (date-of changed)
        (when (and ct mt (< mt (- ct 60)))
          (push (found :strong (format nil "In ~a the file was last changed (~a) before it was made (~a): a date was ~
                                            set by hand." where (utc-time-words mt) (utc-time-words ct))
                       :rule "A file cannot be changed before it was made."
                       :revision revision :evidence (append created-evidence changed-evidence)
                       :method :metadata-edited)
                out))
        (loop for (time evidence) in (list (list ct created-evidence) (list mt changed-evidence))
              when (and time (> time (+ now 86400)))
                do (push (found :strong (format nil "~@(~a~) gives a date in the future, ~a: a clock set wrong, or a ~
                                                     date set by hand." where (utc-time-words time))
                                :rule "No date a file gives can be after today."
                                :revision revision :evidence evidence :method :metadata-edited)
                         out))
        (loop for (zone text evidence) in (list (list cz created created-evidence) (list mz changed changed-evidence))
              when (and zone (or (/= 0 (mod zone 15)) (< zone (* -12 60)) (> zone (* 14 60))))
                do (push (found :look (format nil "~@(~a~) gives a time zone no place uses (~s): a date written by ~
                                                   hand or by an odd program." where text)
                                :rule "Time zones are whole quarters of an hour, from 12 hours behind UTC to 14 ahead."
                                :revision revision :evidence evidence :method :metadata-edited)
                         out))
        (when (and cz mz (/= cz mz))
          (push (found :record (format nil "In ~a the file was made in one time zone (~a minutes from UTC) and changed ~
                                            in another (~a)." where cz mz)
                       :rule "A file made and changed on one computer gives one time zone."
                       :revision revision :evidence (append created-evidence changed-evidence))
                out))))
    (nreverse out)))

(defun metadata-dates (reading)
  "The findings of the dates READING's document information and XMP give
\(METADATA-DATE-FINDINGS), each place on its own."
  (let ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
        (fields (metadata-reading-fields reading)))
    (append (metadata-date-findings reading "the document information"
                                    (info-text pdf last "/CreationDate") (info-text pdf last "/ModDate")
                                    (info-evidence reading "/CreationDate") (info-evidence reading "/ModDate"))
            (metadata-date-findings reading "the XMP metadata"
                                    (xmp-get fields "CreateDate") (xmp-get fields "ModifyDate")
                                    (xmp-evidence reading "CreateDate") (xmp-evidence reading "ModifyDate")))))

(defun metadata-history (reading)
  "The findings of READING's XMP edit history and of the documents it says
the file was made or derived from."
  (let* ((last (metadata-reading-last reading)) (fields (metadata-reading-fields reading))
         (tree (metadata-reading-tree reading))
         (history (and tree (xmp-history tree)))
         (document (xmp-get fields "DocumentID")) (original (xmp-get fields "OriginalDocumentID"))
         (derived (find-if (lambda (f) (string= (xml-unprefixed (car f)) "DerivedFrom")) fields))
         (out '()))
    (when history
      (let ((tools (tools-in (format nil "~{~a~^ ~}" (loop for step in history
                                                           collect (or (cdr (assoc "softwareAgent" step
                                                                                   :test #'string=))
                                                                       ""))))))
        (push (found (if (some (lambda (tool) (eq (third tool) :editor)) tools) :look :record)
                     (format nil "The XMP metadata keeps an edit history of ~d step~:p~@[, by ~{~a~^, ~}~]."
                             (length history) (mapcar #'second tools))
                     :rule "Programs that keep XMP add a step to xmpMM:History each time they save the file."
                     :revision (revision-index last) :objects (list (metadata-reading-xmp-number reading))
                     :details (loop for step in history
                                    collect (format nil "~{~a~^, ~}" (loop for (k . v) in step
                                                                          collect (format nil "~a ~a" k v))))
                     :evidence (xmp-evidence reading "History") :method :pdf-editor)
              out)))
    (when (and document original (string/= document original))
      (push (found :look (format nil "The XMP metadata says the document was made from another one (original ~a, now ~
                                      ~a)." original document)
                   :rule (format nil "xmpMM:OriginalDocumentID names the document a file was first made as; a ~
                                      different xmpMM:DocumentID means it became another.")
                   :revision (revision-index last) :evidence (xmp-evidence reading "OriginalDocumentID")
                   :method :other-program)
            out))
    (when derived
      (push (found :look (format nil "The XMP metadata says the document was derived from another: ~a." (cdr derived))
                   :rule "xmpMM:DerivedFrom names the document a file was made from."
                   :revision (revision-index last) :evidence (xmp-evidence reading "DerivedFrom")
                   :method :other-program)
            out))
    (nreverse out)))

(defun metadata-programs (reading)
  "The findings of the programs READING's producer and creator name: each
that edits PDFs, and Acrobat saving again what another program made."
  (let* ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
         (producer (info-text pdf last "/Producer")) (creator (info-text pdf last "/Creator"))
         (out '()))
    (loop for (place revision text) in (program-names pdf)
          when (eq revision last)
            do (dolist (tool (tools-in text))
               (when (eq (third tool) :editor)
                 (push (found :look (format nil "~a names ~a, ~a: the file passed through a program that edits PDFs."
                                            place (second tool) (fourth tool))
                              :rule (format nil "The programs a file names in its producer and creator, as ~
                                                 *pdf-tools* lists them, tell how it was made and changed.")
                              :revision (revision-index last)
                              :evidence (if (char= #\/ (char place 0))
                                            (info-evidence reading place)
                                            (xmp-evidence reading (xml-unprefixed (subseq place 4))))
                              :method :pdf-editor)
                       out))))
    (when (and producer creator
               (let ((producer (string-downcase producer)))
                 (and (search "acrobat" producer) (not (search "distiller" producer))
                      (not (search "pdfmaker" producer))))
               (not (search "acrobat" (string-downcase creator))))
      (push (found :look (format nil "It was made by ~a and saved again by ~a." creator producer)
                   :rule (format nil "A producer naming Adobe Acrobat (not Distiller or PDFMaker) beside a creator ~
                                      naming another program means the file was opened and saved in Acrobat.")
                   :revision (revision-index last)
                   :evidence (append (info-evidence reading "/Creator") (info-evidence reading "/Producer"))
                   :method :other-program)
            out))
    (nreverse out)))

(defun save-metadata-changes (pdf before revision)
  "The findings of what REVISION of PDF changed in the metadata of the save
BEFORE it: another program named, document information cleared, the XMP
removed, another first ID."
  (let ((out '()))
    (dolist (key '("/Producer" "/Creator"))
      (let ((a (info-text pdf before key)) (b (info-text pdf revision key)))
        (when (and a b (string/= a b))
          (push (found :look (format nil "~@(~a~) names another program in ~a than the save before: ~s, before ~s."
                                     (revision-name revision) key b a)
                       :rule "Each save's document information names the program that saved it."
                       :revision (revision-index revision)
                       :evidence (let ((n (nth-value 1 (info-of pdf revision))))
                                   (and n (key-evidence pdf revision n key)))
                       :method :other-program)
                out))))
    (when (info-of pdf before)
      (let ((gone (remove-if (lambda (key) (info-text pdf revision key))
                             (remove-if-not (lambda (key) (info-text pdf before key))
                                            '("/Title" "/Author" "/Subject" "/Keywords" "/Creator" "/Producer"
                                              "/CreationDate" "/ModDate")))))
        (when gone
          (push (found :look (format nil "~@(~a~) removes ~:[parts of the document information~;the document ~
                                          information~] (~{~a~^, ~}): someone cleared what the file said of itself."
                                     (revision-name revision) (null (info-of pdf revision)) gone)
                       :rule "A save that drops fields of the document information has cleared them on purpose."
                       :revision (revision-index revision) :method :metadata-removed)
                out))))
    (when (and (xmp-of pdf before) (not (xmp-of pdf revision)))
      (push (found :look (format nil "~@(~a~) removes the XMP metadata." (revision-name revision))
                   :rule (format nil "A save that drops the catalogue's /Metadata has removed what the file said of ~
                                      itself.")
                   :revision (revision-index revision) :method :metadata-removed)
            out))
    (let ((a (first (pdf-array-items (pdf-get (revision-trailer before) "/ID"))))
          (b (first (pdf-array-items (pdf-get (revision-trailer revision) "/ID")))))
      (when (and (stringp a) (stringp b) (string/= a b))
        (push (found :look (format nil "~@(~a~) gives the file another first ID than the save before: another ~
                                        document's ID, or one rewritten." (revision-name revision))
                     :rule (format nil "The first part of a file's /ID stays the same in every save; only the second ~
                                        changes.")
                     :revision (revision-index revision)
                     :evidence (list (format nil "before: <~a>; now: <~a>" (id-hex a) (id-hex b))
                                     (format nil "trailer of ~a at ~a" (revision-name revision)
                                             (offset-words (or (revision-trailer-at revision) 0))))
                     :method :metadata-edited)
              out)))
    (nreverse out)))

(defun id-parts-finding (pdf)
  "The finding, for the record, of the two parts of the /ID of PDF, saved
once, differing; NIL when they do not."
  (let* ((last (last-revision pdf)) (id (pdf-array-items (pdf-get (revision-trailer last) "/ID"))))
    (when (and (= (length (pdf-revisions pdf)) 1) (= (length id) 2) (stringp (first id)) (stringp (second id))
               (string/= (first id) (second id)))
      (found :record (format nil "The two parts of the file's /ID differ: it was saved again after it was first ~
                                  made, or its program writes them so.")
             :rule (format nil "A program writes the two parts of /ID alike when it makes a file, and changes the ~
                                second each time it saves it.")
             :revision 0
             :evidence (list (format nil "/ID [<~a> <~a>] in the trailer at ~a" (id-hex (first id)) (id-hex (second id))
                                     (offset-words (or (revision-trailer-at last) 0))))))))

(defun check-metadata (pdf)
  "The signs in the document information and XMP metadata of PDF: the two
disagreeing, impossible dates, edit histories, programs that edit PDFs,
IDs changed and metadata removed between saves. The two are said to give
different dates when more than a minute apart, strongly when more than a
day; a file is changed before it was made when by more than a minute; a
date is in the future when more than a day after today."
  (let ((last (last-revision pdf)))
    (multiple-value-bind (info info-number) (info-of pdf last)
      (multiple-value-bind (fields tree xmp-number) (xmp-of pdf last)
        (let ((reading (make-metadata-reading pdf last info info-number fields tree xmp-number)))
          (reverse (remove nil (append (metadata-disagreements reading)
                                       (metadata-dates reading)
                                       (metadata-history reading)
                                       (metadata-programs reading)
                                       (loop for r across (pdf-revisions pdf)
                                             for before = nil then previous
                                             for previous = r
                                             when before append (save-metadata-changes pdf before r))
                                       (list (id-parts-finding pdf))))))))))

(defun string-octets-as-read (text)
  "The octets of TEXT, a PDF string as it was read, a character for each octet."
  (map 'octets (lambda (c) (logand 255 (char-code c))) text))

(defun id-hex (text)
  "TEXT, a part of a file's /ID as read, in hex."
  (octets-hex (string-octets-as-read text) :upper-case t))

;;; What the pages draw

(defun font-family (font)
  "The family of FONT: its name up to a comma or hyphen (Calibri for Calibri,Italic and Calibri-Bold)."
  (let* ((base (font-base font)) (cut (position-if (lambda (c) (find c ",-")) base)))
    (if (and cut (plusp cut)) (subseq base 0 cut) base)))

(defparameter *white-text-shows-under* 90
  "The most lightness (LIGHTNESS) a shading may have for white text on it to
be seen: a grey of 0.94, all but white, is 96; the pale red, blue and
yellow ends of the shadings that bars and banners are drawn with 60 to
96, the white labels on them seen on those of 60 to 80.")

(defun shapes-words (count colour)
  "COUNT filled shapes of COLOUR, in words: a white shape, 2 white shapes; a
shading of a colour not known, for shapes of a colour not known, as only
shadings are."
  (cond ((null colour) (format nil "~:[a shading~;~:*~d shadings~] of a colour not known" (and (> count 1) count)))
        ((> count 1) (format nil "~d ~a shapes" count (colour-words colour)))
        (t (format nil "a ~a shape" (colour-words colour)))))

(defun white-p* (colour)
  "True when COLOUR is white, or nearly: red, green and blue each 0.95 or
more, so that a white written as 0.98 by a program that rounds is white."
  (and colour (every (lambda (c) (>= c 0.95)) colour)))

(defun centre-inside-p (mark box-mark)
  "True when the middle of MARK lies within BOX-MARK's box."
  (inside-box-p (list (mark-x0 box-mark) (mark-y0 box-mark) (mark-x1 box-mark) (mark-y1 box-mark))
                (/ (+ (mark-x0 mark) (mark-x1 mark)) 2) (/ (+ (mark-y0 mark) (mark-y1 mark)) 2)))

(defun centre-height (mark)
  "The height of the middle of MARK's box."
  (/ (+ (mark-y0 mark) (mark-y1 mark)) 2))

(defun by-centre-height (marks)
  "MARKS (a list) as a vector sorted by the heights of their middles, and a
vector of those heights, as two values, for CENTRED-WITHIN."
  (let ((sorted (sort (coerce marks 'vector) #'< :key #'centre-height)))
    (values sorted (map 'vector #'centre-height sorted))))

(defun centred-within (sorted heights box-mark)
  "The marks of SORTED (BY-CENTRE-HEIGHT, with its HEIGHTS) whose middle lies
within BOX-MARK's box, give or take half a point, as CENTRE-INSIDE-P tells;
only those at its height are looked at."
  (loop for k from (sorted-position-after heights (- (mark-y0 box-mark) 0.5) :below t)
          below (sorted-position-after heights (+ (mark-y1 box-mark) 0.5))
        for mark = (aref sorted k)
        when (centre-inside-p mark box-mark) collect mark))

(defun area (mark)
  "The area of MARK's box, in square points."
  (* (max 0 (- (mark-x1 mark) (mark-x0 mark))) (max 0 (- (mark-y1 mark) (mark-y0 mark)))))

(defun later-source-p (pdf revision mark)
  "True when the stream drawing MARK was written in a later save than the original."
  (let ((w (and (mark-source mark) (written-in pdf revision (mark-source mark))))) (and w (plusp (revision-index w)))))

(defun mark-characters (mark)
  "How many characters other than spaces MARK shows."
  (count #\Space (mark-text mark) :test-not #'char=))

(defun line-usual (marks key)
  "The value of KEY (a function of a mark) that most of MARKS' characters
have, and how many characters have it; values within a hundredth of each
other counted as one."
  (let ((counts '()))
    (dolist (m marks)
      (let* ((value (funcall key m))
             (entry (find-if (lambda (e) (if (and (realp value) (realp (car e)))
                                             (<= (abs (- value (car e))) 0.01)
                                             (equal value (car e))))
                             counts)))
        (if entry
            (incf (cdr entry) (mark-characters m))
            (push (cons value (mark-characters m)) counts))))
    (let ((best (reduce (lambda (a b) (if (> (cdr b) (cdr a)) b a)) counts :initial-value (cons nil 0))))
      (values (car best) (cdr best)))))

(defparameter *line-unlike-least* 3
  "The fewest letters or figures a run of text must show for it to be told
apart from the rest of its line by its size, weight, setting or ink.")

(defun dark-p (colour)
  "True when COLOUR is a black or a dark grey, as text is printed in."
  (and colour (every (lambda (c) (<= c 0.4)) colour)))

(defun render-mode-words (mode)
  "How text in render MODE is drawn, in words."
  (case mode (0 "filled") (1 "outlined") (2 "filled and outlined") (t (format nil "in render mode ~d" mode))))

(defun colour-figures (colour)
  "COLOUR, (red green blue) each 0 to 1, as figures to two places, where
COLOUR-WORDS names it: grey 0.13 when the three are one, else rgb 0.10 0.10
0.12; so that two blacks a hundredth or two apart read apart."
  (if (= (first colour) (second colour) (third colour))
      (format nil "grey ~,2f" (first colour))
      (format nil "rgb ~{~,2f~^ ~}" colour)))

(defparameter *baseline-least* 0.2
  "The least, in points, a run of text set above or below the rest of its
line, or a line set off an edge others share, is told apart by: programs set
the runs of a line on one baseline and lines on one edge to a hundredth.")

(defun line-unlike (line)
  "The runs of text in LINE set unlike the rest of it, a list of (KIND MARK
OWN USUAL). KIND is :size (its height in points), :weight (its render mode,
filled and outlined drawing bolder) or :setting (how much narrower or wider
its characters are set). KIND is :spacing (the space after each character,
in sizes), :ink (its colour, both dark) or :baseline (its baseline, in
points from the page's foot, a run as large as the line's set above or
below the rest by more than *BASELINE-LEAST*). Only a run of
*LINE-UNLIKE-LEAST* letters or figures, in the same font family as most of
the line, among at least twice as many characters set the usual way is told
apart; a run set unlike the rest by a few hundredths is not, nor a
superscript (a size under 70 % of the line's)."
  (let* ((shown (remove-if
                  (lambda (m) (or (member (mark-mode m) '(3 7)) (null (mark-font m)) (white-p* (mark-colour m))))
                  line))
         (family (line-usual shown (lambda (m) (font-family (mark-font m)))))
         (same (remove-if-not (lambda (m) (equal (font-family (mark-font m)) family)) shown))
         (out '()))
    (flet ((odd (kind key unlike-p &optional (among same))
             (multiple-value-bind (usual many) (line-usual among key)
               (dolist (m among)
                 (let ((own (funcall key m)))
                   (when (and usual (>= (count-if #'alphanumericp (mark-text m)) *line-unlike-least*)
                              (>= many (* 2 (mark-characters m)))
                              (funcall unlike-p own usual))
                     (push (list kind m own usual) out)))))))
      (odd :size #'mark-size
           (lambda (own usual) (and (plusp usual) (< 0.7 (/ own usual) 1.43) (> (abs (- (/ own usual) 1)) 0.04))))
      (odd :weight #'mark-mode (lambda (own usual) (and (member own '(0 1 2)) (member usual '(0 1 2)) (/= own usual))))
      (odd :setting #'mark-squeeze (lambda (own usual) (and (plusp usual) (> (abs (- (/ own usual) 1)) 0.03))))
      (odd :spacing #'mark-spacing (lambda (own usual) (> (abs (- own usual)) 0.03)))
      (odd :ink #'mark-colour (lambda (own usual) (and (dark-p own) (dark-p usual)
                                                         (>
                                                           (reduce #'max
                                                                   (mapcar (lambda (a b) (abs (- a b))) own usual))
                                                           0.02)))
           (remove-if-not (lambda (m) (dark-p (mark-colour m))) shown))
      ;; a superscript or subscript, smaller and moved on purpose, is not set beside the line
      (let ((size (line-usual same #'mark-size)))
        (odd :baseline #'mark-y0 (lambda (own usual) (> (abs (- own usual)) *baseline-least*))
             (remove-if-not (lambda (m) (and size (plusp size) (>= (/ (mark-size m) size) 0.85))) same))))
    (nreverse out)))

(defun lines-off-edges (lines)
  "The lines of LINES (PAGE-LINES-OF) set a little off an edge three or more
other lines of about their size share, each (SIDE LINE OWN USUAL MANY).
SIDE is :left for where it starts, :right where it ends (only when every
font of the line gives its widths, so that its end is known). OWN is its
edge, USUAL the shared one, MANY how many lines share it. Off means by more
than *BASELINE-LEAST* and less than 3 points: further is an indent. Lines
of another size (a heading, a header, a footnote) are another style, set to
their own edges, and are not set beside each other."
  (let ((out '()))
    (flet ((size-of (line) (median-of (mapcar #'mark-size line)))
           (alike (a b) (and (plusp a) (plusp b) (<= (/ (abs (- a b)) (max a b)) 0.1))))
      (flet ((edges (side edge-of usable-p)
               (let* ((usable (remove-if-not (lambda (line) (and (funcall usable-p line)
                                                                  (>=
                                                                    (count-if #'alphanumericp
                                                                              (format nil "~{~a~}"
                                                                                      (mapcar #'mark-text line)))
                                                                    3)))
                                             lines))
                      (shared '()))
                 ;; the edges three or more lines of a size share, to a hundredth: each (EDGE SIZE COUNT)
                 (dolist (line usable)
                   (let* ((edge (funcall edge-of line)) (size (size-of line))
                          (entry (find-if
                                   (lambda (e) (and (<= (abs (- (first e) edge)) 0.01) (alike (second e) size)))
                                   shared)))
                     (if entry (incf (third entry)) (push (list edge size 1) shared))))
                 (setf shared (remove-if (lambda (e) (< (third e) 3)) shared))
                 (dolist (line usable)
                   (let* ((edge (funcall edge-of line)) (size (size-of line))
                          (near (find-if
                                  (lambda (e)
                                    (and (alike (second e) size) (< *baseline-least* (abs (- (first e) edge)) 3)))
                                  shared)))
                     (when (and near
                                (not (find-if
                                       (lambda (e) (and (alike (second e) size) (<= (abs (- (first e) edge)) 0.01)))
                                       shared)))
                       (push (list side line edge (first near) (third near)) out)))))))
        (edges :left (lambda (line) (mark-x0 (first line))) (constantly t))
        (edges :right (lambda (line) (mark-x1 (car (last line))))
               (lambda (line)
                 (every (lambda (m) (and (mark-font m) (plusp (hash-table-count (font-widths (mark-font m))))))
                        line)))))
    (nreverse out)))

(defstruct (page-check (:constructor make-page-check (pdf revision page marks texts number scan)) (:copier nil)
                       (:predicate nil))
  "What the checks of one page's content read and what they find: the PDF
and REVISION, the PAGE, the MARKS it draws and the TEXTS among them, its
NUMBER counting from 1, the picture SCAN covering most of it or NIL, and
the FINDINGS so far, the latest first."
  pdf revision page marks texts number scan (findings '()))

(defun page-found (check strength words rule marks-shown &rest keys)
  "Add to CHECK a finding of STRENGTH saying WORDS by RULE, of the marks
MARKS-SHOWN: the evidence of the first four of them, the steps to the
first, the objects drawing them. KEYS go to FOUND."
  (let ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check)))
    (push (apply #'found strength words :rule rule :revision (revision-index revision) :page (page-check-number check)
                 :evidence (loop for m in marks-shown
                                 repeat 4
                                 append (mark-evidence pdf revision m (mark-words page m)))
                 :steps (loop for m in marks-shown
                              repeat 1
                              append (nth-value 1 (mark-evidence pdf revision m)))
                 :objects (remove-duplicates (remove nil (mapcar #'mark-source marks-shown)))
                 keys)
          (page-check-findings check))))

(defun marks-text (marks length)
  "The text of MARKS one after another, a space between, cut to LENGTH
characters on one line."
  (shorten-string (format nil "~{~a~^ ~}" (mapcar #'mark-text marks)) length :one-line t))

(defun cover-cause (check shapes runs within parts framed counts angle size)
  "The honest cause the drawing shows for SHAPES covering RUNS, text drawn
before them: :background or :turned for a panel over a watermark, :outlined,
:labels, :stamp, :form or :map; NIL for none, or when the shapes carry new
text set in place of the old, or when the file is not as its maker wrote
it. WITHIN gives the marks within a shape, PARTS the parts of the runs
hidden, FRAMED the page's text runs whose frames are known, COUNTS how many
of them write each text (TEXT-COUNTS), ANGLE and SIZE their usual turn and
size."
  (let ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (marks (page-check-marks check))
        (hidden-parts (or parts (mapcar (lambda (run) (list run 0 1)) runs))))
    (flet ((later-text-p (s test)
             (some (lambda (m) (and (> (mark-order m) (mark-order s)) (funcall test m))) (funcall within s))))
      (cond ((some (lambda (s) (later-text-p s (lambda (m) (some (lambda (h) (replacement-p m h)) hidden-parts))))
                   shapes)
             nil)
            ;; only for a file as its maker wrote it, as for a text box drawn as made
            ((not (file-as-written-p pdf revision)) nil)
            ;; a panel over a watermark carries text of its own, as panels in documents do
            ((and (every (lambda (run) (background-run-p run framed angle size counts)) runs)
                  (every (lambda (s) (later-text-p s (lambda (m) (own-text-shows-p m s nil)))) shapes))
             (if (every (lambda (run) (eq (background-run-p run framed angle size counts) :light)) runs)
                 :background
                 :turned))
            ((some #'outline-shows-p shapes) :outlined)
            ((every (lambda (run) (some (lambda (s) (label-p run s marks)) shapes)) runs) :labels)
            ((every (lambda (run) (every (lambda (s) (stamp-p run s marks)) shapes)) runs) :stamp)
            ((every (lambda (s) (form-field-p runs s marks)) shapes) :form)
            ((every (lambda (run) (every (lambda (s) (map-label-p run s marks framed counts)) shapes)) runs) :map)))))

(defparameter *cover-cause-words*
  '((:background . "; the text beneath is a watermark or a background word, light and under all the page's text, ~
                    turned, large or written again and again, under a panel with text of its own: panels lie over ~
                    such words in documents as made, as they do when a watermark is hidden")
    (:turned . "; the text beneath is drawn before all the page's text and turned ~d degrees from it, as a watermark ~
                is, under a panel with text of its own: panels lie over such words in documents as made, as they do ~
                when a watermark is hidden")
    (:outlined . "; the shape is outlined in a colour seen against the page, a callout or label meant to be seen, not ~
                  a patch the colour of the paper")
    (:labels . "; labels laid over one another as a drawing lays them, each a box with its text, the text beneath a ~
                label of its own")
    (:stamp . "; a stamp or note laid across the text, its own text turned against the line beneath, not set along it ~
               as new text in place of old is")
    (:form . "; a form field filled in, as a form flattened into its page draws one: a box with its value over the ~
              line to write on and the hint beside it")
    (:map . "; a label laid on a map, a pill round its own name over names written again and again beneath, laid ~
             down first"))
  "What each honest cause COVER-CAUSE names says, after the finding's words:
format controls, the one of :turned taking the angle in degrees.")

(defun cover-rule (made cause kind)
  "The rule of a finding of shapes over text: MADE when drawn as the file was
made, CAUSE the honest cause shown, KIND :covered when the text lies wholly
under the shapes."
  (cond (made (format nil "A filled shape drawn after text, over it, hides that text from the eye but not from the ~
                           file. This one was drawn by the same content stream as the text, with text of its own on it ~
                           as a text box has, in a font kept in the file, in the file's first save, its /ID as its ~
                           maker wrote it, by a program that makes PDFs from documents and with no PDF editor named: ~
                           the document as its program printed it had the shape over the text."))
        (cause (format nil "A filled shape drawn after text, over it, hides that text from the eye but not from the ~
                            file; how this one lies, as the finding names it, is how documents are drawn as made, so ~
                            it is worth a look, not a strong sign."))
        ((eq kind :covered) (format nil "A filled shape drawn after text, over it, hides that text from the eye but ~
                                         not from the file: every character of it lies wholly under the shape, tested ~
                                         along its own baseline."))
        (t (format nil "A filled shape drawn after text, over a part of it, hides that part from the eye but not from ~
                        the file: the way an editor changes one figure of a line."))))

(defun check-covered-text (check)
  "Add to CHECK the text its page draws that filled shapes drawn after it
cover (PAGE-COVERS), wholly or in part: a strong sign when a shape is white
and nothing the drawing shows explains it (DRAWN-AS-MADE-P, COVER-CAUSE),
else worth a look."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check))
         (p (page-check-number check))
         (framed (remove-if-not #'mark-frame (page-check-texts check)))
         (counts (text-counts framed)))
    (multiple-value-bind (entries within) (page-covers (page-check-marks check))
      (multiple-value-bind (angle size) (page-text-usual framed)
        (loop for (kind shapes runs part hidden) in entries
              do (let* ((colour (mark-colour (first shapes)))
                        (black (and colour (every (lambda (c) (<= c 0.05)) colour)))
                        (later (some (lambda (s) (later-source-p pdf revision s)) shapes))
                        (parts (and (eq kind :partly)
                                    (let ((run (first runs)) (first (position t hidden))
                                          (last (position t hidden :from-end t)))
                                      (list (list run (aref (mark-stops run) (* 2 first))
                                                  (aref (mark-stops run) (1+ (* 2 last))))))))
                        (made (every (lambda (s) (drawn-as-made-p pdf revision s runs (funcall within s) page parts))
                                     shapes))
                        ;; an honest cause the drawing itself shows none for a shape added later
                        (cause (and (not (or made later))
                                    (cover-cause check shapes runs within parts framed counts angle size)))
                        (strength (if (and (white-p* colour) (not made) (not cause)) :strong :look))
                        (why (cond (made (format nil "; drawn with the text as the file was made: a shape over text in ~
                                                      the program the document was written in, as a text box over a ~
                                                      spreadsheet's cell is, or a change made there before the PDF was ~
                                                      made"))
                                   (cause (format nil (cdr (assoc cause *cover-cause-words*))
                                                  (and (eq cause :turned)
                                                       (round (reduce #'min runs
                                                                      :key (lambda (run)
                                                                             (nth-value 1 (background-run-p
                                                                                           run framed angle size
                                                                                           counts))))))))
                                   (t "")))
                        (rule (cover-rule made cause kind)))
                   (if (eq kind :covered)
                       (page-found check strength
                                   (format nil "Text on page ~d is covered by ~a drawn over it~:[~;, added in a later ~
                                                save~]: ~s~:[~; (blacked out: the text is still in the file)~]~a."
                                           p (shapes-words (length shapes) colour) later
                                           (marks-text runs 120) black why)
                                   rule (append shapes runs) :method :text-covered)
                       (let ((run (first runs)))
                         (page-found check strength
                                     (format nil "Text on page ~d is partly covered by ~a drawn over it~:[~;, added in ~
                                                  a later save~]: ~s of ~s~a."
                                             p (shapes-words (length shapes) colour) later part
                                             (shorten-string (mark-text run) 100 :one-line t) why)
                                     rule (append shapes runs)
                                     :details (let ((place (mark-place run)))
                                                (and place (list (format nil "the line is drawn at ~a" place))))
                                     :method :text-covered)))))))))

(defun check-unseen-text (check)
  "Add to CHECK the text its page draws that is never seen: invisible (on a
scan, the text recognition of it, for the record), clipped away, outside
the page, or in a layer that is off."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check))
         (p (page-check-number check)) (texts (page-check-texts check))
         (invisible (remove-if-not (lambda (m) (member (mark-mode m) '(3 7))) texts))
         (clipped (remove-if-not #'mark-clipped texts))
         (box (or (page-crop page) (page-media page)))
         (outside (remove-if (lambda (m)
                               (inside-box-p box (/ (+ (mark-x0 m) (mark-x1 m)) 2) (/ (+ (mark-y0 m) (mark-y1 m)) 2) 1))
                             texts))
         (hidden (remove-if-not #'mark-hidden texts)))
    (cond ((null invisible))
          ((page-check-scan check)
           (page-found check :record
                       (format nil "Page ~d is a picture with invisible text over it, as text recognition (OCR) ~
                                    leaves." p)
                       (format nil "Invisible text (render mode 3) over a picture of the whole page is how scanned ~
                                    pages are made searchable.")
                       (list (first invisible))))
          (t
           (page-found check (if (some (lambda (m) (later-source-p pdf revision m)) invisible) :strong :look)
                       (format nil "Page ~d holds invisible text: ~s." p (marks-text invisible 120))
                       "Text in render mode 3 or 7 is never drawn: found by searching and copying, never seen."
                       invisible :method :hidden-text)))
    (when clipped
      (page-found check :look
                  (format nil "Text on page ~d lies outside what the page clips to, so it is never seen: ~s."
                          p (marks-text clipped 120))
                  "Text drawn outside the clipping path is cut away." clipped :method :hidden-text))
    (when outside
      (page-found check :look (format nil "Text on page ~d lies outside the page: ~s." p (marks-text outside 120))
                  "Text drawn outside the page's crop box (else its media box) is never shown."
                  outside :method :hidden-text))
    (when hidden
      (page-found check :look (format nil "Text on page ~d is in a layer that is off: ~s." p (marks-text hidden 120))
                  "Content tagged with an optional content group that is off as the file opens is not shown."
                  hidden :method :hidden-text))))

(defun white-text-unseen (check)
  "The white text CHECK's page draws, not invisible, that nothing drawn
before it beneath shows up: no filled shape or picture not white, nor a
shading dark enough that white shows on it."
  (let* ((white (remove-if-not (lambda (m) (and (not (member (mark-mode m) '(3 7))) (white-p* (mark-colour m))))
                               (page-check-texts check)))
         (seen (make-hash-table :test 'eq)))
    (multiple-value-bind (white-by-height white-heights) (by-centre-height white)
      (loop for under across (page-check-marks check)
            for shading = (equal (mark-text under) "shading")
            when (and (member (mark-kind under) '(:fill :image)) (not (white-p* (mark-colour under)))
                      ;; a shading of a colour not known, or one so light white is lost on it, shows nothing for
                      ;; certain
                      (or (not shading)
                          (and (mark-colour under) (< (lightness (mark-colour under)) *white-text-shows-under*))))
              do (dolist (m (centred-within white-by-height white-heights under))
                   (when (and (< (mark-order under) (mark-order m))
                              ;; a shading paints only where it reaches, not its whole box
                              (or (not shading) (null (mark-outline under))
                                  (covered-point-p (list (make-coverage under))
                                                   (float (/ (+ (mark-x0 m) (mark-x1 m)) 2) 1d0)
                                                   (float (/ (+ (mark-y0 m) (mark-y1 m)) 2) 1d0))))
                     (setf (gethash m seen) t)))))
    (remove-if (lambda (m) (gethash m seen)) white)))

(defun check-faint-text (check)
  "Add to CHECK the white text its page draws on white (WHITE-TEXT-UNSEEN),
and its text under a point tall."
  (let ((p (page-check-number check))
        (white (white-text-unseen check))
        (tiny (remove-if-not (lambda (m) (< 0 (mark-size m) 1)) (page-check-texts check))))
    (when white
      (page-found check
                  (if (some (lambda (m) (later-source-p (page-check-pdf check) (page-check-revision check) m)) white)
                      :strong
                      :look)
                  (format nil "Page ~d holds white text on a white page, unseen: ~s." p (marks-text white 120))
                  "Text the colour of the page beneath it cannot be seen." white :method :hidden-text))
    (when tiny
      (page-found check :look
                  (format nil "Page ~d holds text too small to read (under 1 pt): ~s." p (marks-text tiny 120))
                  "Text smaller than a point cannot be read on the page." tiny :method :hidden-text))))

(defun check-content-streams (check)
  "Add to CHECK, when its page draws from more than one content stream, a
finding saying so: a strong sign when one was appended in a later save,
else for the record."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check))
         (p (page-check-number check)) (contents (page-contents page)))
    (when (> (length contents) 1)
      (let* ((written (mapcar (lambda (c) (let ((w (written-in pdf revision c))) (if w (revision-index w) 0)))
                              contents))
             (late (> (reduce #'max written) (reduce #'min written))))
        (push (found (if late :strong :record)
                     (format nil "Page ~d draws from ~d content streams~:[~;, one of them appended in a later save: ~
                                  something was laid over the page~]." p (length contents) late)
                     :rule (format nil "A page's /Contents may be an array of streams drawn one after another; a ~
                                        stream added in a later save draws over what was there.")
                     :revision (revision-index revision) :page p :objects contents
                     :evidence (loop for c in contents for w in written
                                     append (cons (format nil "content stream object ~d, written in revision ~d" c
                                                          (1+ w))
                                                  (nth-value 0 (object-evidence pdf revision c))))
                     :method :laid-over)
              (page-check-findings check))))))

(defun font-family-counts (texts &key figures)
  "A table of the font families TEXTS are set in: how many characters each
sets; with FIGURES, T for each family setting a figure."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (m texts table)
      (when (mark-font m)
        (if figures
            (when (some #'digit-char-p (mark-text m))
              (setf (gethash (font-family (mark-font m)) table) t))
            (incf (gethash (font-family (mark-font m)) table 0) (length (mark-text m))))))))

(defun line-retyped (line font-characters fonts-with-figures)
  "The runs of LINE that are figures in a font of their own, each (MARK
MAIN), MAIN being the family most of the line is set in, when that family
has figures of its own on the page (FONTS-WITH-FIGURES) and the run's font
sets 40 characters of the page or fewer (FONT-CHARACTERS)."
  (let* ((counts (font-family-counts line))
         (total (loop for v being the hash-values of counts sum v))
         (main (loop with best = nil with most = 0 for k being the hash-keys of counts using (hash-value v)
                     when (or (> v most) (and (= v most) best (string< k best)))
                       do (setf best k most v)
                     finally (return best))))
    (when (and main (>= (gethash main counts) (* 0.7 total)) (gethash main fonts-with-figures)
               (>= (count-if #'alphanumericp
                             (format nil "~{~a~}" (mapcar #'mark-text
                                                          (remove main line
                                                                  :key (lambda (m)
                                                                         (and (mark-font m)
                                                                              (font-family (mark-font m))))
                                                                  :test-not #'equal))))
                   (* 0.5 (gethash main counts))))
      (loop for m in line
            for text = (string-trim " " (mark-text m))
            when (and (mark-font m) (string/= (font-family (mark-font m)) main)
                      (plusp (length text)) (<= (length text) 12)
                      (every (lambda (c) (or (digit-char-p c) (find c ".,:/$€£%()+- "))) text)
                      (>= (count-if #'digit-char-p text) (* 0.5 (length (remove #\Space text))))
                      (<= (gethash (font-family (mark-font m)) font-characters 0) 40))
              collect (list m main)))))

(defun lines-set-apart (texts lines)
  "Three values, of the LINES of a page drawing TEXTS: the lines drawn a
character at a time where the rest of the page is not, those whose
characters are spaced apart one by one where the rest is not, and the runs
that are figures retyped in a font of their own (LINE-RETYPED)."
  (let* ((shows (reduce #'+ texts :key #'mark-shows)) (singles (reduce #'+ texts :key #'mark-singles))
         (page-characters (reduce #'+ texts :key #'mark-characters))
         (page-gaps (reduce #'+ texts :key #'mark-wide))
         (font-characters (font-family-counts texts))
         (fonts-with-figures (font-family-counts texts :figures t))
         (one-by-one '()) (spaced '()) (retyped '()))
    (dolist (line lines)
      (let ((line-shows (reduce #'+ line :key #'mark-shows)) (line-singles (reduce #'+ line :key #'mark-singles))
            (line-characters (reduce #'+ line :key #'mark-characters)) (line-gaps (reduce #'+ line :key #'mark-wide)))
        (when (and (>= line-shows 5)
                   (>= line-characters 10)
                   (>= line-singles (* 0.8 line-shows))
                   (>= (- shows line-shows) 5)
                   (< (- singles line-singles) (* 0.3 (- shows line-shows))))
          (push line one-by-one))
        (when (and (>= line-characters 4)
                   (>= line-gaps (* 0.6 line-characters))
                   (>= (- page-characters line-characters) 20)
                   (< (- page-gaps line-gaps) (* 0.25 (- page-characters line-characters))))
          (push line spaced))
        (setf retyped (revappend (line-retyped line font-characters fonts-with-figures) retyped))))
    (values (nreverse one-by-one) (nreverse spaced) (nreverse retyped))))

(defparameter *unlike-run-words*
  '((:size "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] in another size than the line around ~:*~[~;it~:;them~], ~
            in the same font: pasted, or retyped to fit."
     "A program sets a line in one size; a few words of the same font a little smaller or larger than their line were ~
      set apart from it."
     "~s at ~,1f pt, its line at ~,1f pt")
    (:weight "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] drawn bolder or lighter than the line around ~
              ~:*~[~;it~:;them~]: made to look bold, or pasted."
     "Bold text is set in a bold font; text both filled and outlined (render mode 2) beside text only filled was drawn ~
      heavier another way."
     "~s ~a, its line ~a")
    (:setting "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] set narrower or wider than the line around ~
               ~:*~[~;it~:;them~]: squeezed or stretched to fit a gap."
     "Characters squeezed or stretched (a horizontal scale, a text matrix narrowed) unlike the rest of their line were ~
      fitted into a space."
     "~s at ~d % of its width, its line at ~d %")
    (:spacing "On page ~d, ~:d run~:p of text ~:*~[~;has~:;have~] ~:*~[~;its~:;their~] characters spaced unlike the ~
               line around ~:*~[~;it~:;them~]: fitted into a gap."
     "Space added after each character (character spacing) in a few words of a line, not the rest, spreads or packs ~
      them to fit."
     "~s with ~,2f of its size after each character, its line ~,2f")
    (:ink "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] in another black than the line around ~:*~[~;it~:;them~]: ~
           pasted from another document, or retyped."
     "A program sets a line's text in one colour; a black or dark grey a little unlike the rest of the line came from ~
      elsewhere."
     "~s in ~a, its line in ~a")
    (:baseline "On page ~d, ~:d run~:p of text ~:*~[~;sits~:;sit~] a little above or below the line around ~
                ~:*~[~;it~:;them~]: placed by hand."
     "A program sets the runs of a line on one baseline; a run of the same size a fraction of a point above or below ~
      it was put there apart."
     "~s ~,2f pt ~:[below~;above~] its line"))
  "For each way a run of text is unlike its line (LINE-UNLIKE), in order:
the words of the finding (a format control of the page and the count), its
rule, and the format control of each run's detail.")

(defun unlike-run-detail (kind control text own usual)
  "The detail of a run of TEXT unlike its line in the way KIND: CONTROL (of
*UNLIKE-RUN-WORDS*) given the run's value OWN and the line's USUAL one, as
each kind shows them."
  (ecase kind
    (:size (format nil control text own usual))
    (:weight (format nil control text (render-mode-words own) (render-mode-words usual)))
    (:setting (format nil control text (round (* 100 own)) (round (* 100 usual))))
    (:spacing (format nil control text own usual))
    (:ink (format nil control text (colour-figures own) (colour-figures usual)))
    (:baseline (format nil control text (abs (- own usual)) (> own usual)))))

(defun check-unlike-runs (check unlike)
  "Add to CHECK, for each way runs of text are unlike their line (UNLIKE,
from LINE-UNLIKE), one finding of those runs."
  (loop for (kind words rule control) in *unlike-run-words*
        for runs = (remove kind unlike :key #'first :test-not #'eq)
        when runs
          do (page-found check :look (format nil words (page-check-number check) (length runs)) (format nil rule)
                         (mapcar #'second runs)
                         :method (ecase kind
                                   ((:size :setting :spacing) :fitted)
                                   ((:weight :ink) :retyped)
                                   (:baseline :placed-by-hand))
                         :details (loop for (nil m own usual) in runs
                                        collect (format nil "~a~@[ (~a)~]"
                                                        (unlike-run-detail kind control
                                                                           (shorten-string (mark-text m) 60 :one-line t)
                                                                           own usual)
                                                        (mark-place m))))))

(defun check-text-set-apart (check)
  "Add to CHECK the lines of text its page draws that were set apart from the
rest: drawn a character at a time, spaced one by one, figures in a font of
their own, runs unlike their line, lines a little off the edge the lines
around them keep."
  (let* ((p (page-check-number check))
         (lines (page-lines-of (page-check-marks check)))
         (unlike (loop for line in lines append (line-unlike line))))
    (flet ((line-text (line) (marks-text line 100)))
      (multiple-value-bind (one-by-one spaced retyped) (lines-set-apart (page-check-texts check) lines)
        (when one-by-one
          (page-found check :look
                      (format nil "~:d line~:p on page ~d ~:*~:*~[~;is~:;are~] drawn a character at a time, unlike ~
                                   the rest of the page: retyped, or added by another program."
                              (length one-by-one) p)
                      (format nil "Programs draw a line of text in a few runs; a line drawn character by character ~
                                   where the rest is not was written another way.")
                      (apply #'append one-by-one)
                      :details (mapcar #'line-text one-by-one) :method :retyped))
        (when spaced
          (page-found check :look
                      (format nil "~:d line~:p on page ~d ~:*~:*~[~;is~:;are~] spaced oddly, ~:*~[~;its~:;their~] ~
                                   characters set apart one by one: fitted into a gap."
                              (length spaced) p)
                      (format nil "Characters set apart by moving the text on, nearly one by one, where the rest of ~
                                   the page is not, are how text is fitted into a gap.")
                      (apply #'append spaced)
                      :details (mapcar #'line-text spaced) :method :fitted))
        (when retyped
          (page-found check :look
                      (format nil "On page ~d, ~:d figure~:p ~:*~[~;is~:;are~] in another font than the line around ~
                                   ~:*~[~;it~:;them~]: retyped." p (length retyped))
                      (format nil "Figures in a font used nowhere else on the page, in a line whose own font has ~
                                   figures of its own, were set apart from it, as a retyped number is.")
                      (mapcar #'first retyped)
                      :details (loop for (m main) in retyped
                                     collect (format nil "~s in ~a, its line in ~a~@[ (~a)~]"
                                                     (mark-text m) (font-base (mark-font m)) main (mark-place m)))
                      :method :retyped)))
      (check-unlike-runs check unlike)
      (let ((off (lines-off-edges lines)))
        (when off
          (page-found check :look
                      (format nil "On page ~d, ~:d line~:p of text ~:*~[~;is~:;are~] set a little off the edge the ~
                                   lines around ~:*~[~;it~:;them~] keep: placed by hand." p (length off))
                      (format nil "A program starts the lines of a block, and ends right-aligned ones, on one edge to ~
                                   a hundredth of a point; a line a fraction of a point off it was placed apart.")
                      (loop for (nil line) in off append line)
                      :details (loop for (side line own usual many) in off
                                     collect (format nil "~s ~:[ends~;starts~] at x ~,2f, ~d other lines at ~,2f~
                                                          ~@[ (~a)~]"
                                                     (line-text line) (eq side :left) own many usual
                                                     (mark-place (if (eq side :left) (first line) (car (last line))))))
                      :method :placed-by-hand))))))

(defun check-tagged-text (check)
  "Add to CHECK each stretch of text its page draws whose tag says it stands
for other words than show (its /ActualText or /Alt), spaces left aside."
  (let ((groups '()))
    (dolist (m (page-check-texts check))
      (when (mark-actual m)
        (if (and groups (eq (car (first groups)) (mark-actual m)))
            (push m (cdr (first groups)))
            (push (list (mark-actual m) m) groups))))
    (flet ((bare (text) (remove-if (lambda (c) (member (char-code c) '(9 10 13 32 160))) text)))
      (dolist (group groups)
        (let ((shown (format nil "~{~a~}" (mapcar #'mark-text (reverse (cdr group))))))
          (unless (string-equal (bare shown) (bare (car group)))
            (page-found check :look
                        (format nil "On page ~d the text shown, ~s, is tagged as standing for ~s."
                                (page-check-number check)
                                (shorten-string shown 80 :one-line t) (shorten-string (car group) 80 :one-line t))
                        (format nil "A tag's /ActualText or /Alt says what the text it marks stands for; read aloud ~
                                     and copied, it replaces what shows.")
                        (reverse (cdr group)) :method :retyped)))))))

(defun check-typed-on-scan (check)
  "Add to CHECK, when its page is one picture, the visible text drawn over
that picture."
  (let ((scan (page-check-scan check)))
    (when scan
      (let ((visible (remove-if (lambda (m) (or (member (mark-mode m) '(3 7)) (< (mark-order m) (mark-order scan))))
                                (page-check-texts check))))
        (when visible
          (page-found check :look
                      (format nil "Page ~d is one picture with text drawn over it: ~s; typed onto a scan."
                              (page-check-number check) (marks-text visible 120))
                      (format nil "A page that is a picture of a whole page holds no text of its own; visible text ~
                                   over it was laid on afterwards.")
                      (cons scan visible) :method :typed-on-scan))))))

(defun check-page-content (pdf revision page)
  "The signs in what PAGE of REVISION draws: text covered by shapes,
invisible, clipped away, outside the page, white or tiny text, content
appended later, text drawn a character at a time or in a font of its own,
tagged text unlike what shows, a page that is one picture with text over
it. A picture covering 85 % of the page or more is a scan, and invisible
text over it the text recognition of it, for the record."
  (job-check-stop)
  (let* ((marks (marks-of pdf revision page))
         (page-area (* (- (third (page-media page)) (first (page-media page)))
                       (- (fourth (page-media page)) (second (page-media page)))))
         (scan (and (plusp page-area)
                    (find-if (lambda (m) (and (eq (mark-kind m) :image) (>= (area m) (* 0.85 page-area)))) marks)))
         (check (make-page-check pdf revision page marks
                                 (remove-if-not (lambda (m) (eq (mark-kind m) :text)) (coerce marks 'list))
                                 (1+ (page-index page)) scan)))
    (check-covered-text check)
    (check-unseen-text check)
    (check-faint-text check)
    (check-content-streams check)
    (check-text-set-apart check)
    (check-tagged-text check)
    (check-typed-on-scan check)
    (page-check-findings check)))

(defun font-maker-key (base)
  "BASE, a font's name without its subset tag, as two values: what is the same
for every maker's name of its family (arial for ArialMT, Arial,Bold and
Arial-BoldMT), and how the name was made, \"MT\" when it ends so, as
Monotype names its fonts (ArialMT, Arial-BoldMT, TimesNewRomanPSMT), else
\"\" (Arial, Arial,Bold)."
  (let* ((cut (position-if (lambda (c) (find c ",-")) base))
         (key (string-downcase (subseq base 0 (or cut (length base))))))
    (dolist (suffix '("psmt" "mt" "ps"))
      (when (and (> (length key) (+ 2 (length suffix))) (uiop:string-suffix-p key suffix))
        (setf key (subseq key 0 (- (length key) (length suffix))))
        (return)))
    (values key (if (uiop:string-suffix-p (string-downcase base) "mt") "MT" ""))))

(defconstant +seconds-1900-to-1904+ (* 4 365 86400)
  "Seconds from 1900 to 1904, where a TrueType font's dates count from.")

(defun font-file-dates (pdf revision font)
  "When FONT's file kept in the PDF says it was made and last changed, as two
universal times, from the head table of a TrueType or OpenType font; NIL for
none, a font file of another kind (a Type 1 or bare CFF font has no dates),
or dates that cannot be (before 1985 or after now)."
  (let* ((ref (font-file font))
         (object (and ref (revision-object pdf revision (second ref))))
         (data (and object (multiple-value-bind (octets why) (stream-data pdf object :limit (* 16 1024 1024))
                             (and (null why) octets)))))
    (flet ((u16 (at) (+ (ash (aref data at) 8) (aref data (1+ at))))
           (u32 (at)
             (+ (ash (aref data at) 24)
                (ash (aref data (+ at 1)) 16)
                (ash (aref data (+ at 2)) 8)
                (aref data (+ at 3))))
           (u64 (at) (loop for i below 8 sum (ash (aref data (+ at i)) (* 8 (- 7 i))))))
      (when (and data (>= (length data) 12)
                 (member (u32 0) (list #x00010000 #x4F54544F #x74727565))) ; 1.0, OTTO, true
        (loop for record from 12 below (min (length data) (+ 12 (* 16 (u16 4)))) by 16
              while (<= (+ record 16) (length data))
              when (and (= (aref data record) 104) (= (aref data (+ record 1)) 101)      ; "he"
                        (= (aref data (+ record 2)) 97) (= (aref data (+ record 3)) 100)) ; "ad"
                do (let ((at (u32 (+ record 8))))
                     (when (<= (+ at 36) (length data))
                       (let ((made (+ (u64 (+ at 20)) +seconds-1900-to-1904+))
                             (changed (+ (u64 (+ at 28)) +seconds-1900-to-1904+))
                             (earliest (encode-universal-time 0 0 0 1 1 1985 0)))
                         (when (<= earliest changed (get-universal-time))
                           (return (values (and (<= earliest made (get-universal-time)) made) changed)))))))))))

(defun check-fonts (pdf revision)
  "One face in several subsets, one face both embedded and not, one family
named by two makers on a page, and a font kept in the file changed after the
document was made: more than a day after, so that a font dated in another
time zone than the document is not named."
  (let ((faces (make-hash-table :test 'equal)) (out '()))
    ;; one family named two ways on a page
    (dolist (page (pages-of pdf revision))
      (let ((spellings (make-hash-table :test 'equal)))
        (loop for (name . value) in (page-font-entries pdf revision page)
              do (let ((font (font-of pdf revision value)))
                   (when (plusp (length (font-base font)))
                     (multiple-value-bind (key spelling) (font-maker-key (font-base font))
                       (pushnew (list spelling (font-base font) name value) (gethash key spellings)
                                :test #'equal :key #'first)))))
        (loop for key in (sort (loop for k being the hash-keys of spellings collect k) #'string<)
              for uses = (reverse (gethash key spellings))
              when (> (length uses) 1)
                do (push (found :look (format nil "On page ~d one font is named the ways two programs name it (~{~a~^, ~
                                                   ~}): text set by another program."
                                              (1+ (page-index page)) (mapcar #'second uses))
                                :rule (concatenate 'string "A program names a font one way throughout (ArialMT, "
                                                           "Arial-BoldMT); the same family named another way (Arial, "
                                                           "Arial,Bold) on the same page was written by another.")
                                :revision (revision-index revision) :page (1+ (page-index page))
                                :details (loop for (nil base name) in uses
                                               collect (format nil "~a as ~a" (subseq name 1) base))
                                :objects (remove nil
                                                 (mapcar
                                                   (lambda (u) (and (pdf-ref-p (fourth u)) (second (fourth u))))
                                                   uses))
                                :evidence (loop for u in uses
                                                when (pdf-ref-p (fourth u))
                                                append (key-evidence pdf revision (second (fourth u)) "/BaseFont"))
                                :method :other-program)
                         out))))
    ;; a font kept in the file changed after the document was made
    (let ((made (date-of (info-text pdf revision "/CreationDate"))) (seen (make-hash-table :test 'equal)))
      (when made
        (dolist (page (pages-of pdf revision))
          (loop for (nil . value) in (page-font-entries pdf revision page)
                do (let ((font (font-of pdf revision value)))
                     (when (and (font-file font) (not (gethash (font-file font) seen)))
                       (setf (gethash (font-file font) seen) t)
                       (let ((changed (nth-value 1 (font-file-dates pdf revision font))))
                         (when (and changed (> changed (+ made 86400)))
                           (push (found :strong (format nil "The font ~a kept in the file was last changed ~a, after ~
                                                             the document was made (~a): it was put in later."
                                                        (font-base font) (utc-time-words changed) (utc-time-words made))
                                        :rule (concatenate 'string "A program embeds a font as it writes the document, "
                                                                   "and many date the subset they make then; a font "
                                                                   "changed after the document's creation date was "
                                                                   "embedded by a later edit, or that date was set "
                                                                   "back.")
                                        :revision (revision-index revision) :page (1+ (page-index page))
                                        :objects (list (second (font-file font)))
                                        :evidence (append (let ((number (nth-value 1 (info-of pdf revision))))
                                                            (and number
                                                                 (key-evidence pdf revision number "/CreationDate")))
                                                          (nth-value 0
                                                                     (object-evidence
                                                                       pdf
                                                                       revision
                                                                       (second (font-file font)))))
                                        :method :font-added)
                                 out)))))))))
    (dolist (page (pages-of pdf revision))
      (loop for (name . value) in (page-font-entries pdf revision page)
            do (let ((font (font-of pdf revision value)))
                 (when (plusp (length (font-base font)))
                   (push (list font (1+ (page-index page)) name (pdf-ref-p value)) (gethash (font-base font) faces))))))
    (dolist (base (sort (loop for k being the hash-keys of faces collect k) #'string<))
      (let* ((uses (reverse (gethash base faces)))
             (subsets (remove-duplicates
                        (remove nil (mapcar (lambda (u) (font-subset (first u))) uses))
                        :test #'string=))
             (embedded (remove-duplicates (mapcar (lambda (u) (font-embedded (first u))) uses))))
        (flet ((evidence ()
                 (loop for (font nil nil number) in (remove-duplicates uses :key #'fourth) repeat 6
                       when number
                       append (or (key-evidence pdf revision number "/BaseFont")
                                  (nth-value 0 (object-evidence pdf revision number))))))
          (when (> (length subsets) 1)
            (push (found :look (format nil "The font ~a is in the file as ~d subsets (~{~a+~a~^, ~}): text set at ~
                                            different times or by different programs, or pages joined."
                                       base (length subsets) (loop for s in subsets append (list s base)))
                         :rule (concatenate 'string "A program embeds each font once, as one subset tagged "
                                                    "ABCDEF+Name; two tags for one font mean two separate embeddings.")
                         :revision (revision-index revision)
                         :details (loop for (font page name) in uses
                                        collect (format nil "page ~d: ~a as ~:[~a~;~:*~a+~a~]"
                                                        page
                                                        (subseq name 1)
                                                        (font-subset font)
                                                        base))
                         :objects (remove nil (remove-duplicates (mapcar #'fourth uses)))
                         :evidence (evidence) :method :font-added)
                  out))
          (when (> (length embedded) 1)
            (push (found (if
                           (= 1
                              (length (remove-duplicates
                                        (mapcar (lambda (u) (font-subtype (first u))) uses)
                                        :test #'equal)))
                           :look :record)
                         (format nil "The font ~a is embedded on some pages and not on others: text added by another ~
                                      program, or ~:*~a written two ways by one (a simple and a composite font)." base)
                         :rule "One program writing one document embeds a font everywhere or nowhere."
                         :revision (revision-index revision)
                         :details (loop for (font page name) in uses
                                        collect (format nil "page ~d: ~a ~:[not embedded~;embedded~]"
                                                        page
                                                        (subseq name 1)
                                                        (font-embedded font)))
                         :objects (remove nil (remove-duplicates (mapcar #'fourth uses)))
                         :evidence (evidence) :method :font-added)
                  out)))))
    out))

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
           (dpi (m) (let ((w (pdf-get (mark-actual m) "/Width")) (placed (- (mark-x1 m) (mark-x0 m))))
                      (and (realp w) (> placed 1) (/ w (/ placed 72))))))
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
                           do (push (found :look (format nil "The picture ~a (object ~d) on page ~d has a ~a (~a) ~
                                                              unlike the other pictures: pasted in from elsewhere, as ~
                                                              a signature, stamp or logo is."
                                                         (mark-text (first i))
                                                         (mark-object (first i))
                                                         (1+ (page-index (second i)))
                                                         what
                                                         v)
                                           :rule (concatenate 'string "Pictures made by one program for one document "
                                                                      "share their compression, colour space and bit "
                                                                      "depth.")
                                           :revision (revision-index revision)
                                           :page (1+ (page-index (second i)))
                                           :objects (list (mark-object (first i)))
                                           :evidence (nth-value 0
                                                                (object-evidence pdf revision (mark-object (first i))))
                                           :method :picture-pasted)
                                    out))))
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
                      do (push (found :look (format nil "The picture ~a (object ~d) on page ~d is placed at about ~d ~
                                                         pixels to the inch, the others at about ~d: put in from ~
                                                         elsewhere."
                                                    (mark-text (first i))
                                                    (mark-object (first i))
                                                    (1+ (page-index (second i)))
                                                    (round r)
                                                    (round middle))
                                      :rule "Pictures of one scan or one program share a resolution."
                                      :revision (revision-index revision)
                                      :page (1+ (page-index (second i)))
                                      :objects (list (mark-object (first i)))
                                      :evidence (append (nth-value 0
                                                                   (object-evidence
                                                                     pdf
                                                                     revision
                                                                     (mark-object (first i))))
                                                        (mark-evidence pdf revision (first i) "the picture"))
                                      :method :picture-pasted)
                               out))))))
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
                           :method :picture-pasted)
                    out)))
          (let ((filters (mapcar #'car (pdf-stream-filters (mark-actual m)))))
            (when (and filters (jpeg-filter-p (car (last filters))))
              (let* ((object (revision-object pdf revision (mark-object m)))
                     (data (and object (if (rest filters) (stream-data pdf object) (raw-stream pdf object))))
                     (sections (and data (remove "" (ignore-errors (jpeg-metadata data)) :test #'string=))))
                (when sections
                  (let* ((text (format nil "~{~a~}" sections))
                         (tools (tools-in text)))
                    (push (found (if tools :look :record)
                                 (format nil "The JPEG picture ~a (object ~d) on page ~d keeps metadata of its own~@[, ~
                                              naming ~{~a~^, ~}~]: it was made or edited elsewhere."
                                         (mark-text m) (mark-object m) (1+ (page-index page)) (mapcar #'second tools))
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
                          out))))))))
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
              (push (found (cond
                             ((and later
                                   (member subtype '("/FreeText" "/Stamp" "/Ink" "/Square" "/Redact") :test #'string=))
                              :strong)
                             (later :look) (t :record))
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

;;; Signatures

(defun signer-name (octets)
  "The common name (CN) of the last certificate subject in OCTETS, a
signature's PKCS#7 data, read as data; NIL when none is found."
  (let ((mark (coerce '(6 3 85 4 3) 'octets)) (name nil))
    (loop for at = (octets-search mark octets) then (octets-search mark octets :start (1+ at))
          while at
          do (let ((tag-at (+ at 5)))
               (when (< (1+ tag-at) (length octets))
                 (let ((length (aref octets (1+ tag-at))))
                   (when (and (< length 128) (<= (+ tag-at 2 length) (length octets)))
                     (setf name (octets-string (subseq octets (+ tag-at 2) (+ tag-at 2 length)))))))))
    name))

(defun signatures (pdf revision)
  "The signatures REVISION holds, each (NUMBER DICT FIELD-NAME): every
dictionary with a /ByteRange among its objects."
  (let ((found '()))
    (dolist (n (live-numbers pdf revision))
      (let* ((object (revision-object pdf revision n)) (value (and object (pdf-indirect-get object :value))))
        (when (and (pdf-dictionary-p value) (listp (pdf-get value "/ByteRange")) (pdf-get value "/ByteRange"))
          (push (list n value (let ((field (find n
                                                 (form-fields pdf revision)
                                                 :key (lambda (f) (pdf-ref-p (pdf-get (third f) "/V"))))))
                                (and field (first field))))
                found))))
    (nreverse found)))

(defun change-kind (pdf before after number)
  "What a change to the object NUMBER from BEFORE to AFTER is: :annotation,
:form, :signature, :security (the signatures' validation data) or :content
\(anything else, which changes what the document shows)."
  (let* ((new (deref pdf after (list :ref number 0))) (old (deref pdf before (list :ref number 0)))
         (type (pdf-get new "/Type")) (subtype (pdf-get new "/Subtype")))
    (flet ((without (dict keys) (remove-if (lambda (p) (member (car p) keys :test #'string=)) dict)))
      (cond ((or (name-p type "/Sig") (name-p type "/DocTimeStamp") (pdf-get new "/ByteRange")) :signature)
            ((or (name-p type "/DSS")
                 (pdf-get new "/VRI")
                 (pdf-get new "/Certs")
                 (pdf-get new "/OCSPs")
                 (pdf-get new "/CRLs")) :security)
            ((or (name-p type "/Annot") (and (stringp subtype) (pdf-get new "/Rect") (not (pdf-get new "/FT"))))
             :annotation)
            ((or (pdf-get new "/FT") (pdf-get new "/Fields") (and (pdf-get new "/T") (pdf-get new "/Kids"))) :form)
            ((and (pdf-dictionary-p old) (pdf-dictionary-p new) (name-p type "/Page")
                  (equal (without old '("/Annots")) (without new '("/Annots"))))
             :annotation)
            ((and (pdf-dictionary-p old) (pdf-dictionary-p new) (name-p type "/Catalog")
                  (equal (without old '("/AcroForm" "/DSS" "/Extensions" "/Perms"))
                         (without new '("/AcroForm" "/DSS" "/Extensions" "/Perms"))))
             :form)
            ((let ((roles (roles-of pdf after))) (search "annotation" (or (gethash number roles) ""))) :annotation)
            ((and (name-p type "/XObject") (name-p subtype "/Form") (not (gethash number (roles-of pdf after))))
             :annotation)
            (t :content)))))

(defun check-signatures (pdf)
  "Each signature: what it claims, whether its bytes cover its whole save,
what later saves change, and objects hidden in the signed part brought into
use later."
  (let* ((last (last-revision pdf)) (octets (pdf-octets pdf)) (out '()))
    (dolist (signature (signatures pdf last))
      (guarding-item (out "the signature in object ~d" (first signature))
        (destructuring-bind (number dict field) signature
          (let* ((range (pdf-get dict "/ByteRange"))
                 (written (written-in pdf last number))
                 (signed-end (and (= (length range) 4) (every #'integerp range) (+ (third range) (fourth range))))
                 (covered (and signed-end
                               (find-if (lambda (r)
                                          (and (<= (+ (revision-eof r) 5) signed-end) (<= signed-end (revision-end r))))
                                        (pdf-revisions pdf))))
                 (contents (pdf-get dict "/Contents"))
                 (raw (and (stringp contents) (string-octets-as-read contents)))
                 (reference (find-if (lambda (r) (name-p (pdf-get (deref pdf last r) "/TransformMethod") "/DocMDP"))
                                     (pdf-array-items (dref pdf last dict "/Reference"))))
                 (permission (or (let ((p (pdf-get (dref pdf last (deref pdf last reference) "/TransformParams") "/P")))
                                   (and (integerp p) p))
                                 (and reference 2)))
                 (evidence (append (nth-value 0 (object-evidence pdf last number))
                                   (key-evidence pdf last number "/ByteRange")))
                 (steps (list (format nil "Read /ByteRange ~a in object ~d: the octets from ~a for ~a, and from ~a for ~
                                           ~a, are what was signed."
                                      range number (first range) (second range) (third range) (fourth range)))))
            (push (found :record (format nil "~:[An approval~;A certification~] signature~@[ in field ~s~] (object ~
                                              ~d)~@[ by ~a~]~@[, dated ~a~]~@[, for ~s~]~@[, ~a~]; it is not checked ~
                                              here, only what it covers."
                                         reference field number (and raw (signer-name raw))
                                         (let ((m (pdf-get dict "/M"))) (and (stringp m) (pdf-date-words m)))
                                         (let ((reason (pdf-get dict "/Reason"))) (and (stringp reason) reason))
                                         (let ((s (pdf-get dict "/SubFilter"))) (and (stringp s) (subseq s 1))))
                         :rule (concatenate 'string "A signature dictionary says who signed, when, why and how, and "
                                                    "/ByteRange names the octets it vouches for.")
                         :revision (if written (revision-index written) (revision-index last)) :objects (list number)
                         :evidence evidence :steps steps)
                  out)
            (cond ((not signed-end)
                   (push (found :strong (format nil "The signature in object ~d has a /ByteRange that cannot be read: ~
                                                     it vouches for nothing that can be told." number)
                                :rule "A /ByteRange is four numbers."
                                :revision (revision-index last)
                                :objects (list number)
                                :evidence evidence
                                :method :signature-bypassed)
                         out))
                  (t
                   (let ((gap-ok (and (zerop (first range))
                                      (<= 0 (second range))
                                      (< (second range) (third range))
                                      (< (third range) (length octets))
                                      (= (aref octets (second range)) 60) (= (aref octets (1- (third range))) 62))))
                     (unless gap-ok
                       (push (found :strong (format nil "The signature in object ~d leaves out more than its own ~
                                                         value: its /ByteRange does not begin at 0 or its gap is not ~
                                                         just /Contents." number)
                                    :rule (concatenate 'string "A signature's /ByteRange covers the file from octet 0, "
                                                               "leaving out only the <...> of its own /Contents.")
                                    :revision (revision-index last) :objects (list number)
                                    :evidence (append evidence (and (< -1 (second range) (length octets))
                                                                    (octets-evidence "Where the gap begins"
                                                                                     octets
                                                                                     (second range)
                                                                                     (min (length octets)
                                                                                          (+ (second range) 16)))))
                                    :method :signature-bypassed)
                             out)))
                   (cond ((null covered)
                          (push (found :strong (format nil "The signature in object ~d stops short: its bytes end at ~
                                                            ~a, where no save ends." number (offset-words signed-end))
                                       :rule (concatenate 'string "A signature covers the whole of the save it was "
                                                                  "made in, to the end of its %%EOF line.")
                                       :revision (revision-index last)
                                       :objects (list number)
                                       :evidence evidence
                                       :steps steps
                                       :method :signature-bypassed)
                                out))
                         ((and written (/= (revision-index covered) (revision-index written)))
                          (push (found :strong (format nil "The signature in object ~d, written in ~a, covers only up ~
                                                            to the end of ~a."
                                                       number (revision-name written) (revision-name covered))
                                       :rule "A signature covers the save it is written in."
                                       :revision (revision-index written)
                                       :objects (list number)
                                       :evidence evidence
                                       :steps steps
                                       :method :signature-bypassed)
                                out)))
                   ;; what later saves change
                   (when covered
                     (loop for k from (1+ (revision-index covered)) below (length (pdf-revisions pdf))
                           do (let* ((r (aref (pdf-revisions pdf) k)) (before (aref (pdf-revisions pdf) (1- k)))
                                     (kinds (loop for n in (append (revision-changed r) (revision-added r))
                                                  collect (cons n (change-kind pdf before r n))))
                                     (content (remove :content kinds :key #'cdr :test-not #'eq))
                                     (annotations (remove :annotation kinds :key #'cdr :test-not #'eq))
                                     (forms (remove :form kinds :key #'cdr :test-not #'eq))
                                     (allowed-annotations (or (null permission) (>= permission 3)))
                                     (allowed-forms (or (null permission) (>= permission 2))))
                                (cond (content
                                       (push (found :strong (format nil "~@(~a~), after the signature in object ~d, ~
                                                                         changes what the document shows (object~p ~
                                                                         ~{~d~^, ~}): the signature does not vouch for ~
                                                                         what is shown."
                                                                    (revision-name r)
                                                                    number
                                                                    (length content)
                                                                    (mapcar #'car content))
                                                    :rule (concatenate 'string "Saves after a signature may add "
                                                                               "signatures, validation data, form "
                                                                               "values and annotations as its "
                                                                               "permissions allow; any other change "
                                                                               "alters what was signed.")
                                                    :revision k :objects (mapcar #'car content)
                                                    :details (loop for (n) in content
                                                                   repeat 20
                                                                   collect (format nil "object ~d~@[, ~a~]"
                                                                                   n
                                                                                   (role-words pdf r n)))
                                                    :evidence (loop for (n) in content
                                                                    repeat 10
                                                                    append (nth-value 0 (object-evidence pdf r n)))
                                                    :method :signature-bypassed)
                                             out))
                                      ((or (and annotations (not allowed-annotations))
                                           (and forms (not allowed-forms))
                                           (and permission (= permission 1) kinds))
                                       (push (found :strong (format nil "~@(~a~), after the signature in object ~d, ~
                                                                         makes changes its permissions (/P ~a) do not ~
                                                                         allow." (revision-name r) number permission)
                                                    :rule (concatenate 'string "A certification signature's /P allows "
                                                                               "no changes (1), form filling and "
                                                                               "signing (2), or those and annotations "
                                                                               "(3).")
                                                    :revision k :objects (mapcar #'car kinds)
                                                    :method :signature-bypassed)
                                             out))
                                      (kinds
                                       (push (found :look (format nil "~@(~a~), after the signature in object ~d, adds ~
                                                                       or changes ~{~a~^, ~}." (revision-name r) number
                                                                  (remove-duplicates
                                                                    (mapcar (lambda (k)
                                                                              (ecase (cdr k)
                                                                                (:annotation "annotations")
                                                                                (:form "form values")
                                                                                (:signature "signatures")
                                                                                (:security "validation data")))
                                                                            kinds)
                                                                    :test #'string=))
                                                    :rule (concatenate 'string "Changes after a signature are allowed "
                                                                               "when they are signatures, validation "
                                                                               "data, form values or annotations.")
                                                    :revision k :objects (mapcar #'car kinds))
                                             out)))))
                     ;; shadow attacks: in the signed part, unused, then used
                     (let ((signed-reach (reachable pdf covered)) (final-reach (reachable pdf last)) (shadows '()))
                       (maphash (lambda (n v) (declare (ignore v))
                                  (unless (gethash n signed-reach)
                                    (let ((w (written-in pdf last n)))
                                      (when (and w
                                                 (<= (revision-index w) (revision-index covered))
                                                 (not (structural-p pdf last n)))
                                        (push n shadows)))))
                                final-reach)
                       (when shadows
                         (setf shadows (sort shadows #'<))
                         (push (found :strong (format nil "Object~p ~{~d~^, ~} ~:*~:*~[~;was~:;were~]~* hidden, ~
                                                           unused, in the part the signature in object ~d covers, and ~
                                                           brought into use by a later save: a shadow attack."
                                                      (length shadows) shadows number)
                                      :rule (concatenate 'string "An object in the signed octets that the signed "
                                                                 "document never uses, but a later save does, lets "
                                                                 "what was signed show something else.")
                                      :revision (revision-index last) :objects shadows
                                      :evidence (loop for n in shadows
                                                      repeat 10
                                                      append (nth-value 0 (object-evidence pdf last n)))
                                      :method :signature-bypassed)
                               out))))))))))
    out))

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
                                         :method :pages-changed)
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
                              (format nil "~:[The file is encrypted~;~:*~@(~a~) encrypts the file~]~@[ (~a~]~@[, ~
                                           version ~a~]~@[, revision ~a)~]~@[; ~a~]: its streams and strings are not ~
                                           read here."
                                      (and before (revision-name r))
                                      (let ((f (pdf-get encrypt "/Filter"))) (and (stringp f) (subseq f 1)))
                                      (pdf-get encrypt "/V")
                                      (pdf-get encrypt "/R")
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
never more, as nothing of the file names that save."
  (let* ((earlier (read-pdf (subseq (pdf-octets pdf) 0 (+ eof 5)) :name (pdf-name pdf)))
         (before (last-revision earlier))
         (revision (or (revision-of-offset pdf eof) (last-revision pdf)))
         (old-pages (pages-of earlier before)))
    (loop for page in (pages-of pdf revision) for k from 0
          for old-rest = old-pages then (rest old-rest)
          for old = (first old-rest)
          for old-marks = (if old (marks-of earlier before old) #())
          for new-marks = (marks-of pdf revision page)
          for text = (line-diff (text-lines old-marks) (text-lines new-marks))
          when (or text (null old))
            collect (found :look (format nil "Page ~d reads otherwise in ~a than in a save inferred within the data of ~
                                              stream ~d (its octets up to the %%EOF at ~a): ~:d line~:p of its text ~
                                              differ. That earlier save is inferred from octets the stream's /Length ~
                                              covers; nothing of the file names it."
                                           (1+ k) (revision-name revision) number (offset-words eof) (length text))
                           :rule (concatenate 'string "A save hidden within a stream is read as the octets up to its "
                                              "%%EOF, and its pages are set beside the save that holds it.")
                           :revision (revision-index revision) :page (1+ k)
                           :details (loop for (kind line) in text repeat 12
                                          collect (format nil "~:[-~;+~] ~a" (eq kind :added)
                                                          (shorten-string line 100 :one-line t)))))))

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

;;; All of them

(defun run-check (name function &rest arguments)
  "The findings FUNCTION finds with ARGUMENTS; when it fails on what the file
holds, or runs out of stack or memory on it, one finding saying the check
named NAME could not finish."
  (handler-case (progn (job-check-stop) (apply function arguments))
    ((or error storage-condition) (e) (list (unfinished name e)))))

(defun all-findings (pdf accounts &optional (progress (constantly nil)))
  "Every sign found in PDF, ACCOUNTS being its revisions compared: the
strongest first, then by revision and page; each placed in a revision (the
latest when it is of the file as a whole)."
  (let* ((last (last-revision pdf))
         (pages (pages-of pdf last))
         (count (max 1 (length pages)))
         (findings
           (append (run-check "the saves" #'check-saves pdf accounts)
                   (run-check "the cross-reference sections" #'check-revision-problems pdf)
                   (run-check "the cross-reference offsets" #'check-offsets pdf)
                   (run-check "freed objects" #'check-generations pdf)
                   (run-check "unused objects" #'check-leftovers pdf)
                   (run-check "objects written twice" #'check-twice pdf)
                   (run-check "the file's ends" #'check-ends pdf)
                   (run-check "streams left open" #'check-unended-streams pdf)
                   (run-check "streams hiding a save" #'check-lying-streams pdf)
                   (run-check "saves within streams" #'check-save-signs pdf)
                   (run-check "the kinds of cross-reference" #'check-index-kinds pdf)
                   (run-check "comments" #'check-comments pdf)
                   (run-check "object streams" #'check-packed-changes pdf)
                   (run-check "stream lengths" #'check-lengths pdf)
                   (run-check "the metadata" #'check-metadata pdf)
                   (loop for page in pages for k from 1
                         append (progn (funcall progress (/ k count))
                                       (run-check (format nil "page ~d" k) #'check-page-content pdf last page)))
                   (run-check "the fonts" #'check-fonts pdf last)
                   (run-check "the pictures" #'check-images pdf last)
                   (run-check "the scanned pages" #'check-scans pdf last)
                   (run-check "the scanned pages' JPEG blocks" #'check-scan-pieces pdf last)
                   (run-check "the annotations" #'check-annotations pdf last)
                   (run-check "the form" #'check-forms pdf)
                   (run-check "the layers" #'check-layers pdf last)
                   (run-check "the signatures" #'check-signatures pdf)
                   (run-check "the pages" #'check-pages pdf)
                   (run-check "actions and embedded files" #'check-actions pdf last)
                   (run-check "encryption" #'check-encryption pdf))))
    (dolist (f findings)
      (unless (finding-revision f) (setf (finding-revision f) (revision-index last))))
    (stable-sort findings
                 (lambda (a b)
                   (let ((sa (position (finding-strength a) *strengths*))
                         (sb (position (finding-strength b) *strengths*)))
                     (cond ((/= sa sb) (< sa sb))
                           ((/= (finding-revision a) (finding-revision b))
                            (< (finding-revision a) (finding-revision b)))
                           (t (< (or (finding-page a) 0) (or (finding-page b) 0)))))))))
