;;;; picture.lisp --- A picture file examined as one scanned page, and what it says of itself.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A PNG, JPEG or TIFF, as a scanner, a phone or a picture editor saves one,
;;;; is examined as a PDF's scanned page is: its pixels, which Tuatara turns
;;;; into pixels in the sandbox, are read for words unlike the rest, characters
;;;; copied and pieces with JPEG blocks of their own. What it says of itself
;;;; (EXIF, XMP, a PNG's text and time, a JPEG's comments, a TIFF's tags) is
;;;; read as data, never run, and a JPEG's steps are measured for a second
;;;; save. The checks of a PDF's pages read a page of the package's own making
;;;; that draws the picture over the whole of it.

(in-package #:palimpsest)

(defun scan-named (mark &key short)
  "How a finding names the scanned picture MARK: its name and object number,
the name alone with SHORT; the picture while a picture file is examined
\(*PICTURE-FILE*)."
  (cond (*picture-file* "the picture")
        (short (format nil "the scanned picture ~a" (mark-text mark)))
        (t (format nil "the scanned picture ~a (object ~d)" (mark-text mark) (mark-object mark)))))

(defun picture-number (number)
  "NUMBER, the object number of a scanned picture as a finding's details name
it, or NIL while a picture file is examined (*PICTURE-FILE*), its one
picture the file itself."
  (unless *picture-file* number))

;;; Reading the picture

(defun picture-kind-of (path)
  "The kind of picture file at PATH, a key of *PICTURE-KINDS*, told by its
first octets whatever its name; NIL for any other file."
  (let ((head (file-head path)))
    (and head (car (assoc (nth-value 1 (octets-kind head)) *picture-kinds*)))))

(defun read-picture-file (octets kind)
  "The picture file of KIND whose contents are OCTETS, read as a
PICTURE-FILE: what it says of itself read as data (OCTETS-METADATA), a
JPEG's tables (JPEG-TABLES), and its pixels, which Tuatara turns into
pixels in the sandbox (DECODE-PICTURE), at most *PALIMPSEST-SCAN-PIXELS* of
them. The picture is turned as its EXIF says it is held. When its pixels
cannot be read, its REFUSAL says why; when what it says of itself cannot be
read, even for want of memory, a section says so."
  (let ((file (make-picture-file
               :kind kind
               ;; a file made to run the reading out of memory loses what it says of itself, not the examining
               :sections (handler-case (remove "" (octets-metadata octets kind) :test #'string=)
                           ((or error storage-condition) (e)
                             (list (format nil "* Not read~%- Why :: ~a~%" (shorten-string (princ-to-string e) 200
                                                                                         :one-line t)))))
               :tables (handler-case (and (eq kind :jpeg) (multiple-value-list (jpeg-tables octets)))
                         ((or error storage-condition) () nil)))))
    (multiple-value-bind (image notes) (decode-picture octets kind 16384 16384)
      (cond ((null image)
             (setf (picture-file-refusal file)
                   (string-trim ", " (or (car (first notes))
                                         (format nil "the ~a could not be decoded"
                                                 (cdr (assoc kind *picture-kinds*)))))))
            ((> (* (image-width image) (image-height image)) *palimpsest-scan-pixels*)
             (setf (picture-file-refusal file)
                   (format nil "it has ~:d pixels, more than *palimpsest-scan-pixels* allows"
                           (* (image-width image) (image-height image)))))
            (t
             (let* ((pixels (image-pixels image)) (channels (image-channels image))
                    (coloured (loop for at from 0 below (length pixels) by channels
                                    thereis (or (/= (aref pixels at) (aref pixels (+ at 1)))
                                                (/= (aref pixels at) (aref pixels (+ at 2)))))))
               (setf (picture-file-width file) (image-width image)
                     (picture-file-height file) (image-height image)
                     (picture-file-grey file) (image-grey image)
                     (picture-file-colours file) (and coloured (cons pixels channels)))))))
    file))

(defun picture-page-octets (width height)
  "A PDF of one page WIDTH by HEIGHT points that draws one picture of WIDTH by
HEIGHT pixels over the whole of it, the picture's samples left out: the
page a picture file is examined as, its pixels those of *PICTURE-FILE*."
  (let* ((content (format nil "q ~d 0 0 ~d 0 0 cm /Im1 Do Q" width height))
         (bodies (list "<< /Type /Catalog /Pages 2 0 R >>"
                       "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                       (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d ~d] /Resources << /XObject << /Im1 ~
                                    4 0 R >> >> /Contents 5 0 R >>"
                               width height)
                       (format nil "<< /Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace /DeviceGray ~
                                    /BitsPerComponent 8 /Length 0 >>~%stream~%~%endstream"
                               width height)
                       (format nil "<< /Length ~d >>~%stream~%~a~%endstream" (length content) content)))
         (offsets '())
         (text (with-output-to-string (s)
                 (format s "%PDF-1.4~%")
                 (loop for body in bodies for n from 1
                       do (push (file-position s) offsets)
                          (format s "~d 0 obj~%~a~%endobj~%" n body))
                 (let ((xref (file-position s)))
                   (format s "xref~%0 ~d~%0000000000 65535 f ~%" (1+ (length bodies)))
                   (dolist (at (reverse offsets)) (format s "~10,'0d 00000 n ~%" at))
                   (format s "trailer~%<< /Size ~d /Root 1 0 R >>~%startxref~%~d~%%%EOF~%"
                           (1+ (length bodies)) xref)))))
    (map '(simple-array (unsigned-byte 8) (*)) #'char-code text)))

;;; What it says of itself

(defun picture-fields (sections)
  "The fields of SECTIONS, sections of a note as OCTETS-METADATA gives them,
each (HEADING LABEL VALUE): the section's heading, and its line's label and
value as the note writes them, the label ending at the line's first ::,
which the note never writes within a label, whatever the file calls it."
  (loop for section in sections
        for lines = (uiop:split-string section :separator '(#\Newline))
        for heading = (string-left-trim "* " (first lines))
        append (loop for line in (rest lines)
                     for split = (search " :: " line)
                     when (and split (uiop:string-prefix-p "- " line))
                       collect (list heading (subseq line 2 split) (subseq line (+ split 4))))))

(defparameter *picture-program-labels* '("software" "creatortool" "comment" "history" "processing" "producer")
  "Words in the label of a field of a picture's metadata that names the
programs it went through: EXIF's Software, XMP's CreatorTool and edit
history, a JPEG's or PNG's comment (\"Created with GIMP\").")

(defparameter *picture-made-labels*
  '("DateTimeOriginal" "DateTimeDigitized" "xmp:CreateDate" "exif:DateTimeOriginal" "photoshop:DateCreated")
  "The fields of a picture's metadata that say when it was made, as their labels are.")

(defparameter *picture-changed-labels* '("DateTime" "xmp:ModifyDate" "Last changed (tIME)")
  "The fields of a picture's metadata that say when it was last changed, as their labels are.")

(defparameter *picture-dates-apart* 60
  "The most seconds two dates of a picture's metadata, when it was made and
when it was changed, may lie apart and agree, when both are of its EXIF or
both name their zone. A camera or scanner writes both at once; a picture
editor writes the second when it saves.")

(defparameter *picture-zones-apart* (* 26 3600)
  "The most seconds a date of a picture's metadata that names no zone may lie
from one of another kind and agree: the widest the zones of the world lie
apart, 26 hours, as the EXIF's own dates name none and a PNG's time is
universal.")

(defun picture-date (text)
  "TEXT, a date as a picture's metadata says it, written as XMP writes one:
EXIF's 2026:03:02 10:00:00 as 2026-03-02T10:00:00, a PNG's time in
universal time with its zone; else TEXT as it is."
  (cond ((and (>= (length text) 19) (every #'digit-char-p (subseq text 0 4)) (char= (char text 4) #\:)
              (char= (char text 7) #\:) (char= (char text 10) #\Space))
         (format nil "~a-~a-~aT~a" (subseq text 0 4) (subseq text 5 7) (subseq text 8 10) (subseq text 11 19)))
        ((and (>= (length text) 23) (string= (subseq text 19 23) " UTC"))
         (format nil "~aT~aZ" (subseq text 0 10) (subseq text 11 19)))
        (t text)))

(defun picture-dates-disagree-p (made changed)
  "True when MADE and CHANGED, each (LABEL VALUE) of a picture's metadata,
lie further apart than they may (*PICTURE-DATES-APART*,
*PICTURE-ZONES-APART*)."
  (let* ((a (picture-date (second made))) (b (picture-date (second changed)))
         (apart (dates-apart a b))
         (zoned (lambda (text) (nth-value 1 (date-of text))))
         (strict (or (and (not (find #\: (first made))) (not (find #\: (first changed)))
                          (not (search "tIME" (first changed))))
                     (and (funcall zoned a) (funcall zoned b)))))
    (and apart (> apart (if strict *picture-dates-apart* *picture-zones-apart*)))))

(defun dates-among (fields labels)
  "The first *PICTURE-FIELDS-LISTED* different fields of FIELDS (as
PICTURE-FIELDS gives them) whose label is one of LABELS, each (LABEL VALUE)."
  (let ((out '()))
    (loop for (nil label value) in fields
          while (< (length out) *picture-fields-listed*)
          when (and (member label labels :test #'string=) (not (member (list label value) out :test #'equal)))
            do (push (list label value) out))
    (nreverse out)))

(defun check-picture-metadata (file)
  "What FILE, a PICTURE-FILE, says of itself. For the record, the programs,
device and dates it names, with every field in the details. Worth a look, a
picture editor named, the more so beside a camera's or scanner's own name
(an honest crop, turn or lightening is saved so too, and some scanner and
phone apps name such a program); and dates of when it was made and changed
that disagree (an honest picture turned in a phone's gallery or saved again
by a viewer gives that too)."
  (let* ((fields (picture-fields (picture-file-sections file)))
         (name (cdr (assoc (picture-file-kind file) *picture-kinds*)))
         (named (picture-programs fields))
         (programs (nth-value 1 (picture-programs fields)))
         (editors (remove :editor named :key #'third :test-not #'eq))
         (device (let ((make (third (find "Make" fields :key #'second :test #'string=)))
                       (model (third (find "Model" fields :key #'second :test #'string=))))
                   (and (or make model) (format nil "~@[~a~]~:[~; ~]~@[~a~]" make (and make model) model))))
         ;; each date once, the first of them: a file can hold a great many
         (made (dates-among fields *picture-made-labels*))
         (changed (dates-among fields *picture-changed-labels*))
         (apart (loop for m in made
                      thereis (loop for c in changed
                                    when (picture-dates-disagree-p m c) return (list m c))))
         (details (append (loop for (heading label value) in fields
                                repeat *picture-details-most*
                                collect (format nil "~a: ~a: ~a" heading label value))
                          (and (> (length fields) *picture-details-most*)
                               (list (format nil "and ~:d fields more" (- (length fields) *picture-details-most*))))))
         (out '()))
    (when fields
      (push (found :record (format nil "What the ~a says of itself is listed~:[~;: ~:*~{~a~^; ~}~]."
                                   name
                                   (append (and device (list (format nil "made by ~a" device)))
                                           (and named (list (format nil "naming ~{~a~^, ~}" (mapcar #'second named))))
                                           (mapcar (lambda (d) (format nil "~a ~a" (first d) (second d)))
                                                   (append made changed))))
                   :rule (concatenate 'string "What a picture says of itself (EXIF, XMP, a PNG's text, a JPEG's "
                                              "comments, a TIFF's tags) is read as data, never run, and given "
                                              "whole for the record.")
                   :details details)
            out))
    (when editors
      (push (found :look (format nil "The ~a names ~{~a~^, ~} in what it says of itself (~a): opened in a ~
                                      picture editor and saved again~@[, after ~a made it~]. An honest crop, turn or ~
                                      lightening is saved so too, and some scanner and phone apps name such a program."
                                 name
                                 (mapcar (lambda (e) (format nil "~a (~a)" (second e) (fourth e))) editors)
                                 (listed-words (let ((seen (make-hash-table :test 'equal)))
                                                 (loop for (label value) in programs
                                                       for words = (format nil "~a ~a" label value)
                                                       unless (gethash words seen)
                                                         collect (setf (gethash words seen) words))))
                                 device)
                   :rule (concatenate 'string "A picture editor writes its name into the pictures it saves (EXIF's "
                                              "Software, XMP's CreatorTool and history, a comment). Worth a look, "
                                              "not proof: it tells that the picture was saved by such a program, "
                                              "not what was changed.")
                   :details details
                   :method :picture-editor)
            out))
    (when apart
      (destructuring-bind ((made-label made-value) (changed-label changed-value)) apart
        (push (found :look (format nil "The ~a says it was made ~a (~a) and changed ~a (~a): saved again after it was ~
                                        made. A picture turned or cropped in a phone's gallery, or saved again by a ~
                                        viewer, gives that too."
                                   name made-value made-label changed-value changed-label)
                     :rule (format nil "A camera or scanner writes when a picture was made and when it was changed ~
                                        at once; a program that saves it again writes the second anew. Dates of ~
                                        its EXIF, or both naming their zone, agree within ~d seconds; others, one ~
                                        of which names no zone, within ~d hours, as far as zones lie apart."
                                   *picture-dates-apart* (round *picture-zones-apart* 3600))
                     :details details
                     :method :picture-editor)
              out)))
    (nreverse out)))

(defparameter *picture-fields-listed* 10
  "The most fields of one kind a finding on what a picture says of itself
names in its words (programs, dates), the rest counted: a file can hold a
great many.")

(defparameter *picture-details-most* 200
  "The most fields of what a picture says of itself given in a finding's
details, the rest counted.")

(defun picture-programs (fields)
  "The entries of *PDF-TOOLS* that those of FIELDS (as PICTURE-FIELDS gives
them) naming the programs a picture went through (*PICTURE-PROGRAM-LABELS*)
name, each once, and those fields, each (LABEL VALUE), as two values. A
value held by many fields is looked into once."
  (let* ((programs (loop for (nil label value) in fields
                         when (some (lambda (word) (search word (string-downcase label))) *picture-program-labels*)
                           collect (list label value)))
         (seen (make-hash-table :test 'equal)))
    (values (remove-duplicates (loop for (nil value) in programs
                                     unless (gethash value seen)
                                       append (progn (setf (gethash value seen) t) (tools-in value)))
                               :key #'second :test #'string= :from-end t)
            programs)))

(defun listed-words (items)
  "ITEMS, strings, as words: the first *PICTURE-FIELDS-LISTED* of them, joined
by semicolons, and how many more there are."
  (format nil "~{~a~^; ~}~@[ and ~:d more~]"
          (subseq items 0 (min (length items) *picture-fields-listed*))
          (and (> (length items) *picture-fields-listed*) (- (length items) *picture-fields-listed*))))

(defun picture-traits (file)
  "The traits of FILE, a PICTURE-FILE, for its fingerprint, each (KIND VALUE)
as FINGERPRINT gives them: each picture editor it names, :editor, each other
program, :made-by, and a JPEG's tables, :jpeg."
  (append (loop for tool in (picture-programs (picture-fields (picture-file-sections file)))
                collect (list (if (eq (third tool) :editor) :editor :made-by) (second tool)))
          (destructuring-bind (&optional octets quality table) (picture-file-tables file)
            (declare (ignore table))
            (and octets (list (list :jpeg (format nil "tables ~a~@[, about quality ~d~]" (fnv-hex octets) quality)))))))

;;; A JPEG saved twice

(defparameter *zigzag*
  #(0 1 8 16 9 2 3 10 17 24 32 25 18 11 4 5 12 19 26 33 40 48 41 34 27 20 13 6 7 14 21 28
    35 42 49 56 57 50 43 36 29 22 15 23 30 37 44 51 58 59 52 45 38 31 39 46 53 60 61 54 47 55 62 63)
  "For each step of a JPEG's quantisation table in the order it keeps them,
the frequency it is of: across plus eight times down.")

(defun standard-table-quality (table)
  "The quality from 1 to 100 at which libjpeg, the library most programs save
JPEGs with, makes TABLE (64 steps in the order a JPEG keeps them) from the
standard's luminance table; NIL when TABLE is none of those, a table of a
camera's or scanner's own."
  (loop for quality from 1 to 100
        for scale = (if (< quality 50) (floor 5000 quality) (- 200 (* 2 quality)))
        when (every (lambda (step base) (= step (max 1 (min 255 (floor (+ (* base scale) 50) 100)))))
                    table *jpeg-luminance-table*)
          return quality))

(defparameter *requantised-frequencies* 15
  "How many of a JPEG's lowest frequencies after the first (its blocks' mean),
in the order it keeps them, are measured for a second save: those most
blocks hold enough of to count.")

(defparameter *requantised-largest* 30
  "The most steps from nothing a frequency of a block is counted at, when a
JPEG is measured for a second save.")

(defparameter *requantised-blocks-most* 60000
  "The most blocks of a picture measured for a second save, spread over it.")

(defparameter *requantised-counted-least* 100
  "How many blocks the fuller of a step's two neighbours must hold for the
step to be measured for a second save: fewer count too unsteadily to tell a
gap.")

(defun clipped-pixels (grey colours)
  "Which pixels of GREY, a picture's grey levels, are clipped: black or white,
or, for a picture in COLOURS ((OCTETS . CHANNELS) as PICTURE-GREY gives
them), one of their red, green and blue at its least or most. A JPEG's
decoder clips there, so that no step tells what was saved. A vector of bits,
1 for clipped."
  (let ((clipped (make-array (length grey) :element-type 'bit :initial-element 0)))
    (declare (type grey-pixels grey) (type simple-bit-vector clipped))
    (dotimes (i (length grey))
      (let ((g (aref grey i))) (when (or (= g 0) (= g 255)) (setf (sbit clipped i) 1))))
    (when colours
      (destructuring-bind (octets . channels) colours
        (declare (type grey-pixels octets) (type fixnum channels))
        (dotimes (i (length grey))
          (let ((at (* i channels)))
            (dotimes (c 3)
              (let ((o (aref octets (+ at c)))) (when (or (= o 0) (= o 255)) (setf (sbit clipped i) 1))))))))
    clipped))

(defun block-frequencies (grey clipped width x0 y0 positions out)
  "Into OUT, the frequencies of POSITIONS (across plus eight times down) of
the 8-by-8 block of GREY (WIDTH pixels to a row) whose first pixel is at X0,
Y0, as a JPEG works them out (*BLOCK-BASIS*); NIL, OUT untouched, when the
block holds a pixel CLIPPED says is (CLIPPED-PIXELS), which no JPEG step
tells."
  (declare (type grey-pixels grey) (type simple-bit-vector clipped) (type fixnum width x0 y0)
           (type (simple-array double-float (*)) out))
  (dotimes (y 8)
    (let ((at (+ (* (+ y0 y) width) x0)))
      (dotimes (x 8)
        (when (= 1 (sbit clipped (+ at x))) (return-from block-frequencies nil)))))
  (let ((basis *block-basis*))
    (declare (type (simple-array double-float (64)) basis))
    (loop for p across positions for k fixnum from 0
          do (let ((u (mod p 8)) (v (floor p 8)) (sum 0d0))
               (declare (type double-float sum))
               (dotimes (y 8)
                 (let ((at (+ (* (+ y0 y) width) x0)) (row 0d0))
                   (declare (type double-float row))
                   (dotimes (x 8) (incf row (* (aref basis (+ (* u 8) x)) (aref grey (+ at x)))))
                   (incf sum (* (aref basis (+ (* v 8) y)) row))))
               (setf (aref out k) sum)))
    out))

(defun jpeg-grid (grey clipped width height positions steps)
  "Where the 8-by-8 blocks of the JPEG whose pixels are GREY (WIDTH by HEIGHT)
lie, as the column and row of the first one's corner, each from 0 to 7, and
how far from its steps its frequencies lie there: the mean distance from the
nearest whole step of POSITIONS' frequencies over STEPS, those a step or more
from nothing, 0 on the steps, a quarter for none. A picture turned as its
EXIF says may keep its blocks off the corner. At most 20,000 blocks spread
over the picture are tried for each corner, those with a pixel CLIPPED says
is passed over (CLIPPED-PIXELS). NIL when they hold fewer than 200 such
frequencies."
  (let ((out (make-array (length positions) :element-type 'double-float))
        (best nil) (best-x 0) (best-y 0)
        (stride (max 1 (floor (* (floor width 8) (floor height 8)) 20000))))
    (dotimes (oy 8)
      (dotimes (ox 8)
        (let ((sum 0d0) (count 0) (k 0))
          (loop for y0 from oy to (- height 8) by 8
                do (loop for x0 from ox to (- width 8) by 8
                         do (when (zerop (mod (incf k) stride))
                              (when (block-frequencies grey clipped width x0 y0 positions out)
                                (loop for f across out for q across steps
                                      for c = (/ f q)
                                      when (>= (abs c) 0.75d0)
                                        do (incf sum (abs (- c (fround c)))) (incf count))))))
          (when (>= count 200)
            (let ((mean (/ sum count)))
              (when (or (null best) (< mean best)) (setf best mean best-x ox best-y oy)))))))
    (and best (values best-x best-y best))))

(defun requantised-measure (grey width height table &optional colours)
  "How far the frequencies of the JPEG whose pixels are GREY (WIDTH by
HEIGHT, in COLOURS as PICTURE-GREY gives them when it has them), saved with
TABLE (its 64 steps in the order it keeps them), count unevenly over their
steps, as a second save leaves them. Its blocks are found by the first five
of those frequencies (JPEG-GRID). For each of its lowest
*REQUANTISED-FREQUENCIES* frequencies whose step is 2 or more, the blocks
are counted at each step from nothing, both signs together; each step from
the second the fuller of whose neighbours holds *REQUANTISED-COUNTED-LEAST*
blocks or more is set beside them, the logarithm of its count over theirs
\(their geometric mean, each count one more). Saved once, counts fall evenly
and that is near nothing; saved twice with other steps first, some steps
are left all but empty and others crowded, regularly. As three values: the
middle over the frequencies of each one's mean, the frequencies measured,
each (POSITION STEP MEAN EMPTY), EMPTY how many of its steps held a tenth
of their neighbours or less, and how far from its steps the picture lies
\(JPEG-GRID). NIL, and why not in words, when it cannot be measured."
  (let* ((chosen (loop for k from 1 to *requantised-frequencies*
                       for q = (elt table k)
                       when (>= q 2) collect (list (aref *zigzag* k) q)))
         (positions (coerce (mapcar #'first chosen) '(simple-array fixnum (*))))
         (steps (coerce (mapcar (lambda (c) (float (second c) 1d0)) chosen) '(simple-array double-float (*))))
         (clipped (clipped-pixels grey colours)))
    (when (null chosen)
      (return-from requantised-measure
        (values nil "its steps are all 1, as at the highest quality: a second save leaves no trace there")))
    (multiple-value-bind (ox oy off) (jpeg-grid grey clipped width height
                                                (subseq positions 0 (min 5 (length positions)))
                                                (subseq steps 0 (min 5 (length steps))))
      (cond ((null ox) (return-from requantised-measure (values nil "too few of its blocks hold no black or white")))
            ((> off 0.15d0)
             (return-from requantised-measure
               (values nil (format nil "its pixels lie off its own steps (~,2f of a step, ~
                                        0.25 being none), as when it was turned, sized or changed after it was decoded"
                                   off)))))
      (let* ((largest *requantised-largest*)
             (counts (make-array (list (length positions) (1+ largest)) :element-type 'fixnum :initial-element 0))
             (out (make-array (length positions) :element-type 'double-float))
             (blocks (* (floor (- width ox) 8) (floor (- height oy) 8)))
             (stride (max 1 (ceiling blocks *requantised-blocks-most*))) (k 0))
        (loop for y0 from oy to (- height 8) by 8
              do (job-check-stop)
                 (loop for x0 from ox to (- width 8) by 8
                       do (when (zerop (mod (incf k) stride))
                            (when (block-frequencies grey clipped width x0 y0 positions out)
                              (loop for f across out for q across steps for i from 0
                                    for c = (abs (round f q))
                                    when (<= c largest) do (incf (aref counts i c)))))))
        (let ((measured
                (loop for i below (length positions)
                      for logs = (loop for s from 2 below largest
                                       for before = (aref counts i (1- s)) for after = (aref counts i (1+ s))
                                       when (>= (max before after) *requantised-counted-least*)
                                         collect (log (/ (1+ (aref counts i s)) (sqrt (* (1+ before) (1+ after))))))
                      when (>= (length logs) 3)
                        collect (list (aref positions i) (round (aref steps i))
                                      (/ (reduce #'+ logs :key #'abs) (length logs))
                                      (count-if (lambda (l) (< l (log 0.1))) logs)))))
          (if (null measured)
              (values nil "too few of its blocks hold enough at its lower frequencies to count")
              (values (median-of (mapcar #'third measured)) measured off)))))))

(defparameter *requantised-look* 1.0
  "The measure of a JPEG's second save (REQUANTISED-MEASURE) from which it is
worth a look, never more. Pictures saved once gave at most 0.47 (real scans
and phone pictures) and 0.30 (made-up ones); saved twice, coarser first,
1.1 to 4.3; saved finer first, under 0.8, not told (doc/measures.org).")

(defun check-saved-twice (file)
  "Whether FILE, a JPEG PICTURE-FILE, was saved twice as a JPEG
\(REQUANTISED-MEASURE), worth a look at most, with the honest causes named;
and, for the record, what its tables are, libjpeg's standard ones at a
quality or a camera's or scanner's own."
  (destructuring-bind (&optional octets quality table) (picture-file-tables file)
    (declare (ignore octets))
    (when (and table (picture-file-grey file))
      (let ((standard (standard-table-quality table)) (out '()))
        (push (found :record (format nil "The JPEG's steps are ~:[a table of its own, not libjpeg's standard one ~
                                          (about quality ~d), as cameras, phones and scanners often keep~;~:*~
                                          libjpeg's standard table at quality ~d, as most programs that save ~
                                          JPEGs write~]."
                                     standard (or standard quality))
                     :rule (concatenate 'string "A JPEG keeps the steps its frequencies were rounded to; libjpeg, "
                                                "which most programs save with, scales the standard's table for "
                                                "each quality, while many devices keep tables of their own."))
              out)
        (multiple-value-bind (measure measured off)
            (requantised-measure (picture-file-grey file) (picture-file-width file) (picture-file-height file) table
                                 (picture-file-colours file))
          (cond ((null measure)
                 (push (found :record (format nil "Whether the JPEG was saved twice was not measured: ~a." measured)
                              :rule (documentation 'requantised-measure 'function))
                       out))
                ((>= measure *requantised-look*)
                 (push (found :look (format nil "The JPEG's frequencies fall unevenly over its steps (~,2f, ~,2f or ~
                                                 more being worth a look): saved as a JPEG twice, with other steps ~
                                                 the first time. A scanner or phone app that saves twice, a viewer ~
                                                 that turned it and saved it again, or an honest crop does the same."
                                            measure *requantised-look*)
                              :rule (documentation 'requantised-measure 'function)
                              :details (cons (format nil "its pixels ~,2f of a step from its own steps" off)
                                             (loop for (position step mean empty) in measured
                                                   collect (format nil "frequency ~d across, ~d down, step ~d: ~
                                                                        ~,2f, ~d step~:p all but empty"
                                                                   (mod position 8) (floor position 8) step mean
                                                                   empty)))
                              :method :picture-editor)
                       out))
                (t
                 (push (found :record (format nil "The JPEG's frequencies fall evenly over its steps (~,2f, under ~
                                                   ~,2f): no sign of a second save with other steps. A second save ~
                                                   with the same steps, or at the highest quality, leaves none."
                                              measure *requantised-look*)
                              :rule (documentation 'requantised-measure 'function))
                       out))))
        (nreverse out)))))

(defun picture-findings (pdf file progress)
  "Every sign found in the picture file FILE (a PICTURE-FILE, *PICTURE-FILE*
bound to it), examined as PDF, its one page: what it says of itself, a JPEG
saved twice, and its page's checks (CHECK-SCANS, CHECK-SCAN-PIECES), the
strongest first. PROGRESS is called with a number from 0 to 1."
  (let ((last (last-revision pdf)))
    (sorted-findings
     (append (run-check "what the picture says of itself" #'check-picture-metadata file)
             (progn (funcall progress 1/10) nil)
             (run-check "the JPEG's saves" #'check-saved-twice file)
             (progn (funcall progress 3/10) nil)
             (run-check "the scanned page" #'check-scans pdf last)
             (progn (funcall progress 9/10) nil)
             (run-check "the scanned page's JPEG blocks" #'check-scan-pieces pdf last))
     last)))

(defun examine-picture (job analysis octets kind)
  "What a job examining a picture file of KIND does once its OCTETS are read
and summed into ANALYSIS: the picture read (READ-PICTURE-FILE) and examined
as one scanned page (PICTURE-FINDINGS), its pixels let go after. No revision
is written: a picture file has none."
  (funcall (stage job "Reading the picture" 0.05 0.3) 0)
  (let* ((file (read-picture-file octets kind))
         (*picture-file* file)
         (pdf (read-pdf (picture-page-octets (max 1 (picture-file-width file)) (max 1 (picture-file-height file)))
                        :path (analysis-path analysis) :name (analysis-name analysis))))
    (job-check-stop)
    (setf (analysis-pdf analysis) pdf
          (analysis-picture analysis) file
          (analysis-findings analysis) (picture-findings pdf file (stage job "Looking for signs of editing" 0.3 0.95)))
    (analysis-traits analysis)
    (setf (picture-file-grey file) nil (picture-file-colours file) nil
          (analysis-shown analysis) (multiple-value-list (pieces-text (report-pieces analysis)))
          (palimpsest-job-analysis job) analysis)
    (forget-workings pdf)))
