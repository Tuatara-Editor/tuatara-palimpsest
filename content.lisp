;;;; content.lisp --- A PDF's pages and what each draws: text, filled shapes and pictures, with where.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Each page of a revision is read from its page tree, what it inherits
;;;; with it, and its content streams are read as the operators they hold:
;;;; the text each shows (its characters by the font's ToUnicode map, else
;;;; its encoding, else the codes themselves in hex), in which font, size,
;;;; colour and render mode, and where on the page; each shape filled and
;;;; each picture placed, with its box. What a page draws in one revision is
;;;; then set beside what it drew in the revision before. Nothing is drawn:
;;;; the operators are only followed, each page and all pages together held
;;;; to a count of them.

(in-package #:palimpsest)

(defparameter *most-operators* 2000000
  "The most operators of one page's content followed: past them the rest of the page is not read.")

(defparameter *most-operators-in-all* 50000000
  "The most operators followed in all the pages of a PDF, every save's pages
together, a page read again counted again: past them the pages left are not
read, and the report says so.")

(defparameter *most-marks-kept* 1000000
  "The most marks of pages read kept to be used again, all pages together:
past them those kept longest are let go, and read again when needed.")

(defparameter *deepest-form* 8
  "How deep form XObjects drawn within form XObjects are followed.")

(defparameter *farthest-coordinate* 1d15
  "The farthest from 0 a place, size or matrix entry worked out from a page's
operators goes: one further is taken as this far, so that numbers a file
writes cannot grow without end as its operators are followed.")

;;; Pages

(defstruct (page (:copier nil))
  "A page of a revision: NUMBER, its object's; INDEX from 0; MEDIA and CROP,
its boxes (x0 y0 x1 y1); ROTATE in degrees; RESOURCES, its resource
dictionary; CONTENTS, the object numbers of its content streams; ANNOTS,
its annotations' values (references or dictionaries); DICT, its dictionary."
  number index media crop (rotate 0) resources contents annots dict)

(setf (documentation 'page-p 'function) "True when OBJECT is a PAGE.")

(defun box-of (value)
  "VALUE, a rectangle as a PDF writes it, as (x0 y0 x1 y1), lowest first; NIL when it is none."
  (when (and (listp value) (= (length value) 4) (every #'realp value))
    (destructuring-bind (a b c d) value
      (list (min a c) (min b d) (max a c) (max b d)))))

(defun catalogue (pdf revision)
  "REVISION's document catalogue, the dictionary its trailer's /Root names, or NIL."
  (let ((root (dref pdf revision (revision-trailer revision) "/Root")))
    (and (pdf-dictionary-p root) root)))

(defun pages-of (pdf revision)
  "REVISION's pages, in order (a list of PAGE), read from its page tree once:
each with what it inherits from the nodes above it. A node met twice, or
deeper than 64, is passed over and noted."
  (when (eq (revision-pages revision) :unread)
    (let ((pages '()) (seen (make-hash-table)) (count 0))
      (labels ((walk (value inherited depth)
                 (let* ((number (pdf-ref-p value))
                        (node (deref pdf revision value)))
                   (cond ((not (pdf-dictionary-p node)))
                         ((or (> depth 64) (and number (gethash number seen)))
                          (push (format nil "its page tree meets object ~a twice, or goes deeper than 64" number) (revision-problems revision)))
                         ((> count *palimpsest-most-objects*))
                         (t
                          (when number (setf (gethash number seen) t))
                          (let ((inherited (append (loop for key in '("/Resources" "/MediaBox" "/CropBox" "/Rotate")
                                                         for v = (pdf-get node key)
                                                         when v collect (cons key v))
                                                   inherited))
                                (kids (dref pdf revision node "/Kids")))
                            (if (and (listp kids) kids (not (name-p (pdf-get node "/Type") "/Page")))
                                (dolist (kid kids) (walk kid inherited (1+ depth)))
                                (flet ((inherited-value (key) (deref pdf revision (cdr (assoc key inherited :test #'string=)))))
                                  (incf count)
                                  (push (make-page :number number :index (length pages) :dict node
                                                   :media (or (box-of (inherited-value "/MediaBox")) (list 0 0 612 792))
                                                   :crop (box-of (inherited-value "/CropBox"))
                                                   :rotate (let ((r (inherited-value "/Rotate"))) (if (integerp r) (mod r 360) 0))
                                                   :resources (let ((r (inherited-value "/Resources"))) (and (pdf-dictionary-p r) r))
                                                   :contents (let ((c (pdf-get node "/Contents")))
                                                               (remove nil (mapcar #'pdf-ref-p
                                                                                   (if (pdf-ref-p c)
                                                                                       (let ((v (deref pdf revision c)))
                                                                                         ;; a reference to an array of streams, or to a stream
                                                                                         (if (and (listp v) (every #'pdf-ref-p v) v) v (list c)))
                                                                                       (pdf-array-items c)))))
                                                   :annots (let ((a (dref pdf revision node "/Annots"))) (and (listp a) (not (pdf-dictionary-p a)) a)))
                                        pages)))))))))
        (let ((root (catalogue pdf revision)))
          (when root (walk (pdf-get root "/Pages") '() 0))))
      (setf (revision-pages revision) (nreverse pages))))
  (revision-pages revision))

(defun page-reach (pdf revision page &key (limit 5000) (until (constantly nil)))
  "The numbers of the objects PAGE draws with in REVISION: its own, its
content streams, resources (fonts, pictures, forms within forms) and
annotations, never its parent; at most LIMIT of them. Stops at the first
number UNTIL, a function of a number, is true for, returning true as a
second value."
  (let ((seen (make-hash-table)) (queue (append (and (page-number page) (list (page-number page)))
                                                (references-in (remove "/Parent" (page-dict page) :key #'car :test #'string=))
                                                (references-in (page-resources page)))))
    (loop while (and queue (< (hash-table-count seen) limit))
          do (let ((number (pop queue)))
               (unless (gethash number seen)
                 (setf (gethash number seen) t)
                 (when (funcall until number) (return-from page-reach (values seen t)))
                 (let* ((object (revision-object pdf revision number))
                        (value (and object (pdf-indirect-value object))))
                   (when (and value (not (eql number (page-number page))))
                     (dolist (n (references-in (if (pdf-dictionary-p value)
                                                   (remove-if (lambda (pair) (member (car pair) '("/Parent" "/P" "/Dest" "/A" "/Prev" "/Next" "/First" "/Last") :test #'string=)) value)
                                                   value)))
                       (unless (gethash n seen) (push n queue))))))))
    seen))

;;; What each object is to the document

(defun field-name (pdf revision field)
  "The full name of FIELD, a form field's dictionary, its parents' names before its own, joined by full stops."
  (let ((parts '()))
    (loop for node = field then (dref pdf revision node "/Parent")
          repeat 32
          while (pdf-dictionary-p node)
          do (let ((name (dref pdf revision node "/T"))) (when (stringp name) (push name parts))))
    (format nil "~{~a~^.~}" parts)))

(defun form-fields (pdf revision)
  "The fields of REVISION's form, each (NAME NUMBER DICT): the terminal
fields of its /AcroForm's tree, at most 10,000, none met twice."
  (let ((fields '()) (seen (make-hash-table)))
    (labels ((walk (value depth)
               (let ((number (pdf-ref-p value)) (dict (deref pdf revision value)))
                 (when (and (pdf-dictionary-p dict) (< depth 32) (< (length fields) 10000)
                            (not (and number (gethash number seen))))
                   (when number (setf (gethash number seen) t))
                   (let ((kids (remove-if-not (lambda (k) (pdf-dictionary-p (deref pdf revision k))) (pdf-array-items (dref pdf revision dict "/Kids")))))
                     (if (and kids (some (lambda (k) (pdf-get (deref pdf revision k) "/T")) kids))
                         (dolist (kid kids) (walk kid (1+ depth)))
                         (push (list (field-name pdf revision dict) number dict) fields)))))))
      (let ((form (dref pdf revision (catalogue pdf revision) "/AcroForm")))
        (dolist (field (pdf-array-items (dref pdf revision form "/Fields"))) (walk field 0))))
    (nreverse fields)))

(defun roles-of (pdf revision)
  "What each object of REVISION is to the document, in words, as a hash table
of number to words: its catalogue, document information, pages, their
content, fonts, pictures and annotations, its form's fields, its metadata."
  (or (revision-roles revision)
      (let ((roles (make-hash-table)))
        (flet ((role (value words) (let ((n (pdf-ref-p value))) (when (and n (not (gethash n roles))) (setf (gethash n roles) words)))))
          (let ((trailer (revision-trailer revision)))
            (role (pdf-get trailer "/Root") "the document catalogue")
            (role (pdf-get trailer "/Info") "the document information")
            (role (pdf-get trailer "/Encrypt") "the encryption dictionary"))
          (let ((root (catalogue pdf revision)))
            (role (pdf-get root "/Metadata") "the XMP metadata")
            (role (pdf-get root "/AcroForm") "the form")
            (role (pdf-get root "/Pages") "the page tree")
            (role (pdf-get root "/Outlines") "the outline")
            (role (pdf-get root "/Names") "the name tree")
            (role (pdf-get root "/OCProperties") "the layers (optional content)"))
          (dolist (page (pages-of pdf revision))
            (let ((p (1+ (page-index page))))
              (when (page-number page) (role (list :ref (page-number page) 0) (format nil "page ~d" p)))
              (dolist (c (page-contents page)) (role (list :ref c 0) (format nil "page ~d's content" p)))
              (let ((resources (page-resources page)))
                (loop for (name . value) in (let ((f (dref pdf revision resources "/Font"))) (and (pdf-dictionary-p f) f))
                      do (let ((font (deref pdf revision value)))
                           (role value (format nil "the font ~a~@[ (~a)~] of page ~d" (subseq name 1)
                                               (let ((b (pdf-get font "/BaseFont"))) (and (stringp b) (subseq b 1))) p))))
                (loop for (name . value) in (let ((x (dref pdf revision resources "/XObject"))) (and (pdf-dictionary-p x) x))
                      do (let ((x (deref pdf revision value)))
                           (role value (format nil "the ~:[form~;picture~] ~a of page ~d" (name-p (pdf-get x "/Subtype") "/Image") (subseq name 1) p)))))
              (dolist (a (page-annots page))
                (let ((annot (deref pdf revision a)))
                  (role a (format nil "an annotation~@[ (~a)~] on page ~d"
                                  (let ((s (pdf-get annot "/Subtype"))) (and (stringp s) (subseq s 1))) p))))))
          (dolist (field (form-fields pdf revision))
            (destructuring-bind (name number dict) field
              (when number (role (list :ref number 0) (format nil "the form field ~s" name)))
              (role (pdf-get dict "/V") (format nil "the value of form field ~s" name)))))
        (setf (revision-roles revision) roles))))

(defun role-words (pdf revision number)
  "What the object NUMBER is to REVISION's document, in words; its /Type or
/Subtype when nothing else says; NIL when nothing does."
  (or (gethash number (roles-of pdf revision))
      (let* ((object (revision-object pdf revision number))
             (value (and object (pdf-indirect-value object)))
             (type (or (pdf-get value "/Type") (pdf-get value "/Subtype"))))
        (cond ((name-p type "/ObjStm") "an object stream")
              ((name-p type "/XRef") "a cross-reference stream")
              ((name-p type "/Sig") "a signature")
              ((stringp type) (format nil "an object of type ~a" (subseq type 1)))))))

;;; Content tokens

(defun content-tokens (octets operator &key (limit *most-operators*))
  "Read OCTETS, a content stream (or a CMap), calling OPERATOR with each
operator met (a string), its operands (a list, in order: numbers, names as
strings with their /, strings as octets, arrays as lists, dictionaries as
alists, :true, :false and :null), where its first operand begins (or the
operator, when it has none) and where the operator ends. An inline picture (BI
... ID ... EI) is given as the operator BI with its dictionary as its one
operand, its data passed over. At most LIMIT operators; returns how many
there were. In a job, it ends there when the job is asked to stop."
  (let ((n (length octets)) (i 0) (stack '()) (depth 0) (count 0) (from nil))
    (labels ((value (i depth)
               ;; one operand at I, and where it ends
               (let ((c (aref octets i)))
                 (cond ((= c 40) (pdf-literal-string-at octets i t))
                       ((and (= c 60) (< (1+ i) n) (= (aref octets (1+ i)) 60))
                        (pdf-object-at octets i))
                       ((= c 60) (pdf-hex-string-at octets i t))
                       ((= c 47) (multiple-value-bind (name end) (pdf-object-at octets i) (values name end)))
                       ((= c 91)
                        (let ((items '()) (j (1+ i)))
                          (loop
                            (setf j (pdf-skip-space octets j))
                            (when (or (>= j n) (> depth 16)) (return))
                            (when (= (aref octets j) 93) (incf j) (return))
                            (multiple-value-bind (item end) (value j (1+ depth))
                              (when (<= end j) (return))
                              (setf j end)
                              (unless (eq item :skip) (push item items))))
                          (values (nreverse items) j)))
                       ((or (digit-p c) (member c '(43 45 46)))
                        (multiple-value-bind (number end) (pdf-number-at octets i n)
                          (values (if (numberp number) number :skip) (max end (1+ i)))))
                       ((pdf-delimiter-p c) (values :skip (1+ i)))
                       (t (let ((j i))
                            (loop while (and (< j n) (not (pdf-delimiter-p (aref octets j)))) do (incf j))
                            (values (octets-latin-1 octets :start i :end j) j))))))
             (inline-image (i)
               ;; after BI: its keys and values to ID, then its data to EI
               (let ((pairs '()) (j i))
                 (loop
                   (setf j (pdf-skip-space octets j))
                   (when (>= j n) (return))
                   (when (and (octets-at-p octets "ID" j) (or (>= (+ j 2) n) (pdf-white-p (aref octets (+ j 2)))))
                     (incf j 3) (return))
                   (multiple-value-bind (key end) (value j 0)
                     (when (<= end j) (return))
                     (multiple-value-bind (v after) (value (pdf-skip-space octets end) 0)
                       (when (<= after end) (return))
                       (when (stringp key) (push (cons key v) pairs))
                       (setf j after))))
                 (let ((close (loop for at = (search (load-time-value (string-octets "EI")) octets :start2 j)
                                      then (search (load-time-value (string-octets "EI")) octets :start2 (+ at 2))
                                    while at
                                    when (and (> at 0) (pdf-white-p (aref octets (1- at)))
                                              (or (>= (+ at 2) n) (pdf-white-p (aref octets (+ at 2))) (pdf-delimiter-p (aref octets (+ at 2)))))
                                      return at)))
                   (values (nreverse pairs) (if close (+ close 2) n))))))
      (loop
        (setf i (pdf-skip-space octets i))
        (when (or (>= i n) (>= count limit)) (return))
        (multiple-value-bind (item end) (value i 0)
          (setf end (max end (1+ i)))
          (unless (or from (eq item :skip)) (setf from i))
          (flet ((operand (x)
                   (push x stack)
                   (when (> (incf depth) 100000) (setf stack '() depth 0)))
                 (counted ()
                   (when (zerop (logand (incf count) 4095)) (job-check-stop))))
            (cond ((eq item :skip))
                  ((and (stringp item) (plusp (length item)) (char/= #\/ (char item 0)))
                   (cond ((string= item "true") (operand :true))
                         ((string= item "false") (operand :false))
                         ((string= item "null") (operand :null))
                         ((string= item "BI")
                          (multiple-value-bind (dict after) (inline-image end)
                            (counted)
                            (funcall operator "BI" (list dict) from after)
                            (setf stack '() depth 0 from nil end after)))
                         (t (counted)
                            (funcall operator item (reverse stack) from end)
                            (setf stack '() depth 0 from nil))))
                  (t (operand item))))
          (setf i end))))
    count))

;;; Fonts

(defstruct (font (:copier nil))
  "A font as text is read with it: BASE, its /BaseFont without the subset
tag; SUBSET, that tag (ABCDEF) or NIL; SUBTYPE; EMBEDDED, true when the font
itself is kept in the file; TWO-OCTET, true when each character code takes
two octets; UNICODE, a hash table of code to text (its ToUnicode map), or
NIL; TABLE, a vector of 256 characters (its encoding), or NIL; WIDTHS, a
hash table of code to width in thousandths of the size; DEFAULT-WIDTH; SCALE,
what widths are multiplied by (a Type 3 font's matrix); FILE, the reference
to the font file kept in the PDF (/FontFile, /FontFile2 or /FontFile3), or NIL;
NUMBER, its object's number, NIL for a font written inside its resources."
  base subset subtype embedded two-octet unicode table (widths (make-hash-table)) (default-width 500) (scale 1) file number)

(setf (documentation 'font-p 'function) "True when OBJECT is a FONT.")

(defparameter *glyph-names*
  (let ((table (make-hash-table :test 'equal)))
    (loop for name in '("space" "exclam" "quotedbl" "numbersign" "dollar" "percent" "ampersand" "quotesingle" "parenleft" "parenright"
                        "asterisk" "plus" "comma" "hyphen" "period" "slash" "zero" "one" "two" "three" "four" "five" "six" "seven"
                        "eight" "nine" "colon" "semicolon" "less" "equal" "greater" "question" "at")
          for code from 32 do (setf (gethash name table) (code-char code)))
    (loop for code from 65 to 90 do (setf (gethash (string (code-char code)) table) (code-char code)
                                          (gethash (string (code-char (+ code 32))) table) (code-char (+ code 32))))
    (loop for (name . code) in '(("bracketleft" . 91) ("backslash" . 92) ("bracketright" . 93) ("asciicircum" . 94) ("underscore" . 95)
                                 ("grave" . 96) ("braceleft" . 123) ("bar" . 124) ("braceright" . 125) ("asciitilde" . 126)
                                 ("quoteleft" . #x2018) ("quoteright" . #x2019) ("quotedblleft" . #x201C) ("quotedblright" . #x201D)
                                 ("endash" . #x2013) ("emdash" . #x2014) ("bullet" . #x2022) ("ellipsis" . #x2026) ("Euro" . #x20AC)
                                 ("fi" . #xFB01) ("fl" . #xFB02) ("minus" . #x2212) ("trademark" . #x2122) ("dagger" . #x2020)
                                 ("daggerdbl" . #x2021) ("perthousand" . #x2030) ("quotesinglbase" . #x201A) ("quotedblbase" . #x201E)
                                 ("guilsinglleft" . #x2039) ("guilsinglright" . #x203A) ("OE" . #x152) ("oe" . #x153)
                                 ("Scaron" . #x160) ("scaron" . #x161) ("Zcaron" . #x17D) ("zcaron" . #x17E) ("Ydieresis" . #x178)
                                 ("florin" . #x192) ("circumflex" . #x2C6) ("tilde" . #x2DC) ("dotlessi" . #x131))
          do (setf (gethash name table) (code-char code)))
    (loop for name in '("nbspace" "exclamdown" "cent" "sterling" "currency" "yen" "brokenbar" "section" "dieresis" "copyright"
                        "ordfeminine" "guillemotleft" "logicalnot" "sfthyphen" "registered" "macron" "degree" "plusminus"
                        "twosuperior" "threesuperior" "acute" "mu" "paragraph" "periodcentered" "cedilla" "onesuperior"
                        "ordmasculine" "guillemotright" "onequarter" "onehalf" "threequarters" "questiondown" "Agrave" "Aacute"
                        "Acircumflex" "Atilde" "Adieresis" "Aring" "AE" "Ccedilla" "Egrave" "Eacute" "Ecircumflex" "Edieresis"
                        "Igrave" "Iacute" "Icircumflex" "Idieresis" "Eth" "Ntilde" "Ograve" "Oacute" "Ocircumflex" "Otilde"
                        "Odieresis" "multiply" "Oslash" "Ugrave" "Uacute" "Ucircumflex" "Udieresis" "Yacute" "Thorn" "germandbls"
                        "agrave" "aacute" "acircumflex" "atilde" "adieresis" "aring" "ae" "ccedilla" "egrave" "eacute"
                        "ecircumflex" "edieresis" "igrave" "iacute" "icircumflex" "idieresis" "eth" "ntilde" "ograve" "oacute"
                        "ocircumflex" "otilde" "odieresis" "divide" "oslash" "ugrave" "uacute" "ucircumflex" "udieresis" "yacute"
                        "thorn" "ydieresis")
          for code from 160 do (setf (gethash name table) (code-char code)))
    table)
  "The characters glyph names stand for, as an encoding's /Differences names them.")

(defun glyph-character (name)
  "The text the glyph NAME (without its /) stands for: a known name, uniXXXX
or uXXXX; NIL when it is none of these."
  (let ((known (gethash name *glyph-names*)))
    (cond (known (string known))
          ((and (= (length name) 7) (string= "uni" name :end2 3))
           (let ((code (parse-integer name :start 3 :radix 16 :junk-allowed t))) (and code (< code #xD800) (string (code-char code)))))
          ((and (<= 5 (length name) 7) (char= #\u (char name 0)))
           (let ((code (parse-integer name :start 1 :radix 16 :junk-allowed t))) (and code (< code #x110000) (not (<= #xD800 code #xDFFF)) (string (code-char code))))))))

(defparameter *win-ansi-high*
  #(#x20AC nil #x201A #x192 #x201E #x2026 #x2020 #x2021 #x2C6 #x2030 #x160 #x2039 #x152 nil #x17D nil
    nil #x2018 #x2019 #x201C #x201D #x2022 #x2013 #x2014 #x2DC #x2122 #x161 #x203A #x153 nil #x17E #x178)
  "The characters WinAnsiEncoding has for the codes 128 to 159.")

(defparameter *mac-roman-high*
  #.(concatenate 'string "ÄÅÇÉÑÖÜáàâäãåçéèêëíìîïñóòôöõúùûü†°¢£§•¶ß®©™´¨≠ÆØ∞±≤≥¥µ∂∑∏π∫ªºΩæø¿¡¬√ƒ≈∆«»…" (string (code-char #xA0)) ; no-break space
                 "ÀÃÕŒœ–—“”‘’÷◊ÿŸ⁄€‹›ﬁﬂ‡·‚„‰ÂÊÁËÈÍÎÏÌÓÔ" (string (code-char #xF8FF)) ; the Apple logo, in a private place
                 "ÒÚÛÙıˆ˜¯˘˙˚¸˝˛ˇ")
  "The characters MacRomanEncoding has for the codes 128 to 255.")

(defun base-encoding (name)
  "The vector of 256 characters (or NIL) of the encoding NAME: WinAnsi,
MacRoman, else the standard one (read as Latin-1)."
  (let ((table (make-array 256 :initial-element nil)))
    (loop for code from 32 below 127 do (setf (aref table code) (code-char code)))
    (cond ((equal name "/WinAnsiEncoding")
           (loop for code from 128 below 160 for c across *win-ansi-high* when c do (setf (aref table code) (code-char c)))
           (loop for code from 160 below 256 do (setf (aref table code) (code-char code))))
          ((equal name "/MacRomanEncoding")
           (loop for code from 128 below 256 for c across *mac-roman-high* do (setf (aref table code) c)))
          (t (loop for code from 160 below 256 do (setf (aref table code) (code-char code)))))
    table))

(defparameter *unicode-map-most-codes* 131072
  "Codes a font's ToUnicode CMap is read for at most, all its ranges and
characters together: a font's codes of two octets are no more than 65,536,
and a map made to hold billions is read no further.")

(defun unicode-map (data)
  "The ToUnicode CMap DATA, decoded, as a hash table of code to text, and the
octets a code takes (1 or 2) as a second value; at most
*UNICODE-MAP-MOST-CODES* codes."
  (let ((map (make-hash-table)) (width nil) (left *unicode-map-most-codes*))
    (flet ((code (octets) (let ((v 0)) (loop for o across octets do (setf v (+ (* v 256) o))) v))
           (text (octets) (if (and (vectorp octets) (evenp (length octets)) (plusp (length octets)))
                              (utf-16-text octets :big-endian t)
                              (and (vectorp octets) (octets-latin-1 octets)))))
      (block reading
        (flet ((put (code target)
                 (when (minusp (decf left)) (return-from reading))
                 (setf (gethash code map) (text target))))
          (content-tokens
           data (lambda (operator operands from end)
                  (declare (ignore from end))
                  (cond ((string= operator "endcodespacerange")
                         (let ((first (find-if (lambda (o) (typep o 'octets)) operands)))
                           (when first (setf width (length first)))))
                        ((string= operator "endbfchar")
                         (loop for (from to) on operands by #'cddr
                               when (and (typep from 'octets) (typep to 'octets))
                                 do (put (code from) to)))
                        ((string= operator "endbfrange")
                         (loop for (low high to) on operands by #'cdddr
                               when (and (typep low 'octets) (typep high 'octets))
                                 do (let ((start (code low)) (end (min (code high) (+ (code low) 65535))))
                                      (cond ((typep to 'octets)
                                             ;; the target a big-endian number, raised by one for each code, carried into the octets before
                                             (let ((base (code to)) (size (length to)))
                                               (loop for c from start to end for k from 0
                                                     do (let ((target (make-array size :element-type '(unsigned-byte 8)))
                                                              (value (+ base k)))
                                                          (loop for j from (1- size) downto 0
                                                                do (setf (aref target j) (ldb (byte 8 (* 8 (- size 1 j))) value)))
                                                          (put c target)))))
                                            ((listp to)
                                             (loop for c from start to end for target in to
                                                   when (typep target 'octets) do (put c target)))))))))
           :limit 200000))))
    (values map width)))

(defun font-of (pdf revision value)
  "The FONT the font VALUE (a reference or a dictionary) is, in REVISION, read once for each object."
  (let* ((object (and (pdf-ref-p value) (revision-object pdf revision (second value))))
         (key (or object value))
         (cache (or (getf (pdf-cache pdf) :fonts) (setf (getf (pdf-cache pdf) :fonts) (make-hash-table :test 'eq))))
         (kept (gethash key cache)))
    (or kept
        (setf (gethash key cache)
              (let* ((dict (if object (pdf-indirect-value object) (deref pdf revision value)))
                     (subtype (pdf-get dict "/Subtype"))
                     (base-name (let ((b (pdf-get dict "/BaseFont"))) (if (stringp b) (subseq b (min 1 (length b))) "")))
                     (plus (position #\+ base-name))
                     (descendant (and (name-p subtype "/Type0") (deref pdf revision (first (pdf-array-items (dref pdf revision dict "/DescendantFonts"))))))
                     (descriptor (dref pdf revision (or descendant dict) "/FontDescriptor"))
                     (font (make-font :number (and object (second value))
                                      :base (if (and plus (= plus 6)) (subseq base-name 7) base-name)
                                      :subset (and plus (= plus 6) (subseq base-name 0 6))
                                      :subtype (and (stringp subtype) (subseq subtype 1))
                                      :embedded (and (pdf-dictionary-p descriptor)
                                                     (some (lambda (k) (pdf-get descriptor k)) '("/FontFile" "/FontFile2" "/FontFile3")) t)
                                      :file (and (pdf-dictionary-p descriptor)
                                                 (find-if #'pdf-ref-p (mapcar (lambda (k) (pdf-get descriptor k)) '("/FontFile2" "/FontFile3" "/FontFile"))))
                                      :two-octet (name-p subtype "/Type0"))))
                (let ((to-unicode (pdf-get dict "/ToUnicode")))
                  (when (pdf-ref-p to-unicode)
                    (let ((stream (revision-object pdf revision (second to-unicode))))
                      (when stream
                        (multiple-value-bind (data why) (stream-data pdf stream :limit (* 4 1024 1024))
                          (when (and data (null why))
                            (multiple-value-bind (map width) (ignore-errors (unicode-map data))
                              (when map
                                (setf (font-unicode font) map)
                                (when (and width (font-two-octet font) (= width 1)) (setf (font-two-octet font) nil))))))))))
                (if (font-two-octet font)
                    (let ((w (dref pdf revision descendant "/W")))
                      (setf (font-default-width font) (let ((d (pdf-get descendant "/DW"))) (if (realp d) d 1000)))
                      (loop while (and (consp w) (< (hash-table-count (font-widths font)) 65536))
                            do (let ((first (pop w)) (next (pop w)))
                                 (cond ((and (integerp first) (listp next))
                                        (loop for c from first for width in (deref pdf revision next)
                                              when (realp width) do (setf (gethash c (font-widths font)) width)))
                                       ((and (integerp first) (integerp next) (realp (first w)))
                                        (let ((width (pop w)))
                                          (loop for c from first to (min next (+ first 65535)) do (setf (gethash c (font-widths font)) width))))
                                       (t (return))))))
                    (let ((encoding (dref pdf revision dict "/Encoding"))
                          (first (pdf-get dict "/FirstChar"))
                          (widths (dref pdf revision dict "/Widths")))
                      (setf (font-table font)
                            (let ((table (base-encoding (cond ((stringp encoding) encoding) ((pdf-dictionary-p encoding) (pdf-get encoding "/BaseEncoding"))))))
                              (when (pdf-dictionary-p encoding)
                                (let ((code 0))
                                  (dolist (item (pdf-array-items (dref pdf revision encoding "/Differences")))
                                    (cond ((integerp item) (setf code item))
                                          ((and (stringp item) (< -1 code 256))
                                           (setf (aref table code) (let ((c (glyph-character (subseq item 1)))) (and c (char c 0))))
                                           (incf code))))))
                              table))
                      (when (and (integerp first) (listp widths))
                        (loop for c from first for width in widths repeat 256
                              when (realp width) do (setf (gethash c (font-widths font)) width)))
                      (let ((missing (pdf-get descriptor "/MissingWidth")))
                        (when (realp missing) (setf (font-default-width font) missing)))
                      (when (name-p subtype "/Type3")
                        (let ((matrix (pdf-get dict "/FontMatrix")))
                          (when (and (listp matrix) (realp (first matrix)))
                            (setf (font-scale font) (* 1000 (first matrix))))))))
                font)))))

(defun font-codes (font octets)
  "The character codes OCTETS hold in FONT, each (CODE . TEXT): TEXT by its
ToUnicode map, else its encoding, else the code in hex (<0041>)."
  (let ((step (if (font-two-octet font) 2 1)))
    (loop for i from 0 to (- (length octets) step) by step
          collect (let* ((code (if (= step 2) (+ (* 256 (aref octets i)) (aref octets (1+ i))) (aref octets i)))
                         (text (or (and (font-unicode font) (gethash code (font-unicode font)))
                                   (and (font-table font) (let ((c (aref (font-table font) code))) (and c (string c))))
                                   (if (= step 2) (format nil "<~4,'0x>" code) (format nil "<~2,'0x>" code)))))
                    (cons code text)))))

;;; Drawing followed

(defstruct (mark (:copier nil))
  "What a page draws: KIND :text, :fill or :image; TEXT, the text shown, a
picture's name, or for a filled shape \"rectangle\" when it is one square to the
page; X0 Y0 X1 Y1, its box on the page (a text's from its
baseline to its top); FONT, the FONT; SIZE, its height in points as it
shows; MODE, its text render mode; COLOUR, its colour as (red green blue),
each 0 to 1, or NIL when not known; ALPHA, its opacity; STREAM, which of the
page's content streams draws it (from 0); ORDER, its place in the drawing;
SHOWS and SINGLES, how many text-showing operators it took and how many of
those showed one character; WIDE, how many spaces within it were made by
moving the text on (TJ's spacings), not by space characters; SQUEEZE, how
much wider (above 1) or narrower its characters are set than their font
draws them at its height (the horizontal scale and the text matrix);
SPACING, the space put after each character, in sizes; CLIPPED, true when it lies outside what is
clipped to; HIDDEN, true when it is in a layer that is off; ACTUAL, the
text a tag says it stands for (for a picture, its dictionary); OBJECT, a
picture's object number. SOURCE is the object number of the stream whose
operators draw it (a content stream, or a form's), FROM and TO where those
operators are in its decoded octets, COLOUR-AT where the operator setting
its colour is there (FROM . TO), or NIL."
  kind text x0 y0 x1 y1 font (size 0) (mode 0) colour (alpha 1) (stream 0) (order 0)
  (shows 1) (singles 0) (wide 0) (squeeze 1) (spacing 0) clipped hidden actual object source from to colour-at)

(setf (documentation 'mark-p 'function) "True when OBJECT is a MARK.")

(defun place-label (source from fonts)
  "Where text is drawn, in the words the drawing of objects and the findings
share: the object SOURCE (a content stream or form), @FROM where in its
decoded octets the operators drawing it begin, and FONTS, its font objects:
4 @539 font 8."
  (format nil "~d~@[ @~d~]~@[ font ~{~d~^, ~}~]" source from (sort (copy-list fonts) #'<)))

(defun mark-place (mark)
  "Where MARK, a run of text, is drawn (PLACE-LABEL); NIL when not known."
  (and (mark-source mark)
       (place-label (mark-source mark) (mark-from mark)
                    (let ((n (and (mark-font mark) (font-number (mark-font mark))))) (and n (list n))))))

(defstruct (state (:copier copy-state))
  "The graphics state while a page's content is followed: CTM, FILL, STROKE,
ALPHA, CLIP (a box, or NIL), FONT, SIZE, CHARACTER-SPACING, WORD-SPACING,
SCALE, LEADING, MODE and RISE; FILL-AT and STROKE-AT, where the operators
setting the colours are, each (SOURCE FROM . TO), or NIL."
  (ctm (list 1 0 0 1 0 0)) (fill (list 0 0 0)) (stroke (list 0 0 0)) (alpha 1) clip
  font (size 0) (character-spacing 0) (word-spacing 0) (scale 1) (leading 0) (mode 0) (rise 0) fill-at stroke-at)

(setf (documentation 'state-p 'function) "True when OBJECT is a STATE."
      (documentation 'copy-state 'function) "A copy of a STATE, for q to keep and Q to bring back.")

(defun bounded (x)
  "The real X as a double-float, no further from 0 than *FARTHEST-COORDINATE*."
  (float (max (- *farthest-coordinate*) (min *farthest-coordinate* x)) 1d0))

(defun times (m n)
  "The matrix M times the matrix N, each (a b c d e f) as a PDF writes them,
each entry BOUNDED."
  (destructuring-bind (a b c d e f) m
    (destructuring-bind (a2 b2 c2 d2 e2 f2) n
      (list (bounded (+ (* a a2) (* b c2))) (bounded (+ (* a b2) (* b d2)))
            (bounded (+ (* c a2) (* d c2))) (bounded (+ (* c b2) (* d d2)))
            (bounded (+ (* e a2) (* f c2) e2)) (bounded (+ (* e b2) (* f d2) f2))))))

(defun apply-matrix (m x y)
  "The point X Y moved by the matrix M, as two values, each BOUNDED."
  (destructuring-bind (a b c d e f) m
    (values (bounded (+ (* a x) (* c y) e)) (bounded (+ (* b x) (* d y) f)))))

(defun matrix-p (value)
  "True when VALUE is a matrix: six numbers."
  (and (listp value) (= (length value) 6) (every #'realp value)))

(defun colour-of (operands)
  "The colour OPERANDS give (one grey, three red green blue, four cyan magenta
yellow black), as (red green blue); NIL for another kind."
  (let ((numbers (remove-if-not #'realp operands)))
    (case (length numbers)
      (1 (let ((g (first numbers))) (list g g g)))
      (3 numbers)
      (4 (destructuring-bind (c m y k) numbers
           (list (* (- 1 c) (- 1 k)) (* (- 1 m) (- 1 k)) (* (- 1 y) (- 1 k)))))
      (t nil))))

(defun box-around (points)
  "The box (x0 y0 x1 y1) around POINTS, each (X . Y)."
  (list (reduce #'min points :key #'car) (reduce #'min points :key #'cdr)
        (reduce #'max points :key #'car) (reduce #'max points :key #'cdr)))

(defun rectangle-p (points)
  "True when POINTS, each (X . Y), are the corners of a rectangle square to the page: two values of x and two of y, give or take a tenth of a point."
  (flet ((distinct (values) (let ((found '())) (dolist (v values found) (unless (find v found :test (lambda (a b) (< (abs (- a b)) 0.1))) (push v found))))))
    (and (<= 4 (length points) 5)
         (= 2 (length (distinct (mapcar #'car points))))
         (= 2 (length (distinct (mapcar #'cdr points)))))))

(defun box-intersection (a b)
  "Where the boxes A and B overlap, a box (empty when they do not); the other when either is NIL."
  (cond ((null a) b) ((null b) a)
        (t (list (max (first a) (first b)) (max (second a) (second b))
                 (min (third a) (third b)) (min (fourth a) (fourth b))))))

(defun inside-box-p (box x y &optional (slack 0.5))
  "True when the point X Y is within BOX, give or take SLACK."
  (and (<= (- (first box) slack) x (+ (third box) slack)) (<= (- (second box) slack) y (+ (fourth box) slack))))

(defun hidden-layers (pdf revision)
  "The object numbers of REVISION's layers (optional content groups) that are off as it opens, as a list."
  (let* ((properties (dref pdf revision (catalogue pdf revision) "/OCProperties"))
         (standard (dref pdf revision properties "/D"))
         (all (mapcar #'pdf-ref-p (pdf-array-items (dref pdf revision properties "/OCGs"))))
         (on (mapcar #'pdf-ref-p (pdf-array-items (dref pdf revision standard "/ON"))))
         (off (mapcar #'pdf-ref-p (pdf-array-items (dref pdf revision standard "/OFF")))))
    (remove nil (if (name-p (pdf-get standard "/BaseState") "/OFF")
                    (append off (set-difference all on))
                    off))))

(defun note-all-operators (pdf)
  "Note in PDF that its pages were read no further, *MOST-OPERATORS-IN-ALL* followed."
  (note pdf "Its pages draw more than ~:d operators in all, every save's pages together (*most-operators-in-all*): the pages after were not read"
        *most-operators-in-all*))

(defun joined-content (pieces length)
  "PIECES, a list of octet vectors, joined in one vector of LENGTH octets, a
line end after each."
  (let ((joined (make-array length :element-type '(unsigned-byte 8) :initial-element 10)) (at 0))
    (dolist (piece pieces joined)
      (replace joined piece :start1 at)
      (incf at (1+ (length piece))))))

(defun page-marks (pdf revision page)
  "What PAGE draws in REVISION, in the order drawn (a vector of MARK), and
as a second value what could not be read of it, in words, noted in PDF too
(NOTE) for the report to say. Its operators are counted against
*MOST-OPERATORS-IN-ALL*, and once that is reached no more is read: that is
noted in PDF, and nothing is drawn."
  (let* ((marks (make-array 0 :adjustable t :fill-pointer 0)) (problems '()) (order 0) (operators 0)
         (left (- *most-operators-in-all* (getf (pdf-cache pdf) :operators-read 0)))
         (most (min *most-operators* left))
         (hidden (and (plusp left) (hidden-layers pdf revision))))
    (unless (plusp left)
      (note-all-operators pdf)
      (return-from page-marks (values marks '())))
    (labels
        ((run (data resources state stream source depth &optional segments)
           ;; SEGMENTS, for a page's content streams joined: a vector of (START END NUMBER INDEX) of DATA, in order
           (unless (spend-decoded pdf (length data))
             (return-from run))
           (let ((stack '()) (tm (list 1 0 0 1 0 0)) (tlm (list 1 0 0 1 0 0))
                 (path '()) (path-from nil) (anchor nil) (clip-next nil) (tags '()))
             (labels
                 ((resource (kind name)
                    (let ((group (dref pdf revision resources kind)))
                      (pdf-get group name)))
                  (segment (at)
                    ;; the last segment starting at or before AT, else the first
                    (when segments
                      (let ((low 0) (high (length segments)))
                        (loop while (< low high)
                              do (let ((middle (floor (+ low high) 2)))
                                   (if (<= (first (aref segments middle)) at) (setf low (1+ middle)) (setf high middle))))
                        (aref segments (max 0 (1- low))))))
                  (stream-at (at) (let ((s (segment at))) (if s (fourth s) stream)))
                  (add (mark from to &optional colour-at)
                    (let ((s (segment from)))
                      (setf (mark-order mark) (incf order) (mark-stream mark) (if s (fourth s) stream)
                            (mark-source mark) (if s (third s) source)
                            (mark-from mark) (if s (- from (first s)) from)
                            (mark-to mark) (if s (- (min to (second s)) (first s)) to)
                            (mark-colour-at mark) (and colour-at (eql (first colour-at) source)
                                                       (let ((c (segment (cadr colour-at))))
                                                         (cond ((null c) (rest colour-at))
                                                               ((eq c s) (cons (- (cadr colour-at) (first c)) (- (min (cddr colour-at) (second c)) (first c)))))))
                            (mark-hidden mark) (and (some #'second tags) t)))
                    (let ((actual (find-if #'third tags)))
                      (when actual (setf (mark-actual mark) (third actual))))
                    (vector-push-extend mark marks))
                  (point (x y)
                    (multiple-value-bind (px py) (apply-matrix (state-ctm state) x y) (push (cons px py) path)))
                  (show (items from to)
                    ;; ITEMS, strings as octets and spacings as numbers
                    (let ((font (state-font state)) (size (state-size state)) (scale (state-scale state))
                          (text (make-string-output-stream)) (start nil) (shows 0) (singles 0) (wide 0))
                      (when font
                        (dolist (item items)
                          (cond ((realp item)
                                 ;; a move right of more than a fifth of the size: a space between words
                                 (when (and start (> (/ (- item) 1000) 0.2))
                                   (incf wide)
                                   (write-char #\Space text))
                                 (setf tm (times (list 1 0 0 1 (* (/ (- item) 1000) size scale) 0) tm)))
                                ((typep item 'octets)
                                 (let ((codes (font-codes font item)))
                                   (incf shows)
                                   (when (= (length codes) 1) (incf singles))
                                   (dolist (code codes)
                                     (let ((m (times tm (state-ctm state))))
                                       (unless start
                                         (multiple-value-bind (x y) (apply-matrix m 0 (state-rise state)) (setf start (list x y m)))))
                                     (write-string (cdr code) text)
                                     (let* ((width (* (font-scale font) (or (gethash (car code) (font-widths font)) (font-default-width font))))
                                            (advance (* (+ (* (/ width 1000) size) (state-character-spacing state)
                                                           (if (and (= (car code) 32) (not (font-two-octet font))) (state-word-spacing state) 0))
                                                        scale)))
                                       (setf tm (times (list 1 0 0 1 advance 0) tm)))))))))
                      (let ((words (get-output-stream-string text)))
                        (when (and start (plusp (length words)))
                          (destructuring-bind (x y m) start
                            (multiple-value-bind (x1 y1) (apply-matrix (times tm (state-ctm state)) 0 (state-rise state))
                              (let* ((up (sqrt (+ (expt (third m) 2) (expt (fourth m) 2))))
                                     (across (sqrt (+ (expt (first m) 2) (expt (second m) 2))))
                                     (height (* size up))
                                     (mode (state-mode state)))
                                (add (make-mark :kind :text :text words :x0 (min x x1) :y0 (min y y1) :x1 (max x x1)
                                                :y1 (+ (max y y1) height) :font font :size height :mode mode
                                                :colour (case mode ((0 2 4 6) (state-fill state)) ((1 5) (state-stroke state)))
                                                :alpha (state-alpha state)
                                                :clipped (and (state-clip state) (not (inside-box-p (state-clip state) x y 1)) t)
                                                :shows (max 1 shows) :singles singles :wide wide
                                                :squeeze (if (plusp up) (* scale (/ across up)) 1)
                                                :spacing (if (/= size 0) (/ (state-character-spacing state) size) 0))
                                     (min from (or anchor from)) to
                                     (case mode ((1 5) (state-stroke-at state)) (t (state-fill-at state)))))))))))
                  (paint (fill to)
                    (when (and fill path)
                      ;; what shows of it: its box within what is clipped to
                      (let ((box (box-intersection (box-around path) (state-clip state))))
                        (when (and (< (first box) (third box)) (< (second box) (fourth box)))
                          (add (make-mark :kind :fill :x0 (first box) :y0 (second box) :x1 (third box) :y1 (fourth box)
                                          :colour (state-fill state) :alpha (state-alpha state)
                                          :text (and (rectangle-p path) "rectangle"))
                               (or path-from to) to (state-fill-at state)))))
                    (when (and clip-next path)
                      (setf (state-clip state) (box-intersection (state-clip state) (box-around path))))
                    (setf path '() path-from nil clip-next nil))
                  (place-picture (name object dict from to)
                    (let ((corners (loop for (x y) in '((0 0) (1 0) (0 1) (1 1))
                                         collect (multiple-value-bind (px py) (apply-matrix (state-ctm state) x y) (cons px py)))))
                      (let ((box (box-around corners)))
                        (add (make-mark :kind :image :text name :x0 (first box) :y0 (second box) :x1 (third box) :y1 (fourth box)
                                        :object object :actual (and (pdf-dictionary-p dict) dict))
                             from to)))))
               (content-tokens
                data
                (lambda (operator operands from end)
                  (when (> (incf operators) most)
                    (if (< left *most-operators*)
                        (note-all-operators pdf)
                        (pushnew (format nil "page ~d draws more than ~:d operators: the rest were not read" (1+ (page-index page)) *most-operators*)
                                 problems :test #'string=))
                    (return-from run))
                  (let ((key (and (stringp operator) (find-symbol operator :keyword))))
                    (flet ((n (k) (let ((v (nth k operands))) (if (realp v) (bounded v) 0))))
                      ;; the operator as a keyword, found once, then compared by identity
                      (macrolet ((is (&rest names) `(member key ',(mapcar (lambda (name) (intern name :keyword)) names) :test #'eq)))
                        (when (is "m" "l" "c" "v" "y" "re")
                          (unless path-from (setf path-from from)))
                        (when (is "BT" "Td" "TD" "Tm" "T*")
                          (setf anchor from))
                        (cond
                          ((is "q") (push (copy-state state) stack) (when (> (length stack) 256) (setf stack (subseq stack 0 256))))
                          ((is "Q") (when stack (setf state (pop stack))))
                          ((is "cm") (when (and (= (length operands) 6) (every #'realp operands))
                                       (setf (state-ctm state) (times operands (state-ctm state)))))
                          ((is "g" "rg" "k" "sc" "scn") (setf (state-fill state) (colour-of operands)
                                                              (state-fill-at state) (list* source from end)))
                          ((is "G" "RG" "K" "SC" "SCN") (setf (state-stroke state) (colour-of operands)
                                                              (state-stroke-at state) (list* source from end)))
                          ((is "cs") (setf (state-fill state) (list 0 0 0) (state-fill-at state) (list* source from end)))
                          ((is "gs") (let* ((gs (deref pdf revision (resource "/ExtGState" (first operands))))
                                            (alpha (pdf-get gs "/ca")))
                                       (when (realp alpha) (setf (state-alpha state) alpha))))
                          ((is "m" "l") (point (n 0) (n 1)))
                          ((is "c") (point (n 0) (n 1)) (point (n 2) (n 3)) (point (n 4) (n 5)))
                          ((is "v" "y") (point (n 0) (n 1)) (point (n 2) (n 3)))
                          ((is "re") (let ((x (n 0)) (y (n 1)) (w (n 2)) (h (n 3)))
                                       (point x y) (point (+ x w) y) (point x (+ y h)) (point (+ x w) (+ y h))))
                          ((is "W" "W*") (setf clip-next t))
                          ((is "f" "F" "f*" "B" "B*" "b" "b*") (paint t end))
                          ((is "S" "s" "n") (paint nil end))
                          ((is "BT") (setf tm (list 1 0 0 1 0 0) tlm (list 1 0 0 1 0 0)))
                          ((is "Tf") (let ((value (resource "/Font" (first operands))))
                                       (setf (state-font state) (and value (font-of pdf revision value))
                                             (state-size state) (n 1))))
                          ((is "Tc") (setf (state-character-spacing state) (n 0)))
                          ((is "Tw") (setf (state-word-spacing state) (n 0)))
                          ((is "Tz") (setf (state-scale state) (/ (n 0) 100)))
                          ((is "TL") (setf (state-leading state) (n 0)))
                          ((is "Tr") (setf (state-mode state) (let ((m (first operands))) (if (and (integerp m) (<= 0 m 7)) m 0))))
                          ((is "Ts") (setf (state-rise state) (n 0)))
                          ((is "Td") (setf tlm (times (list 1 0 0 1 (n 0) (n 1)) tlm) tm tlm))
                          ((is "TD") (setf (state-leading state) (- (n 1)) tlm (times (list 1 0 0 1 (n 0) (n 1)) tlm) tm tlm))
                          ((is "Tm") (when (matrix-p operands) (setf tlm operands tm operands)))
                          ((is "T*") (setf tlm (times (list 1 0 0 1 0 (- (state-leading state))) tlm) tm tlm))
                          ((is "Tj") (show (remove-if-not (lambda (o) (typep o 'octets)) operands) from end))
                          ((is "'") (setf tlm (times (list 1 0 0 1 0 (- (state-leading state))) tlm) tm tlm)
                           (show (remove-if-not (lambda (o) (typep o 'octets)) operands) from end))
                          ((is "\"") (setf (state-word-spacing state) (n 0) (state-character-spacing state) (n 1)
                                           tlm (times (list 1 0 0 1 0 (- (state-leading state))) tlm) tm tlm)
                           (show (remove-if-not (lambda (o) (typep o 'octets)) operands) from end))
                          ((is "TJ") (show (let ((items (first operands))) (and (listp items) items)) from end))
                          ((is "BMC") (push (list (first operands) nil nil) tags))
                          ((is "BDC") (let* ((properties (let ((p (second operands)))
                                                           (deref pdf revision (if (stringp p) (resource "/Properties" p) p))))
                                             (layer (and (name-p (first operands) "/OC")
                                                         (let ((p (second operands))) (pdf-ref-p (if (stringp p) (resource "/Properties" p) p)))))
                                             (actual (or (pdf-get properties "/ActualText") (pdf-get properties "/Alt"))))
                                        (push (list (first operands) (and layer (member layer hidden) t) (and (stringp actual) actual)) tags)))
                          ((is "EMC") (pop tags))
                          ((is "BI") (place-picture "(inline)" nil (first operands) from end))
                          ((is "Do")
                           (let* ((value (resource "/XObject" (first operands)))
                                  (number (pdf-ref-p value))
                                  (object (and number (revision-object pdf revision number)))
                                  (dict (and object (pdf-indirect-value object))))
                             (cond ((name-p (pdf-get dict "/Subtype") "/Image")
                                    (place-picture (subseq (first operands) 1) number dict from end))
                                   ((and (name-p (pdf-get dict "/Subtype") "/Form") (< depth *deepest-form*))
                                    (let ((data (stream-data pdf object))
                                          (inner (copy-state state)))
                                      (when data
                                        (let ((matrix (pdf-get dict "/Matrix")))
                                          (when (matrix-p matrix) (setf (state-ctm inner) (times matrix (state-ctm inner)))))
                                        (run data (or (let ((r (dref pdf revision dict "/Resources"))) (and (pdf-dictionary-p r) r)) resources)
                                             inner (stream-at from) number (1+ depth))))))))))))))))))
      ;; a page's content streams are one stream cut in pieces: joined, a line end between each
      (let ((pieces '()) (segments '()) (at 0))
        (loop for number in (page-contents page) for stream from 0
              do (let ((object (revision-object pdf revision number)))
                   (if (null object)
                       (push (format nil "page ~d's content stream, object ~d, is not in the file" (1+ (page-index page)) number) problems)
                       (multiple-value-bind (data why) (stream-data pdf object)
                         (cond ((or (null data) why)
                                (push (format nil "page ~d's content stream, object ~d: ~a" (1+ (page-index page)) number (or why "empty")) problems))
                               ((> (+ at (length data)) *palimpsest-largest-stream*)
                                (push (format nil "page ~d's content streams together decode to more than ~:d octets (*palimpsest-largest-stream*): those from object ~d on were not read"
                                              (1+ (page-index page)) *palimpsest-largest-stream* number)
                                      problems)
                                (loop-finish))
                               (t (push (list at (+ at (length data)) number stream) segments)
                                  (push data pieces)
                                  (incf at (1+ (length data)))))))))
        (when segments
          (run (joined-content (nreverse pieces) at) (page-resources page) (make-state) 0 :page 0
               (coerce (nreverse segments) 'vector)))))
    (incf (getf (pdf-cache pdf) :operators-read 0) (min operators most))
    (dolist (problem problems)
      (note pdf "In revision ~d, ~a" (1+ (revision-index revision)) problem))
    (values marks (nreverse problems))))

(defun marks-of (pdf revision page)
  "What PAGE draws in REVISION (PAGE-MARKS), kept to be used again while the
pages kept draw no more than *MOST-MARKS-KEPT* marks together, those kept
longest let go first."
  (let* ((cache (or (getf (pdf-cache pdf) :marks) (setf (getf (pdf-cache pdf) :marks) (make-hash-table :test 'equal))))
         (key (list (revision-index revision) (page-index page)))
         (kept (gethash key cache)))
    (if kept
        (values-list kept)
        (let* ((read (multiple-value-list (page-marks pdf revision page)))
               (size (max 1 (length (first read))))
               ;; the keys kept, the oldest first: (FIRST . LAST) of a list
               (queue (or (getf (pdf-cache pdf) :marks-queue) (setf (getf (pdf-cache pdf) :marks-queue) (cons nil nil)))))
          (when (<= size *most-marks-kept*)
            (loop while (and (car queue) (> (+ (getf (pdf-cache pdf) :marks-kept 0) size) *most-marks-kept*))
                  do (let ((oldest (pop (car queue))))
                       (decf (getf (pdf-cache pdf) :marks-kept) (max 1 (length (first (gethash oldest cache)))))
                       (remhash oldest cache)))
            (let ((cell (list key)))
              (if (car queue) (setf (cddr queue) cell) (setf (car queue) cell))
              (setf (cdr queue) cell))
            (incf (getf (pdf-cache pdf) :marks-kept 0) size)
            (setf (gethash key cache) read))
          (values-list read)))))

(defun pictures-of (pdf revision &key (keep (constantly t)))
  "The pictures the pages of REVISION draw, each once however many pages draw
it, in the order of the pages, as (MARK PAGE): the first place each is drawn
of those KEEP, called with the page and the mark, is true for."
  (let ((seen (make-hash-table)) (out '()))
    (dolist (page (pages-of pdf revision) (nreverse out))
      (loop for m across (marks-of pdf revision page)
            when (and (eq (mark-kind m) :image) (mark-object m) (not (gethash (mark-object m) seen))
                      (funcall keep page m))
              do (setf (gethash (mark-object m) seen) t)
                 (push (list m page) out)))))

;;; Text in lines

(defun text-lines (marks)
  "The text MARKS show, as lines in reading order: runs on the same baseline
\(give or take half their size) joined from left to right, a space between
those apart."
  (let ((runs (sort (remove-if-not (lambda (m) (eq (mark-kind m) :text)) (coerce marks 'list))
                    (lambda (a b) (if (> (abs (- (mark-y0 a) (mark-y0 b))) (* 0.5 (max 1 (min (mark-size a) (mark-size b)))))
                                      (> (mark-y0 a) (mark-y0 b))
                                      (< (mark-x0 a) (mark-x0 b))))))
        (lines '()) (line '()) (line-y nil))
    (flet ((finish ()
             (when line
               (let ((sorted (sort (nreverse line) #'< :key #'mark-x0)) (out (make-string-output-stream)) (previous nil))
                 (dolist (run sorted)
                   (when (and previous (> (- (mark-x0 run) (mark-x1 previous)) (* 0.2 (max 1 (mark-size run)))))
                     (write-char #\Space out))
                   (write-string (mark-text run) out)
                   (setf previous run))
                 (let ((text (string-trim " " (get-output-stream-string out))))
                   (when (plusp (length text)) (push text lines))))
               (setf line '()))))
      (dolist (run runs)
        (unless (and line-y (<= (abs (- (mark-y0 run) line-y)) (* 0.5 (max 1 (mark-size run)))))
          (finish)
          (setf line-y (mark-y0 run)))
        (push run line))
      (finish))
    (nreverse lines)))

(defun place-words (page x y)
  "Where the point X Y is on PAGE, in words: near the top left, in the middle,
at the bottom right..."
  (destructuring-bind (x0 y0 x1 y1) (page-media page)
    (let ((across (floor (* 3 (/ (- x x0) (max 1 (- x1 x0))))))
          (down (floor (* 3 (/ (- y1 y) (max 1 (- y1 y0)))))))
      (let ((h (case (max 0 (min 2 across)) (0 "left") (1 "centre") (t "right")))
            (v (case (max 0 (min 2 down)) (0 "top") (1 "middle") (t "bottom"))))
        (cond ((and (string= h "centre") (string= v "middle")) "in the middle")
              ((string= h "centre") (format nil "at the ~a" v))
              ((string= v "middle") (format nil "at the ~a" h))
              (t (format nil "near the ~a ~a" v h)))))))

(defun colour-words (colour)
  "COLOUR, (red green blue), in words: white, black, grey, or its values."
  (cond ((null colour) "a colour not known")
        ((every (lambda (c) (>= c 0.97)) colour) "white")
        ((every (lambda (c) (<= c 0.03)) colour) "black")
        ((and (< (- (reduce #'max colour) (reduce #'min colour)) 0.03)) (format nil "grey (~,2f)" (first colour)))
        (t (format nil "red ~,2f, green ~,2f, blue ~,2f" (first colour) (second colour) (third colour)))))

(defun mark-words (page mark)
  "MARK, a thing PAGE draws, in words with where it is, in points from the page's bottom left."
  (let ((where (format nil "at x ~d, y ~d, ~a" (round (mark-x0 mark)) (round (mark-y0 mark)) (place-words page (mark-x0 mark) (mark-y0 mark))))
        (width (round (- (mark-x1 mark) (mark-x0 mark)))) (height (round (- (mark-y1 mark) (mark-y0 mark)))))
    (ecase (mark-kind mark)
      (:text (format nil "text ~s ~a, ~,1f pt~@[ in ~a~]" (shorten-string (mark-text mark) 80 :one-line t) where (mark-size mark)
                     (and (mark-font mark) (font-base (mark-font mark)))))
      (:fill (format nil "a filled ~:[shape~;rectangle~] ~d by ~d pt ~a, ~a" (equal (mark-text mark) "rectangle") width height
                     where (colour-words (mark-colour mark))))
      (:image (format nil "picture ~a~@[ (object ~d)~] ~d by ~d pt ~a" (mark-text mark) (mark-object mark) width height where)))))

(defun mark-key (mark)
  "What MARK is, for telling whether two revisions draw the same: its kind, text, place and colour, rounded."
  (list (mark-kind mark) (mark-text mark) (round (mark-x0 mark)) (round (mark-y0 mark))
        (round (- (mark-x1 mark) (mark-x0 mark))) (round (- (mark-y1 mark) (mark-y0 mark)))
        (mapcar (lambda (c) (round (* 20 c))) (mark-colour mark)) (mark-mode mark)))

(defun drawing-diff (old new)
  "What NEW (marks) draws that OLD does not, and what OLD drew that NEW does
not, as two lists of marks, in drawing order."
  (let ((counts (make-hash-table :test 'equal)))
    (loop for m across old do (incf (gethash (mark-key m) counts 0)))
    (let ((added (loop for m across new
                       for key = (mark-key m)
                       if (plusp (gethash key counts 0)) do (decf (gethash key counts))
                       else collect m)))
      (clrhash counts)
      (loop for m across new do (incf (gethash (mark-key m) counts 0)))
      (values added
              (loop for m across old
                    for key = (mark-key m)
                    if (plusp (gethash key counts 0)) do (decf (gethash key counts))
                    else collect m)))))
