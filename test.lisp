;;;; test.lisp --- The palimpsest package: a PDF's saves taken apart, and the signs of its editing found.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

;; each file read here, in the tests' own process, but where a test reads it shut in
(setf palimpsest::*palimpsest-shut-in* nil)

(define-test palimpsest
  :description "The palimpsest package: PDFs written here by hand (an original and its incremental updates,
cross-reference and object streams with predictors, white boxes over text, invisible text, signatures that stop
short, a shadowed object, mixed font subsets, metadata that disagrees, data after %%EOF) read from their octets
alone; every revision found, compared and written out with a log naming the offsets and octets of each sign;
hostile files (loops, huge counts, offsets out of range, a stream that unpacks without end) read as far as they
make sense, never hanging or failing; the original never changed; the report and the object view. Pictures
(PNG) examined as one scanned page, what they say of themselves read as data, and a JPEG's second save measured.")

;;; PDFs written by hand

(defun pal-octets (&rest parts)
  "PARTS joined as octets: strings as Latin-1, integers as one octet each, vectors as they are."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (part parts (coerce out '(simple-array (unsigned-byte 8) (*))))
      (etypecase part
        (string (loop for c across part do (vector-push-extend (char-code c) out)))
        (integer (vector-push-extend part out))
        (vector (loop for o across part do (vector-push-extend o out)))))))

(defun pal-object (number body)
  "The octets of the object NUMBER 0: BODY a string, its value, or (:stream ENTRIES OCTETS), a stream with
ENTRIES inside its dictionary and its /Length worked out."
  (if (and (consp body) (eq (first body) :stream))
      (let ((data (if (stringp (third body)) (pal-octets (third body)) (third body))))
        (pal-octets (format nil "~d 0 obj~%<< ~a /Length ~d >>~%stream~%" number (second body) (length data))
                    data (format nil "~%endstream~%endobj~%")))
      (pal-octets (format nil "~d 0 obj~%~a~%endobj~%" number body))))

(defun pal-last-startxref (octets)
  "The offset the last startxref of OCTETS names."
  (let ((at (search (pal-octets "startxref") octets :from-end t)))
    (parse-integer (map 'string #'code-char octets) :start (+ at 10) :junk-allowed t)))

(defun pal-save (base objects &key (trailer "/Root 1 0 R /Info 6 0 R") free)
  "BASE (octets, empty for a new file) with OBJECTS (each (NUMBER . BODY)) saved after it: the objects, a
cross-reference table listing them (and the numbers FREE as free), a trailer with TRAILER and /Prev, startxref
and %%EOF."
  (let* ((out (if (plusp (length base)) (pal-octets base) (pal-octets "%PDF-1.7" 10 "%" 226 227 207 211 10)))
         (offsets '()))
    (dolist (object objects)
      (push (cons (car object) (length out)) offsets)
      (setf out (pal-octets out (pal-object (car object) (cdr object)))))
    (let* ((xref (length out))
           (entries (sort (append (mapcar (lambda (o) (list (car o) (cdr o) "n")) offsets)
                                  (mapcar (lambda (n) (list n 0 "f")) free)
                                  (and (zerop (length base)) (list (list 0 0 "f"))))
                          #'< :key #'first))
           (size (1+ (reduce #'max entries :key #'first)))
           (text (with-output-to-string (s)
                   (format s "xref~%")
                   (loop while entries
                         do (let ((run (loop for e on entries
                                             for k from (first (first entries))
                                             while (= (first (first e)) k) collect (first e))))
                              (format s "~d ~d~%" (first (first run)) (length run))
                              (dolist (e run)
                                (format s "~10,'0d ~5,'0d ~a~c~c" (if (= (first e) 0) 0 (second e))
                                        (if (= (first e) 0) 65535 0) (third e) #\Return #\Newline))
                              (setf entries (nthcdr (length run) entries))))
                   (format s "trailer~%<< /Size ~d ~a~@[ /Prev ~d~] >>~%startxref~%~d~%%%EOF~%"
                           (max size (if (plusp (length base)) 20 0)) trailer
                           (and (plusp (length base)) (pal-last-startxref base)) xref))))
      (pal-octets out text))))

(defparameter *pal-original-objects*
  '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
    (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
    (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >>
/Contents 4 0 R >>")
    (4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F1 12 Tf 72 680 Td (Total: 100) Tj ET")
    (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>")
    (6 . "<< /Producer (Kea Writer 1.0) /Creator (Kea Writer) /CreationDate (D:20260101090000+13'00')
/ModDate (D:20260101090000+13'00') >>"))
  "The objects of the original invoice the tests change.")

(defparameter *pal-edits*
  '((4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F1 12 Tf 72 680 Td (Total: 900) Tj ET")
    (6 . "<< /Producer (iLovePDF) /Creator (Kea Writer) /CreationDate (D:20260101090000+13'00') /ModDate
(D:20260105120000+13'00') >>"))
  "The objects the edited invoice's save writes again (PAL-EDITED).")

(defun pal-original ()
  "The original invoice: one page, its total 100."
  (pal-save #() *pal-original-objects*))

(defun pal-edited ()
  "The invoice changed by a later save: its total 900, saved by an online editor."
  (pal-save (pal-original) *pal-edits*))

(defun pal-read (octets)
  "OCTETS read as a PDF, with its revisions compared and every sign found, as three values: the
PDF, the accounts and the findings."
  (let* ((pdf (palimpsest::read-pdf octets :name "test.pdf"))
         (accounts (loop for r across (palimpsest::pdf-revisions pdf) collect (palimpsest::compare-revision pdf r))))
    (values pdf accounts (palimpsest::all-findings pdf accounts))))

(defun pal-finding (findings strength text)
  "The first of FINDINGS of STRENGTH (or any, for NIL) whose words hold TEXT."
  (find-if (lambda (f) (and (or (null strength) (eq strength (palimpsest::finding-strength f)))
                            (search text (palimpsest::finding-words f))))
           findings))

(defun pal-has (text part)
  "Check that TEXT holds PART."
  (true (search part text) "~s not in~%~a" part text))

(defun pal-zlib-stored (octets)
  "OCTETS as zlib data of stored deflate blocks, as /FlateDecode takes them."
  (let ((parts (list (pal-octets #x78 #x01))))
    (loop for start from 0 below (max 1 (length octets)) by 65535
          do (let* ((end (min (length octets) (+ start 65535))) (n (- end start)))
               (push (pal-octets (if (>= end (length octets)) 1 0) (logand n 255) (ash n -8)
                                 (logand (lognot n) 255) (logand (ash (lognot n) -8) 255) (subseq octets start end))
                     parts)))
    (push (pal-octets 0 0 0 0) parts)
    (apply #'pal-octets (nreverse parts))))

(defun pal-zlib-bomb (repeats)
  "Deflate data, fixed codes, of a zero then REPEATS copies of 258 octets back one: tiny, unpacking
to REPEATS times 258 octets."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (buffer 0) (count 0))
    (labels ((bits (value n)
               (dotimes (k n) (setf buffer (logior buffer (ash (ldb (byte 1 k) value) count))) (incf count)
                               (when (= count 8) (vector-push-extend buffer out) (setf buffer 0 count 0))))
             (code (value n) (loop for k from (1- n) downto 0 do (bits (ldb (byte 1 k) value) 1))))
      (bits 1 1) (bits 1 2)
      (code #b00110000 8)
      (dotimes (k repeats) (code #b11000101 8) (code 0 5))
      (code 0 7)
      (when (plusp count) (vector-push-extend buffer out)))
    (pal-octets #x78 #x01 out)))

;;; Saves, cross-references and objects

(define-test (palimpsest palimpsest-saves-found)
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-edited))
    (let ((revisions (palimpsest::pdf-revisions pdf)))
      (is = 2 (length revisions))
      (is eq :table (palimpsest::revision-xref-kind (aref revisions 0)))
      (is equal '(1 2 3 4 5 6) (palimpsest::revision-added (aref revisions 0)))
      (is equal '(4 6) (palimpsest::revision-changed (aref revisions 1)))
      (is eql (palimpsest::revision-xref-at (aref revisions 0)) (palimpsest::revision-prev (aref revisions 1))))
    ;; the page's text compared line by line
    (let ((change (first (palimpsest::account-changes (second accounts)))))
      (true change)
      (is equal '((:removed "Total: 100") (:added "Total: 900")) (palimpsest::page-change-text change)))
    (true (pal-finding findings :record "The file was changed after it was first saved"))
    (true (pal-finding findings :look "Page 1 was changed in revision 2"))
    (true (pal-finding findings :record "/Producer names iLovePDF"))
    (true (pal-finding findings :look "names another program in /Producer"))))

(defun pal-replace (octets old new)
  "OCTETS with the first OLD (a string) put as NEW, a string of the same length."
  (let ((at (search (pal-octets old) octets)) (out (copy-seq octets)))
    (replace out (pal-octets new) :start1 at)
    out))

(defun pal-replace-grown (octets old new)
  "OCTETS with the first OLD (a string) put as NEW, a string of any length."
  (let ((at (search (pal-octets old) octets)))
    (pal-octets (subseq octets 0 at) new (subseq octets (+ at (length old))))))

(defparameter *pal-samples* (merge-pathnames "test/pdf/" (make-pathname :name nil :type nil :defaults *load-truename*))
  "The directory of the sample PDFs the tests read.")

(defun pal-sample (name)
  "The octets of the sample PDF NAME (test/pdf/)."
  (alexandria:read-file-into-byte-vector (merge-pathnames name *pal-samples*)))

(define-test (palimpsest palimpsest-saves-not-hidden)
  ;; a first save whose stream lost its endstream, or with a stream begun in a comment: both saves are read
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-replace (pal-edited) "endstream" "         "))
    (declare (ignore accounts))
    (is = 2 (length (palimpsest::pdf-revisions pdf)))
    (true (pal-finding findings :record "The file was changed after it was first saved"))
    (true (pal-finding findings :look "Page 1 was changed in revision 2"))
    (true (pal-finding findings :look "whose end the file does not confirm")))
  (let* ((original (pal-original))
         (trailer (search (pal-octets "trailer") original :from-end t))
         (commented (pal-save (pal-octets (subseq original 0 trailer) "%9 0 obj<<>>stream" 10 (subseq original trailer))
                              *pal-edits*)))
    (multiple-value-bind (pdf accounts findings) (pal-read commented)
      (declare (ignore accounts))
      (is = 2 (length (palimpsest::pdf-revisions pdf)))
      (true (pal-finding findings :look "Page 1 was changed in revision 2"))
      (false (pal-finding findings nil "whose end the file does not confirm")))))

;; made-up PDFs: an index entry for an object written nowhere, and one that misses an object written elsewhere

(defun pal-xref-orphan (&key later)
  "The original invoice with an index entry for object 7 that points at object 9, no object 7 written anywhere;
with LATER, that entry is in a second save."
  (flet ((orphan (base)
           (pal-replace (pal-save base (list (cons 7 "<< >>") (cons 9 "<< >>")))
                        "7 0 obj" "9 0 obj")))
    (if later (orphan (pal-original)) (orphan #()))))

(define-test (palimpsest palimpsest-xref-slot-left-by-a-program)
  ;; an entry of the first save for an object written nowhere is for the record; in a later save it stays strong
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-xref-orphan))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :record "not point at the object it names"))
    (false (pal-finding findings :strong "not point at the object")))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-xref-orphan :later t))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "not point at the object it names"))))

(define-test (palimpsest palimpsest-xref-entry-off-its-object)
  ;; entry 7 given the offset of object 8 while object 7 is written: strong in the first save
  (let* ((saved (pal-save #() (list (cons 7 "<< >>") (cons 8 "<< /A 1 >>"))))
         (at7 (search (pal-octets "7 0 obj") saved))
         (at8 (search (pal-octets "8 0 obj") saved))
         (edited (pal-replace saved (format nil "~10,'0d 00000 n" at7) (format nil "~10,'0d 00000 n" at8))))
    (multiple-value-bind (pdf accounts findings) (pal-read edited)
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "not point at the object it names")))))

(define-test (palimpsest palimpsest-startxref-outside-its-save)
  ;; a first save whose startxref names the second save's section: that is said, and its own section is read
  (let* ((original (pal-original))
         (named (pal-last-startxref original))
         (padded (pal-replace-grown original (format nil "startxref~%~d~%" named)
                                    (format nil "startxref~%0~d~%" named)))
         (edited (pal-save padded *pal-edits*))
         (forward (pal-replace edited (format nil "startxref~%0~d~%" named)
                               (format nil "startxref~%~v,'0d~%" (1+ (length (princ-to-string named)))
                                       (pal-last-startxref edited)))))
    (multiple-value-bind (pdf accounts findings) (pal-read forward)
      (declare (ignore accounts))
      (is = 2 (length (palimpsest::pdf-revisions pdf)))
      (is equal '(4 6) (palimpsest::revision-changed (aref (palimpsest::pdf-revisions pdf) 1)))
      (true (pal-finding findings :strong "outside this save"))
      (true (pal-finding findings :look "Page 1 was changed in revision 2"))
      (true (pal-finding findings :record "/Producer names iLovePDF")))))

(define-test (palimpsest palimpsest-inline-picture-at-end)
  ;; a page whose content ends inside an inline picture's dictionary is read to its end
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save #() (mapcar (lambda (o)
                                        (if (eql (car o) 4)
                                            '(4 :stream "" "BT /F1 12 Tf 72 700 Td (Total: 100) Tj ET BI /W 1 /H") o))
                                      *pal-original-objects*)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "could not finish"))))

(define-test (palimpsest palimpsest-save-under-a-length)
  ;; a first save a lying /Length runs over, its startxref blanked, or a later save written whole with no /Prev:
  ;; both saves read, the length named a lie
  (dolist (name '("length-over-save.pdf" "indirect-length-over-save.pdf"
                  "length-over-whole-save.pdf" "length-over-whole-save-unnamed.pdf"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-sample name))
      (declare (ignore accounts))
      (is = 2 (length (palimpsest::pdf-revisions pdf)) "~a" name)
      (true (pal-finding findings :strong "a length written to hide that save") "~a" name)
      (true (pal-finding findings :look "Page 1 was changed in revision 2") "~a" name)
      (true (pal-finding findings :record "/Producer names iLovePDF") "~a" name))))

(define-test (palimpsest palimpsest-attached-pdf)
  ;; a PDF attached whole, its own saves inside a stream: one save, nothing strong
  (dolist (name '("attached-pdf.pdf" "attached-pdf-mutool.pdf"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-sample name))
      (declare (ignore accounts))
      (is = 1 (length (palimpsest::pdf-revisions pdf)) "~a" name)
      (false (find :strong findings :key #'palimpsest::finding-strength) "~a" name)))
  ;; one the document's embedded files name gives no sign of a hidden save
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-sample "attached-pdf.pdf"))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "a save may be hidden in it"))))

(define-test (palimpsest palimpsest-attached-to-its-saves)
  ;; mutool attaching a file to itself, or the original to its later save, uncompressed: nothing strong
  (loop for (name saves) in '(("self-attached.pdf" 2) ("original-attached-to-later.pdf" 3))
        do (multiple-value-bind (pdf accounts findings) (pal-read (pal-sample name))
             (declare (ignore accounts))
             (is = saves (length (palimpsest::pdf-revisions pdf)) "~a" name)
             (false (find :strong findings :key #'palimpsest::finding-strength) "~a" name))))

(define-test (palimpsest palimpsest-save-inferred-in-stream)
  ;; a save hidden within a stream's data, nothing naming it: worth a look, its page change seen, never strong
  (dolist (name '("save-in-stream-xref-stream.pdf" "save-in-stream-offsets.pdf" "save-in-stream-bare.pdf"
                  "save-in-stream-said-pdf.pdf" "save-in-stream-said-embedded.pdf"
                  "save-in-stream-own-file-reached.pdf" "save-in-stream-own-file-unreached.pdf"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-sample name))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "a save may be hidden in it") "~a" name)
      (true (pal-finding findings :look "Page 1 reads otherwise") "~a" name)
      (false (find :strong findings :key #'palimpsest::finding-strength) "~a" name))))

(define-test (palimpsest palimpsest-many-quoted-eof)
  ;; a stream of the first save quoting more %%EOF lines than are read: both saves read, the page change seen
  (let* ((quoted (with-output-to-string (out) (loop repeat 100001 do (format out "%%EOF~%"))))
         (original (pal-save #() (append *pal-original-objects* (list (list 9 :stream "" quoted))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-save original *pal-edits*))
      (declare (ignore accounts))
      (is = 2 (length (palimpsest::pdf-revisions pdf)))
      (false (find-if (lambda (n) (search "no %%EOF line" n)) (palimpsest::pdf-notes pdf)))
      (true (pal-finding findings :look "Page 1 was changed in revision 2")))))

(define-test (palimpsest palimpsest-junk-before-header)
  ;; octets before %PDF-: offsets count from the header, the saves read as readers take them, nothing strong
  (dolist (junk '(50 3000))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-octets (make-string junk :initial-element #\J) (pal-edited)))
      (declare (ignore accounts))
      (is = junk (palimpsest::pdf-base pdf))
      (is = 2 (length (palimpsest::pdf-revisions pdf)))
      (false (find :strong findings :key #'palimpsest::finding-strength))
      (true (pal-finding findings :look "Page 1 was changed in revision 2")))))

(define-test (palimpsest palimpsest-cross-reference-streams)
  ;; objects packed in a Flate object stream, a cross-reference stream with a PNG predictor
  (let* ((packed-objects '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
                           (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                           (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font
<< /F1 5 0 R >> >> /Contents 4 0 R >>")
                           (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")))
         (bodies (mapcar (lambda (o) (format nil "~a " (cdr o))) packed-objects))
         (header (format nil "~{~a ~}" (loop for o in packed-objects for k from 0
                                             collect (format nil "~d ~d" (car o)
                                                             (reduce #'+ (subseq bodies 0 k) :key #'length)))))
         (data (pal-octets header (format nil "~{~a~}" bodies)))
         (out (pal-octets "%PDF-1.5" 10))
         (places '()))
    (push (cons 4 (length out)) places)
    (setf out (pal-octets out (pal-object 4 '(:stream "" "BT /F1 10 Tf 50 50 Td (Packed hello) Tj ET"))))
    (push (cons 6 (length out)) places)
    (setf out (pal-octets out (pal-object 6
                                          (list :stream (format nil
                                                                "/Type /ObjStm /N 4 /First ~d /Filter /FlateDecode"
                                                                (length header))
                                                  (pal-zlib-stored data)))))
    (let* ((xref-at (length out))
           (rows (loop for n from 0 to 7
                       collect (cond ((= n 0) (list 0 0 255))
                                     ((assoc n places) (list 1 (cdr (assoc n places)) 0))
                                     ((= n 7) (list 1 xref-at 0))
                                     (t (list 2 6 (position n packed-objects :key #'car))))))
           ;; each row: type (1), offset (2), third (1), each row after the Up predictor's tag 2
           (raw (loop for (a b c) in rows append (list a (ash b -8) (logand b 255) c)))
           (predicted (let ((previous (list 0 0 0 0)) (out '()))
                        (loop for k from 0 below (length raw) by 4
                              do (let ((row (subseq raw k (+ k 4))))
                                   (push 2 out)
                                   (loop for x in row for p in previous do (push (logand 255 (- x p)) out))
                                   (setf previous row)))
                        (coerce (nreverse out) '(vector (unsigned-byte 8))))))
      (setf out (pal-octets out (pal-object 7
                                            (list :stream (format nil "/Type /XRef /Size 8 /W [1 2 1] /Root 1 0 R ~
                                                                       /Filter /FlateDecode /DecodeParms << /Predictor ~
                                                                       12 /Columns 4 >>")
                                                    (pal-zlib-stored predicted)))
                            (format nil "startxref~%~d~%%%EOF~%" xref-at))))
    (multiple-value-bind (pdf) (pal-read out)
      (let ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
        (is eq :stream (palimpsest::revision-xref-kind revision))
        (is eq :packed (palimpsest::entry-kind (palimpsest::lookup pdf revision 3)))
        (let ((pages (palimpsest::pages-of pdf revision)))
          (is = 1 (length pages))
          (is equal '("Packed hello") (palimpsest::text-lines (palimpsest::marks-of pdf revision (first pages))))
          ;; objects in object streams are told alike by their values, not by how the thread prints them
          (let ((*print-level* 1) (*print-length* 2))
            (false (palimpsest::objects-alike-p pdf (palimpsest::revision-object pdf revision 3)
                                                (palimpsest::revision-object pdf revision 5)))))))))

(define-test (palimpsest palimpsest-decoders)
  ;; a stream that would unpack without end is stopped at the limit, said so
  (multiple-value-bind (data why)
      (tuatara:pdf-stream-octets '(("/Filter" . "/FlateDecode")) (pal-zlib-bomb 4000) :limit (* 256 1024))
    (false data)
    (pal-has why "more than")))

;;; What pages draw

(defun pal-one-page (content &key (fonts "/F1 5 0 R") (more '()))
  "A one-page PDF drawing CONTENT with FONTS, MORE objects beside."
  (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                              '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                              (cons 3
                                    (format nil
                                            "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font ~
                                             << ~a >> >> /Contents 4 0 R >>" fonts))
                              (list 4 :stream "" content)
                              '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                              '(6 . "<< /Producer (Kea Writer) >>"))
                        more)))

(define-test (palimpsest palimpsest-content-signs)
  ;; a white box drawn over text: the text named, the operators in the log's evidence
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET 1 1 1 rg 70 695 100 20 re f"))
    (declare (ignore accounts))
    (let ((covered (pal-finding findings :strong "covered by a white shape")))
      (true covered)
      (pal-has (palimpsest::finding-words covered) "Secret 42")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence covered)) "1 1 1 rg")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence covered)) "70 695 100 20 re f")
      (true pdf)))
  ;; invisible, tiny and white text, text off the page
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 3 Tr 72 600 Td (Hidden words) Tj 0 Tr ET BT /F1 0.5 Tf 72 500 ~
                                           Td (Tiny print) Tj ET BT 1 1 1 rg /F1 12 Tf 72 400 Td (White words) Tj 0 g ~
                                           ET BT /F1 12 Tf 900 900 Td (Far away) Tj ET")))
    (declare (ignore pdf accounts))
    (true (pal-finding findings nil "holds invisible text: \"Hidden words\""))
    (true (pal-finding findings :look "too small to read"))
    (true (pal-finding findings nil "white text on a white page"))
    (true (pal-finding findings :look "lies outside the page")))
  ;; a line drawn character by character, figures in another font
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           (format nil "BT /F1 12 Tf 72 700 Td (The quick brown fox) Tj ET BT /F1 12 ~
                                                        Tf 72 680 Td (jumps over the lazy dog) Tj ET ")
                                           (format nil "BT /F1 12 Tf 72 660 Td (Amount due today) Tj ET BT /F1 12 Tf ~
                                                        72 640 Td (Thank you for your business in 2026) Tj ET ")
                                           (format nil "BT /F1 12 Tf 72 620 Td (T) Tj (o) Tj (t) Tj (a) Tj (l) Tj (d) ~
                                                        Tj (u) Tj (e) Tj (n) Tj (o) Tj (w) Tj ET ")
                                           (format nil "BT /F1 12 Tf 72 600 Td (Paid on account:) Tj /F2 12 Tf 180 0 ~
                                                        Td (950) Tj ET"))
                              :fonts "/F1 5 0 R /F2 7 0 R"
                              :more '((7 . "<< /Type /Font /Subtype /Type1 /BaseFont /Courier >>"))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "1 line on page 1 is drawn a character at a time"))
    (let ((retyped (pal-finding findings :look "1 figure is in another font than the line around it")))
      (true retyped)
      (is equal '("\"950\" in Courier, its line in Helvetica (4 @379 font 7)") (palimpsest::finding-details retyped))))
  ;; one font in two subsets, a ToUnicode map read
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td <00010002> Tj ET BT /F2 12 Tf 72 680 Td (Second) Tj ET"
                              :fonts "/F1 7 0 R /F2 8 0 R"
                              :more (list '(7 . #.(concatenate 'string "<< /Type /Font /Subtype /Type0 /BaseFont "
                                                                       "/ABCDEF+Arial /Encoding /Identity-H "
                                                                       "/DescendantFonts [9 0 R] /ToUnicode 10 0 R >>"))
                                          '(8 . "<< /Type /Font /Subtype /TrueType /BaseFont /GHIJKL+Arial >>")
                                          '(9 . #.(concatenate 'string "<< /Type /Font /Subtype /CIDFontType2 "
                                                                       "/BaseFont /ABCDEF+Arial /W [1 [500 600]] >>"))
                                          (list 10 :stream ""
                                                (format nil "/CIDInit /ProcSet findresource begin 1 ~
                                                             begincodespacerange <0000> <FFFF> endcodespacerange 2 ~
                                                             beginbfchar <0001> <004B> <0002> <0065> endbfchar end")))))
    (declare (ignore accounts))
    (true (pal-finding findings :record "The font Arial is in the file as 2 subsets (ABCDEF+Arial, GHIJKL+Arial)"))
    (let* ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (is equal '("Ke" "Second")
          (palimpsest::text-lines (palimpsest::marks-of pdf revision (first (palimpsest::pages-of pdf revision))))))))

(define-test (palimpsest palimpsest-stream-length-line-end)
  ;; a /Length reaching endstream, or a line end before it, agrees: data ending in a carriage return before
  ;; the line feed, data ending in a line feed with endstream after it at once; a /Length two octets past
  ;; the data, or one short, disagrees
  (flet ((named-p (body length)
           (let ((pdf (palimpsest::read-pdf
                       (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                           '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                           '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792]
/Contents 4 0 R >>")
                                           (cons 4 (format nil "<< /Length 7 0 R >>~%stream~%~a" body))
                                           (cons 7 (princ-to-string length))))
                       :name "test.pdf")))
             (and (palimpsest::check-lengths pdf) t))))
    (false (named-p (format nil "BT ET~c~cendstream" #\Return #\Newline) 6))
    (false (named-p (format nil "BT ET~%endstream") 6))
    (false (named-p (format nil "BT ET~%endstream") 5))
    (true (named-p (format nil "BT ET~%endstream") 7))
    (true (named-p (format nil "BT ET~%endstream") 4))))

(define-test (palimpsest palimpsest-hidden-text-letters)
  ;; invisible text of fewer than three letters or figures, or of none: for the record; of three, worth a look
  (dolist (case '(("a." :record) ("--" :record) ("abc" :look)))
    (destructuring-bind (text strength) case
      (let ((findings (nth-value 2 (pal-read (pal-one-page (format nil "BT /F1 12 Tf 3 Tr 72 600 Td (~a) Tj 0 Tr ~
                                                                        ET BT /F1 12 Tf 72 700 Td (Seen) Tj ET"
                                                                   text))))))
        (let ((hidden (pal-finding findings nil "holds invisible text")))
          (true hidden "~s" text)
          (when hidden (is eq strength (palimpsest::finding-strength hidden) "~s" text)))))))

(define-test (palimpsest palimpsest-text-layout-shown-and-habit)
  ;; invisible lines, as text recognition lays over a scan, are not set beside each other's edges
  (let ((findings (nth-value 2 (pal-read (pal-one-page (format nil "~{BT /F1 12 Tf 3 Tr ~a ~d Td (~a) Tj 0 Tr ET ~}"
                                                               '(72 700 "Invoice for Kea Builders"
                                                                 72 680 "To Tui Gardens Ltd" 72.6 660 "Fence repair"
                                                                 72 640 "New gate and latch" 72 620 "Total due")))))))
    (false (pal-finding findings nil "set a little off the edge") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; three lines off the margin in one file: how its program sets text, said once for the record
  (let ((findings (nth-value 2 (pal-read (pal-one-page (format nil "~{BT /F1 12 Tf ~a ~d Td (~a) Tj ET ~}"
                                                               '(72 700 "Invoice for Kea Builders"
                                                                 72.6 680 "To Tui Gardens Ltd" 72 660 "Fence repair"
                                                                 72.5 640 "New gate and latch" 72 620 "Total due"
                                                                 72.7 600 "Paid with thanks")))))))
    (false (pal-finding findings :look "set a little off the edge") "~s" (mapcar #'palimpsest::finding-words findings))
    (let ((habit (pal-finding findings :record "The file sets 3 lines, runs or figures of its text apart")))
      (true habit "~s" (mapcar #'palimpsest::finding-words findings))
      (when habit
        (pal-has (palimpsest::finding-words habit) "on page 1")
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details habit))
                 "\"To Tui Gardens Ltd\" starts at x 72.60")))))

(define-test (palimpsest palimpsest-content-appended)
  ;; a later save lays a second content stream over the page
  (let* ((original (pal-original))
         (edited (pal-save original
                           '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                                                         "/Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 7 0 "
                                                         "R] >>"))
                             (7 :stream "" "1 1 1 rg 70 675 120 20 re f BT /F1 12 Tf 72 680 Td (Total: 50) Tj ET")))))
    (multiple-value-bind (pdf accounts findings) (pal-read edited)
      (declare (ignore pdf))
      (true (pal-finding findings :strong "one of them appended in a later save"))
      (true (pal-finding findings :strong "covered by a white shape drawn over it, added in a later save"))
      (let ((change (first (palimpsest::account-changes (second accounts)))))
        (true (find "Total: 50" (palimpsest::page-change-added change) :key #'palimpsest::mark-text :test #'equal))))))

;;; Metadata, ends

(define-test (palimpsest palimpsest-metadata-and-ends)
  (let* ((xmp (format nil "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta ~
                           xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'> ~
                           <rdf:Description xmlns:xmp='http://ns.adobe.com/xap/1.0/' ~
                           xmlns:xmpMM='http://ns.adobe.com/xap/1.0/mm/' ~
                           xmlns:stEvt='http://ns.adobe.com/xap/1.0/sType/ResourceEvent#' ~
                           xmp:CreateDate='2020-03-01T09:00:00+13:00' xmp:CreatorTool='Kea Writer' ~
                           xmpMM:DocumentID='uuid:kea-1' xmpMM:OriginalDocumentID='uuid:other-9'> ~
                           <xmpMM:History><rdf:Seq><rdf:li stEvt:action='created' ~
                           stEvt:when='2020-03-01T09:00:00+13:00' stEvt:softwareAgent='Kea Writer'/> <rdf:li ~
                           stEvt:action='saved' stEvt:when='2026-01-05T12:00:00+13:00' stEvt:softwareAgent='Sejda ~
                           PDF'/></rdf:Seq></xmpMM:History> </rdf:Description></rdf:RDF></x:xmpmeta><?xpacket ~
                           end='w'?>"))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                     '(6 . #.(concatenate 'string "<< /CreationDate (D:20260301090000+13'00') /ModDate "
                                                                  "(D:20250101090000+13'00') /Creator (Kea Writer) >>"))
                                     (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))
         (with-tail (pal-octets octets "1 0 obj << /Hidden (after the end) >> endobj" 10)))
    (multiple-value-bind (pdf accounts findings) (pal-read with-tail)
      (declare (ignore accounts))
      (true (pal-finding findings :strong "give different creation dates"))
      (true (pal-finding findings :strong "was last changed"))
      (true (pal-finding findings :look "edit history of 2 steps, by Sejda"))
      (true (pal-finding findings :look "made from another one"))
      (let ((tail (pal-finding findings :strong "follow the last %%EOF")))
        (true tail)
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence tail)) "/Hidden (after the end)"))
      (is = 1 (length (palimpsest::pdf-revisions pdf))))))

(defun pal-dated (made changed &optional (tail ""))
  "The findings of a PDF of no pages whose document information says it was made at MADE and changed at
CHANGED (PDF dates without D:), TAIL written after its %%EOF."
  (nth-value 2 (pal-read (pal-octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                                         '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                                         (cons 6 (format nil "<< /CreationDate (D:~a) ~
                                                                              /ModDate (D:~a) >>"
                                                                         made changed))))
                                     tail))))

(define-test (palimpsest palimpsest-honest-ends-and-clocks)
  ;; a printing system's banner page carries comment lines after %%EOF, for the record, and a program writing a
  ;; twelve-hour clock dates a file changed half a day before it was made, worth a look; other data after the end
  ;; and a file changed more than a day before it was made stay strong
  (flet ((findings (made changed tail) (pal-dated made changed tail)))
    (let ((found (findings "20250310080000+03'00'" "20250309200500+03'00'"
                           (format nil "%%#PDF-BANNER~%%%Show printer-name printer-info job-id time-at-creation ~
                                        job-originating-user-name job-name~%"))))
      (true (pal-finding found :record "follow the last %%EOF") "~s" (mapcar #'palimpsest::finding-words found))
      (false (pal-finding found :strong "follow the last %%EOF"))
      (true (pal-finding found :look "or a clock or a time zone written wrong"))
      (false (pal-finding found :strong "was last changed")))
    (let ((found (findings "20250310080000+03'00'" "20250309060000+03'00'"
                           (format nil "%%#PDF-BANNER~%Show printer-name printer-info job-id time-at-creation ~
                                        job-originating-user-name job-name~%"))))
      (true (pal-finding found :strong "follow the last %%EOF") "~s" (mapcar #'palimpsest::finding-words found))
      (true (pal-finding found :strong "was last changed")))))

(defun pal-tail-strength (tail)
  "How strong the finding of data after the last %%EOF is for a PDF of no pages with TAIL written after its
%%EOF line; NIL for no finding."
  (let ((tail-finding (find-if (lambda (f)
                                 (let ((words (palimpsest::finding-words f)))
                                   (or (search "follow the last %%EOF" words) (search "follows the last %%EOF" words))))
                               (pal-dated "20260101000000Z" "20260101000000Z" tail))))
    (and tail-finding (palimpsest::finding-strength tail-finding))))

(define-test (palimpsest palimpsest-comment-lines-bounded)
  ;; after %%EOF only what honest programs write is worth a look: a banner page's instructions as CUPS writes
  ;; them and printer job language lines, each up to 1 KB; any other text of 16 octets or more is strong
  (let ((hex-line (format nil "%~{~2,'0x~}~%" (loop for k below 40 collect (mod (* k 37) 256))))
        (banner "%%#PDF-BANNER~%%%Show printer-name job-id time-at-creation job-name job-billing"))
    (is eq :strong (pal-tail-strength (format nil "~{~a~}" (loop repeat 3 collect hex-line))))
    (is eq :strong (pal-tail-strength (format nil "% kept after the end: the first draft said Total: $1,200.00~%")))
    (is eq :look (pal-tail-strength (format nil "% draft 2~%")))
    (is eq :record (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Font UseDefault~%%%Font-size 11~%")))
    (is eq :record (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Show~{ ~a~}~%"
                                            '("printer-name" "printer-info" "printer-location" "printer-make-and-model"
                                              "printer-driver-name" "printer-driver-version" "paper-size"
                                              "imageable-area" "job-id" "options" "time-at-creation"
                                              "time-at-processing" "job-originating-user-name" "job-name"
                                              "job-originating-host-name" "job-billing" "job-uuid"))))
    (is eq :look (pal-tail-strength (pal-octets 27 "%-12345X@PJL EOJ NAME=\"report\"" 13 10 27 "%-12345X")))
    (is eq :strong (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Kept the first draft said Total: $1,200.00~%")))
    (is eq :strong (pal-tail-strength (pal-octets 27 "%-12345X" "the first draft said Total: $1,200.00" 10)))
    (is eq :strong (pal-tail-strength (format nil "%%#PDF-BANNER~%~{%%Notice ~a~%~}"
                                              (loop repeat 8 collect (make-string 200 :initial-element #\A)))))
    (is eq :strong (pal-tail-strength (pal-octets (format nil banner) 0 "job-uuid job-originating-host-name" 10)))
    (is eq :strong (pal-tail-strength (pal-octets (format nil banner) 12 "job-uuid job-originating-host-name" 10)))))

(define-test (palimpsest palimpsest-white-space-after-the-end)
  ;; honest files end with %%EOF and a line end, a mail program may add one more: nothing; more white space is
  ;; worth a look, and 1 KB of it or spaces and tabs mixed as a code writes them are strong
  (flet ((run (count &rest codes)
           (coerce (loop for k below count collect (nth (mod k (length codes)) codes)) '(vector (unsigned-byte 8)))))
    (is eq nil (pal-tail-strength ""))
    (is eq nil (pal-tail-strength (run 1 10)))
    (is eq nil (pal-tail-strength (run 4 13 10)))
    (is eq :look (pal-tail-strength (run 5 10)))
    (is eq :look (pal-tail-strength (run 1 0)))
    (is eq :look (pal-tail-strength (run 600 32)))
    (is eq :look (pal-tail-strength (run 40 32 9)))
    (is eq :strong (pal-tail-strength (pal-octets (run 2 10) (run 1500 0))))
    (is eq :strong (pal-tail-strength (run 64 32 9 9 32 32 32 9)))
    (is eq :strong (pal-tail-strength (run 100 0 0 32)))
    (pal-has (palimpsest::finding-words (find-if (lambda (f) (search "follow the last %%EOF"
                                                                      (palimpsest::finding-words f)))
                                                 (pal-dated "20260101000000Z" "20260101000000Z" (run 40 32 9))))
             "white space after the end, which can hold data")))

(define-test (palimpsest palimpsest-padding-after-the-end)
  ;; one octet repeated after %%EOF holds no data: a copy padded to a block's size, a carved file, the Ctrl-Z
  ;; padding of old transfers; worth a look at any length, named as padding
  (flet ((run (count code) (make-array count :element-type '(unsigned-byte 8) :initial-element code)))
    (is eq :look (pal-tail-strength (run 3431 0)))
    (is eq :look (pal-tail-strength (pal-octets 13 10 (run 3431 0))))
    (is eq :look (pal-tail-strength (run 103 26)))
    (is eq :look (pal-tail-strength (run 2000 9)))
    (is eq :look (pal-tail-strength (run 200000 10)))
    (is eq :strong (pal-tail-strength (pal-octets (run 3000 0) "the first draft said Total: $1,200.00")))
    (pal-has (palimpsest::finding-words (find-if (lambda (f) (search "the last %%EOF" (palimpsest::finding-words f)))
                                                 (pal-dated "20260101000000Z" "20260101000000Z" (run 3431 0))))
             "padding from a copy made to a block's size")))

(define-test (palimpsest palimpsest-line-ends-coding-after-the-end)
  ;; carriage returns and line feeds mixed at random after %%EOF, or spaces and line feeds, can carry bits as
  ;; spaces and tabs do: strong from 64 octets, as no honest PDF has white space there
  (let ((state (sb-ext:seed-random-state 11)))
    (flet ((coded (count a b)
             (coerce (loop repeat count collect (if (zerop (random 2 state)) a b)) '(vector (unsigned-byte 8)))))
      (is eq :strong (pal-tail-strength (coded 900 13 10)))
      (is eq :strong (pal-tail-strength (coded 900 32 10)))
      (is eq :strong (pal-tail-strength (coded 64 13 10)))
      (is eq :look (pal-tail-strength (coded 40 13 10)))
      (is eq nil (pal-tail-strength (pal-octets 13 10 13 10))))))

(define-test (palimpsest palimpsest-white-space-within-saves)
  ;; white space mixing its kinds hidden within the last save, between the trailer and startxref, between
  ;; startxref and %%EOF, between objects or after a stream's data, is strong, named where it lies; spaces
  ;; between two line ends, as writers pad with, and a few line ends and spaces, are not
  (let* ((state (sb-ext:seed-random-state 29))
         (mixed (map 'string #'code-char (loop repeat 3000 collect (nth (random 5 state) '(0 9 10 13 32)))))
         (base (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                   '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                   '(5 :stream "" "a stream's data")
                                   '(6 . "<< /Producer (Kea) >>")))))
    (labels ((hidden (octets)
               (find-if (lambda (f) (search "octets of white space mixing" (palimpsest::finding-words f)))
                        (nth-value 2 (pal-read octets))))
             (before (mark white)
               (pal-replace-grown base mark (concatenate 'string white mark))))
      (loop for (mark place) in '(("startxref" "between the trailer and startxref")
                                  ("%%EOF" "between startxref and %%EOF")
                                  ("6 0 obj" "between objects")
                                  ("endstream" "after the data of object 5's stream"))
            do (let ((found (hidden (before mark mixed))))
                 (true found "~a" place)
                 (when found
                   (is eq :strong (palimpsest::finding-strength found))
                   (pal-has (palimpsest::finding-words found) place)
                   (pal-has (palimpsest::finding-words found) "3,00")
                   (pal-has (format nil "~{~a~^; ~}" (palimpsest::finding-details found)) "NULs (0x00)"))))
      (false (hidden base))
      (false (hidden (before "6 0 obj" (format nil "~%~a~%" (make-string 3000 :initial-element #\Space)))))
      (false (hidden (before "6 0 obj" (format nil "~c~c ~c~c  " #\Return #\Newline #\Return #\Newline)))))))

(define-test (palimpsest palimpsest-white-space-honest-layouts)
  ;; what honest writers lay out within a file is not named: blank lines ending in carriage returns and line
  ;; feeds, lines indented with spaces, a string of spaces in UTF-16, and the data of a stream whose end is
  ;; not where its /Length says or is never found, however much white space that data holds
  (let ((base (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                  '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                  '(6 . "<< /Producer (Kea) >>"))))
        (dark (let ((state (sb-ext:seed-random-state 31)))
                ;; a picture's dark rows kept without compression: white space, and now and then another octet
                (map 'string #'code-char (loop for k below 3000
                                               collect (if (zerop (mod k 500)) 1 (nth (random 4 state) '(0 9 32 12))))))))
    (flet ((named (octets)
             (find-if (lambda (f) (search "octets of white space mixing" (palimpsest::finding-words f)))
                      (nth-value 2 (pal-read octets))))
           (before (mark text) (pal-replace-grown base mark (concatenate 'string text mark))))
      (false (named (before "6 0 obj" (format nil "~{~a~}" (loop repeat 32 collect (format nil "~c~c" #\Return
                                                                                              #\Newline))))))
      (false (named (before "6 0 obj" (format nil "~{~a~}" (loop repeat 9 collect (format nil "       ~%"))))))
      (false (named (pal-replace-grown base "(Kea)"
                                       (format nil "(~c~c~{~a~})" (code-char 254) (code-char 255)
                                               (loop repeat 40 collect (format nil "~c " (code-char 0)))))))
      (false (named (before "6 0 obj" (format nil "5 0 obj~%<< /Length 5 >>~%stream~%~a~%endstream~%endobj~%" dark))))
      (false (named (before "6 0 obj" (format nil "5 0 obj~%<< /Length 99999 >>~%stream~%~a~%" dark)))))))

(define-test (palimpsest palimpsest-white-space-mixed-named)
  ;; a long run mixing NULs, tabs, form feeds and spaces after %%EOF is strong, named by its kinds with each
  ;; counted, a few other octets among it or not; padding after one line end stays worth a look
  (let* ((state (sb-ext:seed-random-state 23))
         (mixed (coerce (loop repeat 1400000 collect (nth (random 4 state) '(0 9 12 32))) '(vector (unsigned-byte 8))))
         (found (find-if (lambda (f) (search "the last %%EOF" (palimpsest::finding-words f)))
                         (pal-dated "20260101000000Z" "20260101000000Z" mixed))))
    (true found)
    (when found
      (is eq :strong (palimpsest::finding-strength found))
      (pal-has (palimpsest::finding-words found) "1,400,000 octets of white space follow the last %%EOF")
      (pal-has (palimpsest::finding-words found) "mixed, as a code hiding bits in white space")
      (dolist (kind '("NULs" "tabs" "form feeds" "spaces"))
        (pal-has (palimpsest::finding-words found) kind))
      (is = 4 (length (palimpsest::finding-details found)))
      (pal-has (format nil "~{~a~^; ~}" (palimpsest::finding-details found)) "NULs (0x00)"))
    (is eq :strong (pal-tail-strength (pal-octets mixed (make-array 10 :element-type '(unsigned-byte 8)
                                                                       :initial-element 26))))
    (is eq :strong (pal-tail-strength (pal-octets (subseq mixed 0 200) "ab")))
    (is eq :look (pal-tail-strength (pal-octets 13 10 (make-array 3000 :element-type '(unsigned-byte 8)
                                                                       :initial-element 0))))
    (is eq :look (pal-tail-strength (pal-octets 10 (make-array 6 :element-type '(unsigned-byte 8)
                                                                 :initial-element 10))))
    ;; one octet repeated to a block's size and a few others at its end, as a carved file's slack, stays a look
    (is eq :look (pal-tail-strength (pal-octets (make-array 2000 :element-type '(unsigned-byte 8) :initial-element 0)
                                                26 26 26)))
    ;; and so does such padding with a line end before its Ctrl-Z
    (is eq :look (pal-tail-strength (pal-octets (make-array 2000 :element-type '(unsigned-byte 8) :initial-element 0)
                                                10 26)))))

(define-test (palimpsest palimpsest-honest-tail-values)
  ;; a banner's or a printer job's lines are worth a look only with the values CUPS and drivers write: base64 or
  ;; other encoded data carried in their shape is strong
  (let* ((state (sb-ext:seed-random-state 5))
         (alphabet "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")
         (exit (code-char 27)))
    (flet ((encoded (count)
             (coerce (loop repeat count collect (char alphabet (random 64 state))) 'string)))
      (is eq :strong (pal-tail-strength (format nil "%%#PDF-BANNER~%~{%%Show ~a~%~}"
                                                (loop repeat 5 collect (encoded 190)))))
      (is eq :strong (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Show job-id ~{~a~^ ~}~%"
                                                (loop repeat 40 collect (encoded 20)))))
      (is eq :strong (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Notice ~{~a~^ ~}~%"
                                                (loop repeat 5 collect (encoded 20)))))
      (is eq :strong (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Notice Ask at the desk~%~
                                                     %%Notice Ask at the desk~%")))
      (is eq :record (pal-tail-strength (format nil "%%#PDF-BANNER~%%%Notice Printed by the print room, ask at ~
                                                   the desk~%%%Show job-id job-name~%")))
      (is eq :look (pal-tail-strength (format nil "~c%-12345X@PJL COMMENT Printed by a driver v2.1~c~%~
                                                   @PJL EOJ NAME=\"report 7\"~c~%~c%-12345X"
                                              exit #\Return #\Return exit)))
      (is eq :strong (pal-tail-strength (format nil "~c%-12345X@PJL COMMENT ~{~a~^ ~}~%~c%-12345X"
                                                exit (loop repeat 3 collect (encoded 20)) exit)))
      (is eq :strong (pal-tail-strength (format nil "~c%-12345X~{@PJL SET JOBNAME=\"~a\"~%~}"
                                                exit (loop repeat 9 collect (encoded 30)))))
      (is eq :strong (pal-tail-strength (format nil "~c%-12345X@PJL ECHO ~a~%" exit (encoded 30)))))))

(define-test (palimpsest palimpsest-changed-before-made-by-a-slip)
  ;; changed before made is worth a look only as an honest clock slips: zones that differ, an hour, twelve hours;
  ;; any other gap under a day stays strong
  (flet ((strength (made changed)
           (palimpsest::finding-strength
            (find-if (lambda (f) (search "was last changed" (palimpsest::finding-words f))) (pal-dated made changed)))))
    (loop for (made changed) in '(("20240305101000+01'00'" "20240305104100+02'00'")
                                  ("20230612110000+01'00'" "20230612101400+01'00'")
                                  ("20220920090000Z" "20220920091200+02'00'")
                                  ("20210415140000Z" "20210415135800+02'00'")
                                  ("20250310080000+03'00'" "20250309200500+03'00'")
                                  ("20260417200000+02'00'" "20260417060000+01'00'"))
          do (is eq :look (strength made changed) "~a ~a" made changed))
    (loop for (made changed) in '(("20260417120000+01'00'" "20260417060000+01'00'")
                                  ("20260417200000+02'00'" "20260417020000+01'00'")
                                  ("20260417120000+01'00'" "20260416230000+01'00'"))
          do (is eq :strong (strength made changed) "~a ~a" made changed))))

;;; Signatures

(defun pal-signed (&key (short 0) fake-linearised rights)
  "The invoice signed in its first save, its /ByteRange covering it all but SHORT octets less; with
FAKE-LINEARISED, a linearisation dictionary first whose /H names the unused stream 9 as its hint stream,
though the file is no file prepared for the web; with RIGHTS, the signature one of usage rights (/UR3),
as a form's publisher gives a reader's program the right to save it filled in."
  (let* ((placeholder "0000000000")
         (base (pal-save #()
                         (append (and fake-linearised
                                      (list '(10 . #.(concatenate 'string "<< /Linearized 1 /L 99999 /O 3 /E 0 /N 1 /T "
                                                                          "0 /H [HHHHHHHHHH 40] >>"))))
                                     (butlast *pal-original-objects*)
                                     (list '(6 . "<< /Producer (Kea Writer 1.0) >>")
                                           (cons 8
                                                 (format nil
                                                         "<< /Type /Sig /Filter /Adobe.PPKLite /SubFilter ~
                                                          /adbe.pkcs7.detached /Name (Kea Signer) /M ~
                                                          (D:20260102100000+13'00')~@[ ~a~] /ByteRange [0 ~a ~a ~a] ~
                                                          /Contents <~a> >>"
                                                           (and rights
                                                                (concatenate 'string "/Reference [<< /Type /SigRef "
                                                                             "/TransformMethod /UR3 /TransformParams <<"
                                                                             " /Type /TransformParams /V /2.2 >> >>]"))
                                                           placeholder placeholder placeholder
                                                           (make-string 64 :initial-element #\0)))
                                           (list 9 :stream "" "BT /F1 12 Tf 72 300 Td (Shadow total: 1) Tj ET")))))
         (text (map 'string #'code-char base))
         (open (position #\< text :start (search "/Contents" text)))
         (close (1+ (position #\> text :start open)))
         (range (format nil "[0 ~10,'0d ~10,'0d ~10,'0d]" open close (- (length base) close short)))
         (at (search "[0 0000000000" text)))
    (when fake-linearised
      (let ((hint (search "HHHHHHHHHH" text)))
        (setf text
              (concatenate 'string (subseq text 0 hint)
                           (format nil "~10,'0d" (search (format nil "~%9 0 obj") text :start2 0))
                           (subseq text (+ hint 10)))
              base (map '(vector (unsigned-byte 8)) #'char-code text))))
    (pal-octets (subseq base 0 at) range (subseq base (+ at (length range))))))

(define-test (palimpsest palimpsest-one-item-fails-alone)
  ;; a signature whose /ByteRange gap starts before the file: named for it, the check not given up
  (let* ((signed (pal-signed))
         (at (+ (search (pal-octets "/ByteRange [0 ") signed) 14))
         (findings (nth-value 2 (pal-read (pal-replace signed (map 'string #'code-char (subseq signed at (+ at 10)))
                                                       "-000000001")))))
    (false (pal-finding findings nil "could not finish") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :strong "leaves out more than its own value")))
  ;; an item whose check fails adds its own record to the findings before it
  (let ((out (list :before)))
    (palimpsest::guarding-item (out "item ~d" 7) (error "malformed"))
    (is = 2 (length out))
    (pal-has (palimpsest::finding-words (first out)) "The check of item 7 could not finish")))

(define-test (palimpsest palimpsest-signatures)
  ;; signed, then a later save changes the page and brings an object hidden in the signed part into use
  (let ((after (pal-save (pal-signed)
                         '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                                                       "/Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 9 0 R] "
                                                       ">>"))))))
    (multiple-value-bind (pdf accounts findings) (pal-read after)
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "An approval signature (object 8), dated 2026-01-02"))
      (true (pal-finding findings :strong "after the signature in object 8, changes what the document shows"))
      (true (pal-finding findings :strong "Object 9 was hidden, unused, in the part the signature in object 8 covers"))
      (false (pal-finding findings :strong "stops short"))))
  ;; an approval signature, then a note: annotations after it are allowed
  (let ((noted (pal-save (pal-signed)
                         '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                                                       "/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R /Annots "
                                                       "[10 0 R] >>"))
                           (10 . "<< /Type /Annot /Subtype /Text /Rect [72 600 92 620]
/Contents (Checked, R Waimate) >>")))))
    (multiple-value-bind (pdf accounts findings) (pal-read noted)
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "do not allow"))
      (true (pal-finding findings :look "after the signature in object 8, adds or changes annotations."))))
  ;; usage rights, then the publisher's later save changing the page: the rights vouch for nothing shown,
  ;; so the change is for the record
  (let ((after (pal-save (pal-signed :rights t)
                         '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                                                       "/Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 9 0 R] "
                                                       ">>"))))))
    (multiple-value-bind (pdf accounts findings) (pal-read after)
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "A usage rights signature (object 8)"))
      (false (pal-finding findings :strong "") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :record
                         "after the usage rights signature in object 8, changes what the document shows"))))
  ;; a /ByteRange that stops short of where its save ends
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-signed :short 10))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "stops short"))))

;;; Hostile files

(define-test (palimpsest palimpsest-hostile-files)
  (let ((start (get-internal-real-time)))
    (dolist (octets (list
                     ;; a page tree in a circle, a /Prev naming its own section
                     (pal-save #() '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                     (2 . "<< /Type /Pages /Kids [2 0 R 3 0 R] /Count 1 >>")
                                     (3 . "<< /Type /Page /Parent 2 0 R /Contents 4 0 R >>")
                                     (4 :stream "" "q q q Q BT ET")))
                     (pal-octets "%PDF-1.4" 10 "1 0 obj << /Type /Catalog /Pages 1 0 R >> endobj" 10
                                 "xref" 10 "0 999999999" 10
                                 "0000000009 00000 n" 13 10 "trailer << /Root 1 0 R /Prev 20 >>" 10
                                 "startxref" 10 "20" 10 "%%EOF" 10)
                     ;; offsets out of range, a startxref pointing past the end
                     (pal-octets "%PDF-1.4" 10 "xref" 10 "0 2" 10 "0000000000 65535 f" 13 10 "9999999999 00000 n" 13 10
                                 "trailer << /Root 1 0 R /Size 2 >>" 10 "startxref" 10 "123456789" 10 "%%EOF" 10)
                     ;; an object stream claiming a billion objects, arrays nested deep, no %%EOF
                     (pal-octets "%PDF-1.5" 10 "1 0 obj << /Type /ObjStm /N 1000000000 /First 3 /Length 5 >> stream" 10
                                 "1 0 2" 10 "endstream endobj" 10
                                 "2 0 obj " (make-string 5000 :initial-element #\[) " endobj" 10)
                     ;; a content stream that unpacks without end
                     (pal-one-page "" :more (list (list 7 :stream "/Filter /FlateDecode" (pal-zlib-bomb 20000))))
                     ;; nothing of a PDF at all
                     (pal-octets "not a PDF" 0 0 0 255)
                     (pal-octets)))
      (let ((tuatara-user:*palimpsest-largest-stream* (* 1024 1024)))
        (multiple-value-bind (pdf accounts findings) (pal-read octets)
          (declare (ignore accounts))
          (true (plusp (length (palimpsest::pdf-revisions pdf))))
          (true (listp findings)))))
    (true (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))))

(defun pal-pages (sizes fonts)
  "A PDF of one page for each of SIZES (each a width) drawn with the font of
FONTS beside it (each a base font name)."
  (let ((count (length sizes)))
    (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                (cons 2 (format nil "<< /Type /Pages /Kids [~{~d 0 R~^ ~}] /Count ~d >>"
                                                (loop for k below count collect (+ 10 (* 2 k))) count)))
                          (loop for size in sizes for font in fonts for k from 0
                                collect (cons (+ 10 (* 2 k))
                                              (format nil
                                                      "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d 792] /Resources ~
                                                       << /Font << /F1 ~d 0 R >> >> >>"
                                                      size (+ 11 (* 2 k))))
                                collect (cons (+ 11 (* 2 k))
                                              (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /~a >>" font)))))))

(define-test (palimpsest palimpsest-pages-unlike)
  ;; one page of another size, one of another font: each named
  (let* ((pdf (palimpsest::read-pdf (pal-pages '(612 612 500 612) '("Helvetica" "Helvetica" "Helvetica" "Courier"))))
         (findings (palimpsest::check-pages pdf)))
    (true (pal-finding findings :look "Page 3 has a size (500 by 792) unlike every other page"))
    (true (pal-finding findings :look "Page 4 shares no font"))
    (false (pal-finding findings :look "Page 1 has"))
    (false (pal-finding findings :look "Page 3 shares")))
  ;; two pages each alone in its font: neither is named
  (let* ((pdf (palimpsest::read-pdf (pal-pages '(612 612 612 612) '("Helvetica" "Helvetica" "Times" "Courier")))))
    (false (pal-finding (palimpsest::check-pages pdf) :look "shares no font")))
  ;; very many pages, every one of its own size and font: compared at once
  (let* ((count 800)
         (pdf (palimpsest::read-pdf (pal-pages (loop for k below count collect (+ 100 k))
                                               (loop for k below count collect (format nil "Font~d" k)))))
         (start (get-internal-real-time)))
    (is = count (length (palimpsest::pages-of pdf (palimpsest::last-revision pdf))))
    (false (pal-finding (palimpsest::check-pages pdf) :look "unlike every other page"))
    (true (< (- (get-internal-real-time) start) (* 5 internal-time-units-per-second)))))

(define-test (palimpsest palimpsest-mac-roman)
  ;; MacRomanEncoding's 128 characters, the no-break space and the Apple logo in their places
  (let ((table (palimpsest::base-encoding "/MacRomanEncoding")))
    (is = 128 (length palimpsest::*mac-roman-high*))
    (is eql (code-char #xA0) (aref table #xCA))
    (is eql (code-char #xF8FF) (aref table #xF0))
    (is eql #\Ò (aref table #xF1))
    (is eql (code-char #x2C7) (aref table #xFF))))

(define-test (palimpsest palimpsest-operators-budget)
  ;; the marks of pages kept to be used again no more than *most-marks-kept* in all, the pages compared alike
  (let ((palimpsest::*most-marks-kept* 2))
    (multiple-value-bind (pdf accounts) (pal-read (pal-edited))
      (is equal '((:removed "Total: 100") (:added "Total: 900"))
          (palimpsest::page-change-text (first (palimpsest::account-changes (second accounts)))))
      (true (<= (getf (palimpsest::pdf-cache pdf) :marks-kept 0) 2))
      (true (<= (hash-table-count (getf (palimpsest::pdf-cache pdf) :marks)) 1))
      (false (find-if (lambda (n) (search "operators in all" n)) (palimpsest::pdf-notes pdf)))))
  ;; one budget of operators for every save's pages together: past it nothing more is read, and that is noted
  (let ((palimpsest::*most-operators-in-all* 12))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-edited))
      (declare (ignore accounts))
      (true (listp findings))
      (true (find-if (lambda (n) (search "operators in all" n)) (palimpsest::pdf-notes pdf)))
      (true (<= (getf (palimpsest::pdf-cache pdf) :operators-read 0) 12)))))

;;; Each file read shut in

(defun pal-reported (octets name &key shut-in (then #'identity) beside)
  "OCTETS written as NAME in a directory of the test's own and examined as M-x palimpsest examines it, shut in
with SHUT-IN, else in the tests' process: what THEN gives of the editor's ANALYSIS, called while the file is
there. BESIDE, each (NAME . OCTETS), are written beside it and given as its known-genuine files."
  (with-test-directory (dir)
    (flet ((file (name octets)
             (let ((path (namestring (ensure-directories-exist (merge-pathnames (format nil "evidence/~a" name) dir)))))
               (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
                 (write-sequence octets out))
               path)))
      (let* ((path (file name octets))
             (directory (tuatara.defaults::new-private-directory
                         (tuatara.defaults::private-cache-directory "palimpsest/")))
             (references (loop for (name . octets) in beside collect (file name octets)))
             (job (palimpsest::make-palimpsest-job nil path directory references))
             (palimpsest::*palimpsest-shut-in* shut-in))
        (palimpsest::examine job)
        (funcall then (palimpsest::palimpsest-job-analysis job))))))

(defun pal-jpeg-page ()
  "A one-page PDF whose page is one JPEG picture, a scanned page palimpsest turns into pixels in the sandbox."
  (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                      (cons 3 (concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 320 240] "
                                           "/Resources << /XObject << /Im1 7 0 R >> >> /Contents 4 0 R >>"))
                      (list 4 :stream "" "q 320 0 0 240 0 0 cm /Im1 Do Q")
                      (list 7 :stream (concatenate 'string "/Type /XObject /Subtype /Image /Width 320 /Height 240 "
                                                   "/ColorSpace /DeviceGray /BitsPerComponent 8 /Filter /DCTDecode")
                            (pal-sample "thumbnail-honest.jpg")))))

(define-test (palimpsest palimpsest-shut-in-same-report)
  ;; examined shut in or in the tests' process, a file gives the same report, revisions and fingerprint: a PDF
  ;; saved twice, a scanned page whose JPEG is turned into pixels, a JPEG picture file
  (dolist (case (list (list "invoice.pdf" (pal-edited)) (list "scan.pdf" (pal-jpeg-page))
                      (list "photo.jpg" (pal-sample "thumbnail-edited.jpg"))))
    (destructuring-bind (name octets) case
      (let ((here (pal-reported octets name)) (shut (pal-reported octets name :shut-in t)))
        (false (palimpsest::analysis-stopped shut) "~a: ~a" name (palimpsest::analysis-stopped shut))
        ;; but the line naming where the file is: each was in a test directory of its own, so the faces after it
        ;; are counted from its end
        (flet ((placeless (analysis)
                 (cl-ppcre:regex-replace "  File +[^\n]*" (first (palimpsest::analysis-shown analysis)) ""))
               (faces (analysis)
                 (destructuring-bind (text spans &rest more) (palimpsest::analysis-shown analysis)
                   (declare (ignore more))
                   (let ((end (nth-value 1 (cl-ppcre:scan "  File +[^\n]*" text))))
                     (loop for (from to face) in spans
                           collect (if (and end (>= from end)) (list :after (- from end) (- to end) face)
                                       (list from to face)))))))
          (is string= (placeless here) (placeless shut) "~a" name)
          (is equal (faces here) (faces shut) "~a" name))
        (is equal (mapcar #'third (palimpsest::analysis-files here)) (mapcar #'third (palimpsest::analysis-files shut))
            "~a" name)
        (is equal (palimpsest::analysis-fingerprint here) (palimpsest::analysis-fingerprint shut) "~a" name)
        (is equal (palimpsest::analysis-sha256 here) (palimpsest::analysis-sha256 shut) "~a" name)
        (is = (length (third (palimpsest::analysis-shown here))) (length (third (palimpsest::analysis-shown shut)))
            "~a" name))))
  ;; the scanned page was turned into pixels where it was read: its grain was measured, not passed over
  (let ((text (first (palimpsest::analysis-shown (pal-reported (pal-jpeg-page) "scan.pdf" :shut-in t)))))
    (false (search "could not be turned into pixels" text) "~a" text)))

(define-test (palimpsest palimpsest-shut-in-decoder-failing)
  ;; a scanned page whose JPEG the decoder cannot turn into pixels, where the file is read shut in: said in the
  ;; report as a picture not read, and why, never as nothing found
  (let* ((broken (pal-octets 255 216 255 192 0 11 8 0 240 1 64 1 1 17 0 (make-array 64 :initial-element 0)))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                     (cons 3 (concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox "
                                                          "[0 0 320 240] /Resources << /XObject << /Im1 7 0 R >> "
                                                          ">> /Contents 4 0 R >>"))
                                     (list 4 :stream "" "q 320 0 0 240 0 0 cm /Im1 Do Q")
                                     (list 7 :stream (concatenate 'string "/Type /XObject /Subtype /Image /Width 320 "
                                                                  "/Height 240 /ColorSpace /DeviceGray "
                                                                  "/BitsPerComponent 8 /Filter /DCTDecode")
                                           broken))))
         (text (first (palimpsest::analysis-shown (pal-reported octets "broken.pdf" :shut-in t)))))
    (pal-has text "The scanned picture on page 1 was not read (")
    (pal-has text "no copies, grain, words unlike the rest or JPEG blocks were looked for there"))
  ;; a decoder that runs out of time is said the same way, its words given
  (sb-int:encapsulate 'tuatara:decode-picture 'out-of-time
                      (lambda (function &rest arguments)
                        (declare (ignore function arguments))
                        (values nil (list (list ", the decoder was stopped after 10 seconds")))))
  (unwind-protect
       (let ((text (first (palimpsest::analysis-shown (pal-reported (pal-jpeg-page) "scan.pdf")))))
         (pal-has text "was not read (the decoder was stopped after 10 seconds)"))
    (sb-int:unencapsulate 'tuatara:decode-picture 'out-of-time)))

(define-test (palimpsest palimpsest-shut-in-out-of-memory)
  ;; a page that unpacks past the memory of the Tuatara reading it: that one stops, the report says so, and the
  ;; editor goes on
  (let ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                    '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                    '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R >>")
                                    (list 4 :stream "/Filter /FlateDecode" (pal-zlib-bomb 4000000))
                                    '(6 . "<< /Producer (Kea Writer) >>"))))
        (tuatara-user:*palimpsest-largest-stream* (* 4 1024 1024 1024))
        (tuatara-user:*palimpsest-most-decoded* (* 64 1024 1024 1024)))
    (sb-int:encapsulate 'palimpsest::shut-in-memory 'small (lambda (function) (declare (ignore function))
                                                              (expt 2 30)))
    (unwind-protect
         (let* ((analysis (pal-reported octets "bomb.pdf" :shut-in t))
                (text (first (palimpsest::analysis-shown analysis))))
           (pal-has (palimpsest::analysis-stopped analysis)
                "The check of bomb.pdf stopped: it needed more than 1,024 MB of memory")
           (pal-has text "Stopped")
           (pal-has text "it needed more than 1,024 MB of memory")
           (pal-has text "Not examined"))
      (sb-int:unencapsulate 'palimpsest::shut-in-memory 'small))
    ;; the editor is still here and examines the next file
    (true (palimpsest::analysis-files (pal-reported (pal-edited) "invoice.pdf" :shut-in t)))))

(define-test (palimpsest palimpsest-shut-in-object-and-copy)
  ;; an object shown and a copy drawn are read shut in too, as in the tests' process
  (flet ((object (shut-in)
           (pal-reported (pal-edited) "invoice.pdf"
                         :shut-in shut-in
                         :then (lambda (analysis)
                                 (let ((job (palimpsest::make-palimpsest-object-job nil analysis 1 4 nil nil))
                                       (palimpsest::*palimpsest-shut-in* shut-in))
                                   (palimpsest::object-work job)
                                   (palimpsest::palimpsest-object-job-text job))))))
    (let ((shut (object t)))
      (pal-has shut "Object 4 of invoice.pdf")
      (is string= (object nil) shut))))

(define-test (palimpsest palimpsest-shut-in-copy-drawn)
  ;; a copy with its objects drawn is made shut in and copied where it is wanted
  (with-test-directory (out)
    (let ((target (namestring (merge-pathnames "drawn.pdf" out))))
      (pal-reported (pal-edited) "invoice.pdf"
                    :shut-in t
                    :then (lambda (analysis)
                            (let ((job (palimpsest::make-palimpsest-draw-job nil analysis 1 target nil))
                                  (palimpsest::*palimpsest-shut-in* t))
                              (palimpsest::copy-work job)
                              (true (palimpsest::palimpsest-draw-job-done job)))))
      (let ((octets (tuatara.io:file-octets target)))
        (true (search (pal-octets "%PDF-") octets))
        (true (> (length octets) (length (pal-edited))))))))

;;; The revisions written, their logs, the report

(define-test (palimpsest palimpsest-sum-of-the-octets-examined)
  ;; the file changed on disk just after it is read: the sum in the report is of the octets examined
  (with-test-directory (dir)
    (let* ((octets (pal-edited))
           (path (namestring (ensure-directories-exist (merge-pathnames "evidence/invoice.pdf" dir))))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/")))
           (job (palimpsest::make-palimpsest-job nil path directory)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
      (sb-int:encapsulate 'tuatara.io:file-octets 'palimpsest-file-changed-after
                          (lambda (read &rest arguments)
                            (prog1 (apply read arguments)
                              (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)
                                                        :if-exists :append)
                                (write-byte 10 out)))))
      (unwind-protect (palimpsest::examine job)
        (sb-int:unencapsulate 'tuatara.io:file-octets 'palimpsest-file-changed-after))
      (is string= (tuatara:octets-sha256 octets)
          (palimpsest::analysis-sha256 (palimpsest::palimpsest-job-analysis job))))))

(define-test (palimpsest palimpsest-one-job-on-a-pdf-at-a-time)
  ;; while a job reads a report's PDF, no second job starts on it and no object is shown from it
  (let* ((analysis (palimpsest::make-analysis :name "test.pdf" :pdf (palimpsest::read-pdf (pal-edited))))
         (running (palimpsest::save-job nil analysis "/nowhere")))
    (setf (palimpsest::analysis-job analysis) running)
    (false (palimpsest::start-analysis-job analysis (palimpsest::save-job nil analysis "/nowhere")
                                           (lambda (job) (declare (ignore job)) (error "started"))
                                           "tuatara palimpsest test"))
    (false (palimpsest::show-object analysis 1 1))
    (is eq running (palimpsest::analysis-job analysis))))

(define-test (palimpsest palimpsest-revisions-and-logs)
  (with-test-directory (dir)
    (let* ((octets (pal-edited))
           (path (namestring (ensure-directories-exist (merge-pathnames "evidence/invoice.pdf" dir))))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
      (let ((stamp (tuatara.io:path-stamp path))
            (job (palimpsest::make-palimpsest-job nil path directory)))
        (palimpsest::examine job)
        (let* ((analysis (palimpsest::palimpsest-job-analysis job))
               (files (palimpsest::analysis-files analysis))
               ;; the same file as the work shut in examines it, its PDF kept
               (full (palimpsest::examine-file path (namestring (ensure-directories-exist
                                                                 (merge-pathnames "full/" dir)))))
               (pdf (palimpsest::analysis-pdf full))
               (second-revision (aref (palimpsest::pdf-revisions pdf) 1)))
          (is = 2 (length files))
          ;; each revision its octets up to its %%EOF line, read only, in a directory for you alone
          (destructuring-bind (file log sum) (first files)
            (is equalp (subseq octets 0 (palimpsest::revision-end (aref (palimpsest::pdf-revisions pdf) 0)))
                (tuatara.io:file-octets file))
            (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp file))))
            (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp directory))))
            (true (search "/palimpsest/" file))
            (true sum)
            (pal-has (uiop:read-file-string log) "revision-1.pdf SHA-256: "))
          (let ((log (uiop:read-file-string (second (second files)))))
            ;; how the revision was found, with its offsets in hex and decimal
            (pal-has log
                     (format nil "Its %%EOF is at ~a"
                             (palimpsest::offset-words (palimpsest::revision-eof second-revision))))
            (pal-has log
                     (format nil "Its trailer's /Prev is ~d"
                             (palimpsest::revision-xref-at (aref (palimpsest::pdf-revisions pdf) 0))))
            (pal-has log "the cross-reference section of revision 1 (the original)")
            (pal-has log
                     (format nil "This revision is the source file's octets from 0x00000000 to ~a"
                             (palimpsest::hex-offset (1- (length octets)))))
            ;; each sign with its rule, the octets it rests on and the steps to see it
            (pal-has log "For the record: /Producer names iLovePDF")
            (pal-has log "2F 50 72 6F 64 75 63 65 72")
            (pal-has log "text: /Producer (iLovePDF)")
            (pal-has log "Steps to see it by hand:")
            ;; what changed, both places of each object, and the text found in the decoded stream
            (pal-has log (format nil "object 4 now at ~a, before at ~a"
                                 (palimpsest::offset-words
                                  (palimpsest::pdf-indirect-get (palimpsest::revision-object pdf second-revision 4)
                                                                :at))
                                 (palimpsest::offset-words
                                  (palimpsest::pdf-indirect-get
                                   (palimpsest::revision-object pdf (aref (palimpsest::pdf-revisions pdf) 0) 4) :at))))
            (pal-has log "text - Total: 100")
            (pal-has log "text + Total: 900")
            (pal-has log "(Total: 900) Tj")
            ;; the same file, the same log but its date line
            (let ((again (palimpsest::revision-log full second-revision (third (second files)))))
              (flet ((dateless (text) (cl-ppcre:regex-replace "Written: .*" text "")))
                (is string= (dateless log) (dateless again)))))
          ;; the report
          (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*Palimpsest test*"))))
            (tuatara.modes:set-major-mode buffer 'palimpsest::palimpsest-mode)
            (palimpsest::report-render buffer analysis)
            (let ((text (tuatara.buffer:buffer-string buffer)))
              (pal-has text "Palimpsest: invoice.pdf")
              (pal-has text "Signs worth a look, none of them strong.")
              (pal-has text "** Revision 2 (the current file): 0x")
              (pal-has text "revision-2.pdf  its log revision-2.log")
              (pal-has text "+ Total: 900"))
            (true (tuatara.buffer:buffer-local buffer :buttons))
            (tuatara.buffer:kill-buffer buffer))
          ;; an object shown: its place, its dictionary, its stream as text, or in hex in a buffer of its own
          (let ((text (palimpsest::object-text full second-revision 4)))
            (pal-has text "Object 4 0 begins at")
            (pal-has text "(Total: 900) Tj"))
          (pal-has (palimpsest::object-text full second-revision 4 :hex t)
                   "In hex: *Palimpsest: invoice.pdf object 4 stream, revision 2*")
          (is equalp (pal-octets "BT /F1") (subseq (palimpsest::object-stream-shown full second-revision 4) 0 6))
          ;; saved where asked, each copy's sum its original's; never beside the evidence, in the cache, or over a file
          (pal-has (palimpsest::save-refusal analysis (namestring (merge-pathnames "evidence/" dir)))
                   "the directory of the PDF examined")
          ;; nor beside it through a link to its directory
          (let ((link (namestring (merge-pathnames "evidence-link" dir))))
            (tuatara.io:make-symbolic-link (string-right-trim "/" (namestring (merge-pathnames "evidence/" dir))) link)
            (pal-has (palimpsest::save-refusal analysis (concatenate 'string link "/"))
                     "the directory of the PDF examined")
            (tuatara.io:remove-file link))
          (pal-has (palimpsest::save-refusal analysis directory) "within palimpsest's own directory in the cache")
          (let* ((kept (namestring (merge-pathnames "kept/" dir)))
                 (saved (palimpsest::save-copies analysis kept)))
            (is equal
                '("revision-1.pdf" "revision-1.log" "revision-2.pdf" "revision-2.log" "invoice-revision-report.txt"
                  "invoice-fingerprint.txt") (mapcar #'first saved))
            (loop for (name sum) in saved
                  for source
                  in (list (first (first files)) (second (first files)) (first (second files)) (second (second files)))
                  do (is string= (tuatara:file-sha256 source) sum)
                     (is string= sum (tuatara:file-sha256 (tuatara.io:path-join kept name))))
            (is equalp (tuatara.io:file-octets (first (second files)))
                (tuatara.io:file-octets (tuatara.io:path-join kept "revision-2.pdf")))
            (let ((report (uiop:read-file-string (tuatara.io:path-join kept "invoice-revision-report.txt"))))
              (pal-has report "Palimpsest: invoice.pdf")
              (pal-has report (format nil "Saved to ~a" kept))
              (pal-has report (format nil "~a  ~arevision-2.log" (second (fourth saved)) kept)))
            (is string= kept (palimpsest::analysis-saved analysis))
            (pal-has (palimpsest::save-refusal analysis kept)
                     (format nil "revision-1.pdf, revision-1.log, revision-2.pdf, revision-2.log, ~
                                  invoice-revision-report.txt, invoice-fingerprint.txt are there already")))
          ;; the PDF itself never touched
          (is equalp octets (tuatara.io:file-octets path))
          (is = (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime (tuatara.io:path-stamp path))))
        (tuatara.io:delete-tree directory)))))

(define-test (palimpsest palimpsest-evidence-not-remembered)
  ;; the PDF examined and the directory of cases grouped are evidence: neither becomes the path
  ;; the next question starts from
  (with-test-directory (dir)
    (let ((pdf (namestring (merge-pathnames "invoice.pdf" dir)))
          (cases (namestring (merge-pathnames "cases/" dir)))
          (work (namestring (merge-pathnames "work/notes.txt" dir))))
      (ensure-directories-exist cases)
      (with-open-file (out pdf :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) out))
      (tuatara.defaults::note-chosen-path work)
      (flet ((ask (command answer)
               (let ((f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 40)))
                 (tuatara.frontend.fake:fake-queue-keys
                  f (format nil "M-x ~a RET C-a C-k ~a RET" (rimu-typed command) (rimu-typed answer)))
                 (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                               :buffer (or (tuatara.buffer:find-buffer "*evidence-test*")
                                                           (tuatara.buffer:make-buffer "*evidence-test*" :text "")))
                 (loop repeat 1500 while (some (lambda (job) (or (palimpsest::palimpsest-job-p job)
                                                                 (palimpsest::palimpsest-cases-job-p job)))
                                               tuatara.runtime:*jobs*)
                       do (sleep 0.02)))))
        (unwind-protect
             (progn
               (ask "palimpsest" pdf)
               (is string= work (first (tuatara.defaults::chosen-paths)))
               (ask "palimpsest-group-cases" cases)
               (is string= work (first (tuatara.defaults::chosen-paths))))
          (dolist (b (copy-list tuatara.buffer:*buffers*))
            (when (or (string= (tuatara.buffer:buffer-name b) "*evidence-test*")
                      (uiop:string-prefix-p "*Palimpsest" (tuatara.buffer:buffer-name b)))
              (tuatara.buffer:kill-buffer b))))))))

(define-test (palimpsest palimpsest-command)
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "invoice.pdf" dir)))
          (f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 40))
          (start (get-internal-real-time)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) out))
      (let ((scratch (tuatara.buffer:find-buffer "*scratch*"))) (when scratch (tuatara.buffer:kill-buffer scratch)))
      (tuatara.buffer:make-buffer "*scratch*" :text "")
      (tuatara.frontend.fake:fake-queue-keys f
                                             (format nil "M-x ~a RET C-a C-k ~a RET"
                                                     (rimu-typed "palimpsest") (rimu-typed path)))
      (labels ((check ()
                 (when (and (some #'palimpsest::palimpsest-job-p tuatara.runtime:*jobs*)
                            (< (- (get-internal-real-time) start) (* 30 internal-time-units-per-second)))
                   (sleep 0.02)
                   (tuatara.frontend.fake:fake-queue-action f #'check))))
        (tuatara.frontend.fake:fake-queue-action f #'check))
      (let* ((editor (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults
                                                   :buffer (tuatara.buffer:find-buffer "*scratch*")))
             (buffer (session-buffer editor)))
        (is string= "*Palimpsest: invoice.pdf*" (tuatara.buffer:buffer-name buffer))
        (true (tuatara.buffer:buffer-local buffer :read-only))
        (pal-has (session-text editor) "Page 1 was changed in revision 2")
        (let ((directory (tuatara:buffer-private-directory buffer)))
          (true (probe-file (tuatara.io:path-join directory "revision-2.log")))
          ;; closing it with nothing saved asks first: n keeps it, y closes it and deletes what it wrote
          (flet ((close-answering (answer)
                   (let ((f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 40)))
                     (tuatara.frontend.fake:fake-queue-keys f
                                                            (format nil "M-x ~a RET ~a RET"
                                                                    (rimu-typed "kill-buffer") answer))
                     (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer buffer)
                     f)))
            (close-answering "n")
            (true (tuatara.buffer:live-buffer-p buffer))
            (true (probe-file directory))
            (close-answering "y")
            (false (tuatara.buffer:live-buffer-p buffer)))
          (loop repeat 200 while (probe-file directory) do (sleep 0.02))
          (false (probe-file directory))))))
  (true (fboundp 'palimpsest::palimpsest))
  (is eq 'palimpsest::palimpsest-show-object
      (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'palimpsest::palimpsest-mode)
                                  (tuatara.frontend:parse-key-sequence "C-c p o")))
  (is eq 'palimpsest::palimpsest-save
      (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'palimpsest::palimpsest-mode)
                                  (tuatara.frontend:parse-key-sequence "C-c p s"))))

(define-test (palimpsest palimpsest-log-closes-with-q)
  ;; a revision's log, opened from the report, reads as the report does: q closes it
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "revision-1.log" dir))))
      (alexandria:write-string-into-file "Palimpsest log" path)
      (let ((buffer nil) (tuatara.command:*editor* (nth-value 1 (run-keys "" :text ""))))
        (tuatara.defaults::visit-path path :show (lambda (b) (setf buffer b) (palimpsest::show-log b)))
        (unwind-protect
             (is eq 'tuatara.defaults::quit-window
                 (tuatara.command:lookup-key (tuatara.buffer:buffer-local buffer :override-keymap)
                                             (tuatara.frontend:parse-key-sequence "q")))
          (when buffer (tuatara.buffer:kill-buffer buffer)))))))

(define-test (palimpsest palimpsest-revision-written)
  ;; a revision written as a PDF of its own, its log beside it, the copy's sum checked; that PDF, examined
  ;; in turn, holds the saves up to it; nothing written over, the evidence unchanged; the revision meant
  ;; by a revision opened from the report
  (with-test-directory (dir)
    (let* ((octets (pal-edited))
           (path (namestring (ensure-directories-exist (merge-pathnames "evidence/invoice.pdf" dir))))
           (out (namestring (ensure-directories-exist (merge-pathnames "to-show/" dir))))
           (target (concatenate 'string out "invoice-revision-1.pdf"))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/")))
           (again-directory (tuatara.defaults::new-private-directory
                             (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets o))
      (unwind-protect
          (let ((job (palimpsest::make-palimpsest-job nil path directory)))
            (palimpsest::examine job)
            (let* ((analysis (palimpsest::palimpsest-job-analysis job))
                   (first-end (palimpsest::revision-end
                               (aref (palimpsest::pdf-revisions (palimpsest::read-pdf octets)) 0))))
              (true (palimpsest::write-revision analysis 1 target))
              ;; the report alone, under the name it is offered
              (is string= "invoice-revision-report.txt" (palimpsest::report-file-name analysis))
              (let ((report (concatenate 'string out "invoice-revision-report.txt")))
                (true (palimpsest::write-report analysis report))
                (pal-has (uiop:read-file-string report) "* Summary"))
              (is equalp (subseq octets 0 first-end) (tuatara.io:file-octets target))
              (pal-has (uiop:read-file-string (concatenate 'string out "invoice-revision-1.log"))
                       "Palimpsest log of revision 1")
              (pal-has (palimpsest::target-refusal analysis target) "there already")
              ;; never beside the PDF examined, the PDF itself untouched
              (pal-has (palimpsest::target-refusal analysis
                                                   (namestring (merge-pathnames "evidence/invoice-revision-1.pdf" dir)))
                       "the directory of the PDF examined")
              (is equalp octets (tuatara.io:file-octets path))
              ;; the version written holds one save: nothing before it
              (let ((again (palimpsest::make-palimpsest-job nil target again-directory)))
                (palimpsest::examine again)
                (is = 1 (length (palimpsest::analysis-files (palimpsest::palimpsest-job-analysis again))))
                (false (palimpsest::revision-files (palimpsest::palimpsest-job-analysis again) 2)))))
        ;; its private directories go with it
        (tuatara.io:delete-tree directory)
        (tuatara.io:delete-tree again-directory)))))

(define-test (palimpsest palimpsest-revision-meant)
  ;; in a revision opened from the report, the revision meant is that one
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "invoice.pdf" dir)))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/")))
           (tuatara.command:*editor* (nth-value 1 (run-keys "" :text ""))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) o))
      (let ((job (palimpsest::make-palimpsest-job nil path directory))
            (report (tuatara.buffer:make-buffer "*Palimpsest: invoice.pdf*")))
        (palimpsest::examine job)
        (setf (tuatara.buffer:buffer-local report 'palimpsest::analysis) (palimpsest::palimpsest-job-analysis job))
        (let ((second (first (second (palimpsest::analysis-files (palimpsest::palimpsest-job-analysis job)))))
              (shown nil))
          (unwind-protect
               (progn
                 (tuatara.defaults::visit-path second :show (lambda (b) (setf shown b) (tuatara.command:show-buffer b)))
                 (let ((tuatara.command:*current-buffer* shown))
                   (multiple-value-bind (found number) (palimpsest::revision-meant)
                     (is eq report found)
                     (is = 2 number))))
            (when shown (tuatara.buffer:kill-buffer shown))
            (tuatara.buffer:kill-buffer report)
            (tuatara.io:delete-tree directory))))))
  ;; in the report, the revision whose part point is in, told by the report's own
  ;; headings: a PDF that writes one into what it says of itself does not move it
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "invoice.pdf" dir)))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/")))
           (tuatara.command:*editor* (nth-value 1 (run-keys "" :text ""))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (pal-save (pal-original)
                                  (list (cons 6 (format nil "<< /Producer (Kea~%** Revision 1: 0x0 to 0x1, 1 ~
                                                             octets~%) >>"))))
                        o))
      (let ((job (palimpsest::make-palimpsest-job nil path directory))
            (report (tuatara.buffer:make-buffer "*Palimpsest: invoice.pdf*")))
        (unwind-protect
             (progn
               (palimpsest::examine job)
               (let ((analysis (palimpsest::palimpsest-job-analysis job)))
                 (setf (tuatara.buffer:buffer-local report 'palimpsest::analysis) analysis)
                 (destructuring-bind (text spans buttons) (palimpsest::analysis-shown analysis)
                   (tuatara:set-made-text report text :spans spans :buttons buttons))
                 (let ((tuatara.command:*current-buffer* report))
                   ;; at the end, in the latest revision's part
                   (setf (tuatara.buffer:buffer-point report) (tuatara.buffer:buffer-size report))
                   (is = 2 (nth-value 1 (palimpsest::revision-meant)))
                   ;; in the summary, in none
                   (setf (tuatara.buffer:buffer-point report) 10)
                   (false (nth-value 1 (palimpsest::revision-meant))))))
          (tuatara.buffer:kill-buffer report)
          (tuatara.io:delete-tree directory))))))

;;; Bounds, speed and the smaller cases

(defun pal-page-of (page-entries content &key (more '()))
  "A one-page PDF whose page has PAGE-ENTRIES and draws CONTENT (object 4), MORE objects beside."
  (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                              '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                              (cons 3 (format nil "<< /Type /Page /Parent 2 0 R ~a >>" page-entries))
                              (list 4 :stream "" content)
                              '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"))
                        more)))

(defun pal-first-page (pdf)
  "The latest revision of PDF and its first page, as two values."
  (let ((revision (palimpsest::last-revision pdf)))
    (values revision (first (palimpsest::pages-of pdf revision)))))

(defun pal-consed (function)
  "The octets consed while FUNCTION is called."
  (let ((before (sb-ext:get-bytes-consed)))
    (funcall function)
    (- (sb-ext:get-bytes-consed) before)))

(define-test (palimpsest palimpsest-content-streams-joined-within-bound)
  ;; a page naming one stream that unpacks to 1 MB three hundred times: joined only as far as one stream may decode to,
  ;; and said
  (let ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                    '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                    (cons 3
                                          (format nil
                                                  "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents ~
                                                   [~{~a~^ ~}] >>"
                                                    (loop repeat 300 collect "7 0 R")))
                                    (list 7 :stream "/Filter /FlateDecode" (pal-zlib-bomb 4000)))))
        (tuatara-user:*palimpsest-largest-stream* (* 2 1024 1024)))
    (let ((pdf (palimpsest::read-pdf octets)) (problems nil))
      (multiple-value-bind (revision page) (pal-first-page pdf)
        (true (< (pal-consed (lambda () (setf problems (nth-value 1 (palimpsest::page-marks pdf revision page)))))
                 (* 32 1024 1024)))
        (true (find-if (lambda (p) (search "content streams together decode to more than" p)) problems))))))

(define-test (palimpsest palimpsest-decoded-in-all-bounded)
  ;; a form unpacking to 1 MB drawn a hundred times: what is decoded and read in all held to *palimpsest-most-decoded*,
  ;; and noted
  (let ((octets (pal-page-of "/MediaBox [0 0 612 792] /Resources << /XObject << /X1 7 0 R >> >> /Contents 4 0 R"
                             (format nil "~{~a~^ ~}" (loop repeat 100 collect "/X1 Do"))
                             :more (list (list 7
                                               :stream (format nil "/Type /XObject /Subtype /Form /BBox [0 0 10 10] ~
                                                                    /Filter /FlateDecode") (pal-zlib-bomb 4000)))))
        (tuatara-user:*palimpsest-most-decoded* (* 8 1024 1024)))
    (let ((pdf (palimpsest::read-pdf octets)))
      (multiple-value-bind (revision page) (pal-first-page pdf)
        (palimpsest::page-marks pdf revision page))
      (true (find-if (lambda (n) (search "(*palimpsest-most-decoded*)" n)) (palimpsest::pdf-notes pdf)))
      (true (<= (getf (palimpsest::pdf-cache pdf) :decoded-read 0) (* 10 1024 1024))))))

(define-test (palimpsest palimpsest-failed-stream-tried-once)
  ;; a stream unpacking past *palimpsest-largest-stream* fails once: counted against the whole, not tried again
  (let* ((tuatara-user:*palimpsest-largest-stream* 100000)
         (pdf (palimpsest::read-pdf (pal-save (pal-original) (list (list 7 :stream "/Filter /FlateDecode"
                                                                           (pal-zlib-bomb 4000))))))
         (object (palimpsest::revision-object pdf (palimpsest::last-revision pdf) 7))
         (before (getf (palimpsest::pdf-cache pdf) :decoded-read 0)))
    (dotimes (k 2)
      (multiple-value-bind (data why) (palimpsest::stream-data pdf object)
        (false data)
        (true why)))
    (is = (+ before 100000) (getf (palimpsest::pdf-cache pdf) :decoded-read 0))
    (true (gethash object (palimpsest::pdf-decoded pdf)))))

(define-test (palimpsest palimpsest-objects-scanned-in-linear-time)
  ;; finding the objects of the body takes time in proportion to their number
  (flet ((seconds-per-object (count)
           (let* ((octets (pal-octets "%PDF-1.4" 10
                                      (with-output-to-string (s)
                                        (dotimes (k count) (format s "~d 0 obj null endobj~%" (1+ k))))))
                  (pdf (palimpsest::make-pdf :octets octets))
                  (start (get-internal-real-time)))
             (palimpsest::scan-body pdf)
             (is = count (length (palimpsest::pdf-found pdf)))
             (/ (max 1 (- (get-internal-real-time) start)) count))))
    (seconds-per-object 1000)
    (let ((small (seconds-per-object 20000)) (large (seconds-per-object 80000)))
      (true (< large (* 3 small)) "~,2f times as long for each object among 80,000 as among 20,000" (/ large small)))))

(define-test (palimpsest palimpsest-objects-compared-in-place)
  ;; a save writing a 2 MB stream again unchanged: told the same without copying it
  (let* ((data (make-string (* 2 1024 1024) :initial-element #\Space))
         (original (pal-save #() (append *pal-original-objects* (list (list 7 :stream "" data)))))
         (pdf (palimpsest::read-pdf (pal-save original (list (list 7 :stream "" data)))))
         (second (aref (palimpsest::pdf-revisions pdf) 1)))
    (true (< (pal-consed (lambda () (palimpsest::note-changes pdf second))) (* 1024 1024)))
    (is equal '(7) (palimpsest::revision-same second))))

(define-test (palimpsest palimpsest-xmp-read-once)
  ;; the metadata two saves share is read once
  (let* ((xmp (format nil "<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF ~
                           xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description ~
                           xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmp:CreatorTool='Kea ~
                           Writer'/></rdf:RDF></x:xmpmeta>"))
         (original (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                       '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                       (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))
         (pdf (palimpsest::read-pdf (pal-save original '((6 . "<< /Producer (Kea Writer) >>")))))
         (first-tree (nth-value 1 (palimpsest::xmp-of pdf (aref (palimpsest::pdf-revisions pdf) 0)))))
    (true first-tree)
    (is eq first-tree (nth-value 1 (palimpsest::xmp-of pdf (aref (palimpsest::pdf-revisions pdf) 1))))))

(defun pal-pictures (count)
  "A one-page PDF placing COUNT pictures alike, and one more of another bit depth and resolution, object 10."
  (pal-page-of (format nil
                       "/MediaBox [0 0 612 792] /Resources << /XObject << ~{/Im~d ~:*~d 0 R ~} >> >> /Contents 4 0 R"
                       (loop for k from 10 to (+ 10 count) collect k))
               (format nil "~{q 72 0 0 72 0 0 cm /Im~d Do Q ~}" (loop for k from 10 to (+ 10 count) collect k))
               :more (loop for k from 10 to (+ 10 count)
                           collect (list k :stream (format nil
                                                           "/Type /XObject /Subtype /Image /Width ~d /Height 100 ~
                                                            /ColorSpace /DeviceGray /BitsPerComponent ~d"
                                                           (if (= k 10) 1000 100) (if (= k 10) 1 8))
                                         "x"))))

(defun pal-thread-cpu-time ()
  "The processor time this thread has used, in nanoseconds."
  (multiple-value-bind (seconds nanoseconds) (sb-unix:clock-gettime sb-unix:clock-thread-cputime-id)
    (+ (* seconds 1000000000) nanoseconds)))

(define-test (palimpsest palimpsest-pictures-compared-in-linear-time)
  ;; the one picture unlike the others named, and only it
  (let* ((pdf (palimpsest::read-pdf (pal-pictures 40)))
         (findings (palimpsest::check-images pdf (palimpsest::last-revision pdf))))
    (true (pal-finding findings :record "(object 10) on page 1 has a bit depth (1), a resolution of about 1000")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (is = 1 (length findings)))
  ;; the time for each picture the same however many there are
  (flet ((per-picture (count)
           (let* ((pdf (palimpsest::read-pdf (pal-pictures count)))
                  (revision (palimpsest::last-revision pdf)))
             (palimpsest::check-images pdf revision)
             ;; the processor time of this thread, the least of three runs: other programs
             ;; sharing the machine do not lengthen it
             (/ (max 1 (loop repeat 3
                             minimize (let ((start (pal-thread-cpu-time)))
                                        (loop repeat 20 do (palimpsest::check-images pdf revision))
                                        (- (pal-thread-cpu-time) start))))
                count))))
    (let ((small (per-picture 400)) (large (per-picture 1600)))
      (true (< large (* 2 small)) "~,2f times as long for each picture among 1,600 as among 400" (/ large small)))))

(defun pal-peer-glyphs (count &key (widths 1) (heights 1) (pages 1) (inks 100) (seed 99))
  "COUNT made-up characters for GLYPH-PEERS, of WIDTHS widths from 5, HEIGHTS
heights from 9 and PAGES pages, and ink from 1000 to 1000 + INKS, as a
number from SEED sets them; only their boxes, pages and ink."
  (loop repeat count
        collect (flet ((next (n) (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648)) (mod (ash seed -16) n)))
                  (let ((w (+ 5 (next widths))) (h (+ 9 (next heights))))
                    (palimpsest::make-glyph :x0 10 :y0 10 :x1 (+ 10 w) :y1 (+ 10 h) :page (next pages)
                                            :ink (+ 1000 (next inks)))))))

(define-test (palimpsest palimpsest-peers-nearest-in-ink)
  ;; each character's peers: of its width within 3, its height within 1, its page within 2, ink within
  ;; 12 %, never itself; the nearest in ink first, of as near ink the wider, the taller, the later page,
  ;; then the first given; 300 at most
  (let* ((glyphs (pal-peer-glyphs 3000 :widths 10 :heights 4 :pages 6 :inks 300))
         (peers (palimpsest::glyph-peers glyphs))
         (longest 0) (unlike 0))
    (dolist (g glyphs)
      (flet ((w (g) (- (palimpsest::glyph-x1 g) (palimpsest::glyph-x0 g)))
             (h (g) (- (palimpsest::glyph-y1 g) (palimpsest::glyph-y0 g))))
        (let* ((ink (palimpsest::glyph-ink g))
               (near (loop for p in glyphs
                           when (and (not (eq p g)) (<= (abs (- (w p) (w g))) 3) (<= (abs (- (h p) (h g))) 1)
                                     (<= (abs (- (palimpsest::glyph-page p) (palimpsest::glyph-page g))) 2)
                                     (<= (abs (- (palimpsest::glyph-ink p) ink)) (* 0.12 ink)))
                             collect p))
               (order (stable-sort near (lambda (a b)
                                          (let ((da (abs (- (palimpsest::glyph-ink a) ink)))
                                                (db (abs (- (palimpsest::glyph-ink b) ink))))
                                            (cond ((/= da db) (< da db))
                                                  ((/= (w a) (w b)) (> (w a) (w b)))
                                                  ((/= (h a) (h b)) (> (h a) (h b)))
                                                  (t (> (palimpsest::glyph-page a) (palimpsest::glyph-page b))))))))
               (found (funcall peers g)))
          (setf longest (max longest (length found)))
          (unless (and (= (length found) (min 300 (length order))) (every #'eq found order))
            (incf unlike)))))
    (is = 0 unlike "~d characters' peers unlike those expected" unlike)
    (is = 300 longest))
  ;; no characters, none beside any
  (is = 0 (length (funcall (palimpsest::glyph-peers '()) (first (pal-peer-glyphs 1))))))

(define-test (palimpsest palimpsest-peers-found-in-linear-time)
  ;; finding a character's peers costs the same, in time and in garbage, however many characters
  ;; of its size there are: it is not sorting them all again
  (flet ((per-character (count)
           (let* ((glyphs (pal-peer-glyphs count))
                  (peers (palimpsest::glyph-peers glyphs)))
             (funcall peers (first glyphs))
             (let ((start (get-internal-real-time)) (before (sb-ext:get-bytes-consed)))
               (dolist (g glyphs) (funcall peers g))
               (values (/ (max 1 (- (get-internal-real-time) start)) count)
                       (/ (- (sb-ext:get-bytes-consed) before) count))))))
    (multiple-value-bind (small small-octets) (per-character 500)
      (multiple-value-bind (large large-octets) (per-character 4000)
        (true (< large (* 2 small)) "~,2f times as long for each character among 4,000 as among 500" (/ large small))
        (true (<= large-octets (max 64 (* 3/2 small-octets)))
              "~,1f octets of garbage for each character among 4,000, ~,1f among 500"
              large-octets small-octets)))))

(defun pal-rows (count)
  "A one-page PDF of COUNT rows, each a line of text and a black box beside it, apart."
  (pal-page-of (format nil
                       "/MediaBox [0 0 612 ~d] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R"
                       (* 20 (1+ count)))
               (with-output-to-string (s)
                 (dotimes (k count)
                   (let ((y (* 20 (1+ k))))
                     (format s "BT /F1 10 Tf 72 ~d Td (Row ~d) Tj ET 0 0 0 rg 400 ~d 50 8 re f~%" y k (- y 2)))))))

(define-test (palimpsest palimpsest-page-checked-in-linear-time)
  ;; text beside a box is not covered by it; the work for each row the same however many there are
  (flet ((per-row (count)
           (let ((pdf (palimpsest::read-pdf (pal-rows count))))
             (multiple-value-bind (revision page) (pal-first-page pdf)
               (false (palimpsest::check-page-content pdf revision page))
               (/ (pal-consed (lambda () (palimpsest::check-page-content pdf revision page))) count)))))
    (let ((small (per-row 400)) (large (per-row 1600)))
      (true (< large (* 2 small)) "~,2f times the octets for each row among 1,600 as among 400" (/ large small))))
  ;; a box over text still covers it, white text over a dark box is still seen
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-page-of "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R"
                             (format nil "BT /F1 12 Tf 72 700 Td (Kea 7) Tj ET 1 1 1 rg 70 695 100 20 re f 0 0 1 rg 70 ~
                                          495 100 20 re f 1 1 1 rg BT /F1 12 Tf 72 500 Td (Seen) Tj ET")))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape"))
    (false (pal-finding findings nil "white text on a white page"))))

(define-test (palimpsest palimpsest-large-characters-tested-in-bounded-time)
  ;; a character 2,000 points high under a white box takes no longer to find hidden than one 200 high
  (flet ((seconds (size)
           (let ((pdf (palimpsest::read-pdf
                       (pal-one-page (format nil "BT /F1 ~d Tf 0 0 Td (8) Tj ET 1 1 1 rg -5000 -5000 10000 10000 re f"
                                             size)))))
             (multiple-value-bind (revision page) (pal-first-page pdf)
               (let ((start (get-internal-run-time)))
                 (true (palimpsest::check-page-content pdf revision page))
                 (max 1 (- (get-internal-run-time) start)))))))
    (seconds 200)
    (let ((small (seconds 200)) (large (seconds 2000)))
      (true (< large (+ (* 3 small) (floor internal-time-units-per-second 20)))
            "~,2f times as long for a character 2,000 points high as for one 200 high" (/ large small)))))

(define-test (palimpsest palimpsest-file-facts-read-once)
  ;; twenty white boxes over text on a page: what the file says of its makers is read once, not per box
  (let ((pdf (palimpsest::read-pdf
              (pal-one-page (format nil
                                    "~{BT /F1 12 Tf 72 ~d Td (Total ~:*~d) Tj ET 1 1 1 rg 70 ~:*~d 100 14 re f 0 g ~}"
                                    (loop for k below 20 collect (- 700 (* 20 k)))))))
        (calls 0))
    (sb-int:encapsulate 'palimpsest::file-comments 'palimpsest-count-reads
                        (lambda (read &rest arguments) (incf calls) (apply read arguments)))
    (unwind-protect
         (multiple-value-bind (revision page) (pal-first-page pdf)
           (true (palimpsest::check-page-content pdf revision page)))
      (sb-int:unencapsulate 'palimpsest::file-comments 'palimpsest-count-reads))
    (true (<= calls 1) "file comments read ~d times" calls)))

(define-test (palimpsest palimpsest-resources-looked-up-once)
  ;; a page naming a graphics state of its resources thousands of times: the time for each naming the same
  ;; whether the resources hold 40 states or 4,000
  (flet ((per-naming (states)
           (let* ((names (format nil "~{/G~d << /ca 1 >> ~}" (loop for k from (1- states) downto 0 collect k)))
                  (pdf (palimpsest::read-pdf (pal-page-of (format nil
                                                                  "/MediaBox [0 0 612 792] /Resources << /ExtGState << ~
                                                                   ~a >> >> /Contents 4 0 R" names)
                                                          (format nil "~{~a~}" (loop repeat 20000 collect "/G0 gs ")))))
                  (start (get-internal-run-time)))
             (multiple-value-bind (revision page) (pal-first-page pdf)
               (dotimes (k 3) (palimpsest::page-marks pdf revision page)))
             (/ (max 1 (- (get-internal-run-time) start)) 60000))))
    (per-naming 40)
    (let ((small (per-naming 40)) (large (per-naming 4000)))
      (true (< large (* 3 small))
            "~,1f times the time for each naming among 4,000 states as among 40" (/ large small)))))

(define-test (palimpsest palimpsest-odd-structures-noted)
  ;; a cross-reference stream whose /W is not an array: noted, the file still examined
  (let* ((head (pal-octets "%PDF-1.5" 10 "1 0 obj << /Type /Catalog >> endobj" 10))
         (at (length head))
         (octets (pal-octets head "2 0 obj << /Type /XRef /W 5 /Size 3 /Root 1 0 R /Length 3 >> stream" 10
                             "abc" 10 "endstream endobj" 10
                             (format nil "startxref~%~d~%%%EOF~%" at))))
    (multiple-value-bind (pdf accounts findings) (pal-read octets)
      (declare (ignore accounts))
      (true (plusp (length (palimpsest::pdf-revisions pdf))))
      (true (listp findings))))
  ;; a file of its header alone
  (is string= "1.4" (palimpsest::pdf-version (palimpsest::read-pdf (pal-octets "%PDF-1.4")))))

(defun pal-section (entries trailer &optional stream)
  "A cross-reference section listing ENTRIES (each (NUMBER OFFSET), OFFSET NIL for a free one) with TRAILER's
entries: a table and its trailer, or with STREAM, a number, a cross-reference stream object of that number, its
rows not compressed."
  (let ((entries (sort (copy-list entries) #'< :key #'first)) (runs '()))
    (loop while entries
          do (let ((run (loop for e on entries for k from (first (first entries))
                              while (= (first (first e)) k) collect (first e))))
               (push run runs)
               (setf entries (nthcdr (length run) entries))))
    (setf runs (nreverse runs))
    (if stream
        (pal-object stream
                    (list :stream (format nil "/Type /XRef /W [1 4 1] /Index [~{~{~d ~d~}~^ ~}] ~a"
                                          (mapcar (lambda (run) (list (first (first run)) (length run))) runs) trailer)
                          (coerce (loop for run in runs
                                        append (loop for (nil at) in run
                                                     append (if at
                                                                (list 1 (ldb (byte 8 24) at) (ldb (byte 8 16) at)
                                                                      (ldb (byte 8 8) at) (ldb (byte 8 0) at) 0)
                                                                (list 0 0 0 0 0 255))))
                                  '(vector (unsigned-byte 8)))))
        (pal-octets (with-output-to-string (s)
                      (format s "xref~%")
                      (dolist (run runs)
                        (format s "~d ~d~%" (first (first run)) (length run))
                        (dolist (e run)
                          (format s "~10,'0d ~5,'0d ~:[f~;n~]~c~c" (or (second e) 0) (if (second e) 0 65535)
                                  (second e) #\Return #\Newline)))
                      (format s "trailer~%<< ~a >>~%" trailer))))))

(defun pal-linearised
    (&key streams (first-startxref 0) one-save length again (last-names :first) rest-prev leftover (hint "/S 36"))
  "The invoice (*PAL-ORIGINAL-OBJECTS*) prepared for the web (linearised) as Ghostscript writes it, or with
STREAMS as Acrobat Distiller does, with cross-reference streams. Its linearisation dictionary comes
first, then the section of the first page's objects, whose trailer's /Prev names the section of the rest
and whose startxref is FIRST-STARTXREF (0, as they write it). Then come the hint stream and the first
page's objects, then the rest and their section. The file's last startxref names the first page's
section. With ONE-SAVE, the first page's section is followed by no startxref nor %%EOF of its own, as
pdfTeX's files linearised by qpdf are. Made wrong for the tests: LENGTH, the /L its dictionary gives
\(else the file's length). AGAIN, objects (each (NUMBER . BODY)) written in the rest and listed by its
section too, though the first page's lists them. LAST-NAMES :REST, the file's last startxref naming the
rest's section. REST-PREV, the rest's trailer naming the first page's section by /Prev. LEFTOVER, an
object no reference reaches, in the rest, that /H names beside the hint stream. HINT, the entries of the
hint stream's dictionary."
  (let ((places '()) (first-at 0) (main-at 0) (total 0) (out #()))
    (loop repeat 8
          for before = out
          do (let ((now '()))
               (setf out (pal-octets "%PDF-1.7" 10 "%" 226 227 207 211 10))
               (flet ((put (number body)
                        (push (cons number (length out)) now)
                        (setf out (pal-octets out (pal-object number body))))
                      (at (number) (or (cdr (assoc number places)) 0))
                      (body (number) (cdr (assoc number *pal-original-objects*))))
                 (put 10 (format nil "<< /Linearized 1 /L ~d /O 3 /E ~d /N 1 /T ~d /H [~d 40~@[ ~d 9~]] >>"
                                 (or length total) (at 2) main-at (at 9) (and leftover (at 7))))
                 (setf first-at (length out))
                 (setf out (pal-octets out (pal-section (append (list (list 10 (at 10)) (list 9 (at 9)))
                                                                (loop for n in '(1 3 4 5)
                                                                      for at = (assoc (cons :first n) places
                                                                                      :test #'equal)
                                                                      collect (list n (or (cdr at) 0)))
                                                                (and streams (list (list 11 first-at))))
                                                        (format nil "/Size 13 /Root 1 0 R /Info 6 0 R /Prev ~d" main-at)
                                                        (and streams 11))
                                       (if one-save "" (format nil "startxref~%~d~%%%EOF~%" first-startxref))))
                 (put 9 (list :stream hint "hints for the first page, read by no one"))
                 (dolist (n '(1 3 4 5))
                   (push (cons (cons :first n) (length out)) now)
                   (setf out (pal-octets out (pal-object n (body n)))))
                 (dolist (n '(2 6)) (put n (body n)))
                 (when leftover (put 7 '(:stream "" "BT /F1 12 Tf 72 680 Td (Total: 100) Tj ET")))
                 (dolist (object again) (put (car object) (cdr object)))
                 (setf main-at (length out))
                 (setf out (pal-octets out (pal-section (append (list (list 0 nil) (list 2 (at 2)) (list 6 (at 6)))
                                                                (and leftover (list (list 7 (at 7))))
                                                                (loop for (n) in again collect (list n (at n)))
                                                                (and streams (list (list 12 main-at))))
                                                        (format nil
                                                                "/Size 13~@[ /Root 1 0 R /Prev ~d~]"
                                                                (and rest-prev first-at))
                                                        (and streams 12))
                                       (format nil "startxref~%~d~%%%EOF~%"
                                               (if (eq last-names :rest) main-at first-at))))
                 (setf total (length out) places now)))
          until (equalp out before))
    out))

(define-test (palimpsest palimpsest-linearised-read-whole)
  ;; a file prepared for the web, by tables and by streams: one save, its first page's startxref 0 as the
  ;; specification writes it, every object listed and read, its page's text, no sign of editing
  (dolist (streams '(nil t))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-linearised :streams streams))
      (let ((revisions (palimpsest::pdf-revisions pdf)))
        (is = 1 (length revisions))
        (true (palimpsest::revision-linearised (aref revisions 0)))
        (is eq (if streams :stream :table) (palimpsest::revision-xref-kind (aref revisions 0)))
        (is equal '("Invoice for Kea Builders" "Total: 100")
            (palimpsest::text-lines (palimpsest::marks-of pdf (aref revisions 0)
                                                          (first (palimpsest::pages-of pdf (aref revisions 0)))))))
      (false (remove :record findings :key #'palimpsest::finding-strength)
             "~s" (mapcar #'palimpsest::finding-words findings))
      ;; its log says how its two parts make one save
      (let ((log (palimpsest::revision-log (palimpsest::make-analysis :pdf pdf :accounts accounts :name "test.pdf")
                                           (aref (palimpsest::pdf-revisions pdf) 0) "x")))
        (pal-has log "a file prepared for the web (linearised), saved once in two parts")
        (pal-has log "the section of the rest of the file, in this same save")
        (false (search "Not as it should be" log) "~a" log)))))

(define-test (palimpsest palimpsest-linearised-startxref-told)
  ;; a startxref naming nothing stays strong outside a first page's section: in a file not prepared for the
  ;; web, in a save appended to one, and a first page's startxref other than 0
  (flet ((strong-startxref (octets)
           (multiple-value-bind (pdf accounts findings) (pal-read octets)
             (declare (ignore pdf accounts))
             (pal-finding findings :strong "startxref names"))))
    (let* ((plain (pal-original))
           (at (search (pal-octets "startxref") plain :from-end t)))
      (true (strong-startxref (pal-octets (subseq plain 0 at) (format nil "startxref~%0~%%%EOF~%")))))
    (let* ((later (pal-save (pal-linearised) '((6 . "<< /Producer (Kea Writer 2.0) >>"))))
           (at (search (pal-octets "startxref") later :from-end t)))
      (true (strong-startxref (pal-octets (subseq later 0 at) (format nil "startxref~%0~%%%EOF~%")))))
    (true (strong-startxref (pal-linearised :first-startxref 12345))))
  ;; an honest save appended to a file prepared for the web: two saves, the linearisation undone for the record
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save (pal-linearised :streams t) '((6 . "<< /Producer (Kea Writer 2.0) >>"))))
    (declare (ignore accounts))
    (is = 2 (length (palimpsest::pdf-revisions pdf)))
    (is equal '(6) (palimpsest::revision-changed (aref (palimpsest::pdf-revisions pdf) 1)))
    (let ((changed (pal-finding findings
                                :record (format nil "prepared for fast web viewing, then changed by a later save, ~
                                                     which undoes the linearisation"))))
      (true changed)
      (when changed (false (search "not made by the program" (palimpsest::finding-words changed)))))
    (false (pal-finding findings :strong "") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-linearised-parts-agree)
  ;; the two parts of a file prepared for the web read as one only when they agree: /L the file's length, no
  ;; object in both sections, the last startxref naming the first page's section; else two saves and a strong sign
  (flet ((disagree (octets)
           (multiple-value-bind (pdf accounts findings) (pal-read octets)
             (declare (ignore accounts))
             (values (pal-finding findings
                                  :strong "the two parts of a save prepared for the web disagree") pdf findings))))
    (multiple-value-bind (found pdf) (disagree (pal-linearised :length 1000))
      (true found)
      (when found (pal-has (palimpsest::finding-words found) "/L says 1,000 octets"))
      (is = 2 (length (palimpsest::pdf-revisions pdf))))
    ;; the total written again in the rest: readers show the part the last startxref names first
    (dolist (case '((:first "Total: 100") (:rest "Total: 900")))
      (destructuring-bind (names total) case
        (multiple-value-bind (found pdf findings)
            (disagree (pal-linearised :again '((4 :stream ""
                                                "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET
BT /F1 12 Tf 72 680 Td (Total: 900) Tj ET"))
                                      :last-names names :rest-prev t))
          (true found "~a: ~s" names (mapcar #'palimpsest::finding-words findings))
          (when found (pal-has (palimpsest::finding-words found) "object 4 is in use in the sections of both"))
          (let ((latest (palimpsest::last-revision pdf)))
            (is equal (list "Invoice for Kea Builders" total)
                (palimpsest::text-lines (palimpsest::marks-of pdf latest (first (palimpsest::pages-of pdf latest))))))
          (false (pal-finding findings nil "listed by no cross-reference section") "~a: ~s" names
                 (mapcar #'palimpsest::finding-words findings))))))
  ;; written as one save, pdfTeX's files as qpdf linearises them: both sections read, nothing unlisted
  (dolist (streams '(nil t))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-linearised :one-save t :streams streams))
      (let ((revisions (palimpsest::pdf-revisions pdf)))
        (is = 1 (length revisions))
        (true (palimpsest::revision-linearised (aref revisions 0)))
        (is equal '("Invoice for Kea Builders" "Total: 100")
            (palimpsest::text-lines (palimpsest::marks-of pdf (aref revisions 0)
                                                          (first (palimpsest::pages-of pdf (aref revisions 0)))))))
      (false (remove :record findings :key #'palimpsest::finding-strength) "~s"
             (mapcar #'palimpsest::finding-words findings))
      (let ((log (palimpsest::revision-log (palimpsest::make-analysis :pdf pdf :accounts accounts :name "test.pdf")
                                           (aref (palimpsest::pdf-revisions pdf) 0) "x")))
        (pal-has log "saved once in two parts")
        (false (search "a %%EOF of its own" log))
        (false (search "Not as it should be" log) "~a" log)))))

(define-test (palimpsest palimpsest-hint-streams-only-real)
  ;; /H naming an object the file's first page's section does not list: a leftover all the same
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-linearised :leftover t))
    (declare (ignore pdf accounts))
    (let ((unused (pal-finding findings :record "no longer used")))
      (true unused "~s" (mapcar #'palimpsest::finding-words findings))
      (when unused (is equal '(7) (palimpsest::finding-objects unused)))))
  ;; a stream /H names that is no hint stream, without /S: a leftover too
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-linearised :hint ""))
    (declare (ignore pdf accounts))
    (let ((unused (pal-finding findings :record "no longer used")))
      (true unused "~s" (mapcar #'palimpsest::finding-words findings))
      (when unused (is equal '(9) (palimpsest::finding-objects unused)))))
  ;; a /Linearized dictionary in a file that is not prepared for the web hides no shadow attack
  (dolist (later (list '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources "
                                                     "<< /Font << /F1 5 0 R >> >> /Contents [4 0 R 9 0 R] >>")))
                       '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources "
                                                     "<< /Font << /F1 5 0 R >> >> /Contents 4 0 R /Annots [11 0 R] >>"))
                         (11 . "<< /Type /Annot /Subtype /Stamp /Rect [0 0 612 792] /F 4 /AP << /N 9 0 R >> >>"))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-save (pal-signed :fake-linearised t) later))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "Object 9 was hidden, unused, in the part the signature in object 8 covers")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-dates-at-their-precision)
  ;; a date written to the day agrees with the same day written to the second, in its zone or in universal time
  (flet ((creation (info xmp)
           (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                             '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                             (cons 6 (format nil "<< /CreationDate (~a) >>" info))
                                             (list 7 :stream "/Type /Metadata /Subtype /XML"
                                                   (format nil
                                                           "<?xpacket begin='' ~
                                                            id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta ~
                                                            xmlns:x='adobe:ns:meta/'><rdf:RDF ~
                                                            xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>~
                                                            <rdf:Description ~
                                                            xmlns:xmp='http://ns.adobe.com/xap/1.0/' ~
                                                            xmp:CreateDate='~a'/></rdf:RDF></x:xmpmeta><?xpacket ~
                                                            end='w'?>" xmp)))))
             (declare (ignore pdf accounts))
             (pal-finding findings nil "give different creation dates"))))
    (false (creation "D:20130328210040-06'00'" "2013-03-28"))
    (false (creation "D:20130328210040-06'00'" "2013-03-29"))
    (false (creation "D:20130328210040-06'00'" "2013-03"))
    ;; compared only to the month: said for the record
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                      '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                      '(6 . "<< /CreationDate (D:20130328210040-06'00') >>")
                                      (list 7 :stream "/Type /Metadata /Subtype /XML"
                                            (format nil "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta ~
                                                         xmlns:x='adobe:ns:meta/'><rdf:RDF ~
                                                         xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>~
                                                         <rdf:Description ~
                                                         xmlns:xmp='http://ns.adobe.com/xap/1.0/' ~
                                                         xmp:CreateDate='2013-03'/></rdf:RDF></x:xmpmeta><?xpacket ~
                                                         end='w'?>")))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "creation dates only as alike as the coarser is written, to the month")
            "~s" (mapcar #'palimpsest::finding-words findings)))
    (let ((apart (creation "D:20130328210040-06'00'" "2013-03-20")))
      (true apart)
      (when apart (is eq :strong (palimpsest::finding-strength apart))))
    (let ((apart (creation "D:20130328210040-06'00'" "2013-03-28T21:00:40+13:00")))
      (true apart)
      (when apart (is eq :look (palimpsest::finding-strength apart))))))

(define-test (palimpsest palimpsest-white-text-on-a-shading)
  ;; white text on a band a shading paints is seen, as on a page given a background; on a white shading, not
  (dolist (case '(("[0 0 1] /C1 [0 0 0.5]" nil) ("[1 1 1] /C1 [1 1 1]" t)))
    (destructuring-bind (colours unseen) case
      (multiple-value-bind (pdf accounts findings)
          (pal-read (pal-one-page "q 0 600 612 100 re W n /Sh0 sh Q BT 1 g /F1 12 Tf 72 640 Td (Annual report) Tj ET"
                                  :more (list (cons 8
                                                    (format nil
                                                            "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 612 ~
                                                             0] /Function << /FunctionType 2 /Domain [0 1] /C0 ~a /N 1 ~
                                                             >> >>" colours)))
                                  :fonts "/F1 5 0 R >> /Shading << /Sh0 8 0 R"))
        (declare (ignore pdf accounts))
        (is eq unseen (and (pal-finding findings nil "white text on a white page") t) "~a: ~s" colours
            (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-shadings-paint-where-they-reach)
  ;; white text on a shading that paints nothing where it lies, runs to white, or of a colour not worked out:
  ;; still unseen; on a shading of colours read from a table of samples, seen
  (flet ((white-text (shading &rest more)
           (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-one-page "q /Sh0 sh Q BT 1 g /F1 12 Tf 72 640 Td (Paid in full) Tj ET"
                                       :more (append (list (cons 8 shading)) more)
                                       :fonts "/F1 5 0 R >> /Shading << /Sh0 8 0 R"))
             (declare (ignore pdf accounts))
             (pal-finding findings nil "white text on a white page"))))
    (true (white-text (format nil "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [5000 5000 5001 5000] /Extend ~
                                   [false false] /Function << /FunctionType 2 /Domain [0 1] /C0 [0 0 0] /C1 [0 0 0] /N ~
                                   1 >> >>")))
    (true (white-text (format nil "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 612 0] /Function << ~
                                   /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [0.94 0.94 0.94] /N 1 >> >>")))
    (true (white-text "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 612 0] /Function 9 0 R >>"
                      '(9 :stream "/FunctionType 4 /Domain [0 1] /Range [0 1 0 1 0 1]" "{ pop 1 1 1 }")))
    ;; functions stitched together, over a /Domain of their own, as pgf and TikZ draw a shaded bar: seen
    (false (white-text (format nil "<< /ShadingType 2 /ColorSpace /DeviceRGB /Domain [0 100] /Coords [0 0 612 0] ~
                                    /Function << /FunctionType 3 /Domain [0 100] /Functions [ << /FunctionType 2 ~
                                    /Domain [0 1] /C0 [1 0.3 0.3] /C1 [1 0.55 0.55] /N 1 >> << /FunctionType 2 /Domain ~
                                    [0 1] /C0 [1 0.55 0.55] /C1 [1 0.8 0.8] /N 1 >> ] /Bounds [50] /Encode [0 1 0 1] ~
                                    >> >>")))
    ;; one running from white to a pale yellow, white lost on it: unseen
    (true (white-text (format nil "<< /ShadingType 2 /ColorSpace /DeviceRGB /Domain [0 100] /Coords [0 0 612 0] ~
                                   /Function << /FunctionType 3 /Domain [0 100] /Functions [ << /FunctionType 2 ~
                                   /Domain [0 1] /C0 [1 1 1] /C1 [1 1 0.7] /N 1 >> << /FunctionType 2 /Domain [0 1] ~
                                   /C0 [1 1 0.7] /C1 [1 1 0.9] /N 1 >> ] /Bounds [50] /Encode [0 1 0 1] >> >>")))
    ;; a black shading that lies far off the page but is extended past both its ends: the text seen on it
    (false (white-text (format nil "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [5000 5000 5001 5000] /Extend ~
                                    [true true] /Function << /FunctionType 2 /Domain [0 1] /C0 [0 0 0] /C1 [0 0 0] /N ~
                                    1 >> >>")))
    (false (white-text (format nil "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 612 0] /Extend [true true] ~
                                    /Function 9 0 R >>")
                       (list 9 :stream "/FunctionType 0 /Domain [0 1] /Range [0 1 0 1 0 1] /Size [2] /BitsPerSample 8"
                             (pal-octets 0 0 128 0 0 90)))))
  ;; a white shading laid over a total in a later save, a new total typed on it: the old one covered
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save (pal-original)
                          '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                                                        "/Resources << /Font << /F1 5 0 R >> /Shading << /Sh1 8 0 R >> "
                                                        ">> /Contents [4 0 R 7 0 R] >>"))
                            (7 :stream "" "q 70 676 120 18 re W n /Sh1 sh Q BT /F1 12 Tf 72 680 Td (Total: 900) Tj ET")
                            (8 . "<< /ShadingType 2 /ColorSpace /DeviceRGB /Coords [0 0 612 0] /Function
<< /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [1 1 1] /N 1 >> >>"))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape drawn over it, added in a later save: \"Total: 100\"")
          "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-unicode-ranges-carry)
  ;; a ToUnicode range raises its target as a number, carrying past 255
  (let ((map (palimpsest::unicode-map (pal-octets "1 beginbfrange <0000> <01FF> <0041> endbfrange"))))
    (is string= "A" (gethash 0 map))
    (is string= (string (code-char #x100)) (gethash #xBF map))
    (is string= (string (code-char #x240)) (gethash #x1FF map))))

(define-test (palimpsest palimpsest-time-zones-and-empty-pages)
  ;; 13 hours behind UTC is no place's time zone
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save #() (append (remove 6 *pal-original-objects* :key #'car)
                                      '((6 . #.(concatenate 'string "<< /Producer (Kea Writer 1.0) /CreationDate "
                                                                    "(D:20260101090000-13'00') >>"))))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "a time zone no place uses")))
  ;; a page of no size is not taken for a scan of itself
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-page-of (format nil "/MediaBox [0 0 0 0] /Resources << /Font << /F1 5 0 R >> /XObject << /Im1 7 0 ~
                                          R >> >> /Contents 4 0 R")
                             "q 100 0 0 100 0 0 cm /Im1 Do Q BT 3 Tr /F1 12 Tf 10 10 Td (Hidden) Tj ET"
                             :more '((7 :stream #.(concatenate 'string "/Type /XObject /Subtype /Image /Width 10 "
                                                                       "/Height 10 /ColorSpace /DeviceGray "
                                                                       "/BitsPerComponent 8") "x"))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "is a picture with invisible text"))))

(define-test (palimpsest palimpsest-number-at-point-in-characters)
  ;; the object number at point found on a line with letters of more than one octet before it
  (let ((buffer (tuatara.buffer:make-buffer (tuatara.buffer:unique-buffer-name "*Palimpsest number test*"))))
    (unwind-protect
         (let ((text (format nil "first line~%Rotated 90° — é € object 12 there~%")))
           (tuatara:set-made-text buffer text)
           (setf (tuatara.buffer:buffer-point buffer) (tuatara:string-octet-length text :end (+ (search "12" text) 2)))
           (is = 12 (palimpsest::number-at-point buffer)))
      (tuatara.buffer:kill-buffer buffer))))

(define-test (palimpsest palimpsest-nothing-written-beside-the-evidence)
  ;; saving, writing a revision and writing the report refuse the PDF's directory and palimpsest's own, links followed;
  ;; each offers a place outside them; the workings let go of once the report is made
  (with-test-directory (dir)
    (let* ((path (namestring (ensure-directories-exist (merge-pathnames "evidence/invoice.pdf" dir))))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/")))
           (to-cache (namestring (merge-pathnames "to-cache" dir)))
           (to-evidence (namestring (merge-pathnames "to-evidence" dir))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) o))
      (tuatara.io:make-symbolic-link (string-right-trim "/" directory) to-cache)
      (tuatara.io:make-symbolic-link (string-right-trim "/" (namestring (merge-pathnames "evidence/" dir))) to-evidence)
      (unwind-protect
           (let ((job (palimpsest::make-palimpsest-job nil path directory)))
             (palimpsest::examine job)
             (let ((analysis (palimpsest::palimpsest-job-analysis job)))
               ;; the editor's report keeps none of the PDF read
               (false (palimpsest::analysis-pdf analysis))
               (dolist (place (list (namestring (merge-pathnames "evidence/" dir))
                                    (concatenate 'string to-evidence "/")))
                 (pal-has (palimpsest::save-refusal analysis place) "the directory of the PDF examined")
                 (pal-has (palimpsest::target-refusal analysis (concatenate 'string place "invoice-revision-1.pdf"))
                          "the directory of the PDF examined")
                 (pal-has (palimpsest::directory-refusal analysis place) "the directory of the PDF examined"))
               ;; nor a directory within the evidence's, there or yet to be made
               (pal-has (palimpsest::save-refusal analysis (format nil "~aevidence/reports/" (namestring dir)))
                        "within the directory of the PDF examined")
               (pal-has (palimpsest::save-refusal analysis (concatenate 'string to-evidence "/reports/"))
                        "within the directory of the PDF examined")
               (pal-has (palimpsest::save-refusal analysis (concatenate 'string to-cache "/"))
                        "within palimpsest's own directory in the cache")
               (pal-has (palimpsest::target-refusal analysis (concatenate 'string to-cache "/invoice-revision-1.pdf"))
                        "within palimpsest's own directory in the cache")
               (is string= "~/invoice-revision-report.txt"
                   (palimpsest::offered-path analysis "invoice-revision-report.txt"))))
        (tuatara.io:remove-file to-cache)
        (tuatara.io:remove-file to-evidence)
        (tuatara.io:delete-tree directory)))))

(define-test (palimpsest palimpsest-nothing-saved-in-evidence-directories)
  ;; a directory Tuatara writes nothing in (*evidence-directories*) is refused for saving too, said in Tuatara's words
  (with-test-directory (dir)
    (let* ((path (namestring (ensure-directories-exist (merge-pathnames "work/invoice.pdf" dir))))
           (evidence (namestring (ensure-directories-exist (merge-pathnames "case/evidence/" dir))))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/")))
           (old tuatara-user:*evidence-directories*))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) o))
      (unwind-protect
           (let ((job (palimpsest::make-palimpsest-job nil path directory)))
             (palimpsest::examine job)
             (let ((analysis (palimpsest::palimpsest-job-analysis job)))
               (false (palimpsest::directory-refusal analysis evidence))
               (setf tuatara-user:*evidence-directories* (list evidence))
               (pal-has (palimpsest::save-refusal analysis evidence) "is an evidence directory")
               (pal-has (palimpsest::save-refusal analysis
                                                  (concatenate 'string evidence "reports/")) "is an evidence directory")
               (pal-has (palimpsest::target-refusal analysis (concatenate 'string evidence "invoice-revision-1.pdf"))
                        "is an evidence directory")
               (false (palimpsest::directory-refusal analysis (namestring (merge-pathnames "case/work/" dir))))))
        (setf tuatara-user:*evidence-directories* old)
        (tuatara.io:delete-tree directory)))))

;;; Findings that read a page, an action or a save rightly worded

(define-test (palimpsest palimpsest-text-fitted-into-a-gap)
  ;; a line whose characters are moved apart, the rest of the page not: named, and the page's other checks kept
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           (format nil "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders Ltd) Tj ET BT ~
                                                        /F1 12 Tf 72 680 Td (Thank you for your business) Tj ET ")
                                           (format nil "BT /F1 12 Tf 72 660 Td [(T) -400 (o) -400 (t) -400 (a) -400 ~
                                                        (l) -400 (:) -400 (9) -400 (0) -400 (0)] TJ ET ")
                                           "BT /F1 12 Tf 3 Tr 72 600 Td (Hidden words) Tj ET")))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "1 line on page 1 is spaced oddly, its characters set apart one by one"))
    (true (pal-finding findings nil "holds invisible text"))
    (false (pal-finding findings nil "could not finish"))))

(define-test (palimpsest palimpsest-actions-inside-objects)
  ;; a link's action written inside the link: found, named, and the link and a layer named for what they are
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save #() '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources <<
/Font << /F1 5 0 R >> >> /Contents 4 0 R /Annots [7 0 R] >>")
                                (4 :stream "" "BT /F1 12 Tf 72 700 Td (Pay online) Tj ET")
                                (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                                (6 . "<< /Producer (Kea Writer) >>")
                                (7 . "<< /Type /Annot /Subtype /Link /Rect [72 690 300 710] /A << /S /URI
/URI (https://example.org/pay) >> >>")
                                (8 . "<< /Type /OCG /Name (Draft notes) >>"))))
    (declare (ignore accounts))
    (let ((actions (pal-finding findings :record "The file holds 1 action that goes to places")))
      (true actions)
      (is equal '("object 7: URI \"https://example.org/pay\"") (palimpsest::finding-details actions)))
    (is string= "an annotation (Link) on page 1" (palimpsest::role-words pdf (palimpsest::last-revision pdf) 7))
    (is string= "the font F1 (Helvetica) of page 1" (palimpsest::role-words pdf (palimpsest::last-revision pdf) 5))
    (is string= "an object of type OCG" (palimpsest::role-words pdf (palimpsest::last-revision pdf) 8))))

(define-test (palimpsest palimpsest-pages-not-read-said)
  ;; a page whose content stream is not in the file: the report's summary says so
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save #() '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 9 0 R >>")
                                (6 . "<< /Producer (Kea Writer) >>"))))
    (declare (ignore accounts findings))
    (true (find "In revision 1, page 1's content stream, object 9, is not in the file"
                (palimpsest::pdf-notes pdf) :test #'string=))))

(define-test (palimpsest palimpsest-findings-worded)
  ;; dates that disagree, in the plural
  (let ((xmp (format nil "<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF ~
                          xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description ~
                          xmlns:xmp='http://ns.adobe.com/xap/1.0/' ~
                          xmp:ModifyDate='2026-01-01T09:00:00+13:00'/></rdf:RDF></x:xmpmeta>")))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                      '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                      '(6 . "<< /ModDate (D:20260301090000+13'00') >>")
                                      (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "give different dates last changed"))))
  ;; a PDF typed by hand, with no cross-reference section nor startxref: its trailer said, the step to see it true
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "hand.pdf" dir)))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (pal-octets (format nil
                                            "%PDF-1.4~%1 0 obj~%<< /Type /Catalog /Pages 2 0 R >>~%endobj~%2 0 obj~%<< ~
                                             /Type /Pages /Kids [] /Count 0 >>~%endobj~%trailer~%<< /Root 1 0 R ~
                                             >>~%%%EOF~%")) out))
      (let ((job (palimpsest::make-palimpsest-job nil path directory)))
        (palimpsest::examine job)
        (let* ((analysis (palimpsest::examine-file path (namestring dir)))
               (report (first (palimpsest::analysis-shown (palimpsest::palimpsest-job-analysis job))))
               (problem (find-if (lambda (f) (search "startxref" (palimpsest::finding-words f)))
                                 (palimpsest::analysis-findings analysis))))
          (pal-has report "Trailer     /Root object 1")
          (true problem)
          (pal-has (format nil "~{~a~%~}" (palimpsest::finding-steps problem)) "no startxref stands before it")))
      (tuatara.io:delete-tree directory))))

(define-test (palimpsest palimpsest-page-numbers-bounded)
  ;; matrices of the largest whole numbers a file may write, again and again: read at once, the text after them still
  ;; found
  (flet ((page-text (operator count)
           (let* ((pdf (pal-read (pal-one-page (format nil
                                                       "~v@{~a ~:*~}BT /F1 12 Tf 72 700 Td (Still read) Tj ET"
                                                       count operator))))
                  (last (palimpsest::last-revision pdf))
                  (marks (palimpsest::page-marks pdf last (first (palimpsest::pages-of pdf last)))))
             (values (find "Still read" marks :key #'palimpsest::mark-text :test #'equal) pdf))))
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (mark pdf)
          (page-text "99999999999999999999999999999999 0 0 99999999999999999999999999999999 0 0 cm" 1000)
        (true mark)
        (true (typep (palimpsest::mark-x0 mark) 'double-float))
        (false (find-if (lambda (n) (search "could not" n)) (palimpsest::pdf-notes pdf))))
      (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))
            "~,2f s to read 1,000 matrices of large numbers"
            (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    ;; numbers with a point, which no double holds once multiplied a few times over
    (true (page-text "9999999999999999999999999999.9 0 0 9999999999999999999999999999.9 0 0 cm" 50))))

(define-test (palimpsest palimpsest-page-stops-when-asked)
  ;; a page's operators followed in a job asked to stop: it stops there
  (let ((tuatara.runtime::*job* (palimpsest::make-palimpsest-job nil "/nonexistent/a.pdf" "/nonexistent/"))
        (seen 0))
    (setf (car (tuatara.runtime:job-cancel tuatara.runtime::*job*)) t)
    (fail (tuatara:map-pdf-content (lambda (&rest operator) (declare (ignore operator)) (incf seen))
                                   (pal-octets (format nil "~v@{~a ~:*~}" 10000 "0 0 m")))
          'tuatara.runtime:job-stopped)
    (true (< seen 10000))))

(define-test (palimpsest palimpsest-kept-text-shows-controls)
  ;; a /Producer holding a terminal's clipboard and title sequences: the log and report kept show them, never hold them
  (with-test-directory (dir)
    (let* ((producer (format nil "Kea~c]52;c;aGk=~cWriter~c]0;title~c" (code-char 27) (code-char 7)
                             (code-char 27) (code-char 7)))
           (octets (pal-save (pal-original)
                             (list (cons 6 (format nil "<< /Producer (~a) /Creator (Kea Writer) >>" producer)))))
           (path (namestring (merge-pathnames "invoice.pdf" dir)))
           (directory (tuatara.defaults::new-private-directory
                       (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
      (let ((job (palimpsest::make-palimpsest-job nil path directory)))
        (palimpsest::examine job)
        (let ((analysis (palimpsest::palimpsest-job-analysis job))
              (target (namestring (merge-pathnames "report.txt" dir))))
          (palimpsest::write-report analysis target)
          (dolist (text (list (uiop:read-file-string (second (second (palimpsest::analysis-files analysis))))
                              (uiop:read-file-string target)))
            (pal-has text "Kea^[]52;c;aGk=^GWriter")
            (false (find-if (lambda (c) (member (char-code c) '(7 27))) text)))))
      (tuatara.io:delete-tree directory))))

(define-test (palimpsest palimpsest-index-past-its-limit-said-once)
  ;; a cross-reference table past the most entries read: said once, not again for each sub-section after
  (let ((tuatara-user:*palimpsest-most-objects* 1))
    (let* ((pdf (pal-read (pal-save #() '((1 . "<< /Type /Catalog >>") (2 . "null") (5 . "null") (6 . "null")))))
           (problems (palimpsest::revision-problems (aref (palimpsest::pdf-revisions pdf) 0))))
      (is = 1 (count-if (lambda (p) (search "lists more entries than are read" p)) problems))))
  ;; a cross-reference stream whose index names far more than it holds: said once
  (let* ((index (format nil "~{~d 1~^ ~}" (loop for k below 2000 collect (* 3 k))))
         (xref (pal-object 1 (list :stream (format nil "/Type /XRef /W [1 1 1] /Size 6000 /Index [~a]" index)
                                   (pal-octets 1 0 0))))
         (octets (pal-octets "%PDF-1.5" 10 xref (format nil "startxref~%9~%%%EOF~%")))
         (pdf (pal-read octets))
         (problems (palimpsest::revision-problems (aref (palimpsest::pdf-revisions pdf) 0))))
    (is = 1 (count-if (lambda (p) (search "is shorter than its index says" p)) problems))))

(define-test (palimpsest palimpsest-text-set-unlike-its-line)
  ;; a few words of a line in another size, weight, setting, spacing or black: each named, with what it is and
  ;; what its line is
  (flet ((line (odd &optional (before "") (after ""))
           (pal-one-page (format nil
                                 "BT /F1 12 Tf 72 720 Td (Invoice for Kea Builders Ltd) Tj ET BT ~a/F1 12 Tf 72 700 Td ~
                                  (Amount payable to Kea Builders Ltd) Tj ~a( by Friday) Tj ~aET"
                                 before odd after))))
    (loop for (odd before after words detail)
            in '(("/F1 11 Tf " "" "" "in another size than the line around it"
                  "\"by Friday\" at 11.0 pt, its line at 12.0 pt (4 @73 font 5)")
                 ("2 Tr " "" "0 Tr " "drawn bolder or lighter"
                  "\"by Friday\" filled and outlined, its line filled (4 @73 font 5)")
                 ("90 Tz " "" "100 Tz " "set narrower or wider"
                  "\"by Friday\" at 90 % of its width, its line at 100 % (4 @73 font 5)")
                 ("0.6 Tc " "" "0 Tc " "characters spaced unlike"
                  "\"by Friday\" with 0.05 of its size after each character, its line 0.00 (4 @73 font 5)")
                 ("0 g " "0.2 g " "" "in another black"
                  "\"by Friday\" in grey 0.00, its line in grey 0.20 (4 @79 font 5)"))
          do (multiple-value-bind (pdf accounts findings) (pal-read (line odd before after))
               (declare (ignore pdf accounts))
               (let ((finding (pal-finding findings :look words)))
                 (true finding "~a: ~s" words (mapcar #'palimpsest::finding-words findings))
                 (when finding (is equal (list detail) (palimpsest::finding-details finding)))))))
  ;; set alike, a superscript, words spread by word spacing, a bold font of the family: nothing said
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           "BT /F1 12 Tf 72 720 Td (Invoice for Kea Builders Ltd) Tj ET "
                                           (format nil "BT /F1 12 Tf 72 700 Td (Amount payable to Kea Builders) Tj /F1 ~
                                                        7 Tf 2 Ts (note) Tj 0 Ts ET ")
                                           "BT /F1 12 Tf 2 Tw 72 680 Td (Paid in full on the day) Tj ET "
                                           (format nil "BT /F1 12 Tf 72 660 Td (Thank you for your business) Tj /F2 12 ~
                                                        Tf ( Kea) Tj ET"))
                              :fonts "/F1 5 0 R /F2 7 0 R"
                              :more '((7 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold >>"))))
    (declare (ignore pdf accounts))
    (dolist (words '("another size" "bolder or lighter" "narrower or wider" "spaced unlike" "another black"))
      (false (pal-finding findings nil words) "~a: ~s" words (mapcar #'palimpsest::finding-words findings)))))

(defun pal-font-file (changed)
  "The octets of a TrueType font file holding only its head table, which says
it was made in 2020 and last changed at the universal time CHANGED."
  (let ((head (make-array 54 :element-type '(unsigned-byte 8) :initial-element 0)))
    (flet ((put (at value) (loop for i below 8 do (setf (aref head (+ at i)) (ldb (byte 8 (* 8 (- 7 i))) value)))))
      (put 20 (- (encode-universal-time 0 0 0 1 1 2020 0) (* 4 365 86400)))
      (put 28 (- changed (* 4 365 86400))))
    (pal-octets 0 1 0 0  0 1  0 0 0 0 0 0
                "head" 0 0 0 0  0 0 0 28  0 0 0 54
                head)))

(defun pal-embedded (changed)
  "A one-page PDF made on 1 January 2026, its text in an Arial kept in the
file that was last changed at the universal time CHANGED."
  (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                      '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << "
                                                   "/Font << /F1 7 0 R >> >> /Contents 4 0 R >>"))
                      '(4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET")
                      '(6 . "<< /Producer (Kea Writer) /CreationDate (D:20260101090000Z) >>")
                      '(7 . "<< /Type /Font /Subtype /TrueType /BaseFont /ABCDEF+ArialMT /FontDescriptor 8 0 R >>")
                      '(8 . "<< /Type /FontDescriptor /FontName /ABCDEF+ArialMT /FontFile2 9 0 R >>")
                      (list 9 :stream "" (pal-font-file changed)))))

(define-test (palimpsest palimpsest-fonts-named-and-dated)
  ;; one family named as two programs name it on one page; one program's names for its styles are not
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F2 12 Tf 72 680 ~
                                           Td (Total: 900) Tj ET")
                              :fonts "/F1 7 0 R /F2 8 0 R"
                              :more '((7 . "<< /Type /Font /Subtype /TrueType /BaseFont /ArialMT >>")
                                      (8 . "<< /Type /Font /Subtype /TrueType /BaseFont /Arial,Bold >>"))))
    (declare (ignore pdf accounts))
    (let ((named (pal-finding findings
                              :look "On page 1 one font is named the ways two programs name it (ArialMT, Arial,Bold)")))
      (true named "~s" (mapcar #'palimpsest::finding-words findings))
      ;; the names, then the runs in the name that sets less text, each with its place
      (when named
        (is equal '("F1 as ArialMT" "F2 as Arial,Bold") (subseq (palimpsest::finding-details named) 0 2))
        (is = 3 (length (palimpsest::finding-details named)))
        (pal-has (third (palimpsest::finding-details named)) "text \"Total: 900\" at x 72, y 680"))))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F2 12 Tf 72 680 ~
                                           Td (Total: 900) Tj ET")
                              :fonts "/F1 7 0 R /F2 8 0 R"
                              :more '((7 . "<< /Type /Font /Subtype /TrueType /BaseFont /ArialMT >>")
                                      (8 . "<< /Type /Font /Subtype /TrueType /BaseFont /Arial-BoldMT >>"))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "named the ways two programs")))
  ;; a font kept in the file changed after the document was made: put in later; one changed before: nothing
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-embedded (encode-universal-time 0 0 12 3 3 2026 0)))
    (declare (ignore pdf accounts))
    (let ((later (pal-finding findings :strong "The font ArialMT kept in the file was last changed")))
      (true later "~s" (mapcar #'palimpsest::finding-words findings))
      (when later
        (pal-has (palimpsest::finding-words later) "after the document was made")
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence later)) "CreationDate"))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-embedded (encode-universal-time 0 0 12 3 3 2025 0)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "kept in the file was last changed"))))

(defun pal-width-font (number name &key (width 500) (figures width) file)
  "Object NUMBER of a PDF: a TrueType font called NAME in the WinAnsi encoding,
each character WIDTH thousandths of its size wide, the figures FIGURES; its
font program kept in object FILE when given."
  (cons number
        (format nil "<< /Type /Font /Subtype /TrueType /BaseFont /~a /Encoding /WinAnsiEncoding /FirstChar 32 ~
                     /LastChar 122 /Widths [~{~d~^ ~}]~@[ /FontDescriptor << /FontName /~a /FontFile2 ~d 0 R >>~] >>"
                name (loop for c from 32 to 122 collect (if (digit-char-p (code-char c)) figures width))
                (and file name) file)))

(defun pal-pitched (figure)
  "A one-page PDF of lines in a font of characters 5 points wide at its size,
placed by the widths, kerned a pair at a time, spread by word spacing and
moved on between words; the last line ends with FIGURE, the operators
showing it, 150 points from where the line begins."
  (pal-one-page (format nil "BT /F1 10 Tf 72 720 Td [(Invoice for Kea Builders Limited)] TJ ET ~
                             BT /F1 10 Tf 72 700 Td [(W) 60 (ater tanks and T) 70 (ow) 50 (er parts)] TJ ET ~
                             BT /F1 10 Tf 3 Tw 72 680 Td (Delivered in full on the day) Tj 0 Tw ET ~
                             BT /F1 10 Tf 72 660 Td [(Paid) -400 (by) -400 (bank) -400 (transfer)] TJ ET ~
                             BT /F1 10 Tf 72 640 Td [(Amount due for March services )] TJ ~a ET" figure)
                :fonts "/F1 7 0 R" :more (list (pal-width-font 7 "ABCDEF+KeaSans"))))

(define-test (palimpsest palimpsest-pitch-against-widths)
  ;; a figure whose characters are moved on one by one, as if set with wider figures: named, with the moves
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-pitched "[(1) -80 (2) -80 (3) -80 (4)] TJ"))
    (declare (ignore pdf accounts))
    (let ((pitched (pal-finding findings :look "1 stretch of text is placed unlike its own font's widths")))
      (true pitched "~s" (mapcar #'palimpsest::finding-words findings))
      (when pitched
        (pal-has (first (palimpsest::finding-details pitched)) "\"1234\" moved on 80, 80, 80 thousandths")
        (is eq :fitted (palimpsest::finding-method pitched)))))
  ;; the same figure placed one character at a time by moves of its own, drawn narrower than its font says
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-pitched "150 0 Td (1) Tj 4.2 0 Td (2) Tj 4.2 0 Td (3) Tj 4.2 0 Td (4) Tj"))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "placed unlike its own font's widths") "~s"
          (mapcar #'palimpsest::finding-words findings)))
  ;; kerned pairs, word spacing, moves between words, a figure placed by its widths, a figure of one pair kerned
  ;; again and again (1111, as Helvetica kerns 1 1): nothing said
  (dolist (figure '("[(1) -80 (234)] TJ" "[(1) 74 (1) 74 (1) 74 (1)] TJ"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-pitched figure))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "placed unlike") "~a: ~s" figure
             (mapcar #'palimpsest::finding-words findings)))))

(defun pal-two-widths (&key (width 500) (figures 500) later)
  "A one-page PDF of two lines in two subsets of one font, both embedded, the
second's letters WIDTH and figures FIGURES thousandths of its size wide, the
first's all 500; with LATER, the second line and its font added in a later
save."
  (let* ((first-line "BT /F1 10 Tf 72 700 Td (Invoice 2026 for Kea) Tj ET")
         (both (concatenate 'string first-line " BT /F2 10 Tf 72 680 Td (Total 1834) Tj ET"))
         (second-font (list (pal-width-font 8 "GHIJKL+KeaSans" :width width :figures figures :file 10)
                            (list 10 :stream "" "font program"))))
    (flet ((page (content fonts)
             (list (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font ~
                                        << ~a >> >> /Contents 4 0 R >>" fonts))
                   (list 4 :stream "" content))))
      (if later
          (pal-save (pal-one-page first-line :fonts "/F1 7 0 R"
                                             :more (list (pal-width-font 7 "ABCDEF+KeaSans" :file 9)
                                                         (list 9 :stream "" "font program")))
                    (append (page both "/F1 7 0 R /F2 8 0 R") second-font))
          (pal-one-page both :fonts "/F1 7 0 R /F2 8 0 R"
                             :more (append (list (pal-width-font 7 "ABCDEF+KeaSans" :file 9)
                                                 (list 9 :stream "" "font program"))
                                           second-font))))))

(define-test (palimpsest palimpsest-one-font-name-two-widths)
  ;; a subset of the font added in a later save, its letters and figures wider: two programs of the font, named
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-two-widths :width 520 :figures 556 :later t))
    (declare (ignore pdf accounts))
    (let ((twice (pal-finding findings :look "The font KeaSans is in the file twice with other widths for 91 of")))
      (true twice "~s" (mapcar #'palimpsest::finding-words findings))
      (when twice
        (is equal '("ABCDEF+KeaSans (object 7) on page 1: 17 characters, \"Invoice 2026 for Kea\""
                    "GHIJKL+KeaSans (object 8) on page 1: 9 characters, \"Total 1834\"")
            (subseq (palimpsest::finding-details twice) 0 2))
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details twice)) "\"!\" 500, 520"))))
  ;; the same widths; the two written in one save, as a program splits a font or keeps its alternates; only the
  ;; figures other, as tabular and proportional figures are: not named for their widths
  (loop for keys in '((:width 500 :figures 500 :later t) (:width 520 :figures 556) (:figures 556 :later t))
        do (multiple-value-bind (pdf accounts findings) (pal-read (apply #'pal-two-widths keys))
             (declare (ignore pdf accounts))
             (false (pal-finding findings nil "other widths") "~s: ~s" keys
                    (mapcar #'palimpsest::finding-words findings)))))

(defun pal-commented (comments octets)
  "OCTETS, a PDF of one save, with COMMENTS, lines beginning with %, written
between its cross-reference table and its trailer."
  (if comments
      (let ((trailer (search (pal-octets "trailer") octets :from-end t)))
        (pal-octets (subseq octets 0 trailer) (format nil "~{~a~%~}" comments) (subseq octets trailer)))
      octets))

(defun pal-kept-fonts (content names &key (producer "Kea Writer") comments)
  "A one-page PDF drawing CONTENT with the fonts F1, F2 and on, NAMES their
/BaseFont names in order, each a TrueType font whose program is kept in the
file, but for a name beginning with a colon, which is a Type 1 font of the
rest of the name not kept in it; its producer PRODUCER; COMMENTS, lines
written between its cross-reference table and its trailer."
  (let ((fonts (loop for name in names for k from 0
                     for number = (+ 10 (* 2 k))
                     collect (if (char= (char name 0) #\:)
                                 (list (cons number (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /~a >>"
                                                            (subseq name 1))))
                                 (list (pal-width-font number name :file (1+ number))
                                       (list (1+ number) :stream "" (format nil "font program ~d" k)))))))
    (pal-commented
     comments
     (pal-save #()
              (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                            '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                            (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << ~
                                                 /Font << ~{/F~d ~d 0 R ~}>> >> /Contents 4 0 R >>"
                                            (loop for k from 0 below (length names)
                                                  append (list (1+ k) (+ 10 (* 2 k))))))
                            (list 4 :stream "" content)
                            (cons 6 (format nil "<< /Producer (~a) >>" producer)))
                      (apply #'append fonts))))))

(define-test (palimpsest palimpsest-subsets-of-an-honest-writer)
  ;; an office program splits one font over numbered subsets, each setting lines of its own: for the record
  (let ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                          (format nil "BT /F1 10 Tf 72 700 Td (Invoice 2026 for Kea) Tj ET ~
                                                       BT /F2 10 Tf 72 680 Td (Total 1834) Tj ET")
                                          '("BAAAAA+KeaSans" "CAAAAA+KeaSans"))))))
    (true (pal-finding findings :record "The font KeaSans is in the file as 2 subsets")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings :look "subsets")))
  ;; a figure in a second subset, in a line of the first: set into the line, with its place
  (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                           (format nil "BT /F1 10 Tf 72 700 Td (Invoice 2026 for Kea) Tj ET ~
                                                        BT /F1 10 Tf 72 680 Td (Total due) Tj /F2 10 Tf 80 0 Td ~
                                                        (1834) Tj ET")
                                           '("ABCDEF+KeaSans" "QRSTUV+KeaSans")))))
         (set-in (pal-finding findings :look "The font KeaSans is in the file as 2 subsets")))
    (true set-in "~s" (mapcar #'palimpsest::finding-words findings))
    (when set-in
      (pal-has (palimpsest::finding-words set-in) "1 run of plain words or figures stands in a line set in another")
      (is eql 1 (palimpsest::finding-page set-in))
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details set-in)) "page 1: text \"1834\" at x 152, y 680"))))

(define-test (palimpsest palimpsest-subset-tags-of-the-producer)
  ;; a numbered tag given to two fonts: two exports' pages in one file; the page set apart named
  (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                           (format nil "BT /F1 10 Tf 72 700 Td (Invoice 2026 for Kea) Tj ET ~
                                                        BT /F2 10 Tf 72 680 Td (Total 1834) Tj ET")
                                           '("BAAAAA+KeaSans" "BAAAAA+KeaSerif") :producer "LibreOffice 26.8"))))
         (twice (pal-finding findings :look "The subset tag BAAAAA is given twice")))
    (true twice "~s" (mapcar #'palimpsest::finding-words findings))
    (when twice (is eq :pages-changed (palimpsest::finding-method twice))))
  ;; a file naming LibreOffice whose subsets carry tags drawn at random: its pages are another program's
  (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts "BT /F1 10 Tf 72 700 Td (Invoice 2026 for Kea) Tj ET"
                                                          '("EUNSVE+KeaSans") :producer "LibreOffice 26.8"))))
         (other (pal-finding findings :look "names LibreOffice 26.8 as its producer")))
    (true other "~s" (mapcar #'palimpsest::finding-words findings))
    (when other (pal-has (palimpsest::finding-words other) "EUNSVE+KeaSans")))
  ;; LibreOffice's own tags, and random tags under another producer's name: nothing said of them
  (loop for (names producer) in '((("BAAAAA+KeaSans" "CAAAAA+KeaSerif") "LibreOffice 26.8")
                                  (("EUNSVE+KeaSans" "EUNSVE+KeaSerif") "Kea Writer"))
        do (let ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                                   (format nil "BT /F1 10 Tf 72 700 Td (Invoice 2026) Tj ET ~
                                                                BT /F2 10 Tf 72 680 Td (Total 1834) Tj ET")
                                                   names :producer producer)))))
             (false (pal-finding findings nil "is given twice") "~s" names)
             (false (pal-finding findings nil "as its producer, but") "~s" names))))

(define-test (palimpsest palimpsest-text-of-another-program-placed)
  ;; a font named in words beside the same family as a subset: another program's text, its runs named with places
  (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                           (format nil "BT /F1 10 Tf 72 720 Td (Certificate of merit) Tj ET ~
                                                        BT /F1 10 Tf 72 700 Td (Date of award) Tj ET ~
                                                        BT /F2 10 Tf 200 700 Td (30 January 2025) Tj ET")
                                           '("BAAAAA+LiberationSans" "Liberation#20Sans#20Regular")))))
         (named (pal-finding findings :look "one font is named the ways two programs name it")))
    (true named "~s" (mapcar #'palimpsest::finding-words findings))
    (when named
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details named))
               "text \"30 January 2025\" at x 200, y 700")))
  ;; a figure in a font not kept in the file, in a line of text in fonts kept in it
  (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                           (format nil "BT /F1 10 Tf 72 700 Td (Invoice for Kea Builders) Tj ET ~
                                                        BT /F1 10 Tf 72 680 Td (Total due) Tj ET ~
                                                        BT /F2 11 Tf 200 680 Td (2,989.28) Tj ET")
                                           '("BAAAAA+KeaSans" ":Helvetica")))))
         (unkept (pal-finding findings :look "1 run of text is set in a font not kept in the file (Helvetica)")))
    (true unkept "~s" (mapcar #'palimpsest::finding-words findings))
    (when unkept
      (is eq :other-program (palimpsest::finding-method unkept))
      (is equal '("\"2,989.28\" in Helvetica (4 @110 font 12)") (palimpsest::finding-details unkept))))
  ;; a page all in fonts not kept, as a billing system writes: nothing to say
  (let ((findings (nth-value 2 (pal-read (pal-one-page (format nil "BT /F1 10 Tf 72 700 Td (Total due) Tj ET ~
                                                                    BT /F1 11 Tf 200 700 Td (2,989.28) Tj ET"))))))
    (false (pal-finding findings nil "not kept in the file"))))

(define-test (palimpsest palimpsest-figures-set-to-pass-are-no-habit)
  ;; three figures of a payslip set again in another font, at their lines' size: each stays worth a look, where
  ;; three lines off an edge are a program's habit
  (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts
                                           (format nil "~{BT /F1 10 Tf 72 ~d Td (~a) Tj ET BT /F~d 10 Tf 300 ~
                                                        ~:*~:*~:*~d Td~* (~*~a) Tj ET ~}"
                                                   '(700 "Basic pay for the month of March" 1 "2,106.10"
                                                     680 "Holiday pay for the month" 2 "180.80"
                                                     660 "Gross pay for the month" 2 "2,186.10"
                                                     640 "Net pay for the month" 2 "1,783.43"
                                                     620 "Paid into the account ending" 1 "4471"))
                                           '("BAAAAA+KeaSans" "Kea#20Sans#20Regular")))))
         (passing (pal-finding findings :look "in another font than the line around them, at the line's size")))
    (true passing "~s" (mapcar #'palimpsest::finding-words findings))
    (when passing (is = 3 (length (palimpsest::finding-details passing))))
    (false (pal-finding findings :record "The file sets")))
  ;; such figures on page after page are how a program sets its years and section numbers: said once
  (flet ((passing (page)
           (palimpsest::found :look "a figure set to pass" :page page :method :retyped :kind :figure-passing)))
    (is = 2 (count :look (palimpsest::layout-habit (list (passing 1) (passing 1)))
                   :key #'palimpsest::finding-strength))
    (let ((habit (palimpsest::layout-habit (list (passing 1) (passing 2)))))
      (is = 1 (length habit))
      (is eq :record (palimpsest::finding-strength (first habit))))))

(defun pal-priced-rows (rows &rest totals)
  "A one-page PDF of a table: ROWS, each (LABEL QUANTITY PRICE AMOUNT), a
row to a line, its figures in three columns; then TOTALS, each (LABEL
AMOUNT), a line each under the amounts."
  (pal-one-page
   (with-output-to-string (s)
     (format s "BT /F1 10 Tf 72 740 Td (Invoice 2026 from Kea Builders) Tj ET~%")
     (loop for (label . figures) in (append rows (mapcar (lambda (total) (list (first total) nil nil (second total)))
                                                          totals))
           for y downfrom 700 by 16
           do (format s "BT /F1 10 Tf 72 ~d Td (~a) Tj ET~%" y label)
              (loop for figure in figures for x in '(300 380 460)
                    when figure do (format s "BT /F1 10 Tf ~d ~d Td (~a) Tj ET~%" x y figure))))))

(define-test (palimpsest palimpsest-totals-and-rows-add-up)
  (loop for (word value amount) in '(("118.00" 11800 t) ("1,250.00" 125000 t) ("1.250,00" 125000 t) ("3" 300 nil)
                                     ("2.5" 250 nil) ("£34.50" 3450 t) ("-240.00" -24000 t) ("12/03" nil nil)
                                     ("1,25,00" nil nil) ("Kea" nil nil))
        do (multiple-value-bind (read two-decimals) (palimpsest::plain-number word)
             (is eql value read "~s" word)
             (is eq amount two-decimals "~s" word)))
  ;; a quantity written with two decimals, as hours are: read as any other
  (is equal '((3 4250)) (palimpsest::unmultiplied-rows '("Labour 7.50 40.00 300.00" "Travel 2.00 21.00 42.00"
                                                         "Survey 1.50 60.00 90.00" "Extras 1.25 34.00 60.00"
                                                         "Fee 3.00 15.00 45.00")))
  (let ((rows '(("Isolating valve" "12" "3.25" "39.00") ("Pipe insulation" "30" "0.85" "25.50")
                ("Radiator double" "3" "118.00" "354.00") ("Copper elbow" "4" "2.50" "10.00"))))
    ;; a quantity and an amount raised, the subtotal left: the amounts above it named with their places
    (let* ((findings (nth-value 2 (pal-read (pal-priced-rows rows '("Subtotal" "392.50") '("VAT" "78.50")
                                                             '("Total due" "471.00")))))
           (unsummed (pal-finding findings :look "the 4 amounts above \"Subtotal 392.50\" add up to 428,50")))
      (true unsummed "~s" (mapcar #'palimpsest::finding-words findings))
      (when unsummed
        (is eq :figure-unchecked (palimpsest::finding-method unsummed))
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details unsummed))
                 "\"Radiator double 3 118.00 354.00\", text \"354.00\" at x 460, y 668")))
    ;; a table that adds up, its total after VAT the sum of the two above it: nothing said
    (let ((findings (nth-value 2 (pal-read (pal-priced-rows rows '("Subtotal" "428.50") '("VAT" "85.70")
                                                            '("Total due" "514.20"))))))
      (false (pal-finding findings nil "add up to") "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "multiply out")))
    ;; a price raised, its amount and the totals left: the row that does not multiply out
    (let* ((findings (nth-value 2 (pal-read (pal-priced-rows (append rows '(("Day rate" "2" "650.00" "900.00")))
                                                             '("Subtotal" "1,328.50")))))
           (row (pal-finding findings :look "1 row of a table does not multiply out as its other rows do")))
      (true row "~s" (mapcar #'palimpsest::finding-words findings))
      (when row
        (pal-has (palimpsest::finding-words row)
                 "\"Day rate 2 650.00 900.00\", where quantity and price make 1 300,00")))
    ;; an order's total raised by one digit, its subtotal and delivery left; a total of hours before a total
    ;; of pay is no such thing
    (let* ((findings (nth-value 2 (pal-read (pal-priced-rows rows '("Subtotal" "428.50") '("Delivery" "4.95")
                                                             '("Order total" "533.45")))))
           (unsummed (pal-finding findings :look "the 2 amounts above \"Order total 533.45\" add up to 433,45")))
      (true unsummed "~s" (mapcar #'palimpsest::finding-words findings)))
    (let ((findings (nth-value 2 (pal-read (pal-priced-rows '() '("Total hours" "37.50") '("Hourly rate" "12.50")
                                                            '("Total pay" "468.75"))))))
      (false (pal-finding findings nil "add up to") "~s" (mapcar #'palimpsest::finding-words findings)))
    ;; a list of two amounts, and a statement whose last column is a running balance: not judged
    (let ((findings (nth-value 2 (pal-read (pal-priced-rows (subseq rows 0 2) '("Total" "70.00"))))))
      (false (pal-finding findings nil "add up to")))
    (let ((findings (nth-value 2 (pal-read (pal-priced-rows '(("Opening balance" nil nil "500.00")
                                                              ("Card payment" nil "71.44" "428.56")
                                                              ("Salary" nil "2,213.04" "2,641.60")
                                                              ("Rent" nil "950.00" "1,691.60"))
                                                            '("Total paid out" "1,021.44"))))))
      (false (pal-finding findings nil "add up to") "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-checksum-of-a-file-changed-in-place)
  (flet ((file (id &key sealed)
           ;; a file as LibreOffice writes it, its trailer's checksum of the octets before it holding when SEALED
           (let* ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                              '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                              '(6 . "<< /Producer (LibreOffice 26.8) >>"))
                                    :trailer (format nil "/Root 1 0 R /Info 6 0 R /ID [~a] /DocChecksum /~32,'0d"
                                                     id 0)))
                  (trailer (search (pal-octets "trailer") octets :from-end t))
                  (at (search (pal-octets "/DocChecksum /") octets :from-end t)))
             (when sealed
               (replace octets (pal-octets (palimpsest::octets-md5 octets :end trailer)) :start1 (+ at 14)))
             octets)))
    ;; the checksum holds: nothing said
    (false (pal-finding (nth-value 2 (pal-read (file "<AB12> <AB12>" :sealed t))) nil "/DocChecksum"))
    ;; it fails and the two parts of /ID are alike: changed where it lay, strong
    (let ((changed (pal-finding (nth-value 2 (pal-read (file "<AB12> <AB12>"))) :strong "/DocChecksum")))
      (true changed)
      (when changed
        (pal-has (palimpsest::finding-words changed) "changed where it lay")
        (is eq :structure-by-hand (palimpsest::finding-method changed))))
    ;; it fails and the parts differ: written again by a program that says so, for the record
    (let ((findings (nth-value 2 (pal-read (file "<AB12> <CD34>")))))
      (true (pal-finding findings :record "a program wrote the file again and kept the old checksum"))
      (false (pal-finding findings :strong "/DocChecksum")))))

(define-test (palimpsest palimpsest-metadata-editors-named)
  ;; a file left in the form qpdf writes for changing it by hand, and one ExifTool wrote into: worth a look
  (loop for (comments name) in '((("%QDF-1.0" "%% Original object ID: 5 0") "qpdf's QDF form")
                                 (("%BeginExifToolUpdate") "ExifTool"))
        do (let* ((findings (nth-value 2 (pal-read (pal-kept-fonts "BT /F1 10 Tf 72 700 Td (Invoice) Tj ET"
                                                                   '("BAAAAA+KeaSans") :comments comments))))
                  (named (pal-finding findings :look "written between the objects")))
             (true named "~s: ~s" name (mapcar #'palimpsest::finding-words findings))
             (when named (pal-has (palimpsest::finding-words named) name))))
  ;; XMP written by ExifTool in a file whose producer is another program: written anew; the producer's own
  ;; toolkit is nothing to say
  (flet ((findings (toolkit)
           (let ((xmp (format nil "<x:xmpmeta xmlns:x='adobe:ns:meta/' x:xmptk='~a'><rdf:RDF ~
                                   xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description ~
                                   xmlns:pdf='http://ns.adobe.com/pdf/1.3/' pdf:Producer='Kea ~
                                   Writer'/></rdf:RDF></x:xmpmeta>" toolkit)))
             (nth-value 2 (pal-read (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                                        '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                                        '(6 . "<< /Producer (Kea Writer) >>")
                                                        (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))))))
    (let ((anew (pal-finding (findings "Image::ExifTool 13.50") :look "The XMP metadata was written by ExifTool")))
      (true anew)
      (when anew (is eq :metadata-edited (palimpsest::finding-method anew))))
    (false (pal-finding (findings "Kea XMP Core 1.0") nil "The XMP metadata was written by"))))

(defun pal-table (&key (font "F1") (size 10) (grey 0) (spacing 0) total)
  "A one-page PDF of a table of five rows, a label and two figures right
aligned in each, set in font F1 at 10 points in black; the second figure of
the fourth row in FONT, at SIZE, in GREY, SPACING after each character; with TOTAL, a last row set in bold,
its label too."
  (pal-one-page
   (with-output-to-string (s)
     (format s "BT /F1 10 Tf 72 740 Td (Quarterly figures for Kea Builders) Tj ET~%")
     (loop for (label a b) in '(("Timber" "1,250.00" "980.00") ("Roofing" "12,400.00" "11,050.50")
                                ("Labour" "8,975.25" "9,120.00") ("Scaffolding" "640.00" "1,180.75")
                                ("Freight" "312.40" "298.10"))
           for y downfrom 720 by 20
           for row from 1
           do (multiple-value-bind (font size grey spacing)
                  (if (= row 4) (values font size grey spacing) (values "F1" 10 0 0))
                (format s "BT /F1 10 Tf 72 ~d Td (~a) Tj ET BT /F1 10 Tf ~d ~d Td (~a) Tj ET ~
                           q ~a g BT /~a ~a Tf ~a Tc ~,2f ~d Td (~a) Tj ET Q~%"
                        y label (- 300 (* 5 (length a))) y a grey font size spacing
                        (- 400 (* 1/2 size (length b)) (* spacing (1- (length b)))) y b)))
     (when total
       (format s "BT /F3 10 Tf 72 620 Td (Total) Tj ET BT /F3 10 Tf 255 620 Td (23,577.65) Tj ET ~
                  BT /F3 10 Tf 355 620 Td (22,629.35) Tj ET~%")))
   :fonts "/F1 7 0 R /F2 8 0 R /F3 9 0 R"
   :more (list (pal-width-font 7 "ABCDEF+KeaSans") (pal-width-font 8 "GHIJKL+KeaSans")
               (pal-width-font 9 "ABCDEF+KeaSans-Bold"))))

(define-test (palimpsest palimpsest-table-cell-set-apart)
  ;; a cell in another font object, size or ink than its column and its row: named, with both settings; one in
  ;; another font at its column's size, set to pass for the column's own, in words of its own
  (loop for (odd detail words)
          in '(((:font "F2") "\"1,180.75\" in GHIJKL+KeaSans (object 8), the rest of its column in ABCDEF+KeaSans"
                "is set in another font than the rest of its column and its row, at the column's size")
               ((:size 10.4) "\"1,180.75\" in 10.4 pt, the rest of its column in 10.0 pt"
                "1 figure of a table is set in another font, size or ink")
               ((:grey 0.25) "\"1,180.75\" in grey 0.25, the rest of its column in grey 0.00"
                "1 figure of a table is set in another font, size or ink"))
        do (multiple-value-bind (pdf accounts findings) (pal-read (apply #'pal-table odd))
             (declare (ignore pdf accounts))
             (let ((cell (pal-finding findings :look words)))
               (true cell "~a: ~s" odd (mapcar #'palimpsest::finding-words findings))
               (when cell (pal-has (first (palimpsest::finding-details cell)) detail)))))
  ;; a cell in another font with its characters spaced apart, as a font's table of glyphs sets them: nothing said
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-table :font "F2" :spacing 1.5))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "of a table") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; the table set alike, with a total row in bold: nothing said
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-table :total t))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "of a table") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-annotations-compared)
  ;; a later save that only lays annotations over the page, its content untouched: the page set beside the
  ;; one before shows what was laid over it, as it shows text
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save (pal-original)
                          '((3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] "
                                                        "/Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R "
                                                        "/Annots [14 0 R 15 0 R 16 0 R] >>"))
                            (14 . "<< /Type /Annot /Subtype /Square /Rect [70 675 230 693] /C [1 1 1] /IC
[1 1 1] /F 4 >>")
                            (15 . "<< /Type /Annot /Subtype /FreeText /Rect [72 677 230 693] /Contents
(Total: $3,250.00) /DA (/Helv 12 Tf 0 g) /F 4 >>")
                            (16 . "<< /Type /Annot /Subtype /Stamp /Rect [380 700 520 760] /Name /Approved
/Contents (APPROVED) /F 4 >>"))))
    (declare (ignore findings))
    (let ((change (first (palimpsest::account-changes (second accounts)))))
      (true change)
      (when change
        (is equal '((:added "Square, filled white at 70,675 to 230,693")
                    (:added "FreeText \"Total: $3,250.00\" at 72,677 to 230,693")
                    (:added "Stamp Approved \"APPROVED\" at 380,700 to 520,760"))
            (palimpsest::page-change-annotations change))
        (false (palimpsest::page-change-text change))))
    (true pdf)))

(defun pal-scan (&key pasted lowered (tilt 0))
  "The grey octets of a scanned page, 600 by 500 pixels: 12 lines of 6 words, each word strokes 3 pixels
wide whose core is grey 60 and whose edges fade through 140, on paper of grey 235 straying by up to 10.
With PASTED, the third word of the fifth line is crisp, grey 0 with no fading, on clean paper of grey
250. With LOWERED, that word is 4 pixels lower than its line. TILT is the page's slope, its rows falling
that many pixels for each pixel across."
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8))) (seed 12345))
    (dotimes (i (length grey))
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (+ 225 (mod (ash seed -16) 21))))
    (dotimes (line 12)
      (dotimes (word 6)
        (let* ((top (+ 20 (* line 38))) (left (+ 20 (* word 95)))
               (crisp (and pasted (= line 4) (= word 2))))
          ;; the pasted word's clean paper goes with it where it is laid
          (when crisp
            (loop for y from (- top 3) below (+ top 21 (if lowered 4 0))
                  do (loop for x from (- left 3) below (+ left 73) do (setf (aref grey (+ (* y 600) x)) 250))))
          (dotimes (stroke 9)
            (let ((x0 (+ left (* stroke 8))))
              (loop for y from (+ top (if (and lowered (= line 4) (= word 2)) 4 0) (round (* tilt x0)))
                      repeat 18
                    do (loop for dx from -1 to 3
                             do (setf (aref grey (+ (* y 600) x0 dx))
                                      (cond (crisp (if (<= 0 dx 2) 0 250))
                                            ((<= 0 dx 2) 60)
                                            (t 140))))))))))
    grey))

(defun pal-scanned (grey &key (width 600) (height 500) (points (list width height)))
  "A one-page PDF of the scanned page GREY, WIDTH by HEIGHT pixels of raw grey laid over all of the page, the page
POINTS (WIDE HIGH) large: a pixel to a point unless said otherwise."
  (destructuring-bind (wide high) points
    (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                        '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                        (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d ~d] /Resources << ~
                                             /XObject << /Im1 7 0 R >> >> /Contents 4 0 R >>" wide high))
                        (list 4 :stream "" (format nil "q ~d 0 0 ~d 0 0 cm /Im1 Do Q" wide high))
                        '(6 . "<< /Producer (Kea Scan) >>")
                        (list 7
                              :stream (format nil
                                              "/Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace ~
                                               /DeviceGray /BitsPerComponent 8" width height) grey)))))

;;; The checks and the report's Checks

(defun pal-checked (octets)
  "OCTETS read as a PDF and examined, as two values: its findings and each check's outcome (ALL-FINDINGS)."
  (let* ((pdf (palimpsest::read-pdf octets :name "test.pdf"))
         (accounts (loop for r across (palimpsest::pdf-revisions pdf) collect (palimpsest::compare-revision pdf r))))
    (palimpsest::all-findings pdf accounts)))

(defun pal-check-cell (findings outcomes key)
  "The cell of the report's Checks for the check of KEY, given FINDINGS and OUTCOMES (PAL-CHECKED)."
  (palimpsest::check-summary (remove key findings :key #'palimpsest::finding-check :test-not #'eq)
                             (cdr (assoc key outcomes))))

(define-test (palimpsest palimpsest-checks-each-once)
  ;; every check has a label and an explanation; a PDF is checked by each check run on PDFs once, in order, and a
  ;; picture by each check run on pictures; every finding names a check that ran
  (true palimpsest::*checks*)
  (dolist (check palimpsest::*checks*)
    (true (plusp (length (palimpsest::check-label check))))
    (true (< 200 (length (palimpsest::check-explanation check))) "~s" (palimpsest::check-key check)))
  (is = (length palimpsest::*checks*)
      (length (remove-duplicates (mapcar #'palimpsest::check-key palimpsest::*checks*))))
  (multiple-value-bind (findings outcomes) (pal-checked (pal-edited))
    (is equal (loop for check in palimpsest::*checks*
                    when (member :pdf (palimpsest::check-on check)) collect (palimpsest::check-key check))
        (mapcar #'car outcomes))
    (dolist (f findings)
      (true (assoc (palimpsest::finding-check f) outcomes) "~a" (palimpsest::finding-words f))))
  (let* ((file (palimpsest::make-picture-file :kind :png :width 600 :height 500 :grey (pal-scan)))
         (palimpsest::*picture-file* file)
         (outcomes (nth-value 1 (palimpsest::all-findings
                                 (palimpsest::read-pdf (palimpsest::picture-page-octets 600 500) :name "picture")
                                 '() :file file))))
    (is equal (loop for check in palimpsest::*checks*
                    when (member :picture (palimpsest::check-on check)) collect (palimpsest::check-key check))
        (mapcar #'car outcomes))
    (is equal "not a JPEG" (cdr (assoc :saved-twice outcomes)))
    (false (cdr (assoc :copied-characters outcomes)))))

(define-test (palimpsest palimpsest-checks-cells)
  ;; a born-digital PDF: the scan checks not run, with why; a check that ran clean says none; the later save is
  ;; under the check of saves. A scan: its checks run, the fonts not, as it has no text layer
  (multiple-value-bind (findings outcomes) (pal-checked (pal-edited))
    (is string= "not run: no scanned pages" (pal-check-cell findings outcomes :copied-characters))
    (is string= "not run: no scanned pages" (pal-check-cell findings outcomes :resolution))
    (is string= "none" (pal-check-cell findings outcomes :encryption))
    (let ((save (pal-finding findings :record "changed after it was first saved")))
      (true save)
      (is eq :saves (palimpsest::finding-check save)))
    (pal-has (pal-check-cell findings outcomes :saves) "Look: "))
  (multiple-value-bind (findings outcomes) (pal-checked (pal-scanned (pal-scan :pasted t)))
    (is string= "not run: no text layer" (pal-check-cell findings outcomes :fonts))
    (false (cdr (assoc :unlike-words outcomes)))
    (let ((pasted (pal-finding findings :look "has 1 word unlike the rest of it")))
      (true pasted)
      (is eq :unlike-words (palimpsest::finding-check pasted)))
    (is string= "Look: 1 finding, page 1" (pal-check-cell findings outcomes :unlike-words))))

(define-test (palimpsest palimpsest-checks-tallies)
  ;; a cell sums what its findings count and gives their pages as ranges; a resolution is given as it is
  (flet ((cell (&rest findings) (palimpsest::check-summary findings nil)))
    (is string= "Strong: 59 characters, pages 1-2"
        (cell (palimpsest::found :strong "a" :page 1 :tally (list 50 "character" (list 1 2)))
              (palimpsest::found :strong "b" :page 2 :tally (list 9 "character"))
              (palimpsest::found :look "c" :page 5)))
    (is string= "Look: 3 patches, 1 finding, pages 1 and 4"
        (cell (palimpsest::found :look "a" :page 1 :tally (list 3 "patch"))
              (palimpsest::found :look "b" :page 4)))
    (is string= "about 150 dpi, pages 1-3; about 300 dpi, page 5"
        (cell (palimpsest::found :record "a" :page 1 :tally (list 150 :dpi (list 1 2 3)))
              (palimpsest::found :record "b" :page 5 :tally (list 300 :dpi (list 5)))))
    (is string= "pages 1-3, 5 and 7-8" (palimpsest::pages-words '(1 2 3 5 7 8) :ranges t))))

(define-test (palimpsest palimpsest-checks-unfinished)
  ;; a check failing on a file reads could not finish, its finding under it; the parts of a check that did not
  ;; fail keep their findings
  (let ((palimpsest::*checks* '()))
    (palimpsest::define-check :broken "Broken on purpose"
      "A check that fails."
      (:on (:pdf))
      (list (palimpsest::found :look "Found before the failure."))
      (error "malformed"))
    (multiple-value-bind (findings outcomes) (pal-checked (pal-edited))
      (is eq :unfinished (cdr (assoc :broken outcomes)))
      (true (pal-finding findings :look "Found before the failure."))
      (true (pal-finding findings :record "The check of broken on purpose could not finish"))
      (is string= "could not finish" (pal-check-cell findings outcomes :broken)))))

(define-test (palimpsest palimpsest-checks-table)
  ;; the report's Checks lists every check once, under its heading; a check's label opens its explanation in
  ;; help, filled to the help column
  (multiple-value-bind (findings outcomes) (pal-checked (pal-edited))
    (let* ((pdf (palimpsest::read-pdf (pal-edited) :name "test.pdf"))
           (analysis (palimpsest::make-analysis :name "test.pdf" :pdf pdf :findings findings :checks outcomes
                                                :size (length (pal-edited)) :sha256 "x"
                                                :accounts (loop for r across (palimpsest::pdf-revisions pdf)
                                                                collect (palimpsest::compare-revision pdf r))))
           (pieces (palimpsest::report-pieces analysis))
           (text (palimpsest::pieces-text pieces))
           (table (subseq text (search "* Checks" text) (search "* Findings" text))))
      (pal-has table "Check")
      (dolist (check palimpsest::*checks*)
        (when (member :pdf (palimpsest::check-on check))
          (is = 1 (count-if (lambda (line) (search (palimpsest::check-label check) line))
                            (uiop:split-string table :separator '(#\Newline)))
              "~a" (palimpsest::check-label check))))
      (pal-has table "not run: no scanned pages")
      (let* ((check (palimpsest::find-check :hidden-saves))
             (link (find-if (lambda (p) (and (consp p) (equal (first p) (palimpsest::check-label check))
                                             (consp (cdr p)) (equal (third p) '(:check :hidden-saves))))
                            pieces))
             ;; the button as the editor makes it of what was sent back
             (action (palimpsest::report-action analysis (list "check" "hidden-saves"))))
        (true link)
        (true (functionp action))
        (run-keys (list action) :text "" :width 120)
        (let* ((help (tuatara.buffer:buffer-string (tuatara.buffer:find-buffer "*Help*")))
               (lines (uiop:split-string help :separator '(#\Newline))))
          (pal-has help "Saves hidden in streams")
          (pal-has help "swallow it")
          (true (every (lambda (line) (<= (length line) tuatara.defaults::*help-column*)) lines)))))))

(define-test (palimpsest palimpsest-scanned-word-pasted)
  ;; a word laid crisp and black on clean paper over a scan is named, with its place on the page; the scan untouched,
  ;; nothing
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :pasted t)))
    (declare (ignore pdf accounts))
    (let ((pasted (pal-finding findings :look "has 1 word unlike the rest of it")))
      (true pasted "~s" (mapcar #'palimpsest::finding-words findings))
      (when pasted
        (pal-has (palimpsest::finding-words pasted) "darker ink")
        (pal-has (palimpsest::finding-words pasted) "cleaner paper")
        (pal-has (first (palimpsest::finding-details pasted)) "pixels 210,172 to")
        (pal-has (first (palimpsest::finding-details pasted)) "ink grey 0, the words beside it 60"))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))))

(defun pal-strips (grey heights &key (width 600) (gap 0) (first 7) widths spaces bits decodes datas mirrored
                                      flipped patch (top-extra 0) (top-shift 0) clip-page (filter ""))
  "A one-page PDF of the scanned page GREY, WIDTH pixels wide and as many high as HEIGHTS add up to, kept as
pictures of raw grey one under the next, a strip each of HEIGHTS rows, the top strip object FIRST and the
next ones after it, laid over all of the page a pixel to a point. GAP points are left between each strip
and the next. WIDTHS, SPACES, BITS, DECODES and DATAS give each strip's /Width, /ColorSpace,
/BitsPerComponent, /Decode and octets where they are not NIL (else WIDTH, /DeviceGray, 8, none and its
rows of GREY), and FILTER is written in each strip's dictionary. MIRRORED, a list of strips (from 0) drawn
mirrored across. With FLIPPED every strip is drawn flipped down, the page shown upside down: GREY's first
row at the foot of the page. PATCH, (K OCTETS DY HIDDEN TIMES), is a picture of strip K's kind and size drawn
over it last, TIMES times (once when NIL), DY points higher; HIDDEN :CLIP draws it clipped to nothing, :LAYER
in a layer that is off, :ALPHA wholly transparent, :LINE clipped to a line across the page, of no area.
TOP-EXTRA points are added to the height the top strip is drawn at, its foot where it was, and TOP-SHIFT to
where it is drawn. With CLIP-PAGE everything is clipped to the page."
  (let* ((n (length heights)) (high (+ (reduce #'+ heights) (* gap (1- n))))
         (tops (loop for h in heights for top = 0 then (+ top previous) for previous = h collect top))
         (place (lambda (k h top)
                  ;; the matrix that draws strip K, H rows high, its first row TOP rows from GREY's first
                  (let ((y (if flipped (+ top (* k gap)) (- high top (* k gap) h))))
                    (if flipped
                        (format nil "~d 0 0 ~d 0 ~,3f" width (- h) (+ y h))
                        (if (member k mirrored)
                            (format nil "~d 0 0 ~d ~d ~,3f" (- width) h width y)
                            (format nil "~d 0 0 ~,3f 0 ~,3f" width (if (zerop k) (+ h top-extra) h)
                                    (if (zerop k) (+ y top-shift) y)))))))
         (dict (lambda (k h)
                 (format nil "/Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace ~a /BitsPerComponent ~
                              ~d~@[ /Decode ~a~] ~a"
                         (or (nth k widths) width) h (or (nth k spaces) "/DeviceGray") (or (nth k bits) 8)
                         (nth k decodes) filter)))
         (patched (and patch (+ first n)))
         (clip (format nil "0 0 ~d ~,3f re W n" width high))
         (hide (lambda (hidden drawn)
                 ;; DRAWN, the operators drawing the patch, drawn as HIDDEN says
                 (case hidden
                   (:clip (format nil "q 0 0 0 0 re W n ~a Q" drawn))
                   (:layer (format nil "/OC /L1 BDC q ~a Q EMC" drawn))
                   (:alpha (format nil "q /G0 gs ~a Q" drawn))
                   (:line (format nil "q 0 0 m ~d ~,3f l W n ~a Q" width high drawn))
                   (t (format nil "q ~a Q" drawn))))))
    (pal-save #() (append (list (if (eq (fourth patch) :layer)
                                    '(1 . #.(concatenate 'string "<< /Type /Catalog /Pages 2 0 R /OCProperties << "
                                                         "/OCGs [90 0 R] /D << /OFF [90 0 R] >> >> >>"))
                                    '(1 . "<< /Type /Catalog /Pages 2 0 R >>"))
                                '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d ~,3f] /Resources << ~
                                                     /XObject << ~{/Im~d ~d 0 R~^ ~} >>~:[~; /Properties << /L1 90 0 R ~
                                                     >>~]~:[~; /ExtGState << /G0 << /ca 0 >> >>~] >> /Contents 4 0 R >>"
                                                width high (loop for k below (if patch (1+ n) n)
                                                                 append (list (1+ k) (+ first k)))
                                                (eq (fourth patch) :layer) (eq (fourth patch) :alpha)))
                                (list 4 :stream ""
                                      (format nil "~{~a~^ ~}"
                                              (append (and clip-page (list clip))
                                                      (loop for h in heights for k from 0 for top in tops
                                                            collect (format nil "q ~a cm /Im~d Do Q"
                                                                            (funcall place k h top) (1+ k)))
                                                      (and patch
                                                           (destructuring-bind (k octets dy &optional hidden (times 1))
                                                               patch
                                                             (declare (ignore octets))
                                                             (let* ((h (nth k heights))
                                                                    (drawn (format nil "~d 0 0 ~d 0 ~,3f cm /Im~d Do"
                                                                                   width h
                                                                                   (+ (- high (nth k tops) (* k gap) h)
                                                                                      dy)
                                                                                   (1+ n))))
                                                               (loop repeat times
                                                                     collect (funcall hide hidden drawn))))))))
                                '(6 . "<< /Producer (Kea Scan) >>"))
                          (and (eq (fourth patch) :layer) (list '(90 . "<< /Type /OCG /Name (Hidden) >>")))
                          (loop for h in heights for k from 0 for top in tops
                                collect (list (+ first k) :stream (funcall dict k h)
                                              (or (nth k datas) (subseq grey (* top width) (* (+ top h) width)))))
                          (and patch (list (list patched :stream (funcall dict (first patch) (nth (first patch) heights))
                                                 (second patch))))))))

(defun pal-strips-joined (octets)
  "How many strips each scanned picture of the PDF in OCTETS is joined from, 0 for a picture of one piece, those
not to be joined left out."
  (let ((pdf (palimpsest::read-pdf octets :name "test.pdf")))
    (loop for (m) in (palimpsest::scan-marks pdf (palimpsest::last-revision pdf))
          unless (let ((refused (find-symbol "MARK-REFUSED" :palimpsest)))
                   ;; looked up by name, that the counts can be told on code that never refuses a run
                   (and refused (fboundp refused) (funcall refused m)))
            collect (length (palimpsest::mark-strips m)))))

(define-test (palimpsest palimpsest-strips-joined)
  ;; a page kept as narrow strips one under the next is read as one picture: the word laid on it is found as on
  ;; the same page kept whole, named in the strip that holds it, by that strip's own rows and object
  (let ((whole (nth-value 2 (pal-read (pal-scanned (pal-scan :pasted t)))))
        (strips (nth-value 2 (pal-read (pal-strips (pal-scan :pasted t) '(64 64 64 64 64 64 64 52) :first 20)))))
    (true (pal-finding whole :look "has 1 word unlike the rest of it") "~s"
          (mapcar #'palimpsest::finding-words whole))
    (is equal (mapcar #'palimpsest::finding-strength whole) (mapcar #'palimpsest::finding-strength strips))
    (let ((odd (pal-finding strips :look "the scanned page joined from 8 strips")))
      (true odd "~s" (mapcar #'palimpsest::finding-words strips))
      (when odd
        (pal-has (first (palimpsest::finding-details odd)) "pixels 210,44 to")
        (pal-has (first (palimpsest::finding-details odd)) "of picture 22, rows 172 to")
        (is equal '(22) (palimpsest::finding-objects odd))))))

(define-test (palimpsest palimpsest-strips-not-joined)
  ;; strips apart on the page, of another width, colour space, depth or decoding, or one mirrored, are not
  ;; joined to the rest; strips of a JPEG are joined only when their blocks lie on one grid, else named
  (let ((grey (pal-scan)) (heights '(64 64 64 64 64 64 64 52)))
    (is equal '(8) (pal-strips-joined (pal-strips grey heights)))
    (is equal '() (pal-strips-joined (pal-strips grey heights :gap 2)))
    ;; the strip of another kind parts the strips above it from those below: each run joined on its own
    (is equal '(3 4) (pal-strips-joined (pal-strips grey heights :widths '(nil nil nil 599))))
    (is equal '(3 4) (pal-strips-joined (pal-strips grey heights :spaces '(nil nil nil "/DeviceRGB"))))
    (is equal '(3 4) (pal-strips-joined (pal-strips grey heights :bits '(nil nil nil 4))))
    (is equal '(3 4) (pal-strips-joined (pal-strips grey heights :decodes '(nil nil nil "[1 0]"))))
    (is equal '(3 4) (pal-strips-joined (pal-strips grey heights :mirrored '(3))))
    ;; each strip within half a row of where the strips above it end, measured from the first: the error of
    ;; each does not add up
    (false (member 8 (pal-strips-joined (pal-strips grey heights :gap 0.4))))
    (let* ((pdf (palimpsest::read-pdf (pal-strips grey '(100 100 100 100 100) :filter "/Filter /DCTDecode")
                                      :name "test.pdf"))
           (not-read (nth-value 1 (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf)))))
      (is = 1 (length not-read))
      (pal-has (or (third (first not-read)) "") "blocks of 8 by 8 pixels lie on no one grid"))))

(define-test (palimpsest palimpsest-strips-flipped)
  ;; strips drawn flipped down, the page upside down, read as the one picture drawn so would be: its first
  ;; row the foot of the page
  (let* ((grey (pal-scan))
         (pdf (palimpsest::read-pdf (pal-strips grey '(64 64 64 64 64 64 64 52) :flipped t) :name "test.pdf"))
         (revision (palimpsest::last-revision pdf))
         (mark (first (first (palimpsest::scan-marks pdf revision)))))
    (is = 8 (length (palimpsest::mark-strips mark)))
    (true (equalp grey (palimpsest::mark-grey pdf revision mark)))))

(define-test (palimpsest palimpsest-strips-fall-back)
  ;; strips that each cover 30% of the page and cannot be joined, too many pixels together, are read one by
  ;; one as before; a picture of other octets drawn over a strip, in its very box or a little apart, leaves the
  ;; strips unjoined and the page named as not read; one hidden, clipped to nothing or in a layer that is off,
  ;; is passed over and the strip the page shows is read
  (let ((palimpsest::*palimpsest-scan-pixels* 200000))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-strips (pal-scan :pasted t) '(250 250)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "has 1 word unlike the rest of it") "~s"
            (mapcar #'palimpsest::finding-words findings))))
  (let* ((heights '(64 64 64 64 64 64 64 52))
         (original (subseq (pal-scan) (* 128 600) (* 192 600)))
         (pasted (subseq (pal-scan :pasted t) (* 128 600) (* 192 600))))
    (flet ((not-read (octets)
             (let ((pdf (palimpsest::read-pdf octets :name "test.pdf")))
               (nth-value 1 (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf))))))
      (let ((not-read (not-read (pal-strips (pal-scan) heights :patch (list 2 pasted 0)))))
        (is = 1 (length not-read))
        (pal-has (or (third (first not-read)) "") "2 pictures of other octets are drawn in the box of its strip 3"))
      (let ((not-read (not-read (pal-strips (pal-scan) heights :patch (list 2 pasted 0.2)))))
        (is = 1 (length not-read))
        (pal-has (or (third (first not-read)) "") "2 pictures meet the foot of its strip 2")))
    (dolist (hidden '(:clip :layer))
      (multiple-value-bind (pdf accounts findings)
          (pal-read (pal-strips (pal-scan) heights :datas (list nil nil pasted) :patch (list 2 original 0 hidden)))
        (declare (ignore pdf accounts))
        (true (pal-finding findings :look "has 1 word unlike the rest of it") "~a: ~s" hidden
              (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-strips-hidden-twice)
  ;; a forged strip with the original drawn twice over it unseen: wholly transparent, passed over and the forged
  ;; strip read; clipped to a line of no area, the run not joined and the page named; a top strip pushed past the
  ;; page's clip is not passed over but leaves the run unjoined, named
  (let* ((heights '(64 64 64 64 64 64 64 52))
         (original (subseq (pal-scan) (* 128 600) (* 192 600)))
         (pasted (subseq (pal-scan :pasted t) (* 128 600) (* 192 600))))
    (flet ((not-read (octets)
             (let ((pdf (palimpsest::read-pdf octets :name "test.pdf")))
               (nth-value 1 (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf))))))
      (multiple-value-bind (pdf accounts findings)
          (pal-read (pal-strips (pal-scan) heights :datas (list nil nil pasted) :patch (list 2 original 0 :alpha 2)))
        (declare (ignore pdf accounts))
        (true (pal-finding findings :look "has 1 word unlike the rest of it") "~s"
              (mapcar #'palimpsest::finding-words findings)))
      (let ((not-read (not-read (pal-strips (pal-scan) heights :datas (list nil nil pasted)
                                                               :patch (list 2 original 0 :line 2)))))
        (is = 1 (length not-read))
        (pal-has (or (third (first not-read)) "") "2 pictures of other octets are drawn in the box of its strip 3"))
      (let ((not-read (not-read (pal-strips (pal-scan) heights :clip-page t :top-shift 1))))
        (is = 1 (length not-read))
        (pal-has (or (third (first not-read)) "") "its strip 1 reaches past what it is clipped to")))))

(define-test (palimpsest palimpsest-strips-placed)
  ;; a top strip drawn a little taller by rounding, the rest placed true, is joined however many strips follow;
  ;; strips whose gaps add up past half a row are not joined, and the page is named as not read
  (let ((grey (pal-scan)))
    (is equal '(100) (pal-strips-joined (pal-strips grey (loop repeat 100 collect 5) :top-extra 0.02)))
    (let* ((pdf (palimpsest::read-pdf (pal-strips grey '(64 64 64 64 64 64 64 52) :gap 0.4) :name "test.pdf"))
           (not-read (nth-value 1 (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf)))))
      (is = 1 (length not-read))
      (pal-has (or (third (first not-read)) "") "points from where the strips above it end"))))

(define-test (palimpsest palimpsest-strips-run-stoppable)
  ;; thousands of strips of a row each, each in a box of its own, make one run, joined with checks for a stop
  ;; along it
  (let* ((count 3000)
         (pdf (palimpsest::read-pdf (pal-strips (make-array (* 600 count) :element-type '(unsigned-byte 8)
                                                                          :initial-element 200)
                                                (loop repeat count collect 1))
                                    :name "test.pdf"))
         (revision (palimpsest::last-revision pdf))
         (pictures (loop for m across (palimpsest::marks-of pdf revision (first (palimpsest::pages-of pdf revision)))
                         when (eq (palimpsest::mark-kind m) :image) collect m))
         (within nil) (stops 0))
    (sb-int:encapsulate 'palimpsest::strip-run 'palimpsest-strips-within
                        (lambda (function &rest arguments)
                          (setf within t)
                          (unwind-protect (apply function arguments) (setf within nil))))
    (sb-int:encapsulate 'tuatara.runtime:job-check-stop 'palimpsest-strips-stops
                        (lambda (function &rest arguments)
                          (when within (incf stops))
                          (apply function arguments)))
    (unwind-protect
         (is equal (list count) (mapcar (lambda (m) (length (palimpsest::mark-strips m)))
                                        (remove-if-not #'palimpsest::mark-strips
                                                       (palimpsest::joined-strips pdf revision pictures))))
      (sb-int:unencapsulate 'palimpsest::strip-run 'palimpsest-strips-within)
      (sb-int:unencapsulate 'tuatara.runtime:job-check-stop 'palimpsest-strips-stops))
    (true (plusp stops))))

(define-test (palimpsest palimpsest-strip-tables-tie)
  ;; two JPEG strips of other tables: the second, in the order of the page's rows, is the one named
  (flet ((jpeg (scale)
           (coerce (append '(#xff #xd8 #xff #xdb 0 67 0)
                           (loop for s across palimpsest::*jpeg-luminance-table*
                                 collect (max 1 (floor (+ (* s scale) 50) 100)))
                           '(#xff #xda 0 2))
                   '(simple-array (unsigned-byte 8) (*)))))
    (dolist (scales '((100 20) (20 100)))
      (let* ((pdf (palimpsest::read-pdf (pal-strips (pal-scan) '(256 244) :filter "/Filter /DCTDecode"
                                                    :datas (mapcar #'jpeg scales))
                                        :name "test.pdf"))
             (found (palimpsest::strip-table-findings pdf (palimpsest::last-revision pdf))))
        (is equal '(8) (and found (palimpsest::finding-objects (first found))) "~a" scales)))))

(define-test (palimpsest palimpsest-strips-bounded)
  ;; thousands of strips stacked in one box are joined in time that grows with their number, and the join can
  ;; be stopped; more JPEG strips than a page is decoded for are not joined, none decoded, and named
  (flet ((stacked (count)
           ;; the pictures of a page drawing one strip of a row COUNT times in one box, and COUNT a row lower
           (let* ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                              '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                              '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 "
                                                                           "600 500] /Resources << /XObject << /Im1 7 "
                                                                           "0 R >> >> /Contents 4 0 R >>"))
                                              (list 4 :stream ""
                                                    (format nil "~{~a~^ ~}"
                                                            (loop for k below (* 2 count)
                                                                  collect (if (< k count)
                                                                              "q 600 0 0 1 0 100 cm /Im1 Do Q"
                                                                              "q 600 0 0 1 0 99 cm /Im1 Do Q"))))
                                              (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 600 ~
                                                                           /Height 1 /ColorSpace /DeviceGray ~
                                                                           /BitsPerComponent 8")
                                                    (make-array 600 :element-type '(unsigned-byte 8)
                                                                    :initial-element 200)))))
                  (pdf (palimpsest::read-pdf octets :name "test.pdf"))
                  (revision (palimpsest::last-revision pdf))
                  (page (first (palimpsest::pages-of pdf revision))))
             (values pdf revision (loop for m across (palimpsest::marks-of pdf revision page)
                                        when (eq (palimpsest::mark-kind m) :image) collect m)))))
    (flet ((cost (count)
             (multiple-value-bind (pdf revision pictures) (stacked count)
               (loop repeat 3
                     minimize (let ((start (pal-thread-cpu-time)))
                                (palimpsest::joined-strips pdf revision pictures)
                                (- (pal-thread-cpu-time) start))))))
      (let ((small (cost 1000)) (large (cost 4000)))
        (true (< large (* 8 (max small 1))) "1,000: ~:d ns, 4,000: ~:d ns" small large)))
    (multiple-value-bind (pdf revision pictures) (stacked 2000)
      (let ((stops 0))
        (sb-int:encapsulate 'tuatara.runtime:job-check-stop 'palimpsest-strips-stop
                            (lambda (function &rest arguments)
                              (incf stops)
                              (apply function arguments)))
        (unwind-protect (palimpsest::joined-strips pdf revision pictures)
          (sb-int:unencapsulate 'tuatara.runtime:job-check-stop 'palimpsest-strips-stop))
        (true (plusp stops)))))
  (let ((decoded 0))
    (sb-int:encapsulate 'palimpsest::picture-grey 'palimpsest-strips-decoded
                        (lambda (function &rest arguments)
                          (declare (ignore function arguments))
                          (incf decoded)
                          (values nil "not decoded in this test")))
    (unwind-protect
         (let* ((heights (loop repeat 200 collect 8))
                (pdf (palimpsest::read-pdf (pal-strips (make-array (* 600 1600) :element-type '(unsigned-byte 8)
                                                                                :initial-element 200)
                                                       heights :filter "/Filter /DCTDecode")
                                           :name "test.pdf"))
                (not-read (nth-value 1 (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf)))))
           (is = 0 decoded)
           (is = 1 (length not-read))
           (pal-has (or (third (first not-read)) "") "the 128 JPEG strips a page is turned into pixels for"))
      (sb-int:unencapsulate 'palimpsest::picture-grey 'palimpsest-strips-decoded))))

(define-test (palimpsest palimpsest-strips-evidence-and-tables)
  ;; a finding on a page of a hundred strips gives the evidence of the strips that hold it, not of all; JPEG
  ;; strips saved with other tables than the rest are named, worth a look
  (let* ((whole (pal-finding (nth-value 2 (pal-read (pal-scanned (pal-scan :pasted t)))) :look "unlike the rest"))
         (strips (pal-finding (nth-value 2 (pal-read (pal-strips (pal-scan :pasted t) (loop repeat 100 collect 5))))
                              :look "unlike the rest")))
    (true (and whole strips))
    (when (and whole strips)
      (true (<= (length (palimpsest::finding-evidence strips)) (* 6 (length (palimpsest::finding-evidence whole))))
            "~d lines" (length (palimpsest::finding-evidence strips)))))
  (flet ((jpeg (scale)
           (coerce (append '(#xff #xd8 #xff #xdb 0 67 0)
                           (loop for s across palimpsest::*jpeg-luminance-table*
                                 collect (max 1 (floor (+ (* s scale) 50) 100)))
                           '(#xff #xda 0 2))
                   '(simple-array (unsigned-byte 8) (*)))))
    (let* ((heights '(64 64 64 64 64 64 64 52))
           (pdf (palimpsest::read-pdf (pal-strips (pal-scan) heights :filter "/Filter /DCTDecode"
                                                  :datas (loop for k below 8 collect (jpeg (if (= k 3) 100 20))))
                                      :name "test.pdf"))
           (found (palimpsest::strip-table-findings pdf (palimpsest::last-revision pdf))))
      (is = 1 (length found))
      (when found
        (is eq :look (palimpsest::finding-strength (first found)))
        (pal-has (palimpsest::finding-words (first found)) "1 of the 8 JPEG strips")
        (is equal '(10) (palimpsest::finding-objects (first found)))))))

(defun pal-grained (grey &optional (seed 777))
  "GREY, a scanned page (PAL-SCAN), with grain over all of it, ink too: each
pixel straying by up to 12, as no two prints of a stroke are alike; another
SEED, another grain."
  (dotimes (i (length grey) grey)
    (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
    (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) -12 (mod (ash seed -16) 25)))))))

(define-test (palimpsest palimpsest-scanned-character-copied)
  ;; a stroke of a grained scan cut out with its paper and laid over another: the two named, a strong sign
  (let* ((grey (pal-grained (pal-scan)))
         (from-x (+ 20 95 -2)) (from-y (+ 20 38 -2))
         (to-x (+ 20 (* 2 95) (* 3 8) -2)) (to-y (+ 20 (* 4 38) -2)))
    (loop for y below 22
          do (loop for x below 8
                   do (setf (aref grey (+ (* (+ to-y y) 600) to-x x)) (aref grey (+ (* (+ from-y y) 600) from-x x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (let ((copies (pal-finding findings :strong "2 characters of the scanned page are copies of one another")))
        (true copies "~s" (mapcar #'palimpsest::finding-words findings))
        (when copies
          (pal-has (first (palimpsest::finding-details copies)) "pixels 114,58 to 119,76 of picture 7")
          (pal-has (first (palimpsest::finding-details copies)) "pixels 233,172 to 238,190 of picture 7")))))
  ;; laid with a Darken blend over paper cut from elsewhere: its ink its source's, the paper round it not
  (let* ((grey (pal-grained (pal-scan)))
         (from-x (+ 20 95 -2)) (from-y (+ 20 38 -2))
         (to-x (+ 20 (* 2 95) (* 3 8) -2)) (to-y (+ 20 (* 4 38) -2)))
    (loop for y below 22
          do (loop for x below 8
                   do (setf (aref grey (+ (* (+ to-y y) 600) to-x x)) (aref grey (+ (* (+ 470 y) 600) 300 x)))))
    (loop for y below 22
          do (loop for x below 8
                   do (let ((at (+ (* (+ to-y y) 600) to-x x)))
                        (setf (aref grey at) (min (aref grey at) (aref grey (+ (* (+ from-y y) 600) from-x x)))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; the same scan untouched: every stroke alike in shape, none in its grain
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-scan))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings)))
  ;; the characters keep cuts of their own, not the page: 648 strokes, 22 by 9 pixels each, of a page of 300,000
  (let* ((reading (palimpsest::picture-words (pal-grained (pal-scan)) 600 500))
         (words (palimpsest::picture-reading-words reading)) (tilt (palimpsest::picture-reading-tilt reading))
         (glyphs (palimpsest::picture-reading-glyphs reading)))
    (declare (ignore words tilt))
    (is = 648 (length glyphs))
    (is = (* 648 22 9) (reduce #'+ glyphs :key (lambda (g) (length (palimpsest::glyph-pixels g)))))))

(defun pal-copy-box (from to fx fy tx ty &optional jitter)
  "The 5 by 18 pixels of the scanned page FROM (PAL-SCAN) at FX FY, a
stroke's box, laid over TO at TX TY, pixel for pixel; with JITTER, each
grey level moved by up to 4, as saving again as a JPEG moves them. TO."
  (dotimes (y 18 to)
    (dotimes (x 5)
      (let ((g (aref from (+ (* (+ fy y) 600) fx x))))
        (setf (aref to (+ (* (+ ty y) 600) tx x))
              (if jitter (max 0 (min 255 (+ g (- (mod (* 7 (+ (* 5 y) x)) 9) 4)))) g))))))

(define-test (palimpsest palimpsest-exact-copies-hashed)
  ;; characters of the same pixels found at once, a difference of 0, even five pages apart, where no
  ;; comparison reaches; a stroke alone copied with its grey levels moved no copy, as strokes alone come
  ;; alike by chance; the other prints of the same stroke no copies, nor a mark of one ink without grain
  ;; printed the same twice
  (let* ((a (pal-grained (pal-scan)))
         (b (pal-grained (pal-scan) 4242)))
    (pal-copy-box a a 114 58 233 172)
    (pal-copy-box a a 19 96 344 286 t)
    (pal-copy-box a b 415 362 550 400)
    ;; a mark of one ink, no grain within it, printed the same on both pages
    (dolist (grey (list a b))
      (dotimes (y 8) (dotimes (x 4) (setf (aref grey (+ (* (+ 255 y) 600) 92 x)) 0))))
    (let ((glyphs (loop for (grey page picture) in (list (list a 0 7) (list b 5 8))
                        append (let ((glyphs (palimpsest::picture-reading-glyphs
                                              (palimpsest::picture-words grey 600 500))))
                                 (dolist (g glyphs glyphs)
                                   (setf (palimpsest::glyph-page g) page (palimpsest::glyph-picture g) picture))))))
      (let* ((reading (palimpsest::copied-glyphs glyphs))
             (copies (palimpsest::scan-reading-copies reading)) (alike (palimpsest::scan-reading-alike reading)))
        (flet ((group (page x y)
                 (find-if (lambda (c)
                            (find-if (lambda (g)
                                       (and (= page (palimpsest::glyph-page g)) (= x (palimpsest::glyph-x0 g))
                                            (= y (palimpsest::glyph-y0 g))))
                                               (first c)))
                          copies))
               (places (c)
                 (sort (mapcar (lambda (g)
                                 (list (palimpsest::glyph-page g)
                                       (palimpsest::glyph-x0 g) (palimpsest::glyph-y0 g))) (first c))
                                 #'< :key #'second)))
          (true (and alike (>= alike 4)) "~s" alike)
          (is = 2 (length copies) "~s" (mapcar #'places copies))
          (is equal '((0 114 58) (0 233 172)) (places (group 0 114 58)))
          (is eql 0 (second (group 0 114 58)))
          (is equal '((0 415 362) (5 550 400)) (places (group 5 550 400)))
          (is eql 0 (second (group 5 550 400)))
          (false (group 0 19 96))
          (false (group 0 92 255))
          (let ((marks (remove-if-not (lambda (g)
                                        (and (= 92 (palimpsest::glyph-x0 g))
                                             (= 255 (palimpsest::glyph-y0 g)))) glyphs)))
            (is = 2 (length marks))
            (true (palimpsest::same-pixels-p (first marks) (second marks)))
            (true (palimpsest::glyph-bare (first marks))))))))
  ;; the same pixels told by every grey level, not by the hash alone: one level apart in each column, no copies
  (let* ((grey (pal-grained (pal-scan)))
         (glyphs (palimpsest::picture-reading-glyphs
                  (palimpsest::picture-words (pal-copy-box grey grey 114 58 233 172) 600 500)))
         (one (find-if (lambda (g) (and (= 114 (palimpsest::glyph-x0 g)) (= 58 (palimpsest::glyph-y0 g)))) glyphs))
         (two (find-if (lambda (g) (and (= 233 (palimpsest::glyph-x0 g)) (= 172 (palimpsest::glyph-y0 g)))) glyphs)))
    (true (palimpsest::same-pixels-p one two))
    (is = (palimpsest::glyph-box-hash one) (palimpsest::glyph-box-hash two))
    (is = 1 (length (palimpsest::exact-copies glyphs)))
    (dotimes (i (palimpsest::glyph-width two))
      (let ((at (+ (* 10 (palimpsest::glyph-width two)) i)))
        (setf (aref (palimpsest::glyph-pixels two) at) (logxor 1 (aref (palimpsest::glyph-pixels two) at)))))
    (false (palimpsest::same-pixels-p one two))
    (is = 0 (length (palimpsest::exact-copies glyphs)))))

(defparameter *pal-jpeg-table*
  #(16 11 10 16 24 40 51 61  12 12 14 19 26 58 60 55  14 13 16 24 40 57 69 56  14 17 22 29 51 87 80 62
    18 22 37 56 68 109 103 77  24 35 55 64 81 104 113 92  49 64 78 87 103 121 120 101  72 92 95 98 112 100 103 99)
  "The luminance table of the JPEG standard, row by row, that PAL-JPEG-SAVED scales.")

(defun pal-jpeg-saved (grey x0 y0 w h &key (quality 75) (width 600) table)
  "GREY (WIDTH wide), with the region from X0 Y0, W by H pixels, saved as a grey JPEG of QUALITY on a grid
of its own and opened again, as a JPEG keeps it. Each block of 8 by 8 from X0 Y0 is turned into its
frequencies, each divided by the standard's table scaled for QUALITY (or by TABLE, 64 steps in the order a
JPEG keeps them, a scanner's own), made whole and multiplied again, and turned back into whole grey levels.
A block past the region's edge is filled out with the region's last pixels. GREY, changed."
  (let ((c (make-array '(8 8))) (scale (if (< quality 50) (floor 5000 quality) (- 200 (* 2 quality))))
        (own (and table (let ((steps (make-array 64)))
                          (loop for k below 64 do (setf (aref steps (aref palimpsest::*zigzag* k)) (elt table k)))
                          steps))))
    (dotimes (u 8)
      (dotimes (x 8) (setf (aref c u x) (* (if (zerop u) (sqrt 1/8) 1/2) (cos (/ (* (+ (* 2 x) 1) u pi) 16))))))
    (loop for by from 0 below h by 8
          do (loop for bx from 0 below w by 8
                   do (let ((block (make-array '(8 8))) (f (make-array '(8 8))))
                        (dotimes (y 8)
                          (dotimes (x 8)
                            (setf (aref block y x)
                                  (- (aref grey
                                           (+ (* (+ y0 (min (+ by y) (1- h))) width) x0 (min (+ bx x) (1- w)))) 128))))
                        (dotimes (v 8)
                          (dotimes (u 8)
                            (let ((q (if own
                                         (aref own (+ (* v 8) u))
                                         (max 1
                                              (min 255
                                                   (floor (+ (* (aref *pal-jpeg-table* (+ (* v 8) u)) scale) 50)
                                                          100))))))
                              (setf (aref f v u)
                                    (* q
                                       (round (loop for y below 8
                                                    sum (loop for x below 8
                                                              sum (* (aref c v y)
                                                                     (aref c u x) (aref block y x)))) q))))))
                        (dotimes (y 8)
                          (dotimes (x 8)
                            (when (and (< (+ by y) h) (< (+ bx x) w))
                              (setf (aref grey (+ (* (+ y0 by y) width) x0 bx x))
                                    (max 0
                                         (min 255
                                              (round (+ 128
                                                        (loop for v below 8
                                                              sum (loop for u below 8
                                                                        sum (* (aref c v y)
                                                                               (aref c u x) (aref f v u))))))))))))))))
  grey)

(defun pal-noisy-scan (&key (sigma 2) (ink-sigma sigma) (paper 235) (core 60) (edge 140) bold (bold-lines 1)
                            (seed 4242))
  "The grey octets of a scanned page as PAL-SCAN draws it, 600 by 500
pixels, with a scanner's grain of SIGMA grey levels (Gaussian, seeded by
SEED) on its paper and INK-SIGMA on its ink; PAPER, CORE and EDGE its
greys; with BOLD, the first BOLD-LINES lines set in four strokes BOLD
pixels wide a word, as a heading is."
  (let ((g (make-array (* 600 500) :initial-element paper)) (random (sb-ext:seed-random-state seed)))
    (dotimes (line 12)
      (dotimes (word 6)
        (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 95))))
          (if (and bold (< line bold-lines))
              (dotimes (k 4)
                (let ((x0 (+ left (* k 18))))
                  (loop for y from top repeat 18
                        do (loop for dx from -1 to bold
                                 do (setf (aref g (+ (* y 600) x0 dx)) (if (or (= dx -1) (= dx bold)) edge core))))))
              (dotimes (stroke 9)
                (let ((x0 (+ left (* stroke 8))))
                  (loop for y from top repeat 18
                        do (loop for dx from -1 to 3
                                 do (setf (aref g (+ (* y 600) x0 dx)) (if (<= 0 dx 2) core edge))))))))))
    (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
      (dotimes (i (length g) out)
        (let ((s (if (< (aref g i) 100) ink-sigma sigma)))
          (setf (aref out i)
                (max 0 (min 255 (round (+ (aref g i)
                                          (if (zerop s) 0
                                              (* s (sqrt (* -2 (log (- 1 (random 1d0 random)))))
                                                 (cos (* 2 pi (random 1d0 random)))))))))))))))

(defun pal-jpeg-page-findings (grey table)
  "Every sign found in GREY, a scanned page 600 by 500 pixels, examined as a JPEG picture file saved with
TABLE (64 steps in the order a JPEG keeps them, PAL-JPEG-SAVED), its pixels as decoded."
  (let* ((file (palimpsest::make-picture-file :kind :jpeg :width 600 :height 500 :grey grey
                                              :tables (list nil (palimpsest::standard-table-quality table) table)))
         (palimpsest::*picture-file* file))
    (values (palimpsest::all-findings (palimpsest::read-pdf (palimpsest::picture-page-octets 600 500) :name "picture")
                                      '() :file file))))

(defun pal-bold-copied (grey)
  "GREY, a scanned page as PAL-NOISY-SCAN draws it with its first six lines BOLD 8, with the first bold stroke
and two pixels of paper round it laid over the third stroke of the third word, 226 pixels across, off a
JPEG's grid. GREY, changed."
  (dotimes (y 22 grey)
    (dotimes (x 14)
      (setf (aref grey (+ (* (+ 18 y) 600) 243 x)) (aref grey (+ (* (+ 18 y) 600) 17 x))))))

(defun pal-near-place-p (detail x y)
  "True when DETAIL names a box of pixels whose first corner lies within a pixel of X, Y."
  (loop for dx from -1 to 1
        thereis (loop for dy from -1 to 1
                      thereis (search (format nil "pixels ~d,~d to" (+ x dx) (+ y dy)) detail))))

(define-test (palimpsest palimpsest-resaved-copy)
  ;; a bold stroke of a grained scan kept as a JPEG copied off the JPEG's grid, with the paper round it, and
  ;; the page saved again with the same steps: no longer alike pixel for pixel, but what that save makes of
  ;; its source laid there, and named so; the page saved twice untouched, nothing
  (let* ((honest (pal-jpeg-saved (pal-noisy-scan :sigma 5 :bold 8 :bold-lines 6) 0 0 600 500 :quality 92))
         (forged (pal-bold-copied (copy-seq honest))))
    (pal-jpeg-saved forged 0 0 600 500 :quality 92)
    (let* ((findings (pal-jpeg-page-findings forged (palimpsest::standard-table 92)))
           (copy (pal-finding findings :look "what a JPEG save with the page's own steps makes")))
      (true copy "~s" (mapcar #'palimpsest::finding-words findings))
      (when copy
        (is eq :characters-composed-resaved (palimpsest::finding-method copy))
        (true (pal-near-place-p (first (palimpsest::finding-details copy)) 19 20)
              "~s" (palimpsest::finding-details copy))
        (true (pal-near-place-p (first (palimpsest::finding-details copy)) 245 20)
              "~s" (palimpsest::finding-details copy))))
    (pal-jpeg-saved honest 0 0 600 500 :quality 92)
    (let ((findings (pal-jpeg-page-findings honest (palimpsest::standard-table 92))))
      (false (pal-finding findings nil "what a JPEG save with the page's own steps makes")
             "~s" (mapcar #'palimpsest::finding-words findings)))))

(defparameter *pal-scanner-table*
  (let ((rows '(8 10 9 14 22 36 46 55 11 11 13 17 23 52 54 50 13 12 14 22 36 51 62 50 13 15 20 26 46 78 72 56
                16 20 33 50 61 98 93 69 22 32 50 58 73 94 102 83 44 58 70 78 93 109 108 91 65 83 86 88 101 90 93 89)))
    (loop for k below 64 collect (elt rows (aref palimpsest::*zigzag* k))))
  "A real scanner's own luminance table, of about quality 55, in the order a JPEG keeps it.")

(defun pal-digit-column (&key (sigma 4) (seed 77))
  "The grey octets of a scanned page as PAL-NOISY-SCAN draws it, its first six lines bold, with its last
word on every line replaced by two figure-like marks, rings 12 by 18 pixels, printed alike down the page
at the same place in each line, 38 pixels apart, as a column of figures drawn by a program and printed;
with grain of SIGMA grey levels (Gaussian, seeded by SEED) over all of it."
  (let ((g (pal-noisy-scan :sigma 0 :bold 8 :bold-lines 6)) (random (sb-ext:seed-random-state seed)))
    (dotimes (line 12)
      (let ((top (+ 20 (* line 38))))
        (loop for y from top below (+ top 18)
              do (loop for x from 490 below 580 do (setf (aref g (+ (* y 600) x)) 235)))
        (dolist (left '(500 530))
          (dotimes (y 18)
            (dotimes (x 12)
              (when (or (< x 3) (>= x 9) (< y 3) (>= y 15))
                (setf (aref g (+ (* (+ top y) 600) left x)) 50)))))))
    (dotimes (i (length g) g)
      (setf (aref g i)
            (max 0 (min 255 (round (+ (aref g i)
                                      (* sigma (sqrt (* -2 (log (- 1 (random 1d0 random)))))
                                         (cos (* 2 pi (random 1d0 random))))))))))))

(define-test (palimpsest palimpsest-resaved-copy-libjpeg)
  ;; the same, saved by libjpeg rather than worked out here: a bold stroke copied 226 pixels across between
  ;; two saves at quality 92 named, with both places; the page saved twice untouched, nothing
  (let* ((findings (pal-picture-file-findings (pal-sample "resaved-copied.jpg")))
         (copy (pal-finding findings :look "what a JPEG save with the page's own steps makes")))
    (true copy "~s" (mapcar #'palimpsest::finding-words findings))
    (when copy
      (true (pal-near-place-p (first (palimpsest::finding-details copy)) 19 20))
      (true (pal-near-place-p (first (palimpsest::finding-details copy)) 245 20))))
  (let ((findings (pal-picture-file-findings (pal-sample "resaved-honest.jpg"))))
    (false (pal-finding findings nil "what a JPEG save with the page's own steps makes")
           "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-rendered-page-saved-straight)
  ;; a form drawn by a program (FreeType, Liberation Serif at 21 pixels, lines 32 pixels apart) and saved straight
  ;; as a grey JPEG of quality 40 by libjpeg: words drawn at the same place across on lines whole blocks apart come
  ;; out the same pixel for pixel, some over more than two characters' width, as the JPEG keeps every block alike
  ;; that was drawn alike. Its prints repeat so on the grid all over the page: no strong sign
  (let ((findings (pal-picture-file-findings (pal-sample "rendered-straight.jpg"))))
    (true (or (pal-finding findings :look "nearer one another than prints")
              (pal-finding findings :record "come near one another as clean prints"))
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings :strong "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-wide-copy-on-a-jpeg-grid)
  ;; three letters of a grained scan, joined by a stroke under them into one mark, laid pixel for pixel 19 blocks
  ;; below on a page kept as a JPEG: the same over more than two characters' width at the one move of the
  ;; paste, the page repeating nothing else on its grid, a strong sign
  (let ((page (pal-typeset *pal-honest-lines*)))
    (loop for y from 40 below 42
          do (loop for x from 20 below 72 do (setf (aref page (+ (* y 600) x)) (+ 45 (mod (* 7 (+ x y)) 13)))))
    (pal-laid page 18 18 18 170 58 28)
    (let ((copies (palimpsest::scan-reading-copies (palimpsest::copied-glyphs (pal-page-glyphs page) :dct '(7)))))
      (true (find-if (lambda (c) (and (eq (fifth c) :strong)
                                      (some (lambda (g) (<= 168 (palimpsest::glyph-y0 g) 175)) (first c))))
                     copies)
            "~s" (mapcar (lambda (c) (list (fifth c) (sixth c) (third c) (mapcar #'palimpsest::glyph-y0 (first c))))
                         copies)))))

(define-test (palimpsest palimpsest-resaved-copy-honest-pages)
  ;; honest pages saved twice where the steps, not the grain, make prints differ: coarse quality, a scanner's
  ;; own coarse table, little grain, and one figure printed alike down columns at the same place; none named
  (flet ((twice (grey table)
           (pal-jpeg-saved grey 0 0 600 500 :table table)
           (pal-jpeg-saved grey 0 0 600 500 :table table)
           (let ((findings (pal-jpeg-page-findings grey table)))
             (false (pal-finding findings nil "no words could be read") "~s" (mapcar #'palimpsest::finding-words findings))
             (false (pal-finding findings nil "what a JPEG save with the page's own steps makes")
                    "~s" (mapcar #'palimpsest::finding-words findings)))))
    (twice (pal-noisy-scan :sigma 5 :bold 8 :bold-lines 6) (palimpsest::standard-table 50))
    (twice (pal-noisy-scan :sigma 4 :bold 8 :bold-lines 6) (palimpsest::standard-table 40))
    (twice (pal-noisy-scan :sigma 5 :bold 8 :bold-lines 6) *pal-scanner-table*)
    (twice (pal-noisy-scan :sigma 2 :bold 8 :bold-lines 6) (palimpsest::standard-table 75))
    (twice (pal-digit-column) (palimpsest::standard-table 65))
    (twice (pal-digit-column :sigma 2) *pal-scanner-table*)))

(define-test (palimpsest palimpsest-resaved-copy-step-of-nothing)
  ;; a table holding a step of nothing is not taken for one a JPEG rounds by: no trial save divides by it
  (let ((grey (pal-jpeg-saved (pal-noisy-scan :sigma 5 :bold 8 :bold-lines 6) 0 0 600 500 :quality 92))
        (table (palimpsest::standard-table 92)))
    (true (palimpsest::jpeg-corner-steps grey 600 500 nil table))
    (false (palimpsest::jpeg-corner-steps grey 600 500 nil (substitute 0 (elt table 20) table :start 20 :count 1)))))

(define-test (palimpsest palimpsest-resaved-copy-measure)
  ;; the copy comes out as the save makes its source, a grey level or so off; its source as the save makes
  ;; the copy, as far as two prints; the next print of the stroke as far too; a stroke a whole number of
  ;; blocks away is not measured
  (let* ((grey (pal-bold-copied (pal-jpeg-saved (pal-noisy-scan :sigma 5 :bold 8 :bold-lines 6) 0 0 600 500 :quality 92)))
         (table (palimpsest::standard-table 92)))
    (pal-jpeg-saved grey 0 0 600 500 :quality 92)
    (let* ((steps (palimpsest::jpeg-corner-steps grey 600 500 nil table))
           (glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words grey 600 500)))
           (room (palimpsest::make-resave-room)))
      (true steps)
      (dolist (g glyphs) (setf (palimpsest::glyph-jpeg-cut g) (palimpsest::jpeg-cut-of grey 600 500 g steps)))
      (flet ((at (x y) (find-if (lambda (g) (and (<= (abs (- x (palimpsest::glyph-x0 g))) 1)
                                                 (<= (abs (- y (palimpsest::glyph-y0 g))) 1)))
                                glyphs)))
        (multiple-value-bind (laid apart) (palimpsest::resaved-copy (at 245 20) (at 19 20) room)
          (true (and laid (> apart 2) (< laid (* 1/3 apart))) "~s ~s" laid apart))
        (multiple-value-bind (laid apart) (palimpsest::resaved-copy (at 19 20) (at 245 20) room)
          (true (and laid (> laid (* 1/2 apart))) "~s ~s" laid apart))
        (multiple-value-bind (laid apart) (palimpsest::resaved-copy (at 263 20) (at 19 20) room)
          (true (and laid (> laid (* 1/2 apart))) "~s ~s" laid apart))
        (false (palimpsest::resaved-copy (at 227 20) (at 19 20) room))))))

(defun pal-blurred (grey x0 y0 x1 y1 &optional (times 2))
  "GREY (600 by 500) out of focus from X0 Y0 to X1 Y1: each pixel the mean
of the 9 about it, TIMES over. GREY, changed."
  (dotimes (n times grey)
    (let ((c (copy-seq grey)))
      (loop for y from (max 1 y0) below (min 499 y1)
            do (loop for x from (max 1 x0) below (min 599 x1)
                     do (setf (aref grey (+ (* y 600) x))
                              (round (loop for dy from -1 to 1
                                           sum (loop for dx from -1 to 1
                                                     sum (aref c (+ (* (+ y dy) 600) x dx)))) 9)))))))

(defun pal-resampled (grey scale &optional (degrees 0))
  "GREY (600 by 500) made SCALE times as large about its middle and turned
DEGREES, each pixel blended from the four about where it came from, paper
235 where nothing was: a scan made larger, smaller or turned."
  (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8)))
        (c (cos (* degrees (/ pi 180)))) (s (sin (* degrees (/ pi 180)))))
    (dotimes (y 500 out)
      (dotimes (x 600)
        (let* ((dx (- x 300)) (dy (- y 250))
               (sx (+ 300 (/ (- (* c dx) (* s dy)) scale))) (sy (+ 250 (/ (+ (* s dx) (* c dy)) scale)))
               (x0 (floor sx)) (y0 (floor sy)) (fx (- sx x0)) (fy (- sy y0)))
          (setf (aref out (+ (* y 600) x))
                (if (and (<= 0 x0 598) (<= 0 y0 498))
                    (max 0 (min 255 (round (+ (* (- 1 fx) (- 1 fy) (aref grey (+ (* y0 600) x0)))
                                              (* fx (- 1 fy) (aref grey (+ (* y0 600) x0 1)))
                                              (* (- 1 fx) fy (aref grey (+ (* (1+ y0) 600) x0)))
                                              (* fx fy (aref grey (+ (* (1+ y0) 600) x0 1)))))))
                    235)))))))

(defun pal-pieces (grey &key jpeg)
  "The regions JPEG-PIECES names on the scanned page GREY (600 by 500),
kept as a JPEG when JPEG is true."
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words grey 600 500))))
    (dolist (g glyphs) (setf (palimpsest::glyph-page g) 0 (palimpsest::glyph-picture g) 7))
    (palimpsest::jpeg-pieces glyphs :jpeg jpeg)))

(defun pal-word-relaid (grey &key (quality 75))
  "GREY (600 by 500), a scan saved as a JPEG, with a word of it cut out,
saved as a JPEG of QUALITY on its own grid and laid over another word,
off the page's grid. GREY, changed."
  (let ((piece (make-array (* 76 24))))
    (dotimes (y 24) (dotimes (x 76) (setf (aref piece (+ (* y 76) x)) (aref grey (+ (* (+ 360 y) 600) 302 x)))))
    (pal-jpeg-saved piece 0 0 76 24 :width 76 :quality quality)
    (dotimes (y 24) (dotimes (x 76) (setf (aref grey (+ (* (+ 170 y) 600) 207 x)) (aref piece (+ (* y 76) x)))))
    grey))

(define-test (palimpsest palimpsest-scanned-piece-saved-apart)
  ;; a word of a scan kept without loss saved as a JPEG on its own grid and laid back: named, with its place,
  ;; at a scanner's grain from 1.5 to 7 grey levels, quality 75 and 90; worth a look, as nothing else agrees
  ;; on it, and honest software laying a stamp kept as a JPEG leaves the same
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "JPEG blocks") "~s" (mapcar #'palimpsest::finding-words findings))
    (let ((piece (pal-finding findings :look "carries JPEG blocks of its own")))
      (true piece "~s" (mapcar #'palimpsest::finding-words findings))
      (when piece
        (pal-has (palimpsest::finding-words piece) "A region of page 1, about")
        (pal-has (palimpsest::finding-words piece) "a piece saved apart and laid in")
        (pal-has (first (palimpsest::finding-details piece))
                 "of picture 7), 9 characters, 9 with a block at column 7, row 1")
        (is eq :piece-laid-in (palimpsest::finding-method piece)))))
  (dolist (quality '(75 90))
    (dolist (sigma '(1.5 2 3 7))
      (let ((pieces (pal-pieces (pal-jpeg-saved (pal-noisy-scan :sigma sigma) 207 169 76 24 :quality quality))))
        (true (find :strong pieces :key #'palimpsest::block-piece-strength)
              "quality ~d, grain ~a: ~s" quality sigma pieces)
        (palimpsest::backed-pieces pieces '() '())
        (true (every (lambda (p)
                       (and (eq :look (palimpsest::block-piece-strength p))
                            (eq :alone (palimpsest::block-piece-backing p)))) pieces)
              "quality ~d, grain ~a: ~s" quality sigma pieces)
        (true (every (lambda (p)
                       (and (<= 207 (palimpsest::block-piece-x0 p))
                            (<= (palimpsest::block-piece-x1 p) 283))) pieces)))))
  ;; a single character so saved is not told from a smooth one
  (false (pal-pieces (pal-jpeg-saved (pal-noisy-scan :sigma 3) 207 170 10 22)))
  ;; a scan saved as a JPEG, then a word of it cut out, saved as a JPEG on its own grid and laid off the
  ;; page's grid: named, with its place; for the record, as no other sign meets it
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-word-relaid (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :look "the JPEG blocks the rest of the scan carries on its grid"))
    (let ((piece (pal-finding findings :record "the JPEG blocks the rest of the scan carries on its grid")))
      (true piece "~s" (mapcar #'palimpsest::finding-words findings))
      (when piece
        (pal-has (palimpsest::finding-words piece) "no other sign meets")
        (pal-has (first (palimpsest::finding-details piece)) "its blocks on the scan's grid with")))
    (false (pal-finding findings nil "of its own")))
  (dolist (quality '(75 90))
    (dolist (sigma '(1.5 3 7))
      (true (pal-pieces (pal-word-relaid (pal-jpeg-saved (pal-noisy-scan :sigma sigma) 0
                                                         0 600 500 :quality quality) :quality quality))
            "quality ~d, grain ~a" quality sigma)))
  ;; a picture kept as a JPEG is told by its filter, alone or last of a chain
  (let ((pdf (palimpsest::read-pdf (pal-scanned (pal-scan)) :name "test.pdf")))
    (false (palimpsest::picture-jpeg-p pdf (palimpsest::last-revision pdf) 7)))
  (dolist (filter '("/DCTDecode" "[/FlateDecode /DCTDecode]"))
    (let ((pdf (palimpsest::read-pdf (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                                         '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                                         '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R "
                                                                                      "/MediaBox [0 0 600 500] "
                                                                                      "/Resources << /XObject << /Im1 "
                                                                                      "7 0 R >> >> /Contents 4 0 R >>"))
                                                         '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                                         '(6 . "<< /Producer (Kea Scan) >>")
                                                         (list 7
                                                               :stream (format nil
                                                                               "/Type /XObject /Subtype /Image /Width ~
                                                                                8 /Height 8 /ColorSpace /DeviceGray ~
                                                                                /BitsPerComponent 8 /Filter ~a" filter)
                                                               (make-array 16
                                                                           :element-type '(unsigned-byte 8)
                                                                           :initial-element 0))))
                                     :name "test.pdf")))
      (true (palimpsest::picture-jpeg-p pdf (palimpsest::last-revision pdf) 7) "~a" filter)))
  ;; a page drawn and kept without loss, its paper white, with a picture in it saved as a JPEG: its blocks
  ;; the only ones measured, the rest passed over as white, so never a strong sign
  (dolist (quality '(50 75))
    (dolist (box '((113 17 76 26) (113 17 170 26) (113 17 170 100)))
      (destructuring-bind (x y w h) box
        (false (find :strong (pal-pieces (pal-jpeg-saved (pal-noisy-scan :sigma 0
                                                                         :paper 255 :core 30 :edge 130)
                                                         x y w h :quality quality))
                     :key #'palimpsest::block-piece-strength)
               "quality ~d, ~a" quality box))))
  ;; the frequencies counted as a JPEG sets them: a block of grain keeps them, the same block saved as a JPEG does not
  (let* ((grey (pal-grained (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 200)))
         (glyph (palimpsest::make-glyph :pixels (subseq grey 0 (* 12 600)) :width 600 :x0 2 :y0 2 :x1 10 :y1 10)))
    (true (< (reduce #'max (palimpsest::glyph-block-scores glyph)) 14))
    (pal-jpeg-saved grey 0 0 600 12)
    (let ((saved (palimpsest::make-glyph :pixels (subseq grey 0 (* 12 600)) :width 600 :x0 2 :y0 2 :x1 10 :y1 10)))
      (is = 28 (aref (palimpsest::glyph-block-scores saved) 0)))))

(define-test (palimpsest palimpsest-lone-piece-noted)
  ;; a region lacking the JPEG's blocks that nothing else meets is noted; one a copied character meets, or a
  ;; word unlike the rest, stays worth a look
  (let ((piece (palimpsest::make-block-piece :page 0 :picture 7 :x0 100 :y0 100 :x1 150 :y1 130
                                             :kind :off-grid :strength :look))
        (glyph (palimpsest::make-glyph :page 0 :picture 7 :x0 140 :y0 110 :x1 160 :y1 140)))
    (false (palimpsest::piece-met-p piece (palimpsest::make-scan-reading)))
    (true (palimpsest::piece-met-p piece (palimpsest::make-scan-reading
                                          :copies (list (list (list glyph) 3.0 nil 10.0 :look nil nil)))))
    (false (palimpsest::piece-met-p piece (palimpsest::make-scan-reading
                                           :copies (list (list (list (palimpsest::make-glyph :page 0 :picture 7
                                                                                              :x0 200 :y0 110
                                                                                              :x1 220 :y1 140))
                                                               3.0 nil 10.0 :look nil nil)))))))

(define-test (palimpsest palimpsest-program-names-of-each-save)
  ;; the programs each save names, read once for the checks and the fingerprint
  (let ((pdf (palimpsest::read-pdf (pal-edited) :name "test.pdf")))
    (is equal '(("/Producer" 0 "Kea Writer 1.0") ("/Creator" 0 "Kea Writer") ("/Producer" 1 "iLovePDF")
                ("/Creator" 1 "Kea Writer"))
        (mapcar (lambda (entry) (list (first entry) (palimpsest::revision-index (second entry)) (third entry)))
                (palimpsest::program-names pdf)))))

(define-test (palimpsest palimpsest-pages-matched-by-number-then-place)
  ;; a page set beside the first of its object number in the other revision, else the first of its place
  (let* ((old (list (palimpsest::make-page :number 7 :index 0) (palimpsest::make-page :number 9 :index 1)
                    (palimpsest::make-page :number 9 :index 2)))
         (matching (palimpsest::page-matcher old)))
    (is eq (second old) (funcall matching (palimpsest::make-page :number 9 :index 0)))
    (is eq (third old) (funcall matching (palimpsest::make-page :number 4 :index 2)))
    (is eq (first old) (funcall matching (palimpsest::make-page :index 0)))
    (false (funcall matching (palimpsest::make-page :number 4 :index 5)))))

(define-test (palimpsest palimpsest-kept-once-for-each-revision)
  ;; what depends on one revision alone worked out once for it, apart for each revision
  (let ((pdf (palimpsest::read-pdf (pal-edited) :name "test.pdf")) (worked 0))
    (dotimes (k 2)
      (loop for revision across (palimpsest::pdf-revisions pdf)
            do (is eq revision (palimpsest::kept-for-pdf (pdf :test-kept revision) (incf worked) revision))))
    (is = 2 worked)))

(define-test (palimpsest palimpsest-glyph-hashes-are-fnv-1a)
  ;; a character's hashes are FNV-1a over the octets they name, sizes and places lowest octet first
  (flet ((fnv (octets)
           (let ((hash #xcbf29ce484222325))
             (dolist (o octets hash) (setf hash (ldb (byte 64 0) (* (logxor hash o) #x100000001b3)))))))
    (let* ((pixels (coerce (loop for i below 63 collect (mod (* 37 i) 256)) '(simple-array (unsigned-byte 8) (*))))
           (glyph (palimpsest::make-glyph :pixels pixels :width 9 :x0 300 :y0 2 :x1 305 :y1 5 :picture 7 :page 2)))
      ;; the box is 5 by 3, two pixels in from the cut's top left corner
      (flet ((rows (top bottom left right)
               (loop for j from top below bottom
                     append (loop for i from left below right collect (aref pixels (+ (* j 9) i))))))
        (is = (ldb (byte 62 0) (fnv (append (list 5 0 3 0) (rows 2 5 2 7)))) (palimpsest::glyph-box-hash glyph))
        (is = (ldb (byte 38 0) (fnv (append (list 2 0 3 0) (rows 1 4 4 6)))) (palimpsest::strip-hash glyph 4 2 1 4)))
      (is = (ldb (byte 62 0) (fnv (list 7 0 0 0 2 0 0 0 44 1 0 0 2 0 0 0)))
          (palimpsest::glyph-place-hash glyph)))))

(define-test (palimpsest palimpsest-grey-counts)
  ;; the grey levels of a picture counted, rows skipped and counts added to
  (let ((grey (coerce '(0 0 9 9 255 255) '(simple-array (unsigned-byte 8) (*))))
        (skipped (make-array 3 :element-type 'bit :initial-contents '(0 1 0))))
    (is = 2 (aref (palimpsest::grey-histogram grey) 9))
    (is = 0 (aref (palimpsest::grey-histogram grey :width 2 :skipped skipped) 9))
    (is = 2 (aref (palimpsest::grey-histogram grey :width 2 :skipped skipped) 255))
    (is = 1 (aref (palimpsest::grey-histogram grey :start 3 :end 5) 9))
    (let ((counts (palimpsest::grey-histogram grey)))
      (palimpsest::grey-histogram grey :into counts)
      (is = 4 (aref counts 0)))))

(define-test (palimpsest palimpsest-slanted-rule-pixels-found-by-row)
  ;; the pixels of the slanted rules taken out, sorted by place, found from a row's first pixel on
  (let ((changed (coerce (mapcar (lambda (place) (+ (* place 256) 40)) '(3 10 10 25 40))
                         '(simple-array fixnum (*)))))
    (is = 0 (palimpsest::changed-from changed 0))
    (is = 1 (palimpsest::changed-from changed 4))
    (is = 1 (palimpsest::changed-from changed 10))
    (is = 3 (palimpsest::changed-from changed 11))
    (is = 5 (palimpsest::changed-from changed 41))))

(define-test (palimpsest palimpsest-block-scores-worked-out-once)
  ;; a character's blocks are measured once, whichever of the checks asks first
  (let* ((grey (pal-grained (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 200)))
         (glyph (palimpsest::make-glyph :pixels (subseq grey 0 (* 12 600)) :width 600 :x0 2 :y0 2 :x1 10 :y1 10)))
    (true (eq (palimpsest::glyph-block-scores glyph) (palimpsest::glyph-block-scores glyph)))))

(define-test (palimpsest palimpsest-scan-smoothness-no-piece)
  ;; honest scans smooth in places, at a real scanner's grain, are not taken for pieces saved as JPEGs: a
  ;; smooth stretch is flat however a block sits on it
  (flet ((none (grey what &key jpeg)
           (let ((pieces (pal-pieces grey :jpeg jpeg)))
             (false pieces "~a: ~s" what pieces))))
    (dolist (sigma '(0.5 1 1.5 2 3))
      (none (pal-noisy-scan :sigma sigma) (format nil "grain ~a" sigma))
      (none (pal-noisy-scan :sigma sigma :bold 12) (format nil "a bold line, grain ~a" sigma)))
    (dolist (sigma '(1.5 2 3 6))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 110 15 195 45)
            (format nil "a word out of focus, grain ~a" sigma))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 110 15 195 45 1)
            (format nil "a word a little out of focus, grain ~a" sigma))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 0 15 600 45) (format nil "a line out of focus, grain ~a" sigma))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 300 250 600 500)
            (format nil "a quarter out of focus, grain ~a" sigma)))
    (dolist (low '(1 0.7 0.3))
      (dolist (sigma '(2 3))
        (let ((grey (pal-noisy-scan :sigma sigma)) (smooth (pal-noisy-scan :sigma low :seed 99)))
          (replace grey smooth :end2 (* 600 160))
          (none grey (format nil "grain ~a, its top third ~a, the scanner's denoising" sigma low)))))
    (dolist (ink '(0.5 1.2))
      (none (pal-noisy-scan :sigma 3 :ink-sigma ink :bold 12) (format nil "a bold line of flat ink ~a" ink)))
    (none (pal-noisy-scan :sigma 0 :paper 255 :core 100 :edge 180) "drawn, not scanned")
    (none (pal-noisy-scan :sigma 0 :paper 255 :core 0 :edge 128 :bold 12) "drawn, a black heading")
    ;; pages kept as JPEGs, of any quality, one word out of focus; and JPEGs made larger, smaller or turned
    (dolist (quality '(75 90 95 98))
      (dolist (sigma '(1.5 2 3))
        (let ((jpeg (pal-jpeg-saved (pal-noisy-scan :sigma sigma) 0 0 600 500 :quality quality)))
          (none (copy-seq jpeg) (format nil "a JPEG of ~d, grain ~a" quality sigma) :jpeg t)
          (none (copy-seq jpeg) (format nil "a JPEG of ~d kept without loss, grain ~a" quality sigma))
          (none (pal-resampled jpeg 0.97) (format nil "a JPEG of ~d made smaller, grain ~a" quality sigma))
          (none (pal-resampled jpeg 1.25) (format nil "a JPEG of ~d made larger, grain ~a" quality sigma))
          (none (pal-resampled jpeg 1 0.4) (format nil "a JPEG of ~d turned, grain ~a" quality sigma)))
        (none (pal-jpeg-saved (pal-blurred (pal-noisy-scan :sigma sigma) 110 15 195 45 1) 0 0 600 500 :quality quality)
              (format nil "a JPEG of ~d, a word out of focus, grain ~a" quality sigma) :jpeg t)))
    ;; a word laid crisp, black on white, over a scan saved as a JPEG: its blocks clipped, told neither way
    (let ((grey (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500)) (crisp (pal-scan :pasted t)))
      (loop for y from 169 below 193
            do (loop for x from 207 below 283
                     do (setf (aref grey (+ (* y 600) x)) (if (< (aref crisp (+ (* y 600) x)) 128) 0 255))))
      (none grey "a crisp word on a JPEG")))
  ;; the scans of the other tests, untouched and saved as JPEGs, through the whole examination
  (dolist (grey (list (pal-grained (pal-scan)) (pal-scan) (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500)
                      (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500 :quality 90)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "JPEG blocks") "~s" (mapcar #'palimpsest::finding-words findings)))))
;;; Pages in heavier type and drawn into, for the grain of characters

(defun pal-typed (grey line &key (words '(0 1 2 3 4 5)) erased (ink 60) clean)
  "GREY, a scanned page (PAL-SCAN), with the WORDS (from 0) of LINE (from 0) drawn again as a program draws
text into a picture. The old word is covered with the page's own paper (from below its lines), each
stroke 3 pixels of one grey INK, smoothed as a text tool smooths where a stroke falls between pixels, a
pixel of 150 to its right on every other row, no grain. With CLEAN, it is on paper of grey 250 and
without the smoothing. The words ERASED of the line covered with paper alone. GREY."
  (flet ((cover (word)
           (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 95))))
             (loop for y from (- top 3) below (+ top 21) for k from 0
                   do (loop for x from (- left 3) below (+ left 73)
                            do (setf (aref grey (+ (* y 600) x))
                                     (if clean 250 (aref grey (+ (* (+ 467 k) 600) x)))))))))
    (mapc #'cover erased)
    (dolist (word words grey)
      (cover word)
      (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 95))))
        (dotimes (stroke 9)
          (loop for y from top repeat 18
                do (loop for dx from 0 to (if (or clean (oddp y)) 2 3)
                         do (setf (aref grey (+ (* y 600) left (* stroke 8) dx)) (if (= dx 3) 150 ink)))))))))

(defun pal-varied (&key (grain 12) (line 10) (width 3) (height 18) (ink 60) (strokes 9) (pitch 8))
  "The grey octets of an honest scanned page of 12 lines of 6 words, as
PAL-SCAN's, its grain over all of it up to GRAIN either way, but for LINE,
in another type: strokes WIDTH wide and HEIGHT tall of INK, STROKES of
them a word, PITCH apart (a bold line, or ink a darker pass clipped)."
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8))) (seed 12345))
    (dotimes (i (length grey))
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (+ 225 (mod (ash seed -16) 21))))
    (dotimes (l 12)
      (dotimes (word 6)
        (let* ((other (eql l line)) (top (+ 20 (* l 38))) (left (+ 20 (* word 95)))
               (w (if other width 3)) (h (if other height 18)) (k (if other ink 60)))
          (dotimes (stroke (if other strokes 9))
            (let ((x0 (+ left (* stroke (if other pitch 8)))))
              (loop for y from top repeat h
                    do (loop for dx from -1 to w
                             do (setf (aref grey (+ (* y 600) x0 dx)) (if (<= 0 dx (1- w)) k 140)))))))))
    (setf seed 777)
    (dotimes (i (length grey) grey)
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) (- grain) (mod (ash seed -16) (1+ (* 2 grain))))))))))

(defun pal-stamped
    (&key (sigma 6) uniform (blur (not uniform)) (line 10) (words '(3)) (level 76) (ink 60) bold (lowered 0) width
       height strokes pitch (seed 99))
  "The grey octets of an honest scanned page of PAL-SCAN's lines, blurred by a 3 by 3 mean with BLUR
\(without UNIFORM unless told), then given grain: Gaussian of SIGMA grey levels, or with UNIFORM, up to
SIGMA either way. The WORDS of LINE are a stamp, a coloured ink or a heavy ink the scanner clipped, its
strokes' cores one grey LEVEL after the grain, or with LEVEL (GREY . SPREAD) that grey jittering by
SPREAD as a scanner's clipping does. With LEVEL NIL, they are printed in another INK with grain as the
rest, as a typewriter or a second pass prints. They are wider with BOLD (5 pixels, 7 a word), or WIDTH by
HEIGHT, STROKES a word PITCH apart. LOWERED is that many pixels below the line (above, when less than 0)."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0))
        (flat (make-array (* 600 500) :initial-element nil)) (s seed))
    (labels ((uniform () (setf s (mod (+ (* s 1103515245) 12345) 2147483648)) (/ (+ 1 s) 2147483649d0))
             (gauss () (* (sqrt (* -2 (log (uniform)))) (cos (* 2 pi (uniform))))))
      (dotimes (l 12)
        (dotimes (word 6)
          (let* ((stamp (and (= l line) (member word words))) (top (+ 20 (* l 38) (if stamp lowered 0)))
                 (left (+ 20 (* word 95)))
                 (w (cond ((not stamp) 3) (width) (bold 5) (t 3))))
            (dotimes (stroke (cond ((not stamp) 9) (strokes) (bold 7) (t 9)))
              (let ((x0 (+ left (* stroke (cond ((not stamp) 8) (pitch) (bold 9) (t 8))))))
                (loop for y from top repeat (if (and stamp height) height 18)
                      do (loop for dx from -1 to w
                               do (let ((i (+ (* y 600) x0 dx)))
                                    (setf (aref grey i) (if (<= 0 dx (1- w)) (float (if stamp ink 60) 1d0) 140d0))
                                    (when (and stamp level (<= 0 dx (1- w))) (setf (aref flat i) level))))))))))
      (when blur
        (let ((copy (copy-seq grey)))
          (loop for y from 1 below 499
                do (loop for x from 1 below 599
                         do (setf (aref grey (+ (* y 600) x))
                                  (/ (loop for dy from -1 to 1
                                           sum (loop for dx from -1 to 1
                                                     sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9))))))
      (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
        (dotimes (i (length out) out)
          (let ((stamp (aref flat i)))
            (setf (aref out i)
                  (max 0 (min 255 (round (cond ((consp stamp) (+ (car stamp) (* (cdr stamp) (gauss))))
                                               (stamp)
                                               (uniform (+ (aref grey i) (- (* (uniform) (1+ (* 2 sigma))) sigma 0.5)))
                                               (t (+ (aref grey i) (* sigma (gauss)))))))))))))))

(defun pal-alike (pdf)
  "How alike characters of the same shape typically are on the scanned pages of PDF (COPIED-GLYPHS); NIL
when the scan has too little grain for its characters to be set beside each other."
  (let ((alike (palimpsest::scan-reading-alike (palimpsest::scans-read pdf (aref (palimpsest::pdf-revisions pdf) 0)))))
    (and alike (>= alike 4) alike)))

(define-test (palimpsest palimpsest-marks-without-grain)
  ;; two marks of one ink, no grain within them, the same pixel for pixel: no copies, but marks worth a look
  (let ((grey (pal-grained (pal-scan))))
    (dolist (at '((92 . 255) (282 . 329)))
      (dotimes (y 8) (dotimes (x 4) (setf (aref grey (+ (* (+ (cdr at) y) 600) (car at) x)) 0))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (let ((marks (pal-finding findings
                                :look "2 marks on page 1 are the same pixel for pixel, with no grain of their own")))
        (true marks "~s" (mapcar #'palimpsest::finding-words findings))
        (when marks
          (is eq :typed-into-scan (palimpsest::finding-method marks))
          (pal-has (first (palimpsest::finding-details marks)) "pixels 92,255 to 96,263 of picture 7")
          (pal-has (first (palimpsest::finding-details marks)) "pixels 282,329 to 286,337 of picture 7")))
      (false (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "of the scanned page has no grain") "~s"
             (mapcar #'palimpsest::finding-words findings))))
  ;; a word drawn into the scan in the page's own ink, on the page's own paper, its edges smoothed: no grain of
  ;; its own, worth a look, the way named, not for its ink or paper
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-typed (pal-grained (pal-scan)) 10 :words '(3))))
    (declare (ignore pdf accounts))
    (let ((typed (pal-finding findings
                              :look (format nil "On page 1, a word of the scanned page has no grain of its own: drawn ~
                                                 into the scan by a program, or in bold or heavier type, or ink, a ~
                                                 stamp or coloured ink the scanner clipped."))))
      (true typed "~s" (mapcar #'palimpsest::finding-words findings))
      (when typed
        (is eq :typed-into-scan (palimpsest::finding-method typed))
        (pal-has (first (palimpsest::finding-details typed)) "a word at x ")
        (pal-has (first (palimpsest::finding-details typed)) "(pixels 305,400 to ")
        (pal-has (first (palimpsest::finding-details typed)) "9 characters, none with grain of its own")))
    (false (pal-finding findings :strong "no grain of") "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings nil "marks on page") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a whole line so, of three words: the line named; drawn in black on clean paper: the same
  (dolist (page (list (pal-typed (pal-grained (pal-scan)) 10 :words '(0 1 2) :erased '(3 4 5))
                      (pal-typed (pal-grained (pal-scan)) 10 :words '(3) :ink 0 :clean t)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "of the scanned page has no grain of its own: drawn into the scan by a program")
            "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :strong "no grain of") "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; honest pages, the scan's grain such that characters are set beside each other: a bold line, a line in
  ;; heavier type, two bold words, a line the scanner clipped; stamps or coloured inks clipped to one grey, 29,
  ;; 76 or 100, plain, lowered or bold, at Gaussian grain of 5 and 8 and uniform of 6 and 12: no strong sign of grain
  (loop for (name . page) in (append (list (cons "bold line"
                                                 (pal-varied :grain 6 :width 6 :height 30 :strokes 6 :pitch 10))
                                           (cons "heavier line"
                                                 (pal-varied :grain 6 :width 8 :height 34 :ink 40 :strokes 6 :pitch 12))
                                           (cons "bold words"
                                                 (pal-varied :grain 6 :width 6 :height 30 :strokes 4 :pitch 10))
                                           (cons "clipped line" (pal-varied :ink 0)))
                                     (loop for (sigma uniform) in '((5 nil) (8 nil) (6 t) (12 t))
                                           append (loop for level in '(29 76 100)
                                                        append (loop for (how bold lowered)
                                                                     in '(("plain" nil 0) ("lowered" nil 5)
                                                                          ("bold" t 0) ("bold, lowered" t 5))
                                                                     collect (cons (format nil "stamp ~d ~a, grain ~
                                                                                                ~d~:[~; uniform~]"
                                                                                           level how sigma uniform)
                                                                                   (pal-stamped :sigma sigma
                                                                                                :uniform uniform
                                                                                                :level level
                                                                                                :bold bold
                                                                                                :lowered lowered))))))
        do (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
             (declare (ignore accounts))
             (true (pal-alike pdf) "~a: characters set beside each other" name)
             (false (pal-finding findings :strong "no grain of") "~a: ~s" name
                    (mapcar #'palimpsest::finding-words findings))
             ;; nor copies: characters alike for their cores of one grey alone
             (false (pal-finding findings :strong "copies of one another") "~a: ~s" name
                    (mapcar #'palimpsest::finding-words findings))))
  ;; honest ink the scanner clipped to a grey or two that jitters, as clipping does: a heading at 1 jittering
  ;; by 0.7, a body line at 2 by 0.7 or 0.3, a typewriter word at 3 by 1 set lower, stamps at 29 by 0.3 and
  ;; 0.8, plain and lowered; each at uniform grain of 6 and 12 and Gaussian of 5 and 8: no strong sign at all
  (loop for (name . args) in '(("heading 1 by 0.7" :words (0 1 2 3 4 5) :width 6 :height 30 :strokes 6
                                :pitch 10 :level (1 . 0.7))
                              ("body line 2 by 0.7" :words (0 1 2 3 4 5) :level (2 . 0.7))
                              ("body line 2 by 0.3" :words (0 1 2 3 4 5) :level (2 . 0.3))
                              ("typewriter word 3 by 1, lower" :level (3 . 1.0) :lowered 4)
                              ("stamp 29 by 0.3" :level (29 . 0.3))
                              ("stamp 29 by 0.8, lower" :level (29 . 0.8) :lowered 5))
        do (loop for (sigma uniform) in '((6 t) (12 t) (5 nil) (8 nil))
                 do (multiple-value-bind (pdf accounts findings)
                        (pal-read (pal-scanned (apply #'pal-stamped :sigma sigma :uniform uniform args)))
                      (declare (ignore accounts))
                      (true (pal-alike pdf) "~a, grain ~d: characters set beside each other" name sigma)
                      (false (find :strong findings :key #'palimpsest::finding-strength)
                             "~a, grain ~d~:[~; uniform~]: ~s"
                             name sigma uniform
                             (mapcar #'palimpsest::finding-words
                                     (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq))))))
  ;; honest words printed in another pass, thin strokes 2 pixels wide set 5 or 7 pixels lower or 5 higher: a
  ;; typewriter word in ink 30, two in ink 60, a thin print in ink 80; at Gaussian grain of 5 and 8 and
  ;; uniform of 6 and 12: no strong sign (their narrow strokes read as cleaner paper within the word's box)
  (loop for (name . args) in '(("typewriter word" :width 2 :height 16 :ink 30)
                              ("typewriter words" :words (3 4) :width 2 :height 16 :ink 60)
                              ("thin print" :width 2 :height 14 :ink 80))
        do (loop for lowered in '(5 7 -5)
                 do (loop for (sigma uniform) in '((5 nil) (8 nil) (6 t) (12 t))
                          do (multiple-value-bind (pdf accounts findings)
                                 (pal-read (pal-scanned (apply #'pal-stamped :sigma sigma :uniform uniform :blur t
                                                               :level nil :lowered lowered args)))
                               (declare (ignore accounts))
                               (true (pal-alike pdf)
                                     "~a ~d, grain ~d: characters set beside each other" name lowered sigma)
                               (false (find :strong findings :key #'palimpsest::finding-strength)
                                      "~a ~d, grain ~d~:[~; uniform~]: ~s"
                                      name lowered sigma uniform
                                      (mapcar #'palimpsest::finding-words
                                              (remove :strong findings
                                                      :key #'palimpsest::finding-strength :test-not #'eq)))))))
  ;; the scan untouched, with grain: nothing; a page without grain anywhere, a word drawn on it too: nothing told
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-scan))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "grain of") "~s" (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-typed (pal-scan) 10 :words '(3))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "of the scanned page has no grain") "~s"
           (mapcar #'palimpsest::finding-words findings))
    ;; and the analyst told nothing was judged there, and why
    (let ((record (pal-finding findings
                               :record (format nil "Copies and grain not checked on page 1: the scan's grain is too ~
                                                    faint to tell prints apart."))))
      (true record "~s" (mapcar #'palimpsest::finding-words findings))
      (when record (pal-has (first (palimpsest::finding-details record)) "4 are needed"))))
  ;; a scan with grain: no such record
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-scan))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "not checked") "~s" (mapcar #'palimpsest::finding-words findings))))

(defun pal-clipped-heading (grey)
  "GREY, a page of PAL-VARIED with its line 10 in bold, with that line's ink
clipped to black wherever it is darker than 13, as a scanner clips a
heavy heading: its strokes' cores all of grey 0. GREY."
  (loop for y from (+ 20 (* 10 38)) below (+ 20 (* 10 38) 30)
        do (dotimes (x 600) (when (< (aref grey (+ (* y 600) x)) 13) (setf (aref grey (+ (* y 600) x)) 0))))
  grey)

(define-test (palimpsest palimpsest-clipped-ink-set-aside)
  ;; honest pages with a line whose ink the scanner clipped: no prints taken for copies saved again
  (dolist (page (list (pal-varied :ink 0) (pal-varied :grain 6 :ink 0)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore accounts))
      (true (pal-alike pdf))
      (false (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "nearer one another than prints") "~s"
             (mapcar #'palimpsest::finding-words findings))))
  ;; real copies on such a page still found: a stroke of the body copied over another, a strong sign; a
  ;; stroke of the clipped line copied over another, its core one grey, marks worth a look
  (let ((page (pal-varied :ink 0)))
    (pal-copy-box page page 114 58 233 172)
    (pal-copy-box page page (+ 20 95 -1) (+ 20 (* 10 38)) (+ 20 (* 3 95) (* 2 8) -1) (+ 20 (* 10 38)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "2 characters of the scanned page are copies of one another"))
            (look (pal-finding findings
                               :look "2 marks on page 1 are the same pixel for pixel, with no grain of their own")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        (when strong (pal-has (first (palimpsest::finding-details strong)) "pixels 114,58 to 119,76 of picture 7"))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when look (pal-has (first (palimpsest::finding-details look)) "pixels 114,400 to 119,418 of picture 7")))))
  ;; a heading the scanner clipped to black: its characters not judged, said so in the report and drawn so
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-clipped-heading (pal-varied :width 6 :height 30 :strokes 6 :pitch 10 :ink 0))))
    (declare (ignore accounts))
    (let ((record (pal-finding findings
                               :record (format nil "36 characters not judged: too few pixels left once the ink the ~
                                                    scanner clipped is set aside, on page 1."))))
      (true record "~s" (mapcar #'palimpsest::finding-words findings))
      (when record (pal-has (first (palimpsest::finding-details record)) "page 1 at x ")))
    (false (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))
    (let ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (let* ((reading (palimpsest::scans-read pdf revision))
             (scans (palimpsest::scan-reading-scans reading)) (copies (palimpsest::scan-reading-copies reading))
             (not-judged (palimpsest::scan-reading-not-judged reading)))
        (is = 36 (length not-judged))
        (let ((drawn (palimpsest::overlay-operators pdf revision (palimpsest::scan-picture-page (first scans))
                                                    :scans scans :copies copies :not-judged not-judged)))
          (pal-has drawn "(not judged: clipped) Tj")
          (pal-has drawn "[0.4 0.8] 0 d")
          (pal-has drawn "0.000 0.451 0.149 RG")
          (pal-has drawn
                   (format nil "(Not judged: too few pixels left once the ink the scanner clipped ~
                                is set aside. Grey hatch: clipped ink set aside.) Tj"))))))
  ;; the marks of a clipped page hatched where their clipped ink was set aside
  (let ((page (pal-varied :ink 0)))
    (pal-copy-box page page (+ 20 95 -1) (+ 20 (* 10 38)) (+ 20 (* 3 95) (* 2 8) -1) (+ 20 (* 10 38)))
    (let* ((pdf (pal-read (pal-scanned page))) (revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (let* ((reading (palimpsest::scans-read pdf revision))
             (scans (palimpsest::scan-reading-scans reading)) (copies (palimpsest::scan-reading-copies reading))
             (marks (palimpsest::scan-reading-marks reading)))
        (true marks)
        (pal-has (palimpsest::overlay-operators pdf revision (palimpsest::scan-picture-page (first scans))
                                                :scans scans :copies copies :bare (mapcar #'list marks))
                 "q 0.6 0.6 0.6 RG 0.1 w ")))))

;; signs framed by their strength: red whole for strong, blue dashed for worth a look, with the key
(define-test (palimpsest palimpsest-frames-by-strength)
  (let ((page (pal-varied :ink 0)))
    (pal-copy-box page page 114 58 233 172)
    (let* ((pdf (pal-read (pal-scanned page))) (revision (aref (palimpsest::pdf-revisions pdf) 0))
           (reading (palimpsest::scans-read pdf revision))
           (scans (palimpsest::scan-reading-scans reading))
           (copies (remove :strong (palimpsest::scan-reading-copies reading) :key #'fifth :test-not #'eq))
           (on (palimpsest::scan-picture-page (first scans))))
      (true copies)
      (let ((drawn (palimpsest::overlay-operators pdf revision on :scans scans :copies copies)))
        (pal-has drawn "q 0.902 0.000 0.000 RG 0.6 w [] 0 d")
        (pal-has drawn "(copy) Tj")
        (pal-has drawn "(A strong sign.) Tj")
        (pal-has drawn "(A sign worth a look.) Tj")
        ;; each line of the key begun by a piece of its frame, in its colour and line
        (pal-has drawn "re f 0.902 0.000 0.000 RG 0.6 w [] 0 d")
        (pal-has drawn "re f 0.102 0.298 0.902 RG 0.6 w [2 1] 0 d")
        (false (search "Dark green" drawn)))
      (let ((drawn (palimpsest::overlay-operators pdf revision on :scans scans
                                                  :copies (mapcar (lambda (entry)
                                                                    (append (subseq entry 0 4) (list :look)
                                                                            (nthcdr 5 entry)))
                                                                  copies))))
        (pal-has drawn "q 0.102 0.298 0.902 RG 0.6 w [2 1] 0 d")
        (false (search "q 0.902 0.000 0.000 RG" drawn)))
      ;; a strength's colour is its setting's, in the frames and in the key; a setting holding no colour keeps
      ;; the standard one
      (let ((palimpsest::*palimpsest-strong-colour* "#00ff00") (palimpsest::*palimpsest-look-colour* "not a colour"))
        (let ((drawn (palimpsest::overlay-operators pdf revision on :scans scans :copies copies)))
          (pal-has drawn "q 0.000 1.000 0.000 RG 0.6 w [] 0 d")
          (pal-has drawn "0.000 1.000 0.000 rg BT /PalimpsestLabel 6 Tf")
          (false (search "q 0.902 0.000 0.000 RG" drawn)))
        (true (equal (palimpsest::strength-colour :look) (palimpsest::strength-colour-of "#1a4ce6"))))
      ;; and its line its line setting's, in the frames and in the key
      (let ((palimpsest::*palimpsest-strong-line* :dotted))
        (let ((drawn (palimpsest::overlay-operators pdf revision on :scans scans :copies copies)))
          (pal-has drawn "q 0.902 0.000 0.000 RG 0.6 w [0.4 0.8] 0 d")
          (pal-has drawn "re f 0.902 0.000 0.000 RG 0.6 w [0.4 0.8] 0 d")
          (false (search "q 0.902 0.000 0.000 RG 0.6 w [] 0 d" drawn))))))
  ;; no object's own frame takes the standard colour of a strength
  (dolist (colour (append palimpsest::*draw-colours* (list palimpsest::*shape-frame-colour*)))
    (false (find colour (mapcar (lambda (entry) (palimpsest::strength-colour-of (third entry)))
                                palimpsest::*strength-colours*)
                 :test #'equal)
           "~s" colour)))

;;; Letters the blur joins

(defun pal-joined
    (&key (sigma 6) uniform (blur t) (width 3) (height 18) (pitch 8) (ink 60) (neck 120) dark varied clipped (seed 99))
  "The grey octets of an honest scanned page of PAL-SCAN's lines, 600 by 500 pixels, of words of nine
strokes, the third and fourth and the seventh and eighth of each joined at a neck as letters set tight
are by a scanner's blur. The neck is three rows of NECK grey bridging the paper between the two, from
their second row. With DARK, the lines of that list (from 0) have a kerned pair or a ligature in each
word: its first two strokes joined by a bridge of the ink itself, the third joined to them at a neck.
With VARIED, every two strokes are joined but the last, no two pairs alike, as no two pairs of letters
set tight are. Each pair's neck is two rows of a grey 20 darker than NECK at a height of its own, its
second stroke in an ink up to 60 lighter, up to 2 rows shorter and set up to 2 rows lower. Strokes are
WIDTH wide and HEIGHT tall with edges of 140, PITCH apart, of INK. The page is blurred by a 3 by 3 mean
with BLUR, then given grain: Gaussian of SIGMA grey levels, or with UNIFORM, up to SIGMA either way. With
CLIPPED, every pixel darker than it made it, as a scanner clips."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0)) (s seed))
    (labels ((uniform () (setf s (mod (+ (* s 1103515245) 12345) 2147483648)) (/ (+ 1 s) 2147483649d0))
             (gauss () (* (sqrt (* -2 (log (uniform)))) (cos (* 2 pi (uniform)))))
             (bridge (x0 top value &optional (from 1) (rows 3))
               (loop for y from (+ top from) repeat rows
                     do (loop for x from (+ x0 width 1) to (+ x0 pitch -2)
                              do (setf (aref grey (+ (* y 600) x)) (float value 1d0))))))
      (dotimes (l 12)
        (dotimes (word 6)
          (let ((top (+ 20 (* l 38))) (left (+ 20 (* word 95))))
            (dotimes (stroke 9)
              (let ((x0 (+ left (* stroke pitch))))
                (let* ((v (+ (floor stroke 2) (* 4 word) (* 24 l)))
                       (second (and varied (oddp stroke) (< stroke 8))))
                  (loop for y from (+ top (if second (mod (floor v 96) 3) 0))
                        repeat (- height (if second (mod (floor v 32) 3) 0))
                        do (loop for dx from -1 to width
                                 do (setf (aref grey (+ (* y 600) x0 dx))
                                          (if (<= 0 dx (1- width))
                                              (float (+ ink (if second (* 20 (mod (floor v 8) 4)) 0)) 1d0) 140d0))))
                  (when (and varied (evenp stroke) (< stroke 8))
                    (bridge x0 top (- neck 20) (+ 1 (mod v (- height 4))) 2)))
                (if varied
                    nil
                    (case stroke
                      ((2 6) (bridge x0 top neck))
                      (0 (when (member l dark) (bridge x0 top ink)))
                      (1 (when (member l dark) (bridge x0 top neck))))))))))
      (when blur
        (let ((copy (copy-seq grey)))
          (loop for y from 1 below 499
                do (loop for x from 1 below 599
                         do (setf (aref grey (+ (* y 600) x))
                                  (/ (loop for dy from -1 to 1
                                           sum (loop for dx from -1 to 1
                                                     sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9))))))
      (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
        (dotimes (i (length out) out)
          (setf (aref out i)
                (max (or clipped 0) 0
                     (min 255 (round (if uniform
                                         (+ (aref grey i) (- (* (uniform) (1+ (* 2 sigma))) sigma 0.5))
                                         (+ (aref grey i) (* sigma (gauss)))))))))))))

(defun pal-laid (page fx fy tx ty w h &optional jitter)
  "PAGE (600 wide) with its W by H pixels at FX FY laid over those at TX TY,
pixel for pixel, as a picture editor lays a piece cut out; with JITTER,
each grey level moved by up to 2 either way, as saving again moves them.
PAGE."
  (dotimes (y h page)
    (dotimes (x w)
      (let ((g (aref page (+ (* (+ fy y) 600) fx x))))
        (setf (aref page (+ (* (+ ty y) 600) tx x))
              (if jitter (max 0 (min 255 (+ g (- (mod (* 7 (+ (* w y) x)) 5) 2)))) g))))))

(define-test (palimpsest palimpsest-joined-letters-cut)
  ;; strokes joined two by two at a neck of half-tones: cut apart there, each a piece as wide as a letter,
  ;; the neck in neither; the stroke alone left as it was; strokes joined by their ink's core not cut
  (let* ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-joined) 600 500)))
         (first-word (remove-if-not (lambda (g)
                                      (and (< (palimpsest::glyph-x0 g) 100) (< (palimpsest::glyph-y0 g) 40))) glyphs)))
    (is = 648 (length glyphs))
    (is = 288 (count :sure glyphs :key #'palimpsest::glyph-split))
    (is = 360 (count nil glyphs :key #'palimpsest::glyph-split))
    (is = 9 (length first-word))
    (dolist (neck '(41 73))
      (false (find-if (lambda (g) (<= (palimpsest::glyph-x0 g) neck (1- (palimpsest::glyph-x1 g)))) first-word)
             "the neck at ~d in a piece: ~s" neck
             (mapcar (lambda (g) (list (palimpsest::glyph-x0 g) (palimpsest::glyph-x1 g))) first-word))))
  ;; kerned pairs: the two joined by their core left together, an unsure piece; the next two cut off at their necks
  (let ((glyphs (palimpsest::picture-reading-glyphs
                 (palimpsest::picture-words (pal-joined :dark '(0 1 2 3 4 5 6 7 8 9 10 11)) 600 500))))
    (is = 576 (length glyphs))
    (is = 72
        (count-if (lambda (g)
                    (and (eq :unsure (palimpsest::glyph-split g))
                         (>= (- (palimpsest::glyph-x1 g) (palimpsest::glyph-x0 g)) 13))) glyphs))
    (is = 360 (count-if #'palimpsest::glyph-split glyphs))))

(define-test (palimpsest palimpsest-copies-in-joined-letters)
  ;; a stroke joined to its neighbour laid with the paper round it over another stroke so joined, and over
  ;; a stroke standing alone: both found, the same pixel for pixel, a strong sign, each named where it is
  (let ((page (pal-joined)))
    (pal-laid page (+ 20 95 16 -3) (+ 20 38 -2) (+ 20 (* 3 95) 48 -3) (+ 20 (* 6 38) -2) 9 22)
    (pal-laid page (+ 20 16 -3) (+ 20 (* 2 38) -2) (+ 20 (* 5 95) 64 -3) (+ 20 (* 9 38) -2) 9 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((copies (pal-finding findings :strong "4 characters of the scanned page are copies of one another")))
        (true copies "~s" (mapcar #'palimpsest::finding-words findings))
        (when copies
          (let ((details (format nil "~{~a~%~}" (palimpsest::finding-details copies)))
                (lefts '()))
            (cl-ppcre:do-register-groups ((#'parse-integer x)) ("pixels (\\d+)," details) (push x lefts))
            (dolist (x '(130 352 35 558))
              (true (find-if (lambda (l) (<= (abs (- l x)) 1)) lefts) "a character at x ~d: ~a" x details))
            (pal-has details "cut at a neck from a mark wider than a letter")
            (pal-has details "the same pixel for pixel over ")))
        (false (pal-finding findings :look "copies of one another") "~s"
               (mapcar #'palimpsest::finding-words findings)))))
  ;; a kerned pair laid over another, saved again on the way: alike, but the pieces' cut unsure, noted for
  ;; the record as near pairs of pieces are
  (let ((page (pal-joined :sigma 12 :uniform t :dark '(3 7))))
    (pal-laid page (+ 20 95 -3) (+ 20 (* 3 38) -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 17 22 t)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s"
             (mapcar #'palimpsest::finding-words
                     (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
      (false (pal-finding findings :look "nearer one another than prints of one character are"))
      (let ((noted (pal-finding findings :record "come near one another as clean prints of one character can")))
        (true noted "~s" (mapcar #'palimpsest::finding-words findings))
        (when noted
          (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details noted)) "no neck found to cut it at")
          (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details noted))
                   "a piece cut at a neck, or a mark wider than a letter, among them"))))))

(define-test (palimpsest palimpsest-joined-letters-honest)
  ;; honest pages of letters joined by the blur at necks, or by their ink (kerned pairs, ligatures), tight,
  ;; bold, clipped, sharp or blurred, at Gaussian grain of 1.5 to 3 and 5 and 8 and uniform of 6 and 12:
  ;; no strong sign, and nothing copied; nor where most letters touch, each pair its own way, which would
  ;; make prints of a letter seem further apart than they are were the joined marks measured with them
  (loop for (name . args) in '(("tight" :neck 120) ("tight, lighter necks" :neck 160) ("kerned" :dark (0 2 4 6 8 10))
                              ("bold" :width 5 :pitch 10) ("clipped" :ink 0 :clipped 20) ("sharp" :blur nil)
                              ("kerned, bold" :width 5 :pitch 10 :dark (0 1 2 3 4 5 6 7 8 9 10 11))
                              ("tight, necks at every height" :height 11 :pitch 9 :varied t)
                              ("tight, necks at every height, sharp" :height 11 :pitch 9 :varied t :blur nil))
        do (loop for (sigma uniform) in '((1.5 nil) (2 nil) (3 nil) (5 nil) (8 nil) (6 t) (12 t))
                 do (multiple-value-bind (pdf accounts findings)
                        (pal-read (pal-scanned (apply #'pal-joined :sigma sigma :uniform uniform args)))
                      (declare (ignore pdf accounts))
                      (false (find :strong findings :key #'palimpsest::finding-strength)
                             "~a, grain ~a~:[~; uniform~]: ~s"
                             name sigma uniform
                             (mapcar #'palimpsest::finding-words
                                     (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
                      (false (pal-finding findings nil "copies of one another") "~a, grain ~a~:[~; uniform~]: ~s"
                             name sigma uniform (mapcar #'palimpsest::finding-words findings))))))

;;; Pages of one file judged apart, and strips that prove a copy

(defun pal-scanned-pages (greys)
  "A PDF of the scanned pages GREYS, each 600 by 500 raw grey pixels laid over all of its page."
  (let ((n (length greys)))
    (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                (cons 2 (format nil "<< /Type /Pages /Kids [~{~d 0 R~^ ~}] /Count ~d >>"
                                                (loop for k below n collect (+ 10 (* 3 k))) n))
                                '(6 . "<< /Producer (Kea Scan) >>"))
                          (loop for grey in greys for k from 0
                                for page = (+ 10 (* 3 k))
                                append (list (cons page
                                                   (format nil
                                                           "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] ~
                                                            /Resources << /XObject << /Im1 ~d 0 R >> >> /Contents ~d 0 ~
                                                            R >>" (+ page 2) (+ page 1)))
                                             (list (+ page 1) :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                             (list (+ page 2)
                                                   :stream (format nil "/Type /XObject /Subtype /Image /Width 600 ~
                                                                        /Height 500 /ColorSpace /DeviceGray ~
                                                                        /BitsPerComponent 8") grey)))))))

(defun pal-few-lines (grey lines)
  "GREY, a page of 12 lines (PAL-JOINED), with all but its first LINES lines covered with paper. GREY."
  (loop for i from (* 600 (+ 20 (* lines 38) -4)) below (length grey) do (setf (aref grey i) 235))
  grey)

(define-test (palimpsest palimpsest-pages-judged-apart)
  ;; a file put together from a grainy scan and a page of a few lines with hardly any grain: the second page's
  ;; honest prints, alike for want of grain, are not judged by the first page's grain; it is said unchecked,
  ;; its paper and ink as a program draws them
  (let ((first (pal-joined :sigma 6 :uniform t)) (second (pal-few-lines (pal-joined :sigma 0.5 :seed 7) 3)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore pdf accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s"
             (mapcar #'palimpsest::finding-words
                     (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
      (true (pal-finding findings
                         :record (format nil "Copies and grain not checked on page 2: its paper and ink as flat as a ~
                                              program draws them, or as an app's clipping leaves them."))
            "~s" (mapcar #'palimpsest::finding-words findings)))
    ;; a stroke copied on the grainy page still found there
    (pal-laid first (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 9 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-page-glyphs (grey &optional (page 0) (picture 7))
  "The characters of the scanned page GREY (600 by 500), on PAGE of PICTURE, their grain measured (GRAIN-LEVELS)."
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words grey 600 500))))
    (dolist (g glyphs) (setf (palimpsest::glyph-page g) page (palimpsest::glyph-picture g) picture))
    (palimpsest::grain-levels glyphs)))

(define-test (palimpsest palimpsest-strips-on-a-jpeg-grid)
  ;; a stroke laid a whole number of JPEG blocks from its source: on a page kept as a JPEG, a block the
  ;; same there is marked as on the grid, worth a look; a block off the grid, or a page kept without loss, not
  (flet ((pairs (dy dct)
           (let ((page (pal-grained (pal-scan))))
             (pal-copy-box page page 114 58 122 (+ 58 dy))
             (palimpsest::same-strips (pal-page-glyphs page) :grids (mapcar (lambda (o) (list o 0 0)) dct)))))
    (let ((on (pairs 152 '(7))) (off (pairs 190 '(7))) (lossless (pairs 152 '())))
      (is = 1 (length on)) (is = 1 (length off)) (is = 1 (length lossless))
      (true (fourth (first on)))
      (false (fourth (first off)))
      (false (fourth (first lossless)))
      ;; the block's ink not clipped counted beside its size
      (true (<= 20 (third (third (first on)))))))
  ;; a block on the grid sharing more ink than honest pairs on faint or stepped pages reach (48 pixels) is
  ;; still worth a look, as copies on the grid share no more than honest pairs do
  (let ((page (pal-grained (pal-scan))))
    (pal-copy-box page page 114 58 122 210)
    (let ((ink (third (third (first (palimpsest::same-strips (pal-page-glyphs page) :grids '((7 0 0)))))))
          (copies (palimpsest::scan-reading-copies (palimpsest::copied-glyphs (pal-page-glyphs page) :dct '(7)))))
      (true (< 48 ink) "~d pixels of ink" ink)
      (true copies)
      (false (find :strong copies :key #'fifth) "~s" (mapcar #'fifth copies))))
  ;; strips whose core has no grain of its own are not set beside each other, though every print of a stroke
  ;; drawn without grain is the same there and far more than 64 share one number
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-scan) 600 500))))
    (dolist (g glyphs)
      (setf (palimpsest::glyph-page g) 0 (palimpsest::glyph-picture g) 7 (palimpsest::glyph-typical g) 17))
    (is = 0 (length (palimpsest::same-strips glyphs))))
  ;; past the most strips a character may give, those left out are counted, never dropped unsaid
  (let* ((wide 3200) (grey (make-array (* 14 (+ wide 4)) :element-type '(unsigned-byte 8))) (seed 5))
    (dotimes (i (length grey))
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (if (<= 3 (floor i (+ wide 4)) 10) (+ 40 (mod (ash seed -16) 40)) 235)))
    (let ((g (palimpsest::glyph-measured (palimpsest::make-glyph :pixels grey :width (+ wide 4) :x0 2 :y0 2
                                                                 :x1 (+ wide 2) :y1 12 :ink 5000 :page 0
                                                                 :picture 7 :typical 17))))
      (true (plusp (nth-value 1 (palimpsest::same-strips (list g g)))) "strips counted as left out"))))

(define-test (palimpsest palimpsest-many-copies-of-one-character)
  ;; one stroke laid over 70 others: every copy found by its strips, the same pixel for pixel, a strong sign;
  ;; no detail without its block's size
  (let ((page (pal-grained (pal-scan))))
    (loop for (line word stroke) in (append (loop for line from 1 to 11
                                                  append (loop for word below 6 collect (list line word 1)))
                                            (loop for line from 1 to 4 collect (list line 1 2)))
          do (pal-copy-box page page 114 58 (+ 20 (* word 95) (* 8 stroke) -1) (+ 20 (* line 38))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "71 characters of the scanned page are copies of one another")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        (dolist (f findings)
          (dolist (d (palimpsest::finding-details f))
            (false (search "NIL" d) "~a" d)))
        (when strong (pal-has (first (palimpsest::finding-details strong)) "the same pixel for pixel over "))))))

(define-test (palimpsest palimpsest-pieces-strong-only-by-their-pixels)
  ;; marks wider than letters, joined at their cores with no neck: unsure, not taken for letters
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-joined :neck 60) 600 500))))
    (is = 144 (count :unsure glyphs :key #'palimpsest::glyph-split)))
  ;; a stroke laid the same pixel for pixel over another, and the same stroke, saved again on the way, over one
  ;; of a pair the blur joins: the first two a strong sign; the piece, a stroke alone alike to it with the
  ;; paper about it, noted for the record as near pairs of pieces are, never in the strong group
  (let ((page (pal-joined :sigma 12 :uniform t)))
    (pal-laid page (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 5 38) -2) 9 22)
    (pal-laid page (+ 20 95 16 -3) (+ 20 38 -2) (+ 20 (* 2 95) 48 -3) (+ 20 (* 8 38) -2) 9 22 t)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "2 characters of the scanned page are copies of one another"))
            (look (pal-finding findings :record "come near one another as clean prints of one character can")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when strong (false (search "cut at a neck" (first (palimpsest::finding-details strong)))))
        (when look (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details look)) "cut at a neck"))))))

(defun pal-ledger (&key (sigma 1.75) (quality 50) (seed 31))
  "The grey octets of an honest page, 600 by 500 pixels, kept as a JPEG of QUALITY: six lines of print above
whose ink varies pixel by pixel, as toner does, and below a clean ledger of figures, each two strokes, a
comma (3 by 6 pixels) and a stroke 8 pixels apart. The figures are set on whole pixels 64 apart across
and 32 down, all on the JPEG's grid of 8 by 8 pixels. It is blurred by a 3 by 3 mean, then given Gaussian
grain of SIGMA."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0)) (s seed))
    (labels ((uniform () (setf s (mod (+ (* s 1103515245) 12345) 2147483648)) (/ (+ 1 s) 2147483649d0))
             (gauss () (* (sqrt (* -2 (log (uniform)))) (cos (* 2 pi (uniform)))))
             (stroke (x0 top tall ink &optional (toner 0))
               (loop for y from top repeat tall
                     do (loop for dx from -1 to 3
                              do (setf (aref grey (+ (* y 600) x0 dx))
                                       (+ (if (<= 0 dx 2) ink 140d0) (* toner (- (uniform) 0.5))))))))
      (dotimes (l 6)
        (dotimes (word 6)
          (dotimes (k 9)
            (stroke (+ 20 (* word 95) (* k 8)) (+ 20 (* l 38)) 18 60d0 100))))
      (loop for y from 256 below 480 by 32
            do (loop for x from 24 below 540 by 64
                     do (stroke x y 14 60d0) (stroke (+ x 8) y 14 60d0) (stroke (+ x 16) (+ y 8) 6 60d0)
                     (stroke (+ x 24) y 14 60d0)))
      (let ((copy (copy-seq grey)))
        (loop for y from 1 below 499
              do (loop for x from 1 below 599
                       do (setf (aref grey (+ (* y 600) x))
                                (/ (loop for dy from -1 to 1
                                         sum (loop for dx from -1 to 1
                                                   sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9)))))
      (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
        (dotimes (i (length out)) (setf (aref out i) (max 0 (min 255 (round (+ (aref grey i) (* sigma (gauss))))))))
        (pal-jpeg-saved out 0 0 600 500 :quality quality)))))

(define-test (palimpsest palimpsest-jpeg-likeness-worth-a-look)
  ;; a page kept as a JPEG, prints of varied toner above and a clean ledger on the JPEG's grid below: the
  ;; ledger's figures and commas come alike, but on a page kept as a JPEG likeness alone is never a strong sign
  (dolist (sigma '(1.75 2.5))
    (let* ((glyphs (pal-page-glyphs (pal-ledger :sigma sigma)))
           (copies (palimpsest::scan-reading-copies (palimpsest::copied-glyphs glyphs :dct '(7)))))
      (false (find :strong copies :key #'fifth) "grain ~a: ~d strong groups, of differences ~a" sigma
             (count :strong copies :key #'fifth) (mapcar #'second (remove :look copies :key #'fifth)))
      (true (every (lambda (c) (or (eq (fifth c) :look) (third c))) copies)))))

(define-test (palimpsest palimpsest-large-mark-measured)
  ;; a mark of more than 65,535 pixels, a line run together as a tight page set small and blurred can make,
  ;; is measured as any other
  (let* ((w 400) (h 200) (pixels (make-array (* (+ w 4) (+ h 4)) :element-type '(unsigned-byte 8) :initial-element 40)))
    (true (palimpsest::glyph-measured (palimpsest::make-glyph :pixels pixels :width (+ w 4) :x0 2 :y0 2
                                                              :x1 (+ w 2) :y1 (+ h 2) :ink (* w h))))))

(defun pal-stamp (&key (seed 17))
  "A stamp's bitmap, 140 by 56 grey octets: a frame 3 pixels wide round
five strokes 4 wide and 32 tall, 20 apart, of ink 60, on paper of 240, all
with grain of up to 12 either way, as a scanned stamp kept as a picture is."
  (let ((stamp (make-array (* 140 56) :element-type '(unsigned-byte 8))) (s seed))
    (dotimes (y 56 stamp)
      (dotimes (x 140)
        (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
        (let ((ink (or (< x 3) (>= x 137) (< y 3) (>= y 53)
                       (and (<= 12 y 43) (>= x 20) (< x 120) (< (mod (- x 20) 20) 4)))))
          (setf (aref stamp (+ (* y 140) x)) (+ (if ink 60 240) -12 (mod (ash s -16) 25))))))))

(defun pal-stamped-on (page stamp x0 y0)
  "PAGE (600 wide) with STAMP (140 by 56) laid on it at X0 Y0 with a Darken blend, as a program lays a stamp
on a scanned page. PAGE."
  (dotimes (y 56 page)
    (dotimes (x 140)
      (let ((at (+ (* (+ y0 y) 600) x0 x)))
        (setf (aref page at) (min (aref page at) (aref stamp (+ (* y 140) x))))))))

(define-test (palimpsest palimpsest-stamp-in-a-frame)
  ;; one stamp laid twice on a scanned page: its letters, apart from the frame round them, found the same
  ;; in every pixel of their ink, a strong sign, a stamp laid by software named among its causes
  (let* ((stamp (pal-stamp))
         (page (pal-stamped-on (pal-stamped-on (pal-few-lines (pal-joined :sigma 6 :uniform t) 6) stamp 40 300)
                               stamp 330 400)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "10 characters of the scanned page are copies of one another")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        ;; letters taller than the page's type: a stamp laid by software named as one honest cause
        (when strong
          (pal-has (palimpsest::finding-words strong)
                   "or a stamp, a logo or a signature laid on the pages by software"))
        (when strong
          (dolist (x '(60 80 100 120 140))
            (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details strong)) (format nil "pixels ~d,312 to " x))
            (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details strong))
                     (format nil "pixels ~d,412 to " (+ x 290)))))))))

(define-test (palimpsest palimpsest-mark-at-one-place-on-every-page)
  ;; a character laid at the same place on both pages of a file: a strong sign, a mark laid by software named
  ;; among its causes; a character copied elsewhere on a page: no such cause
  (let ((first (pal-joined :sigma 6 :uniform t)) (second (pal-joined :sigma 6 :uniform t :seed 5)))
    (dotimes (y 22)
      (dotimes (x 9) (setf (aref second (+ (* (+ 56 y) 600) 112 x)) (aref first (+ (* (+ 56 y) 600) 112 x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings
                         :strong (format nil "2 characters of the scanned pages are copies of one another, the grain ~
                                              of their ink too: copied within the scan, or a stamp, a logo or a ~
                                              signature laid on the pages by software."))
            "~s" (mapcar #'palimpsest::finding-words findings))))
  (let ((page (pal-joined :sigma 6 :uniform t)))
    (pal-laid page (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 9 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "copied within the scan, or typed onto it.") "~s"
            (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "laid on the pages by software")))))

(defun pal-ruled (page)
  "PAGE (PAL-JOINED, 600 by 500) with a table's rules a pixel wide drawn
about its lines and between its words, of grey 120, the scanner leaving a
pixel of them light every 40 along them. PAGE."
  (flet ((dot (x y) (setf (aref page (+ (* y 600) x)) (if (zerop (mod (+ x y) 40)) 230 120))))
    (dotimes (l 12)
      (let ((top (+ 20 (* l 38))))
        (loop for x from 4 below 596 do (dot x (- top 5)) (dot x (+ top 23)))))
    (dotimes (w 7)
      (let ((x (+ 20 (* w 95) -10)))
        (when (< x 596) (loop for y from 10 below 486 do (dot x y)))))
    page))

(define-test (palimpsest palimpsest-characters-among-thin-rules)
  ;; characters in a table ruled a pixel wide, its rules broken here and there: every character found apart
  ;; from the rules, and the page judged
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-ruled (pal-joined)) 600 500))))
    (true (<= 640 (length glyphs) 648) "~d characters" (length glyphs)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-ruled (pal-joined))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "not checked") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-born-digital-page-unjudged)
  ;; a grainy scan and a page drawn by a program in one file: the drawn page's characters, alike to the last
  ;; grey, never judged by the scan's grain, and taken as without grain of their own
  (let ((first (pal-joined :sigma 6 :uniform t)) (second (pal-joined :sigma 0 :blur nil :seed 9)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s"
             (mapcar #'palimpsest::finding-words
                     (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
      (true (pal-finding findings :record "not checked on page 2") "~s" (mapcar #'palimpsest::finding-words findings))
      (let* ((revision (aref (palimpsest::pdf-revisions pdf) 0))
             (scans (palimpsest::scan-pictures pdf revision))
             (glyphs (loop for scan in scans append (palimpsest::scan-picture-glyphs scan)))
             (drawn (remove-if-not (lambda (s) (< (palimpsest::scan-picture-paper s) 0.5)) scans)))
        (is = 1 (length drawn))
        (palimpsest::copied-glyphs glyphs
                                   :drawn (list (palimpsest::mark-object
                                                 (palimpsest::scan-picture-mark (first drawn)))))
        (true (every #'palimpsest::glyph-bare (remove-if-not (lambda (g) (= 1 (palimpsest::glyph-page g))) glyphs)))))))

(define-test (palimpsest palimpsest-smoothed-region-worth-a-look)
  ;; a region of a grainy scan smoothed by the scanner's software: its prints, near alike for the grain
  ;; smoothed away, worth a look at most, never a strong sign
  (let ((page (pal-joined :sigma 6 :uniform t)))
    (pal-blurred page 0 50 600 140 3)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s"
             (mapcar #'palimpsest::finding-words
                     (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq))))))

(define-test (palimpsest palimpsest-white-paper-scan-judged)
  ;; a grainy scan whose paper the scanner made pure white, a stroke laid over another: its paper is clean, but
  ;; its ink keeps its grain, and the copy is a strong sign, not taken for a page drawn by a program
  (let ((page (pal-joined :sigma 6 :uniform t)))
    (dotimes (i (length page)) (when (> (aref page i) 215) (setf (aref page i) 255)))
    (pal-laid page (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 9 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "no grain of their own")))))

(define-test (palimpsest palimpsest-pages-not-fully-read-said)
  ;; a page from which no words can be read is said unchecked; a page of which most lines run together, so
  ;; that few of its characters are found, says how many of about how many were checked
  (let ((dark (make-array (* 600 500) :element-type '(unsigned-byte 8))))
    (dotimes (i (length dark)) (setf (aref dark i) (if (< (mod i 600) 300) 40 230)))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-scanned-pages (list (pal-joined :sigma 6 :uniform t) dark)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings
                         :record "Copies and grain not checked on page 2: no words could be read from its picture.")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  (let ((page (pal-joined :sigma 6 :uniform t)))
    ;; a hatching of dots between lines 4 to 12 runs them into one band, too tall to be a line
    (loop for y from (+ 20 (* 3 38)) below 480
          do (loop for x from 20 below 580 when (zerop (mod (+ x y) 3)) do (setf (aref page (+ (* y 600) x)) 60)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "Copies and grain checked on ") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(defun pal-close-lines (&key (pitch 26) (lines 12) (sigma 6) bars (seed 61))
  "The grey octets of an honest scanned page, 600 by 500 pixels, of LINES lines PITCH rows apart, each of 6
words of 9 strokes 3 wide and 14 tall, the second stroke of each word rising 8 rows higher and the sixth
falling 8 lower, as ascenders and descenders do. At a PITCH under 30 those of one line touch those of the
next. With BARS, ten change bars 2 pixels wide and 100 tall down the right margin. Grain up to SIGMA
either way."
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 235)) (s seed))
    (dotimes (l lines)
      (let ((top (+ 20 (* l pitch))))
        (dotimes (word 6)
          (dotimes (k 9)
            (let ((x0 (+ 20 (* word 90) (* k 8)))
                  (from (if (= k 1) (- top 8) top)) (to (if (= k 5) (+ top 22) (+ top 14))))
              (loop for y from (max 0 from) below (min 500 to)
                    do (loop for dx from -1 to 3
                             do (setf (aref grey (+ (* y 600) x0 dx)) (if (<= 0 dx 2) 60 140)))))))))
    (when bars
      (dotimes (b 10)
        (loop for y from (+ 10 (* b 4)) repeat 100
              do (loop for x from (+ 560 (* b 3)) repeat 2 do (setf (aref grey (+ (* y 600) x)) 50)))))
    (dotimes (i (length grey) grey)
      (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
      (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) (- sigma) (mod (ash s -16) (1+ (* 2 sigma))))))))))

(define-test (palimpsest palimpsest-touching-lines-read)
  ;; lines set so close that ascenders and descenders touch: cut apart where they meet, and read
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-close-lines) 600 500))))
    (true (<= 640 (length glyphs) 648) "~d characters" (length glyphs)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-close-lines)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "not checked") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-unread-pages-told-why)
  ;; lines run together by a hatching between them that no cut can part, change bars beside them: no words
  ;; read, said alone, not that they run together nor are lost among rules
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (let ((page (pal-close-lines :bars t)))
                               (loop for y from 10 below 340
                                     do (loop for x from 15 below 550 when (zerop (mod (+ x y) 3))
                                              do (setf (aref page (+ (* y 600) x)) 60)))
                               page)))
    (declare (ignore pdf accounts))
    (true (pal-finding findings
                       :record "Copies and grain not checked on page 1: no words could be read from its picture.")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (is = 1 (count-if (lambda (f) (search "not checked" (palimpsest::finding-words f))) findings)
        "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; two pages of no words: their pictures
  (let ((dark (make-array (* 600 500) :element-type '(unsigned-byte 8))))
    (dotimes (i (length dark)) (setf (aref dark i) (if (< (mod i 600) 300) 40 230)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list dark (copy-seq dark))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings
                         :record (format nil "Copies and grain not checked on pages 1 and 2: no words could be read ~
                                              from their pictures."))
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-pictures-not-read-said)
  ;; a page scanned as JBIG2 whose data cannot be read: said for the record, not passed over
  (let ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                    '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                    '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] "
                                                                 "/Resources << /XObject << /Im1 7 0 R >> >> /Contents "
                                                                 "4 0 R >>"))
                                    '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                    '(6 . "<< /Producer (Kea Scan) >>")
                                    '(7 :stream #.(concatenate 'string "/Type /XObject /Subtype /Image /Width 600 "
                                                                       "/Height 500 /ColorSpace /DeviceGray "
                                                                       "/BitsPerComponent 8 /Filter /JBIG2Decode")
                                      "not a picture: never decoded")))))
    (multiple-value-bind (pdf accounts findings) (pal-read octets)
      (declare (ignore pdf accounts))
      (let ((said (pal-finding findings :record
                               "The scanned picture on page 1 was not read (its JBIG2 could not be read")))
        (true said "~s" (mapcar #'palimpsest::finding-words findings))
        (when said
          (is equal '(7) (palimpsest::finding-objects said))
          (pal-has (first (palimpsest::finding-details said)) "page 1: the picture Im1 (object 7)"))))))

;; Pages in black and white, kept as fax (CCITT)

(defun pal-black-and-white (grey &key (level 140))
  "GREY, a scanned page 600 by 500 pixels, made black and white: each pixel
darker than LEVEL black, the rest white."
  (map '(simple-array (unsigned-byte 8) (*)) (lambda (g) (if (< g level) 0 255)) grey))

(defun pal-fax-pdf (bits)
  "A PDF of one page whose picture is BITS (600 by 500 pixels, each 0 or
255) kept as fax (CCITT group 4), made by ImageMagick from a picture of one
bit a pixel written for it to a file of the test's own."
  (uiop:with-temporary-file (:pathname pbm :type "pbm" :keep nil)
    (uiop:with-temporary-file (:pathname pdf :type "pdf" :keep nil)
      (with-open-file (out pbm :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (pal-octets (format nil "P4~%600 500~%")) out)
        (let ((row (make-array 75 :element-type '(unsigned-byte 8))))
          (dotimes (y 500)
            (fill row 0)
            (dotimes (x 600)
              (when (zerop (aref bits (+ (* y 600) x)))
                (setf (ldb (byte 1 (- 7 (mod x 8))) (aref row (ash x -3))) 1)))
            (write-sequence row out))))
      (uiop:run-program (list "magick" (namestring pbm) "-compress" "Group4" (format nil "PDF:~a" (namestring pdf))))
      (alexandria:read-file-into-byte-vector pdf))))

(define-test (palimpsest palimpsest-fax-picture-read)
  ;; a page kept as fax is turned into pixels in the sandbox, the same as it was made, its words read and
  ;; said to be black and white, where copies cannot be told by a character alone; black and white alone
  ;; is parted at the level between them
  (is = 128 (palimpsest::otsu-level (let ((counts (make-array 256 :initial-element 0)))
                                      (setf (aref counts 0) 700 (aref counts 255) 9300)
                                      counts)))
  (let ((bits (pal-black-and-white (pal-grained (pal-scan)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-fax-pdf bits))
      (declare (ignore accounts))
      (let ((picture (find-if (lambda (o)
                                (let ((value (palimpsest::pdf-indirect-get o :value)))
                                  (and (equal (palimpsest::pdf-get value "/Subtype") "/Image")
                                       (eql (palimpsest::pdf-get value "/Width") 600))))
                              (palimpsest::pdf-found pdf))))
        (true picture)
        (when picture
          (true (equalp bits (palimpsest::picture-grey pdf (palimpsest::last-revision pdf)
                                                       (palimpsest::pdf-indirect-get picture :number))))))
      (false (pal-finding findings nil "was not read") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :record "it is black and white, where honest prints of one character")
            "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :record "drawn by a program rather than scanned")))))

(define-test (palimpsest palimpsest-stencil-ink-dark)
  ;; a stencil (an image mask) paints its ink where its samples are 0: read as black there, as a grey
  ;; picture of the same samples is, and as white there when its Decode says 1
  (flet ((greys (entries)
           (let* ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                              '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                              (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 16 ~
                                                                           /Height 1 ~a" entries)
                                                    (pal-octets #x0f #xff)))))
                  (pdf (palimpsest::read-pdf octets :name "test.pdf")))
             (palimpsest::picture-grey pdf (palimpsest::last-revision pdf) 7))))
    (let ((stencil (greys "/ImageMask true /BitsPerComponent 1"))
          (grey (greys "/ColorSpace /DeviceGray /BitsPerComponent 1")))
      (is equalp grey stencil)
      (is = 0 (aref stencil 0))
      (is = 255 (aref stencil 4)))
    (is = 255 (aref (greys "/ImageMask true /BitsPerComponent 1 /Decode [1 0]") 0))))

(define-test (palimpsest palimpsest-predicted-picture-read)
  ;; a page kept without loss behind PNG predictors, as Tesseract writes one, unpacks to a predictor
  ;; octet more for each row than its samples take: read all the same, every row's samples
  (let* ((width 4) (height 1100)
         (rows (apply #'pal-octets (loop for y below height append (list 0 10 (mod y 256) 200 255))))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                     (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width ~d /Height ~d ~
                                                                  /ColorSpace /DeviceGray /BitsPerComponent 8 ~
                                                                  /Filter /FlateDecode /DecodeParms << /Predictor 14 ~
                                                                  /Colors 1 /Columns ~d /BitsPerComponent 8 >>"
                                                             width height width)
                                           (pal-zlib-stored rows)))))
         (pdf (palimpsest::read-pdf octets :name "test.pdf")))
    (multiple-value-bind (grey why) (palimpsest::picture-grey pdf (palimpsest::last-revision pdf) 7)
      (true grey "~a" why)
      (when grey
        (is = (* width height) (length grey))
        (is = 3 (aref grey (+ (* 3 width) 1)))
        (is = 200 (aref grey (+ (* 1099 width) 2)))))))

(define-test (palimpsest palimpsest-fax-words-placed-roughly)
  ;; on a black and white page where a word sits is read too roughly to judge: a word four pixels below its
  ;; line is not named there, though it is on the grey page it came from
  (let ((findings (nth-value 2 (pal-read (pal-fax-pdf (pal-black-and-white (pal-grained (pal-scan :lowered t))))))))
    (false (pal-finding findings nil "baseline") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-fax-run-copied)
  ;; three strokes of a black and white page laid over another word, pixel for pixel, are named worth a
  ;; look; a stroke alone so laid is not, as honest prints come out alike so; the page untouched, nothing
  (flet ((copied (strokes)
           (let ((bits (pal-black-and-white (pal-grained (pal-scan)))))
             (loop for y from 18 below 42
                   do (loop for x from 113 below (+ 113 (* 8 strokes))
                            do (setf (aref bits (+ (* (+ y 266) 600) x 190)) (aref bits (+ (* y 600) x)))))
             (nth-value 2 (pal-read (pal-fax-pdf bits))))))
    (let* ((findings (copied 3))
           (run (pal-finding findings :look "characters the same pixel for pixel as")))
      (true run "~s" (mapcar #'palimpsest::finding-words findings))
      (when run
        (pal-has (palimpsest::finding-words run) "3 characters the same pixel for pixel as 3 others")
        (pal-has (palimpsest::finding-words run) "190 pixels across and 266 down")))
    (let ((findings (copied 1)))
      (false (pal-finding findings nil "characters the same pixel for pixel as")
             "~s" (mapcar #'palimpsest::finding-words findings)))
    (let ((findings (nth-value 2 (pal-read (pal-fax-pdf (pal-black-and-white (pal-grained (pal-scan))))))))
      (false (pal-finding findings nil "characters the same pixel for pixel as")
             "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-j2k (grey &key (width 600) (height 500))
  "GREY (WIDTH by HEIGHT grey levels) as a bare JPEG 2000 codestream, made by
ImageMagick from a picture written for it to a file of the test's own."
  (uiop:with-temporary-file (:pathname pgm :type "pgm" :keep nil)
    (uiop:with-temporary-file (:pathname j2k :type "j2k" :keep nil)
      (with-open-file (out pgm :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
        (write-sequence (pal-octets (format nil "P5~%~d ~d~%255~%" width height)) out)
        (write-sequence grey out))
      (uiop:run-program (list "magick" (namestring pgm) (format nil "J2K:~a" (namestring j2k))))
      (alexandria:read-file-into-byte-vector j2k))))

(define-test (palimpsest palimpsest-jpeg-2000-read)
  ;; a page scanned as JPEG 2000 is read as a JPEG is, in the sandbox: its words found, a word pasted named
  (let* ((codestream (pal-j2k (pal-scan :pasted t)))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                     '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 "
                                                                  "500] /Resources << /XObject << /Im1 7 0 R >> >> "
                                                                  "/Contents 4 0 R >>"))
                                     '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                     '(6 . "<< /Producer (Kea Scan) >>")
                                     (list 7
                                           :stream (format nil "/Type /XObject /Subtype /Image /Width 600 /Height 500 ~
                                                                /ColorSpace /DeviceGray /BitsPerComponent 8 /Filter ~
                                                                /JPXDecode")
                                           codestream)))))
    (true (equalp (pal-octets #xff #x4f #xff #x51) (subseq codestream 0 4)))
    (multiple-value-bind (pdf accounts findings) (pal-read octets)
      (declare (ignore accounts))
      (false (pal-finding findings :record "was not read") "~s" (mapcar #'palimpsest::finding-words findings))
      (is = 1 (length (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf))))
      (true (pal-finding findings nil "has 1 word unlike the rest of it") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-signature-over-close-lines)
  ;; signatures scrawled across sixteen lines set so close that they touch, each over two or three lines: those
  ;; they run across are left unread, the rest of the page read
  (let ((page (pal-close-lines :lines 16)))
    (dolist (at '((60 . 100) (180 . 220) (120 . 340)))
      (loop with last = nil
            for x from (car at) below (+ (car at) 360)
            for y = (+ (cdr at) (round (+ (* 17 (sin (/ x 10.0))) (* 7 (sin (/ x 3.2))))))
            do (loop for row from (min y (or last y)) to (max y (or last y))
                     do (loop for dx below 5 do (setf (aref page (+ (* row 600) x dx)) 50)))
               (setf last y)))
    (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words page 600 500))))
      (true (<= 400 (length glyphs) 864) "~d characters" (length glyphs)))))

(define-test (palimpsest palimpsest-lines-left-unread-said)
  ;; one signature across lines that touch: most of the page read, and the band it left unread said
  (let ((page (pal-close-lines :lines 16)))
    (loop with last = nil
          for x from 60 below 420
          for y = (+ 100 (round (+ (* 17 (sin (/ x 10.0))) (* 7 (sin (/ x 3.2))))))
          do (loop for row from (min y (or last y)) to (max y (or last y))
                   do (loop for dx below 5 do (setf (aref page (+ (* row 600) x dx)) 50)))
             (setf last y))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((record (pal-finding findings :record "Copies and grain checked on ")))
        (true record "~s" (mapcar #'palimpsest::finding-words findings))
        (when record
          (pal-has (palimpsest::finding-words record) "of lines run together")
          (pal-has (palimpsest::finding-words record) "left unread, the rest of the page read."))))))

(define-test (palimpsest palimpsest-lines-beside-a-logo-read)
  ;; a logo beside the first three lines, its ink joining their rows into one band: the three lines read apart
  ;; from it, each word of the second line a word of its own line, its characters letters
  (let ((page (pal-typeset (append (mapcar (lambda (line) (subseq line 0 2)) (subseq *pal-honest-lines* 0 3))
                                   (subseq *pal-honest-lines* 3)))))
    (loop for y from 18 below 119
          do (loop for x from 400 below 501
                   when (<= 34 (+ (abs (- x 450)) (abs (- y 68))) 50) do (setf (aref page (+ (* y 600) x)) 40)))
    (let* ((reading (palimpsest::picture-words page 600 500))
           (second (remove-if-not (lambda (w) (<= 55 (palimpsest::word-box-y0 w) 60))
                                  (palimpsest::picture-reading-words reading))))
      (is = 2 (length second) "~s" (mapcar (lambda (w) (list (palimpsest::word-box-x0 w) (palimpsest::word-box-y0 w)
                                                             (palimpsest::word-box-y1 w)))
                                           (palimpsest::picture-reading-words reading)))
      (true (every (lambda (g) (< (- (palimpsest::glyph-y1 g) (palimpsest::glyph-y0 g)) 30))
                   (remove-if-not (lambda (g) (member (palimpsest::glyph-word g) second))
                                  (palimpsest::picture-reading-glyphs reading)))))))

(define-test (palimpsest palimpsest-light-text-beside-a-dark-heading-read)
  ;; a page printed in a light ink under one heading in a dark one: its light lines are its words, not marks too
  ;; faint to be letters
  (let ((page (pal-typeset *pal-honest-lines*)))
    (loop for i from (* 50 600) below (length page)
          do (setf (aref page i) (- 235 (round (* 3/5 (- 235 (aref page i)))))))
    (let ((words (palimpsest::picture-reading-words (palimpsest::picture-words page 600 500))))
      (true (>= (count-if (lambda (w) (> (palimpsest::word-box-y0 w) 50)) words) 30) "~d words" (length words)))))

(define-test (palimpsest palimpsest-some-light-marks-beside-dark-ones-kept-as-before)
  ;; a page of which two fifths of the lines are printed in a light ink, the rest dark: the light marks stay under
  ;; the bar that parts marks too faint to be letters, as on any page of mostly dark ink
  (let* ((page (pal-typeset *pal-honest-lines*))
         (all (palimpsest::picture-reading-words (palimpsest::picture-words (pal-typeset *pal-honest-lines*) 600 500)))
         (cut (nth (floor (* 3 (length all)) 5) (sort (mapcar #'palimpsest::word-box-y0 all) #'<))))
    (loop for i from (* cut 600) below (length page)
          do (setf (aref page i) (- 235 (round (* 3/5 (- 235 (aref page i)))))))
    (let ((words (palimpsest::picture-reading-words (palimpsest::picture-words page 600 500))))
      (true (< (count-if (lambda (w) (>= (palimpsest::word-box-y0 w) cut)) words) (floor (length all) 10))
            "~d words of ~d" (length words) (length all)))))

(define-test (palimpsest palimpsest-columns-of-different-leading)
  ;; two columns of lines that touch, set at different leading, so that the lines of one fall between those of
  ;; the other: each column's lines found apart, and read
  (let ((page (pal-close-lines :pitch 26)) (other (pal-close-lines :pitch 23 :seed 62)))
    (dotimes (y 500)
      (loop for x from 280 below 600
            do (setf (aref page (+ (* y 600) x))
                     (if (< x 380) (aref page (+ (* y 600) 279)) (aref other (+ (* y 600) x))))))
    (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words page 600 500))))
      (true (<= 450 (length glyphs) 540) "~d characters" (length glyphs)))))

(defun pal-ledger-page (&key (sigma 6) (seed 71))
  "The grey octets of an honest scanned page, 600 by 500 pixels: six lines of dense print (PAL-JOINED's)
above, and below six rows of a ledger, each of five amounts of three figures drawn as a 0 is, 8 pixels
wide and 12 tall, 10 apart. Their top and bottom strokes are 2 rows deep and their sides 2 pixels wide,
so that their top and bottom rows hold more ink than their middle. Grain up to SIGMA either way."
  (let ((grey (pal-few-lines (pal-joined :sigma 0 :blur nil) 6)) (s seed))
    (dotimes (r 6)
      (dotimes (k 15)
        (let ((x0 (+ 30 (* (floor k 3) 110) (* (mod k 3) 10))) (y0 (+ 270 (* r 36))))
          (dotimes (y 12)
            (dotimes (x 8)
              (when (or (< y 2) (>= y 10) (< x 2) (>= x 6))
                (setf (aref grey (+ (* (+ y0 y) 600) x0 x)) 60)))))))
    (dotimes (i (length grey) grey)
      (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
      (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) (- sigma) (mod (ash s -16) (1+ (* 2 sigma))))))))))

(defun pal-total-row-page ()
  "The grey octets of an honest scanned page, 600 by 500 pixels: six lines of dense print (PAL-JOINED's)
above, and below a total's row: on the left a label of nine strokes 3 pixels wide and 18 tall, as the
letters above are, from row 308, and at the far right an amount of five bold figures 6 pixels wide and 9
tall from row 300, so that the row's band runs from the amount's top to the label's foot."
  (let ((grey (pal-few-lines (pal-joined :sigma 0 :blur nil) 6)))
    (flet ((box (x0 y0 w h)
             (dotimes (y h) (dotimes (x w) (setf (aref grey (+ (* (+ y0 y) 600) x0 x)) 50)))))
      (dotimes (k 9) (box (+ 30 (* k 8)) 308 3 18))
      (dotimes (k 5) (box (+ 450 (* k 10)) 300 6 9)))
    grey))

(define-test (palimpsest palimpsest-small-amount-beside-taller-label)
  ;; an amount in figures half as tall as the label on its row, set higher at the far end of it, is read as a
  ;; word, not left out as a dash would be
  (let ((words (remove-if-not (lambda (w) (>= (palimpsest::word-box-y0 w) 280))
                              (palimpsest::picture-reading-words
                               (palimpsest::picture-words (pal-total-row-page) 600 500)))))
    (is equal '(30 450) (sort (mapcar #'palimpsest::word-box-x0 words) #'<))))

(define-test (palimpsest palimpsest-ledger-rows-not-cut)
  ;; a ledger below dense print: its rows of figures, inkier at their top and bottom than in their middle, are
  ;; no two lines and are never cut through; an amount of three figures laid over another is found, a strong sign
  (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-ledger-page) 600 500))))
    (let ((figures (remove-if-not (lambda (g) (>= (palimpsest::glyph-y0 g) 260)) glyphs)))
      (is = 90 (length figures))
      (true (every (lambda (g) (>= (- (palimpsest::glyph-y1 g) (palimpsest::glyph-y0 g)) 12)) figures))))
  (let ((page (pal-ledger-page)))
    (pal-laid page 27 268 137 304 32 16)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "6 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-raw-pixels-read)
  ;; a picture's samples made grey levels whatever their colours and depth
  (flet ((grey (data width bits kind &optional table invert)
           (coerce (palimpsest::raw-grey (coerce data '(simple-array (unsigned-byte 8) (*)))
                                         width 1 bits kind table invert) 'list)))
    (is equal '(0 255 128) (grey '(0 255 128) 3 8 :grey))
    (is equal '(76 150 29) (grey '(255 0 0  0 255 0  0 0 255) 3 8 :rgb))
    (is equal '(255 0) (grey '(0 0 0 0  0 0 0 255) 2 8 :cmyk))
    (is equal '(0 255 0 255 0 255 0 255) (grey '(#b01010101) 8 1 :grey))
    (is equal '(255 0 255 0 255 0 255 0) (grey '(#b01010101) 8 1 :grey nil t))
    (is equal '(76 255)
        (grey '(0 1) 2 8 :indexed (list :rgb 1 (coerce '(255 0 0 255 255 255) '(simple-array (unsigned-byte 8) (*)))))))
  ;; too short for its size: none
  (false (palimpsest::raw-grey (make-array 2 :element-type '(unsigned-byte 8)) 3 1 8 :grey nil nil)))

;;; Pictures read from hostile dictionaries

(define-test (palimpsest palimpsest-picture-key-bounded)
  ;; a picture whose /Decode is an array of references to arrays of references, four deep: each
  ;; object followed once, the key made at once
  (let* ((levels 4) (width 60)
         (objects (append (list (list 10
                                      :stream (format nil "/Type /XObject /Subtype /Image /Width 1 /Height 1 ~
                                                           /BitsPerComponent 8 /ColorSpace /DeviceGray /Decode 11 0 R")
                                      (pal-octets 0)))
                          (loop for k below levels
                                collect (cons (+ 11 k)
                                              (format nil "[~{~a~^ ~}]"
                                                      (make-list width :initial-element
                                                                 (if (< k (1- levels))
                                                                     (format nil "~d 0 R" (+ 12 k))
                                                                     "1")))))))
         (pdf (palimpsest::read-pdf (pal-save (pal-original) objects) :name "test.pdf"))
         (revision (palimpsest::last-revision pdf))
         (start (get-internal-real-time)))
    (multiple-value-bind (place key) (palimpsest::picture-octets pdf revision 10)
      (true place)
      (is equal '(:object . 10) (assoc :object (cddr key))))
    (true (< (- (get-internal-real-time) start) internal-time-units-per-second))))

(define-test (palimpsest palimpsest-palettes-read-safely)
  ;; a palette whose base is itself, a palette of codes past an octet, one too short, and a negative
  ;; highest index: none read, each with why, nothing signalled
  (let* ((pdf (palimpsest::read-pdf (pal-save (pal-original) '((10 . "[/Indexed 10 0 R 1 <0000>]"))) :name "test.pdf"))
         (revision (palimpsest::last-revision pdf)))
    (false (palimpsest::colour-space-of pdf revision '(:ref 10 0))))
  (dolist (space (list (list "/Indexed" "/DeviceRGB" 1
                             (coerce (list (code-char 256) (code-char 512) #\a #\b #\c #\d) 'string))
                       (list "/Indexed" "/DeviceRGB" 1 "abc")
                       (list "/Indexed" "/DeviceGray" -1 "ab")))
    (is equal "its palette cannot be read" (nth-value 2 (palimpsest::colour-space-of nil nil space)) "~s" space))
  (is eq :indexed (palimpsest::colour-space-of nil nil (list "/Indexed" "/DeviceGray" 1 "ab"))))

(define-test (palimpsest palimpsest-picture-stream-held-to-its-samples)
  ;; a picture's data is unpacked no further than its depth and colours need
  (is = 1 (palimpsest::picture-sample-bits nil nil '(("/BitsPerComponent" . 1) ("/ColorSpace" . "/DeviceGray"))))
  (is = 1 (palimpsest::picture-sample-bits nil nil '(("/ImageMask" . "true"))))
  (is = 24 (palimpsest::picture-sample-bits nil nil '(("/BitsPerComponent" . 8) ("/ColorSpace" . "/DeviceRGB"))))
  (is = 64 (palimpsest::picture-sample-bits nil nil '())))

(define-test (palimpsest palimpsest-booleans-read)
  ;; true as a dictionary writes it, and as a content stream does
  (true (palimpsest::pdf-true-p "true"))
  (true (palimpsest::pdf-true-p :true))
  (false (palimpsest::pdf-true-p "false"))
  (false (palimpsest::pdf-true-p "/true")))

(define-test (palimpsest palimpsest-copy-trailer-without-startxref)
  ;; a copy of a revision that names no cross-reference section goes back to none
  (is equal '("/Info 7 0 R" "/Root 1 0 R")
      (palimpsest::copy-trailer-entries (palimpsest::make-revision :trailer '(("/Root" :ref 1 0))) 7 0))
  (is equal '("/Info 7 0 R" "/Root 1 0 R" "/Prev 400")
      (palimpsest::copy-trailer-entries (palimpsest::make-revision :trailer '(("/Root" :ref 1 0)) :startxref 500)
                                        7 100)))

(define-test (palimpsest palimpsest-odd-glyph-names-and-maps)
  ;; glyph names with a sign or a letter among their digits stand for nothing
  (dolist (name '("uni-123" "uni00zz" "u-1234" "u12zz4"))
    (false (palimpsest::glyph-character name) "~a" name))
  (is string= "A" (palimpsest::glyph-character "uni0041"))
  ;; a ToUnicode range whose text is long, and a code of five octets: passed over
  (let ((map (palimpsest::unicode-map
              (pal-octets (format nil
                                  "begincmap 1 beginbfrange <00> <ff> <~a> endbfrange 1 beginbfchar <0102030405> ~
                                   <0041> <41> <0042> endbfchar endcmap"
                                  (make-string 200 :initial-element #\4))))))
    (is = 1 (hash-table-count map))
    (is string= "B" (gethash #x41 map)))
  ;; a tint function raising to a power past what a number holds: none, nothing signalled
  (false (palimpsest::tint-function-output nil nil
                                           '(("/FunctionType" . 2) ("/Domain" 0 1d300) ("/C0" 0) ("/C1" 1)
                                             ("/N" . 1d300))
                                           '(1d300)))
  (is equal '(0.25d0) (palimpsest::tint-function-output nil nil '(("/FunctionType" . 2) ("/C0" 0) ("/C1" 1) ("/N" . 2))
                                                        '(0.5))))

(define-test (palimpsest palimpsest-text-placed-off)
  ;; a figure a little above its line; a superscript, smaller, left alone
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           (format nil "BT /F1 12 Tf 72 700 Td (Total due for fence and gate: $) Tj ET ~
                                                        BT /F1 12 Tf 250 700.6 Td (8,250.00) Tj ET ")
                                           (format nil "BT /F1 12 Tf 72 680 Td (Paid in full on the day it was due) Tj ~
                                                        /F1 7 Tf 3 Ts (note) Tj 0 Ts ET"))))
    (declare (ignore pdf accounts))
    (let ((above (pal-finding findings :look "sits a little above or below")))
      (true above "~s" (mapcar #'palimpsest::finding-words findings))
      (when above
        (is equal '("\"8,250.00\" 0.60 pt above its line (4 @76 font 5)") (palimpsest::finding-details above)))))
  ;; a line starting a little right of the margin four others share; an indent of 18 points left alone
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "~{BT /F1 12 Tf ~a ~d Td (~a) Tj ET ~}"
                                      '(72 700 "Invoice for Kea Builders" 72 680 "To Tui Gardens Ltd"
                                        72.6 660 "Fence repair, 12 m"
                                        72 640 "New gate and latch" 72 620 "Total: $1,250.00" 90
                                        600 "Paid with thanks"))))
    (declare (ignore pdf accounts))
    (let ((off (pal-finding findings :look "set a little off the edge")))
      (true off "~s" (mapcar #'palimpsest::finding-words findings))
      (when off
        (is equal '("\"Fence repair, 12 m\" starts at x 72.60, 4 other lines at 72.00 (4 @119 font 5)")
            (palimpsest::finding-details off)))))
  ;; an amount a little off a right-aligned column, its font giving its widths; with no widths, ends are not judged
  (flet ((column (widths)
           (let ((content (with-output-to-string (s)
                            (loop for (item amount off)
                                  in '(("Posts" "480" 0) ("Rails" "360" 0) ("Gate" "210" 0.6)
                                       ("Labour" "200" 0) ("Total" "1250" 0))
                                  for y downfrom 700 by 20
                                  do (format s "BT /F1 12 Tf 72 ~d Td (~a) Tj ET BT /F1 12 Tf ~,2f ~d Td (~a) Tj ET "
                                             y item (+ (- 500 (* (length amount) 556/1000 12)) off) y amount)))))
             (pal-one-page content :fonts "/F1 7 0 R"
                                   :more (list (cons 7
                                                     (format nil
                                                             "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica~@[ ~
                                                              /FirstChar 48 /LastChar 57 /Widths [~{~d~^ ~}]~] >>"
                                                               (and widths (make-list 10 :initial-element 556)))))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (column t))
      (declare (ignore pdf accounts))
      (let ((off (pal-finding findings :look "set a little off the edge")))
        (true off "~s" (mapcar #'palimpsest::finding-words findings))
        (when off
          (is equal '("\"Gate 210\" ends at x 500.60, 4 other lines at 500.00 (4 @201 font 7)")
              (palimpsest::finding-details off)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (column nil))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "set a little off the edge")))))

(define-test (palimpsest palimpsest-scanned-word-placed-off)
  ;; a word of a scan 4 pixels below its line, within a paste's reach, is named with its place and how often
  ;; honest files show such words; a whole page scanned crooked is not
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :lowered t)))
    (declare (ignore pdf accounts))
    (let ((lower (pal-finding findings :look "a lower baseline")))
      (true lower "~s" (mapcar #'palimpsest::finding-words findings))
      (when lower
        (pal-has (palimpsest::finding-words lower) "Honest files show this too")
        (pal-has (first (palimpsest::finding-details lower)) "its baseline 4.0 pixels below its line's"))))
  ;; a word both pasted and lowered on a scan with grain: two measures apart agree, named so, worth a look
  ;; and never a strong sign, as honest words printed in another pass are both too
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (let ((page (pal-grained (pal-scan :lowered t))) (crisp (pal-scan :pasted t :lowered t)))
                               ;; the word laid crisp on clean paper after the page was scanned
                               (loop for y from 169 below 197
                                     do (loop for x from 207 below 283
                                              do (setf (aref page (+ (* y 600) x)) (aref crisp (+ (* y 600) x)))))
                               page)))
    (declare (ignore pdf accounts))
    (let ((both (pal-finding findings :look "both in how it looks and where it sits")))
      (true both "~s" (mapcar #'palimpsest::finding-words findings))
      (when both
        (pal-has (palimpsest::finding-words both) "darker ink")
        (pal-has (palimpsest::finding-words both) "a lower baseline")
        (pal-has (first (palimpsest::finding-details both)) "its baseline 4.0 pixels below its line's")))
    (false (pal-finding findings :strong "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))
    (is = 1 (count-if (lambda (f) (search "unlike the rest of it" (palimpsest::finding-words f))) findings)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :tilt 0.012)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))))

;;; Pages photographed

(defun pal-bowed (&key (bow 8) (droop 0) (light 0) form pasted)
  "The grey octets of a page as a phone photographs it, 600 by 500 pixels: 12 lines of 5 words of 13
strokes, drawn as PAL-SCAN draws them, each line arched BOW pixels down at its middle, as paper curled
over its width bows them. Each line falls DROOP pixels more towards its right end, steeply, as paper
curling off the edge of a desk falls. The page is lighter by up to LIGHT grey levels towards the right.
With FORM, its last four lines are a form, a label at the left and its value at the middle set 6 pixels
lower on every line. There is grain over all of it (PAL-GRAINED). With PASTED, the third word of the
fourth line is then laid over it crisp (grey 0, no fading) on clean paper (grey 250), 4 pixels lower than
its line."
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 230)))
    (labels ((fall (x) (round (+ (* bow (- 1 (expt (/ (- x 300) 300) 2))) (* droop (expt (/ (- x 20) 560) 4)))))
             (word (line word lower crisp)
               (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 115))))
                 (when crisp
                   (loop for x from (- left 3) below (+ left 105)
                         do (loop for y from (- top 3) below (+ top bow 25)
                                  do (setf (aref grey (+ (* y 600) x)) 250))))
                 (dotimes (stroke 13)
                   (let ((x0 (+ left (* stroke 8))))
                     (loop for y from (+ top lower (fall x0)) repeat 18
                           do (loop for dx from -1 to 3
                                    do (setf (aref grey (+ (* y 600) x0 dx))
                                             (cond (crisp (if (<= 0 dx 2) 0 250))
                                                   ((<= 0 dx 2) 60)
                                                   (t 140))))))))))
      (dotimes (line 12)
        (dotimes (w 5)
          (when (or (not form) (< line 8) (member w '(0 3)))
            (word line w (if (and form (>= line 8) (= w 3)) 6 0) nil))))
      (dotimes (i (length grey))
        (setf (aref grey i) (min 255 (+ (aref grey i) (round (* light (mod i 600)) 600)))))
      (pal-grained grey)
      (when pasted (word 3 2 4 t))
      grey)))

(define-test (palimpsest palimpsest-photographed-lines-bowed)
  ;; lines bowed by curled paper, light changing across the page: no word unlike the rest; a form whose values
  ;; sit lower than their labels on every line, on a page whose bow all its lines share: the labels and values
  ;; named in pairs at most, worth a look, one of each moved or a form filled in; a word pasted onto such a
  ;; page, crisp and stepped down from its neighbours, still found
  (loop for (bow light droop) in '((8 0 0) (12 0 0) (8 20 0) (0 0 12) (4 0 16))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-scanned (pal-bowed :bow bow :light light :droop droop)))
             (declare (ignore pdf accounts))
             (false (pal-finding findings nil "unlike the rest of it") "bow ~d, droop ~d, light ~d: ~s"
                    bow droop light (mapcar #'palimpsest::finding-words findings))))
  (loop for (bow light) in '((8 0) (12 20))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-scanned (pal-bowed :bow bow :light light :form t)))
             (declare (ignore pdf accounts))
             (false (pal-finding findings :strong "unlike the rest of it"))
             (let ((unlike (pal-finding findings nil "unlike the rest of it")))
               (when unlike
                 (true (every (lambda (d) (search "one of them was moved" d)) (palimpsest::finding-details unlike))
                       "bow ~d, light ~d, form: ~s" bow light (palimpsest::finding-details unlike))))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-bowed :bow 12 :light 20 :pasted t)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "unlike the rest of it"))
    (let ((pasted (pal-finding findings
                               :look "has 1 word unlike the rest of it both in how it looks and where it sits")))
      (true pasted "~s" (mapcar #'palimpsest::finding-words findings))
      (when pasted
        (pal-has (palimpsest::finding-words pasted) "a lower baseline")
        (pal-has (first (palimpsest::finding-details pasted)) "pixels 250,")))))

(define-test (palimpsest palimpsest-words-judged-only-with-grain)
  ;; a word pasted and lowered on a picture whose characters are all alike (no grain to judge, as a
  ;; screenshot's or a chart's): worth a look at most, and said why
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :pasted t :lowered t)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :look "its grain too faint to tell prints apart, or drawn by a program"))))

(define-test (palimpsest palimpsest-pieces-on-scans-of-faint-grain)
  ;; a word saved as a JPEG on its own grid and laid back on a scan whose grain is too faint to judge
  ;; copies: a strong sign still, the blocks needing no grain of the prints
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-jpeg-saved (pal-noisy-scan :sigma 1.5) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :record "the scan's grain is too faint to tell prints apart") "~s"
          (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :look "carries JPEG blocks of its own") "~s"
          (mapcar #'palimpsest::finding-words findings)))
  ;; the same on a picture drawn by a program, its paper and its ink each one grey, as a screenshot's are:
  ;; worth a look at most
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-jpeg-saved (pal-noisy-scan :sigma 0) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "JPEG blocks") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-pieces-laid-on-every-page)
  ;; a region with JPEG blocks of its own at the same place on both pages, as a logo laid by an office's
  ;; software: worth a look, the honest cause named; on one page alone, not the laid cause
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned-pages (list (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)
                                         (pal-jpeg-saved (pal-grained (pal-scan) 4242) 207 169 76 24))))
    (declare (ignore pdf accounts))
    (let ((laid (remove-if-not (lambda (f) (and (eq :look (palimpsest::finding-strength f))
                                                (search "carries JPEG blocks of its own"
                                                        (palimpsest::finding-words f))))
                               findings)))
      (is = 2 (length laid) "~s" (mapcar #'palimpsest::finding-words findings))
      (dolist (f laid)
        (pal-has (palimpsest::finding-words f) "laid into the page by the software that made it")
        (pal-has (palimpsest::finding-rule f) "Whether such a mark belongs there is for the examiner to judge"))))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (let ((piece (pal-finding findings :look "carries JPEG blocks of its own")))
      (true piece)
      (when piece (false (search "a stamp, a logo" (palimpsest::finding-words piece)))))))

(define-test (palimpsest palimpsest-jpeg-piece-backed)
  ;; a stamp of a scan kept as a JPEG, laid by software into a page kept without loss: its blocks of its own
  ;; worth a look, never strong alone
  (let ((page (pal-noisy-scan :sigma 3))
        (stamp (make-array (* 76 24))))
    (let ((other (pal-noisy-scan :sigma 3 :seed 99)))
      (dotimes (y 24) (dotimes (x 76) (setf (aref stamp (+ (* y 76) x)) (aref other (+ (* (+ 169 y) 600) 207 x))))))
    (pal-jpeg-saved stamp 0 0 76 24 :width 76)
    (dotimes (y 24) (dotimes (x 76) (setf (aref page (+ (* (+ 169 y) 600) 207 x)) (aref stamp (+ (* y 76) x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "carries JPEG blocks of its own") "~s"
            (mapcar #'palimpsest::finding-words findings))
      (false (find :strong findings :key #'palimpsest::finding-strength) "~s"
             (mapcar #'palimpsest::finding-words findings))))
  ;; the same region saved as a JPEG, then a stroke from elsewhere on the page laid over one within it the
  ;; same pixel for pixel: two signs on one region, strong
  (let ((page (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)))
    (pal-laid page (+ 20 95 8 -2) (+ 20 (* 7 38) -2) (+ 20 (* 2 95) (* 3 8) -2) (+ 20 (* 4 38) -2) 6 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((piece (pal-finding findings
                                :strong (format nil "carries JPEG blocks of its own, which the rest of the scan lacks, ~
                                                     and another sign"))))
        (true piece "~s" (mapcar #'palimpsest::finding-words findings))
        (when piece (pal-has (palimpsest::finding-rule piece) "copies of others the same pixel for pixel"))))))

(defun pal-date-stamp (&key (quality 75) (figures '(1 1 1 1 1 1 1 1 1 1)))
  "A date stamp as office software lays it, 192 by 40 pixels of grainy paper: a frame and ten FIGURES 16
pixels apart, each of two shapes (0 a box, 1 a taller box) kept once with the paper about it, a cell of
16 by 24 pixels on the JPEG's grid, and drawn the same every time. It is saved as a JPEG of QUALITY."
  (let ((stamp (make-array (* 192 40))) (seed 7))
    (flet ((grain (base) (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648)) (+ base (mod (ash seed -16) 21))))
      (dotimes (i (length stamp)) (setf (aref stamp i) (grain 220)))
      (dotimes (x 192)
        (dotimes (k 3) (setf (aref stamp (+ (* k 192) x)) (grain 60) (aref stamp (+ (* (- 39 k) 192) x)) (grain 60))))
      (dotimes (y 40)
        (dotimes (k 3) (setf (aref stamp (+ (* y 192) k)) (grain 60) (aref stamp (+ (* y 192) (- 191 k))) (grain 60))))
      ;; the two shapes once, their paper with them, then laid: each figure of a shape the same pixel for pixel
      (let ((shapes (loop repeat 2
                          collect (let ((cell (make-array (* 16 24))))
                                    (dotimes (i (length cell) cell) (setf (aref cell i) (grain 220)))))))
        (loop for shape in shapes for top in '(6 3)
              do (loop for y from top below 21
                       do (loop for x from 3 below 13
                                when (or (< y (+ top 3)) (> y 17) (< x 6) (> x 9))
                                  do (setf (aref shape (+ (* y 16) x)) (grain 60)))))
        (loop for figure in figures for k from 0
              do (let ((shape (nth figure shapes)))
                   (dotimes (y 24)
                     (dotimes (x 16)
                       (setf (aref stamp (+ (* (+ 8 y) 192) 16 (* k 16) x)) (aref shape (+ (* y 16) x)))))))))
    (pal-jpeg-saved stamp 0 0 192 40 :width 192 :quality quality)))

(define-test (palimpsest palimpsest-date-stamp-honest)
  ;; a scan kept without loss with a date stamp of repeated figures laid in its margin by software, the stamp
  ;; kept as a JPEG: its figures alike on the JPEG's own grid, no strong sign
  (dolist (quality '(75))
    (let ((page (pal-grained (pal-scan))) (stamp (pal-date-stamp :quality quality)))
      (dotimes (y 40) (dotimes (x 192) (setf (aref page (+ (* (+ 460 y) 600) 200 x)) (aref stamp (+ (* y 192) x)))))
      (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
        (declare (ignore pdf accounts))
        (false (find :strong findings :key #'palimpsest::finding-strength)
               "quality ~d: ~s" quality
               (mapcar #'palimpsest::finding-words
                       (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq))))))
  ;; a stroke of a scan kept without loss laid over another two blocks of 8 away: no JPEG's blocks on them, a copy
  (let ((page (pal-grained (pal-scan))))
    (pal-laid page (+ 20 95 8 -2) (+ 20 38 -2) (+ 20 95 24 -2) (+ 20 38 -2) 6 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "characters of the scanned page are copies of one another") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(defun pal-ring-cover (x y top)
  "How much of the pixel X Y, in a cell 16 pixels wide and 21 high, a thin ring's stroke covers, from 0 to 1:
a ring about a pixel wide, 9 pixels across, from row TOP to the cell's foot, as a program draws an O smooth."
  (let* ((a 4.5) (b (/ (- 21 top) 2)) (dx (/ (- (+ x 1/2) 8) a)) (dy (/ (- (+ y 1/2) (/ (+ top 21) 2)) b)))
    (max 0 (min 1 (- 1.1 (* (abs (- (sqrt (+ (* dx dx) (* dy dy))) 1)) (min a b)))))))

(defun pal-stamped-page (&key (ink '(200 30 30)) (turn 0) copy-into-text pressed copied every bar (stray 1)
                                (figures 6) from-text (from-turn 0) thin channels)
  "A scan (PAL-SCAN) blurred as a scanner's lens blurs it, then given grain (PAL-GRAINED), kept without loss
in colours, 600 by 560 pixels, its text black, with a stamp drawn by software below it. The stamp is
FIGURES figures of two shapes (a box, a taller box) in turn, each shape drawn the same pixel for pixel in INK
\(red, green, blue, each 0 to 255), its shades straying by up to 10 from pixel to pixel as a JPEG's
ringing leaves them, the same in every print. The figures are 16 pixels apart along a line turned TURN
degrees, each figure turned so too. With COPY-INTO-TEXT, one figure is laid at the end of a line of the
text as well. With PRESSED, a rubber stamp pressed on the paper and scanned with it: each figure a print
of its own, its ink straying as the page's scanned text does. With COPIED too, its first figure is copied
pixel for pixel over its third after the scan. With EVERY, its first two figures are copied over every
other print of their shapes, so that each shape's prints are all the same. With BAR, its second shape is a
plain bar, a stroke alone, as a figure 1 is. STRAY scales how far the figures' shades stray. With
FROM-TEXT, a stroke of the page's text, its grain kept, is recoloured red and laid twice beside the stamp's
figures, as a forger copies a page's character into a stamp; FROM-TEXT :FAINT makes it a lighter red, its
red, green and blue each as far from the paper as 0.17, 0.85 and 0.8 of the stroke's grey, its grain scaled
so too; FROM-TEXT :WHITE copies instead a box-shaped character the text ends its fourth line with, cut out
of its paper first, every pixel lighter than 150 made white; FROM-TEXT :LIGHTER cuts it out so and makes
its ink lighter by a constant in each colour, 120 in red and 10 in green and blue, a light red;
FROM-TEXT :PALER cuts it out so and makes it lighter by 80 in every colour, a light grey; FROM-TEXT
:FAINTER copies a larger such character, 24 by 30, printed twice, with its paper, each colour as far from the paper as
0.17, 0.6 and 0.6 of its grey, laid twice further apart.
FROM-TURN turns what is copied that many degrees, read between its pixels. With
CHANNELS, the scan's red, green and blue each carry grain of their own as well, up to 10 either way, as a
colour scanner's do; a character copied takes its greys from the red alone. With THIN, each shape is a thin ring instead
\(an O, a taller O), drawn smooth: a pixel the stroke covers in part takes the ink in that part and the
paper in the rest, as a program draws a thin face; and the page's last line is set so, scanned with it.
The page as red, green and blue octets."
  (let* ((grey (pal-grained (pal-blurred (let ((page (pal-scan)))
                                           (when (member from-text '(:white :lighter :paler :fainter))
                                             ;; a box-shaped character after the fourth line's words, its strokes
                                             ;; 3 pixels wide with a pixel of edge, the paper within it; for the
                                             ;; copies made fainter a larger one, 24 by 30, and another print of it
                                             ;; at the end of the sixth line
                                             (dolist (box (if (eq from-text :fainter)
                                                              '((570 131 593 160) (570 207 593 236))
                                                              '((574 133 588 152))))
                                               (destructuring-bind (left top right bottom) box
                                                 (loop for y from top to bottom
                                                       do (loop for x from left to right
                                                                do (let ((in (min (- x left) (- right x) (- y top)
                                                                                  (- bottom y))))
                                                                     (when (<= in 3)
                                                                       (setf (aref page (+ (* y 600) x))
                                                                             (if (or (= in 0) (= in 3)) 140 60)))))))))
                                           (when thin
                                             ;; the last line of the text set in the thin face instead
                                             (loop for y from 435 below 462
                                                   do (fill page 235 :start (* y 600) :end (* (1+ y) 600)))
                                             (dotimes (k 30 page)
                                               (dotimes (y 21)
                                                 (dotimes (x 16)
                                                   (let ((cover (pal-ring-cover x y (if (evenp k) 6 3)))
                                                         (at (+ (* (+ 438 y) 600) 20 (* k 18) x)))
                                                     (when (plusp cover)
                                                       (setf (aref page at)
                                                             (min (aref page at) (round (- 235 (* cover 175)))))))))))
                                           page)
                                         0 0 600 500 1)))
         (rgb (make-array (* 3 600 560) :element-type '(unsigned-byte 8)))
         (seed 11)
         (radians (* turn (/ pi 180))))
    (labels ((grain (base) (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648)) (+ base (mod (ash seed -16) 21)))
             (put (x y r g b)
               (let ((at (* 3 (+ (* y 600) x)))) (setf (aref rgb at) r (aref rgb (+ at 1)) g (aref rgb (+ at 2)) b)))
             (shape (top)
               ;; a box of ink, its shades as the stamp's software or its rubber leaves them; a drawn
               ;; stamp's figure laid with its own paper (white, as its picture's), a pressed one's on the page's
               (let ((cell (make-array (* 16 24) :initial-element (if pressed nil :paper))))
                 (when thin
                   (dotimes (y 24)
                     (dotimes (x 16)
                       (let ((cover (pal-ring-cover x y top)))
                         (when (plusp cover) (setf (aref cell (+ (* y 16) x)) (cons :cover cover))))))
                   (return-from shape cell))
                 (loop for y from top below 21
                       do (loop for x from 3 below 13
                                when (if (and bar (= top 3)) (<= 6 x 9) (or (< y (+ top 3)) (> y 17) (< x 6) (> x 9)))
                                  do (setf (aref cell (+ (* y 16) x))
                                           (round (* stray (if pressed (- (* 2 (grain 0)) 20) (- (grain 0) 10)))))))
                 cell)))
      (dotimes (y 560)
        (dotimes (x 600)
          (let ((k (if (< y 500) (aref grey (+ (* y 600) x)) (grain 225))))
            (if channels
                (flet ((part () (max 0 (min 255 (- (grain k) 10)))))
                  (put x y (part) (part) (part)))
                (put x y k k k)))))
      (let ((shapes (list (shape 6) (shape 3))))
        (flet ((lay (cell cx cy)
                 ;; CELL turned by RADIANS about its middle, its middle at CX CY
                 (loop for dy from -16 to 16
                       do (loop for dx from -16 to 16
                                do (let* ((sx (round (+ 8 (* (cos radians) dx) (* (sin radians) dy))))
                                          (sy (round (+ 12 (- (* (cos radians) dy) (* (sin radians) dx)))))
                                          (shade (and (< -1 sx 16) (< -1 sy 24) (aref cell (+ (* sy 16) sx)))))
                                     (cond ((eq shade :paper) (put (+ cx dx) (+ cy dy) 250 250 250))
                                           ((consp shade)
                                            (flet ((part (c) (round (- 250 (* (cdr shade) (- 250 c))))))
                                              (put (+ cx dx) (+ cy dy)
                                                   (part (first ink)) (part (second ink)) (part (third ink)))))
                                           (shade
                                            (put (+ cx dx) (+ cy dy)
                                                 (max 0 (min 255 (+ (first ink) shade)))
                                                 (max 0 (min 255 (+ (second ink) shade)))
                                                 (max 0 (min 255 (+ (third ink) shade)))))))))))
          (dotimes (k figures)
            (lay (if pressed (shape (if (evenp k) 6 3)) (nth (mod k 2) shapes))
                 (round (+ 220 (* k 16 (cos radians)))) (round (- 530 (* k 16 (sin radians))))))
          (when copy-into-text
            ;; at the end of a line of the text
            (lay (first shapes) 585 (+ 20 (* 3 38) 9)))))
      (flet ((copy (from to)
               ;; figure FROM's pixels laid over figure TO's, as an editor lays a piece
               (loop for y from 514 below 546
                     do (loop for x from (+ 212 (* 16 from)) below (+ 228 (* 16 from))
                              do (dotimes (c 3)
                                   (setf (aref rgb (+ (* 3 (+ (* y 600) x (* 16 (- to from)))) c))
                                         (aref rgb (+ (* 3 (+ (* y 600) x)) c))))))))
        (when copied (copy 0 2))
        (when every (copy 0 2) (copy 0 4) (copy 1 3) (copy 1 5)))
      (when from-text
        ;; the first stroke of the text, its paper about it, made red pixel for pixel, laid after the stamp: each
        ;; colour's distance from the paper scaled, its grain with it
        (dolist (left (if (eq from-text :fainter) '(330 375) '(322 340)))
          (loop with (x0 y0 x1 y1) = (case from-text
                                       (:fainter '(568 129 596 163))
                                       ((:white :lighter :paler) '(572 131 591 155))
                                       (t '(16 16 27 42)))
                with cx = (/ (+ x0 x1 -1) 2) and cy = (/ (+ y0 y1 -1) 2)
                with cosine = (cos (* from-turn (/ pi 180))) and sine = (sin (* from-turn (/ pi 180)))
                for y from (- y0 3) below (+ y1 3)
                do (loop for x from (- x0 3) below (+ x1 3)
                         ;; where the pixel came from, turned back: the source read between its pixels
                         for sx = (+ cx (* cosine (- x cx)) (* sine (- y cy)))
                         and sy = (+ cy (- (* cosine (- y cy)) (* sine (- x cx))))
                         when (and (<= x0 sx (1- x1)) (<= y0 sy (1- y1)))
                         do (let ((v (let* ((i (floor sx)) (j (floor sy)) (fx (- sx i)) (fy (- sy j)))
                                       (flet ((p (i j) (aref rgb (* 3 (+ (* (min j (1- y1)) 600) (min i (1- x1)))))))
                                         (round (+ (* (- 1 fx) (- 1 fy) (p i j)) (* fx (- 1 fy) (p (1+ i) j))
                                                   (* (- 1 fx) fy (p i (1+ j))) (* fx fy (p (1+ i) (1+ j))))))))
                                  (at (* 3 (+ (* (+ y (- 518 y0)) 600) x (- left x0)))))
                              (loop for scale in (case from-text (:faint '(0.17 0.85 0.8)) (:fainter '(0.17 0.6 0.6))
                                                   (t '(0.2 1 1)))
                                    for c from 0
                                    do (setf (aref rgb (+ at c))
                                             (case from-text
                                               (:white (if (> v 150) 255 (round (- 236 (* scale (- 236 v))))))
                                               (:lighter (if (> v 150) 255 (min 255 (+ v (if (zerop c) 120 10)))))
                                               (:paler (if (> v 150) 255 (min 255 (+ v 80))))
                                               (t (round (- 236 (* scale (- 236 v))))))))))))))
    rgb))

(defun pal-scanned-rgb (rgb &key (width 600) (height 560))
  "A one-page PDF of the scanned page RGB, raw red, green and blue pixels laid over all of the page."
  (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                      (cons 3
                            (format nil
                                    "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d ~d] /Resources << /XObject << /Im1 ~
                                     7 0 R >> >> /Contents 4 0 R >>" width height))
                      (list 4 :stream "" (format nil "q ~d 0 0 ~d 0 0 cm /Im1 Do Q" width height))
                      '(6 . "<< /Producer (Kea Scan) >>")
                      (list 7
                            :stream (format nil
                                            "/Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace ~
                                             /DeviceRGB /BitsPerComponent 8" width height) rgb))))

(define-test (palimpsest palimpsest-stamp-drawn-by-software)
  ;; a stamp's figures drawn the same pixel for pixel by software: in red ink, or turned 15 degrees, a stamp
  ;; drawn by software, worth a look; in the page's own black ink at its angle, or one figure also laid over
  ;; the text, a strong sign
  (flet ((findings (&rest args)
           (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-rgb (apply #'pal-stamped-page args)))
             (declare (ignore pdf accounts))
             findings)))
    (dolist (args '((:ink (200 30 30)) (:ink (40 40 40) :turn 15)))
      (let ((found (apply #'findings args)))
        (true (pal-finding found :look "a stamp drawn by software") "~s: ~s" args
              (mapcar #'palimpsest::finding-words found))
        (false (pal-finding found :strong "copies of one another") "~s: ~s" args
               (mapcar #'palimpsest::finding-words found))))
    ;; a rubber stamp pressed and scanned, each print its own: no copies; one figure copied over another: strong
    (let ((found (findings :pressed t)))
      (false (find :strong found :key #'palimpsest::finding-strength) "pressed: ~s"
             (mapcar #'palimpsest::finding-words found)))
    (dolist (args '((:ink (40 40 40)) (:ink (200 30 30) :copy-into-text t)
                    (:pressed t :copied t) (:pressed t :every t)))
      (let ((found (apply #'findings args)))
        (true (pal-finding found :strong "copies of one another") "~s: ~s" args
              (mapcar #'palimpsest::finding-words found))
        ;; the figures copied into the text strong; the stamp's other shape, the same only within it, may be named a
        ;; stamp
        (unless (getf args :copy-into-text)
          (false (pal-finding found nil "a stamp drawn by software") "~s: ~s" args
                 (mapcar #'palimpsest::finding-words found)))))))

(define-test (palimpsest palimpsest-stamp-turned-ten-degrees)
  ;; a stamp drawn by software in the page's own ink, turned ten degrees, measures a tenth of a degree either side
  ;; of ten: a stamp drawn by software, worth a look; turned nine or eight degrees, its copies stay strong
  (flet ((findings (turn)
           (nth-value 2 (pal-read (pal-scanned-rgb (pal-stamped-page :ink '(40 40 40) :turn turn))))))
    (let ((found (findings 10)))
      (true (pal-finding found :look "a stamp drawn by software") "~s" (mapcar #'palimpsest::finding-words found))
      (false (pal-finding found :strong "copies of one another")))
    (true (pal-finding (findings 9) :strong "copies of one another"))
    (true (pal-finding (findings 8) :strong "copies of one another"))))

(define-test (palimpsest palimpsest-stamp-with-plain-figures)
  ;; a stamp drawn by software whose second figure is a plain bar, as a 1 is, too plain to be judged a copy: its
  ;; prints the same pixel for pixel, a stamp drawn by software, worth a look; a rubber stamp so pressed, one of
  ;; its figures copied over another, stays strong
  (let ((found (nth-value 2 (pal-read (pal-scanned-rgb (pal-stamped-page :bar t))))))
    (true (pal-finding found :look "a stamp drawn by software") "~s" (mapcar #'palimpsest::finding-words found))
    (false (pal-finding found :strong "copies of one another")))
  (let ((found (nth-value 2 (pal-read (pal-scanned-rgb (pal-stamped-page :bar t :pressed t :copied t))))))
    (true (pal-finding found :strong "copies of one another") "~s" (mapcar #'palimpsest::finding-words found))))

(defun pal-stamp-findings (&rest arguments)
  "The findings of a scan with the stamp PAL-STAMPED-PAGE draws given ARGUMENTS."
  (nth-value 2 (pal-read (pal-scanned-rgb (apply #'pal-stamped-page arguments)))))

(define-test (palimpsest palimpsest-stamp-with-one-figure-repeated)
  ;; a stamp drawn by software of three figures, one shape printed twice: worth a look; a rubber stamp of three
  ;; figures pressed and scanned, its first copied over its third, stays strong
  (let ((found (pal-stamp-findings :figures 3)))
    (true (pal-finding found :look "a stamp drawn by software") "~s" (mapcar #'palimpsest::finding-words found))
    (false (pal-finding found :strong "copies of one another")))
  (true (pal-finding (pal-stamp-findings :figures 3 :pressed t :copied t) :strong "copies of one another")))

(define-test (palimpsest palimpsest-stamp-grain-by-group)
  ;; each group of copies in a stamp is judged by its own grain: a stamp drawn by software, its ringing the same in
  ;; every print, worth a look; a stroke of the page's text recoloured and laid twice beside it, straight or by a
  ;; stamp turned 10 degrees, stays strong, as does a rubber stamp pressed and scanned whose every print of a
  ;; figure was copied from one, its grain kept, made faint as 0.7 of it
  (let ((found (pal-stamp-findings)))
    (true (pal-finding found :look "a stamp drawn by software") "~s" (mapcar #'palimpsest::finding-words found))
    (false (pal-finding found :strong "copies of one another")))
  (dolist (turn '(0 10))
    (let ((found (pal-stamp-findings :from-text t :turn turn)))
      (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~d: ~s" turn
            (mapcar #'palimpsest::finding-words found))))
  (dolist (stray '(0.7 0.8))
    (let ((found (pal-stamp-findings :pressed t :every t :stray stray)))
      (true (pal-finding found :strong "6 characters of the scanned page are copies of one another") "~a: ~s" stray
            (mapcar #'palimpsest::finding-words found))))
  (true (pal-finding (pal-stamp-findings :pressed t :copied t) :strong "copies of one another")))

(define-test (palimpsest palimpsest-recoloured-copies-read)
  ;; strokes of the page copied in a light red, their grain scaled with their colours, read in grey as faint as
  ;; marks too faint to be letters: read by their darkest colour, they are found copies, a strong sign
  (let ((found (pal-stamp-findings :from-text :faint)))
    (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~s"
          (mapcar #'palimpsest::finding-words found))))

(define-test (palimpsest palimpsest-stamp-with-copies-cut-out)
  ;; a character of the page cut out of its paper onto white, recoloured red and laid twice beside a stamp
  ;; drawn by software: its paper is as clean as a drawn figure's, but its ink is the page's stroke recoloured, so its
  ;; copies stay a strong sign
  (let ((found (pal-stamp-findings :from-text :white)))
    (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~s"
          (mapcar #'palimpsest::finding-words found))))

(define-test (palimpsest palimpsest-stamp-with-copies-made-lighter)
  ;; a character of the page cut out onto white, its ink made lighter by a constant in each colour, and laid twice
  ;; beside a stamp drawn by software: its ink is the source's and a constant, not the same pixels, so its copies
  ;; stay a strong sign
  (let ((found (pal-stamp-findings :from-text :lighter)))
    (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~s"
          (mapcar #'palimpsest::finding-words found))))

(define-test (palimpsest palimpsest-regreyed-copy-bound)
  ;; a box-shaped character with the scanner's grain, set beside the same shape drawn smooth (a stamp drawn in the
  ;; page's face and blurred as the scan is), and beside itself recoloured: only the recoloured one is its copy,
  ;; judged against the grain of the paper within the source
  (flet ((glyph (shade)
           (let ((pixels (make-array (* 20 24) :element-type '(unsigned-byte 8))))
             (dotimes (y 24)
               (dotimes (x 20)
                 (let ((in (min (- x 2) (- 17 x) (- y 2) (- 21 y))))
                   (setf (aref pixels (+ (* y 20) x))
                         (funcall shade x y
                                  (cond ((< in 0) 235) ((or (= in 0) (= in 4)) 140) ((< in 4) 60) (t 235)))))))
             (palimpsest::make-glyph :pixels pixels :width 20 :x0 0 :y0 0 :x1 16 :y1 20))))
    (let* ((random (sb-ext:seed-random-state 41))
           (grain (make-array (* 20 24)))
           (source (progn (dotimes (k (length grain)) (setf (aref grain k) (- (random 15 random) 7)))
                          (glyph (lambda (x y v) (max 0 (min 255 (+ v (aref grain (+ (* y 20) x)))))))))
           (drawn (glyph (lambda (x y v) (declare (ignore x y)) v)))
           (recoloured (glyph (lambda (x y v) (round (+ (* 0.6 (max 0 (min 255 (+ v (aref grain (+ (* y 20) x))))))
                                                        90))))))
      (let ((most (* palimpsest::*regreyed-share* (palimpsest::glyph-paper-grain source))))
        (true (palimpsest::regreyed-copy-p recoloured source most))
        (false (palimpsest::regreyed-copy-p drawn source most) "drawn: ~,2f of ~,2f"
               (palimpsest::regreyed-distance drawn source) most)))))

(define-test (palimpsest palimpsest-stamp-with-copies-cut-out-colour-grain)
  ;; the cut-out copies on a scan whose colours carry grain of their own, made red from one colour: the grain of
  ;; the colours dropped lies between copy and source, a share of the page's own, and the copies stay strong
  (let ((found (pal-stamp-findings :from-text :white :channels t)))
    (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~s"
          (mapcar #'palimpsest::finding-words found))))

;; Copies turned as well as recoloured

(define-test (palimpsest palimpsest-recoloured-copies-turned)
  ;; a box-shaped character of the page copied with its paper in a light red, its grain scaled down with its
  ;; colours, turned 9 degrees and laid twice beside a stamp drawn by software: its ink no longer fits its
  ;; source closely, but the grain of its paper follows the source's, so its copies stay a strong sign
  (let ((found (pal-stamp-findings :from-text :fainter :from-turn 9)))
    (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~s"
          (mapcar #'palimpsest::finding-words found))))

(define-test (palimpsest palimpsest-turned-copies-source)
  ;; the source of a copy is sought turned up to 15 degrees either way: the light red copies turned 8 and 10
  ;; degrees, as a stamp is laid, are found copies of the page's character, a strong sign
  (dolist (turn '(8 10))
    (let ((found (pal-stamp-findings :from-text :fainter :from-turn turn)))
      (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~d: ~s" turn
            (mapcar #'palimpsest::finding-words found)))))

;; A box-shaped character 24 by 30, its strokes 3 pixels wide with a pixel of edge, the paper within it, for the
;; tests of the grain a copy shares with its source

(defun pal-box-shade (x y &optional (wide 24) (high 30))
  "The grey of the box-shaped character at X Y of its box, WIDE by HIGH: ink 60, its edge 140, paper 235."
  (let ((in (min x (- wide 1 x) y (- high 1 y))))
    (cond ((or (< in 0) (> in 3)) 235) ((or (= in 0) (= in 3)) 140) (t 60))))

(defun pal-box-glyph (shade &key (x0 0) (y0 0) (turn 0) (wide 24) (high 30))
  "A GLYPH of the box-shaped character WIDE by HIGH, its greys SHADE of X Y (from its box's corner, read between
whole pixels when turned), turned TURN degrees about its middle in a box grown to hold it, at X0 Y0 of its page."
  (let* ((r (* turn (/ pi 180)))
         (w (ceiling (+ (* wide (cos r)) (* high (abs (sin r))))))
         (h (ceiling (+ (* high (cos r)) (* wide (abs (sin r))))))
         (across (+ w 4)) (pixels (make-array (* across (+ h 4)) :element-type '(unsigned-byte 8))))
    (dotimes (j (+ h 4))
      (dotimes (i across)
        ;; where the pixel came from in the box, turned back about the middles
        (let* ((u (- (+ i 0.5) 2 (/ w 2))) (v (- (+ j 0.5) 2 (/ h 2)))
               (x (+ (/ wide 2) (* (cos r) u) (* (sin r) v) -0.5))
               (y (+ (/ high 2) (- (* (cos r) v) (* (sin r) u)) -0.5)))
          (setf (aref pixels (+ (* j across) i)) (max 0 (min 255 (round (funcall shade x y))))))))
    (palimpsest::make-glyph :pixels pixels :width across :x0 x0 :y0 y0 :x1 (+ x0 w) :y1 (+ y0 h))))

(defun pal-grain-field (seed)
  "A field of grain 40 by 46, each value straying by up to 7 either way, read between its points: a function of
X Y, the box's corner 8 in."
  (let ((random (sb-ext:seed-random-state seed)) (field (make-array '(46 40))))
    (dotimes (y 46) (dotimes (x 40) (setf (aref field y x) (- (random 15 random) 7))))
    (lambda (x y)
      (let* ((x (max 0 (min 38.99 (+ x 8)))) (y (max 0 (min 44.99 (+ y 8))))
             (i (floor x)) (j (floor y)) (fx (- x i)) (fy (- y j)))
        (+ (* (- 1 fx) (- 1 fy) (aref field j i)) (* fx (- 1 fy) (aref field j (1+ i)))
           (* (- 1 fx) fy (aref field (1+ j) i)) (* fx fy (aref field (1+ j) (1+ i))))))))

(defun pal-smooth-box (x y &optional (wide 24) (high 30))
  "The box-shaped character WIDE by HIGH drawn smooth, as a program draws a figure and blurs it: the mean of the 25
about X Y, so its edge fades into the paper over two pixels."
  (/ (loop for dy from -2 to 2
           sum (loop for dx from -2 to 2 sum (pal-box-shade (round (+ x dx)) (round (+ y dy)) wide high)))
     25))

(define-test (palimpsest palimpsest-shared-grain-bound)
  ;; the box-shaped character with the scanner's grain on its ink and paper, among three other prints of it with
  ;; grain of their own: the source turned 9 degrees and made lighter, read between its pixels, follows its
  ;; source's grain and no other's; the same shape drawn smooth with ringing of its own follows none, though its
  ;; edge fades into its paper; over few pixels the bound is higher
  (let* ((own (pal-grain-field 43))
         (source (pal-box-glyph (lambda (x y) (+ (pal-smooth-box x y) (funcall own x y))) :x0 300 :y0 40))
         (others (loop for seed in '(44 45 46) for x0 from 100 by 50
                       collect (let ((grain (pal-grain-field seed)))
                                 (pal-box-glyph (lambda (x y) (+ (pal-smooth-box x y) (funcall grain x y)))
                                                :x0 x0 :y0 40))))
         (candidates (append (subseq others 0 2) (list source) (subseq others 2)))
         (copy (pal-box-glyph (lambda (x y) (- 236 (* 0.6 (- 236 (+ (pal-smooth-box x y) (funcall own x y))))))
                              :x0 300 :y0 300 :turn 9))
         (drawn (pal-box-glyph (lambda (x y) (+ (pal-smooth-box x y) (* 2 (sin (+ (* 1.7 x) (* 2.3 y))))))
                               :x0 300 :y0 300 :turn 9)))
    (is eq source (palimpsest::grain-source copy candidates))
    (false (palimpsest::grain-source drawn candidates))
    (true (> (palimpsest::shared-grain-bound 16) (palimpsest::shared-grain-bound 100)))
    (is = palimpsest::*shared-grain-least* (palimpsest::shared-grain-bound 100))))

(define-test (palimpsest palimpsest-shared-grain-streaks)
  ;; a scanner that draws a streak down or across the page lays it on every print in those columns or rows: a print
  ;; of the box below another, or beside it, its grain its own but the same streaks, does not carry the other's grain
  (flet ((print-of (seed streak x0 y0)
           (let ((grain (pal-grain-field seed)))
             (pal-box-glyph (lambda (x y) (+ (pal-smooth-box x y) (* 0.3 (funcall grain x y)) (funcall streak x y)))
                            :x0 x0 :y0 y0))))
    (let ((down (lambda (x y) (declare (ignore y)) (* 6 (sin (* 2.1 (round x))))))
          (across (lambda (x y) (declare (ignore x)) (* 6 (sin (* 2.1 (round y)))))))
      (false (palimpsest::grain-source (print-of 48 down 301 85) (list (print-of 47 down 301 40))))
      (false (palimpsest::grain-source (print-of 48 across 346 40) (list (print-of 47 across 301 40)))))))

(define-test (palimpsest palimpsest-shared-grain-few-pixels)
  ;; a box-shaped character 19 by 20: its paper two pixels from its ink, past the blur of its edge, leaves 16 to 19
  ;; pixels free once the columns' and rows' means are taken out, where the bound is above 0.75 (from 20 it is 0.75);
  ;; the character made lighter and laid elsewhere follows its source's grain closely enough among two other prints
  ;; of it, but with its source alone there is no margin to show and it is not taken
  (flet ((print-of (shade x0 y0) (pal-box-glyph shade :x0 x0 :y0 y0 :wide 19 :high 20)))
    (let* ((own (pal-grain-field 51))
           (source (print-of (lambda (x y) (+ (pal-smooth-box x y 19 20) (funcall own x y))) 300 41))
           (others (loop for seed in '(52 53) for x0 in '(110 170)
                         collect (let ((grain (pal-grain-field seed)))
                                   (print-of (lambda (x y) (+ (pal-smooth-box x y 19 20) (funcall grain x y))) x0 41))))
           (copy (print-of (lambda (x y) (- 236 (* 0.6 (- 236 (+ (pal-smooth-box x y 19 20) (funcall own x y))))))
                           300 300)))
      (multiple-value-bind (radians dx dy) (palimpsest::source-placement copy source (palimpsest::copy-ink copy))
        (multiple-value-bind (r free) (palimpsest::shared-grain copy source radians dx dy)
          (true (and r (<= 16 free 19)) "~a over ~a" r free)
          (true (> (palimpsest::shared-grain-bound free) palimpsest::*shared-grain-least*))))
      (is eq source (palimpsest::grain-source copy (cons source others)))
      ;; one source scoring: no margin, not taken
      (false (palimpsest::grain-source copy (list source))))))

(define-test (palimpsest palimpsest-grain-source-bounded)
  ;; a copy set beside 500 prints of its shape: 400 of them are laid at a glance, 40 in full, however many there are
  (let* ((base (pal-box-glyph #'pal-smooth-box))
         (random (sb-ext:seed-random-state 61))
         (prints (loop for k below 500
                       collect (let ((pixels (copy-seq (palimpsest::glyph-pixels base))))
                                 (dotimes (i (length pixels))
                                   (setf (aref pixels i) (max 0 (min 255 (+ (aref pixels i) (random 9 random) -4)))))
                                 (palimpsest::make-glyph :pixels pixels :width (palimpsest::glyph-width base)
                                                         :x0 (* 30 (mod k 20)) :y0 (* 40 (floor k 20))
                                                         :x1 (+ 24 (* 30 (mod k 20))) :y1 (+ 30 (* 40 (floor k 20)))))))
         (copy (pal-box-glyph (lambda (x y) (+ (pal-smooth-box x y) (* 3 (sin (* 1.3 x y))))) :x0 700 :y0 700 :turn 9))
         (fits 0) (placed 0)
         (fit (fdefinition 'palimpsest::source-fit)) (placement (fdefinition 'palimpsest::source-placement)))
    (unwind-protect
         (progn
           (setf (fdefinition 'palimpsest::source-fit) (lambda (&rest arguments) (incf fits) (apply fit arguments))
                 (fdefinition 'palimpsest::source-placement)
                 (lambda (&rest arguments) (incf placed) (apply placement arguments)))
           (palimpsest::grain-source copy prints))
      (setf (fdefinition 'palimpsest::source-fit) fit (fdefinition 'palimpsest::source-placement) placement))
    (is = 40 placed)
    (true (<= (* 400 31) fits (+ (* 400 39) (* 40 (+ (* 31 9) (* 7 49))))) "~d fits" fits)))

(define-test (palimpsest palimpsest-stamp-in-the-page-face-turned)
  ;; a stamp drawn by software in red in the face of the page's box-shaped character, 24 by 30, on a cell of its own
  ;; with ringing of its own in its paper, the same in every print, turned 0, 5, 10 and 15 degrees and laid three
  ;; times beside the page's text, which ends four lines with that character, each with its grain: the stamp's
  ;; paper follows the grain of none of them, and its prints stay worth a look (turned under 10 degrees, where they
  ;; are found alike), never strong
  (dolist (turn '(0 5 10 15))
    (let* ((page (pal-scan)) (r (* turn (/ pi 180))))
      ;; the box-shaped character at the end of four of the text's lines
      (dolist (top '(131 207 283 359))
        (loop for y from top to (+ top 29)
              do (loop for x from 570 to 593 do (setf (aref page (+ (* y 600) x)) (pal-box-shade (- x 570) (- y top))))))
      (pal-grained (pal-blurred page 0 0 600 500 1))
      (let ((rgb (make-array (* 3 600 500) :element-type '(unsigned-byte 8))))
        (dotimes (k (* 600 500)) (fill rgb (aref page k) :start (* 3 k) :end (+ 3 (* 3 k))))
        (dolist (left '(200 260 320))
          (loop for y from 380 below 430
                do (loop for x from left below (+ left 45)
                         do (let* ((u (- x left 22)) (v (- y 405))
                                   (sx (+ 12 (* (cos r) u) (* (sin r) v))) (sy (+ 15 (- (* (cos r) v) (* (sin r) u))))
                                   (cover (/ (- 235 (pal-box-shade (round sx) (round sy))) 175))
                                   (ringing (round (* 3 (sin (+ (* 1.7 u) (* 2.3 v))))))
                                   (at (* 3 (+ (* y 600) x))))
                              (loop for c from 0 for ink in '(200 30 30)
                                    do (setf (aref rgb (+ at c))
                                             (max 0 (min 255 (+ ringing (round (- 245 (* cover (- 245 ink)))))))))))))
        (let ((found (nth-value 2 (pal-read (pal-scanned-rgb rgb :height 500)))))
          (false (pal-finding found :strong "copies of one another") "~d: ~s" turn
                 (mapcar #'palimpsest::finding-words found))
          ;; turned 10 degrees or more its figures are not found alike at all
          (when (< turn 10)
            (true (pal-finding found :look "the same pixel for pixel") "~d: ~s" turn
                  (mapcar #'palimpsest::finding-words found))))))))

;; Pages a program drew

(defun pal-flat-prints (&key (differ 1/4) (seed 5))
  "60 characters of one shape on one picture, a box 12 by 18 pixels in a cut of 16 by 22, as GLYPHs: every print
the same grain for grain, as a program or a scan made black and white then smoothed draws them, but for DIFFER
of the pixels of its ink, each moved by up to 20 grey levels either way."
  (let ((base (make-array (* 16 22) :element-type '(unsigned-byte 8))) (state (sb-ext:seed-random-state seed)))
    (dotimes (y 22)
      (dotimes (x 16)
        (let ((in (min (- x 2) (- 13 x) (- y 2) (- 19 y))))
          (setf (aref base (+ (* y 16) x)) (if (<= 0 in 2) (+ 50 (random 30 state)) 235)))))
    (loop for k below 60
          collect (let ((pixels (copy-seq base)))
                    (dotimes (i (length pixels))
                      (when (and (< (aref pixels i) 100) (< (random 1.0 state) differ))
                        (setf (aref pixels i) (+ (aref pixels i) (- (random 41 state) 20)))))
                    (palimpsest::glyph-measured
                     (palimpsest::make-glyph :pixels pixels :width 16 :x0 (+ 20 (* 20 (mod k 20)))
                                             :y0 (+ 20 (* 40 (floor k 20))) :x1 (+ 32 (* 20 (mod k 20)))
                                             :y1 (+ 38 (* 40 (floor k 20))) :ink 60 :picture 7 :page 0))))))

(define-test (palimpsest palimpsest-flat-prints-not-judged)
  ;; a print copied pixel for pixel among prints that share most of their ink grey for grey with the nearest of their
  ;; shape, as a part of a page made black and white without dithering and then smoothed draws them: worth a look;
  ;; among prints whose every pixel moves, as a scanner's grain moves them, a strong sign
  (flet ((strength (prints)
           (let* ((one (first prints))
                  (copy (palimpsest::glyph-measured
                         (palimpsest::make-glyph :pixels (copy-seq (palimpsest::glyph-pixels one)) :width 16
                                                 :x0 500 :y0 400 :x1 512 :y1 418 :ink 60 :picture 7 :page 0)))
                  (group (list (list one copy) 0 nil 5 :strong nil nil)))
             (palimpsest::flat-copies (list group) (palimpsest::read-pictures (cons copy prints) '() '())
                                      (make-hash-table))
             (fifth group))))
    (is eq :look (strength (pal-flat-prints)))
    (is eq :strong (strength (pal-flat-prints :differ 1)))))

(define-test (palimpsest palimpsest-flat-prints-by-line)
  ;; a line made black and white without dithering and smoothed, grey lines with grain just below it: a print
  ;; copied pixel for pixel on the black and white line is judged by the prints of its own line, worth a look
  (let* ((flat (subseq (pal-flat-prints) 0 20))
         (grained (mapc (lambda (g) (incf (palimpsest::glyph-y0 g) 40) (incf (palimpsest::glyph-y1 g) 40))
                        (pal-flat-prints :differ 1 :seed 6)))
         (one (first flat))
         (copy (palimpsest::glyph-measured
                (palimpsest::make-glyph :pixels (copy-seq (palimpsest::glyph-pixels one)) :width 16
                                        :x0 450 :y0 20 :x1 462 :y1 38 :ink 60 :picture 7 :page 0)))
         (group (list (list one copy) 0 nil 5 :strong nil nil)))
    (palimpsest::flat-copies (list group) (palimpsest::read-pictures (append (list copy) flat grained) '() '())
                             (make-hash-table))
    (is eq :look (fifth group))))

(define-test (palimpsest palimpsest-flat-prints-not-light)
  ;; light marks sharing their ink grey for grey are no prints of the page to judge a copy's line by: copies laid in
  ;; a row of light prints stay a strong sign
  (let* ((faint (palimpsest::make-word-box :x0 0 :y0 0 :x1 600 :y1 500 :cut t :faint t))
         (prints (mapc (lambda (g) (setf (palimpsest::glyph-word g) faint)) (pal-flat-prints)))
         (one (first prints))
         (copy (palimpsest::glyph-measured
                (palimpsest::make-glyph :pixels (copy-seq (palimpsest::glyph-pixels one)) :width 16 :word faint
                                        :x0 450 :y0 20 :x1 462 :y1 38 :ink 60 :picture 7 :page 0)))
         (group (list (list one copy) 0 nil 5 :strong nil nil)))
    (palimpsest::flat-copies (list group) (palimpsest::read-pictures (cons copy prints) '() '()) (make-hash-table))
    (is eq :strong (fifth group))))

(define-test (palimpsest palimpsest-page-drawn-on-a-grid)
  ;; a page drawn on a grid every 8 pixels holds the grid in its blank paper, a scanned page does not; the characters
  ;; of a picture drawn on a grid are not judged, the grid crossing each print where it falls
  (let ((rows (make-array 500 :element-type 'fixnum :initial-element 0))
        (drawn (make-array (* 600 500) :element-type '(unsigned-byte 8))))
    (dotimes (y 500)
      (dotimes (x 600)
        (setf (aref drawn (+ (* y 600) x)) (if (or (zerop (mod x 8)) (zerop (mod y 8))) 200 240))))
    (true (palimpsest::ruled-grid-p drawn 600 500 rows))
    (false (palimpsest::ruled-grid-p (pal-grained (make-array (* 600 500) :element-type '(unsigned-byte 8)
                                                                          :initial-element 235))
                                     600 500 rows)))
  (let ((pictures (palimpsest::read-pictures (pal-flat-prints :differ 1) '() '(7))))
    (false (second (first pictures)) "~s" pictures)))

;; A stamp drawn and smoothed

(define-test (palimpsest palimpsest-stamp-colours-on-one-line)
  ;; a box-shaped figure in red ink smoothed into a white ground keeps every colour on one line from the ink to the
  ;; white: drawn by a program; the page's characters with a scanner's grain in each colour, and one of them made red
  ;; with its paper by one rule (its line through the page's paper), are not
  (let ((random (sb-ext:seed-random-state 43)))
    (labels ((cover (x y)
               ;; how much of the pixel the box's strokes cover, its edges smoothed over a pixel
               (let ((in (min (- x 2) (- 17 x) (- y 2) (- 21 y))))
                 (cond ((< in -1) 0) ((< in 0) 1/2) ((<= in 3) 1) ((< in 4) 1/2) (t 0))))
             (glyph (colour)
               (let ((cuts (loop repeat 3 collect (make-array (* 20 24) :element-type '(unsigned-byte 8))))
                     (pixels (make-array (* 20 24) :element-type '(unsigned-byte 8))))
                 (dotimes (y 24)
                   (dotimes (x 20)
                     (let ((rgb (funcall colour (cover x y))) (k (+ (* y 20) x)))
                       (loop for cut in cuts for c in rgb do (setf (aref cut k) (max 0 (min 255 (round c)))))
                       (setf (aref pixels k) (palimpsest::grey-of :rgb (aref (first cuts) k) (aref (second cuts) k)
                                                                  (aref (third cuts) k) 0)))))
                 (palimpsest::make-glyph :pixels pixels :width 20 :x0 0 :y0 0 :x1 16 :y1 20 :channels cuts
                                         :word (palimpsest::make-word-box :paper 236.0))))
             (grain () (- (random 15 random) 7))
             (scanned (a)
               (let ((g (- 236 (* a 176)))) (list (+ g (grain)) (+ g (grain)) (+ g (grain)))))
             (drawn (a) (list (- 255 (* a 55)) (- 255 (* a 225)) (- 255 (* a 225))))
             (recoloured (a)
               (let ((g (+ (- 236 (* a 176)) (grain)))) (list (+ g (* 0.8 (- 236 g))) g g))))
      (let ((page (loop repeat 20 collect (glyph #'scanned))))
        (true (palimpsest::drawn-colours-p (list (glyph #'drawn) (glyph #'drawn)) page))
        (false (palimpsest::drawn-colours-p (list (glyph #'recoloured) (glyph #'recoloured)) page))
        (false (palimpsest::drawn-colours-p (list (glyph #'scanned) (glyph #'scanned)) page))))))

;; Characters far lighter than the page's text

(define-test (palimpsest palimpsest-copies-made-paler)
  ;; a character of the page cut out onto white, made lighter by 80 grey levels and laid twice beside a stamp drawn by
  ;; software: too faint to be read at the page's level of ink, it is read at its own, and its copies, the page's
  ;; character made lighter, are a strong sign
  (let ((found (pal-stamp-findings :from-text :paler)))
    (true (pal-finding found :strong "2 characters of the scanned page are copies of one another") "~s"
          (mapcar #'palimpsest::finding-words found))))

(define-test (palimpsest palimpsest-light-copies-bounded)
  ;; a character of the page made lighter and laid twice beside a stamp, on a page of some 700 characters: each copy
  ;; is set beside the few characters nearest it in size for its source, not every character of the page
  (let ((calls 0) (inside nil)
        (light (fdefinition 'palimpsest::light-copies)) (distance (fdefinition 'palimpsest::regreyed-distance)))
    (unwind-protect
         (progn
           (setf (fdefinition 'palimpsest::light-copies)
                 (lambda (&rest arguments) (setf inside t) (unwind-protect (apply light arguments) (setf inside nil)))
                 (fdefinition 'palimpsest::regreyed-distance)
                 (lambda (&rest arguments) (when inside (incf calls)) (apply distance arguments)))
           (pal-stamp-findings :from-text :paler))
      (setf (fdefinition 'palimpsest::light-copies) light
            (fdefinition 'palimpsest::regreyed-distance) distance))
    ;; 40 of the page's characters for each of the two copies at most
    (true (<= 1 calls 80) "~d calls" calls))
  ;; copies turned as well are laid on their page's characters for the grain they share: 400 of them by whole
  ;; degrees, 40 in full, some 37,000 fits of a copy's ink at most, for each of the two copies and its source match
  (let ((calls 0) (fit (fdefinition 'palimpsest::source-fit)))
    (unwind-protect
         (progn
           (setf (fdefinition 'palimpsest::source-fit) (lambda (&rest arguments) (incf calls) (apply fit arguments)))
           (pal-stamp-findings :from-text :fainter :from-turn 9))
      (setf (fdefinition 'palimpsest::source-fit) fit))
    (true (<= 1 calls (* 2 (+ (* 400 31) (* 40 622) (* 40 49 4)))) "~d calls" calls)))

(define-test (palimpsest palimpsest-light-prints-on-whitened-paper)
  ;; a scan whose paper was whitened, a band of it shaded, with a box drawn by a program in light grey four times the
  ;; same pixel for pixel, a box of the page's text its like: the paper shows no grain to tell a copy made lighter
  ;; from a light print, and a clean shape is no copy of a grained one; worth a look, never strong
  (let ((page (pal-scan)))
    (flet ((box (left top core edge &optional (shade 0))
             ;; a box-shaped character; with SHADE its core's greys step across it, as a program smooths a face
             (loop for y from top below (+ top 20)
                   do (loop for x from left below (+ left 15)
                            do (let ((in (min (- x left) (- (+ left 14) x) (- y top) (- (+ top 19) y))))
                                 (when (<= in 3)
                                   (setf (aref page (+ (* y 600) x))
                                         (if (or (= in 0) (= in 3))
                                             edge
                                             (+ core (mod (* shade (+ (* 7 (- x left)) (* 3 (- y top)))) 25))))))))))
      (box 574 133 60 140)
      (pal-grained page)
      (dotimes (k (length page))
        (when (> (aref page k) 200) (setf (aref page k) (if (<= 300 (floor k 600) 330) (+ 200 (mod k 50)) 255))))
      (dolist (left '(300 330 360 390)) (box left 470 140 200 1)))
    (let ((found (nth-value 2 (pal-read (pal-scanned-pages (list page))))))
      (false (pal-finding found :strong "copies of one another") "~s" (mapcar #'palimpsest::finding-words found)))))

(define-test (palimpsest palimpsest-light-prints-alike)
  ;; a mark too faint to be a letter, an L with grain of its own, laid four times the same pixel for pixel, no
  ;; character of the page made lighter: worth a look, never strong
  (let ((page (pal-grained (pal-scan))) (seed 99) (mark (make-array (* 18 12))))
    (dotimes (k (length mark))
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref mark k) (if (or (< (mod k 12) 3) (>= (floor k 12) 15)) (+ 165 (mod (ash seed -16) 21)) 235)))
    (dolist (left '(300 340 380 420))
      (dotimes (y 18)
        (dotimes (x 12)
          (setf (aref page (+ (* (+ 478 y) 600) left x)) (aref mark (+ (* y 12) x))))))
    (let ((found (nth-value 2 (pal-read (pal-scanned-pages (list page))))))
      (false (pal-finding found :strong "copies of one another") "~s" (mapcar #'palimpsest::finding-words found)))))

(define-test (palimpsest palimpsest-stamp-in-a-thin-face)
  ;; a stamp drawn by software in thin smooth strokes on a grainy page set in the same face: the cores of so thin
  ;; a stroke are mostly its smoothed edge and stray as a scan's do, but the paper within its figures is clean;
  ;; worth a look
  (let ((found (pal-stamp-findings :thin t)))
    (true (pal-finding found :look "a stamp drawn by software") "~s" (mapcar #'palimpsest::finding-words found))
    (false (pal-finding found :strong "copies of one another") "~s" (mapcar #'palimpsest::finding-words found))))

(define-test (palimpsest palimpsest-picture-shown-twice)
  ;; one picture kept twice in a file, the same octet for octet, shown on two pages: no copies, both pages
  ;; checked; a stroke laid over another in it is still a strong sign
  (let ((page (pal-grained (pal-scan))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list page (copy-seq page))))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "characters of the scanned page") "~s"
             (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :record "not checked") "~s" (mapcar #'palimpsest::finding-words findings)))
    (loop for y below 22
          do (loop for x below 8
                   do (setf (aref page (+ (* (+ 20 (* 4 38) -2 y) 600) (+ 20 (* 2 95) (* 3 8) -2) x))
                            (aref page (+ (* (+ 20 38 -2 y) 600) (+ 20 95 -2) x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list page (copy-seq page))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page") "~s"
            (mapcar #'palimpsest::finding-words findings))))
  ;; the same octets drawn otherwise, inverted by /Decode, are another picture: read for themselves
  (let* ((page (pal-grained (pal-scan)))
         (octets (map '(vector (unsigned-byte 8)) #'char-code
                      (cl-ppcre:regex-replace (format nil "(?s)(/Width 600 /Height 500 /ColorSpace /DeviceGray ~
                                                           /BitsPerComponent 8)(.*?/Width 600 /Height 500 /ColorSpace ~
                                                           /DeviceGray /BitsPerComponent 8)")
                                              (map 'string #'code-char (pal-scanned-pages (list page (copy-seq page))))
                                              "\\1\\2 /Decode [1 0]")))
         (pdf (palimpsest::read-pdf (pal-scanned-pages (list page (copy-seq page))) :name "twins.pdf"))
         (twins (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf))))
    (is = 2 (length twins))
    (true (palimpsest::scan-picture-twin (second twins)))
    (let* ((pdf (palimpsest::read-pdf octets :name "decoded.pdf"))
           (scans (palimpsest::scan-pictures pdf (palimpsest::last-revision pdf))))
      (is = 2 (length scans))
      (false (palimpsest::scan-picture-twin (second scans))))))

(define-test (palimpsest palimpsest-objects-drawn)
  ;; values written back as they were read
  (is string= "<< /Type /Page /Kids [3 0 R 4 0 R] /Count 2 /Scale 1.5 /Title (A \\(b\\) \\\\c) /Open true >>"
      (palimpsest::pdf-value-text '(("/Type" . "/Page") ("/Kids" (:ref 3 0) (:ref 4 0)) ("/Count" . 2) ("/Scale" . 1.5)
                                    ("/Title" . "A (b) \\c") ("/Open" . "true"))))
  ;; the copy: the revision as it was and one save more, its page framing each line with its stream's number
  (with-test-directory (dir)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-edited))
      (declare (ignore findings))
      (let* ((analysis (palimpsest::make-analysis :pdf pdf :accounts accounts :name "test.pdf"))
             (revision (aref (palimpsest::pdf-revisions pdf) 0))
             (target (namestring (merge-pathnames "objects-revision-1.pdf" dir))))
        (palimpsest::marked-copy analysis revision target)
        (let* ((octets (alexandria:read-file-into-byte-vector target))
               (copy (palimpsest::read-pdf octets :name "copy.pdf"))
               (last (palimpsest::last-revision copy))
               (page (first (palimpsest::pages-of copy last)))
               (lines (palimpsest::text-lines (palimpsest::marks-of copy last page))))
          ;; the revision kept octet for octet, one save added
          (true (equalp (subseq (palimpsest::pdf-octets pdf) 0 (palimpsest::revision-end revision))
                        (subseq octets 0 (palimpsest::revision-end revision))))
          (is = 2 (length (palimpsest::pdf-revisions copy)))
          ;; its text still read, and the labels: content stream 4, where each line's operators begin in it,
          ;; and its font, object 5
          (true (find "Total: 100" lines :test #'search) "~s" lines)
          (true (find "4 @13 font 5" lines :test #'search) "~s" lines)
          (true (find-if (lambda (l)
                           (and (search "4 @" l) (search "font 5" l) (not (search "@13 " l)))) lines) "~s" lines))))
    ;; the PDF copied once into the copy written, however large
    (let* ((octets (pal-page-of "/MediaBox [0 0 612 792] /Contents 4 0 R" "BT /F1 12 Tf 72 700 Td (Total: 100) Tj ET"
                                :more (list (list 9 :stream "" (make-string (* 2 1024 1024) :initial-element #\x)))))
           (pdf (palimpsest::read-pdf octets :name "large.pdf"))
           (analysis (palimpsest::make-analysis :pdf pdf :name "large.pdf"))
           (target (namestring (merge-pathnames "large-marked.pdf" dir))))
      (let ((consed (pal-consed (lambda () (palimpsest::marked-copy analysis (palimpsest::last-revision pdf) target)))))
        (true (< consed (* 3/2 (length octets))) "~,1f times the PDF's octets made" (/ consed (length octets))))
      (true (equalp octets (subseq (alexandria:read-file-into-byte-vector target) 0 (length octets)))))))

(define-test (palimpsest palimpsest-pictures-listed-once)
  ;; a picture drawn twice is listed once, where it is first drawn of those kept
  (let* ((pdf (palimpsest::read-pdf
               (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /XObject << /Im10 10 0 R /Im11 11 0 R ~
                                         >> >> /Contents 4 0 R")
                            (format nil "q 10 0 0 10 0 0 cm /Im10 Do Q q 500 0 0 500 0 0 cm /Im10 Do Q q 10 0 0 10 0 0 ~
                                         cm /Im11 Do Q")
                            :more (loop for k in '(10 11)
                                        collect (list k
                                                      :stream (format nil "/Type /XObject /Subtype /Image /Width 2 ~
                                                                           /Height 2 /ColorSpace /DeviceGray ~
                                                                           /BitsPerComponent 8") "abcd")))))
         (revision (palimpsest::last-revision pdf)))
    (is equal '(10 11) (mapcar (lambda (p) (palimpsest::mark-object (first p))) (palimpsest::pictures-of pdf revision)))
    (is equal '((10 500))
        (mapcar (lambda (p) (list (palimpsest::mark-object (first p)) (round (palimpsest::mark-x1 (first p)))))
                                  (palimpsest::pictures-of pdf revision :keep (lambda (page m) (declare (ignore page))
                                                                                 (> (palimpsest::mark-x1 m) 100)))))))

(defun pal-office-page
    (content &key (producer "Acrobat Distiller 10.1.2 (Windows)") (creator "PScript5.dll Version 5.2.2")
                                     (id "/ID [<0a1b2c3d> <0a1b2c3d>]") tagged trailer)
  "A one-page PDF drawing CONTENT, its document information naming PRODUCER and CREATOR, its trailer
giving ID: /F1 a font kept in the file (Calibri), /F2 Helvetica-Bold, not kept, /F3 another font kept
in the file (Liberation Serif); TAGGED, a tagged PDF
whose structure tree names the page's marked content 0, 1 and 2; TRAILER, more for the trailer."
  (pal-save #() (list (if tagged
                          '(1 . "<< /Type /Catalog /Pages 2 0 R /StructTreeRoot 11 0 R /MarkInfo << /Marked true >> >>")
                          '(1 . "<< /Type /Catalog /Pages 2 0 R >>"))
                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                      '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << "
                                                   "/Font << /F1 5 0 R /F2 10 0 R /F3 13 0 R >> >> /Contents 4 0 R >>"))
                      (list 4 :stream "" content)
                      '(5 . #.(concatenate 'string "<< /Type /Font /Subtype /TrueType /BaseFont /ABCDEF+Calibri "
                                                   "/FirstChar 32 /LastChar 126 /FontDescriptor 8 0 R >>"))
                      (cons 6 (format nil "<< /Producer (~a) /Creator (~a) >>" producer creator))
                      '(8 . "<< /Type /FontDescriptor /FontName /ABCDEF+Calibri /Flags 32 /FontFile2 9 0 R >>")
                      '(9 :stream "" "kept font")
                      '(10 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold /Encoding /WinAnsiEncoding >>")
                      '(11 . "<< /Type /StructTreeRoot /K 12 0 R >>")
                      '(12 . "<< /Type /StructElem /S /P /Pg 3 0 R /K [0 1 2] >>")
                      '(13 . #.(concatenate 'string "<< /Type /Font /Subtype /TrueType /BaseFont "
                                                    "/GHIJKL+LiberationSerif /FirstChar 32 /LastChar 126 "
                                                    "/FontDescriptor 14 0 R >>"))
                      '(14 . #.(concatenate 'string "<< /Type /FontDescriptor /FontName /GHIJKL+LiberationSerif /Flags "
                                                    "34 /FontFile2 15 0 R >>"))
                      '(15 :stream "" "kept font"))
            :trailer (format nil "/Root 1 0 R /Info 6 0 R~@[ ~a~]~@[ ~a~]" id trailer)))

(define-test (palimpsest palimpsest-text-box-drawn-as-made)
  ;; an office program's text box, white with its own text in the sheet's font, over a cell's text, all in the
  ;; one stream of the first save, its /ID as made: worth a look, with its honest cause; Distiller names no
  ;; PDF editor
  (let ((box (format nil "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf ~
                          72 703 Td (Data Size in Bytes) Tj ET")))
    (is equal '("Acrobat Distiller") (mapcar #'second (palimpsest::tools-in "Acrobat Distiller 10.1.2 (Windows)")))
    (is equal '("Adobe Acrobat" "Acrobat Distiller")
        (mapcar #'second (palimpsest::tools-in "Acrobat Distiller 10.1.2 (Windows); Adobe Acrobat Pro DC")))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page box))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "covered by a white shape") "~s"
             (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :look "drawn with the text as the file was made"))
      (false (pal-finding findings nil "names Adobe Acrobat")))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page box :id nil))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "drawn with the text as the file was made") "~s"
            (mapcar #'palimpsest::finding-words findings)))
    ;; the same with a PDF editor named, by a program not known to make PDFs, or with the /ID written again: strong
    (loop for (producer creator id) in '(("pypdf" "PScript5.dll Version 5.2.2" "/ID [<0a1b2c3d> <0a1b2c3d>]")
                                         ("Kea Writer" "Kea Writer" "/ID [<0a1b2c3d> <0a1b2c3d>]")
                                         ("Acrobat Distiller 10.1.2 (Windows)" "PScript5.dll Version 5.2.2"
                                          "/ID [<0a1b2c3d> <99887766>]"))
          do (multiple-value-bind (pdf accounts findings)
                 (pal-read (pal-office-page box :producer producer :creator creator :id id))
               (declare (ignore pdf accounts))
               (true (pal-finding findings :strong "covered by a white shape") "~s ~s" producer
                     (mapcar #'palimpsest::finding-words findings)))))
  ;; drawn after the page's outermost q ... Q block the hidden text is in: strong; within it, worth a look
  (dolist (case '((#.(concatenate 'string "q BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET Q 1 1 1 rg 70 695 110 18 "
                                          "re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET") :strong)
                  ("q BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT
/F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET Q" :look)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page (first case)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings (second case) "covered by a white shape") "~s ~s" (second case)
            (mapcar #'palimpsest::finding-words findings))))
  ;; a file LibreOffice made carries /DocChecksum in its trailer, the MD5 of the file up to its
  ;; trailer: without it, or with one that does not hold, written again or changed, strong
  (let* ((content (format nil "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 ~
                               12 Tf 72 703 Td (Data Size in Bytes) Tj ET"))
         (made (pal-office-page content :producer "LibreOffice 7.6" :creator "Writer"))
         (sum (string-upcase (tuatara:octets-md5 made :end (search (pal-octets "trailer") made)))))
    (dolist (case (list (list nil :strong) (list (format nil "/DocChecksum /~a" sum) :look)
                        (list "/DocChecksum /CBF78CB392F8D620019445DBEF0F0037" :strong)))
      (multiple-value-bind (pdf accounts findings)
          (pal-read (pal-office-page content :producer "LibreOffice 7.6" :creator "Writer" :trailer (first case)))
        (declare (ignore pdf accounts))
        (true (pal-finding findings (second case) "covered by a white shape") "~s ~s" case
              (mapcar #'palimpsest::finding-words findings)))))
  ;; a program that writes PDFs anew named: strong
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page (format nil "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re ~
                                              f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET")
                                 :creator "PScript5.dll Version 5.2.2; pikepdf 8.15"))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a tagged page: the box and its text within the marked content its structure tree names, worth a look;
  ;; the same box and text appended untagged, strong
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page (format nil "/P <</MCID 0>> BDC BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET EMC /P ~
                                              <</MCID 1>> BDC 1 1 1 rg 70 695 110 18 re f EMC /P <</MCID 2>> BDC 0 g ~
                                              BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET EMC")
                                 :tagged t))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "drawn with the text as the file was made") "~s"
          (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page (format nil "/P <</MCID 0>> BDC BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET EMC 1 ~
                                              1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in ~
                                              Bytes) Tj ET")
                                 :tagged t))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a box whose text is in a font not kept in the file, as an editor adds one, and a box with nothing of its
  ;; own on it, though drawn as the file was made: strong
  (dolist (content '(#.(concatenate 'string "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re "
                                            "f 0 g BT /F2 12 Tf 72 703 Td (Data Size in Bytes) Tj ET")
                     "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page content))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "covered by a white shape") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-figure-in-a-line-covered)
  ;; a white box over one figure of a long line, as an online editor changes it: the line partly covered,
  ;; the figure named; a box over a whole run, as before
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Price NZ$ 184,500.00 plus GST, being the total of ~
                                           the prices in Schedule 1 of this Agreement.) Tj ET 1 1 1 rg 130 697 64 14 ~
                                           re f")))
    (declare (ignore pdf accounts))
    (let ((partly (pal-finding findings :strong "partly covered by a white shape")))
      (true partly "~s" (mapcar #'palimpsest::finding-words findings))
      (when partly
        (pal-has (palimpsest::finding-words partly) "184,500.00\" of \"Price NZ$ 184,500.00 plus GST")))))

;;; Covers drawn as paths

(defparameter *pal-white-paths*
  '(("a box with rounded corners"
        "1 1 1 rg 72 694 m 134 694 l 138 694 140 696 140 700 c 140 712 l 140 716 138 718 134 718 c 72 718
l 68 718 66 716 66 712 c 66 700 l 66 696 68 694 72 694 c h f")
    ("a rectangle drawn as LibreOffice draws one"
        "1 1 1 rg 100 694 m 66 694 l 66 718 l 140 718 l 140 694 l 100 694 l h f")
    ("two squares filled by the nonzero rule" "1 1 1 rg 60 690 90 40 re 66 696 78 28 re f"))
  "White shapes drawn as paths over \"Secret 42\" (72 to 126 across, 700 to 712 up), each with what it is.")

(define-test (palimpsest palimpsest-path-covers)
  ;; a white shape drawn as a path, its curves and lines, over text: strong, as a rectangle is
  (loop for (what path) in *pal-white-paths*
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET ~a" path)))
             (declare (ignore pdf accounts))
             (let ((covered (pal-finding findings :strong "covered by a white shape")))
               (true covered "~a: ~s" what (mapcar #'palimpsest::finding-words findings))
               (when covered (pal-has (palimpsest::finding-words covered) "Secret 42")))))
  ;; its box over the text, but the text in a notch of it, or in a hole the even-odd rule leaves:
  ;; not covered
  (dolist (path '("1 1 1 rg 60 690 m 150 690 l 150 730 l 140 730 l 140 696 l 66 696 l 66 730 l 60 730 l h f"
                  "1 1 1 rg 60 690 90 40 re 66 696 78 28 re f*"))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET ~a" path)))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "covered by a") "~s: ~s" path (mapcar #'palimpsest::finding-words findings))))
  ;; an oval over one figure of a line: that figure named, the letters it only grazes not
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Price NZ$ 184,500.00 plus GST) Tj ET 1 1 1 rg 196 ~
                                           704 m 196 709.5 180.8 714 162 714 c 143.2 714 128 709.5 128 704 c 128 698.5 ~
                                           143.2 694 162 694 c 180.8 694 196 698.5 196 704 c h f")))
    (declare (ignore pdf accounts))
    (let ((partly (pal-finding findings :strong "partly covered by a white shape")))
      (true partly "~s" (mapcar #'palimpsest::finding-words findings))
      (when partly (pal-has (palimpsest::finding-words partly) "\"184,500.00\" of")))))

;; a rounded box from X Y, W across and H up, its corners of radius R, as a path
(defun pal-rounded (x y w h r)
  "The operators drawing a box from X Y, W across and H up, its corners rounded to radius R."
  (let ((c (* r 0.5523)))
    (format nil
            "~,3f ~,3f m ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c ~,3f ~
             ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c h "
            (+ x r) y (- (+ x w) r) y (+ (- (+ x w) r) c) y (+ x w) (- (+ y r) c) (+ x w) (+ y r)
            (+ x w) (- (+ y h) r) (+ x w) (+ (- (+ y h) r) c) (+ (- (+ x w) r) c) (+ y h) (- (+ x w) r) (+ y h)
            (+ x r) (+ y h) (- (+ x r) c) (+ y h) x (+ (- (+ y h) r) c) x (- (+ y h) r)
            x (+ y r) x (- (+ y r) c) (- (+ x r) c) y (+ x r) y)))

(define-test (palimpsest palimpsest-covers-in-the-text-frame)
  ;; a rising line: a panel in the empty corner of its upright box hides nothing of it; a panel over
  ;; it hides it
  (let ((line "BT /F1 12 Tf 0.5 0.866 -0.866 0.5 72 600 Tm (Received 1,250.00 on account) Tj ET "))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-one-page (format nil "~a1 1 1 rg ~af" line (pal-rounded 138 604 30 94 4))))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "covered by a") "~s" (mapcar #'palimpsest::finding-words findings)))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-one-page (format nil "~aq 0.5 0.866 -0.866 0.5 72 600 cm 1 1 1 rg ~af Q" line
                                        (pal-rounded -4 -4 180 18 4))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "covered by a white shape") "~s"
            (mapcar #'palimpsest::finding-words findings))))
  ;; text set up the page under a panel: covered
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 0 1 -1 0 300 500 Tm (Vertical note 1,250.00) Tj ET 1 1 1 rg ~af"
                                      (pal-rounded 284 494 22 148 4))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s"
          (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-covers-by-their-outline-wholly)
  (flet ((findings-of (shape)
           (nth-value 2
                      (pal-read (pal-one-page (format nil
                                                      "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET 1 1 1 rg ~a" shape))))))
    ;; a shape of thousands of points, none dropped
    (dolist (count '(4097 20000))
      (let ((findings (findings-of (with-output-to-string (s)
                                     (dotimes (k count)
                                       (let* ((angle (/ (* 2 pi k) count)) (c (cos angle)) (n (sin angle)))
                                         (format s "~,3f ~,3f ~a " (+ 100 (* 40 (signum c) (expt (abs c) 0.2)))
                                                 (+ 705 (* 12 (signum n) (expt (abs n) 0.2)))
                                                 (if (zerop k) "m" "l"))))
                                     (write-string "h f" s)))))
        (true (pal-finding findings :strong "covered by a white shape") "~d points: ~s"
              count (mapcar #'palimpsest::finding-words findings))))
    ;; narrow white strips each filled on its own, side by side: hidden together
    (let ((findings (findings-of (with-output-to-string (s)
                                   (dotimes (k 40) (format s "~d 694 2 22 re f " (+ 60 (* 2 k))))))))
      (true (pal-finding findings :strong "white shapes drawn over it") "~s"
            (mapcar #'palimpsest::finding-words findings)))
    ;; two pieces of one path, each over half: covered, not "X of X"
    (let ((findings (findings-of (format nil "~a~af" (pal-rounded 66 694 34 22 3) (pal-rounded 98 694 34 22 3)))))
      (true (pal-finding findings :strong "covered by a white shape") "~s"
            (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "partly covered")))
    ;; within what is clipped to: a page-wide white box clipped to a ring with the text in its hole
    ;; hides nothing
    (let ((findings (findings-of "q 60 690 90 40 re 66 696 78 28 re W* n 0 0 612 792 re f Q")))
      (false (pal-finding findings nil "covered by a") "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; a bow-tie with a rectangle's corners is no rectangle
  (false (palimpsest::rectangle-p '((54 . 694) (164 . 716) (164 . 694) (54 . 716))))
  (true (palimpsest::rectangle-p '((54 . 694) (164 . 694) (164 . 716) (54 . 716)))))

(define-test (palimpsest palimpsest-covers-seen-through)
  ;; a white shape under a blend mode other than Normal, or a soft mask, hides nothing; under Normal
  ;; it covers
  (loop for (state strength) in '(("<< /BM /Multiply >>" nil) ("<< /BM /Darken >>" nil)
                                  ("<< /BM [/Screen /Normal] >>" nil)
                                  ("<< /SMask << /Type /Mask /S /Luminosity /G 9 0 R >> >>" nil)
                                  ("<< /BM /Normal >>" :strong) ("<< /BM /Compatible /SMask /None >>" :strong))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> ~
                                                   /ExtGState << /G1 8 0 R >> >> /Contents 4 0 R")
                                      (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET /G1 gs 1 1 1 rg ~af"
                                              (pal-rounded 66 694 74 22 4))
                                      :more (list (cons 8 state)
                                                  '(9 :stream #.(concatenate 'string "/Type /XObject /Subtype /Form "
                                                                                     "/BBox [0 0 612 792] /Group << /S "
                                                                                     "/Transparency /CS /DeviceGray >>")
                                                    "0 g 0 0 612 792 re f"))))
             (declare (ignore pdf accounts))
             (if strength
                 (true (pal-finding findings strength "covered by a white shape") "~a: ~s" state
                       (mapcar #'palimpsest::finding-words findings))
                 (false (pal-finding findings nil "covered by a") "~a: ~s" state
                        (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-cover-colours-read)
  ;; white set in a colorant at tint 0, in Lab, as an indexed entry, or as a pattern painting white:
  ;; a white cover; a colorant /None paints nothing
  (loop for (spaces fill strength) in
        '(("/C1 [/Separation /Black /DeviceCMYK << /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1 [0 0 0 1] /N 1 >>]"
                "/C1 cs 0 sc" :strong)
          ("/C1 [/Separation /Spot /DeviceRGB << /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [0.2 0.3 0.9] /N 1 >>]"
                "/C1 cs 0.02 scn" :strong)
          ("/C1 [/Separation /All /DeviceGray << /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >>]" "/C1 cs 0 sc"
                :strong)
          ("/C1 [/DeviceN [/Cyan /Magenta] /DeviceCMYK << /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1
[1 1 0 0] /N 1 >>]" "/C1 cs 0 0 scn" :strong)
          ("/C1 [/Lab << /WhitePoint [0.9505 1 1.089] /Range [-100 100 -100 100] >>]" "/C1 cs 100 0 0 sc" :strong)
          ("/C1 [/Indexed /DeviceRGB 1 <000000FFFFFF>]" "/C1 cs 1 sc" :strong)
          ("/C1 [/Separation /None /DeviceGray << /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >>]" "/C1 cs 0 sc"
                nil)
          ("/C1 [/Separation /Black /DeviceCMYK << /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1 [0 0 0 1] /N 1 >>]"
                "/C1 cs 1 sc" :look))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-page-of (format nil
                                              "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /ColorSpace ~
                                               << ~a >> >> /Contents 4 0 R" spaces)
                                      (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET ~a ~af" fill
                                              (pal-rounded 66 694 74 22 4))))
             (declare (ignore pdf accounts))
             (case strength
               (:strong (true (pal-finding findings :strong "covered by a white shape") "~a: ~s" spaces
                              (mapcar #'palimpsest::finding-words findings)))
               (:look (true (pal-finding findings :look "covered by a black shape") "~a: ~s" spaces
                            (mapcar #'palimpsest::finding-words findings)))
               (t (false (pal-finding findings nil "covered by a") "~a: ~s" spaces
                         (mapcar #'palimpsest::finding-words findings))))))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /Pattern << /P1 ~
                                          8 0 R >> >> /Contents 4 0 R")
                             (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET /Pattern cs /P1 scn ~af"
                                     (pal-rounded 66 694 74 22 4))
                             :more '((8 :stream #.(concatenate 'string "/Type /Pattern /PatternType 1 /PaintType 1 "
                                                                       "/TilingType 1 /BBox [0 0 10 10] /XStep 10 "
                                                                       "/YStep 10 /Resources << >>")
                                      "1 1 1 rg 0 0 10 10 re f"))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s"
          (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-cover-honest-causes)
  ;; shapes over text as documents draw them, each worth a look with its cause named; the same
  ;; shapes over a line of the page set as forgers set them, strong
  (flet ((findings-of (content) (nth-value 2 (pal-read (pal-one-page content))))
         (words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; a panel over a watermark: light, large, turned, drawn under all the page's text
    (let ((findings (findings-of (format nil
                                         "0.85 g BT /F1 60 Tf 0.707 0.707 -0.707 0.707 100 400 Tm (DRAFT) Tj ET 0 g BT ~
                                          /F1 12 Tf 72 700 Td (Body text of the page) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 ~
                                          Tf 80 560 Td (Summary) Tj ET"
                                         (pal-rounded 60 380 300 200 6)))))
      (true (pal-finding findings :look "a watermark or a background word") "~s" (words findings))
      (false (pal-finding findings :strong "covered by")))
    (let ((findings (findings-of (format nil
                                         "BT /F1 12 Tf 72 400 Td (Amount 1,250.00) Tj ET 0 g BT /F1 12 Tf 72 700 Td ~
                                          (Body text of the page) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td ~
                                          (Summary) Tj ET"
                                         (pal-rounded 60 380 300 200 6)))))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (words findings)))
    ;; a callout outlined in blue; outlined in white it is a patch the colour of the paper
    (dolist (case '(("0 0 1 RG" :look) ("1 1 1 RG" :strong)))
      (let ((findings (findings-of (format nil
                                           "BT /F1 12 Tf 72 700 Td (Revenue grew to 1,250.00 this year) Tj ET 1 1 1 rg ~
                                            ~a ~aB 0 g BT /F1 10 Tf 125 704 Td (see note) Tj ET"
                                           (first case) (pal-rounded 120 694 70 20 4)))))
        (true (pal-finding findings (second case) "partly covered by a white shape") "~s ~s" case (words findings))
        (when (eq (second case) :look) (true (pal-finding findings :look "a callout or label meant to be seen")))))
    ;; one label over another, each a box and its text; the same box over a line that has no box of
    ;; its own, strong
    (let ((findings (findings-of (format nil "1 1 1 rg 100 498 115 14 re f 0 g BT /F1 10 Tf 102 501 Td (Planning phase ~
                                              begins) Tj ET 1 1 1 rg 150 500 70 13 re f 0 g BT /F1 10 Tf 152 506 Td ~
                                              (Design review) Tj ET"))))
      (true (pal-finding findings :look "labels laid over one another") "~s" (words findings))
      (false (pal-finding findings :strong "covered by")))
    (let ((findings (findings-of (format nil "0 g BT /F1 10 Tf 102 501 Td (Planning phase begins) Tj ET 1 1 1 rg 150 ~
                                              500 70 13 re f 0 g BT /F1 10 Tf 152 506 Td (Design review) Tj ET"))))
      (true (pal-finding findings :strong "partly covered by a white shape") "~s" (words findings)))
    ;; a stamp, its own text at a slant; its text along the line, strong
    (dolist (case '(("0.906 0.423 -0.423 0.906 140 695" :look) ("1 0 0 1 140 708" :strong)))
      (let ((findings (findings-of (format nil
                                           "BT /F1 12 Tf 72 700 Td (Line of the report 1,250.00 shown) Tj ET 1 1 1 rg ~
                                            ~af 0.8 0 0 rg BT /F1 14 Tf ~a Tm (PAID) Tj ET"
                                           (pal-rounded 130 690 80 30 6) (first case)))))
        (true (pal-finding findings (second case) "partly covered by a white shape") "~s ~s" case (words findings))))))

(define-test (palimpsest palimpsest-cover-causes-not-for-forgeries)
  (flet ((findings-of (content) (nth-value 2 (pal-read (pal-one-page content))))
         (words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; a panel over a watermark is worth a look; the same panel added in a later save is strong
    (let* ((watermark (format nil "0.85 g BT /F1 60 Tf 0.707 0.707 -0.707 0.707 100 400 Tm (UNPAID) Tj ET 0 g BT /F1 ~
                                   12 Tf 72 700 Td (Body text of the page) Tj ET "))
           (panel (format nil "1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td (Summary) Tj ET" (pal-rounded 60 380 300 220 6)))
           (base (pal-one-page watermark)))
      (true (pal-finding (findings-of (concatenate 'string watermark panel)) :look "a watermark or a background word"))
      (let ((findings (nth-value 2 (pal-read (pal-save base
                                                       (list '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 "
                                                                                          "R /MediaBox [0 0 612 792] "
                                                                                          "/Resources << /Font << /F1 "
                                                                                          "5 0 R >> >> /Contents [4 0 "
                                                                                          "R 7 0 R] >>"))
                                                                  (list 7 :stream "" panel)))))))
        (true (pal-finding findings
                           :strong "covered by a white shape drawn over it, added in a later save") "~s"
                           (words findings))))
    ;; new text set on the old line in the old font and size: strong under any cause
    (dolist (content (list (format nil
                                   "BT /F1 12 Tf 72 700 Td (Total 1,250.00) Tj ET 1 1 1 rg 0 0 1 RG ~aB 0 g BT /F1 12 ~
                                    Tf 72 700 Td (Total 9,250.00) Tj ET"
                                   (pal-rounded 66 694 100 20 4))
                           (format nil "1 1 1 rg 100 498 115 14 re f 0 g BT /F1 10 Tf 102 501 Td (Planning phase ~
                                        begins) Tj ET 1 1 1 rg 150 498 70 14 re f 0 g BT /F1 10 Tf 152 501 Td (Design ~
                                        review) Tj ET")))
      (let ((findings (findings-of content)))
        (true (pal-finding findings :strong "covered by a white shape") "~s ~s" content (words findings))))
    ;; an outline that does not show: too light, a hairline, wholly see-through, or clipped away
    (dolist (outline (list (format nil "0.94 G ~aB" (pal-rounded 66 694 100 20 4))
                           (format nil "0 0 1 RG 0.1 w ~aB" (pal-rounded 66 694 100 20 4))
                           (format nil "/G1 gs 0 0 1 RG ~aB" (pal-rounded 66 694 100 20 4))
                           (format nil "q 70 696 90 16 re W n 0 0 1 RG ~aB Q" (pal-rounded 66 694 100 20 4))))
      (let ((findings (nth-value 2
                                 (pal-read (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font << ~
                                                                     /F1 5 0 R >> /ExtGState << /G1 8 0 R >> >> ~
                                                                     /Contents 4 0 R")
                                                         (format nil
                                                                 "BT /F1 12 Tf 72 700 Td (Total 1,250.00) Tj ET 1 1 1 ~
                                                                  rg ~a" outline)
                                                         :more '((8 . "<< /CA 0 >>")))))))
        (true (pal-finding findings :strong "covered by a white shape") "~s ~s" outline (words findings))))
    ;; a stamp whose own text is not seen or is no words: invisible, white, tiny, one letter
    (dolist (stamp '("3 Tr 0.8 0 0 rg BT /F1 14 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (PAID) Tj ET 0 Tr"
                     "1 1 1 rg BT /F1 14 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (PAID) Tj ET"
                     "0.8 0 0 rg BT /F1 3 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (PAID) Tj ET"
                     "0.8 0 0 rg BT /F1 14 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (X) Tj ET"))
      (let ((findings (findings-of (format nil
                                           "BT /F1 12 Tf 72 700 Td (Line of the report 1,250.00 shown) Tj ET 1 1 1 rg ~
                                            ~af ~a" (pal-rounded 130 690 80 30 6) stamp))))
        (true (pal-finding findings :strong "partly covered by a white shape") "~s ~s" stamp (words findings))))))

(define-test (palimpsest palimpsest-cover-causes-of-documents)
  (flet ((findings-of (content) (nth-value 2 (pal-read (pal-one-page content))))
         (words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; watermarks: mid grey and turned, drawn first; a tint; light and a little larger, not turned
    (dolist (mark '("0.5 g BT /F1 40 Tf 0.707 0.707 -0.707 0.707 100 420 Tm (Confidential draft) Tj ET"
                    "1 0.55 0.55 rg BT /F1 40 Tf 0.5 0.866 -0.866 0.5 200 400 Tm (Confidential draft) Tj ET"
                    "0.85 g BT /F1 17 Tf 80 500 Td (Confidential draft) Tj ET"))
      (let ((findings (findings-of (format nil
                                           "~a 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET BT /F1 12 Tf ~
                                            72 300 Td (More body text) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td ~
                                            (Summary) Tj ET"
                                           mark (pal-rounded 60 380 400 240 6)))))
        (true (pal-finding findings :look "watermark") "~s ~s" mark (words findings))
        (false (pal-finding findings :strong "covered by"))))
    ;; dark large text not turned, or turned but not under all the page's text: strong
    (dolist (mark '("0 g BT /F1 30 Tf 80 500 Td (Amount 1,250.00) Tj ET"
                    "0 g BT /F1 12 Tf 72 300 Td (More body text) Tj ET 0 g BT /F1 40 Tf 0.707 0.707 -0.707
0.707 100 420 Tm (Amount 1,250.00) Tj ET"))
      (let ((findings (findings-of (format nil
                                           "~a 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET 1 1 1 rg ~af 0 ~
                                            g BT /F1 12 Tf 80 560 Td (Summary) Tj ET"
                                           mark (pal-rounded 60 380 400 240 6)))))
        (true (pal-finding findings :strong "covered by a white shape") "~s ~s" mark (words findings))))
    ;; a form flattened: field boxes with their values over the lines to write on and their hints
    (let ((findings (findings-of (format nil "0 g BT /F1 11 Tf 60 742 Td (Name:) Tj ET BT /F1 11 Tf 130 740 Td ~
                                              (______________________________) Tj ET BT /F1 6 Tf 130 751 Td (\\(write ~
                                              in block letters\\)) Tj ET 1 1 1 rg 128 736 200 20 re f 0 0 0.5 rg BT ~
                                              /F1 11 Tf 132 742 Td (Kea Builders Ltd) Tj ET"))))
      (true (pal-finding findings :look "a form field filled in") "~s" (words findings))
      (false (pal-finding findings :strong "covered by")))
    ;; a field box over a figure, not a line to write on: strong
    (let ((findings (findings-of (format nil "0 g BT /F1 11 Tf 130 740 Td (1,250.00) Tj ET 1 1 1 rg 128 736 200 20 re ~
                                              f 0 0 0.5 rg BT /F1 11 Tf 132 742 Td (9,250.00) Tj ET"))))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (words findings)))
    ;; labels on a map: pills round names over street names written again and again beneath; the
    ;; same pill over lines that are not written again, strong
    (dolist (case '(t nil))
      (let ((findings (findings-of (format nil "~{~a~} 1 1 1 rg ~af 0 g BT /F1 11 Tf 120 722 Td (Library) Tj ET"
                                           (loop for k below 6
                                                 collect (format nil "0.3 g BT /F1 8 Tf 40 ~d Td (~a) Tj ET "
                                                                 (- 762 (* 40 k))
                                                                 (if case
                                                                     "KAURI ROAD  KAURI ROAD  KAURI ROAD  KAURI ROAD"
                                                                     (format nil "Line ~d of the report and its figures"
                                                                             k))))
                                           (pal-rounded 114 717 50 17 8)))))
        (if case
            (true (pal-finding findings :look "a label laid on a map") "~s" (words findings))
            (true (pal-finding findings :strong "partly covered by a white shape") "~s" (words findings)))))))

(define-test (palimpsest palimpsest-cover-causes-as-made-only)
  (flet ((words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; an outlined callout is worth a look in a file as its maker wrote it, strong once a PDF editor or a program
    ;; that writes PDFs anew is named, or LibreOffice's checksum is missing
    (let ((content (format nil
                           "BT /F1 12 Tf 72 700 Td (Revenue grew to 1,250.00 this year) Tj ET 1 1 1 rg 0 0 1 RG ~aB 0 ~
                            g BT /F1 10 Tf 125 704 Td (see note) Tj ET"
                           (pal-rounded 120 694 70 20 4))))
      (loop for (producer strength)
            in
            '(("Kea Writer" :look) ("Sejda PDF Desktop" :strong) ("pikepdf 8.15" :strong) ("LibreOffice 7.6" :strong))
            do
            (let ((findings (nth-value 2
                                       (pal-read (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font ~
                                                                           << /F1 5 0 R >> >> /Contents 4 0 R") content
                                                                   :more (list (cons 6
                                                                                     (format nil
                                                                                             "<< /Producer (~a) >>"
                                                                                             producer))))))))
                 (true (pal-finding findings strength "partly covered by a white shape") "~a ~s"
                       producer (words findings)))))
    ;; a panel over a watermark with no text of its own: strong; with text of its own, worth a look
    (dolist (case '(("" :strong) ("0 g BT /F1 12 Tf 80 560 Td (Summary) Tj ET" :look)))
      (let ((findings (nth-value 2 (pal-read (pal-one-page (format nil
                                                                   "0.85 g BT /F1 60 Tf 0.707 0.707 -0.707 0.707 100 ~
                                                                    400 Tm (UNPAID) Tj ET 0 g BT /F1 12 Tf 72 700 Td ~
                                                                    (Body text of the page) Tj ET 1 1 1 rg ~af ~a"
                                                                   (pal-rounded 60 380 300 220 6) (first case)))))))
        (true (pal-finding findings (second case) "covered by a white shape") "~s ~s" case (words findings))))
    ;; a watermark told by its turn alone is named by what was measured, not called light
    (let* ((findings (nth-value 2 (pal-read (pal-one-page (format nil
                                                                  "0.5 g BT /F1 40 Tf 0.707 0.707 -0.707 0.707 100 420 ~
                                                                   Tm (Confidential draft) Tj ET 0 g BT /F1 12 Tf 72 ~
                                                                   700 Td (Body text of the page) Tj ET 1 1 1 rg ~af 0 ~
                                                                   g BT /F1 12 Tf 80 560 Td (Summary) Tj ET"
                                                                  (pal-rounded 60 380 400 240 6))))))
           (found (pal-finding findings :look "drawn before all the page's text and turned 45 degrees")))
      (true found "~s" (words findings))
      (when found (false (search "light" (palimpsest::finding-words found)))))))

(define-test (palimpsest palimpsest-path-text-box-drawn-as-made)
  ;; an office program's text box drawn as a path, its own text on it: worth a look, as a
  ;; rectangle's is
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page (format nil "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 100 694 m 66 694 ~
                                              l 66 714 l 184 714 l 184 694 l 100 694 l h f 0 g BT /F1 12 Tf 72 703 Td ~
                                              (Data Size in Bytes) Tj ET")))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :look "drawn with the text as the file was made")))
  ;; as LibreOffice Calc draws one: the cell and the box each in a q ... Q block of their own, the
  ;; box's text in another font kept in the file, all named by the structure tree: worth a look;
  ;; untagged, or with more than the box and its text in its block, or the hidden text sharing its
  ;; block with other text, strong
  (flet ((calc (more cell)
           (format nil
                   "/P <</MCID 0>> BDC q BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET ~a Q EMC q /P <</MCID 1>> BDC ~
                    1 1 1 rg 100 694 m 66 694 l 66 714 l 184 714 l 184 694 l 100 694 l h f EMC /P <</MCID 2>> BDC 0 g ~
                    BT /F3 12 Tf 72 703 Td (Data Size in Bytes) Tj ET EMC ~a Q" cell more)))
    (loop for (more cell tagged
                    strength) in
                    '(("" "" t :look) ("" "" nil :strong) ("BT /F1 12 Tf 300 500 Td (Other) Tj ET" "" t :strong)
                                               ("" "BT /F1 12 Tf 300 600 Td (Next line) Tj ET" t :strong))
          do (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page (calc more cell) :tagged tagged))
               (declare (ignore pdf accounts))
               (true (pal-finding findings strength "covered by a white shape") "~s ~s ~s ~s" more cell tagged
                     (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-cover-replacement-not-made)
  ;; a box whose own text is set as the hidden text is, its font, size and line, over the part
  ;; hidden: new text in place of old, strong however the file was made; set off the line, as a text
  ;; box's text is, worth a look
  (loop for (own strength) in '(("BT /F1 12 Tf 72 700 Td (Data Size in Bytes) Tj ET" :strong)
                                ("BT /F1 12.2 Tf 72 700.8 Td (Data Size in Bytes) Tj ET" :strong)
                                ("BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET" :look))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-office-page (format nil
                                                  "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 ~
                                                   18 re f 0 g ~a" own)))
             (declare (ignore pdf accounts))
             (true (pal-finding findings strength "covered by a white shape") "~s ~s" own
                   (mapcar #'palimpsest::finding-words findings))))
  ;; a figure in mid-line: the new one over the part hidden, strong; text of the box's own beside
  ;; it, a look
  (loop for (own strength)
        in '(("BT /F1 12 Tf 132 700 Td (195,000.00) Tj ET" :strong)
             ("BT /F1 12 Tf 132 703.5 Td (195,000.00) Tj ET" :look))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-office-page (format nil
                                                  "BT /F1 12 Tf 72 700 Td (Price NZ$ 184,500.00 plus GST and costs) Tj ~
                                                   ET 1 1 1 rg 130 697 64 18 re f 0 g ~a" own)))
             (declare (ignore pdf accounts))
             (true (pal-finding findings strength "partly covered by a white shape") "~s ~s" own
                   (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-keys-under-c-c-p)
  ;; its commands under a prefix of its own, C-c p, as every package's are, the prefix named palimpsest; no key
  ;; of the report outside it
  (let ((keymap (tuatara:mode-keymap 'palimpsest::palimpsest-mode)))
    (loop for (keys command) in '(("C-c p d" palimpsest::palimpsest-draw-objects)
                                  ("C-c p o" palimpsest::palimpsest-show-object)
                                  ("C-c p s" palimpsest::palimpsest-save)
                                  ("C-c p w" palimpsest::palimpsest-write-revision)
                                  ("C-c p r" palimpsest::palimpsest-write-report)
                                  ("C-c p q" palimpsest::palimpsest-quit)
                                  ("C-c p D" palimpsest::palimpsest-write-drawing)
                                  ("C-c p c" palimpsest::palimpsest-group-cases)
                                  ("C-c p t" palimpsest::palimpsest-show-shapes)
                                  ("C-c p T" palimpsest::palimpsest-write-shapes)
                                  ("C-c p g" palimpsest::palimpsest-compare-genuine))
          do (is eq command (tuatara.command:lookup-key keymap (tuatara.frontend:parse-key-sequence keys)) "~a" keys))
    (dolist (keys '("o" "s" "w" "d" "q" "D" "c" "t" "T" "g" "C-x C-w"))
      (false (let ((command (tuatara.command:lookup-key keymap (tuatara.frontend:parse-key-sequence keys))))
               (and (symbolp command) (eq (symbol-package command) (find-package '#:palimpsest))))
             "~a" keys))))

(define-test (palimpsest palimpsest-methods-and-craft)
  ;; a stroke of a grained scan copied over another: the way named, skilled, shown by a strong sign
  (let* ((grey (pal-grained (pal-scan)))
         (from-x (+ 20 95 -2)) (from-y (+ 20 38 -2))
         (to-x (+ 20 (* 2 95) (* 3 8) -2)) (to-y (+ 20 (* 4 38) -2)))
    (loop for y below 22
          do (loop for x below 8
                   do (setf (aref grey (+ (* (+ to-y y) 600) to-x x)) (aref grey (+ (* (+ from-y y) 600) from-x x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (let ((ways (palimpsest::methods-found findings)))
        (is eq :characters-composed (first (first ways)))
        (is = 3 (second (first ways)))
        (true (search "Skilled, level 3 of 4" (palimpsest::craft-words ways)))
        (false (search "worth a look" (palimpsest::craft-words ways))))))
  ;; a scan untouched: no way of editing
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-scan))))
    (declare (ignore pdf accounts))
    (is equal '() (palimpsest::methods-found findings))
    (is string= "No way of editing shown." (palimpsest::craft-words '())))
  ;; signs worth a look alone, the highest of them expert work: no level given as shown
  (let ((words (palimpsest::craft-words
                (palimpsest::methods-found
                 (list (palimpsest::found :look "near" :method :characters-composed-resaved)
                       (palimpsest::found :look "moved" :method :moved-in-scan))))))
    (false (search "Expert, level 4 of 4" words) "~a" words)
    (pal-has words "No strong sign, so no level of craft shown")
    (pal-has words "up to level 4 of 4, expert")
    (pal-has words "An honest file can show as much.")))

(define-test (palimpsest palimpsest-near-pairs-noted)
  ;; near pairs that hold a piece cut at a neck, or that stand on a page naming more than five: noted for the
  ;; record; a pair alike only as each other's nearest, a page of two near pairs, and pairs named for another
  ;; reason, stay as they are
  (flet ((group (page &key split (difference 3.0) why)
           (list (list (palimpsest::make-glyph :page page :split split) (palimpsest::make-glyph :page page))
                 difference nil 10.0 :look why nil)))
    (let* ((piece (group 0 :split :sure)) (far (group 0 :difference 6.0)) (stamp (group 1 :why :stamp))
           (many (loop repeat 6 collect (group 1))) (few (loop repeat 2 collect (group 2))))
      (multiple-value-bind (kept noted) (palimpsest::near-copies-noted (append (list piece far stamp) many few))
        (is equal (list* far stamp few) kept)
        (is equal (list* :piece (make-list 6 :initial-element :many)) (mapcar #'cdr noted))
        (is eq piece (car (first noted)))))))

(define-test (palimpsest palimpsest-baseline-alone-bound)
  ;; a word off its line by its baseline alone up to 5 pixels, as a paste lies, stays worth a look; further, as
  ;; headings and superscripts lie, it is noted
  (false (palimpsest::baseline-only-p '((:offset 2.0 0 "a ~a baseline"))))
  (false (palimpsest::baseline-only-p '((:offset -4.9 0 "a ~a baseline"))))
  (true (palimpsest::baseline-only-p '((:offset 5.5 0 "a ~a baseline"))))
  (false (palimpsest::baseline-only-p '((:offset 6.0 0 "a ~a baseline") (:ink 90 50 "~a ink")))))

(define-test (palimpsest palimpsest-honest-kinds-said)
  ;; a sign worth a look of a kind honest files show says how often they do and its honest causes, once; a kind
  ;; honest files are not known to show says nothing more
  (let ((sentence (palimpsest::honest-sentence :near-pairs)))
    (pal-has sentence "Honest files show this too: of 1,445 measured, ")
    (pal-has sentence "show characters nearer one another than prints; known honest causes: ")
    (is eq nil (palimpsest::honest-sentence :no-such-kind))
    (let ((f (palimpsest::found :look "Two characters are near." :kind :near-pairs)))
      (palimpsest::weigh-finding f)
      (palimpsest::weigh-finding f)
      (is string= (format nil "Two characters are near. ~a" sentence) (palimpsest::finding-words f)))
    (let ((f (palimpsest::found :look "Paper copied." :kind nil)))
      (is string= "Paper copied." (palimpsest::finding-words (palimpsest::weigh-finding f))))
    (let ((both (palimpsest::honest-sentence '(:words-ink :words-character))))
      (pal-has both "show a word in another ink, ")
      (pal-has both "show a character off its line"))))

(define-test (palimpsest palimpsest-place-worth-changing)
  ;; a line holding a figure or a field's label is worth changing; five words of running text or more without
  ;; either are not; a short line without figures, a name or a heading, cannot be told
  (is eq :field (palimpsest::place-worth-changing "Total due for fence and gate: $8,250.00"))
  (is eq :field (palimpsest::place-worth-changing "Invoice date"))
  (is eq :field (palimpsest::place-worth-changing "IBAN SE45 5000 0000 0583 9825 7466"))
  (is eq :prose (palimpsest::place-worth-changing "The goods remain the property of the seller until paid in full"))
  (is eq nil (palimpsest::place-worth-changing "Tui Gardens Ltd"))
  (is eq nil (palimpsest::place-worth-changing nil))
  ;; a run a little above its line in running text is noted, with why; on an amount it stays worth a look, first
  (let ((findings (nth-value 2 (pal-read (pal-one-page
                                          (format nil "BT /F1 12 Tf 72 700 Td (The goods remain the property of the ~
                                                       seller) Tj ET BT /F1 12 Tf 330 700.6 Td (until paid) Tj ET ~
                                                       BT /F1 12 Tf 72 600 Td (Total due for fence and gate: $) Tj ~
                                                       ET BT /F1 12 Tf 250 600.6 Td (8,250.00) Tj ET"))))))
    (let ((prose (pal-finding findings :record "a little above or below"))
          (field (pal-finding findings :look "sits a little above or below")))
      (true prose "~s" (mapcar #'palimpsest::finding-words findings))
      (true field "~s" (mapcar #'palimpsest::finding-words findings))
      (when prose
        (pal-has (palimpsest::finding-words prose) "running text or boilerplate, where a change would serve no one")
        (pal-has (palimpsest::finding-words prose) "Honest files show this too")
        (pal-has (format nil "~{~a~}" (palimpsest::finding-details prose)) "until paid"))
      (when field
        (is eq :field (palimpsest::finding-place field))
        (pal-has (format nil "~{~a~}" (palimpsest::finding-details field)) "8,250.00")))))

(define-test (palimpsest palimpsest-looks-ordered)
  ;; worth a look: kinds honest files are not known to show first, then signs on places worth changing, then the
  ;; rest
  (let* ((rest (palimpsest::found :look "rest" :kind :near-pairs :page 1))
         (field (palimpsest::found :look "field" :kind :near-pairs :place :field :page 2))
         (never (palimpsest::found :look "never" :page 3))
         (pdf (palimpsest::read-pdf (pal-original) :name "test.pdf")))
    (is equal '("never" "field" "rest")
        (mapcar (lambda (f) (first (uiop:split-string (palimpsest::finding-words f))))
                (palimpsest::sorted-findings (list rest field never) (palimpsest::last-revision pdf))))))

(define-test (palimpsest palimpsest-jpeg-tables-told)
  ;; the standard's table scaled for quality 50 is itself; for 90, a fifth of it; the same tables the same name
  (flet ((jpeg (scale)
           (coerce (append '(#xff #xd8 #xff #xdb 0 67 0)
                           (loop for s across palimpsest::*jpeg-luminance-table*
                                 collect (max 1 (floor (+ (* s scale) 50) 100)))
                           '(#xff #xda 0 2))
                   '(simple-array (unsigned-byte 8) (*)))))
    (is = 50 (nth-value 1 (palimpsest::jpeg-tables (jpeg 100))))
    (is = 90 (nth-value 1 (palimpsest::jpeg-tables (jpeg 20))))
    (is string= (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 20)))
        (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 20))))
    (isnt string= (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 20)))
          (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 100))))
    (false (palimpsest::jpeg-tables (coerce '(#xff #xd8 #xff #xda 0 2) '(simple-array (unsigned-byte 8) (*)))))
    ;; a table cut short, or a segment longer than the octets, read as far as they go, no quality told
    (dolist (octets '((#xff #xd8 #xff #xdb 0 67 0) (#xff #xd8 #xff #xdb 0 67 16 1 2 3)
                      (#xff #xd8 #xff #xdb 255 255 0 9)))
      (multiple-value-bind (tables quality)
          (palimpsest::jpeg-tables (coerce octets '(simple-array (unsigned-byte 8) (*))))
        (declare (ignore tables))
        (false quality)))))

(define-test (palimpsest palimpsest-written-names-and-labels)
  ;; a name with delimiters and spaces in it written with # and hex, ending nowhere early; a label always text
  (is string= "/a#20b#28c#29#2F#3E#3E" (palimpsest::pdf-name-text "/a b(c)/>>"))
  (is string= "<< /Subtype /Q#20Q#0A >>" (palimpsest::pdf-value-text '(("/Subtype" . "/Q Q
"))))
  (is string= "(/Square \\(x\\) \\033)" (palimpsest::pdf-literal-text (format nil "/Square (x) ~c" (code-char 27)))))

(define-test (palimpsest palimpsest-fingerprint-escaped)
  ;; a name or a program with a newline or an escape in it stays one line, shown as the editor shows it
  (let* ((analysis (palimpsest::make-analysis :name (format nil "a~%method: forged.pdf") :sha256 "x"
                                              :fingerprint (list (list :editor (format nil "Weka~c]0;title~c"
                                                                                       (code-char 27) (code-char 7))))))
         (text (palimpsest::fingerprint-text analysis)))
    (is = 4 (count #\Newline text))
    (false (find (code-char 27) text))
    (false (find (code-char 7) text))
    (false (search (format nil "~%method:") text))))

(define-test (palimpsest palimpsest-scans-bounded)
  ;; a picture of more words than a page holds is no page of text
  (let ((palimpsest::*scan-words-most* 50))
    (false (palimpsest::picture-words (pal-scan) 600 500)))
  (true (palimpsest::picture-words (pal-scan) 600 500)))

(define-test (palimpsest palimpsest-cases-grouped)
  ;; two files edited the same ways with the same program are one case; one sharing a way alone is not;
  ;; one of the same source edited by no one never is
  (with-test-directory (dir)
    (flet ((written (directory name lines)
             (let ((path (format nil "~a~a/~a-fingerprint.txt" dir directory name)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output)
                 (format out "palimpsest fingerprint 1~%file: ~a.pdf~%sha-256: x~%~{~a~%~}" name lines)))))
      (written "case-a" "invoice"
               '("method: text-covered" "method: pdf-editor" "editor: Weka PDF 3" "made-by: Kea Writer"))
      (written "case-b" "receipt"
               '("method: text-covered" "method: pdf-editor" "editor: Weka PDF 3" "made-by: Tui Office"))
      (written "case-b" "statement" '("method: text-covered" "made-by: Kea Writer"))
      (written "case-c" "original" '("made-by: Kea Writer"))
      (let* ((prints (palimpsest::fingerprints-under (namestring dir)))
             (groups (palimpsest::group-fingerprints prints)))
        (is = 4 (length prints))
        (is equal '("invoice.pdf" "receipt.pdf") (mapcar #'second (first (first groups))))
        (is equal '((:editor "Weka PDF 3") (:method "pdf-editor") (:method "text-covered"))
            (sort (copy-list (second (first groups))) #'string< :key #'second))
        (is = 3 (length groups))
        (true (every (lambda (g) (= 1 (length (first g)))) (rest groups)))
        ;; a fingerprint read as it was written
        (multiple-value-bind (name traits)
            (palimpsest::read-fingerprint (format nil "~acase-c/original-fingerprint.txt" dir))
          (is string= "original.pdf" name)
          (is equal '((:made-by "Kea Writer")) traits))
        (false (palimpsest::read-fingerprint (format nil "~acase-c/none.txt" dir)))))))

;;; The shapes of a scan's characters

(defparameter *pal-type*
  '((#\a "....." "....." ".###." "....#" ".####" "#...#" ".####")
    (#\c "....." "....." ".###." "#...." "#...." "#...#" ".###.")
    (#\d "....#" "....#" ".####" "#...#" "#...#" "#...#" ".####")
    (#\e "....." "....." ".###." "#...#" "#####" "#...." ".###.")
    (#\h "#...." "#...." "####." "#...#" "#...#" "#...#" "#...#")
    (#\i "..#.." "....." ".##.." "..#.." "..#.." "..#.." ".###.")
    (#\l ".##.." "..#.." "..#.." "..#.." "..#.." "..#.." ".###.")
    (#\m "....." "....." "##.#." "#.#.#" "#.#.#" "#.#.#" "#.#.#")
    (#\n "....." "....." "####." "#...#" "#...#" "#...#" "#...#")
    (#\o "....." "....." ".###." "#...#" "#...#" "#...#" ".###.")
    (#\r "....." "....." "#.##." "##..#" "#...." "#...." "#....")
    (#\s "....." "....." ".####" "#...." ".###." "....#" "####.")
    (#\t ".#..." ".#..." "####." ".#..." ".#..." ".#..#" "..##.")
    (#\u "....." "....." "#...#" "#...#" "#...#" "#..##" ".##.#"))
  "A typeface for made-up scans: each letter five dots wide and seven tall, # for ink.")

(defparameter *pal-other-type*
  '((#\a "....." "....." "..#.." ".#.#." "#...#" "#####" "#...#")
    (#\d "....#" "....#" "..###" ".#..#" "#...#" ".#..#" "..###")
    (#\e "....." "....." "#####" "#...." "####." "#...." "#####")
    (#\r "....." "....." "##.##" ".##.." ".#..." ".#..." "###..")
    (#\t "..#.." "..#.." "#####" "..#.." "..#.." "..#.." "..###"))
  "Another typeface for made-up scans, some letters of *PAL-TYPE* drawn otherwise.")

(defun pal-typeset (lines &key (seed 5) lowered)
  "The grey octets of a scanned page, 600 by 500 pixels, of LINES, each a list of words (text, set in
*PAL-TYPE*) or (TEXT :other), (TEXT :bold) or (TEXT :italic). Each dot of a letter is 3 pixels square
\(bold, 5 wide; italic, leaning right by a pixel every six rows; :other in *PAL-OTHER-TYPE*). LOWERED holds
characters set lower, each (LINE CHARACTER BY): the CHARACTERth of line LINE (both from 0, the characters of
the line counted across its words) set BY pixels lower, a fraction of a pixel as its share of each row. The
page is blurred by a 3 by 3 mean and given grain of up to 6 grey levels either way, seeded by SEED."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0)))
    (loop for line in lines for l from 0
          do (let ((x 20) (top (+ 20 (* l 38))) (k -1))
               (dolist (word line)
                 (destructuring-bind (text &optional style) (if (consp word) word (list word))
                   (loop for c across text
                         do (let ((rows (rest (or (and (eq style :other) (assoc c *pal-other-type*))
                                                  (assoc c *pal-type*))))
                                  (by (or (third (find-if (lambda (o) (and (= (first o) l) (= (second o) (incf k))))
                                                          lowered))
                                          0)))
                              (loop for row in rows for r from 0
                                    do (loop for dot across row for d from 0
                                             when (char= dot #\#)
                                               do (loop for dy below 3
                                                        for lean = (if (eq style :italic) (floor (- 21 (* 3 r) dy) 6) 0)
                                                        do (loop for dx below (if (eq style :bold) 5 3)
                                                                 do (multiple-value-bind (y part)
                                                                        (floor (+ top (* 3 r) dy by))
                                                                      ;; the row it falls in, and the next its share
                                                                      (loop for (row share) in (list (list y (- 1 part))
                                                                                                     (list (1+ y) part))
                                                                            for at = (+ (* row 600) x lean (* 3 d) dx)
                                                                            do (setf (aref grey at)
                                                                                     (max 50d0 (- (aref grey at)
                                                                                                  (* 185 share))))))))))
                              (incf x (+ 15 (if (eq style :bold) 2 0) 4))))
                   (incf x 12)))))
    (let ((copy (copy-seq grey)) (out (make-array (* 600 500) :element-type '(unsigned-byte 8))) (s seed))
      (loop for y from 1 below 499
            do (loop for x from 1 below 599
                     do (setf (aref grey (+ (* y 600) x))
                              (/ (loop for dy from -1 to 1
                                       sum (loop for dx from -1 to 1 sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9))))
      (dotimes (i (length out) out)
        (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
        (setf (aref out i) (max 0 (min 255 (round (+ (aref grey i) -6 (mod (ash s -16) 13))))))))))

(defparameter *pal-honest-lines*
  '(("the" "stone" "cloud" "and" "the" "rain")
    ("mother" "danced" "in" "the" "tunnel")
    ("salt" "on" "the" "island" "and" "moon")
    ("the" "chest" "of" "mind" "is" "lunch")
    ("hold" "the" "nest" "and" "the" "home")
    ("sound" "of" "the" "rest" "and" "dream")
    ("tumen" "harm" "cold" "lead" "dish"))
  "Lines of a made-up scan in *PAL-TYPE*, every letter in them more than once.")

(defun pal-page-scan (grey)
  "GREY, a scanned page 600 by 500 pixels, read as palimpsest reads a scanned picture (SCAN-PICTURE-READ)."
  (let* ((pdf (palimpsest::read-pdf (pal-scanned grey) :name "scan.pdf"))
         (revision (palimpsest::last-revision pdf)))
    (destructuring-bind (mark page) (first (palimpsest::pictures-of pdf revision :keep #'palimpsest::scan-picture-p))
      (palimpsest::scan-picture-read pdf revision mark page))))

(defun pal-cropped (grey dx dy &key (width 600) (height 500))
  "A new vector of GREY (WIDTH by HEIGHT) with its first DX columns and DY rows cut away, as a page cropped after
it was saved as a JPEG; it is DX narrower and DY lower."
  (let ((out (make-array (* (- width dx) (- height dy)) :element-type '(unsigned-byte 8))))
    (dotimes (y (- height dy) out)
      (dotimes (x (- width dx))
        (setf (aref out (+ (* y (- width dx)) x)) (aref grey (+ (* (+ y dy) width) x dx)))))))

(define-test (palimpsest palimpsest-jpeg-history-told)
  ;; a page saved as a JPEG of quality 30 to 98 and kept without loss is told so, its grid found where the
  ;; picture now begins when it was cropped after; the page never saved so, posterised or made 1-bit, is not
  (let ((page (pal-typeset *pal-honest-lines*)))
    (false (palimpsest::jpeg-history page 600 500) "never saved")
    (false (palimpsest::jpeg-history (map '(simple-array (unsigned-byte 8) (*)) (lambda (v) (* 36 (round v 36))) page)
                                     600 500)
           "posterised")
    (false (palimpsest::jpeg-history (map '(simple-array (unsigned-byte 8) (*)) (lambda (v) (if (< v 128) 30 230)) page)
                                     600 500)
           "1-bit")
    (dolist (quality '(30 75 98))
      (let ((saved (pal-jpeg-saved (copy-seq page) 0 0 600 500 :quality quality)))
        (true (palimpsest::jpeg-history saved 600 500) "quality ~d" quality)
        (multiple-value-bind (once mean sigmas corner) (palimpsest::jpeg-history (pal-cropped saved 3 5) 597 495)
          (true once "quality ~d, cropped: ~,3f, ~,1f" quality mean sigmas)
          (is equal '(5 3) corner "quality ~d, cropped" quality))))))

(defun pal-drawn-on-grid (&key (blur 0.5d0))
  "The grey octets of a page drawn by a program, 600 by 500 pixels, no grain: the letters of *PAL-HONEST-LINES*
in dots of 3 pixels (ink 40) on paper 235, over a grid of lines a pixel wide every 8 pixels (60), the whole
blurred by a Gaussian of BLUR pixels, as a form or graph paper rendered and smoothed is."
  (let ((g (make-array (* 600 500) :element-type 'double-float :initial-element 235d0)))
    (loop for line in *pal-honest-lines* for l from 0
          do (let ((x 20) (top (+ 20 (* l 38))))
               (dolist (word line)
                 (loop for c across word
                       do (loop for row in (rest (assoc c *pal-type*)) for r from 0
                                do (loop for dot across row for d from 0
                                         when (char= dot #\#)
                                           do (dotimes (dy 3)
                                                (dotimes (dx 3)
                                                  (setf (aref g (+ (* (+ top (* 3 r) dy) 600) x (* 3 d) dx)) 40d0)))))
                          (incf x 19))
                 (incf x 12))))
    (dotimes (y 500)
      (dotimes (x 600)
        (when (or (zerop (mod x 8)) (zerop (mod y 8)))
          (setf (aref g (+ (* y 600) x)) (min (aref g (+ (* y 600) x)) 60d0)))))
    (let* ((w (loop for k from -2 to 2 collect (exp (/ (* k k) (* -2 blur blur)))))
           (w (mapcar (lambda (v) (/ v (reduce #'+ w))) w))
           (across (copy-seq g)))
      (loop for y below 500
            do (loop for x from 2 below 598
                     do (setf (aref across (+ (* y 600) x))
                              (loop for k from -2 to 2 for v in w sum (* v (aref g (+ (* y 600) x k)))))))
      (loop for y from 2 below 498
            do (loop for x below 600
                     do (setf (aref g (+ (* y 600) x))
                              (loop for k from -2 to 2 for v in w sum (* v (aref across (+ (* (+ y k) 600) x))))))))
    (map '(simple-array (unsigned-byte 8) (*)) #'round g)))

(define-test (palimpsest palimpsest-jpeg-history-drawn-grid)
  ;; a page drawn by a program on a grid every 8 pixels, smoothed, no grain, repeats its blocks exactly, and their
  ;; few frequencies can lie near whole numbers by chance: not read as once a JPEG; saved as one, it is
  (let ((page (pal-drawn-on-grid)))
    (false (palimpsest::jpeg-history page 600 500))
    (true (palimpsest::jpeg-history (pal-jpeg-saved (copy-seq page) 0 0 600 500 :quality 75) 600 500))))

(define-test (palimpsest palimpsest-once-jpeg-grid-pairs)
  ;; a page saved as a JPEG of quality 30 and kept without loss is read as once a JPEG, its grid from the corner;
  ;; the same page never so saved is not. On a page once a JPEG, a letter the same pixel for pixel as another whole
  ;; blocks of 8 apart, as the JPEG makes clean prints, is worth a look; a letter laid 2 rows off the grid stays a
  ;; strong sign, as on the page never saved
  (let ((page (pal-typeset *pal-honest-lines*)))
    (false (palimpsest::scan-picture-once-jpeg (pal-page-scan page)))
    (is equal '(0 0) (palimpsest::scan-picture-once-jpeg
                      (pal-page-scan (pal-jpeg-saved (copy-seq page) 0 0 600 500 :quality 30))))
    (let ((glyphs (pal-page-glyphs (pal-laid (copy-seq page) 18 18 18 322 20 26))))
      (true (find :strong (palimpsest::scan-reading-copies (palimpsest::copied-glyphs glyphs)) :key #'fifth))
      (let ((copies (palimpsest::scan-reading-copies (palimpsest::copied-glyphs glyphs :once-jpeg '((7 0 0))))))
        (false (find :strong copies :key #'fifth))
        (true (find :grid copies :key #'sixth))))
    (let ((glyphs (pal-page-glyphs (pal-laid (copy-seq page) 18 18 18 324 20 26))))
      (true (find :strong (palimpsest::scan-reading-copies (palimpsest::copied-glyphs glyphs :once-jpeg '((7 0 0))))
                  :key #'fifth)))))

(define-test (palimpsest palimpsest-once-jpeg-copy-off-grid)
  ;; a page saved as a JPEG of quality 90 or 50 and kept without loss, its characters showing the JPEG's grid; a
  ;; letter laid pixel for pixel whole blocks from its source is worth a look on that grid, one laid 3 across and
  ;; 5 down from it is a strong sign, the grid known from the whole picture
  (dolist (quality '(90 50))
    (let ((page (pal-jpeg-saved (pal-typeset *pal-honest-lines*) 0 0 600 500 :quality quality)))
      (pal-laid page 18 18 18 322 20 26)
      (pal-laid page 113 18 116 327 20 26)
      (let* ((glyphs (pal-page-glyphs page))
             (copies (palimpsest::scan-reading-copies (palimpsest::copied-glyphs glyphs :once-jpeg '((7 0 0))))))
        (true (palimpsest::stepped-picture-p glyphs) "quality ~d" quality)
        (flet ((strengths-near (low high)
                 (loop for c in copies
                       when (some (lambda (g) (<= low (palimpsest::glyph-y0 g) high)) (first c)) collect (fifth c))))
          (is equal '(:look) (remove-duplicates (strengths-near 320 325)) "quality ~d, on the grid" quality)
          (true (member :strong (strengths-near 326 331)) "quality ~d, off the grid" quality))))))

(defun pal-places (shapes)
  "Where each of SHAPES is, as (LINE POSITION)."
  (mapcar (lambda (s) (list (palimpsest::shape-line s) (palimpsest::shape-position s))) shapes))

(defun pal-shape-groups (lines)
  "The groups and the shapes alone (SHAPE-GROUPS) of the made-up scan of LINES (PAL-TYPESET), of its one size."
  (let ((sizes (palimpsest::page-shapes (pal-page-scan (pal-typeset lines)))))
    (is = 1 (length sizes) "~s" (mapcar #'car sizes))
    (palimpsest::shape-groups (cdr (first sizes)))))

(define-test (palimpsest palimpsest-shapes-grouped)
  ;; an honest page: every character in a group of its shape, none alone; the same page, the same groups
  (multiple-value-bind (groups alone) (pal-shape-groups *pal-honest-lines*)
    (is equal '() (pal-places alone))
    (true (<= 10 (length groups) 14) "~s" (mapcar #'pal-places groups))
    (multiple-value-bind (again again-alone) (pal-shape-groups *pal-honest-lines*)
      (is equal (mapcar #'pal-places groups) (mapcar #'pal-places again))
      (is equal (pal-places alone) (pal-places again-alone))))
  ;; one word set in another typeface: each of its letters alone, nothing else
  (multiple-value-bind (groups alone)
      (pal-shape-groups (append (subseq *pal-honest-lines* 0 2) '(("the" ("trade" :other) "was" "done"))
                                (subseq *pal-honest-lines* 2)))
    (declare (ignore groups))
    (is equal '((3 4) (3 5) (3 6) (3 7) (3 8)) (pal-places alone)))
  ;; a line set bold and one set in italic: groups of their own, none alone, none with the upright letters
  (let ((lines (append *pal-honest-lines*
                       '((("salt" :bold) ("and" :bold) ("sand" :bold) ("tea" :bold) ("seat" :bold) ("last" :bold)))
                       '((("mint" :italic) ("mine" :italic) ("time" :italic) ("emit" :italic))))))
    (multiple-value-bind (groups alone) (pal-shape-groups lines)
      (is equal '() (pal-places alone))
      (true (every (lambda (group)
                     (let ((lines (remove-duplicates (mapcar #'palimpsest::shape-line group))))
                       (or (every (lambda (l) (<= l 7)) lines) (equal lines '(8)) (equal lines '(9)))))
                   groups)
            "~s" (mapcar #'pal-places groups))
      (true (find-if (lambda (group) (every (lambda (s) (= 8 (palimpsest::shape-line s))) group)) groups))
      (true (find-if (lambda (group) (every (lambda (s) (= 9 (palimpsest::shape-line s))) group)) groups)))))

;;; Characters standing alone

(defparameter *pal-lone-lines*
  (append *pal-honest-lines*
          '(("a" "t" "o" "a" "t" "o" "a" "t" "o")
            ("o" "a" "t" "o" "a" "t" "o" "a" "t")
            ("t" "o" "a" "t" "o" "a" "t" "o" "a")))
  "Lines of a made-up scan in *PAL-TYPE*: honest lines, then three of letters
standing alone, as figures alone in a table's cells stand.")

(defun pal-lone-laid (page from-line from-word to-line to-word)
  "PAGE, a made-up scan of *PAL-LONE-LINES* (PAL-TYPESET), with the letter
standing alone at FROM-WORD of FROM-LINE cut out with the paper about it
and laid over the one at TO-WORD of TO-LINE, pixel for pixel. PAGE."
  (flet ((left (word) (+ 20 (* word 31) -3)) (top (line) (+ 20 (* line 38) -3)))
    (dotimes (y 27 page)
      (dotimes (x 21)
        (setf (aref page (+ (* (+ (top to-line) y) 600) (left to-word) x))
              (aref page (+ (* (+ (top from-line) y) 600) (left from-word) x)))))))

(define-test (palimpsest palimpsest-lone-characters-compared)
  ;; letters standing alone, narrower than their line is tall, are characters set beside others but no words:
  ;; the 27 of the last three lines and the o of each "of", its f not in the typeface
  (let* ((reading (palimpsest::picture-words (pal-grained (pal-typeset *pal-lone-lines*)) 600 500))
         (words (palimpsest::picture-reading-words reading)) (glyphs (palimpsest::picture-reading-glyphs reading)))
    (is = 29 (count-if (lambda (g) (palimpsest::word-box-lone (palimpsest::glyph-word g))) glyphs))
    (false (find-if #'palimpsest::word-box-lone words)))
  ;; one of them laid over another of another shape: the two copies, a strong sign, on another line
  (let ((page (pal-lone-laid (pal-grained (pal-typeset *pal-lone-lines*)) 7 0 8 2)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; the same page untouched: its letters standing alone alike in shape, none in grain
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-typeset *pal-lone-lines*))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-likeness-sample-steady)
  ;; the characters a page's likeness is measured on stay the same as more are read beside them: the 29
  ;; standing alone added to a sample of 40 change it by no more than themselves
  (let* ((glyphs (palimpsest::picture-reading-glyphs
                  (palimpsest::picture-words (pal-grained (pal-typeset *pal-lone-lines*)) 600 500)))
         (words (remove-if (lambda (g) (palimpsest::word-box-lone (palimpsest::glyph-word g))) glyphs)))
    (dolist (g glyphs) (setf (palimpsest::glyph-picture g) 7 (palimpsest::glyph-page g) 0))
    (let ((before (palimpsest::likeness-sample words 40)) (after (palimpsest::likeness-sample glyphs 40)))
      (is = 40 (length before))
      (is = 40 (length after))
      (is = (count-if (lambda (g) (palimpsest::word-box-lone (palimpsest::glyph-word g))) after)
          (length (set-difference before after))))))

(define-test (palimpsest palimpsest-shapes-written)
  ;; the pages of shapes before the scan's own, the revision kept octet for octet, each character a link to
  ;; its place; the word in another typeface framed on the scanned page with its line and place
  (with-test-directory (dir)
    (let* ((octets (pal-scanned (pal-typeset (append (subseq *pal-honest-lines* 0 2)
                                                     '(("the" ("trade" :other) "was" "done"))
                                                     (subseq *pal-honest-lines* 2)))))
           (pdf (palimpsest::read-pdf octets :name "scan.pdf"))
           (analysis (palimpsest::make-analysis :pdf pdf :name "scan.pdf"))
           (revision (palimpsest::last-revision pdf))
           (target (namestring (merge-pathnames "shapes-revision-1.pdf" dir))))
      (is string= target (palimpsest::shapes-copy analysis revision target))
      (let* ((copy (palimpsest::read-pdf (alexandria:read-file-into-byte-vector target) :name "copy.pdf"))
             (last (palimpsest::last-revision copy))
             (pages (palimpsest::pages-of copy last))
             (first (palimpsest::text-lines (palimpsest::marks-of copy last (first pages))))
             (scanned (car (last pages))))
        (true (equalp octets (subseq (alexandria:read-file-into-byte-vector target) 0 (length octets))))
        (is = 2 (length (palimpsest::pdf-revisions copy)))
        (is = 2 (length pages))
        (is = 3 (palimpsest::page-number scanned))
        (true (find "No finding and no judgement" first :test #'search) "~s" first)
        (true (find "Page 1 (page 2 of this copy)" first :test #'search) "~s" first)
        (true (find "Matching no other shape of this size on page 1: 5" first :test #'search) "~s" first)
        (true (find "3:4 3:5 3:6 3:7 3:8" first :test #'search) "~s" first)
        ;; every character drawn leads to the scanned page
        (let ((links (mapcar (lambda (a) (palimpsest::deref copy last a)) (palimpsest::page-annots (first pages)))))
          (true (> (length links) 40))
          (true (every (lambda (link) (equal '(:ref 3 0) (first (palimpsest::pdf-get link "/Dest")))) links)))
        (let ((labels (palimpsest::text-lines (palimpsest::marks-of copy last scanned))))
          (true (find "3:6" labels :test #'search) "~s" labels)))))
  ;; a PDF with no scanned page has none to set side by side
  (multiple-value-bind (pdf accounts) (pal-read (pal-edited))
    (declare (ignore accounts))
    (is eq
        :none (palimpsest::shapes-copy (palimpsest::make-analysis :pdf pdf :name "test.pdf")
                                       (palimpsest::last-revision pdf) "/nonexistent/x.pdf"))))

(defun pal-odd-word-lines ()
  "*PAL-HONEST-LINES* with a word in *PAL-OTHER-TYPE* on the third line, its five letters alone."
  (append (subseq *pal-honest-lines* 0 2) '(("the" ("trade" :other) "was" "done")) (subseq *pal-honest-lines* 2)))

(defun pal-random-shapes (count &optional (seed 7))
  "COUNT shapes of one size, 32 rows of 20 pixels, their pixels at random (SEED): each unlike every other."
  (let ((state (sb-ext:seed-random-state seed)))
    (loop for serial below count
          collect (let ((rows (make-array 32)))
                    (dotimes (j 32) (setf (svref rows j) (random (ash 1 20) state)))
                    (palimpsest::make-shape :rows rows :grown (palimpsest::grown-rows rows 1) :across 20
                                            :ink (palimpsest::rows-ink rows) :serial serial)))))

(defun pal-copy-text (path)
  "The text of each page of the PDF at PATH, as its latest revision draws it: a list of lists of lines."
  (let* ((copy (palimpsest::read-pdf (alexandria:read-file-into-byte-vector path) :name "copy.pdf"))
         (last (palimpsest::last-revision copy)))
    (mapcar (lambda (page) (palimpsest::text-lines (palimpsest::marks-of copy last page)))
            (palimpsest::pages-of copy last))))

(define-test (palimpsest palimpsest-shapes-grouped-in-linear-time)
  ;; characters all unlike each other, as a page of noise or handwriting is: each set beside a bounded number
  ;; of others, so that four times as many take about four times as long, not sixteen
  (flet ((seconds (n)
           (let ((shapes (pal-random-shapes n)) (start (get-internal-real-time)))
             (multiple-value-bind (groups alone) (palimpsest::shape-groups shapes)
               (is = n (+ (length alone) (reduce #'+ groups :key #'length))))
             (/ (- (get-internal-real-time) start) internal-time-units-per-second))))
    (seconds 200)
    (let ((small (max 0.001 (seconds 750))) (large (seconds 3000)))
      (true (< (/ large small) 6) "~,2f s for 750, ~,2f s for 3,000" small large))))

(define-test (palimpsest palimpsest-shapes-bounded)
  ;; what is drawn is bounded, and what is left out said: a group's characters past *shapes-shown-most*,
  ;; groups past *shapes-groups-shown-most*, characters alone past *shapes-alone-shown-most* (framed only
  ;; when drawn), a page's characters past *shapes-page-most*
  (with-test-directory (dir)
    (let* ((pdf (palimpsest::read-pdf (pal-scanned (pal-typeset (pal-odd-word-lines))) :name "scan.pdf"))
           (analysis (palimpsest::make-analysis :pdf pdf :name "scan.pdf"))
           (target (namestring (merge-pathnames "shapes.pdf" dir))))
      (let ((palimpsest::*shapes-shown-most* 3) (palimpsest::*shapes-groups-shown-most* 2)
            (palimpsest::*shapes-alone-shown-most* 2))
        (palimpsest::shapes-copy analysis (palimpsest::last-revision pdf) target))
      (let* ((pages (pal-copy-text target)) (shapes (first pages)) (scanned (car (last pages))))
        (true (find "Matching no other shape of this size on page 1: 5, 2 of them drawn and framed" shapes
                    :test #'search) "~s" shapes)
        ;; the words saying how many more end the row of those drawn
        (true (find "and 3 more" shapes :test #'string=) "~s" shapes)
        (true (find "3:4 3:5" shapes :test #'string=) "~s" shapes)
        (true (find-if (lambda (line) (search "more shapes, of" line)) shapes) "~s" shapes)
        (true (find-if (lambda (line) (cl-ppcre:scan "^\\d+ and \\d+ more$" line)) shapes) "~s" shapes)
        (is = 2 (/ (length (cl-ppcre:all-matches "3:\\d" (format nil "~{~a ~}" scanned))) 2) "~s" scanned)
        (true (search "3:5" (format nil "~{~a ~}" scanned)))
        (false (search "3:6" (format nil "~{~a ~}" scanned))))
      (delete-file target)
      (let ((palimpsest::*shapes-page-most* 50))
        (palimpsest::shapes-copy analysis (palimpsest::last-revision pdf) target))
      ;; the o of each "of" (its f not in the typeface) stands alone, and is among them
      (true (find-if (lambda (line) (search "the first 50 of its 165 characters, from the top" line))
                     (first (pal-copy-text target)))
            "~s" (first (pal-copy-text target)))))
  ;; past the characters read in all, a page's characters are not set side by side, and that is said
  (let ((scan (pal-page-scan (pal-typeset *pal-honest-lines*))))
    (let ((palimpsest::*scan-characters-in-all* 100))
      (multiple-value-bind (count cut) (palimpsest::characters-kept scan 0)
        (true cut)
        (is = 0 (length (palimpsest::scan-picture-glyphs scan)))
        (true (> count 100))
        (true (search "as many as are read in all" (palimpsest::page-heading-words scan cut)))))))

(defun pal-generation-one (octets number)
  "OCTETS, a PDF written by PAL-SAVE, with the object NUMBER written as of generation 1, and so referred to."
  (let* ((text (map 'string #'code-char octets))
         (text (cl-ppcre:regex-replace-all (format nil "(?<![0-9])~d 0 (obj|R)" number) text
                                           (format nil "~d 1 \\1" number)))
         (xref (+ (search (format nil "~%xref~%") text :from-end t) 6))
         (entries (1+ (position #\Newline text :start xref)))
         (entry (+ entries (* 20 number) 11)))
    (setf (subseq text entry (+ entry 5)) "00001")
    (map '(vector (unsigned-byte 8)) #'char-code text)))

(define-test (palimpsest palimpsest-shapes-catalogue-kept)
  ;; the catalogue and the old top of the page tree written again as they were but for where they point, their
  ;; strings kept strings; objects written again with the generation they are referred to by, in the drawing too
  (with-test-directory (dir)
    (let* ((grey (pal-typeset (pal-odd-word-lines)))
           (octets (pal-generation-one
                    (pal-save #()
                              (list '(1 . #.(concatenate 'string "<< /Type /Catalog /Pages 2 0 R /Lang (en-NZ) "
                                                                 "/PageLabels << /Nums [0 << /P (A-) >>] >> >>"))
                                        '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                        '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 "
                                                                     "500] /Resources << /XObject << /Im1 7 0 R >> >> "
                                                                     "/Contents 4 0 R >>"))
                                        '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                        '(5 . "<< >>")
                                        '(6 . "<< /Producer (Kea Scan) >>")
                                        (list 7
                                              :stream (format nil "/Type /XObject /Subtype /Image /Width 600 /Height ~
                                                                   500 /ColorSpace /DeviceGray /BitsPerComponent 8")
                                              grey)))
                    3))
           (pdf (palimpsest::read-pdf octets :name "scan.pdf"))
           (analysis (palimpsest::make-analysis :pdf pdf :name "scan.pdf"))
           (revision (palimpsest::last-revision pdf))
           (target (namestring (merge-pathnames "shapes.pdf" dir)))
           (drawing (namestring (merge-pathnames "objects.pdf" dir))))
      (is = 1 (palimpsest::page-generation (first (palimpsest::pages-of pdf revision))))
      (palimpsest::shapes-copy analysis revision target)
      (let* ((written (map 'string #'code-char (alexandria:read-file-into-byte-vector target)))
             (added (subseq written (length octets))))
        (true (search "<< /Type /Catalog /Pages " added) "~a" added)
        (true (search " /Lang (en-NZ) /PageLabels << /Nums [0 << /P (A-) >>] >> >>" added))
        (true (search "/Kids [3 1 R] /Count 1 /Parent " added))
        (true (search (format nil "~%3 1 obj~%") added))
        (true (cl-ppcre:scan "\\d{10} 00001 n" added))
        (let* ((copy (palimpsest::read-pdf (alexandria:read-file-into-byte-vector target) :name "copy.pdf"))
               (last (palimpsest::last-revision copy)))
          (is string= "en-NZ" (palimpsest::pdf-get (palimpsest::catalogue copy last) "/Lang"))
          (is = 3 (palimpsest::page-number (car (last (palimpsest::pages-of copy last)))))))
      (palimpsest::marked-copy analysis revision drawing)
      (let ((added (subseq (map 'string #'code-char (alexandria:read-file-into-byte-vector drawing)) (length octets))))
        (true (search (format nil "~%3 1 obj~%") added))
        (false (search (format nil "~%3 0 obj~%") added)))))
  ;; a catalogue whose /Pages names a page is refused, nothing written twice
  (let* ((pdf (palimpsest::read-pdf (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 3 0 R >>")
                                                        '(3 . #.(concatenate 'string "<< /Type /Page /MediaBox [0 0 "
                                                                                     "600 500] /Contents 4 0 R >>"))
                                                        '(4 :stream "" "")))
                                    :name "odd.pdf")))
    (pal-has (palimpsest::page-tree-refusal pdf (palimpsest::last-revision pdf)) "names a page, not a tree of pages"))
  ;; a catalogue naming /Pages twice, or a top of the page tree naming /Parent twice, as no honest program
  ;; writes them, refused: a reader takes the last, and pages set before it by the first would not be seen
  (flet ((refusal (catalogue top)
           (let ((pdf (palimpsest::read-pdf (pal-save #() (list (cons 1 catalogue) (cons 2 top)
                                                                '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 "
                                                                                             "0 R /MediaBox [0 0 600 "
                                                                                             "500] /Contents 4 0 R >>"))
                                                                '(4 :stream "" "")))
                                            :name "twice.pdf")))
             (palimpsest::page-tree-refusal pdf (palimpsest::last-revision pdf)))))
    (false (refusal "<< /Type /Catalog /Pages 2 0 R >>" "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"))
    (pal-has (refusal "<< /Type /Catalog /Pages 2 0 R /Pages 2 0 R >>" "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
             "its catalogue names /Pages twice")
    (pal-has (refusal "<< /Type /Catalog /Pages 2 0 R >>"
                      "<< /Type /Pages /Parent 2 0 R /Kids [3 0 R] /Count 1 /Parent 2 0 R >>")
             "its top of the page tree names /Parent twice")))

(define-test (palimpsest palimpsest-shapes-frame-not-judged)
  ;; a heading the scanner clipped: its characters framed dotted and labelled where they are set among the
  ;; shapes and on the scanned page, with the key; a page with none has no such frame and no key
  (let ((label "(not judged: clipped) Tj")
        (key "(Not judged: too few pixels left once the ink the scanner clipped is set aside.) Tj"))
    (flet ((added (grey)
             (with-test-directory (dir)
               (let* ((octets (pal-scanned grey))
                      (pdf (palimpsest::read-pdf octets :name "scan.pdf"))
                      (target (namestring (merge-pathnames "shapes.pdf" dir))))
                 (palimpsest::shapes-copy (palimpsest::make-analysis :pdf pdf :name "scan.pdf")
                                          (palimpsest::last-revision pdf) target)
                 (subseq (map 'string #'code-char (alexandria:read-file-into-byte-vector target)) (length octets)))))
           (occurrences (text in)
             (loop for at = (search text in) then (search text in :start2 (1+ at))
                   while at count t)))
      (let ((clipped (added (pal-clipped-heading (pal-varied :width 6 :height 30 :strokes 6 :pitch 10 :ink 0)))))
        (true (plusp (occurrences label clipped)))
        ;; once on a page of shapes, once on the scanned page, as many of each
        (true (evenp (occurrences label clipped)) "~d" (occurrences label clipped))
        (is = 2 (occurrences key clipped))
        (true (search "[0.4 0.8] 0 d" clipped))
        (true (search "0.000 0.451 0.149 RG" clipped))
        (false (search "A strong sign." clipped)))
      (let ((honest (added (pal-varied :ink 0))))
        (is = 0 (occurrences label honest))
        (is = 0 (occurrences key honest))
        (false (search "[0.4 0.8] 0 d" honest))))))

(define-test (palimpsest palimpsest-write-refusal-names-every-file-there)
  ;; a revision and its log both there already: both named, nothing written over
  (with-test-directory (dir)
    (let* ((evidence (namestring (ensure-directories-exist (merge-pathnames "evidence/test.pdf" dir))))
           (analysis (palimpsest::make-analysis :name "test.pdf" :path evidence))
           (target (namestring (merge-pathnames "test-revision-1.pdf" dir))))
      (with-open-file (out target :direction :output) (write-string "x" out))
      (with-open-file (out (palimpsest::log-beside target) :direction :output) (write-string "x" out))
      (pal-has (palimpsest::target-refusal analysis target (palimpsest::log-beside target))
               "test-revision-1.pdf, test-revision-1.log are there already"))))

(define-test (palimpsest palimpsest-text-written-again-counted)
  ;; how often each text of a page is written, counted once for the page
  (let* ((runs (mapcar (lambda (text) (palimpsest::make-mark :kind :text :text text))
                       '("Main St" "Main St" "Main St" "Bay Rd")))
         (counts (palimpsest::text-counts runs)))
    (is = 3 (gethash "Main St" counts))
    (is = 1 (gethash "Bay Rd" counts))))

(define-test (palimpsest palimpsest-copies-made-by-jobs)
  ;; t and d in the background: done when written, none for a PDF with no scanned page; stopped half-way,
  ;; nothing written; T and D refused beside the evidence and over a file there
  (with-test-directory (dir)
    (let* ((scan (palimpsest::read-pdf (pal-scanned (pal-typeset *pal-honest-lines*)) :name "scan.pdf"))
           (plain (nth-value 0 (pal-read (pal-edited))))
           (shapes (namestring (merge-pathnames "shapes.pdf" dir)))
           (objects (namestring (merge-pathnames "objects.pdf" dir)))
           (none (namestring (merge-pathnames "none.pdf" dir))))
      (flet ((work (pdf target kind)
               (let* ((analysis (pal-examined-in dir (palimpsest::pdf-octets pdf) "x.pdf"))
                      (job (palimpsest::make-palimpsest-draw-job nil analysis
                                                                 (1- (length (palimpsest::pdf-revisions pdf)))
                                                                 target kind)))
                 (palimpsest::copy-work job)
                 (uiop:delete-directory-tree (merge-pathnames "evidence/" dir) :validate t)
                 (uiop:delete-directory-tree (merge-pathnames "report/" dir) :validate t)
                 (list (palimpsest::palimpsest-draw-job-done job) (palimpsest::palimpsest-draw-job-none job)))))
        (is equal '(t nil) (work scan shapes t))
        (true (probe-file shapes))
        (is equal '(t nil) (work scan objects nil))
        (is equal '(nil t) (work plain none t))
        (false (probe-file none)))
      ;; stopped half-way: nothing written
      (let ((stopped (namestring (merge-pathnames "stopped.pdf" dir))))
        (false (palimpsest::shapes-copy (palimpsest::make-analysis :pdf scan :name "scan.pdf")
                                        (palimpsest::last-revision scan) stopped
                                        :cancel (list t)))
        (false (probe-file stopped)))
      ;; where they may be written
      (let* ((evidence (namestring (ensure-directories-exist (merge-pathnames "evidence/scan.pdf" dir))))
             (analysis (palimpsest::make-analysis :pdf scan :name "scan.pdf" :path evidence)))
        (pal-has (palimpsest::target-refusal analysis (namestring (merge-pathnames "evidence/scan-shapes.pdf" dir)))
                 "the directory of the PDF examined")
        (pal-has (palimpsest::target-refusal analysis shapes) "there already")
        (false (palimpsest::target-refusal analysis (namestring (merge-pathnames "elsewhere.pdf" dir))))))))

;;; Likeness of shaped letters, and pages not judged

(defun pal-box-laid (from to fx fy tx ty w h &optional nudge)
  "TO, a made-up scan 600 pixels wide, with the W by H pixels of the scan
FROM at FX FY laid over it at TX TY, pixel for pixel; with NUDGE, every
third pixel of them a grey level lighter. TO."
  (dotimes (y h to)
    (dotimes (x w)
      (let ((g (aref from (+ (* (+ fy y) 600) fx x))))
        (setf (aref to (+ (* (+ ty y) 600) tx x))
              (if (and nudge (zerop (mod (+ (* w y) x) 3))) (min 255 (1+ g)) g))))))

(defun pal-letter-laid (page &optional nudge)
  "PAGE, a made-up scan of *PAL-HONEST-LINES* (PAL-TYPESET), with the e of
its first word laid over the e that ends its second, its paper with it,
pixel for pixel; with NUDGE, every third pixel of it a grey level lighter.
PAGE."
  (pal-box-laid page page 56 23 163 23 20 20 nudge))

(define-test (palimpsest palimpsest-copy-recoloured)
  ;; the e of the first word laid over the e that ends the second, its paper with it, every grey's darkness
  ;; made three fifths by one rule, as a copy given a lighter ink is, its faintest edge lost to the page's level:
  ;; named as recoloured, worth a look; the page untouched, nothing so
  (let ((page (pal-letter-laid (pal-grained (pal-typeset *pal-honest-lines*)))))
    (loop for y from 23 below 43
          do (loop for x from 163 below 183
                   do (let ((at (+ (* y 600) x))) (setf (aref page at) (+ 94 (floor (* 3 (aref page at)) 5))))))
    (let ((found (pal-finding (nth-value 2 (pal-read (pal-scanned page))) :look "greys changed by one rule")))
      (true found)
      (when found (is eq :characters-composed-resaved (palimpsest::finding-method found)))))
  (false (pal-finding (nth-value 2 (pal-read (pal-scanned (pal-grained (pal-typeset *pal-honest-lines*)))))
                      nil "greys changed by one rule")))

(define-test (palimpsest palimpsest-likeness-alone-worth-a-look)
  ;; a letter laid over another of its shape with a third of its grey levels moved by one, on a page kept
  ;; without loss: found by its likeness, far nearer than prints, worth a look and never a strong sign; laid
  ;; pixel for pixel, strong
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-letter-laid (pal-grained (pal-typeset *pal-honest-lines*)) t)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :look "2 characters of the scanned page are nearer one another than prints")
          "~s" (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-letter-laid (pal-grained (pal-typeset *pal-honest-lines*)))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
          "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a stroke alone so laid, its grey levels moved: too little shape for its likeness to tell, not named;
  ;; laid pixel for pixel, strong
  (let ((page (pal-grained (pal-scan))))
    (pal-copy-box page page 19 96 344 286)
    (dotimes (y 18)
      (dotimes (x 5)
        (when (zerop (mod (+ (* 5 y) x) 3))
          (let ((at (+ (* (+ 286 y) 600) 344 x))) (setf (aref page at) (min 255 (1+ (aref page at))))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "characters of the scanned page") "~s"
             (mapcar #'palimpsest::finding-words findings))))
  (let ((page (pal-grained (pal-scan))))
    (pal-copy-box page page 19 96 344 286)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; how much shape a character has: a stroke none, a letter of loops and arms much
  (let ((glyphs (palimpsest::picture-reading-glyphs
                 (palimpsest::picture-words (pal-grained (pal-typeset *pal-honest-lines*)) 600 500))))
    (flet ((at (x)
             (find-if (lambda (g)
                        (and (<= (palimpsest::glyph-x0 g) x (1- (palimpsest::glyph-x1 g)))
                             (< (palimpsest::glyph-y0 g) 50))) glyphs)))
      (true (palimpsest::shapeless-glyph-p (first (palimpsest::picture-reading-glyphs
                                                   (palimpsest::picture-words (pal-grained (pal-scan)) 600 500)))))
      (false (palimpsest::shapeless-glyph-p (at 65)) "the e: ~a" (and (at 65) (palimpsest::glyph-crossings (at 65))))
      (true (> (palimpsest::glyph-crossings (at 65)) 1/2)))))

(defun pal-uneven-prints (grey)
  "GREY, a made-up scan (PAL-TYPESET), its ink straying by up to 50 grey
levels either way, pixel by pixel, as uneven light, focus and sharpening
across a photograph leave prints of one letter unlike. GREY, changed."
  (let ((seed 4321))
    (dotimes (i (length grey) grey)
      (when (< (aref grey i) 150)
        (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
        (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) -50 (mod (ash seed -16) 101)))))))))

(define-test (palimpsest palimpsest-unjudged-pages-measured)
  ;; prints of each letter made unlike by light changing letter by letter: not judged, and said why, with
  ;; how many were set beside others and how few came near
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-uneven-prints (pal-typeset *pal-honest-lines*))))
    (declare (ignore pdf accounts))
    (let ((record (pal-finding findings
                               :record (format nil "Copies and grain not checked on page 1: prints of one character ~
                                                    are too unlike each other to measure"))))
      (true record "~s" (mapcar #'palimpsest::finding-words findings))
      (when record
        (pal-has (first (palimpsest::finding-details record)) "characters set beside others, ")
        (pal-has (first (palimpsest::finding-details record)) "had one of their shape within 30 grey levels")
        (pal-has (first (palimpsest::finding-details record)) "40 are needed"))))
  ;; such prints among a table's rules: both said
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-ruled (pal-uneven-prints (pal-typeset *pal-honest-lines*)))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings
                       :record (format nil "Copies and grain not checked on page 1: prints of one character are too ~
                                            unlike each other to measure"))
          "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings
                       :record "Copies and grain not checked on page 1: characters could not be found among the rules.")
          "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a page drawn by a program, its paper and its ink one grey each: not judged, and said why
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-joined :sigma 0 :blur nil :seed 9)))
    (declare (ignore pdf accounts))
    (let ((record (pal-finding findings
                               :record (format nil "Copies and grain not checked on page 1: its paper and ink as flat ~
                                                    as a program draws them, or as an app's clipping leaves them."))))
      (true record "~s" (mapcar #'palimpsest::finding-words findings))
      (when record
        (pal-has (first (palimpsest::finding-details record)) "its blank paper strays by 0.0 grey levels")))))

(define-test (palimpsest palimpsest-copies-on-an-uneven-page)
  ;; a letter laid over another of its shape on a page whose prints are too unlike to judge it as a whole,
  ;; a third of its grey levels moved by one: found, far nearer than the page's prints, worth a look; the
  ;; page still said not judged, and that its copies were sought all the same
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-letter-laid (pal-uneven-prints (pal-typeset *pal-honest-lines*)) t)))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "2 characters of the scanned page are far nearer one another than its prints")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings :strong "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings))
    (let ((record (pal-finding findings
                               :record (format nil "Copies and grain not checked on page 1: prints of one character ~
                                                    are too unlike each other to measure"))))
      (true record "~s" (mapcar #'palimpsest::finding-words findings))
      (when record
        (pal-has (first (palimpsest::finding-details record)) "within 60 grey levels they are alike to")
        (pal-has (first (palimpsest::finding-details record)) "sought all the same"))))
  ;; laid pixel for pixel there: still worth a look at most, the page's likeness never judged
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-letter-laid (pal-uneven-prints (pal-typeset *pal-honest-lines*)))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "2 characters of the scanned page are far nearer one another than its prints")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings :strong "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings)))
  ;; the same page untouched: no copy named
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-uneven-prints (pal-typeset *pal-honest-lines*))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings))))

(defun pal-calm-within (uneven calm x0 y0 x1 y1 &optional (depth 1))
  "UNEVEN, a made-up scan 600 pixels wide, with the pixels of CALM from X0 Y0
up to X1 Y1 laid over it, as a calmer part of a page from another source,
each as far below the paper's grey of 235 as DEPTH of how far it was: a
lighter ink. UNEVEN, changed."
  (loop for y from y0 below y1
        do (loop for x from x0 below x1
                 do (let ((g (aref calm (+ (* y 600) x))))
                      (setf (aref uneven (+ (* y 600) x)) (max 0 (min 255 (round (- 235 (* depth (- 235 g))))))))))
  uneven)

(defun pal-uneven-findings (grey)
  "The findings of GREY, a made-up scan, read as one scanned page (PAL-READ)."
  (nth-value 2 (pal-read (pal-scanned grey))))

(define-test (palimpsest palimpsest-uneven-page-calmer-part)
  ;; an uneven page whose last line comes from a calmer source, as a receipt photographed on a letter: its
  ;; letters set twice in that line are far nearer each other than the page's prints, but no nearer than
  ;; the prints of their own line; none named
  (let* ((calm (pal-typeset *pal-honest-lines*))
         (findings (pal-uneven-findings (pal-calm-within (pal-uneven-prints (copy-seq calm)) calm 0 244 600 278 0.8))))
    (true (pal-finding findings :record "prints of one character are too unlike each other")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings nil "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-uneven-page-flat-strokes)
  ;; strokes of one grey without grain down a column of an uneven page, two of them alike by their edges, as a
  ;; scanner clips a monospace column: their likeness tells nothing; not named
  (let ((grey (pal-uneven-prints (pal-typeset *pal-honest-lines*))))
    (loop for top in '(20 58 96) for edge in '(150 150 110)
          do (dotimes (y 21)
               (dotimes (x 5)
                 (setf (aref grey (+ (* (+ top y) 600) 560 x)) (if (<= 1 x 3) 40 edge)))))
    (let ((findings (pal-uneven-findings grey)))
      (false (pal-finding findings nil "characters of the scanned page") "~s"
             (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-uneven-page-grain-not-measured)
  ;; a word of one grey without grain on an uneven page: its words are not measured for grain there
  (let ((grey (pal-uneven-prints (pal-typeset *pal-honest-lines*))))
    (loop for y from 16 below 46
          do (loop for x from 86 below 184
                   for at = (+ (* y 600) x)
                   when (< (aref grey at) 150) do (setf (aref grey at) 60)))
    (let ((findings (pal-uneven-findings grey)))
      (false (pal-finding findings nil "no grain of") "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-uneven-page-kept-apart)
  ;; a judged page and an uneven one beside it, a letter of the judged page laid on the uneven one, a third
  ;; of its grey levels moved by one: found, the uneven page's prints set beside the judged page's, worth a
  ;; look; the judged page's prints are never set beside the uneven page's
  (let ((judged (pal-grained (pal-typeset *pal-honest-lines*)))
        (uneven (pal-uneven-prints (pal-typeset *pal-honest-lines*))))
    (pal-box-laid judged uneven 56 23 163 23 20 20 t)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list judged uneven)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "2 characters of the scanned pages are far nearer one another") "~s"
            (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :strong "characters of the scanned page") "~s"
             (mapcar #'palimpsest::finding-words findings)))
    (let* ((glyphs (loop for (grey page) in (list (list judged 0) (list uneven 1))
                         append (let ((glyphs (palimpsest::picture-reading-glyphs
                                                (palimpsest::picture-words grey 600 500))))
                                  (dolist (g glyphs glyphs)
                                    (setf (palimpsest::glyph-page g) page (palimpsest::glyph-picture g) 7)))))
           (uneven-table (let ((table (make-hash-table :test 'equal))) (setf (gethash (cons 7 1) table) t) table))
           (peers (palimpsest::apart-peers glyphs uneven-table)))
      (true (loop for g in glyphs
                  never (and (= 0 (palimpsest::glyph-page g))
                             (find 1 (funcall peers g) :key #'palimpsest::glyph-page))))
      (true (loop for g in glyphs
                  thereis (and (= 1 (palimpsest::glyph-page g))
                               (find 0 (funcall peers g) :key #'palimpsest::glyph-page)))))))

(defparameter *pal-uneven-lines* (append *pal-honest-lines* '(("dish" "dish")))
  "*PAL-HONEST-LINES* and a short last line of one word twice.")

(define-test (palimpsest palimpsest-uneven-page-word-copied-in-a-short-line)
  ;; on an uneven page, a short line of one word set twice, the second laid over by the first, a third of its
  ;; grey levels moved by one: the copies' fellows are no measure of the prints about them; found
  (let ((grey (pal-uneven-prints (pal-typeset *pal-uneven-lines*))))
    (pal-box-laid grey grey 18 284 106 284 82 30 t)
    (let ((findings (pal-uneven-findings grey)))
      (true (pal-finding findings :look "characters of the scanned page are far nearer one another") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-uneven-page-short-calm-line)
  ;; an uneven page whose short last line comes from a calmer source in a lighter ink: its h's, set twice, are
  ;; far nearer each other than the page's prints, but that line holds too few prints to tell; not named
  (let* ((lines (append *pal-honest-lines* '(("harm" "h"))))
         (calm (pal-typeset lines))
         (findings (pal-uneven-findings (pal-calm-within (pal-uneven-prints (copy-seq calm)) calm 0 282 600 316 0.62))))
    (false (pal-finding findings nil "characters of the scanned page") "~s"
           (mapcar #'palimpsest::finding-words findings))))

(defun pal-halved (grey)
  "GREY, a made-up scan 600 by 500 pixels, made half as large in its top left
quarter, each pixel the mean of four, on paper of grey 235 elsewhere: a
page photographed from further away."
  (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 235)))
    (dotimes (y 250 out)
      (dotimes (x 300)
        (setf (aref out (+ (* y 600) x))
              (round (+ (aref grey (+ (* 2 y 600) (* 2 x))) (aref grey (+ (* 2 y 600) (* 2 x) 1))
                        (aref grey (+ (* (1+ (* 2 y)) 600) (* 2 x))) (aref grey (+ (* (1+ (* 2 y)) 600) (* 2 x) 1)))
                     4))))))

(define-test (palimpsest palimpsest-uneven-page-own-scale)
  ;; a page photographed at half the scale of the scan beside it, unevenly, a letter of it laid over another
  ;; of its shape, a third of its grey levels moved by one: judged by its own type's size, found
  (let ((scan (pal-grained (pal-typeset *pal-honest-lines*)))
        (photo (pal-uneven-prints (pal-halved (pal-typeset *pal-honest-lines*)))))
    (pal-box-laid photo photo 27 10 80 10 12 13 t)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list scan photo)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "characters of the scanned page") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-uneven-page-figure-one)
  ;; a figure 1, narrower than half a typical letter, laid over another on an uneven page, a third of its grey
  ;; levels moved by one: found
  (let* ((*pal-type* (cons '(#\1 "..#.." ".##.." "..#.." "..#.." "..#.." "..#.." "..#..") *pal-type*))
         (lines (append *pal-honest-lines* '(("1" "1" "cold" "lead" "1" "rain" "and" "dial"))))
         (grey (pal-uneven-prints (pal-typeset lines))))
    (pal-box-laid grey grey 18 284 49 284 19 30 t)
    (let ((findings (pal-uneven-findings grey)))
      (true (pal-finding findings :look "characters of the scanned page are far nearer one another") "~s"
            (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-likeness-of-a-tight-page)
  ;; a page set so tight that its letters are pieces cut from marks the blur joined: as many of them come
  ;; near a print of their shape as when they stand alone, and the page is judged
  (let* ((glyphs (pal-page-glyphs (pal-grained (pal-scan))))
         (alone (multiple-value-list (palimpsest::alike-characters glyphs (palimpsest::glyph-peers glyphs)))))
    (dolist (g glyphs) (setf (palimpsest::glyph-split g) :sure))
    (let ((tight (multiple-value-list (palimpsest::alike-characters glyphs (palimpsest::glyph-peers glyphs)))))
      (true (>= (third alone) 40) "~s" alone)
      (is = (third alone) (third tight) "alone ~s, tight ~s" alone tight)
      (true (first tight) "~s" tight))))

;;; Words set apart together

(defun pal-lowered (grey places &key (by 4) crisp)
  "GREY, a scanned page drawn as PAL-SCAN draws it, with the words at
PLACES, each (LINE . WORD), drawn again BY pixels lower, over paper of grey
230; with CRISP, grey 0 with no fading on clean paper of grey 250. GREY,
changed."
  (dolist (place places grey)
    (destructuring-bind (line . word) place
      (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 95))))
        (loop for y from (- top 3) below (+ top 18 by 3)
              do (loop for x from (- left 3) below (+ left 73)
                       do (setf (aref grey (+ (* y 600) x)) (if crisp 250 230))))
        (dotimes (stroke 9)
          (let ((x0 (+ left (* stroke 8))))
            (loop for y from (+ top by) repeat 18
                  do (loop for dx from -1 to 3
                           do (setf (aref grey (+ (* y 600) x0 dx))
                                    (cond ((and crisp (<= 0 dx 2)) 0) (crisp 250) ((<= 0 dx 2) 60) (t 140)))))))))))

(define-test (palimpsest palimpsest-character-set-off-its-line)
  ;; the c of danced set 1.5 pixels low: named, its offset and place said; set 0.3 pixels low, and an
  ;; honest page: nothing
  (flet ((characters (by)
           (let ((scan (pal-page-scan (pal-typeset *pal-honest-lines* :lowered (list (list 1 9 by))))))
             (values (loop for (word . ways) in (palimpsest::scan-picture-odd scan)
                           append (loop for way in ways when (eq (first way) :character) collect way))
                     scan))))
    (multiple-value-bind (ways scan) (characters 1.5)
      (is = 1 (length ways) "~s" ways)
      (let ((glyph (fifth (first ways))))
        (true (< 1.2 (second (first ways)) 1.8) "~s" ways)
        (is = (+ 20 (* 9 19) 12 -1) (palimpsest::glyph-x0 glyph))
        (true (search "pixels below the line through its neighbours' feet"
                      (first (palimpsest::odd-word-details scan (palimpsest::scan-picture-odd scan)))))))
    (false (characters 0.3))
    (false (characters 0))))

(define-test (palimpsest palimpsest-words-set-apart-together)
  ;; two words side by side set lower, and the words ending three lines one under another: each still named,
  ;; their neighbours over or beside them being set apart too
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-grained (pal-lowered (pal-scan) '((4 . 2) (4 . 3))))))
    (declare (ignore pdf accounts))
    (let ((lower (pal-finding findings :look "has 2 words unlike the rest of it, a lower baseline")))
      (true lower "~s" (mapcar #'palimpsest::finding-words findings))))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-grained (pal-lowered (pal-scan) '((3 . 5) (4 . 5) (5 . 5))))))
    (declare (ignore pdf accounts))
    (let ((lower (pal-finding findings :look "has 3 words unlike the rest of it, a lower baseline")))
      (true lower "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; a run of ink across most of a line, lines run together or a rule, is no word: not set apart
  (let ((page (pal-scan)))
    (loop for y from (+ 20 (* 6 38) 4) repeat 18
          do (loop for x from 20 below 580 do (setf (aref page (+ (* y 600) x)) (if (oddp (floor x 4)) 60 140))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained page)))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-strokes-told-by-their-paper)
  ;; a stroke laid with the paper about it over another, a third of its grey levels moved by one, as a 1 or
  ;; a 7 copied and saved again is: named, worth a look; a bar five times as tall as it is wide or more, a
  ;; piece of a rule, so laid: no character, not named
  (flet ((laid (page from-x from-y to-x to-y w h)
           (dotimes (y h page)
             (dotimes (x w)
               (let ((g (aref page (+ (* (+ from-y y) 600) from-x x))))
                 (setf (aref page (+ (* (+ to-y y) 600) to-x x))
                       (if (zerop (mod (+ (* w y) x) 3)) (min 255 (1+ g)) g)))))))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-scanned (laid (pal-grained (pal-scan)) 17 94 342 284 9 22)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "2 characters of the scanned page are nearer one another than prints")
            "~s" (mapcar #'palimpsest::finding-words findings)))
    (let ((page (pal-scan)))
      ;; two bars of a table's rules between the words, 30 rows tall
      (dolist (x '(105 485))
        (loop for y from 100 repeat 30 do (loop for dx below 3 do (setf (aref page (+ (* y 600) x dx)) 60))))
      (setf page (pal-grained page))
      (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (laid page 103 98 483 98 7 34)))
        (declare (ignore pdf accounts))
        (false (pal-finding findings nil "characters of the scanned page") "~s"
               (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-slanted-rules-taken-out)
  ;; a rule across the page under a line, falling a row every 40 pixels as a photographed table's does: taken
  ;; out, the line's words read apart from it; no rule, nothing taken out
  (let ((page (pal-scan)))
    (loop for x from 10 below 590
          for y = (+ 20 (* 2 38) 21 (floor (- x 10) 40))
          do (dotimes (dy 2) (setf (aref page (+ (* (+ y dy) 600) x)) 50)))
    (multiple-value-bind (cleaned taken)
        (palimpsest::without-rules page 600 500 (palimpsest::otsu-level (palimpsest::grey-histogram page)) 235)
      (declare (ignore cleaned))
      (true (>= taken 1000) "~d" taken)))
  (let ((page (pal-scan)))
    (true (zerop (nth-value 1 (palimpsest::without-rules page 600 500
                                                         (palimpsest::otsu-level (palimpsest::grey-histogram page))
                                                         235))))))

(define-test (palimpsest palimpsest-word-unlike-its-line-worth-a-look)
  ;; a word printed apart, sharper, on cleaner paper and set 4 pixels lower, as honest catalogues and forms
  ;; have them: named, both how it looks and where it sits measured, worth a look and never a strong sign
  (let ((page (pal-grained (pal-scan))) (top (+ 20 (* 6 38))) (left (+ 20 (* 2 95))))
    (loop for y from (- top 2) below (+ top 26)
          do (loop for x from (- left 2) below (+ left 75) do (setf (aref page (+ (* y 600) x)) 232)))
    (dotimes (stroke 9)
      (loop for y from (+ top 4) repeat 18
            do (loop for dx from 0 to 2 do (setf (aref page (+ (* y 600) left (* stroke 8) dx)) 60))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))
      (let ((word (pal-finding findings :look "both in how it looks and where it sits")))
        (true word "~s" (mapcar #'palimpsest::finding-words findings))
        (when word (pal-has (palimpsest::finding-words word) "a lower baseline"))))))

(define-test (palimpsest palimpsest-slanted-rules-traced-quickly)
  ;; tracing rules at a slant takes less than half of all the taking out of rules, the straight ones'
  ;; included, on a page of text and rules
  (let* ((grey (let ((page (pal-scan)))
                 (loop for x from 10 below 590
                       for y = (+ 20 (* 2 38) 21 (floor (- x 10) 40))
                       do (dotimes (dy 2) (setf (aref page (+ (* (+ y dy) 600) x)) 50)))
                 page))
         (level (palimpsest::otsu-level (palimpsest::grey-histogram grey))) (out (copy-seq grey)))
    (palimpsest::without-rules grey 600 500 level 235)
    (let* ((t0 (pal-thread-cpu-time))
           (t1 (progn (dotimes (k 20) (palimpsest::without-rules grey 600 500 level 235)) (pal-thread-cpu-time)))
           (t2 (progn (dotimes (k 20)
                        (palimpsest::without-slanted-rules grey out 600 500 level 235 80)) (pal-thread-cpu-time))))
      (true (< (- t2 t1) (* 1/2 (- t1 t0))) "slanted ~,3fs of ~,3fs" (/ (- t2 t1) 1d9) (/ (- t1 t0) 1d9)))))

(define-test (palimpsest palimpsest-region-set-apart-together)
  ;; a block of words smoothed together by a picture editor, their paper cleaner than the rest of their lines
  ;; by less than a word alone would be named for: named as words set apart together, for the record; the
  ;; same page untouched, nothing
  (let ((page (pal-grained (pal-scan))))
    (let ((copy (copy-seq page)))
      (loop for y from (+ 20 (* 3 38) -6) below (+ 20 (* 7 38) -6)
            do (loop for x from (+ 20 95 -6) below (+ 20 (* 4 95) -6)
                     do (setf (aref page (+ (* y 600) x))
                              (round (loop for dy from -1 to 1
                                           sum (loop for dx from -1 to 1
                                                     sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "unlike the rest of it"))
      (true (pal-finding findings :record "set apart together from the lines above and below") "~s"
            (mapcar #'palimpsest::finding-words findings))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-scan))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-letters-on-a-slanted-rule-whole)
  ;; a rule falling a row every 40 pixels through the middle of a line's strokes, as a photographed form's
  ;; underline runs through its letters: the rule taken out between the strokes, the strokes left whole
  (let ((page (pal-scan)))
    (loop for x from 10 below 590
          for y = (+ 20 (* 2 38) 6 (floor (- x 10) 40))
          do (dotimes (dy 2) (setf (aref page (+ (* (+ y dy) 600) x)) 50)))
    (let* ((level (palimpsest::otsu-level (palimpsest::grey-histogram page)))
           (out (copy-seq page)))
      (true (plusp (palimpsest::without-slanted-rules page out 600 500 level 235 80)))
      ;; every pixel of every stroke of that line still ink
      (true (loop for word below 6
                  always (loop for stroke below 9
                               for x0 = (+ 20 (* word 95) (* stroke 8))
                               always (loop for y from (+ 20 (* 2 38)) repeat 18
                                            always (loop for dx below 3
                                                         always (< (aref out (+ (* y 600) x0 dx)) level)))))
            "strokes cut"))))

(defun pal-columns (&key (lowered 0))
  "The grey octets of a scanned page, 600 by 500 pixels, of PAL-SCAN's
strokes: four lines of six words, then a table of eight lines of three
words, at the left, the middle and the right, the right one set LOWERED
pixels lower in every line, as a column of figures laid in again; grain
over all of it (PAL-GRAINED)."
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 230)))
    (dotimes (line 12)
      (dolist (word (if (< line 4) '(0 1 2 3 4 5) '(0 2 5)))
        (let ((top (+ 20 (* line 38) (if (and (>= line 4) (= word 5)) lowered 0))) (left (+ 20 (* word 95))))
          (dotimes (stroke 9)
            (let ((x0 (+ left (* stroke 8))))
              (loop for y from top repeat 18
                    do (loop for dx from -1 to 3
                             do (setf (aref grey (+ (* y 600) x0 dx)) (if (<= 0 dx 2) 60 140)))))))))
    (pal-grained grey)))

(define-test (palimpsest palimpsest-column-set-off-together)
  ;; a table's right column set 3 pixels lower in every line, each figure stepping off its line as the
  ;; others in its column do: named as a column, worth a look; the table untouched, nothing
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-columns :lowered 3)))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "words unlike the rest of it, a lower baseline") "~s"
          (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-columns)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-band-set-apart-together)
  ;; two whole lines darkened by 50 grey levels, paper and all, as a band printed again: named as words set
  ;; apart together, worth a look, each like its line and neither line like the lines above and below
  (let ((page (pal-grained (pal-scan))))
    (loop for y from (+ 20 (* 5 38) -6) below (+ 20 (* 7 38) -6)
          do (loop for x below 600 do (setf (aref page (+ (* y 600) x)) (max 0 (- (aref page (+ (* y 600) x)) 50)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "unlike the rest of it"))
      (let ((band (pal-finding findings :record "set apart together from the lines above and below")))
        (true band "~s" (mapcar #'palimpsest::finding-words findings))
        (when band (pal-has (palimpsest::finding-words band) "darker ink"))))))

(define-test (palimpsest palimpsest-shadow-on-a-curled-page-no-region)
  ;; the soft shadow of a hand across a page photographed, its light falling across it too: up to 50 darker,
  ;; paper and all, fading over two lines from its middle: no block set apart, as a photograph's light makes
  ;; such shadows; a band printed again, with edges of its own, is named (PALIMPSEST-BAND-SET-APART-TOGETHER)
  (let ((page (pal-bowed :bow 12 :light 20)))
    (dotimes (y 500)
      (dotimes (x 600)
        (let ((dark (* 50 (exp (- (+ (expt (/ (- y 250) 76) 2) (expt (/ (- x 200) 300) 2)))))))
          (setf (aref page (+ (* y 600) x)) (max 0 (round (- (aref page (+ (* y 600) x)) dark)))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-characters-not-reshaped-by-slanted-rules)
  ;; rules falling a row every 40 pixels between a page's lines, as a photographed table's do: its
  ;; characters cut as they would be with no rule taken out at a slant, the same boxes
  (let ((page (pal-scan)))
    (dolist (line '(2 5 8))
      (loop for x from 10 below 590
            for y = (+ 20 (* line 38) 24 (floor (- x 10) 120))
            do (dotimes (dy 2) (setf (aref page (+ (* (+ y dy) 600) x)) 50))))
    (setf page (pal-grained page))
    (flet ((boxes ()
             (sort (mapcar (lambda (g)
                             (list (palimpsest::glyph-x0 g) (palimpsest::glyph-y0 g)
                                   (palimpsest::glyph-x1 g) (palimpsest::glyph-y1 g)))
                           (palimpsest::picture-reading-glyphs (palimpsest::picture-words page 600 500)))
                   (lambda (a b)
                     (or (< (second a) (second b)) (and (= (second a) (second b)) (< (first a) (first b))))))))
      (let ((with (boxes))
            (without (let ((traced (fdefinition 'palimpsest::without-slanted-rules)))
                       (unwind-protect
                            (progn (setf (fdefinition 'palimpsest::without-slanted-rules)
                                         (lambda (&rest args) (declare (ignore args)) 0))
                                   (boxes))
                         (setf (fdefinition 'palimpsest::without-slanted-rules) traced)))))
        (is equal without with)))))

(define-test (palimpsest palimpsest-value-beside-its-label)
  ;; a form of labels and values below six lines of text on a flat page, one value set 4 pixels lower than
  ;; its label: the two named,
  ;; set beside each other alone, one of them moved; the form untouched, nothing
  (flet ((form (lowered)
           (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 230)))
             (dotimes (line 12)
               (dolist (word (if (< line 6) '(0 1 2 3 4 5) '(0 3)))
                 (let ((top (+ 20 (* line 38) (if (and (= line 8) (= word 3)) lowered 0))) (left (+ 20 (* word 95))))
                   (dotimes (stroke 9)
                     (let ((x0 (+ left (* stroke 8))))
                       (loop for y from top repeat 18
                             do (loop for dx from -1 to 3
                                      do (setf (aref grey (+ (* y 600) x0 dx)) (if (<= 0 dx 2) 60 140)))))))))
             (pal-grained grey))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (form 4)))
      (declare (ignore pdf accounts))
      (let ((pair (pal-finding findings :look "has 2 words unlike the rest of it")))
        (true pair "~s" (mapcar #'palimpsest::finding-words findings))
        (when pair
          (pal-has (palimpsest::finding-words pair) "a lower baseline")
          (pal-has (palimpsest::finding-words pair) "a higher baseline")
          (let ((value (find-if (lambda (d) (search "pixels 304," d)) (palimpsest::finding-details pair))))
            (true value "~s" (palimpsest::finding-details pair))
            (when value (pal-has value "one of them was moved"))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (form 0)))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-columns-moved-together)
  ;; a table's two right columns set 4 pixels lower together, as figures laid in again side by side: named,
  ;; the one not hiding the other
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 230)))
    (dotimes (line 12)
      (dolist (word (if (< line 4) '(0 1 2 3 4 5) '(0 1 3 5)))
        (let ((top (+ 20 (* line 38) (if (and (>= line 4) (member word '(3 5))) 4 0))) (left (+ 20 (* word 95))))
          (dotimes (stroke 9)
            (let ((x0 (+ left (* stroke 8))))
              (loop for y from top repeat 18
                    do (loop for dx from -1 to 3
                             do (setf (aref grey (+ (* y 600) x0 dx)) (if (<= 0 dx 2) 60 140)))))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained grey)))
      (declare (ignore pdf accounts))
      (let ((lower (pal-finding findings :look "words unlike the rest of it, a lower baseline")))
        (true lower "~s" (mapcar #'palimpsest::finding-words findings))
        (when lower
          (true (>= (length (palimpsest::finding-details lower)) 8) "~d"
                (length (palimpsest::finding-details lower))))))))

(define-test (palimpsest palimpsest-band-on-a-bowed-page)
  ;; two whole lines darkened by 50 on a page bowed by 3 pixels, its light even, as a flatbed's: named
  (let ((page (pal-bowed :bow 3)))
    (loop for y from (+ 20 (* 5 38) -6) below (+ 20 (* 7 38) -6)
          do (loop for x below 600 do (setf (aref page (+ (* y 600) x)) (max 0 (- (aref page (+ (* y 600) x)) 50)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((band (pal-finding findings :record "set apart together from the lines above and below")))
        (true band "~s" (mapcar #'palimpsest::finding-words findings))
        (when band (pal-has (palimpsest::finding-words band) "darker ink"))))))

(define-test (palimpsest palimpsest-unjudged-details-said-once)
  ;; one line read of lines run together by a hatching, below them a table's rules: both reasons said, how
  ;; many characters were set beside others said once
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (let ((page (pal-few-lines (pal-joined) 1)))
                               (loop for y from 60 below 340
                                     do (loop for x from 15 below 580 when (zerop (mod (+ x y) 3))
                                              do (setf (aref page (+ (* y 600) x)) 60)))
                               (loop for y from 350 below 490 by 10
                                     do (loop for x from 10 below 590
                                              do (setf (aref page (+ (* y 600) x)) 50
                                                       (aref page (+ (* (1+ y) 600) x)) 50)))
                               page)))
    (declare (ignore pdf accounts))
    (let* ((records (remove-if-not (lambda (f) (search "not checked" (palimpsest::finding-words f))) findings))
           (details (loop for f in records append (palimpsest::finding-details f))))
      (true (cdr records) "~s" (mapcar #'palimpsest::finding-words findings))
      (true details "~s" (mapcar #'palimpsest::finding-words findings))
      (is = (length details) (length (remove-duplicates details :test #'string=)) "~s" details))))

(defun pal-lighter-block (&key (ink 0) (paper 0))
  "A scanned page of PAL-SCAN's strokes with grain over it (PAL-GRAINED),
the second to fourth words of its fifth to eighth lines printed INK grey
levels lighter and their paper PAPER grey levels lighter."
  (let ((page (pal-scan)))
    (loop for line from 4 to 7
          do (loop for y from (+ 20 (* line 38) -4) repeat 26
                   do (loop for x from (+ 20 95 -4) below (+ 20 (* 4 95) -4)
                            for i = (+ (* y 600) x)
                            do (incf (aref page i) (if (< (aref page i) 200) ink paper)))))
    (pal-grained page)))

(define-test (palimpsest palimpsest-region-lighter-than-a-word-needs)
  ;; a block of four lines printed 20 grey levels lighter, less than a word alone is named for, more than
  ;; half, its paper 10 lighter too, as a block pasted or printed again brings its paper: one finding, the
  ;; block's extent, worth a look; the block's ink alone lighter, its paper the page's, as a second pass of
  ;; the press prints it: nothing
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-lighter-block :ink 20 :paper 10)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "unlike"))
    (let ((block (pal-finding findings :record "set apart together from the lines above and below")))
      (true block "~s" (mapcar #'palimpsest::finding-words findings))
      (when block
        (pal-has (palimpsest::finding-words block) "lighter ink")
        (is = 1 (length (palimpsest::finding-details block))))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-lighter-block :ink 20)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-part-of-a-line-set-apart)
  ;; four of a line's six words darkened by 50 grey levels, paper and all, the line's two other words like
  ;; the lines above and below: named as words set apart together, for the record
  (let ((page (pal-grained (pal-scan))))
    (loop for y from (+ 20 (* 6 38) -6) below (+ 20 (* 6 38) 26)
          do (loop for x from (+ 20 95 -6) below (+ 20 (* 5 95) -6)
                   do (setf (aref page (+ (* y 600) x)) (max 0 (- (aref page (+ (* y 600) x)) 50)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "unlike the rest of it"))
      (let ((block (pal-finding findings :record "set apart together from the lines above and below")))
        (true block "~s" (mapcar #'palimpsest::finding-words findings))
        (when block (pal-has (palimpsest::finding-words block) "darker ink"))))))

(defun pal-bowed-table (&key (lowered 0) (bow 3) curl (columns '(0 2 5)) (moved '(5)))
  "Words as a page 2,000 pixels wide lying in a scanner bowed by BOW pixels reads them (WORD-BOXes): six
lines of six words 200 pixels wide, then a table of eight lines of the six places' COLUMNS (the left, the
middle and the right), those of them MOVED (the right) set LOWERED pixels lower in every line. Every line
is arched BOW pixels down at its middle, or with CURL each by its own part of BOW, as a page photographed
curls, each word straying from it by a tenth of a pixel at most, as a scan's do."
  (let ((words '()) (seed 5))
    (dotimes (line 14 (nreverse words))
      (dolist (k (if (< line 6) '(0 1 2 3 4 5) columns))
        (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
        (let* ((x0 (+ 20 (* k 330))) (centre (+ x0 100)) (top (+ 20 (* line 60)))
               (stray (/ (- (mod (ash seed -16) 21) 10) 100d0)))
          (push (palimpsest::make-word-box
                 :x0 x0 :x1 (+ x0 200) :y0 top :y1 (+ top 30)
                 :baseline (+ top 25 stray (if (and (>= line 6) (member k moved)) lowered 0)
                              (* (if curl (/ (1+ (mod (* line 7) 11)) 11) 1) bow
                                 (- 1 (expt (/ (- centre 1000) 1000d0) 2))))
                 :slope 0d0 :ink (+ 60 (* 10 stray)) :edges (+ 0.4d0 (/ stray 10)) :paper (+ 235 (* 10 stray))
                 :noise (+ 5 (* 10 stray)))
                words))))))

(define-test (palimpsest palimpsest-column-on-a-bowed-page)
  ;; a table's right column set 3 pixels lower in every line on a page lying bowed by 3 pixels in a scanner,
  ;; the bow all its lines share taken out: set off its lines; the bowed table untouched, nothing
  (let ((found (palimpsest::unlike-words (pal-bowed-table :lowered 3))))
    (is = 8 (count-if (lambda (entry) (and (>= (palimpsest::word-box-x0 (first entry)) 1600)
                                            (>= (palimpsest::word-box-y0 (first entry)) 380)
                                            (assoc :offset (rest entry))))
                      found)
        "~s"
        (mapcar (lambda (entry)
                  (list (palimpsest::word-box-x0 (first entry)) (palimpsest::word-box-y0 (first entry)))) found)))
  (false (palimpsest::unlike-words (pal-bowed-table))))

(define-test (palimpsest palimpsest-turn-against-page-and-line)
  ;; a level word under and over lines of a form whose label and value stand at different heights, so that
  ;; they fall steeply across its columns: not turned, as the page is not; a word turned against both the
  ;; page and its lines: turned
  (let ((steep (list (vector (cons 90d0 50d0) (cons 210d0 62d0)) (vector (cons 90d0 150d0) (cons 210d0 162d0))))
        (level (list (vector (cons 90d0 50d0) (cons 210d0 50d0)) (vector (cons 90d0 150d0) (cons 210d0 150d0)))))
    (is = 0 (palimpsest::turn-residual (palimpsest::make-word-box :x0 100 :x1 200 :slope 0d0) steep 0))
    (true (< 5.5 (palimpsest::turn-residual (palimpsest::make-word-box :x0 100 :x1 200 :slope 0.1d0) level 0) 6))
    (true (< 5.5 (palimpsest::turn-residual (palimpsest::make-word-box :x0 100 :x1 200 :slope 0.1d0) nil 0) 6))))

(define-test (palimpsest palimpsest-bow-not-a-tilt)
  ;; bends of 3 and of 10 pixels at the middle of 2,000 are bows a scanner's page takes; one rising 11 pixels
  ;; from end to end and hardly bowed is the page's tilt misread, as a photograph's
  (true (palimpsest::bow-p (lambda (x) (* 3 (- 1 (expt (/ (- x 1000) 1000) 2)))) 0 2000))
  (true (palimpsest::bow-p (lambda (x) (* 10 (- 1 (expt (/ (- x 1000) 1000) 2)))) 0 2000))
  (false (palimpsest::bow-p (lambda (x) (+ (* 11/2000 x) (* 1 (- 1 (expt (/ (- x 1000) 1000) 2))))) 0 2000)))

(define-test (palimpsest palimpsest-column-on-a-deeply-bowed-page)
  ;; a table's right column set 3 pixels lower on a page bowed by 10 pixels, every line alike: set off its
  ;; lines; the same page untouched, nothing; lines each curled their own way, as a photograph's, no column
  (let ((found (palimpsest::unlike-words (pal-bowed-table :lowered 3 :bow 10))))
    (is = 8 (count-if (lambda (entry) (and (>= (palimpsest::word-box-x0 (first entry)) 1600)
                                            (>= (palimpsest::word-box-y0 (first entry)) 380)
                                            (assoc :offset (rest entry))))
                      found)
        "~s"
        (mapcar (lambda (entry)
                  (list (palimpsest::word-box-x0 (first entry)) (palimpsest::word-box-y0 (first entry)))) found)))
  (false (palimpsest::unlike-words (pal-bowed-table :bow 10)))
  (let ((lines (palimpsest::scan-lines (pal-bowed-table :bow 10 :curl t))))
    (false (palimpsest::flat-page-p lines 0
                                    (palimpsest::page-trend lines #'palimpsest::word-box-baseline (constantly t))))))

(define-test (palimpsest palimpsest-dashes-are-characters)
  ;; on a page of characters 60 pixels tall and 30 wide: a dash 40 by 6 and a hyphen are characters, set beside
  ;; others; a bar 80 by 6 is a piece of a rule; on a page of characters 50 tall, marks 17 by 71 and 20 by 62,
  ;; taller by a tenth and narrow, are pieces of rules, an l 12 by 52 a character
  (flet ((mark (w h) (palimpsest::make-glyph :x0 100 :y0 100 :x1 (+ 100 w) :y1 (+ 100 h))))
    (false (palimpsest::rule-piece-p (mark 40 6) 60 30))
    (false (palimpsest::rule-piece-p (mark 18 5) 60 30))
    (true (palimpsest::rule-piece-p (mark 80 6) 60 30))
    (true (palimpsest::rule-piece-p (mark 17 71) 50 30))
    (true (palimpsest::rule-piece-p (mark 20 62) 50 30))
    (false (palimpsest::rule-piece-p (mark 20 54) 50 30))
    (false (palimpsest::rule-piece-p (mark 12 52) 50 30)))
  ;; a hyphen 11 pixels wide and 4 tall within a word, in place of two of its strokes, is among the page's
  ;; characters
  (let* ((page (pal-scan)) (top (+ 20 (* 3 38))) (left (+ 20 95 32)))
    (loop for y from (- top 1) below (+ top 19)
          do (loop for x from (1- left) below (+ left 12) do (setf (aref page (+ (* y 600) x)) 235)))
    (loop for y from (+ top 8) repeat 4 do (loop for x from left repeat 11 do (setf (aref page (+ (* y 600) x)) 60)))
    (let ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-grained page) 600 500))))
      (true (find-if (lambda (g)
                       (and (<= (abs (- (palimpsest::glyph-x0 g) left)) 2)
                            (<= (- (palimpsest::glyph-y1 g) (palimpsest::glyph-y0 g)) 6)))
                     glyphs)
            "~s" (loop for g in glyphs when (< 120 (palimpsest::glyph-y0 g) 160)
                       collect (list (palimpsest::glyph-x0 g) (palimpsest::glyph-y0 g)
                                     (palimpsest::glyph-x1 g) (palimpsest::glyph-y1 g)))))))

(defun pal-lit-across (grey &optional (darkest 215))
  "GREY, a page 600 pixels wide (PAL-SCAN), lit unevenly across it, as a
lamp at its side lights it: each pixel times a light falling from 1 at its
right edge to DARKEST/255 at its left. GREY."
  (dotimes (i (length grey) grey)
    (let ((x (mod i 600)))
      (setf (aref grey i) (round (* (aref grey i) (+ darkest (* (- 255 darkest) (/ x 599)))) 255)))))

(define-test (palimpsest palimpsest-band-under-uneven-light)
  ;; a page lit 16 % darker at its left than at its right, two lines darkened by 50, paper and all: the
  ;; light's smooth fall taken out, the band named; the same page untouched, nothing
  (let ((page (pal-grained (pal-scan))))
    (loop for y from (+ 20 (* 5 38) -6) below (+ 20 (* 7 38) -6)
          do (loop for x below 600 do (setf (aref page (+ (* y 600) x)) (max 0 (- (aref page (+ (* y 600) x)) 50)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-lit-across page)))
      (declare (ignore pdf accounts))
      (let ((band (pal-finding findings :record "set apart together from the lines above and below")))
        (true band "~s" (mapcar #'palimpsest::finding-words findings))
        (when band (pal-has (palimpsest::finding-words band) "darker ink")))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-lit-across (pal-grained (pal-scan)))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-columns-moved-either-way)
  ;; a table of four columns, its two right ones set 4 pixels lower, which sets the two left ones off the
  ;; other way beside their lines' middle: all four named, as one or the other pair moved
  (let ((found (palimpsest::unlike-words (pal-bowed-table :columns '(0 2 4 5) :moved '(4 5) :lowered 4 :bow 0))))
    (flet ((named (x0)
             (count-if (lambda (entry) (and (= (palimpsest::word-box-x0 (first entry)) x0)
                                            (>= (palimpsest::word-box-y0 (first entry)) 380)
                                            (assoc :offset (rest entry))))
                       found)))
      (is equal '(8 8 8 8) (mapcar #'named '(20 680 1340 1670))
          "~s"
          (mapcar (lambda (e) (list (palimpsest::word-box-x0 (first e)) (palimpsest::word-box-y0 (first e)))) found)))))

(define-test (palimpsest palimpsest-moved-column-keeps-the-page-flat)
  ;; a flat page's table of five columns, its last two set 3 pixels lower in every row: its rows still count
  ;; as straight, the page as flat, and the columns are named
  (let* ((words (pal-bowed-table :columns '(0 1 2 4 5) :moved '(4 5) :lowered 3 :bow 0))
         (lines (palimpsest::scan-lines words)))
    (true (palimpsest::flat-page-p lines 0))
    (let ((found (palimpsest::unlike-words words)))
      (is = 16 (count-if (lambda (entry) (and (>= (palimpsest::word-box-x0 (first entry)) 1340)
                                               (>= (palimpsest::word-box-y0 (first entry)) 380)
                                               (assoc :offset (rest entry))))
                         found)
          "~s"
          (mapcar (lambda (e) (list (palimpsest::word-box-x0 (first e)) (palimpsest::word-box-y0 (first e)))) found)))))

(define-test (palimpsest palimpsest-column-offset-not-its-row)
  ;; a table whose left column runs on five lines below its right one, each of those lines holding a word of
  ;; no column beside it: their lines cannot be told from the column, and none is set off by its own row
  (let ((words '()))
    (flet ((word (x top)
             (push (palimpsest::make-word-box :x0 x :x1 (+ x 200) :y0 top :y1 (+ top 30) :baseline (+ top 25)
                                              :slope 0d0)
                   words)))
      (dotimes (row 10)
        (let ((top (+ 380 (* row 60))))
          (word 20 top)
          (word (if (< row 5) 680 (+ 1000 (* (- row 5) 300))) top))))
    (is equal '() (palimpsest::columns-unlike (palimpsest::scan-lines (reverse words))
                                              #'palimpsest::word-box-baseline (constantly t) 5 1.5 (constantly 0)))))

(define-test (palimpsest palimpsest-slanted-rules-taken-out-in-place)
  ;; a page's words are found with the rules at a slant taken out of the page's own picture: reading a page
  ;; of 300,000 pixels makes no second picture of them, under nine octets for each pixel in all
  (let ((page (pal-grained (pal-scan))))
    (palimpsest::picture-words page 600 500)
    (let* ((before (sb-ext:get-bytes-consed))
           (made (progn (palimpsest::picture-words page 600 500) (- (sb-ext:get-bytes-consed) before))))
      (true (< made (* 9 600 500)) "~:d octets" made))))

(defun pal-small-letters ()
  "The grey octets of a scanned page, 600 by 500 pixels, of PAL-SCAN's lines and words, every stroke 12
pixels tall standing on its line but the fourth of each word, 18 tall as an l rising above small letters
is, on paper of grey 235 straying by up to 10, grain over all of it (PAL-GRAINED). It is a page whose
taller letters are fewer than a quarter of its characters."
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8))) (seed 12345))
    (dotimes (i (length grey))
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (+ 225 (mod (ash seed -16) 21))))
    (dotimes (line 12)
      (dotimes (word 6)
        (dotimes (stroke 9)
          (let ((x0 (+ 20 (* word 95) (* stroke 8))) (top (+ 20 (* line 38))))
            (loop for y from (+ top (if (= stroke 3) 0 6)) below (+ top 18)
                  do (loop for dx from -1 to 3
                           do (setf (aref grey (+ (* y 600) x0 dx)) (if (<= 0 dx 2) 60 140))))))))
    (pal-grained grey)))

(define-test (palimpsest palimpsest-tall-letters-few-not-rules)
  ;; a page of small letters, one in nine rising 18 pixels as an l does, 5 wide with its blur: the taller
  ;; letters are characters, not pieces of rules, however few they are; one of them laid with its paper
  ;; over another, a third of its grey levels moved by one, is named
  (let* ((glyphs (palimpsest::picture-reading-glyphs (palimpsest::picture-words (pal-small-letters) 600 500)))
         (tallest (palimpsest::upper-height glyphs)) (typical (palimpsest::typical-width glyphs))
         (tall (remove-if-not (lambda (g) (>= (- (palimpsest::glyph-y1 g) (palimpsest::glyph-y0 g)) 17)) glyphs)))
    (true (>= (length tall) 60) "~d tall of ~d" (length tall) (length glyphs))
    (is = 0 (count-if (lambda (g) (palimpsest::rule-piece-p g tallest typical)) tall) "upper height ~a" tallest))
  (let ((page (pal-small-letters)))
    (dotimes (y 22)
      (dotimes (x 9)
        (let ((g (aref page (+ (* (+ 94 y) 600) 41 x))))
          (setf (aref page (+ (* (+ 284 y) 600) 326 x)) (if (zerop (mod (+ (* 9 y) x) 3)) (min 255 (1+ g)) g)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "2 characters of the scanned page are nearer one another than prints")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-band (factor &key ink-only (first 5) (last 6) thin pasted jpeg)
  "A scanned page of PAL-SCAN's strokes with grain over it (PAL-GRAINED), its lines FIRST to LAST (from 0)
printed again before the grain, as the made-up pages of the handbook's figures are. Every grey of the
rows from the paper above the first to the paper below the last is FACTOR times what it was, or with
INK-ONLY those of the ink alone, the paper the page's. With THIN, the strokes are lighter, cores of grey
130 fading through 180, as thin type scanned at 200 dots an inch is, and grain is of up to 4 levels
either way in place of PAL-GRAINED's 12. With PASTED, PAL-SCAN's word is pasted on its clean paper and
lowered 4 pixels, the third word of the fifth line. With JPEG, the page is saved as a JPEG of that
quality after its grain."
  (let ((page (pal-scan :pasted pasted :lowered pasted)))
    (when thin
      (dotimes (i (length page)) (case (aref page i) (60 (setf (aref page i) 130)) (140 (setf (aref page i) 180)))))
    (loop for y from (+ 20 (* first 38) -6) below (+ 20 (* (1+ last) 38) -6)
          do (loop for x below 600
                   for i = (+ (* y 600) x)
                   unless (and ink-only (>= (aref page i) 200))
                     do (setf (aref page i) (round (* factor (aref page i))))))
    (if thin
        (let ((seed 4321))
          (dotimes (i (length page))
            (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
            (setf (aref page i) (max 0 (min 255 (+ (aref page i) -4 (mod (ash seed -16) 9)))))))
        (pal-grained page))
    (if jpeg (pal-jpeg-saved page 0 0 600 500 :quality jpeg) page)))

(define-test (palimpsest palimpsest-band-named-however-plain)
  ;; two lines printed again, paper and all, from 0.95 of their grey to half of it: each named once, one
  ;; finding over both lines, the plainer as surely as the fainter, the words under it no findings of their
  ;; own; the ink alone printed darker on the page's paper, as a second pass of the press prints it: nothing
  (dolist (factor '(0.95 0.9 0.85 0.8 0.75 0.7 0.6 0.5))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-band factor)))
      (declare (ignore pdf accounts))
      (let ((bands (remove-if-not (lambda (f) (search "set apart together" (palimpsest::finding-words f))) findings)))
        (is = 1 (length bands) "~a: ~s" factor (mapcar #'palimpsest::finding-words findings))
        (is = 1 (count-if (lambda (f) (search "unlike the rest of it" (palimpsest::finding-words f))) findings)
            "~a: ~s" factor (mapcar #'palimpsest::finding-words findings))
        (when bands
          (pal-has (palimpsest::finding-words (first bands)) "over 2 lines")
          (pal-has (palimpsest::finding-words (first bands)) "darker paper")))))
  (dolist (factor '(0.9 0.7 0.5))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-band factor :ink-only t)))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "unlike the rest of it") "~a: ~s" factor
             (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-band-named-thin-or-kept-as-jpeg)
  ;; two lines printed again, their strokes thin and light as type scanned at 200 dots an inch, their ink
  ;; nearer their paper than half of it, or the page kept as a JPEG of quality 90: named once at every
  ;; factor tried
  (flet ((bands (page) (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
                         (declare (ignore pdf accounts))
                         (count-if (lambda (f) (search "set apart together" (palimpsest::finding-words f))) findings))))
    (dolist (factor '(0.95 0.9 0.85 0.8 0.7 0.6 0.5))
      (is = 1 (bands (pal-band factor :thin t)) "thin, ~a" factor))
    (dolist (factor '(0.94 0.85 0.7))
      (is = 1 (bands (pal-band factor :jpeg 90)) "JPEG 90, ~a" factor))))

(define-test (palimpsest palimpsest-band-only-with-paper-both-sides)
  ;; the paper of the lower half of a page darker by 20 grey levels from a gap between lines to its foot, as
  ;; a tinted panel or a shadow brings, and of its upper half from its head to a gap: a step on one side, no
  ;; band, nothing named; a band of two lines in the same place, darker than the paper both above and below
  ;; it, named
  (flet ((stepped (from to)
           (let ((page (pal-scan)))
             (loop for y from from below to do (loop for x below 600 do (decf (aref page (+ (* y 600) x)) 20)))
             (pal-grained page)))
         (bands (page) (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
                         (declare (ignore pdf accounts))
                         (remove-if-not (lambda (f)
                                          (search "set apart together" (palimpsest::finding-words f))) findings))))
    (is = 0 (length (bands (stepped (+ 20 (* 6 38) -6) 500))))
    (is = 0 (length (bands (stepped 0 (+ 20 (* 6 38) -6)))))
    (let ((band (bands (stepped (+ 20 (* 5 38) -6) (+ 20 (* 7 38) -6)))))
      (is = 1 (length band))
      (when band (pal-has (first (palimpsest::finding-details (first band))) "the paper above it")))))

(define-test (palimpsest palimpsest-word-in-a-band-told-apart)
  ;; a word pasted crisp on clean paper and lowered, in a band of two lines printed again, however dark,
  ;; its paper under the page's level of ink from 0.8 on: the band named once, the word named on its own
  ;; with its place
  (dolist (factor '(0.85 0.8 0.7 0.6))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-band factor :first 4 :last 5 :pasted t)))
      (declare (ignore pdf accounts))
      (is = 1 (count-if (lambda (f) (search "set apart together" (palimpsest::finding-words f))) findings)
          "~a: ~s" factor (mapcar #'palimpsest::finding-words findings))
      (let ((word (pal-finding findings :look "has 1 word unlike the rest of it")))
        (true word "~a: ~s" factor (mapcar #'palimpsest::finding-words findings))
        (when word (pal-has (first (palimpsest::finding-details word)) "pixels 210,"))))))

(define-test (palimpsest palimpsest-upper-height-bounded)
  ;; forty characters 10 pixels tall and ten 15 tall, their words' lines reaching 60 pixels above their
  ;; baselines, as lines run together with marks above them do: the taller characters' height taken as 16
  ;; at most, eight fifths of the quarter's 10, not 60
  (let* ((word (palimpsest::make-word-box :x0 0 :y0 40 :x1 400 :y1 110 :baseline 100))
         (glyphs (append (loop repeat 40 collect (palimpsest::make-glyph :x0 0 :y0 90 :x1 6 :y1 100 :word word))
                         (loop repeat 10 collect (palimpsest::make-glyph :x0 0 :y0 85 :x1 4 :y1 100 :word word)))))
    (is = 16 (palimpsest::upper-height glyphs))))

(defun pal-parted-band (factor from count)
  "PAL-BAND's page, its fifth and sixth lines printed again at FACTOR of
their grey but for COUNT rows from row FROM of the fifth line, which keep
the page's paper, as a band's own noise lifts some of its rows back to the
paper about it."
  (let ((page (pal-scan)))
    (loop for y from (+ 20 (* 5 38) -6) below (+ 20 (* 7 38) -6)
          unless (<= from (- y (+ 20 (* 5 38))) (+ from count -1))
            do (loop for x below 600 for i = (+ (* y 600) x) do (setf (aref page i) (round (* factor (aref page i))))))
    (pal-grained page)))

(define-test (palimpsest palimpsest-band-parted-judged-whole)
  ;; a band of two lines whose rows are parted by six of the page's paper, near the foot of its first
  ;; line or between its lines: one band, named once over both lines; seven lines printed again, more than
  ;; a band may span: nothing
  (flet ((bands (page) (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
                         (declare (ignore pdf accounts))
                         (remove-if-not (lambda (f)
                                          (search "set apart together" (palimpsest::finding-words f))) findings))))
    (dolist (from '(18 22))
      (let ((found (bands (pal-parted-band 0.9 from 6))))
        (is = 1 (length found) "parted at ~d" from)
        (when found (pal-has (palimpsest::finding-words (first found)) "over 2 lines"))))
    (is = 0 (length (bands (pal-band 0.9 :first 2 :last 8))))))

(define-test (palimpsest palimpsest-black-bar-no-band)
  ;; twelve lines of words, two of them under a bar of grey 0 from gap to gap, as a redaction drawn across a
  ;; page is, a little lighter where the letters were: no band of paper, and no failing on its paper of 0
  (let ((grey (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 235))
        (words '()))
    (dotimes (line 12)
      (let ((top (+ 20 (* line 38))))
        (dotimes (word 6)
          (push (palimpsest::make-word-box :x0 (+ 20 (* word 95)) :y0 top :x1 (+ 90 (* word 95)) :y1 (+ top 20)
                                           :baseline (+ top 18) :paper 235 :ink 60)
                words))
        (loop for y from (+ top 4) below (+ top 18)
              do (loop for x from 20 below 580 by 6 do (setf (aref grey (+ (* y 600) x)) 60)))))
    (loop for y from (+ 20 (* 5 38) -8) below (+ 20 (* 7 38) -8)
          for letters = (< 2 (mod (- y 20) 38) 20)
          do (loop for x below 600 do (setf (aref grey (+ (* y 600) x)) (if (and letters (zerop (mod x 8))) 100 0))))
    (is eq nil (palimpsest::paper-bands grey 600 500 0 words))))

(define-test (palimpsest palimpsest-copy-in-a-dark-band-found)
  ;; two lines printed again at 0.7 of their grey, their paper under the page's level of ink, and a word of
  ;; the first laid pixel for pixel over a word of the second: the copies found, as they are anywhere else
  (let ((page (pal-laid (pal-band 0.7) 18 208 303 246 78 22)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-faint-grained (grey &optional (seed 555))
  "GREY, a scanned page (PAL-SCAN), with faint grain over all of it, ink
too: each pixel straying by up to 3, so that prints of a stroke differ, but
by less than the 4 grey levels a page's likeness is judged by. GREY."
  (dotimes (i (length grey) grey)
    (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
    (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) -3 (mod (ash seed -16) 7)))))))

(define-test (palimpsest palimpsest-exact-copy-on-faint-grain)
  ;; a page whose prints differ by less than its likeness is judged by, a word of its third line laid pixel
  ;; for pixel over a word of its tenth: the copies named, worth a look, as the steps of a JPEG kept without
  ;; loss can make honest prints there the same, never a strong sign; the page untouched, no copies
  (let ((page (pal-faint-grained (pal-scan))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :record "the scan's grain is too faint to tell prints apart")
            "~s" (mapcar #'palimpsest::finding-words findings)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-laid page 18 94 303 360 78 22)))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "characters of the scanned page") "~s"
             (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :look "the same pixel for pixel on a page whose prints")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-typed-on-grid (spread &key (seed 31))
  "The grey octets of a scanned page, 600 by 500 pixels, of 11 lines of 23 letters of *PAL-TYPE* typed on a
grid, every letter's top left on a multiple of 8 pixels, as a typewriter or a form's fixed pitch sets
them, so that a JPEG's blocks fall alike on every print of a letter. It is blurred by a 3 by 3 mean and
given grain of up to SPREAD grey levels either way, seeded by SEED."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0))
        (letters (map 'vector #'first *pal-type*)) (s seed))
    (dotimes (l 11)
      (dotimes (i 23)
        (let ((rows (rest (assoc (aref letters
                                       (mod (+ (* 7 l) (* 3 i) (floor (* l i) 5)) (length letters))) *pal-type*)))
              (x (+ 16 (* 24 i))) (top (+ 24 (* 40 l))))
          (loop for row in rows for r from 0
                do (loop for dot across row for d from 0
                         when (char= dot #\#)
                           do (dotimes (dy 3)
                                (dotimes (dx 3)
                                  (setf (aref grey (+ (* (+ top (* 3 r) dy) 600) x (* 3 d) dx)) 50d0))))))))
    (let ((copy (copy-seq grey)) (out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
      (loop for y from 1 below 499
            do (loop for x from 1 below 599
                     do (setf (aref grey (+ (* y 600) x))
                              (/ (loop for dy from -1 to 1
                                       sum (loop for dx from -1 to 1 sum (aref copy (+ (* (+ y dy) 600) x dx))))
                                 9))))
      (dotimes (i (length out) out)
        (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
        (setf (aref out i)
              (max 0 (min 255 (round (+ (aref grey i) (- spread) (mod (ash s -16) (1+ (* 2 spread))))))))))))

(define-test (palimpsest palimpsest-exact-copy-on-stepped-page)
  ;; letters typed on a grid with grain, a word of the first line laid pixel for pixel over the eighth: a
  ;; strong sign; the same page saved as a JPEG of quality 30 and kept without loss, or posterised to 16
  ;; grey levels, steps every print's grey levels alike, where honest prints can come out the same: the
  ;; picture told stepped, the copy named worth a look, never a strong sign
  (flet ((made (kind)
           (let ((page (pal-typed-on-grid 8)))
             (case kind
               (:jpeg (pal-jpeg-saved page 0 0 600 500 :quality 30))
               (:posterised (dotimes (i (length page)) (setf (aref page i) (+ 8 (* 16 (floor (aref page i) 16)))))))
             page)))
    (dolist (kind '(:none :jpeg :posterised))
      (let ((page (made kind)))
        (is eq
            (not (eq kind :none))
            (and (palimpsest::stepped-picture-p (palimpsest::picture-reading-glyphs
                                                 (palimpsest::picture-words page 600 500)))
                 t)
            "~a" kind)
        (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-laid page 62 22 62 302 122 26)))
          (declare (ignore pdf accounts))
          (if (eq kind :none)
              (true (pal-finding findings :strong "copies of one another") "~s"
                    (mapcar #'palimpsest::finding-words findings))
              (progn
                (false (pal-finding findings :strong "characters of the scanned page") "~a: ~s" kind
                       (mapcar #'palimpsest::finding-words findings))
                (true (pal-finding findings :look "the same pixel for pixel on a page whose prints")
                      "~a: ~s" kind (mapcar #'palimpsest::finding-words findings)))))))))

(define-test (palimpsest palimpsest-outline-of-many-curves-bounded)
  ;; curves at the farthest coordinates are halved to the end every time: the outline gives way, not memory
  (let* ((far 1d15) (path '()) (curves '()) (points 0))
    (push (cons 0d0 0d0) path)
    (incf points)
    ;; arches a thousand million million points high, from one end of the page's space to the other and back
    (dotimes (k 100)
      (let ((from (if (evenp k) 0d0 far)) (to (if (evenp k) far 0d0)))
        (push points curves)
        (push (cons from far) path) (push (cons to far) path) (push (cons to 0d0) path)
        (incf points 3)))
    (false (palimpsest::outline-of path '(0) curves nil))
    (true (palimpsest::outline-of (list (cons 0d0 10d0) (cons 10d0 0d0) (cons 0d0 0d0)) '(0) '() nil))))

(define-test (palimpsest palimpsest-text-lines-as-page-lines)
  ;; three runs each within half a size of the next but not of the last: one line, as the checks read it
  (let ((marks (vector (palimpsest::make-mark :kind :text :text "a" :x0 0 :x1 10 :y0 100 :y1 108 :size 8)
                       (palimpsest::make-mark :kind :text :text "b" :x0 50 :x1 60 :y0 103 :y1 111 :size 8)
                       (palimpsest::make-mark :kind :text :text "c" :x0 100 :x1 110 :y0 106 :y1 114 :size 8))))
    (is = 1 (length (palimpsest::page-lines-of marks)))
    (is equal '("a b c") (palimpsest::text-lines marks))))

(define-test (palimpsest palimpsest-chat-traces)
  ;; what a chatbot writes about itself, worth a look, quoted with its page and the line shown
  (let* ((findings (nth-value 2 (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Dear Sir,) Tj ET BT /F1 12 Tf 72 680 Td
(As an AI language model, I cannot sign this letter.) Tj ET"))))
         (found (pal-finding findings :look "words a chatbot writes about itself")))
    (true found)
    (pal-has (palimpsest::finding-words found) "Page 1 keeps")
    (pal-has (palimpsest::finding-words found) "\"as an ai language model\"")
    (pal-has (palimpsest::finding-words found) "pasted from a chatbot")
    (is eq :chatbot-text (palimpsest::finding-method found))
    (is eql 1 (palimpsest::finding-page found))
    (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence found)) "As an AI language model"))
  ;; a phrase broken over two lines, and a link's tracking part
  (let ((findings (nth-value 2 (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Thanks! Regenerate) Tj ET BT /F1 12 Tf
72 686 Td (response) Tj ET BT /F1 12 Tf 72 600 Td (See https://example.org/a?utm_source=chatgpt.com) Tj ET")))))
    (true (pal-finding findings :look "the labels of a chatbot's page"))
    (true (pal-finding findings :look "\"utm_source=chatgpt.com\"")))
  ;; a place left to fill alone is said to be a template's as well
  (let ((found (pal-finding (nth-value 2 (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Yours, [Your Name]) Tj ET")))
                            :look "a place left to fill")))
    (true found)
    (pal-has (palimpsest::finding-words found) "\"[your name]\"")
    (pal-has (palimpsest::finding-words found) "office template"))
  ;; honest pages: writing about language models, references, editors' brackets
  (dolist (content '("BT /F1 12 Tf 72 700 Td (A study of AI language models and their use.) Tj ET"
                     "BT /F1 12 Tf 72 700 Td (As shown in [1] and [12], the response [sic] was late.) Tj ET"
                     "BT /F1 12 Tf 72 700 Td (Total: 100 [paid]) Tj ET"))
    (false (pal-finding (nth-value 2 (pal-read (pal-one-page content))) nil "chatbot") "~s" content))
  ;; the search reads curly quotes and Swedish as written
  (is equal '((:self "i'm an ai language model"))
      (palimpsest::chat-traces (palimpsest::chat-normal-text (format nil "I~cm an AI   language model"
                                                                     #\Right_single_quotation_mark))))
  (is equal '((:self "som en ai-språkmodell") (:placeholder "[infoga datum]"))
      (palimpsest::chat-traces (palimpsest::chat-normal-text "Som en AI-språkmodell kan jag inte [Infoga datum]")))
  (false (palimpsest::chat-traces (palimpsest::chat-normal-text "[Insert] [your] ["))))

(defun pal-lines-page (&rest lines)
  "A one-page PDF writing LINES in Helvetica, one under another from the top."
  (pal-one-page (format nil "~{~a~^~%~}"
                        (loop for line in lines for y downfrom 700 by 16
                              collect (format nil "BT /F1 11 Tf 72 ~d Td (~a) Tj ET" y line)))))

(defun pal-recognised-page (line)
  "A one-page PDF that is one scanned picture with LINE over it as invisible text, as text recognition leaves it."
  (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /XObject << /Im1 7 0 R >> >> ~
                           /Contents 4 0 R")
               (format nil "q 612 0 0 792 0 0 cm /Im1 Do Q BT 3 Tr /F1 11 Tf 72 700 Td (~a) Tj ET" line)
               :more (list (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 10 /Height 10 ~
                                                         /ColorSpace /DeviceGray /BitsPerComponent 8")
                                 "x"))))

(define-test (palimpsest palimpsest-check-digits)
  ;; an organisation number and an IBAN whose checks fail: named with their page and line, worth a look
  (let* ((findings (nth-value 2 (pal-read (pal-lines-page "Kea Bygg AB" "Org.nr 556036-0794"
                                                          "IBAN SE45 5000 0000 0583 9825 7467"))))
         (found (pal-finding findings :look "whose check fails")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found) "Page 1 has numbers")
      (pal-has (palimpsest::finding-words found) "the organisation number 556036-0794, whose check digit would be 3")
      (pal-has (palimpsest::finding-words found) "the IBAN SE4550000000058398257467")
      (is eq :figure-unchecked (palimpsest::finding-method found))
      (is eql 1 (palimpsest::finding-page found))
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) "Org.nr 556036-0794")))
  ;; a label ending one line and its number beginning the next
  (true (pal-finding (nth-value 2 (pal-read (pal-lines-page "Bankgiro:" "5555-5552"))) :look "the bankgiro number"))
  ;; honest numbers, and numbers that only look like ones that check
  (let ((findings (nth-value 2 (pal-read (pal-lines-page "Org.nr 556036-0793" "Momsreg.nr SE556036079301"
                                                         "IBAN SE45 5000 0000 0583 9825 7466 BIC ESSESESS"
                                                         "Bankgiro 991-2346" "OCR-nummer 12345678903"
                                                         "Personnummer 811218-9876" "Fakturanummer 5560360794"
                                                         "Telefon 08-123 45 67" "OCR 123456 2024-01-31"
                                                         "Org.nr: 01-556047-3521" "Kundnr 1234567890")))))
    (false (pal-finding findings nil "check fails") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; each check by itself
  (true (tuatara:luhn-p "5560360793"))
  (false (tuatara:luhn-p "5560360794"))
  (is char= #\3 (palimpsest::luhn-check-digit "5560360794"))
  (true (tuatara:iban-check-p "GB82WEST12345698765432"))
  (false (tuatara:iban-check-p "GB82WEST12345698765433"))
  (is equal '((:personal "8112189876" t 0 nil)) (palimpsest::check-numbers-in "Personnummer 811218-9876"))
  ;; a date that cannot be is no personal identity number
  (false (palimpsest::check-numbers-in "Personnummer 811318-9876"))
  ;; a year cited in brackets after OCR, an office's or text recognition's short name, is no reference
  (false (palimpsest::check-numbers-in "breach reporting, e.g., OCR (2023) or NHTSA (2022) crash reporting"))
  ;; a header row over a row of values: the label's number is the one under it, not the row's first
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "Kundnr Fakturanr OCR-nummer" "10234 556677 5566778")))
                      nil "check fails"))
  ;; groups of digits are not read across a gap between columns, nor a number before such a gap and more digits
  (false (pal-finding (nth-value 2 (pal-read (pal-one-page "BT /F1 11 Tf 72 700 Td (OCR 1234) Tj ET
BT /F1 11 Tf 200 700 Td (5678) Tj ET"))) nil "check fails"))
  ;; a scan's text recognition: named for the record only, as a digit may be misread
  (let* ((findings (nth-value 2 (pal-read (pal-recognised-page "Org.nr 556036-0794"))))
         (found (pal-finding findings nil "whose check fails")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (is eq :record (palimpsest::finding-strength found))
      (pal-has (palimpsest::finding-words found) "may have misread or dropped a digit"))))

(define-test (palimpsest palimpsest-totals)
  ;; a net and VAT that do not make the total
  (let* ((findings (nth-value 2 (pal-read (pal-lines-page "Netto 1 000,00 kr" "Moms 25 % 250,00 kr"
                                                          "Att betala 1 350,00 kr"))))
         (found (pal-finding findings :look "states a net of")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found)
               "Page 1 states a net of 1 000,00 and VAT of 250,00, which make 1 250,00, but its total is 1 350,00")
      (is eq :figure-unchecked (palimpsest::finding-method found))))
  (true (pal-finding (nth-value 2 (pal-read (pal-lines-page "Net amount $1,000.00" "VAT 20% $200.00"
                                                            "Amount due $1,300.00")))
                     :look "states a net of"))
  ;; honest sums: whole kronor after rounding, English labels, a credit, and a weight beside them
  (dolist (lines '(("Summa exkl. moms 1 000,30" "Moms 25% 250,08" "Avrundning -0,38" "Att betala 1 250,00")
                   ("Net weight 12,50" "Netto 400,00" "Moms 100,00" "Summa 500:-")
                   ("Subtotal 1,000.00" "VAT 200.00" "Total 1,200.00")
                   ("Netto -800,00" "Moms -200,00" "Att betala -1 000,00")
                   ("Netto 100,00" "Moms 25,00" "Netto 200,00" "Moms 50,00" "Att betala 999,00")
                   ("Subtotal 100,00" "Shipping 5,00" "VAT 21,00" "Total 126,00")
                   ("Netto 100,00" "Fakturaavgift 29,00" "Moms 25,00" "Att betala 154,00")
                   ("Netto 1 000,00" "Moms 250,00" "Betalt i förskott 250,00" "Att betala 1 000,00")))
    (false (pal-finding (nth-value 2 (pal-read (apply #'pal-lines-page lines))) nil "states a net of") "~s" lines))
  ;; what an amount is read as
  (is equal '(125050 "1,250.50" 20)
      (multiple-value-list (palimpsest::amount-at-end "Total incl. VAT EUR 1,250.50")))
  (false (palimpsest::amount-at-end "Moms 25 % 1 000,00 250,00 extra"))
  ;; spaces of other widths keep the thousands together; a figure before across a space is no part of it
  (is eql 1296000 (palimpsest::amount-at-end (format nil "Netto 12~c960,00" #\No-break_space)))
  (is eql 1296000 (palimpsest::amount-at-end (format nil "Netto 12~c960,00" #\Narrow_no-break_space)))
  (false (palimpsest::amount-at-end "Konto 5 12 960,00"))
  (is eql 125000 (palimpsest::amount-at-end "Att betala SEK1 250,00"))
  (is eq :vat (palimpsest::amount-role "moms ingår med"))
  (false (palimpsest::amount-role "net weight"))
  (is eq :vat (palimpsest::amount-role "summa moms 25 %"))
  (is eq :net (palimpsest::amount-role "momsunderlag")))

(defun pal-jbig2-segment (number type referred length)
  "The octets of a JBIG2 segment as a PDF keeps it: its header, numbered NUMBER, of TYPE, referring to the
segments REFERRED (fewer than five), on page 1, and LENGTH octets of data."
  (apply #'pal-octets
         (append (list (ldb (byte 8 24) number) (ldb (byte 8 16) number) (ldb (byte 8 8) number) (ldb (byte 8 0) number)
                       type (ash (length referred) 5))
                 referred
                 (list 1 0 0 0 length)
                 (make-list length :initial-element 0))))

(defun pal-jbig2-page (data &optional globals)
  "A one-page PDF whose page is a JBIG2 picture of DATA, sharing the segments GLOBALS when given."
  (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                              '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                              '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] "
                                                           "/Resources << /XObject << /Im1 7 0 R >> >> /Contents "
                                                           "4 0 R >>"))
                              '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                              '(6 . "<< /Producer (Kea Scan) >>")
                              (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 600 /Height 500 ~
                                                           /ColorSpace /DeviceGray /BitsPerComponent 1 ~
                                                           /Filter /JBIG2Decode~@[ /DecodeParms << /JBIG2Globals ~
                                                           8 0 R >>~]" globals)
                                    data))
                        (and globals (list (list 8 :stream "" globals))))))

(define-test (palimpsest palimpsest-jbig2-symbols-for-the-record)
  ;; segment headers read by their lengths, the referred segments of each
  (let ((data (pal-octets (pal-jbig2-segment 0 48 '() 19) (pal-jbig2-segment 1 0 '() 5)
                          (pal-jbig2-segment 2 6 '(1) 7) (pal-jbig2-segment 3 49 '() 0))))
    (is equal '((0 48 ()) (1 0 ()) (2 6 (1)) (3 49 ())) (palimpsest::jbig2-segments data))
    ;; cut short: what is whole is read, nothing past it
    (is equal '((0 48 ()) (1 0 ())) (palimpsest::jbig2-segments (subseq data 0 (+ 30 11 5 3))))
    ;; a page whose text region draws on a symbol dictionary: for the record
    (let* ((findings (nth-value 2 (pal-read (pal-jbig2-page data))))
           (found (pal-finding findings :record "kept as JBIG2 with symbol dictionaries")))
      (true found "~s" (mapcar #'palimpsest::finding-words findings))
      (when found
        (pal-has (palimpsest::finding-words found) "The scanned picture on page 1 is kept")
        (pal-has (palimpsest::finding-words found) "swapped for one that looks like it")
        (pal-has (palimpsest::finding-rule found) "Xerox")
        (is equal '(7) (palimpsest::finding-objects found)))))
  ;; the symbol dictionary kept apart, among the segments pages share
  (true (pal-finding (nth-value 2 (pal-read (pal-jbig2-page (pal-octets (pal-jbig2-segment 0 48 '() 19)
                                                                        (pal-jbig2-segment 2 6 '(1) 7))
                                                            (pal-jbig2-segment 1 0 '() 5))))
                     :record "with symbol dictionaries"))
  ;; generic regions only, and a text region its coder says is lossless: not named
  (dolist (data (list (pal-octets (pal-jbig2-segment 0 48 '() 19) (pal-jbig2-segment 1 38 '() 9))
                      (pal-octets (pal-jbig2-segment 0 48 '() 19) (pal-jbig2-segment 1 0 '() 5)
                                  (pal-jbig2-segment 2 7 '(1) 7))))
    (false (pal-finding (nth-value 2 (pal-read (pal-jbig2-page data))) nil "symbol dictionaries")))
  ;; an encrypted file's streams are not decrypted, so their segments are not read
  (let* ((data (pal-octets (pal-jbig2-segment 1 0 '() 5) (pal-jbig2-segment 2 6 '(1) 7)))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                     '(3 . #.(concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox "
                                                                  "[0 0 600 500] /Resources << /XObject << /Im1 7 0 R "
                                                                  ">> >> /Contents 4 0 R >>"))
                                     '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                     '(6 . "<< /Producer (Kea Scan) >>")
                                     (list 7 :stream #.(concatenate 'string "/Type /XObject /Subtype /Image /Width 600 "
                                                                            "/Height 500 /ColorSpace /DeviceGray "
                                                                            "/BitsPerComponent 1 /Filter /JBIG2Decode")
                                           data)
                                     '(9 . "<< /Filter /Standard /V 1 /R 2 /O <00> /U <00> /P -4 >>"))
                           :trailer "/Root 1 0 R /Info 6 0 R /Encrypt 9 0 R")))
    (false (pal-finding (nth-value 2 (pal-read octets)) nil "symbol dictionaries"))))

(define-test (palimpsest palimpsest-fingerprints-grouped-once)
  ;; prints of one case are set beside each other only until they are one group
  (let ((calls 0)
        (prints (loop for k below 40
                      collect (list (format nil "/cases/~d.txt" k) (format nil "~d" k)
                                    '((:editor "Some editor") (:jpeg "75") (:method :pasted-on-scan))))))
    (sb-int:encapsulate 'palimpsest::cases-alike-p 'palimpsest-count-pairs
                        (lambda (alike &rest arguments) (incf calls) (apply alike arguments)))
    (unwind-protect (is = 1 (length (palimpsest::group-fingerprints prints)))
      (sb-int:unencapsulate 'palimpsest::cases-alike-p 'palimpsest-count-pairs))
    (true (< calls 100) "~d pairs compared for 40 prints of one case" calls)))

(define-test (palimpsest palimpsest-empty-page-node-no-page)
  ;; a node of the page tree with no kids holds no page, and is not taken as one
  (let ((pdf (palimpsest::read-pdf
              (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                  '(2 . "<< /Type /Pages /Kids [3 0 R 7 0 R] /Count 1 >>")
                                  '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R >>")
                                  (list 4 :stream "" "")
                                  '(6 . "<< /Producer (Kea Writer) >>")
                                  '(7 . "<< /Type /Pages /Parent 2 0 R /Kids [] /Count 0 >>"))))))
    (is = 1 (length (palimpsest::pages-of pdf (palimpsest::last-revision pdf))))))

(define-test (palimpsest palimpsest-standard-encoding-read)
  ;; a simple font with no encoding of its own reads its codes by the standard encoding, not as Latin-1
  (let ((table (palimpsest::base-encoding nil)))
    (is eql (code-char #x2019) (aref table 39))
    (is eql (code-char #x2018) (aref table 96))
    (is eql (code-char #xFB01) (aref table 174))
    (is eql #\A (aref table 65))
    (false (aref table 176))))

(define-test (palimpsest palimpsest-copy-strings-past-latin-1)
  ;; a label past Latin-1 is a question mark, never another letter; a title keeps every character
  (is string= "(?ukasz)" (palimpsest::pdf-literal-text (format nil "~cukasz" (code-char #x141))))
  (is string= "(caf\\351)" (palimpsest::pdf-literal-text (format nil "caf~c" (code-char #xE9))))
  (is string= "<FEFF03A90020D83DDE00>" (palimpsest::pdf-text-string (format nil "~c ~c" (code-char #x3A9)
                                                                             (code-char #x1F600))))
  (is string= "(plain)" (palimpsest::pdf-text-string "plain")))

(define-test (palimpsest palimpsest-object-read-in-a-job)
  ;; showing an object starts a job that reads it and decodes its stream; nothing is read on the asking thread
  (let ((read-here 0) (started nil))
    (pal-reported (pal-edited) "test.pdf"
                  :then (lambda (analysis)
                          (sb-int:encapsulate 'palimpsest::object-text 'palimpsest-object-read-here
                                              (lambda (read &rest arguments) (incf read-here) (apply read arguments)))
                          (sb-int:encapsulate 'palimpsest::start-analysis-job 'palimpsest-object-job-kept
                                              (lambda (start analysis job work name)
                                                (declare (ignore start analysis name))
                                                (setf started (list job work))
                                                job))
                          (unwind-protect
                               (progn
                                 (palimpsest::show-object analysis 1 1)
                                 (is = 0 read-here)
                                 (destructuring-bind (job work) started
                                   (true (typep job 'palimpsest::palimpsest-object-job))
                                   (funcall work job)
                                   (true (search "Object 1 of test.pdf" (palimpsest::palimpsest-object-job-text job)))))
                            (sb-int:unencapsulate 'palimpsest::start-analysis-job 'palimpsest-object-job-kept)
                            (sb-int:unencapsulate 'palimpsest::object-text 'palimpsest-object-read-here))))))

(define-test (palimpsest palimpsest-columns-sharing-a-top-two-lines)
  ;; the first lines of two columns of a page read by columns share a top, and stay two lines
  (let ((lines (palimpsest::scan-lines
                (list (palimpsest::make-word-box :x0 10 :y0 50 :x1 60 :y1 62 :column 0)
                      (palimpsest::make-word-box :x0 70 :y0 50 :x1 120 :y1 62 :column 0)
                      (palimpsest::make-word-box :x0 310 :y0 50 :x1 360 :y1 62 :column 300)
                      (palimpsest::make-word-box :x0 370 :y0 50 :x1 420 :y1 62 :column 300)))))
    (is = 2 (length lines))
    (is equal '(10 310) (map 'list (lambda (line) (palimpsest::word-box-x0 (svref line 0))) lines))))

(define-test (palimpsest palimpsest-places-on-a-turned-page)
  ;; on a page shown turned a quarter clockwise, what is at the top of its own space is shown at the right
  (let ((page (palimpsest::make-page :media '(0 0 612 792) :rotate 90))
        (upright (palimpsest::make-page :media '(0 0 612 792))))
    (is string= "at the top" (palimpsest::place-words upright 306 780))
    (is string= "at the right" (palimpsest::place-words page 306 780))
    (is string= "near the top left" (palimpsest::place-words page 10 10))))

(defun pal-repeated (grey k)
  "GREY (600 by 500) made K times larger about its top left by repeating each pixel K times across and down,
as a picture editor's nearest-pixel scaling does."
  (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
    (dotimes (y 500 out)
      (dotimes (x 600)
        (setf (aref out (+ (* y 600) x)) (aref grey (+ (* (floor y k) 600) (floor x k))))))))

(define-test (palimpsest palimpsest-jpeg-density-read)
  ;; the pixels to the inch a JPEG says: by its JFIF header in inches or centimetres, else by its EXIF; none
  ;; for a JFIF header giving only how its pixels are shaped
  (flet ((jpeg (&rest segments) (coerce (append '(#xff #xd8) (apply #'append segments) '(#xff #xda 0 2))
                                        '(simple-array (unsigned-byte 8) (*))))
         (jfif (unit x y) (list #xff #xe0 0 16 74 70 73 70 0 1 2 unit (ash x -8) (logand x 255) (ash y -8)
                                (logand y 255) 0 0)))
    (let ((exif (append '(#xff #xe1 0 66 69 120 105 102 0 0)
                        ;; a little-endian TIFF header, three entries: the resolution across and down, 600/1 at 50,
                        ;; in inches
                        '(73 73 42 0 8 0 0 0 3 0 26 1 5 0 1 0 0 0 50 0 0 0 27 1 5 0 1 0 0 0 50 0 0 0)
                        '(40 1 3 0 1 0 0 0 2 0 0 0 0 0 0 0 88 2 0 0 1 0 0 0))))
      (is equal '(300 300 "JFIF") (multiple-value-list (palimpsest::jpeg-density (jpeg (jfif 1 300 300)))))
      (is = 300 (round (palimpsest::jpeg-density (jpeg (jfif 2 118 118)))))
      (false (palimpsest::jpeg-density (jpeg (jfif 0 1 1))))
      (false (palimpsest::jpeg-density (jpeg)))
      (is equal '(600 600 "EXIF") (multiple-value-list (palimpsest::jpeg-density (jpeg exif))))
      (is equal '(300 300 "JFIF") (multiple-value-list (palimpsest::jpeg-density (jpeg (jfif 1 300 300) exif))))
      ;; a header cut short is read as far as it goes
      (false (palimpsest::jpeg-density (coerce '(#xff #xd8 #xff #xe0 0 16 74 70 73 70)
                                               '(simple-array (unsigned-byte 8) (*))))))))

(define-test (palimpsest palimpsest-scan-resolution-told)
  ;; a page is of a paper size either way up, within 3 %
  (is string= "A4" (palimpsest::paper-size (palimpsest::make-page :media '(0 0 595 842))))
  (is string= "Letter" (palimpsest::paper-size (palimpsest::make-page :media '(0 0 792 607))))
  (false (palimpsest::paper-size (palimpsest::make-page :media '(0 0 144 120))))
  ;; a page of 600 by 500 pixels drawn 2 by 1 2/3 inches: 300 to the inch, its edges a scan's
  (flet ((said (grey)
           (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey :points '(144 120)))
             (declare (ignore pdf accounts))
             (find-if (lambda (f) (search "A scanned page's pixels to the inch" (palimpsest::finding-rule f)))
                      findings)))
         (detail (finding) (first (palimpsest::finding-details finding))))
    (let ((sharp (said (pal-noisy-scan :sigma 1.5))))
      (true sharp)
      (when sharp
        ;; never "scanned at" flatly: a page made smaller, or made larger from a sharp page, has edges as sharp
        (pal-has (palimpsest::finding-words sharp)
                 "are as sharp as a scan's at about 300 to the inch of the page as drawn")
        (pal-has (palimpsest::finding-words sharp) "made smaller from a finer scan, or made larger")
        (false (search "was scanned at" (palimpsest::finding-words sharp)))
        (pal-has (detail sharp) (concatenate 'string "as sharp as a scan's at about 300 to the inch of the page as "
                                             "drawn: scanned at that, made smaller from a finer scan, or made "
                                             "larger from no less than"))
        (pal-has (detail sharp) "600 by 500 pixels drawn on a page of no paper size (5.1 by 4.2 cm)")
        (pal-has (detail sharp) "300 to the inch")))
    ;; made twice as large with blending: the detail of a lower resolution, made larger or blurred, never more
    ;; than for the record
    (let ((larger (said (pal-resampled (pal-noisy-scan :sigma 3) 2))))
      (true larger)
      (when larger
        (is eq :record (palimpsest::finding-strength larger))
        (pal-has (palimpsest::finding-words larger) "holds the detail of a lower resolution than its pixels")
        (pal-has (detail larger) "its pixels 300 to the inch")
        ;; a scanner whose optics are softer than its pixels has such edges too
        (pal-has (palimpsest::finding-words larger) "a scanner whose optics are softer than its pixels")
        (pal-has (detail larger) "or that of its pixels from a scanner softer than them")))
    ;; made twice as large by repeating pixels
    (let ((repeated (said (pal-repeated (pal-noisy-scan :sigma 1.5) 2))))
      (true repeated)
      (when repeated
        (pal-has (palimpsest::finding-words repeated) "was made 2 times larger by repeating its pixels")
        (pal-has (detail repeated) "from about 150 dpi")))
    ;; black and white: its edges tell nothing
    (let ((bilevel (said (map '(simple-array (unsigned-byte 8) (*)) (lambda (g) (if (< g 150) 0 255))
                              (pal-noisy-scan :sigma 1.5)))))
      (true bilevel)
      (when bilevel
        (pal-has (palimpsest::finding-words bilevel) "cannot be told: black and white")))))

;;; Picture files

(defun pal-be (n count)
  "N as COUNT octets, the most significant first."
  (let ((out (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (i count out) (setf (aref out (- count 1 i)) (ldb (byte 8 (* 8 i)) n)))))

(defun pal-png-chunk (type data)
  "A PNG chunk of TYPE holding DATA, with its length and CRC."
  (let ((body (pal-octets type data)))
    (pal-octets (pal-be (length data) 4) body (pal-be (tuatara.io:crc32 body 0 (length body)) 4))))

(defun pal-png (grey width height &rest chunks)
  "A PNG of the grey picture GREY, WIDTH by HEIGHT pixels, its rows unfiltered and stored without packing,
CHUNKS, each (TYPE DATA), between its header and its pixels."
  (let* ((rows (apply #'pal-octets (loop for y below height
                                         collect (pal-octets 0 (subseq grey (* y width) (* (1+ y) width))))))
         (zlib (pal-zlib-stored rows))
         (adler (let ((a 1) (b 0))
                  (loop for o across rows do (setf a (mod (+ a o) 65521) b (mod (+ b a) 65521)))
                  (+ (* b 65536) a))))
    (replace zlib (pal-be adler 4) :start1 (- (length zlib) 4))
    (apply #'pal-octets
           (pal-octets #x89 "PNG" 13 10 26 10)
           (pal-png-chunk "IHDR" (pal-octets (pal-be width 4) (pal-be height 4) 8 0 0 0 0))
           (append (loop for (type data) in chunks collect (pal-png-chunk type data))
                   (list (pal-png-chunk "IDAT" zlib) (pal-png-chunk "IEND" #()))))))

(defun pal-examined-in (dir octets name)
  "The analysis of the file NAME holding OCTETS, examined as the work shut in examines it (EXAMINE-FILE), in this
process: the file in evidence/ of the test directory DIR, the report's directory report/ beside it."
  (let* ((path (namestring (ensure-directories-exist (merge-pathnames (format nil "evidence/~a" name) dir))))
         (directory (namestring (ensure-directories-exist (merge-pathnames "report/" dir)))))
    (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
    (true (palimpsest::examinable-p path))
    (palimpsest::examine-file path directory)))

(defun pal-examined (octets name)
  "The analysis of the file NAME holding OCTETS (PAL-EXAMINED-IN), in a test directory since gone."
  (with-test-directory (dir)
    (pal-examined-in dir octets name)))

(define-test (palimpsest palimpsest-picture-file-examined)
  ;; a PNG saved by a picture editor, a stroke copied within it: examined as one scanned page, the copy strong
  ;; as on a PDF's scan, named by its pixels alone, the editor worth a look, no revisions
  (let* ((grey (pal-grained (pal-scan)))
         (from-x (+ 20 95 -2)) (from-y (+ 20 38 -2))
         (to-x (+ 20 (* 2 95) (* 3 8) -2)) (to-y (+ 20 (* 4 38) -2)))
    (loop for y below 22
          do (loop for x below 8
                   do (setf (aref grey (+ (* (+ to-y y) 600) to-x x)) (aref grey (+ (* (+ from-y y) 600) from-x x)))))
    (let* ((analysis (pal-examined (pal-png grey 600 500 (list "tEXt" (pal-octets "Software" 0 "GIMP 2.10.38"))
                                            (list "tEXt" (pal-octets "Comment" 0 "Created with GIMP")))
                                   "letter.png"))
           (findings (palimpsest::analysis-findings analysis))
           (copies (pal-finding findings :strong "2 characters of the scanned page are copies of one another"))
           (editor (pal-finding findings :look "names GIMP (a picture editor)"))
           (report (palimpsest::pieces-text (palimpsest::report-pieces analysis))))
      (true copies "~s" (mapcar #'palimpsest::finding-words findings))
      (when copies
        (pal-has (first (palimpsest::finding-details copies)) "pixels 114,58 to 119,76)"))
      (true editor "~s" (mapcar #'palimpsest::finding-words findings))
      (pal-has report "PNG, 600 by 500 pixels, examined as one scanned page")
      (false (search "* Revisions" report))
      (false (search "(object" report) "~a" report)
      (false (search "of picture" report) "~a" report)))
  ;; the same picture untouched, saying nothing of itself: no strong sign, no editor
  (let ((findings (palimpsest::analysis-findings (pal-examined (pal-png (pal-grained (pal-scan)) 600 500)
                                                               "letter.png"))))
    (false (pal-finding findings :strong "") "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings nil "picture editor")))
  ;; a file neither a PDF nor a picture is not examined
  (with-test-directory (dir)
    (let ((path (namestring (merge-pathnames "notes.txt" dir))))
      (alexandria:write-string-into-file "Not a picture" path)
      (false (palimpsest::examinable-p path)))))

(define-test (palimpsest palimpsest-picture-dates-and-programs)
  ;; what a picture says of itself, as the note gives it: a camera's name beside an editor's, dates apart
  (flet ((findings (&rest sections)
           (palimpsest::check-picture-metadata (palimpsest::make-picture-file :kind :jpeg :sections sections))))
    (let ((found (findings (format nil "* EXIF (in its APP1 segment)~%- Make :: Kea Phones~%- Model :: K7~%~
                                        - Software :: GIMP 2.10.38~%- DateTime :: 2026:03:02 14:12:05~%~
                                        - DateTimeOriginal :: 2026:03:01 09:30:00~%"))))
      (true (pal-finding found :record "made by Kea Phones K7; naming GIMP"))
      (true (pal-finding found :look "names GIMP (a picture editor)"))
      (true (pal-finding found :look "after Kea Phones K7 made it"))
      (true (pal-finding found :look "made 2026:03:01 09:30:00 (DateTimeOriginal) and changed 2026:03:02 14:12:05"))
      (false (pal-finding found :strong "")))
    ;; a camera's own: both dates at once, its own software
    (let ((found (findings (format nil "* EXIF (in its APP1 segment)~%- Make :: Kea Phones~%~
                                        - Software :: HDR+ 1.0~%- DateTime :: 2026:03:01 09:30:00~%~
                                        - DateTimeOriginal :: 2026:03:01 09:30:00~%"))))
      (false (pal-finding found :look "")))
    ;; a PNG's time is universal, the EXIF's names no zone: hours apart, not a day, agree
    (let ((found (findings (format nil "* EXIF (in its eXIf chunk)~%- DateTimeOriginal :: 2026:03:01 09:30:00~%")
                           (format nil "* PNG text~%- Last changed (tIME) :: 2026-03-01 20:30:00 UTC~%"))))
      (false (pal-finding found :look "")))
    (let ((found (findings (format nil "* EXIF (in its eXIf chunk)~%- DateTimeOriginal :: 2026:03:01 09:30:00~%")
                           (format nil "* PNG text~%- Last changed (tIME) :: 2026-03-04 20:30:00 UTC~%"))))
      (true (pal-finding found :look "changed 2026-03-04 20:30:00 UTC")))))

(defun pal-jpeg-like (quality &optional first)
  "The grey pixels, 512 by 512, of a picture as a JPEG decoder gives them back, saved at QUALITY with the
standard's tables, after a first save at FIRST when given: each block's frequencies drawn smoothly falling, as
a photograph's or a scan's do, rounded to the steps of each save in turn, the pixels made whole greys between."
  (let ((grey (make-array (* 512 512) :element-type '(unsigned-byte 8)))
        (basis palimpsest::*block-basis*) (seed 4242))
    (labels ((table (q)
               ;; the standard's table scaled for quality Q, by frequency across plus eight times down
               (let ((scale (if (< q 50) (floor 5000 q) (- 200 (* 2 q)))) (out (make-array 64)))
                 (loop for k below 64
                       for base across palimpsest::*jpeg-luminance-table*
                       do (setf (aref out (aref palimpsest::*zigzag* k))
                                (max 1 (min 255 (floor (+ (* base scale) 50) 100)))))
                 out))
             (random-unit ()
               (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
               (/ (+ 0.5d0 (ash seed -8)) 8388608d0))
             (weight (u x) (aref basis (+ (* u 8) x)))
             (pixels (f)
               ;; the block of frequencies F as whole greys
               (let ((out (make-array 64)))
                 (dotimes (y 8 out)
                   (dotimes (x 8)
                     (setf (aref out (+ (* y 8) x))
                           (max 0 (min 255 (round (+ 128 (loop for v below 8
                                                                sum (loop for u below 8
                                                                          sum (* (aref f (+ (* v 8) u))
                                                                                 (weight u x) (weight v y)))))))))))))
             (frequencies (p)
               (let ((out (make-array 64)))
                 (dotimes (v 8 out)
                   (dotimes (u 8)
                     (setf (aref out (+ (* v 8) u))
                           (loop for y below 8
                                 sum (loop for x below 8
                                           sum (* (- (aref p (+ (* y 8) x)) 128) (weight u x) (weight v y)))))))))
             (saved (f q)
               (pixels (map 'vector (lambda (c s) (* s (round c s))) f (table q)))))
      (dotimes (by 64 grey)
        (dotimes (bx 64)
          (let ((f (make-array 64)))
            (dotimes (k 64)
              ;; falling with the frequency, signed, as a Laplace distribution is
              (let ((size (if (zerop k) 0 (/ 40d0 (1+ (floor k 2))))))
                (setf (aref f k) (* size (log (random-unit)) (if (< (random-unit) 0.5d0) -1 1)))))
            (let ((p (saved (if first (frequencies (saved f first)) f) quality)))
              (dotimes (y 8)
                (dotimes (x 8)
                  (setf (aref grey (+ (* (+ (* by 8) y) 512) (* bx 8) x)) (aref p (+ (* y 8) x))))))))))))

(define-test (palimpsest palimpsest-jpeg-saved-twice-measured)
  ;; saved once, the frequencies fall evenly over the steps; saved first coarser, then finer, they do not
  (let ((table (palimpsest::standard-table 85)))
    (is = 85 (palimpsest::standard-table-quality table))
    (false (palimpsest::standard-table-quality (cons 3 (rest table))))
    (let ((once (palimpsest::requantised-measure (pal-jpeg-like 85) 512 512 table))
          (twice (palimpsest::requantised-measure (pal-jpeg-like 85 60) 512 512 table)))
      (true (and once (< once palimpsest::*requantised-look*)) "once ~a" once)
      (true (and twice (>= twice palimpsest::*requantised-look*)) "twice ~a" twice))))

(define-test (palimpsest palimpsest-page-made-one-picture)
  ;; a page drawn by a program as one picture, flat paper, flat strokes, smoothed edges: named plainly
  (let ((drawn (map '(simple-array (unsigned-byte 8) (*))
                    (lambda (g) (cond ((>= g 200) 255) ((<= g 80) 0) (t 128)))
                    (pal-scan))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned drawn))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "the whole page is one picture; text a program would have kept as text")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; a scanner set to black and white leaves no half-tones; a scan has grain: neither is named so
  (dolist (grey (list (map '(simple-array (unsigned-byte 8) (*)) (lambda (g) (if (>= g 200) 255 0)) (pal-scan))
                      (pal-grained (pal-scan))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "the whole page is one picture")))))

(define-test (palimpsest palimpsest-picture-metadata-out-of-memory)
  ;; reading what a picture says of itself running out of memory leaves its pixels examined, said in the report
  (sb-int:encapsulate 'tuatara:octets-metadata 'palimpsest-out-of-memory
                      (lambda (read &rest arguments)
                        (declare (ignore read arguments))
                        (error (make-condition 'storage-condition))))
  (unwind-protect
       (let ((file (palimpsest::read-picture-file (pal-png (pal-scan) 600 500) :png)))
         (is = 600 (palimpsest::picture-file-width file))
         (pal-has (format nil "~{~a~}" (palimpsest::picture-file-sections file)) "* Not read"))
    (sb-int:unencapsulate 'tuatara:octets-metadata 'palimpsest-out-of-memory)))

(define-test (palimpsest palimpsest-picture-fields-listed-within-bounds)
  ;; a picture naming its program in thousands of fields: the finding names a few and counts the rest
  (let* ((section (with-output-to-string (s)
                    (format s "* PNG text~%")
                    (dotimes (k 5000) (format s "- Software :: GIMP 2.10.~d~%" k))))
         (found (palimpsest::check-picture-metadata
                 (palimpsest::make-picture-file :kind :png :sections (list section))))
         (editor (pal-finding found :look "names GIMP")))
    (true editor)
    (when editor
      (pal-has (palimpsest::finding-words editor) "and 4,990 more")
      (true (< (length (palimpsest::finding-words editor)) 1000))
      (true (<= (length (palimpsest::finding-details editor)) 201)))))

(define-test (palimpsest palimpsest-picture-fields-not-planted)
  ;; a PNG text whose keyword holds :: and a date does not become a date field of its own
  (let* ((png (pal-png (pal-scan) 600 500
                       (list "tEXt" (pal-octets "DateTimeOriginal :: 2026:03:01 09:30:00" 0 "x"))
                       (list "tEXt" (pal-octets "DateTime :: 2026:03:09 09:30:00" 0 "x"))))
         (found (palimpsest::check-picture-metadata
                 (palimpsest::make-picture-file :kind :png :sections (tuatara:octets-metadata png :png)))))
    (false (pal-finding found :look "says it was made") "~s" (mapcar #'palimpsest::finding-words found))))

;;; Content credentials and image generators' marks

(defun pal-cbor (item)
  "ITEM as CBOR: an integer, a string as text (Latin-1), an octet vector as a byte string, :NULL as null, a list
\(:MAP KEY VALUE ...) as a map, (:TAG N ITEM) as a tagged item, any other list or NIL as an array."
  (labels ((head (major n)
             (cond ((< n 24) (pal-octets (+ (* 32 major) n)))
                   ((< n 256) (pal-octets (+ (* 32 major) 24) n))
                   ((< n 65536) (pal-octets (+ (* 32 major) 25) (pal-be n 2)))
                   (t (pal-octets (+ (* 32 major) 26) (pal-be n 4))))))
    (etypecase item
      ((integer 0) (head 0 item))
      (integer (head 1 (- -1 item)))
      (string (pal-octets (head 3 (length item)) item))
      ((vector (unsigned-byte 8)) (pal-octets (head 2 (length item)) item))
      ((eql :null) (pal-octets #xF6))
      (list (case (first item)
              (:map (apply #'pal-octets (head 5 (floor (length (rest item)) 2)) (mapcar #'pal-cbor (rest item))))
              (:tag (pal-octets (head 6 (second item)) (pal-cbor (third item))))
              (t (apply #'pal-octets (head 4 (length item)) (mapcar #'pal-cbor item))))))))

(defun pal-box (type &rest contents)
  "A box of TYPE holding CONTENTS (as PAL-OCTETS joins them), its length first, as JUMBF and ISO write one."
  (let ((body (apply #'pal-octets contents)))
    (pal-octets (pal-be (+ 8 (length body)) 4) type body)))

(defun pal-superbox (kind label &rest boxes)
  "A JUMBF superbox of KIND (four letters) and LABEL holding BOXES after its description box."
  (apply #'pal-box "jumb" (pal-box "jumd" kind 0 #x11 0 #x10 #x80 0 0 #xAA 0 #x38 #x9B #x71 3 label 0) boxes))

(defun pal-der (tag &rest contents)
  "A DER item of TAG holding CONTENTS (as PAL-OCTETS joins them)."
  (let ((body (apply #'pal-octets contents)))
    (pal-octets tag (cond ((< (length body) 128) (pal-octets (length body)))
                          ((< (length body) 65536) (pal-octets #x82 (pal-be (length body) 2)))
                          (t (pal-octets #x83 (pal-be (length body) 3))))
                body)))

(defun pal-certificate (subject issuer)
  "An X.509 certificate, unsigned, for SUBJECT of the organisation Kea Test, issued by ISSUER, both common names."
  (flet ((name (&rest parts)
           (apply #'pal-der #x30 (loop for (oid text) in parts
                                       collect (pal-der #x31 (pal-der #x30 (pal-der 6 oid) (pal-der #x0C text)))))))
    (pal-der #x30
             (pal-der #x30 (pal-der #xA0 (pal-der 2 2)) (pal-der 2 1)
                      (pal-der #x30 (pal-der 6 #x2A #x86 #x48 #xCE #x3D 4 3 2))
                      (name (list (pal-octets #x55 4 3) issuer)) (pal-der #x30)
                      (name (list (pal-octets #x55 4 3) subject) (list (pal-octets #x55 4 10) "Kea Test")))
             (pal-der #x30) (pal-der 3 0))))

(defun pal-c2pa (&key (generator "Kea Maker/1.0") (actions '(("c2pa.created" nil nil))) ingredients
                      (claim nil claim-p))
  "A C2PA manifest store of one manifest: its claim by GENERATOR (or the octets CLAIM in its place), an actions
assertion of ACTIONS, each (ACTION SOURCE-TYPE AGENT), an ingredient for each title of INGREDIENTS, and a
signature whose certificate is Kea Signer's, issued by Kea Root CA."
  (pal-superbox
   "c2pa" "c2pa"
   (pal-superbox
    "c2ma" "urn:uuid:00000000-0000-4000-8000-000000000001"
    (apply #'pal-superbox "c2as" "c2pa.assertions"
           (pal-superbox "cbor" "c2pa.actions"
                         (pal-box "cbor"
                                  (pal-cbor
                                   (list :map "actions"
                                         (loop for (action source agent) in actions
                                               collect (append (list :map "action" action)
                                                               (and source
                                                                    (list "digitalSourceType"
                                                                          (format nil "http://cv.iptc.org/newscodes/~
                                                                                       digitalsourcetype/~a"
                                                                                  source)))
                                                               (and agent (list "softwareAgent" agent))))))))
           (loop for title in ingredients
                 collect (pal-superbox "cbor" "c2pa.ingredient.v3"
                                       (pal-box "cbor" (pal-cbor (list :map "dc:title" title
                                                                       "relationship" "parentOf"))))))
    (pal-superbox "c2cl" "c2pa.claim"
                  (pal-box "cbor" (if claim-p
                                      claim
                                      (pal-cbor (list :map "claim_generator" generator "dc:title" "kea.png"
                                                      "dc:format" "image/png")))))
    (pal-superbox "c2cs" "c2pa.signature"
                  (pal-box "cbor" (pal-cbor (list :tag 18 (list (pal-cbor (list :map 1 -7 33 (pal-certificate
                                                                                              "Kea Signer"
                                                                                              "Kea Root CA")))
                                                                (list :map) :null (pal-octets 1 2 3 4)))))))))

(defun pal-jpeg-app11 (store &key (pieces 3) reversed (box 1) (sequences nil))
  "The octets of a JPEG holding STORE, a JUMBF box, split over PIECES APP11 segments as JPEG XT splits one, each
after the first repeating the box's header; written last first when REVERSED. BOX is the box's number, and
SEQUENCES, when given, the sequence numbers the pieces are written with in their place. No picture: only its
segments."
  (let* ((header (subseq store 0 8)) (payload (subseq store 8)) (size (ceiling (length payload) pieces))
         (segments (loop for k below pieces
                         for from = (min (length payload) (* k size)) for to = (min (length payload) (+ from size))
                         collect (let ((data (pal-octets "JP" (pal-be box 2)
                                                         (pal-be (if sequences (nth k sequences) (1+ k)) 4) header
                                                         (subseq payload from to))))
                                   (pal-octets #xFF #xEB (pal-be (+ 2 (length data)) 2) data)))))
    (apply #'pal-octets (pal-octets #xFF #xD8) (append (if reversed (reverse segments) segments)
                                                       (list (pal-octets #xFF #xD9))))))

(define-test (palimpsest palimpsest-credentials-say-generated)
  ;; content credentials whose action names an image generator's digital source type: worth a look, never strong,
  ;; signed by whom they say and not verified; the same store in a PNG's caBX chunk and over a JPEG's APP11 segments
  (let* ((store (pal-c2pa :generator "Adobe_Firefly adobe_c2pa/0.5.0"
                          :actions '(("c2pa.created" "trainedAlgorithmicMedia" "Kea Draw 2"))))
         (png (palimpsest::read-picture-file (pal-png (pal-scan) 600 500 (list "caBX" store)) :png)))
    (dolist (file (list png
                        (palimpsest::make-picture-file
                         :kind :jpeg :width 1024 :height 1024
                         :credentials (palimpsest::picture-credentials (pal-jpeg-app11 store :reversed t) :jpeg))))
      (let* ((found (palimpsest::check-picture-making file)) (look (pal-finding found :look "")))
        (true (pal-finding found :look "says it was made by an image generator") "~s"
              (mapcar #'palimpsest::finding-words found))
        (true (pal-finding found :look "trainedAlgorithmicMedia (made by a model trained on other pictures"))
        (true (pal-finding found :look "the signature is not verified here"))
        (true (pal-finding found :look "names Adobe Firefly"))
        (true (pal-finding found :look "not that the document is false"))
        (false (pal-finding found :strong ""))
        (when look
          (is eq :generated (palimpsest::finding-method look))
          (true (find-if (lambda (d)
                           (search (concatenate 'string "signed by CN=Kea Signer, O=Kea Test, its certificate issued "
                                                        "by CN=Kea Root CA, not verified")
                                   d))
                         (palimpsest::finding-details look))
                "~s" (palimpsest::finding-details look)))))
    ;; the size only beside the credentials, never alone
    (true (pal-finding (palimpsest::check-picture-making
                        (palimpsest::make-picture-file :kind :jpeg :width 1024 :height 1024
                                                       :credentials (palimpsest::picture-credentials
                                                                     (pal-jpeg-app11 store) :jpeg)))
                       :look "Its size, 1024 by 1024 pixels"))
    (false (palimpsest::check-picture-making (palimpsest::make-picture-file :kind :png :width 1024 :height 1024)))
    ;; an ingredient made by a generator is said as the ingredient's
    (true (pal-finding (palimpsest::making-findings
                        "the picture"
                        (list (palimpsest::make-credential :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil)))
                              (palimpsest::make-credential :generator "Kea Editor" :active t
                                                           :actions '(("c2pa.opened" nil nil))))
                        '() '())
                       :look "the content credentials of a file it was made from say: c2pa.created"))))

(define-test (palimpsest palimpsest-credentials-say-no-generator)
  ;; content credentials of a camera: recorded with what they say, nothing worth a look
  (let* ((store (pal-c2pa :generator "Kea Camera 3" :actions '(("c2pa.created" "digitalCapture" nil)
                                                               ("c2pa.edited" nil "Kea Editor"))
                          :ingredients '("first.jpg")))
         (found (palimpsest::check-picture-making
                 (palimpsest::make-picture-file :kind :png :width 1024 :height 1024
                                                :credentials (palimpsest::picture-credentials
                                                              (pal-png (pal-scan) 600 500 (list "caBX" store))
                                                              :png)))))
    (false (pal-finding found :look "") "~s" (mapcar #'palimpsest::finding-words found))
    (let ((record (pal-finding found :record (concatenate 'string "holds content credentials (C2PA) written by Kea "
                                                                  "Camera 3 that say nothing of an image generator: "
                                                                  "c2pa.created, c2pa.edited; signed by CN=Kea Signer"))))
      (true record "~s" (mapcar #'palimpsest::finding-words found))
      (when record
        (true (find "  made from: first.jpg (parentOf)" (palimpsest::finding-details record) :test #'string=))
        (true (find-if (lambda (d) (search "digital source type digitalCapture (taken by a camera or scanner)" d))
                       (palimpsest::finding-details record)))))))

(define-test (palimpsest palimpsest-generator-marks)
  ;; what a picture's metadata says of an image generator, as the note gives it
  (flet ((findings (&rest sections)
           (palimpsest::check-picture-making (palimpsest::make-picture-file :kind :png :sections sections
                                                                            :width 832 :height 1216))))
    (dolist (case (list (list (format nil "* PNG text~%- parameters :: a kea on a hill Steps: 20, Sampler: Euler a, ~
                                           CFG scale: 7, Seed: 1~%")
                              "Stable Diffusion's settings")
                        (list (format nil "* PNG text~%- prompt :: {\"3\": {\"class_type\": \"KSampler\"}}~%")
                              "ComfyUI's prompt")
                        (list (format nil "* XMP~%- Iptc4xmpExt:DigitalSourceType :: http://cv.iptc.org/newscodes/~
                                           digitalsourcetype/compositeWithTrainedAlgorithmicMedia~%")
                              "put together with parts an image generator made")
                        (list (format nil "* EXIF (in its eXIf chunk)~%- Software :: Midjourney 6~%")
                              "names Midjourney (an image generator)")
                        (list (format nil "* PNG text~%- Software :: NovelAI~%- Source :: Stable Diffusion XL~%")
                              "names NovelAI")
                        ;; a prompt long enough to push the settings past what the note keeps
                        (list (format nil "* PNG text~%- parameters :: ~a~%"
                                      (with-output-to-string (s) (dotimes (k 80) (write-string "a kea " s))))
                              "Stable Diffusion's settings")))
      (destructuring-bind (section words) case
        (let ((found (findings section)))
          (true (pal-finding found :look words) "~s" (mapcar #'palimpsest::finding-words found))
          (true (pal-finding found :look "Its size, 832 by 1216 pixels"))
          (false (pal-finding found :look "signature"))
          (false (pal-finding found :strong "")))))
    ;; honest pictures: an editor, a camera's source type, a short text called parameters, content credentials kept
    ;; elsewhere
    (let ((found (findings (format nil "* EXIF (in its APP1 segment)~%- Software :: GIMP 2.10.38~%~
                                        - Make :: Kea Phones~%")
                           (format nil "* PNG text~%- parameters :: brightness 3~%- Comment :: Created with GIMP~%~
                                        - prompt :: Sign here~%")
                           (format nil "* XMP~%- Iptc4xmpExt:DigitalSourceType :: http://cv.iptc.org/newscodes/~
                                        digitalsourcetype/digitalCapture~%- dcterms:provenance :: ~
                                        https://example.org/kea.c2pa~%"))))
      (false (pal-finding found :look "") "~s" (mapcar #'palimpsest::finding-words found))
      (true (pal-finding found :record "digitalCapture (taken by a camera or scanner)"))
      (true (pal-finding found :record (concatenate 'string "points to content credentials kept elsewhere, at "
                                                            "https://example.org/kea.c2pa, not fetched"))))
    ;; credentials the XMP says the file keeps: said only when it holds none
    (let ((section (format nil "* XMP~%- dcterms:provenance :: self#jumbf=/c2pa/kea:urn:uuid:1/c2pa.claim~%")))
      (true (pal-finding (findings section) :record "which it does not hold: taken out, or never written"))
      (false (pal-finding (palimpsest::check-picture-making
                           (palimpsest::make-picture-file :kind :png :sections (list section)
                                                          :credentials (list (palimpsest::make-credential
                                                                              :generator "Kea Camera" :active t))))
                          nil "does not hold")))))

(define-test (palimpsest palimpsest-credentials-hostile)
  ;; content credentials made to break their reader end quickly, within bounded memory, as a problem recorded
  (let ((started (get-internal-run-time))
        (consed
          (pal-consed
           (lambda ()
             (dolist (octets (list (make-array 100001 :element-type '(unsigned-byte 8) :initial-element #x81)
                                   (pal-octets #x9B 255 255 255 255 255 255 255 255)
                                   (pal-octets #x7B 255 255 255 255 255 255 255 255 "kea")
                                   (pal-octets #x9F (make-array 100000 :element-type '(unsigned-byte 8)
                                                                       :initial-element 0))
                                   (pal-octets #xBF #x61 "k" #x5F #x41 "a" #x41 "b")
                                   (pal-octets #x9A (pal-be 300000 4)
                                               (make-array 300000 :element-type '(unsigned-byte 8) :initial-element 0))
                                   (pal-octets #x1C) (pal-octets #x65 "ka") (pal-octets #xFF) (pal-octets #xC1)))
               (fail (palimpsest::cbor-read octets) 'palimpsest::unreadable-credentials))
             (dolist (store (list (pal-octets (pal-be 1000 4) "jumb")
                                  (pal-octets (pal-be 1 4) "jumb" (pal-be (expt 2 62) 8))
                                  (pal-octets (pal-be 4 4) "jumb")
                                  (let ((box (pal-superbox "c2ma" "deep")))
                                    (dotimes (k 40 (pal-superbox "c2pa" "c2pa" box))
                                      (setf box (pal-superbox "c2ma" "deep" box))))
                                  (apply #'pal-superbox "c2pa" "c2pa"
                                         (loop repeat 20000 collect (pal-box "free")))
                                  (pal-box "jumb" (pal-box "free"))))
               (let ((credentials (palimpsest::credentials-in store)))
                 (is = 1 (length credentials))
                 (true (palimpsest::credential-problem (first credentials)))))
             ;; a manifest whose claim nests too deep: its problem said, the rest of it read
             (let ((credential (first (palimpsest::credentials-in
                                       (pal-c2pa :claim (make-array 5000 :element-type '(unsigned-byte 8)
                                                                         :initial-element #x81)
                                                 :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil)))))))
               (pal-has (palimpsest::credential-problem credential) "nest deeper")
               (true (palimpsest::credential-actions credential)))
             ;; text that would act on a terminal stays on one line in the finding
             (let ((found (palimpsest::check-picture-making
                           (palimpsest::make-picture-file
                            :kind :png
                            :credentials (palimpsest::credentials-in
                                          (pal-c2pa :generator (format nil "Kea~%Maker~c[31m" (code-char 27))))))))
               (true (pal-finding found :record "Kea Maker"))
               (false (some (lambda (f) (find #\Newline (palimpsest::finding-words f))) found)))
             ;; a picture's APP11 segments that are not JUMBF, or cut short
             (false (palimpsest::picture-credentials (pal-octets #xFF #xD8 #xFF #xEB 0 6 "JPab" #xFF #xD9) :jpeg))))))
    (true (< (- (get-internal-run-time) started) (* 5 internal-time-units-per-second)))
    (true (< consed (* 256 1024 1024)) "~:d octets consed" consed)))

(define-test (palimpsest palimpsest-credentials-in-a-pdf)
  ;; a PDF keeping content credentials in an associated file, as C2PA keeps them, packed: worth a look, never strong
  (let* ((store (pal-c2pa :generator "Kea Writer" :actions '(("c2pa.created" "trainedAlgorithmicMedia" "Kea Draw 2"))))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /AF [7 0 R] >>")
                                     '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                     '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents 4 0 R >>")
                                     '(4 :stream "" "")
                                     '(6 . "<< /Producer (Kea Writer) >>")
                                     '(7 . "<< /Type /Filespec /F (manifest.c2pa) /AFRelationship /C2PA_Manifest
/EF << /F 8 0 R >> >>")
                                     (list 8 :stream "/Subtype /application#2Fc2pa /Filter /FlateDecode"
                                           (pal-zlib-stored store))))))
    (multiple-value-bind (pdf accounts findings) (pal-read octets)
      (declare (ignore pdf accounts))
      (let ((look (pal-finding findings :look "The PDF says it was made by an image generator")))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when look (is equal '(8) (palimpsest::finding-objects look))))
      (false (pal-finding findings :strong "")))
    ;; the invoice, honest, says nothing of how it was made
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-original))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "content credentials"))
      (false (pal-finding findings nil "image generator")))))

(defun pal-smooth-page (pattern)
  "The grey octets of a page as an image generator draws one, 600 by 500 pixels: PAL-SCAN's words on paper of
grey 235 straying by 0 to 2, with PATTERN, a function of a pixel's column and row giving grey levels to add,
over all of it."
  (let ((grey (pal-scan)) (seed 4242))
    (dotimes (y 500 grey)
      (dotimes (x 600)
        (let ((at (+ (* y 600) x)))
          (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
          (when (>= (aref grey at) 225)
            (setf (aref grey at) (+ 235 (mod (ash seed -16) 3))))
          (setf (aref grey at) (max 0 (min 255 (+ (aref grey at) (funcall pattern x y))))))))))

(defun pal-decoder-tile (x y)
  "A faint pattern of 8 by 8 pixels changing along rows and columns at once, as an image generator's decoder
leaves one: -1, 0 or 1 grey level, the same in every cell, at about one pixel in 8 (a hash of X across and Y
down picks them), and 0 at the rest."
  (if (zerop (mod (ash (logand (* (+ (* x 7919) y 104729) 2654435761) #xFFFFFFFF) -13) 8))
      (1- (mod (+ (* 7 (mod x 8) (mod y 8)) (* 3 (mod x 8)) (mod y 8)) 3))
      0))

(define-test (palimpsest palimpsest-generator-grid)
  ;; a picture kept without loss, its flat parts carrying a decoder's pattern of 8 by 8 pixels: worth a look, never
  ;; strong; measured alike when the picture was cut at another place
  (let* ((analysis (pal-examined (pal-png (pal-smooth-page #'pal-decoder-tile) 600 500) "page.png"))
         (findings (palimpsest::analysis-findings analysis))
         (look (pal-finding findings :look "a faint pattern repeating every 8 by 8 pixels")))
    (true look "~s" (mapcar #'palimpsest::finding-words findings))
    (when look
      (pal-has (palimpsest::finding-words look) "The picture carries in its flat parts")
      (pal-has (first (palimpsest::finding-details look)) "600 by 500 pixels, the pattern"))
    (false (pal-finding findings :strong "")))
  (true (multiple-value-call #'palimpsest::generator-grid-p
          (palimpsest::generator-grid (pal-smooth-page (lambda (x y) (pal-decoder-tile (+ x 3) (+ y 5)))) 600 500)))
  ;; the same page with no pattern, or with steps along the rows and columns of 8 by 8 blocks as a JPEG's blocks
  ;; and a scanner's sensor leave: not named
  (flet ((named (pattern)
           (multiple-value-call #'palimpsest::generator-grid-p
             (palimpsest::generator-grid (pal-smooth-page pattern) 600 500))))
    (false (named (constantly 0)))
    (false (named (lambda (x y) (if (or (zerop (mod x 8)) (zerop (mod y 8))) 1 0))))
    (false (named (lambda (x y) (declare (ignore y)) (if (= 3 (mod x 8)) 1 0)))))
  ;; a page whose flat parts are one grey to the last pixel cannot be measured; a picture kept as a JPEG is not
  (false (palimpsest::generator-grid (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 200)
                                     600 500))
  (let ((palimpsest::*picture-file* (palimpsest::make-picture-file :kind :jpeg)))
    (false (palimpsest::scan-generator-grid nil nil (palimpsest::make-mark :object 1)
                                            (pal-smooth-page #'pal-decoder-tile) 600 500 nil))))

(define-test (palimpsest palimpsest-generator-grid-not-once-a-jpeg)
  ;; a picture kept without loss but once saved as a JPEG is not measured: a JPEG cropped by a few pixels and saved
  ;; again leaves a pattern of 8 by 8 pixels changing both ways, as a decoder's does
  (let ((page (pal-smooth-page #'pal-decoder-tile)) (mark (palimpsest::make-mark :object 1))
        (palimpsest::*picture-file* (palimpsest::make-picture-file :kind :png)))
    (true (palimpsest::scan-generator-grid nil nil mark page 600 500 nil))
    (false (palimpsest::scan-generator-grid nil nil mark page 600 500 '(4 4))))
  ;; an ordered dither on grey paper repeats every 8 by 8 pixels far more strongly than a decoder's pattern: not
  ;; named
  (false (multiple-value-call #'palimpsest::generator-grid-p
           (palimpsest::generator-grid (pal-smooth-page (lambda (x y) (- (mod (+ (* 3 x) (* 5 y) (* x y)) 4) 2)))
                                       600 500))))

(defun pal-blank-page (sigma width height seed)
  "Plain paper of grey 200, WIDTH by HEIGHT pixels, with noise of about SIGMA grey levels, seeded by SEED."
  (let ((grey (make-array (* width height) :element-type '(unsigned-byte 8))))
    (dotimes (i (length grey) grey)
      (let ((sum 0))
        (dotimes (k 4)
          (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
          (incf sum (/ (ash seed -15) 65536.0)))
        (setf (aref grey i) (max 0 (min 255 (round (+ 200 (* sigma 1.732 (- sum 2)))))))))))

(define-test (palimpsest palimpsest-generator-grid-not-on-smoothed-paper)
  ;; blank grey paper saved as a JPEG of quality 60, cropped by 3 and 5 pixels and saved at 95, kept as a PNG:
  ;; the JPEG smooths its noise away and leaves no history to tell, and the two grids out of step leave a pattern
  ;; that over so little noise would pass for a decoder's; not named
  (let* ((first (pal-jpeg-saved (pal-blank-page 1.6 800 600 7) 0 0 800 600 :quality 60 :width 800))
         (again (pal-jpeg-saved first 3 5 797 595 :quality 95 :width 800)))
    (false (multiple-value-call #'palimpsest::generator-grid-p (palimpsest::generator-grid again 800 600)))))

(define-test (palimpsest palimpsest-jpeg-in-a-tiff)
  ;; a TIFF whose Compression is 7 or 6 keeps a JPEG: kept with loss, as a JPEG file is; one of 1 or 5 is not.
  ;; Its JPEG blocks are searched as those of a picture once a JPEG, as before (PICTURE-JPEG-P stays false)
  (flet ((tiff (compression)
           (pal-octets "II" 42 0 8 0 0 0 1 0 3 1 3 0 1 0 0 0 compression 0 0 0 0 0 0 0)))
    (dolist (c '(6 7))
      (true (palimpsest::tiff-jpeg-p (tiff c)))
      (let ((palimpsest::*picture-file* (palimpsest::make-picture-file :kind :tiff :jpeg-tiff t)))
        (true (palimpsest::picture-lossy-p nil nil (palimpsest::make-mark :object 1)))
        (false (palimpsest::picture-jpeg-p nil nil 1))))
    (dolist (c '(1 5))
      (false (palimpsest::tiff-jpeg-p (tiff c))))
    (let ((palimpsest::*picture-file* (palimpsest::make-picture-file :kind :tiff)))
      (false (palimpsest::picture-lossy-p nil nil (palimpsest::make-mark :object 1))))))

(define-test (palimpsest palimpsest-credentials-long-oid)
  ;; a certificate whose name holds an object identifier of 200 KB: named cut, quickly and in little memory
  (let* ((oid (make-array 200000 :element-type '(unsigned-byte 8) :initial-element #xFF))
         (name (pal-der #x30 (pal-der #x31 (pal-der #x30 (pal-der 6 oid) (pal-der #x0C "Kea")))))
         (certificate (pal-der #x30 (pal-der #x30 (pal-der #xA0 (pal-der 2 2)) (pal-der 2 1) (pal-der #x30) name
                                             (pal-der #x30) name)))
         (started (get-internal-run-time)) (subject nil)
         (consed (pal-consed (lambda () (setf subject (palimpsest::certificate-names certificate))))))
    (pal-has subject "...=Kea")
    (true (< (- (get-internal-run-time) started) internal-time-units-per-second))
    (true (< consed (* 16 1024 1024)) "~:d octets consed" consed)))

(define-test (palimpsest palimpsest-credentials-pieces-counted)
  ;; a string of undetermined length in millions of empty pieces is refused at the item budget; a PNG's credentials
  ;; larger than the most read are not read
  (let ((octets (make-array 300002 :element-type '(unsigned-byte 8) :initial-element #x40)))
    (setf (aref octets 0) #x5F (aref octets 300001) #xFF)
    (fail (palimpsest::cbor-read octets) 'palimpsest::unreadable-credentials))
  (let* ((store (pal-c2pa :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil))))
         (png (pal-png (pal-scan) 600 500 (list "caBX" store)))
         (credentials (let ((palimpsest::*credentials-store-most* 100)) (palimpsest::picture-credentials png :png))))
    (is = 1 (length credentials))
    (pal-has (or (palimpsest::credential-problem (first credentials)) "") "more than 100 ")))

(define-test (palimpsest palimpsest-credentials-every-store)
  ;; an honest store first and one that says a generator made the picture after: the last is its own, and two
  ;; stores are said
  (let* ((honest (pal-c2pa :generator "Kea Camera" :actions '(("c2pa.created" "digitalCapture" nil))))
         (made (pal-c2pa :generator "Kea Maker" :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil))))
         (first (pal-jpeg-app11 honest :box 1)) (second (pal-jpeg-app11 made :box 2))
         (jpeg (pal-octets (subseq first 0 (- (length first) 2)) (subseq second 2)))
         (found (palimpsest::check-picture-making
                 (palimpsest::make-picture-file :kind :jpeg
                                                :credentials (palimpsest::picture-credentials jpeg :jpeg)))))
    (true (pal-finding found :look (concatenate 'string "its content credentials (C2PA) say: c2pa.created, "
                                                        "digital source type trainedAlgorithmicMedia"))
          "~s" (mapcar #'palimpsest::finding-words found))
    (true (pal-finding found :record "holds 2 stores of content credentials"))
    ;; two caBX chunks in a PNG are read too
    (let ((found (palimpsest::check-picture-making
                  (palimpsest::make-picture-file
                   :kind :png :credentials (palimpsest::picture-credentials
                                            (pal-png (pal-scan) 600 500 (list "caBX" honest) (list "caBX" made))
                                            :png)))))
      (true (pal-finding found :look "trainedAlgorithmicMedia"))))
  ;; APP11 segments repeated or missing: said
  (let ((store (pal-c2pa :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil)))))
    (let ((found (palimpsest::check-picture-making
                  (palimpsest::make-picture-file
                   :kind :jpeg :credentials (palimpsest::picture-credentials
                                             (pal-jpeg-app11 store :pieces 3 :sequences '(1 2 2)) :jpeg)))))
      (true (pal-finding found :record "box 1 repeats sequence 2") "~s" (mapcar #'palimpsest::finding-words found))
      (false (pal-finding found :strong "")))
    (let ((found (palimpsest::check-picture-making
                  (palimpsest::make-picture-file
                   :kind :jpeg :credentials (palimpsest::picture-credentials
                                             (pal-jpeg-app11 store :pieces 3 :sequences '(1 3 4)) :jpeg)))))
      (true (pal-finding found :record "box 1 lacks sequence 2") "~s" (mapcar #'palimpsest::finding-words found))
      (false (pal-finding found :strong "")))))

(define-test (palimpsest palimpsest-generator-names-only-as-programs)
  ;; a generator's name in a comment or a source is anyone's words: nothing worth a look; in Software it is a mark
  (flet ((findings (&rest sections)
           (palimpsest::check-picture-making (palimpsest::make-picture-file :kind :jpeg :sections sections))))
    (let ((found (findings (format nil "* JPEG comments~%- Comment :: I like to draw things~%")
                           (format nil "* EXIF (in its APP1 segment)~%- UserComment :: Interview with OpenAI staff~%")
                           (format nil "* XMP~%- photoshop:Source :: Midjourney office~%~
                                        - dc:source :: Stable Diffusion talk~%"))))
      (false (pal-finding found :look "") "~s" (mapcar #'palimpsest::finding-words found)))
    (false (pal-finding (palimpsest::check-picture-metadata
                         (palimpsest::make-picture-file
                          :kind :jpeg :sections (list (format nil "* JPEG comments~%- Comment :: draw things~%"))))
                        nil "Draw Things")
           "a generator's name in a comment is not a program named")
    (true (pal-finding (findings (format nil "* XMP~%- xmp:CreatorTool :: Midjourney~%")) :look "names Midjourney"))))

(define-test (palimpsest palimpsest-credentials-key-twice)
  ;; a map giving a key twice, as a claim that says two things at once: refused, the problem said
  (fail (palimpsest::cbor-read (pal-cbor (list :map "claim_generator" "Kea Camera" "claim_generator" "Kea Maker")))
        'palimpsest::unreadable-credentials)
  (let ((credential (first (palimpsest::credentials-in
                            (pal-c2pa :claim (pal-cbor (list :map "claim_generator" "Kea Camera"
                                                             "claim_generator" "Kea Maker")))))))
    (pal-has (or (palimpsest::credential-problem credential) "") "gives the key claim_generator twice")
    (false (palimpsest::credential-generator credential))))

(define-test (palimpsest palimpsest-generated-headline)
  ;; algorithmicMedia alone says a program made it, not an image generator; a generator in an ingredient's
  ;; credentials alone says a part of it was made so
  (flet ((words (credentials &optional marks)
           (let ((look (pal-finding (palimpsest::making-findings "the picture" credentials marks '()) :look "")))
             (and look (palimpsest::finding-words look)))))
    (let ((said (words (list (palimpsest::make-credential :active t
                                                          :actions '(("c2pa.created" "algorithmicMedia" nil)))))))
      (pal-has (or said "") "The picture says it was made by a program, not taken from life")
      (false (search "image generator:" (or said ""))))
    (pal-has (or (words (list (palimpsest::make-credential :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil)))
                              (palimpsest::make-credential :active t :actions '(("c2pa.opened" nil nil)))))
                 "")
             "The picture says a part of it was made by an image generator: the content credentials of a file")
    (pal-has (or (words '() (nth-value 0 (palimpsest::generator-marks
                                          '(("XMP" "Iptc4xmpExt:DigitalSourceType" "algorithmicMedia")))))
                 "")
             "says it was made by a program, not taken from life")))

(define-test (palimpsest palimpsest-credentials-keys-told-apart)
  ;; keys told apart by their whole value: two floats, long texts differing late, a byte string beside the same text
  ;; are different keys; the same byte string twice is the same key
  (flet ((reads (octets) (handler-case (progn (palimpsest::cbor-read octets) t)
                           (palimpsest::unreadable-credentials () nil)))
         (long (last) (concatenate 'string (make-string 5000 :initial-element #\k) last)))
    (true (reads (pal-octets #xA2 #xF9 #x3E #x00 #x61 "x" #xF9 #x41 #x00 #x61 "y")) "1.5 and 2.5 as keys")
    (true (reads (pal-cbor (list :map (long "a") 1 (long "b") 2))) "long keys differing past the text kept")
    (true (reads (pal-cbor (list :map (pal-octets "k") 1 "k" 2))) "a byte string and a text alike")
    (false (reads (pal-cbor (list :map (pal-octets "kea") 1 (pal-octets "kea") 2))) "the same byte string twice")
    (false (reads (pal-octets #xA2 #x7F #x61 "k" #x61 "ea" #xFF 1 #x63 "kea" 2)) "a text in pieces and whole")
    ;; a string of undetermined length nested in another is refused
    (false (reads (pal-octets #x7F #x7F #x61 "k" #xFF #xFF))))
  ;; several caBX chunks under the most read each, over it together: not read
  (let* ((store (pal-c2pa :actions '(("c2pa.created" "trainedAlgorithmicMedia" nil))))
         (png (pal-png (pal-scan) 600 500 (list "caBX" store) (list "caBX" store)))
         (credentials (let ((palimpsest::*credentials-store-most* (+ (length store) 10)))
                        (palimpsest::picture-credentials png :png))))
    (is = 1 (length credentials))
    (pal-has (or (palimpsest::credential-problem (first credentials)) "") "more than")))

;;; Local traces: regions of a page short of the JPEG traces the rest carries

(defun pal-region-laid (grey from x0 y0 x1 y1)
  "GREY (600 by 500) with the pixels of FROM, another page of the same size,
laid over the box from X0 Y0 to X1 Y1 (the last excluded), as a word typed
again over healed paper is laid in after a save. GREY, changed."
  (loop for y from y0 below y1
        do (loop for x from x0 below x1 do (setf (aref grey (+ (* y 600) x)) (aref from (+ (* y 600) x)))))
  grey)

(defun pal-detail-box (detail)
  "The box (X0 Y0 X1 Y1) a finding's DETAIL names as \"pixels X0,Y0 to X1,Y1\"."
  (let ((at (+ (search "pixels " detail) 7)))
    (multiple-value-bind (x0 end) (parse-integer detail :start at :junk-allowed t)
      (multiple-value-bind (y0 end) (parse-integer detail :start (1+ end) :junk-allowed t)
        (multiple-value-bind (x1 end) (parse-integer detail :start (+ end 4) :junk-allowed t)
          (list x0 y0 x1 (parse-integer detail :start (1+ end) :junk-allowed t)))))))

(define-test (palimpsest palimpsest-local-lattice-short)
  ;; a page saved as a JPEG and kept without loss, a line of it laid in again after the save with the same
  ;; look, ink and grain: the region named, worth a look; the page untouched, at quality 90 and 95, nothing
  (dolist (quality '(90 95))
    (let ((page (pal-jpeg-saved (pal-noisy-scan :sigma 2) 0 0 600 500 :quality quality)))
      (multiple-value-bind (pdf accounts findings)
          (pal-read (pal-scanned (pal-region-laid (copy-seq page) (pal-noisy-scan :sigma 2 :seed 99) 16 160 400 208)))
        (declare (ignore pdf accounts))
        (let ((laid (pal-finding findings :look "the traces of the JPEG the rest of the page was once saved")))
          (true laid "quality ~d: ~s" quality (mapcar #'palimpsest::finding-words findings))
          (when laid
            (destructuring-bind (x0 y0 x1 y1) (pal-detail-box (first (palimpsest::finding-details laid)))
              (true (and (< x0 400) (> x1 16) (< y0 208) (> y1 160)) "quality ~d: ~a ~a ~a ~a" quality x0 y0 x1 y1)))))
      (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
        (declare (ignore pdf accounts))
        (false (pal-finding findings nil "lacks the traces of the JPEG") "quality ~d: ~s" quality
               (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-local-requantised-short)
  ;; a JPEG saved twice, coarser first, a region of it laid in between the saves: named, with its place;
  ;; the same JPEG untouched, and one saved once, nothing
  (let ((table (palimpsest::standard-table 85))
        (twice (pal-jpeg-like 85 60)) (once (pal-jpeg-like 85)))
    (flet ((regions (grey)
             (let ((palimpsest::*picture-file* (palimpsest::make-picture-file :kind :jpeg
                                                                              :tables (list nil nil table))))
               (palimpsest::jpeg-traces nil nil nil grey 512 512 nil nil))))
      (let ((laid (copy-seq twice)))
        (loop for y from 128 below 224
              do (loop for x from 128 below 256 do (setf (aref laid (+ (* y 512) x)) (aref once (+ (* y 512) x)))))
        (let ((found (regions laid)))
          (true found)
          (when found
            (let ((r (first found)))
              (is eq :requantised (palimpsest::trace-region-kind r))
              (true (and (< (palimpsest::trace-region-x0 r) 256) (> (palimpsest::trace-region-x1 r) 128)
                         (< (palimpsest::trace-region-y0 r) 224) (> (palimpsest::trace-region-y1 r) 128))
                    "~s" r)))))
      (is equal '() (regions twice))
      (is equal '() (regions once)))))


(defun pal-painted (grey x0 y0 x1 y1 level)
  "GREY (600 by 500) with the box from X0 Y0 to X1 Y1 (the last excluded)
painted over in the one grey LEVEL, as a box laid over words in a picture
editor. GREY, changed."
  (loop for y from y0 below y1
        do (loop for x from x0 below x1 do (setf (aref grey (+ (* y 600) x)) level)))
  grey)

(define-test (palimpsest palimpsest-local-paper-painted-over)
  ;; a word of a grainy scan painted over in white or in the paper's grey: the region named, worth a look,
  ;; with its place; the page untouched, a page without grain and white on paper nearly white, nothing
  (dolist (level '(255 235))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-scanned (pal-painted (pal-noisy-scan :sigma 2) 206 168 290 196 level)))
      (declare (ignore pdf accounts))
      (let ((painted (pal-finding findings :look "without the grain the rest of the page's paper has")))
        (true painted "grey ~d: ~s" level (mapcar #'palimpsest::finding-words findings))
        (when painted
          (is eq :painted-over (palimpsest::finding-method painted))
          (pal-has (first (palimpsest::finding-details painted)) "times less")))))
  (flet ((named (grey)
           (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
             (declare (ignore pdf accounts))
             (pal-finding findings nil "without the grain"))))
    (false (named (pal-noisy-scan :sigma 2)))
    (false (named (pal-painted (pal-noisy-scan :sigma 0.3) 206 168 290 196 235)))
    (false (named (pal-painted (pal-noisy-scan :sigma 2 :paper 253) 206 168 290 196 255))))
  ;; ink in the picture's first and last columns and rows is no error
  (let ((grey (make-array (* 64 64) :element-type '(unsigned-byte 8) :initial-element 230)))
    (dolist (at (list 0 63 (* 63 64) (1- (* 64 64)))) (setf (aref grey at) 0))
    (let ((near (palimpsest::near-ink grey 64 64 128)))
      (is = 1 (sbit near 0))
      (is = 1 (sbit near (+ (* 3 64) 3)))
      (is = 0 (sbit near (+ (* 4 64) 4))))))

(defun pal-paper-columns (grey x0 x1)
  "GREY (600 by 500) with the columns from X0 to X1 (the last excluded) made
blank paper of its own grain, each row's taken from the blank rows under
its last line (460 to 499), so that no word stands there. GREY, changed."
  (dotimes (y 500 grey)
    (loop for x from x0 below x1
          do (setf (aref grey (+ (* y 600) x)) (aref grey (+ (* (+ 460 (mod y 40)) 600) x))))))

(define-test (palimpsest palimpsest-local-paint-where-text-runs)
  ;; paper painted smooth in a margin or a gutter with words on one side only: not named; a line painted over
  ;; from end to end: named, though it covers more of the page than words do
  (flet ((named (grey)
           (pal-finding (nth-value 2 (pal-read (pal-scanned grey))) :look "without the grain")))
    (false (named (pal-painted (pal-paper-columns (pal-noisy-scan :sigma 2) 0 110) 16 160 64 176 235))
           "a smooth left margin")
    (false (named (pal-painted (pal-paper-columns (pal-noisy-scan :sigma 2) 470 600) 528 160 576 176 235))
           "a smooth gutter beside the text")
    (true (named (pal-painted (pal-noisy-scan :sigma 2) 16 168 584 196 235)) "a line painted over")))

(defun pal-cloned (grey fx fy tx ty w h)
  "GREY (600 by 500) with the W by H pixels at FX FY laid over those at TX TY,
pixel for pixel, as a clone stamp copies paper over words. GREY, changed."
  (dotimes (y h grey)
    (dotimes (x w)
      (setf (aref grey (+ (* (+ ty y) 600) tx x)) (aref grey (+ (* (+ fy y) 600) fx x))))))

(defun pal-copied (grey)
  "The regions of copied paper COPIED-PAPER finds in the scanned page GREY
\(600 by 500), read as its scan reading reads it, with how many blocks it
searched as a second value."
  (let ((reading (palimpsest::picture-words grey 600 500)))
    (palimpsest::copied-paper grey 600 500 (and reading (palimpsest::picture-reading-words reading))
                              (and reading (palimpsest::picture-reading-tilt reading))
                              (palimpsest::page-level grey 600 500))))

(define-test (palimpsest palimpsest-copied-paper)
  ;; the end of a word of a grainy page kept without loss covered with paper cloned from under the last line:
  ;; both places named, worth a look, with the shift between them; the page untouched, nothing, though searched
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-cloned (pal-noisy-scan :sigma 2) 300 462 256 168 30 26)))
    (declare (ignore pdf accounts))
    (let ((copied (pal-finding findings :look "alike pixel for pixel")))
      (true copied "~s" (mapcar #'palimpsest::finding-words findings))
      (when copied
        (is eq :paper-copied (palimpsest::finding-method copied))
        (pal-has (palimpsest::finding-words copied) "44 pixels across and 294 down")
        (pal-has (first (palimpsest::finding-details copied)) "pixels 256,168 to")
        (pal-has (first (palimpsest::finding-details copied)) "pixels 300,462 to"))))
  (multiple-value-bind (regions searched) (pal-copied (pal-noisy-scan :sigma 2))
    (is equal '() regions)
    (true (> searched 10000)))
  (false (pal-finding (nth-value 2 (pal-read (pal-scanned (pal-noisy-scan :sigma 2)))) nil "alike pixel for pixel")))

(define-test (palimpsest palimpsest-copied-paper-not-searched)
  ;; paper cloned and then the page saved as a JPEG: the copy no longer matches, and kept as a JPEG it is not
  ;; searched; nor is a page of white paper clipped by the scanner
  (let ((cloned (pal-jpeg-saved (pal-cloned (pal-noisy-scan :sigma 2) 300 462 256 168 30 26) 0 0 600 500
                                :quality 90)))
    (false (pal-finding (nth-value 2 (pal-read (pal-scanned cloned))) nil "alike pixel for pixel"))
    (let ((palimpsest::*picture-file* (palimpsest::make-picture-file :kind :jpeg)))
      (false (palimpsest::scan-copied-paper nil nil (palimpsest::make-mark :object 1) cloned 600 500 nil 0 128))))
  ;; a page once saved as a JPEG, its paper cloned after and kept without loss: searched, and the copy found
  (let ((cloned (pal-cloned (pal-jpeg-saved (pal-noisy-scan :sigma 2) 0 0 600 500 :quality 90)
                            300 462 256 168 30 26)))
    (true (pal-finding (nth-value 2 (pal-read (pal-scanned cloned))) :look "alike pixel for pixel")))
  (let ((white (pal-cloned (pal-noisy-scan :sigma 2 :paper 255) 300 462 256 168 30 26)))
    (multiple-value-bind (regions searched) (pal-copied white)
      (is equal '() regions)
      (is = 0 searched))))

(defun pal-filled (grey x0 y0 x1 y1)
  "GREY (600 by 500) with the box from X0 Y0 to X1 Y1 covered with pieces of
8 by 8 pixels, each laid from another place of the blank paper just under
its last line (from rows 460 to 467 down), as a content-aware fill builds
paper over words.
GREY, changed."
  (loop for y from y0 below y1 by 8 for row from 0
        do (loop for x from x0 below x1 by 8 for k from 0
                 do (let ((fx (+ 20 (mod (+ (* 37 k) (* 101 row)) 520))) (fy (+ 460 (mod (+ (* 5 k) (* 3 row)) 8))))
                      (dotimes (j (min 8 (- y1 y)))
                        (dotimes (i (min 8 (- x1 x)))
                          (setf (aref grey (+ (* (+ y j) 600) x i)) (aref grey (+ (* (+ fy j) 600) fx i))))))))
  grey)

(define-test (palimpsest palimpsest-copied-paper-pieced)
  ;; two words covered with paper pieced together from many places, as a content-aware fill does: each piece
  ;; matches at a shift of its own, too few at one shift for a copied patch, and the patch is named as pieced
  ;; together, the paper between the line's other words searched though no word is left there
  (let ((regions (pal-copied (pal-filled (pal-noisy-scan :sigma 2) 210 168 380 196))))
    (true (find-if (lambda (r) (>= (palimpsest::copied-region-shifts r) 4)) regions) "~s" regions))
  (let ((copied (find :paper-copied (nth-value 2 (pal-read (pal-scanned (pal-filled (pal-noisy-scan :sigma 2)
                                                                                    210 168 380 196))))
                      :key #'palimpsest::finding-method)))
    (true copied)
    (when copied
      (is eq :look (palimpsest::finding-strength copied))
      (true (some (lambda (d) (search "the same as paper about them" d)) (palimpsest::finding-details copied))
            "~s" (palimpsest::finding-details copied)))))

(define-test (palimpsest palimpsest-copied-paper-jpeg-grid)
  ;; paper repeating every 8 pixels across and down, as a JPEG's flat blocks repeat, and a patch copied a whole
  ;; number of blocks away both ways: not named
  (let ((tiled (pal-noisy-scan :sigma 2)))
    (dotimes (y 500)
      (dotimes (x 600)
        (when (> (aref tiled (+ (* y 600) x)) 200)
          (setf (aref tiled (+ (* y 600) x)) (aref tiled (+ (* (mod y 8) 600) (mod x 8)))))))
    (is equal '() (pal-copied tiled)))
  (is equal '() (pal-copied (pal-cloned (pal-noisy-scan :sigma 2) 304 464 256 168 30 26)))
  (true (pal-copied (pal-cloned (pal-noisy-scan :sigma 2) 300 462 256 168 30 26))))

(define-test (palimpsest palimpsest-local-paint-clipped-white)
  ;; white on paper of grey 235 is named, the rule naming the honest look-alikes; white clipped on paper nearly
  ;; white, slanted or decoded as 254 in part, is not
  (let ((painted (pal-finding (nth-value 2 (pal-read (pal-scanned (pal-painted (pal-noisy-scan :sigma 2)
                                                                                206 168 290 196 255))))
                              :look "without the grain")))
    (true painted)
    (when painted (pal-has (palimpsest::finding-rule painted) "correction fluid")))
  (let ((grey (pal-noisy-scan :sigma 2 :paper 251)))
    (loop for y from 166 below 198
          do (loop for x from (+ 200 (floor (- y 166) 2)) below (+ 292 (floor (- y 166) 2))
                   do (setf (aref grey (+ (* y 600) x)) (if (zerop (mod (+ x y) 7)) 254 255))))
    (false (pal-finding (nth-value 2 (pal-read (pal-scanned grey))) nil "without the grain"))))

(define-test (palimpsest palimpsest-local-paint-crowded)
  ;; a page whose smooth paper lies in many pieces all over it is said not to have been judged, for the record
  (let ((grey (pal-noisy-scan :sigma 2)))
    (loop for y from 16 below 480 by 48
          do (loop for x from 16 below 580 by 48 do (pal-painted grey x y (+ x 16) (+ y 16) 235)))
    (let ((findings (nth-value 2 (pal-read (pal-scanned grey)))))
      (true (pal-finding findings :record "was not judged for paper painted over"))
      (false (pal-finding findings nil "without the grain")))))

(defun pal-jpeg-headers (&key (chroma-first t) (luma 90))
  "The octets of a JPEG's headers: a chroma table (number 1, all 99) and a
luminance table (number 0, the standard's scaled for LUMA), the chroma one
first with CHROMA-FIRST, in one DQT segment; a frame whose first component
uses table 0; and the start of its scan."
  (let* ((scale (if (< luma 50) (floor 5000 luma) (- 200 (* 2 luma))))
         (luminance (cons 0 (loop for s across palimpsest::*jpeg-luminance-table*
                                  collect (max 1 (min 255 (floor (+ (* s scale) 50) 100))))))
         (chroma (cons 1 (make-list 64 :initial-element 99)))
         (tables (if chroma-first (append chroma luminance) (append luminance chroma))))
    (coerce (append '(#xff #xd8 #xff #xdb) (list 0 (+ 2 (length tables))) tables
                    '(#xff #xc0 0 17 8 0 8 0 8 3 1 #x22 0 2 #x11 1 3 #x11 1)
                    '(#xff #xda 0 2))
            '(simple-array (unsigned-byte 8) (*)))))

(define-test (palimpsest palimpsest-local-table-of-luminance)
  ;; the table the frame's first component uses is read, though the chroma table comes first; and from a
  ;; JPEG kept in a PDF under a Flate filter before its own
  (dolist (first '(t nil))
    (multiple-value-bind (octets quality table) (palimpsest::jpeg-tables (pal-jpeg-headers :chroma-first first))
      (declare (ignore octets))
      (is = 90 quality "chroma first ~a" first)
      (is = 3 (first table) "chroma first ~a" first)))
  (let* ((pdf (pal-read (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                            '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                            (list 7 :stream (concatenate 'string "/Type /XObject /Subtype /Image "
                                                                         "/Width 8 /Height 8 /ColorSpace /DeviceGray "
                                                                         "/BitsPerComponent 8 "
                                                                         "/Filter [/FlateDecode /DCTDecode]")
                                                  (pal-zlib-stored (pal-jpeg-headers)))))))
         (table (palimpsest::picture-jpeg-table pdf (palimpsest::last-revision pdf) 7)))
    (true table)
    (is = 3 (first table))))

(defun pal-picture-file-findings (octets)
  "Every sign found in the JPEG file OCTETS, read and examined as palimpsest examines a picture file."
  (let* ((file (palimpsest::read-picture-file octets :jpeg))
         (palimpsest::*picture-file* file))
    (values (palimpsest::all-findings
             (palimpsest::read-pdf (palimpsest::picture-page-octets (palimpsest::picture-file-width file)
                                                                    (palimpsest::picture-file-height file))
                                   :name "picture")
             '() :file file))))

(define-test (palimpsest palimpsest-jpeg-thumbnail-file)
  ;; a camera's picture whose region was painted over after its EXIF thumbnail was made: worth a look, the
  ;; region named; the same picture untouched, or turned a quarter with the old thumbnail kept, alike
  (let* ((findings (pal-picture-file-findings (pal-sample "thumbnail-edited.jpg")))
         (look (pal-finding findings :look "The EXIF thumbnail of the picture differs from the picture at pixels")))
    (true look "~s" (mapcar #'palimpsest::finding-words findings))
    (when look
      (let* ((words (palimpsest::finding-words look))
             (from (+ 7 (search "pixels " words)))
             (box (mapcar #'parse-integer
                          (remove-if-not (lambda (s) (and (plusp (length s)) (every #'digit-char-p s)))
                                         (uiop:split-string (subseq words from (search " (" words :start2 from))
                                                            :separator ", ")))))
        ;; the cell lies within the box painted, 180,90 to 250,150
        (destructuring-bind (x0 y0 x1 y1) box
          (true (and (<= 170 x0) (<= x1 260) (<= 80 y0) (<= y1 160)) "~a" words)))
      (true (find-if (lambda (s) (search "dd bs=1 skip=" s)) (palimpsest::finding-steps look)))))
  (dolist (name '("thumbnail-honest.jpg" "thumbnail-turned.jpg"))
    (let ((findings (pal-picture-file-findings (pal-sample name))))
      (true (pal-finding findings :record "The EXIF thumbnail of the picture is like the picture") "~a ~s" name
            (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :look "EXIF thumbnail") "~a" name)
      ;; written as a camera writes: its EXIF first, baseline, the standard's Huffman tables
      (true (pal-finding findings :record "Canon EOS 5D as its maker, and its header is laid out as a device")
            "~a" name))))

(define-test (palimpsest palimpsest-jpeg-thumbnail-in-pdf)
  ;; the same pictures kept in a PDF as /DCTDecode: the one painted over worth a look, the one untouched not
  (dolist (case '(("thumbnail-edited.jpg" t) ("thumbnail-honest.jpg" nil)))
    (destructuring-bind (name edited) case
      (let* ((pdf (palimpsest::read-pdf
                   (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                       '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                       (cons 3 (concatenate 'string "<< /Type /Page /Parent 2 0 R /MediaBox "
                                                            "[0 0 320 240] /Resources << /XObject << /Im1 7 0 R >> "
                                                            ">> /Contents 4 0 R >>"))
                                       (list 4 :stream "" "q 320 0 0 240 0 0 cm /Im1 Do Q")
                                       (list 7 :stream (concatenate 'string "/Type /XObject /Subtype /Image /Width 320 "
                                                                    "/Height 240 /ColorSpace /DeviceGray "
                                                                    "/BitsPerComponent 8 /Filter /DCTDecode")
                                             (pal-sample name))))
                   :name "test.pdf"))
             (findings (palimpsest::check-jpeg-headers pdf (palimpsest::last-revision pdf))))
        (is eq edited (and (pal-finding findings :look "thumbnail of the JPEG picture Im1 (object 7) on page 1 differs")
                           t)
            "~a ~s" name (mapcar #'palimpsest::finding-words findings))
        (true (pal-finding findings :record "names Canon Canon EOS 5D as its maker") "~a" name)))))

(defun pal-exif-segment (&key (make "Canon") (model "Canon EOS 5D") software thumbnail)
  "An APP1 segment of an EXIF naming MAKE, MODEL and SOFTWARE (each left out when NIL) in its first directory and,
in a second, a thumbnail of THUMBNAIL, a list (OFFSET LENGTH) given as they are, whatever the EXIF holds."
  (let* ((texts (remove nil (list (and make (cons #x010f make)) (and model (cons #x0110 model))
                                  (and software (cons #x0131 software)))))
         (data (+ 8 2 (* 12 (length texts)) 4))
         (entries '()) (strings '()) (next nil))
    (dolist (text texts)
      (let ((value (pal-octets (cdr text) 0)))
        (push (pal-octets (ash (car text) -8) (logand (car text) 255) 0 2 0 0 0 (length value)
                          0 0 (ash data -8) (logand data 255))
              entries)
        (push value strings)
        (incf data (length value))))
    (setf next data)
    (let* ((tiff (apply #'pal-octets "MM" 0 42 0 0 0 8 0 (length texts)
                        (append (reverse entries)
                                (list 0 0 (ash next -8) (logand next 255))
                                (reverse strings)
                                (if thumbnail
                                    (destructuring-bind (offset length) thumbnail
                                      (flet ((long (n) (pal-octets (ldb (byte 8 24) n) (ldb (byte 8 16) n)
                                                                   (ldb (byte 8 8) n) (ldb (byte 8 0) n))))
                                        (list 0 2 (pal-octets 2 1 0 4 0 0 0 1 (long offset))
                                              (pal-octets 2 2 0 4 0 0 0 1 (long length)) 0 0 0 0)))
                                    (list 0 0)))))
           (body (pal-octets "Exif" 0 0 tiff)))
      (pal-octets #xff #xe1 (ash (+ 2 (length body)) -8) (logand (+ 2 (length body)) 255) body))))

(defun pal-jpeg-header-findings (&key (exif (pal-exif-segment)) before after (frame #xc0))
  "What palimpsest says of the header of a JPEG beginning with the segments BEFORE (octets), then EXIF, then AFTER,
then the tables and FRAME of PAL-JPEG-HEADERS: its header beside the device its EXIF names."
  (let* ((headers (pal-jpeg-headers))
         (frame-at (search (pal-octets #xff #xc0) headers))
         (octets (pal-octets #xff #xd8 (or before #()) exif (or after #()) (subseq headers 2 frame-at)
                             #xff frame (subseq headers (+ 2 frame-at)))))
    (palimpsest::header-findings (palimpsest::jpeg-layout octets) (multiple-value-list (palimpsest::jpeg-tables octets))
                                 (palimpsest::picture-fields (tuatara:octets-metadata octets :jpeg))
                                 "the picture")))

(define-test (palimpsest palimpsest-jpeg-header-program)
  ;; a camera named while the header is a program's: a progressive frame or Adobe's segment, each worth a look;
  ;; JFIF before the EXIF, which scanner drivers and phones write too, for the record; the same with an editor
  ;; named, for the record; laid out as a camera writes, for the record; no device named, nothing
  (let ((jfif (pal-octets #xff #xe0 0 16 "JFIF" 0 1 1 0 0 1 0 1 0 0))
        (adobe (pal-octets #xff #xee 0 14 "Adobe" 0 100 0 0 0 0 1)))
    (loop for (what findings) in (list (list "progressive" (pal-jpeg-header-findings :frame #xc2))
                                       (list "Adobe's segment" (pal-jpeg-header-findings :after adobe)))
          do (true (pal-finding findings :look "names Canon Canon EOS 5D as its maker, but a program wrote it")
                   "~a ~s" what (mapcar #'palimpsest::finding-words findings)))
    (let ((findings (pal-jpeg-header-findings :before jfif)))
      (false (pal-finding findings :look "") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :record "though a JFIF segment comes before its EXIF")))
    (let ((findings (pal-jpeg-header-findings :exif (pal-exif-segment :software "Adobe Photoshop 25.0") :frame #xc2)))
      (false (pal-finding findings :look ""))
      (true (pal-finding findings :record "Adobe Photoshop as a program it went through")))
    ;; JFIF after the EXIF, which still comes first as the standard has it, is no sign
    (let ((findings (pal-jpeg-header-findings :after jfif)))
      (false (pal-finding findings :look ""))
      (true (pal-finding findings :record "laid out as a device writes it"))
      (true (pal-finding findings :record "libjpeg's standard steps at quality 90")))
    (is equal '() (pal-jpeg-header-findings :exif (pal-exif-segment :make nil :model nil) :frame #xc2))))

(define-test (palimpsest palimpsest-jpeg-exif-hostile)
  ;; an EXIF whose thumbnail lies past its end, is too long, or whose directories say they hold 65,535 entries or
  ;; point at themselves: read within its bounds, the thumbnail refused with why, no error
  (flet ((layout (exif) (palimpsest::jpeg-layout (pal-octets #xff #xd8 exif (subseq (pal-jpeg-headers) 2)))))
    (is equal "it says it lies past the end of the EXIF"
        (palimpsest::jpeg-layout-thumbnail-why (layout (pal-exif-segment :thumbnail '(60 4000)))))
    (true (search "it says it takes" (palimpsest::jpeg-layout-thumbnail-why
                                      (layout (pal-exif-segment :thumbnail '(60 #xffffffff))))))
    (is equal "it does not begin as a JPEG does"
        (palimpsest::jpeg-layout-thumbnail-why (layout (pal-exif-segment :thumbnail '(8 4)))))
    ;; a first directory of 65,535 entries in a structure of 20 octets, and one pointing at itself as the next
    (is equal '() (palimpsest::exif-fields (pal-octets "MM" 0 42 0 0 0 8 #xff #xff 1 2 3 4 5 6 7 8 9 10) 0 20))
    (let ((self (pal-octets "MM" 0 42 0 0 0 8 0 1 1 #x0f 0 3 0 0 0 1 0 7 0 0 0 0 0 8)))
      (is equal '((:image #x010f 7)) (palimpsest::exif-fields self 0 (length self))))
    (is equal '() (palimpsest::exif-fields (pal-octets "MM" 0 42) 0 4))
    ;; three numbers of 16 bits, or two of 32, do not fit in an entry: it holds where they lie, not a number
    (let ((offsets (pal-octets "MM" 0 42 0 0 0 8 0 2 1 2 0 3 0 0 0 3 0 0 0 38 2 1 0 4 0 0 0 2 0 0 0 38 0 0 0 0
                               0 1 0 2 0 3 0 0 0 4)))
      (is equal '((:image #x0102 nil) (:image #x0201 nil)) (palimpsest::exif-fields offsets 0 (length offsets))))))

(defun pal-thumbnail-large-findings (thumbnail)
  "What THUMBNAIL-FINDINGS says of THUMBNAIL kept as the thumbnail of a smooth picture of 4096 by 3072 pixels, and
the seconds it took."
  (let* ((layout (palimpsest::make-jpeg-layout :thumbnail thumbnail :thumbnail-at 100))
         (begun (get-internal-real-time))
         (findings (palimpsest::thumbnail-findings
                    layout
                    (lambda ()
                      (let ((grey (make-array (* 4096 3072) :element-type '(unsigned-byte 8))))
                        (dotimes (k (length grey)) (setf (aref grey k) (mod (floor k 4096) 256)))
                        (values grey 4096 3072)))
                    "the picture" "the file")))
    (values findings (/ (- (get-internal-real-time) begun) internal-time-units-per-second))))

(define-test (palimpsest palimpsest-jpeg-thumbnail-too-large)
  ;; a smooth JPEG of 1024 by 1024 pixels in 22 KB kept as the thumbnail of a picture of 4096 by 3072: decoded
  ;; made 320 across, then compared (unlike it, so worth a look), quickly; the same JPEG claiming 2048 by 2048
  ;; refused for the record by its size before anything is decoded
  (let ((thumbnail (pal-sample "thumbnail-large.jpg")))
    (is = 1024 (palimpsest::jpeg-layout-width (palimpsest::jpeg-layout thumbnail)))
    (multiple-value-bind (findings took) (pal-thumbnail-large-findings thumbnail)
      (true (pal-finding findings :look "The EXIF thumbnail of the picture is unlike the picture as a whole")
            "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :record "was not set beside it") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (< took 3) "~,2f s" took))
    (let* ((claiming (copy-seq thumbnail))
           (frame (or (search (pal-octets #xff #xc0) claiming) (search (pal-octets #xff #xc2) claiming))))
      ;; the frame's height and width follow its marker, its length and the precision
      (setf (aref claiming (+ frame 5)) 8 (aref claiming (+ frame 7)) 8)
      (is = 2048 (palimpsest::jpeg-layout-width (palimpsest::jpeg-layout claiming)))
      (multiple-value-bind (findings took) (pal-thumbnail-large-findings claiming)
        (true (pal-finding findings :record "was not set beside it: not a thumbnail's size (2048 by 2048 pixels)")
              "~s" (mapcar #'palimpsest::finding-words findings))
        (true (< took 2) "~,2f s" took)))))

(define-test (palimpsest palimpsest-jpeg-thumbnail-larger)
  ;; a thumbnail of 512 by 384 pixels, as some phones keep, is made 320 across and compared: the picture painted
  ;; over after it was made is worth a look, the untouched one alike
  (let ((findings (pal-picture-file-findings (pal-sample "thumbnail-512-edited.jpg"))))
    (true (pal-finding findings :look "The EXIF thumbnail of the picture differs from the picture at pixels")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings :record "was not set beside it") "~s" (mapcar #'palimpsest::finding-words findings)))
  (let ((findings (pal-picture-file-findings (pal-sample "thumbnail-512-honest.jpg"))))
    (true (pal-finding findings :record "The EXIF thumbnail of the picture is like the picture")
          "~s" (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings :look "EXIF thumbnail") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-trace-region-backed)
  ;; a region lacking a save's traces is backed by a copy whose twin stands away from every such region and off
  ;; its rows, or by a word at it holding a character off its line; not by twins within the regions or on its rows,
  ;; as a stamp's own figures stand, nor by characters without grain, copies laid by software, or a word off its
  ;; line as a whole
  (let* ((region (palimpsest::make-trace-region :kind :lattice :x0 400 :y0 300 :x1 480 :y1 332))
         (other (palimpsest::make-trace-region :kind :lattice :x0 100 :y0 900 :x1 180 :y1 932))
         (regions (list region other)))
    (labels ((glyph (x y &optional bare)
               (palimpsest::make-glyph :page 0 :picture 7 :x0 x :y0 y :x1 (+ x 14) :y1 (+ y 20) :bare bare))
             (scan (&rest odd)
               (palimpsest::make-scan-picture :page (palimpsest::make-page :index 0)
                                              :mark (palimpsest::make-mark :object 7) :odd odd))
             (group (a b &key why laid) (list (list a b) 3.0 nil 10.0 :look why laid nil))
             (backing (copies &rest odd)
               (palimpsest::trace-backing region regions (apply #'scan odd) copies))
             (word (&rest ways)
               (cons (palimpsest::make-word-box :x0 410 :y0 304 :x1 470 :y1 326) ways)))
      (is eq :copy (backing (list (group (glyph 420 306) (glyph 800 600)))))
      ;; the twin on the region's rows, within it, or at another such region: a stamp's own figures
      (false (backing (list (group (glyph 420 306) (glyph 800 310)))))
      (false (backing (list (group (glyph 420 306) (glyph 450 306)))))
      (false (backing (list (group (glyph 420 306) (glyph 120 906)))))
      ;; no grain of their own, laid by software, or named as a stamp's
      (false (backing (list (group (glyph 420 306 t) (glyph 800 600)))))
      (false (backing (list (group (glyph 420 306) (glyph 800 600 t)))))
      (false (backing (list (group (glyph 420 306) (glyph 800 600) :laid t))))
      (false (backing (list (group (glyph 420 306) (glyph 800 600) :why :stamp))))
      ;; neither character at the region
      (false (backing (list (group (glyph 700 500) (glyph 800 600)))))
      ;; a character off its line in a word at the region; the word's own baseline is not enough
      (is eq :character (backing '() (word '(:character 3.0 0 "a character set ~a"))))
      (false (backing '() (word '(:offset 3.0 0 "a ~a baseline")))))))

(define-test (palimpsest palimpsest-local-traces-edges)
  ;; a grid said to begin past a narrow picture's edge leaves no tile, and no error
  (is equal '() (palimpsest::jpeg-traces nil nil nil
                                         (make-array 200000 :element-type '(unsigned-byte 8) :initial-element 200)
                                         1 200000 nil '(7 7)))
  ;; a JPEG turned a quarter, as its EXIF says it is held, saved once: its table read turned, as many steps
  ;; left empty as unturned
  (let* ((table (palimpsest::standard-table 85))
         (once (pal-jpeg-like 85))
         (turned (let ((out (make-array (* 512 512) :element-type '(unsigned-byte 8))))
                   (dotimes (y 512 out)
                     (dotimes (x 512) (setf (aref out (+ (* x 512) y)) (aref once (+ (* y 512) x))))))))
    (is = (nth-value 4 (palimpsest::requantised-tiles once 512 512 nil table))
        (nth-value 4 (palimpsest::requantised-tiles turned 512 512 nil table)))))

(define-test (palimpsest palimpsest-local-regions-counted)
  ;; two regions laid in after the save: named as two, "lack" agreeing with them, the honest causes in the rule
  (let ((page (pal-jpeg-saved (pal-noisy-scan :sigma 2) 0 0 600 500 :quality 95))
        (from (pal-noisy-scan :sigma 2 :seed 99)))
    (pal-region-laid page from 16 160 400 208)
    (pal-region-laid page from 16 384 400 432)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((laid (pal-finding findings :look "the traces of the JPEG the rest of the page")))
        (true laid "~s" (mapcar #'palimpsest::finding-words findings))
        (when laid
          (pal-has (palimpsest::finding-words laid)
                   "regions of page 1, in the scanned picture Im1 (object 7), lack the")
          (pal-has (palimpsest::finding-rule laid) "filtered after it"))))))

(define-test (palimpsest palimpsest-local-traces-guarded)
  ;; a JPEG whose table holds a step of nothing is measured on its other frequencies; a JPEG turned a quarter
  ;; and cut, its grid off the corner, is measured on its steps turned; and a failure in measuring leaves the
  ;; page's other checks to finish, said for itself
  (let* ((table (palimpsest::standard-table 85))
         (once (pal-jpeg-like 85)) (twice (pal-jpeg-like 85 60)) (laid (copy-seq twice)))
    (true (nth-value 4 (palimpsest::requantised-tiles twice 512 512 nil (substitute 0 (elt table 3) table :start 3
                                                                                                       :count 1))))
    (loop for y from 128 below 224
          do (loop for x from 128 below 256 do (setf (aref laid (+ (* y 512) x)) (aref once (+ (* y 512) x)))))
    (let ((turned (make-array (* 512 512) :element-type '(unsigned-byte 8))))
      (dotimes (y 512) (dotimes (x 512) (setf (aref turned (+ (* x 512) y)) (aref laid (+ (* y 512) x)))))
      (let ((palimpsest::*picture-file* (palimpsest::make-picture-file :kind :jpeg :tables (list nil nil table))))
        (flet ((score (grey)
                 ;; the furthest region of GREY, cut by 6 rows so that its grid begins at row 2
                 (let ((found (palimpsest::jpeg-traces nil nil nil (subseq grey (* 6 512)) 512 506 nil nil)))
                   (if found (palimpsest::trace-region-score (first found)) 0))))
          (let ((plain (score laid)) (turned (score turned)))
            (true (> turned (* 0.9 plain)) "turned ~,1f, unturned ~,1f" turned plain))))))
  ;; each kind is guarded apart: the JPEG traces failing leave paper painted over named, and the paper's grain
  ;; failing leaves the region laid in after a JPEG save named, each said not to have finished in its own words
  (flet ((failing (name)
           (sb-int:encapsulate name 'palimpsest-traces-fail
                               (lambda (measure &rest arguments)
                                 (declare (ignore measure arguments))
                                 (error "measuring failed"))))
         (findings (grey)
           (nth-value 2 (pal-read (pal-scanned grey)))))
    (failing 'palimpsest::jpeg-traces)
    (unwind-protect
         (let ((findings (findings (pal-painted (pal-jpeg-saved (pal-noisy-scan :sigma 2) 0 0 600 500 :quality 90)
                                                206 168 290 196 255))))
           (true (pal-finding findings :record "The check of the JPEG traces of page 1 could not finish"))
           (is = 1 (count-if (lambda (f) (search "could not finish" (palimpsest::finding-words f))) findings))
           (true (pal-finding findings :look "without the grain the rest of the page's paper has")))
      (sb-int:unencapsulate 'palimpsest::jpeg-traces 'palimpsest-traces-fail))
    (failing 'palimpsest::flat-regions)
    (unwind-protect
         (let ((findings (findings (pal-region-laid (pal-jpeg-saved (pal-noisy-scan :sigma 2) 0 0 600 500 :quality 90)
                                                    (pal-noisy-scan :sigma 2 :seed 99) 16 160 400 208))))
           (true (pal-finding findings :record "The check of the paper's grain of page 1 could not finish"))
           (true (pal-finding findings :look "the traces of the JPEG the rest of the page was once saved")))
      (sb-int:unencapsulate 'palimpsest::flat-regions 'palimpsest-traces-fail))))

(defun pal-jpeg-file (grey)
  "The grey picture GREY, 600 by 500 pixels, as a JPEG file of the highest quality made by ImageMagick; NIL when
it is not there."
  (when (find-if #'probe-file '("/usr/bin/convert" "/usr/bin/magick"))
    (with-test-directory (dir)
      (let ((pgm (namestring (merge-pathnames "page.pgm" dir))) (jpg (namestring (merge-pathnames "page.jpg" dir))))
        (with-open-file (out pgm :direction :output :element-type '(unsigned-byte 8))
          (write-sequence (pal-octets "P5 600 500 255 " grey) out))
        (ignore-errors
         (uiop:run-program (list (if (probe-file "/usr/bin/convert") "/usr/bin/convert" "/usr/bin/magick")
                                 pgm "-quality" "100" "-sampling-factor" "1x1" jpg))
         (alexandria:read-file-into-byte-vector jpg))))))

(define-test (palimpsest palimpsest-picture-file-drawn)
  ;; a picture file with a stroke copied: the page standing for it drawn with the signs framed by strength and the
  ;; key, in the report's directory, named after the picture, nothing written beside the picture; no object frames
  (let* ((png-page (pal-varied :ink 0))
         (jpeg-page (pal-varied :ink 0)))
    (pal-copy-box png-page png-page 114 58 233 172)
    ;; a word copied on whole JPEG blocks (8 by 8), as one saved once keeps it exactly
    (dotimes (y 24)
      (dotimes (x 80)
        (setf (aref jpeg-page (+ (* (+ 168 y) 600) 208 x)) (aref jpeg-page (+ (* (+ 56 y) 600) 112 x)))))
    (loop for (name octets) in (list (list "letter.png" (pal-png png-page 600 500))
                                     (list "letter.jpg" (pal-jpeg-file jpeg-page)))
          when octets
            do (with-test-directory (dir)
                 (let* ((analysis (pal-examined-in dir octets name))
                        (revision 0)
                        (target (namestring (merge-pathnames "report/" dir)))
                        (before (directory (merge-pathnames "evidence/*.*" dir))))
                   (true (palimpsest::analysis-picture analysis) "~a" name)
                   (setf target (palimpsest::path-join target (palimpsest::copy-target-name analysis nil revision)))
                   (is string= (format nil "~a-drawing.pdf" (subseq name 0 (position #\. name)))
                       (file-namestring target))
                   (let ((job (palimpsest::make-palimpsest-draw-job nil analysis revision target nil)))
                     (palimpsest::copy-work job)
                     (true (palimpsest::palimpsest-draw-job-done job) "~a" name))
                   (let ((text (map 'string #'code-char (alexandria:read-file-into-byte-vector target))))
                     ;; a JPEG's copy is told by its likeness: worth a look (blue), a PNG's by its pixels: strong
                     (pal-has text (if (search ".png" name) "q 0.902 0.000 0.000 RG 0.6 w [] 0 d" "RG 0.6 w [2 1] 0 d"))
                     (pal-has text "(copy) Tj")
                     (pal-has text "(A strong sign.) Tj")
                     (false (search "(4 picture)" text) "~a: no frame of its own for the picture" name))
                   ;; the page carries the picture itself, its samples, not a blank box
                   (let* ((copy (palimpsest::read-pdf (alexandria:read-file-into-byte-vector target) :name "copy.pdf"))
                          (last (palimpsest::last-revision copy))
                          (dict (palimpsest::pdf-indirect-get (palimpsest::revision-object copy last 4) :value)))
                     (is = 600 (palimpsest::pdf-get dict "/Width") "~a" name)
                     (is = 500 (palimpsest::pdf-get dict "/Height") "~a" name)
                     (true (< (palimpsest::pdf-get dict "/Length") 300000) "~a" name)
                     (is equal "/FlateDecode" (palimpsest::pdf-get dict "/Filter") "~a" name)
                     (multiple-value-bind (grey width) (palimpsest::picture-grey copy last 4)
                       (is eql 600 width)
                       (when (search ".png" name)
                         (true (equalp (pal-octets grey) (pal-octets (if (search ".png" name) png-page jpeg-page)))
                               "~a: its pixels, as the PNG has them" name))))
                   (true (search "/report/" target) "~a" target)
                   (is equal before (directory (merge-pathnames "evidence/*.*" dir)) "nothing beside ~a" name)
                   ;; a file changed since it was examined is not drawn from
                   (with-open-file (out (palimpsest::analysis-path analysis) :direction :output :if-exists :append
                                                                              :element-type '(unsigned-byte 8))
                     (write-byte 0 out))
                   (let ((job (palimpsest::make-palimpsest-draw-job nil analysis revision
                                                                    (concatenate 'string target "2") nil)))
                     (ignore-errors (palimpsest::copy-work job))
                     (false (palimpsest::palimpsest-draw-job-done job) "~a changed" name))))))
  ;; a command that needs revisions or objects is still refused for a picture, and drawing is not for a PDF's
  (let ((analysis (pal-examined (pal-png (pal-scan) 600 500) "page.png")))
    (true (palimpsest::picture-refused-p analysis "write"))
    (true (palimpsest::drawing-refused-p analysis t))
    (false (palimpsest::drawing-refused-p analysis nil)))
  (false (palimpsest::picture-refused-p (palimpsest::make-analysis :name "a.pdf") "write")))

(defun pal-sfnt-with-cmap (platform encoding)
  "The octets of a TrueType font file holding only a cmap table, whose one subtable is for PLATFORM and ENCODING."
  (let ((octets (list 0 1 0 0  0 1  0 16  0 0  0 0               ; version 1.0, one table
                      99 109 97 112  0 0 0 0  0 0 0 28  0 0 0 12 ; "cmap", its checksum, offset and length
                      0 0  0 1  0 platform  0 encoding  0 0 0 12)))
    (make-array (length octets) :element-type '(unsigned-byte 8) :initial-contents octets)))

(defun pal-mapped-font (encoding map &key (flags 32) (text "(Total 7 200)") program)
  "A one-page PDF showing TEXT in a font of ENCODING (its /Encoding value, as written) whose ToUnicode map is MAP,
the inside of a CMap's bfchar and bfrange sections, and whose descriptor's /Flags are FLAGS. With PROGRAM, the
octets of its font file, kept in the PDF as its /FontFile2."
  (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td ~a Tj ET" text)
                :fonts "/F1 7 0 R"
                :more (append (list (cons 7 (format nil "<< /Type /Font /Subtype /TrueType /BaseFont /Arial /Encoding ~
                                                         ~a /FontDescriptor 9 0 R /ToUnicode 8 0 R >>" encoding))
                                    (list 8 :stream "" (format nil "/CIDInit /ProcSet findresource begin begincmap 1 ~
                                                                    begincodespacerange <00> <FF> endcodespacerange ~
                                                                    ~a endcmap end" map))
                                    (cons 9 (format nil "<< /Type /FontDescriptor /FontName /Arial /Flags ~d~:[~; ~
                                                         /FontFile2 10 0 R~] >>" flags program)))
                              (and program (list (list 10 :stream "" program))))))

(define-test (palimpsest palimpsest-font-maps)
  ;; WinAnsi's seven copied as 1: named, with what the page shows and what copying gives
  (let* ((identity "1 beginbfrange <20> <7E> <0020> endbfrange")
         (findings (nth-value 2 (pal-read (pal-mapped-font "/WinAnsiEncoding"
                                                           (format nil "~a 1 beginbfchar <37> <0031> endbfchar"
                                                                   identity)))))
         (remapped (pal-finding findings :look "draws figures or letters that copy as others")))
    (true remapped "~s" (mapcar #'palimpsest::finding-words findings))
    (when remapped
      (pal-has (palimpsest::finding-words remapped)
               "shows \"Total 7 200\" where copying and searching give \"Total 1 200\"")
      (pal-has (first (palimpsest::finding-details remapped)) "code 55 draws 7 (WinAnsiEncoding) and copies as 1")
      (is eq :text-remapped (palimpsest::finding-method remapped))))
  ;; a glyph named by /Differences: the code of 1 drawing seven
  (let ((remapped (pal-finding (nth-value 2 (pal-read (pal-mapped-font "<< /Differences [49 /seven] >>"
                                                                       "1 beginbfrange <20> <7E> <0020> endbfrange"
                                                                       :text "(Total 1 200)")))
                               :look "draws figures or letters that copy as others")))
    (true remapped)
    (when remapped
      (pal-has (palimpsest::finding-words remapped)
               "shows \"Total 7 200\" where copying and searching give \"Total 1 200\"")
      (pal-has (first (palimpsest::finding-details remapped)) "code 49 draws 7 (its glyph seven) and copies as 1")))
  ;; an embedded subset with a cmap from Unicode draws by the names its encoding gives: named; one with only a cmap
  ;; of its own codes (platform 3, encoding 0) is drawn by its codes, its glyphs numbered afresh honestly: not named
  (let ((map "1 beginbfrange <20> <7E> <0020> endbfrange 1 beginbfchar <37> <0031> endbfchar"))
    (true (pal-finding (nth-value 2 (pal-read (pal-mapped-font "/WinAnsiEncoding" map
                                                               :program (pal-sfnt-with-cmap 3 1))))
                       :look "copy as others"))
    (false (pal-finding (nth-value 2 (pal-read (pal-mapped-font "/WinAnsiEncoding" map
                                                                :program (pal-sfnt-with-cmap 3 0))))
                        nil "copy as others")))
  ;; honest maps: identity, a ligature, a symbol in a private place, letters of another case, and a symbolic font
  ;; whose codes name its own glyphs whatever encoding it gives
  (dolist (case (list (list "/WinAnsiEncoding" "1 beginbfrange <20> <7E> <0020> endbfrange" 32)
                      (list "/WinAnsiEncoding" "3 beginbfchar <37> <00660066> <32> <E001> <30> <0041> endbfchar" 32)
                      (list "<< /Differences [55 /a] >>" "1 beginbfchar <37> <0041> endbfchar" 32)
                      (list "/WinAnsiEncoding" "1 beginbfchar <37> <0031> endbfchar" 4)))
    (destructuring-bind (encoding map flags) case
      (false (pal-finding (nth-value 2 (pal-read (pal-mapped-font encoding map :flags flags)))
                          nil "copy as others")
             "~s" case))))

;; numbers checked as their glyphs draw them, and the words of a run placed by their share of it
(define-test (palimpsest palimpsest-figures-as-drawn)
  ;; an organisation number copied as one whose check digit holds, drawn with a 4 for its last 3: checked as drawn
  (let* ((map "1 beginbfrange <20> <7E> <0020> endbfrange 1 beginbfchar <34> <0033> endbfchar")
         (findings (nth-value 2 (pal-read (pal-mapped-font "/WinAnsiEncoding" map :text "(Org.nr 556036-0794)"))))
         (found (pal-finding findings :look "whose check fails")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found) "the organisation number 556036-0794")))
  ;; a run whose characters' places are not known: one word or none, or its words shared out when asked
  (let ((run (palimpsest::make-mark :kind :text :text "ab cd")))
    (is equal '() (palimpsest::mark-placed-words run))
    (is equal '(("ab" 0 2) ("cd" 3 5)) (palimpsest::mark-placed-words run t))
    (is = 3/5 (palimpsest::mark-character-along run 3))
    (is = 1 (palimpsest::mark-character-along run 4 t))))

(defun pal-layered (grey &key (leave-out '()) mask)
  "A one-page PDF of the scanned page GREY (PAL-SCAN's 600 by 500 pixels, a pixel to a point) with an invisible text
layer over it as text recognition writes one: each of its 72 words where the word's ink is, but those whose
\(LINE WORD) is in LEAVE-OUT. With MASK, the picture is drawn through a soft mask of its own."
  (let ((layer (with-output-to-string (out)
                 (format out "BT 3 Tr /F1 20 Tf ")
                 (dotimes (line 12)
                   (dotimes (word 6)
                     (unless (member (list line word) leave-out :test #'equal)
                       (format out "1 0 0 1 ~d ~d Tm (Wd~2,'0d~2,'0dx) Tj "
                               (+ 19 (* word 95)) (- 500 (+ 20 (* line 38) 18)) line word))))
                 (format out "ET"))))
    (pal-save #() (append
                   (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                         '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                         (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] /Resources << ~
                                              /XObject << /Im1 7 0 R >> /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
                         (list 4 :stream "" (format nil "q 600 0 0 500 0 0 cm /Im1 Do Q ~a" layer))
                         '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /GlyphLessFont >>")
                         '(6 . "<< /Producer (Kea Scan) >>")
                         (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 600 /Height 500 ~
                                                      /ColorSpace /DeviceGray /BitsPerComponent 8~:[~; /SMask 8 0 R~]"
                                                 mask)
                               grey))
                   (and mask (list (list 8 :stream (format nil "/Type /XObject /Subtype /Image /Width 1 /Height 1 ~
                                                                /ColorSpace /DeviceGray /BitsPerComponent 8")
                                         (make-array 1 :element-type '(unsigned-byte 8) :initial-element 255))))))))

(define-test (palimpsest palimpsest-text-layer)
  ;; the layer where the ink is: nothing said of it but that it is there
  (let ((findings (nth-value 2 (pal-read (pal-layered (pal-scan))))))
    (false (pal-finding findings nil "disagrees with") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :record "invisible text over it")))
  ;; a word painted over with paper after the text was recognised: the layer's word named over blank paper
  (let* ((grey (pal-scan)))
    (loop for y from 56 below 80
          do (loop for x from 205 below 295 do (setf (aref grey (+ (* y 600) x)) 230)))
    (let ((found (pal-finding (nth-value 2 (pal-read (pal-layered grey))) :look "disagrees with")))
      (true found)
      (when found
        (pal-has (palimpsest::finding-words found) "1 word of the layer lies over blank paper (\"Wd0102x\")")
        (pal-has (first (palimpsest::finding-details found)) "\"Wd0102x\" at x 209")
        (is eq :scan-changed-after-ocr (palimpsest::finding-method found)))))
  ;; a word of ink the layer passes over, between two words it holds
  (let ((found (pal-finding (nth-value 2 (pal-read (pal-layered (pal-scan) :leave-out '((6 3))))) :look
                            "disagrees with")))
    (true found)
    (when found
      (pal-has (palimpsest::finding-words found) "1 word of ink between the layer's words has none of its own")))
  ;; a picture drawn through a mask of its own may keep its text's ink in the mask: not judged, and said so
  (let* ((grey (pal-scan)))
    (loop for y from 56 below 80
          do (loop for x from 205 below 295 do (setf (aref grey (+ (* y 600) x)) 230)))
    (let ((findings (nth-value 2 (pal-read (pal-layered grey :mask t)))))
      (false (pal-finding findings nil "disagrees with") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :record "was not set against"))))
  ;; a word at a line's end the layer passes over is what text recognition leaves out honestly
  (false (pal-finding (nth-value 2 (pal-read (pal-layered (pal-scan) :leave-out '((6 5))))) nil "disagrees with")))

;;; Dates in the text against dates in the file

(defun pal-dated-page (lines &key (info "/CreationDate (D:20260301090000+01'00')") (more '()))
  "A one-page PDF writing LINES in Helvetica from the top, its document information INFO, MORE objects beside."
  (pal-page-of "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R"
               (format nil "~{~a~^~%~}" (loop for line in lines for y downfrom 700 by 16
                                              collect (format nil "BT /F1 11 Tf 72 ~d Td (~a) Tj ET" y line)))
               :more (cons (cons 6 (format nil "<< /Producer (Kea Writer) ~a >>" info)) more)))

(defun pal-exif-jpeg (made)
  "The octets of a JPEG holding only an EXIF that says it was made at MADE (as EXIF writes a date, 19
characters), with no picture after it."
  (let ((tiff (pal-octets "MM" 0 42 0 0 0 8
                          ;; the first directory: one entry, a pointer to the EXIF directory at 26
                          0 1 #x87 #x69 0 4 0 0 0 1 0 0 0 26 0 0 0 0
                          ;; the EXIF directory: DateTimeOriginal, 20 characters at 44
                          0 1 #x90 #x03 0 2 0 0 0 20 0 0 0 44 0 0 0 0
                          made 0)))
    (pal-octets #xff #xd8 #xff #xe1 0 (+ 2 6 (length tiff)) "Exif" 0 0 tiff #xff #xd9)))

(defun pal-scanned-dated (line made &key (info "/CreationDate (D:20260320090000+01'00')"))
  "A one-page PDF that is one JPEG picture whose EXIF says it was made at MADE, LINE over it as invisible
text, as text recognition leaves it, its document information INFO."
  (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /XObject << /Im1 7 0 R >> >> ~
                           /Contents 4 0 R")
               (format nil "q 612 0 0 792 0 0 cm /Im1 Do Q BT 3 Tr /F1 11 Tf 72 700 Td (~a) Tj ET" line)
               :more (list (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 10 /Height 10 ~
                                                         /ColorSpace /DeviceGray /BitsPerComponent 8 ~
                                                         /Filter /DCTDecode")
                                 (pal-exif-jpeg made))
                           (cons 6 (format nil "<< /Producer (Kea Scan) ~a >>" info)))))

(define-test (palimpsest palimpsest-text-dates)
  ;; dates as pages write them: each form read, a day and month that could be swapped refused
  (flet ((day (text) (let ((d (first (palimpsest::text-dates-in text)))) (and d (palimpsest::day-words (first d))))))
    (dolist (text '("2024-03-15" "15 mars 2024" "15th March 2024" "15. mars 2024" "March 15, 2024" "Mar. 15 2024"
                    "15/3/2024" "15.03.2024" "15-03-2024" "3/15/2024" "den 15 mars 2024" "2024-03-15T10:00"))
      (is equal "2024-03-15" (day text) "~s" text))
    (is equal "2024-03-03" (day "3.3.2024"))
    ;; swappable, two-digit years, impossible days, dates run on into more figures, ranges
    (dolist (text '("05/03/2024" "5.3.2024" "15/03/24" "31.04.2024" "2024-02-30" "1.15.03.2024" "15.03.2024.7"
                    "3.15.2024" "2024-03-01 – 2024-03-31" "1 mars till 31 mars 2024" "15 mars" "Mars 2024"
                    "1979-03-15" "120240315" "2024-03-15T" "2024-03-15Tx"))
      (false (day text) "~s" text)))
  ;; the document's own dates by their labels, and those that are not
  (flet ((own (text) (mapcar (lambda (d) (palimpsest::day-words (first d))) (palimpsest::own-dates-in text))))
    (is equal '("2024-03-15") (own "Fakturadatum 2024-03-15	Förfallodatum 2024-04-14"))
    (is equal '("2024-03-15") (own "Datum: 15.03.2024"))
    (is equal '("2024-03-15") (own "Invoice date: March 15, 2024"))
    (is equal '("2024-03-15") (own "Utfärdad den 15 mars 2024"))
    (is equal '("2024-03-15") (own "Date: 2024-03-15T10:00:00"))
    ;; text recognition joining a label to its date
    (is equal '("2025-03-10") (own "FOI	Datum2025-03-10	Nr F01-2024-1929"))
    ;; a row of labels over a row of values: the date under its label
    (is equal (list (list (tuatara:day-number 2015 9 22) "2015-09-22" t t))
        (palimpsest::own-dates-in "Datum	Diarienummer"
                                  (lambda (at) (declare (ignore at)) (values "2015-09-22	1914/2015" 0))))
    (false (palimpsest::own-dates-in "Datum	Diarienummer"
                                     (lambda (at) (declare (ignore at)) (values "1914/2015	2015-09-22" 0))))
    (false (own "Ert datum 2018-07-03"))
    (dolist (text '("Förfallodatum 2024-04-14" "Due date: 15 March 2024" "Delivery date 2024-03-15"
                    "Datum för leverans 2024-03-15" "Datum 2024-03-01 - 2024-03-31" "Period 2024-03-01"
                    "Datum 05/03/2024" "Betala senast 2024-03-31"))
      (false (own text) "~s" text)))
  ;; a letter's date line, and lines that are not one
  (is eql (tuatara:day-number 2024 3 15) (palimpsest::date-line-day "Stockholm den 15 mars 2024"))
  (is eql (tuatara:day-number 2024 3 15) (palimpsest::date-line-day "London, 15 March 2024"))
  (false (palimpsest::date-line-day "Förfallodag 15 mars 2024"))
  (false (palimpsest::date-line-day "15 mars 2024" "Förfallodatum"))
  (false (palimpsest::date-line-day "2024-03-15"))
  ;; an invoice dated after every date the file gives: worth a look, the date and its line named
  (let* ((findings (nth-value 2 (pal-read (pal-dated-page '("Kea Bygg AB" "Fakturadatum 2026-03-15")))))
         (found (pal-finding findings :look "after every date the file gives")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found) "Page 1 is dated 2026-03-15 (\"2026-03-15\")")
      (pal-has (palimpsest::finding-words found) "/CreationDate 2026-03-01 09:00:00 +01:00")
      (is eq :text-dated-later (palimpsest::finding-method found))
      (is eql 1 (palimpsest::finding-page found))
      (pal-has (first (palimpsest::finding-details found)) "Fakturadatum 2026-03-15")))
  ;; the same by a label that says only that a date follows, as agendas, invitations and tickets are dated ahead:
  ;; for the record; given by a stronger label too, worth a look
  (let ((findings (nth-value 2 (pal-read (pal-dated-page '("Kea Bygg AB" "Date: 2026-03-15"))))))
    (false (pal-finding findings :look "is dated") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :record "by a label that says only that a date follows")))
  (true (pal-finding (nth-value 2 (pal-read (pal-dated-page '("Datum 2026-03-15" "Fakturadatum 2026-03-15")))) :look
                     "is dated"))
  ;; a label ending a line, its date under it; and a letter's date line
  (true (pal-finding (nth-value 2 (pal-read (pal-dated-page '("Fakturadatum" "2026-03-15")))) :look "is dated"))
  (true (pal-finding (nth-value 2 (pal-read (pal-dated-page '("Kea Bygg AB" "Stockholm den 15 mars 2026")))) :look
                     "is dated"))
  ;; honest pages: dated the day the file was made, the next day where a zone ahead already had it, a due date,
  ;; a period, a date that may be read two ways, a file that gives no date, a file whose date is a clock never set
  (dolist (case '((("Fakturadatum 2026-03-01" "Förfallodatum 2026-03-31") "/CreationDate (D:20260301090000+01'00')")
                  (("Fakturadatum 2026-03-02") "/CreationDate (D:20260301230000Z)")
                  (("Invoice date 2026-03-02") "/CreationDate (D:20260301110000)")
                  (("Due date: 15 March 2026" "Period 2026-04-01 - 2026-04-30") "/CreationDate (D:20260301090000Z)")
                  (("Datum 05/04/2026") "/CreationDate (D:20260301090000Z)")
                  (("Fakturadatum 2026-03-15") "")
                  (("Fakturadatum 2026-03-15") "/CreationDate (D:19700101000000Z)")))
    (let ((findings (nth-value 2 (pal-read (pal-dated-page (first case) :info (second case))))))
      (false (pal-finding findings nil "is dated") "~s ~s" case (mapcar #'palimpsest::finding-words findings))))
  ;; dated after the file was made but not after its last change: for the record
  (let ((found (pal-finding (nth-value 2 (pal-read (pal-dated-page '("Datum: 15 mars 2026")
                                                                   :info (concatenate
                                                                           'string "/CreationDate (D:20260301090000Z) "
                                                                           "/ModDate (D:20260320090000Z)"))))
                            nil "is dated")))
    (true found)
    (when found
      (is eq :record (palimpsest::finding-strength found))
      (pal-has (palimpsest::finding-words found) "but not after its last change")))
  ;; a scan's text recognition: after the file, for the record; after the scanner's own date, for the record
  (let ((findings (nth-value 2 (pal-read (pal-scanned-dated "Fakturadatum 2026-03-25" "2026:03:24 10:00:00")))))
    (let ((found (pal-finding findings nil "as its text recognition reads it, after every date")))
      (true found "~s" (mapcar #'palimpsest::finding-words findings))
      (when found (is eq :record (palimpsest::finding-strength found))))
    (false (pal-finding findings :look "is dated")))
  (let* ((findings (nth-value 2 (pal-read (pal-scanned-dated "Fakturadatum 2026-03-10" "2026:03:05 10:00:00"))))
         (found (pal-finding findings nil "after its scanned picture says it was made")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (is eq :record (palimpsest::finding-strength found))
      (pal-has (palimpsest::finding-words found) "DateTimeOriginal 2026:03:05 10:00:00")))
  (false (pal-finding (nth-value 2 (pal-read (pal-scanned-dated "Fakturadatum 2026-03-05" "2026:03:05 10:00:00")))
                      nil "is dated"))
  ;; a picture made after every date the file gives: for the record; one made the same day in another zone is not
  (let ((found (pal-finding (nth-value 2 (pal-read (pal-scanned-dated "Kea" "2026:03:25 10:00:00")))
                            nil "after every date the file gives (the latest")))
    (true found)
    (when found (is eq :record (palimpsest::finding-strength found))))
  (false (pal-finding (nth-value 2 (pal-read (pal-scanned-dated "Kea" "2026:03:20 20:00:00"))) nil
                      "after every date the file gives (the latest")))

(define-test (palimpsest palimpsest-text-dates-long-line)
  ;; a line of thousands of labelled dates, ranges or dates under labels is read in one pass over it, not once
  ;; for each label
  (flet ((repeated (text n) (with-output-to-string (out) (dotimes (i n) (write-string text out))))
         (seconds (thunk)
           (let ((begun (get-internal-real-time)))
             (values (funcall thunk) (/ (- (get-internal-real-time) begun) internal-time-units-per-second)))))
    (multiple-value-bind (dates took)
        (seconds (lambda () (palimpsest::own-dates-in (repeated "Datum 2024-03-15 " 20000))))
      (is = 20000 (length dates))
      (true (< took 2) "~,2f s" took))
    (multiple-value-bind (dates took)
        (seconds (lambda () (palimpsest::text-dates-in (repeated "2024-03-01 - 2024-03-31 " 10000))))
      (is equal '() dates)
      (true (< took 2) "~,2f s" took))
    ;; a row of 20,000 labels over a row of as many dates, each date under its label
    (let* ((n 20000)
           (runs (make-array (* 6 n))) (next-runs (make-array (* 11 n))))
      (dotimes (k n)
        (let ((label (palimpsest::make-mark :kind :text :text "Datum" :x0 (* 100 k) :x1 (+ (* 100 k) 30) :size 11))
              (date (palimpsest::make-mark :kind :text :text "2024-03-15" :x0 (* 100 k) :x1 (+ (* 100 k) 55)
                                           :size 11)))
          (fill runs label :start (* 6 k) :end (+ (* 6 k) 5))
          (setf (aref runs (+ (* 6 k) 5)) (cons 70 11))
          (fill next-runs date :start (* 11 k) :end (+ (* 11 k) 10))
          (setf (aref next-runs (+ (* 11 k) 10)) (cons 45 11))))
      (multiple-value-bind (dates took)
          (seconds (lambda () (palimpsest::own-dates-in (repeated "Datum	" n)
                                                        (palimpsest::text-below runs (repeated "2024-03-15 " n)
                                                                                next-runs))))
        (is = n (length dates))
        (true (every #'third dates))
        (true (< took 2) "~,2f s" took)))))

(define-test (palimpsest palimpsest-file-date-order)
  ;; a later save giving another creation date, and a last change before the save before's: for the record
  (let* ((first (pal-save #() (append (butlast *pal-original-objects*)
                                      (list (cons 6 (concatenate 'string "<< /CreationDate (D:20260101090000Z) "
                                                                         "/ModDate (D:20260105090000Z) >>"))))))
         (second (pal-save first '((6 . "<< /CreationDate (D:20260103090000Z) /ModDate (D:20260102090000Z) >>"))))
         (findings (nth-value 2 (pal-read second))))
    (let ((found (pal-finding findings nil "gives another creation date than the save before")))
      (true found "~s" (mapcar #'palimpsest::finding-words findings))
      (when found (is eq :record (palimpsest::finding-strength found))))
    (let ((found (pal-finding findings nil "before the save before it")))
      (true found)
      (when found (is eq :record (palimpsest::finding-strength found)))))
  ;; saves in order, the creation date kept in another zone: nothing said
  (let* ((first (pal-save #() (append (butlast *pal-original-objects*)
                                      (list (cons 6 (concatenate 'string "<< /CreationDate (D:20260101090000Z) "
                                                                         "/ModDate (D:20260101090000Z) >>"))))))
         (second (pal-save first '((6 . "<< /CreationDate (D:20260101100000+01'00') /ModDate (D:20260102090000Z) >>"))))
         (findings (nth-value 2 (pal-read second))))
    (false (pal-finding findings nil "creation date than the save before") "~s"
           (mapcar #'palimpsest::finding-words findings))
    (false (pal-finding findings nil "before the save before it")))
  ;; the XMP's metadata changed before the file was made
  (let* ((xmp (format nil "<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF ~
                           xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description ~
                           xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmp:CreateDate='2026-03-01T09:00:00Z' ~
                           xmp:ModifyDate='2026-03-01T09:00:00Z' xmp:MetadataDate='2026-02-01T09:00:00Z'/></rdf:RDF>~
                           </x:xmpmeta>"))
         (findings (nth-value 2 (pal-read (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                                             '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                                             (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))))
         (found (pal-finding findings nil "its metadata was last changed")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found (is eq :record (palimpsest::finding-strength found)))))

;;; Signatures and stamps the same in two places

(defun pal-gaussian (random sigma)
  "A number drawn from a normal distribution of spread SIGMA about 0, from the state RANDOM."
  (* sigma (sqrt (* -2 (log (- 1 (random 1d0 random))))) (cos (* 2 pi (random 1d0 random)))))

(defun pal-signed-page (&key (seed 7) signed (at '(300 340)) (phase '(0 0)) (turn 0) (sigma 2))
  "A scanned page, 600 by 500 grey pixels, of 7 lines of words as PAL-NOISY-SCAN draws them (grain SIGMA,
seeded by SEED), the rest of it grained paper. With SIGNED, a signature written on it, a looped stroke of
about 170 by 80 pixels whose box begins at AT (X Y), traced finely and laid on the paper as ink of grey 30
covering each pixel as much as the stroke does, with grain of its own. PHASE (DX DY) moves the stroke by
parts of a pixel and TURN turns it, in radians: one print of a signature scanned again, its pixels falling
elsewhere on its strokes."
  (let ((grey (pal-noisy-scan :sigma sigma :seed seed)) (random (sb-ext:seed-random-state (+ seed 1000))))
    (loop for i from (* 600 254) below (length grey)
          do (setf (aref grey i) (max 0 (min 255 (round (+ 235 (pal-gaussian random sigma)))))))
    (when signed
      (let ((cover (make-array (* 200 100) :element-type 'double-float :initial-element 0d0)) (c (cos turn))
            (s (sin turn)))
        (dotimes (k 4000)
          (let* ((u (/ k 4000d0))
                 (x (+ 10 (* 170 u) (* 8 (sin (* 31 u))) (first phase)))
                 (y (+ 50 (* 30 (sin (* 13 u))) (* 10 (cos (* 7 u))) (second phase)))
                 (rx (+ 100 (- (* c (- x 100)) (* s (- y 50))))) (ry (+ 50 (* s (- x 100)) (* c (- y 50)))))
            (loop for dy from -6 to 6
                  do (loop for dx from -6 to 6
                           do (when (<= (+ (* dx dx) (* dy dy)) 36)
                                (let ((i (+ (* (floor (+ ry (/ dy 4d0))) 200) (floor (+ rx (/ dx 4d0))))))
                                  (when (< -1 i (length cover))
                                    (setf (aref cover i) (min 1d0 (+ (aref cover i) 0.01d0))))))))))
        (destructuring-bind (sx sy) at
          (dotimes (y 100)
            (dotimes (x 200)
              (let ((v (aref cover (+ (* y 200) x))) (i (+ (* (+ sy y) 600) sx x)))
                (when (plusp v)
                  (setf (aref grey i) (max 0 (min 255 (round (+ (* (aref grey i) (- 1 v)) (* 30 v)
                                                                 (* v (pal-gaussian random sigma))))))))))))))
    grey))

(defun pal-signature-laid (to from fx fy tx ty &key (w 210) (h 110) darken)
  "TO, a scanned page (600 by 500), with the W by H pixels of FROM at FX FY laid on it at TX TY: copied
whole, paper and all, or with DARKEN only where they are darker, as a picture editor's darken mode lays a
signature cut out. TO, changed."
  (dotimes (y h to)
    (dotimes (x w)
      (let ((s (aref from (+ (* (+ fy y) 600) fx x))) (d (+ (* (+ ty y) 600) tx x)))
        (setf (aref to d) (if darken (min s (aref to d)) s))))))

(defun pal-repeats (greys)
  "The findings of marks the same in two places in the scanned pages GREYS (PAL-SCANNED-PAGES)."
  (let ((pdf (palimpsest::read-pdf (pal-scanned-pages greys) :name "test.pdf")))
    (palimpsest::check-repeats pdf (palimpsest::last-revision pdf))))

(defun pal-page-marks (grey &optional lossy)
  "The marks as large as a signature or a stamp of the scanned page GREY (600 by 500), read as its scan
reading reads it, LOSSY saying whether it is kept as a JPEG (SCAN-INK-MARKS)."
  (let ((reading (palimpsest::picture-words grey 600 500)))
    (palimpsest::scan-ink-marks grey 600 500 (palimpsest::page-level grey 600 500)
                                (palimpsest::picture-reading-words reading) (palimpsest::picture-reading-glyphs reading)
                                lossy)))

(define-test (palimpsest palimpsest-signature-copied-in-scans)
  ;; a signature cut from one scanned page and laid on another, whole or in darken mode, or twice on one page:
  ;; worth a look, both places named
  (let ((signed (pal-signed-page :signed t)))
    (loop for (label greys) in (list (list "laid whole" (list signed (pal-signature-laid (pal-signed-page :seed 8) signed
                                                                               295 335 300 340)))
                                     (list "darkened" (list signed (pal-signature-laid (pal-signed-page :seed 8) signed
                                                                             295 335 250 330 :darken t)))
                                     (list "on one page" (list (pal-signature-laid (copy-seq signed) signed 295 335 40 300))))
          do (let ((same (find :look (pal-repeats greys) :key #'palimpsest::finding-strength)))
               (true same label)
               (when same
                 (is eq :picture-pasted (palimpsest::finding-method same))
                 (pal-has (palimpsest::finding-words same) "1 pair of marks")
                 (pal-has (first (palimpsest::finding-details same)) "the same grey in every pixel of their ink")
                 (pal-has (first (palimpsest::finding-details same)) "pixels 309,350 to 478,429")))))
  ;; one print scanned twice, its pixels falling half a pixel apart or turned half a degree; one print at the
  ;; same place to the pixel, the scanner's grain alone apart; the same page kept twice, a grey lighter: nothing
  (let ((signed (pal-signed-page :signed t)))
    (loop for (label other) in (list (list "half a pixel" (pal-signed-page :seed 8 :signed t :phase '(0.5 0.3)))
                                     (list "turned" (pal-signed-page :seed 8 :signed t :turn 0.01))
                                     (list "grain alone" (pal-signed-page :seed 8 :signed t))
                                     (list "kept twice" (map '(simple-array (unsigned-byte 8) (*))
                                                             (lambda (g) (min 255 (1+ g))) signed)))
          do (false (pal-repeats (list signed other)) label)))
  ;; a mark copied on one page into the same row or the same column, as a form prints its boxes: not named
  (let ((signed (pal-signed-page :signed t)))
    (false (pal-repeats (list (pal-signature-laid (copy-seq signed) signed 295 335 40 335))) "one row")
    (false (pal-repeats (list (pal-signature-laid (copy-seq signed) signed 295 335 295 130))) "one column")))

(define-test (palimpsest palimpsest-signature-copied-after-a-jpeg)
  ;; both pages saved as JPEGs after a signature was copied, at quality 75: the same; one print scanned
  ;; twice a fifth of a pixel apart or turned a quarter of a degree, then saved so: not
  (let* ((signed (pal-signed-page :signed t))
         (copied (pal-signature-laid (pal-signed-page :seed 8) signed 295 335 253 331 :darken t)))
    (flet ((same (a b quality)
             (let ((one (first (pal-page-marks (pal-jpeg-saved (copy-seq a) 0 0 600 500 :quality quality) t)))
                   (two (first (pal-page-marks (pal-jpeg-saved (copy-seq b) 0 0 600 500 :quality quality) t))))
               (and one two (palimpsest::same-mark-p one two)))))
      (true (same signed copied 75))
      ;; on pages whose grain was not judged, prints alike for want of it, only the same in every pixel is a copy
      (let ((one (first (pal-page-marks (pal-jpeg-saved (copy-seq signed) 0 0 600 500 :quality 75) t)))
            (two (first (pal-page-marks (pal-jpeg-saved (copy-seq copied) 0 0 600 500 :quality 75) t)))
            (faint (palimpsest::make-scan-picture :judged nil)))
        (setf (palimpsest::ink-mark-scan one) faint (palimpsest::ink-mark-scan two) faint)
        (false (palimpsest::same-mark-p one two)))
      ;; at quality 50 the copy strays by 21 grey levels, past the rule: missed
      (false (same signed copied 50))
      (false (same signed (pal-signed-page :seed 8 :signed t :phase '(0.2 0.1)) 75))
      (false (same signed (pal-signed-page :seed 8 :signed t :turn 0.004) 75))
      ;; kept without loss the rule is strict: the same print at the same place, the grain apart, is no copy
      (let ((one (first (pal-page-marks signed))) (two (first (pal-page-marks (pal-signed-page :seed 8 :signed t)))))
        (false (palimpsest::same-mark-p one two))))))

(define-test (palimpsest palimpsest-marks-bounded)
  ;; the words and the rules of a page are no marks; the marks' pixels kept are bounded, and so are the pairs
  ;; laid over each other
  (is equal '() (pal-page-marks (pal-signed-page)))
  (let ((ruled (pal-signed-page)))
    (loop for x from 20 below 580 do (loop for y from 300 below 303 do (setf (aref ruled (+ (* y 600) x)) 40)))
    (is equal '() (pal-page-marks ruled)))
  (is = 1 (length (pal-page-marks (pal-signed-page :signed t))))
  (let ((palimpsest::*marks-pixels-in-all* 1000))
    (let* ((pdf (palimpsest::read-pdf (pal-scanned-pages (list (pal-signed-page :signed t))) :name "test.pdf"))
           (scans (palimpsest::scan-reading-scans (palimpsest::scans-read pdf (palimpsest::last-revision pdf)))))
      (is eq :let-go (palimpsest::scan-picture-marks (first scans)))))
  (let* ((signed (pal-signed-page :signed t))
         (marks (append (pal-page-marks signed)
                        (pal-page-marks (pal-signature-laid (pal-signed-page :seed 8) signed 295 335 300 340))
                        (pal-page-marks (pal-signature-laid (pal-signed-page :seed 9) signed 295 335 200 300))))
         (palimpsest::*mark-tries-most* 1))
    (multiple-value-bind (pairs left) (palimpsest::alike-marks marks (constantly t))
      (is = 1 (length pairs))
      (is = 2 left))))

(defun pal-picture-pages (pages pictures)
  "A PDF of PAGES pages (612 by 792 points), each drawing pictures: PICTURES is a list of (OBJECT PAGE X Y W
H OCTETS), the picture object OBJECT, 64 by 32 grey pixels of OCTETS, drawn on PAGE (from 0) at X Y, W by H
points. An object named again draws the picture it was given first."
  (let ((objects (remove-duplicates pictures :key #'first :from-end t)))
    (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                (cons 2 (format nil "<< /Type /Pages /Kids [~{~d 0 R~^ ~}] /Count ~d >>"
                                                (loop for k below pages collect (+ 100 (* 2 k))) pages))
                                '(6 . "<< /Producer (Kea Writer) >>"))
                          (loop for k below pages
                                for here = (remove k pictures :key #'second :test-not #'=)
                                append (list (cons (+ 100 (* 2 k))
                                                   (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 ~
                                                                792] /Resources << /XObject << ~{/Im~d ~:*~d 0 R ~}~
                                                                >> >> /Contents ~d 0 R >>"
                                                           (remove-duplicates (mapcar #'first here)) (+ 101 (* 2 k))))
                                             (list (+ 101 (* 2 k)) :stream ""
                                                   (format nil "~:{q ~d 0 0 ~d ~d ~d cm /Im~d Do Q ~}"
                                                           (loop for (object nil x y w h) in here
                                                                 collect (list w h x y object))))))
                          (loop for (object nil nil nil nil nil octets) in objects
                                collect (list object :stream (format nil "/Type /XObject /Subtype /Image /Width 64 ~
                                                                          /Height 32 /ColorSpace /DeviceGray ~
                                                                          /BitsPerComponent 8")
                                              octets))))))

(defun pal-signature-picture (seed)
  "64 by 32 grey octets, a signature kept as a picture, told apart by SEED."
  (let ((out (make-array (* 64 32) :element-type '(unsigned-byte 8) :initial-element 255)))
    (dotimes (x 64 out)
      (setf (aref out (+ (* (+ 16 (round (* 10 (sin (/ (+ x seed) 5))))) 64) x)) 0))))

(define-test (palimpsest palimpsest-pictures-repeated)
  ;; a signature kept as a picture drawn on two pages, by one object or by two of the same octets: for the
  ;; record, with each place
  (let ((one (pal-signature-picture 1)))
    (loop for (label pictures detail)
            in (list (list "one object" (list (list 20 0 72 100 120 60 one) (list 20 1 300 100 120 60 one))
                           "Im20 (object 20), 64 by 32 pixels: page 1 at x 72, y 100; page 2 at x 300, y 100")
                     (list "two objects" (list (list 20 0 72 100 120 60 one) (list 21 1 72 100 120 60 one))
                           "kept as 2 objects, 20, 21"))
          do (let ((found (pal-finding (nth-value 2 (pal-read (pal-picture-pages 2 pictures))) :record
                                       "each drawn in more than one place")))
               (true found label)
               (when found
                 (is eq :picture-pasted (palimpsest::finding-method found))
                 (pal-has (first (palimpsest::finding-details found)) detail)))))
  ;; two signatures that differ, a picture drawn once, and one too small to be a signature: nothing
  (loop for pictures in (list (list (list 20 0 72 100 120 60 (pal-signature-picture 1))
                                    (list 21 1 72 100 120 60 (pal-signature-picture 2)))
                              (list (list 20 0 72 100 120 60 (pal-signature-picture 1)))
                              (list (list 20 0 72 100 10 10 (pal-signature-picture 1))
                                    (list 20 1 72 100 10 10 (pal-signature-picture 1))))
        do (false (pal-finding (nth-value 2 (pal-read (pal-picture-pages 2 pictures))) nil
                               "drawn in more than one place"))))

(define-test (palimpsest palimpsest-files-compared)
  ;; three PDFs of a directory: a signature picture in two of them and a scanned signature copied between two;
  ;; listed, each with its file; the third shares nothing
  (with-test-directory (dir)
    (let ((signed (pal-signed-page :signed t)))
      (flet ((put (name octets)
               (let ((path (namestring (merge-pathnames name dir))))
                 (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
                   (write-sequence octets out))
                 path)))
        (put "a.pdf" (pal-picture-pages 1 (list (list 20 0 72 100 120 60 (pal-signature-picture 1)))))
        (put "b.PDF" (pal-picture-pages 1 (list (list 30 0 300 400 120 60 (pal-signature-picture 1)))))
        (put "c.pdf" (pal-scanned-pages (list signed)))
        (put "d.pdf" (pal-scanned-pages (list (pal-signature-laid (pal-signed-page :seed 8) signed 295 335 300 340))))
        (put "e.pdf" (pal-picture-pages 1 (list (list 20 0 72 100 120 60 (pal-signature-picture 3)))))
        (put "notes.txt" (pal-octets "not a PDF"))
        (let ((paths (palimpsest::directory-pdfs (namestring dir))))
          (is equal '("a.pdf" "b.PDF" "c.pdf" "d.pdf" "e.pdf") (mapcar #'file-namestring paths))
          (multiple-value-bind (files pictures pairs) (palimpsest::compared-files paths)
            (is = 5 (length files))
            (is = 1 (length pictures))
            (is equal '("a.pdf" "b.PDF")
                (mapcar (lambda (held) (palimpsest::compared-file-name (first held))) (first pictures)))
            (is = 1 (length pairs))
            (when pairs
              (pal-has (palimpsest::mark-pair-words (first pairs)) "c.pdf, page 1")
              (pal-has (palimpsest::mark-pair-words (first pairs)) "d.pdf, page 1"))
            (let ((text (palimpsest::pieces-text
                         (palimpsest::compared-pieces (namestring dir) files pictures pairs 0))))
              (pal-has text "5 PDFs: 1 picture in more than one, 1 pair of marks")
              (pal-has text "In 2 files")))
          ;; as the command does it, each PDF read shut in, one to a Tuatara: the same list as in this process
          (flet ((listed (shut-in)
                   (let ((job (palimpsest::make-palimpsest-compare-job nil (namestring dir)))
                         (palimpsest::*palimpsest-shut-in* shut-in))
                     (palimpsest::compare-work job (namestring dir))
                     (list (palimpsest::palimpsest-compare-job-found job)
                           (first (palimpsest::palimpsest-compare-job-shown job))
                           (length (third (palimpsest::palimpsest-compare-job-shown job)))))))
            (let ((shut (listed t)))
              (is equal '(5 1 1) (first shut))
              (pal-has (second shut) "c.pdf, page 1")
              (is equal (listed nil) shut))))))))

(define-test (palimpsest palimpsest-marks-work-bounded)
  ;; the pixels read laying marks over each other are bounded, the pairs past the bound counted, and a file
  ;; not wholly searched said so for the record; so are the marks set beside each by their coarse parts
  (let* ((signed (pal-signed-page :signed t))
         (copied (pal-signature-laid (pal-signed-page :seed 8) signed 295 335 300 340))
         (third (pal-signature-laid (pal-signed-page :seed 9) signed 295 335 200 300))
         (marks (append (pal-page-marks signed) (pal-page-marks copied) (pal-page-marks third))))
    (let ((palimpsest::*mark-pixels-read-most* 1))
      (multiple-value-bind (pairs left) (palimpsest::alike-marks marks (constantly t))
        (is = 0 (length pairs))
        (is = 3 left))
      (let ((findings (pal-repeats (list signed copied))))
        (false (find :look findings :key #'palimpsest::finding-strength))
        (true (find-if (lambda (f) (search "not wholly searched" (palimpsest::finding-words f))) findings))))
    (let ((palimpsest::*mark-candidates-most* 1))
      (multiple-value-bind (pairs left) (palimpsest::alike-marks marks (constantly t))
        (true (< (length pairs) 3))
        (true (plusp left))))))

(define-test (palimpsest palimpsest-marks-let-go-said)
  ;; marks let go past the pixels a file's marks may keep: said for the record, with the pages
  (let ((palimpsest::*marks-pixels-in-all* 1000))
    (let ((let-go (find-if (lambda (f) (search "were not set beside the others" (palimpsest::finding-words f)))
                           (pal-repeats (list (pal-signed-page :signed t))))))
      (true let-go)
      (when let-go (pal-has (palimpsest::finding-words let-go) "1 scanned page (1)")))))

(define-test (palimpsest palimpsest-marks-of-a-twin-not-read)
  ;; a scanned picture kept twice the same octet for octet: its marks found once, not read and let go again
  (let ((calls 0) (signed (pal-signed-page :signed t)))
    (sb-int:encapsulate 'palimpsest::scan-ink-marks 'palimpsest-count-marks
                        (lambda (f &rest arguments) (incf calls) (apply f arguments)))
    (unwind-protect (pal-repeats (list signed (copy-seq signed)))
      (sb-int:unencapsulate 'palimpsest::scan-ink-marks 'palimpsest-count-marks))
    (is = 1 calls)))

(define-test (palimpsest palimpsest-picture-identity)
  ;; two objects of the same octets draw the same; another picture, or the same octets drawn another way, not
  (let* ((one (pal-signature-picture 1))
         (pdf (palimpsest::read-pdf (pal-picture-pages 1 (list (list 20 0 72 100 120 60 one)
                                                               (list 21 0 300 100 120 60 one)
                                                               (list 22 0 72 400 120 60 (pal-signature-picture 2))))
                                    :name "test.pdf"))
         (revision (palimpsest::last-revision pdf)))
    (is equal (palimpsest::picture-identity pdf revision 20) (palimpsest::picture-identity pdf revision 21))
    (isnt equal (palimpsest::picture-identity pdf revision 20) (palimpsest::picture-identity pdf revision 22))
    (is = 1 (length (palimpsest::repeated-pictures pdf revision)))))

(define-test (palimpsest palimpsest-files-compared-apart)
  ;; a file that is no PDF named .pdf: listed as not wholly compared; marks past the room of all files: not
  ;; kept, and said; two letters signed by different hands and with different pictures: nothing shared
  (with-test-directory (dir)
    (flet ((put (name octets)
             (let ((path (namestring (merge-pathnames name dir))))
               (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
                 (write-sequence octets out))
               path)))
      (let* ((a (put "a.pdf" (pal-scanned-pages (list (pal-signed-page :signed t)))))
             (b (put "b.pdf" (pal-scanned-pages (list (pal-signed-page :seed 8 :signed t :phase '(0.5 0.3))))))
             (c (put "c.pdf" (pal-picture-pages 1 (list (list 20 0 72 100 120 60 (pal-signature-picture 1))))))
             (d (put "d.pdf" (pal-picture-pages 1 (list (list 20 0 72 100 120 60 (pal-signature-picture 2))))))
             (broken (put "broken.pdf" (pal-octets "%PDF-1.7" 10 "not a PDF past its first line"))))
        (multiple-value-bind (files pictures pairs) (palimpsest::compared-files (list a b c d broken))
          (is = 0 (length pictures))
          (is = 0 (length pairs))
          (dolist (file files)
            (dolist (m (palimpsest::compared-file-marks file))
              ;; a mark keeps what it needs of its scan alone
              (false (palimpsest::scan-picture-paper (palimpsest::ink-mark-scan m)))))
          (let ((text (palimpsest::pieces-text (palimpsest::compared-pieces (namestring dir) files pictures pairs 0))))
            (pal-has text "Not wholly compared")
            (pal-has text "broken.pdf")))
        (let ((palimpsest::*compared-marks-pixels-most* 1000))
          (let ((files (palimpsest::compared-files (list a b))))
            (true (every (lambda (f) (and (null (palimpsest::compared-file-marks f))
                                          (search "not kept" (palimpsest::compared-file-why f))))
                         files))))))))

;;; Read as written, whatever the file holds

(define-test (palimpsest palimpsest-iban-of-every-country)
  ;; an IBAN of a country of ISO 13616's whole register, Yemen's, is checked after its label
  (true (pal-finding (nth-value 2 (pal-read (pal-lines-page "IBAN YE64 CBYE 0001 0186 1234 5678 9123 45")))
                     :look "which does not give remainder 1"))
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "IBAN YE63 CBYE 0001 0186 1234 5678 9123 45")))
                      nil "check fails")))

(define-test (palimpsest palimpsest-glyph-names-above-surrogates)
  ;; uniXXXX above the surrogates stands for its character, as uXXXX does; a surrogate for none
  (is string= (string (code-char #xE000)) (palimpsest::glyph-character "uniE000"))
  (is string= (string (code-char #xFB01)) (palimpsest::glyph-character "uniFB01"))
  (false (palimpsest::glyph-character "uniD800"))
  (false (palimpsest::glyph-character "uD800")))

(define-test (palimpsest palimpsest-free-number-of-empty-saves)
  ;; a file whose saves name no object: the first free number is 1
  (is = 1 (palimpsest::first-free-number
           (palimpsest::make-pdf :revisions (vector (palimpsest::make-revision :entries (make-hash-table)))))))

(define-test (palimpsest palimpsest-encryption-words-balanced)
  ;; the encryption finding names the parts it has, its parentheses closed whichever are given
  (dolist (entries '("/Filter /Standard /V 2 /R 3 /P -4" "/Filter /Standard /P -4" "/V 2 /P -4" "/R 3"))
    (let* ((octets (pal-save #() (append *pal-original-objects* (list (cons 9 (format nil "<< ~a >>" entries))))
                             :trailer "/Root 1 0 R /Info 6 0 R /Encrypt 9 0 R"))
           (found (pal-finding (nth-value 2 (pal-read octets)) nil "The file is encrypted")))
      (true found "~a" entries)
      (when found
        (let ((words (palimpsest::finding-words found)))
          (is = (count #\( words) (count #\) words) "~a: ~a" entries words))))))

(define-test (palimpsest palimpsest-credentials-line-names-its-label)
  ;; one store: the line names the manifest's label and no store; with two or more, the store too
  (let ((label "urn:uuid:0a1b2c3d-0000-4000-8000-000000000001"))
    (is string= (format nil "Content credentials (the file's own, ~a)" label)
        (first (palimpsest::credential-details (palimpsest::make-credential :label label :active t :store 1))))
    (is string= (format nil "Content credentials (the file's own, store 2, ~a)" label)
        (first (palimpsest::credential-details (palimpsest::make-credential :label label :active t :store 2))))))

(define-test (palimpsest palimpsest-dates-as-written)
  ;; a file's date holding format directives is named as written, never read as part of the finding's words
  (dolist (date '("2026-03-02T09:00:00Z~:*~:*" "2026-03-02T09:00:00Z~1000000000a"))
    (let* ((xmp (format nil "<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF ~
                             xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description ~
                             xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmp:CreateDate='~a'/></rdf:RDF></x:xmpmeta>"
                        date))
           (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                       '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                       '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font
<< /F1 5 0 R >> >> /Contents 4 0 R >>")
                                       '(4 :stream "" "BT /F1 11 Tf 72 700 Td (Fakturadatum 2026-03-15) Tj ET")
                                       '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                                       '(6 . "<< /Producer (Kea Writer) /CreationDate (D:20260301090000+01'00') >>")
                                       (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))
           (found (pal-finding (nth-value 2 (pal-read octets)) :look "after every date the file gives")))
      (true found "~a" date)
      (when found
        (pal-has (palimpsest::finding-words found) (format nil "xmp:CreateDate ~a" date))
        (true (< (length (palimpsest::finding-words found)) 1000))))))

(define-test (palimpsest palimpsest-page-check-failing-alone)
  ;; one check of a page failing on what the file holds: it could not finish, the others' findings stand
  (let ((was (fdefinition 'palimpsest::check-chat-traces)))
    (unwind-protect
         (progn
           (setf (fdefinition 'palimpsest::check-chat-traces) (lambda (check) (declare (ignore check)) (error "Kea")))
           (let ((findings (nth-value 2 (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET
1 1 1 rg 70 695 100 20 re f")))))
             (true (pal-finding findings :strong "covered by a white shape"))
             (true (pal-finding findings :record "could not finish"))))
      (setf (fdefinition 'palimpsest::check-chat-traces) was))))

(define-test (palimpsest palimpsest-sampled-function-size-bounded)
  ;; a sampled function whose /Size claims a trillion samples over a small stream: never decoded past the setting
  (let* ((octets (pal-save #() (append *pal-original-objects*
                                       (list (list 9 :stream (format nil "/FunctionType 0 /Domain [0 1] /Range [0 1] ~
                                                                          /Size [1000000000000] /BitsPerSample 8 ~
                                                                          /Filter /FlateDecode")
                                                   (pal-zlib-stored (pal-octets 0 128 255)))))))
         (pdf (palimpsest::read-pdf octets :name "test.pdf"))
         (revision (palimpsest::last-revision pdf))
         (object (palimpsest::revision-object pdf revision 9))
         (limits '())
         (was (fdefinition 'palimpsest::stream-data)))
    (unwind-protect
         (progn
           (setf (fdefinition 'palimpsest::stream-data)
                 (lambda (pdf object &rest keys &key (limit tuatara-user:*palimpsest-largest-stream*))
                   (push limit limits)
                   (apply was pdf object keys)))
           (false (palimpsest::sampled-output pdf revision object (palimpsest::pdf-indirect-get object :value) 1/2)))
      (setf (fdefinition 'palimpsest::stream-data) was))
    (true (every (lambda (limit) (<= limit tuatara-user:*palimpsest-largest-stream*)) limits) "~s" limits)))

(define-test (palimpsest palimpsest-inferred-save-counted-against-its-file)
  ;; what reading a save hidden in a stream decodes and runs is counted against the file's own limits
  (true (loop for name in '("save-in-stream-xref-stream.pdf" "save-in-stream-offsets.pdf" "save-in-stream-bare.pdf"
                            "save-in-stream-said-pdf.pdf" "save-in-stream-said-embedded.pdf"
                            "save-in-stream-own-file-reached.pdf" "save-in-stream-own-file-unreached.pdf")
              for pdf = (pal-read (pal-sample name))
              for sign = (find-if #'cdr (getf (palimpsest::pdf-cache pdf) :streams-with-save-signs))
              thereis (and sign
                           (progn
                             ;; the file's own pages read once and kept, so that a second reading spends only on
                             ;; the hidden save
                             (palimpsest::inferred-save-changes pdf (car sign) (cdr sign))
                             (setf (getf (palimpsest::pdf-cache pdf) :operators-read) 0)
                             (palimpsest::inferred-save-changes pdf (car sign) (cdr sign))
                             (plusp (getf (palimpsest::pdf-cache pdf) :operators-read)))))))

(define-test (palimpsest palimpsest-shading-in-a-long-clip)
  ;; an axial shading clipped to a convex outline of 100,000 points: its region worked out in well under a second
  (let* ((octets (pal-save #() (append *pal-original-objects*
                                       (list '(9 . "<< /ShadingType 2 /ColorSpace /DeviceGray /Coords [100 0 500 0]
/Function << /FunctionType 2 /Domain [0 1] /C0 [0] /C1 [1] /N 1 >> >>")))))
         (pdf (palimpsest::read-pdf octets :name "test.pdf"))
         (revision (palimpsest::last-revision pdf))
         (outline (let ((v (make-array 200000)))
                    (dotimes (k 100000 v)
                      (let ((angle (/ (* 2 pi k) 100000)))
                        (setf (aref v (* 2 k)) (+ 300 (* 200 (cos angle)))
                              (aref v (1+ (* 2 k))) (+ 400 (* 200 (sin angle))))))))
         (started (get-internal-run-time))
         (region (palimpsest::shading-region pdf revision '(:ref 9 0) '(1 0 0 1 0 0) '(0 0 612 792)
                                             (list (list nil outline)))))
    (true (< (- (get-internal-run-time) started) internal-time-units-per-second))
    (true (<= 3 (length region)))
    ;; within the outline's box and the shading's two ends
    (true (every (lambda (p) (and (<= 99 (car p) 501) (<= 199 (cdr p) 601))) region))))

(define-test (palimpsest palimpsest-rules-written-for-the-examiner)
  ;; a finding's rule is a sentence for the examiner, never a function's documentation written for programmers
  (let ((files (remove "test" (directory (make-pathname :directory (butlast (pathname-directory *pal-samples*) 2)
                                                        :name :wild :type "lisp" :defaults *pal-samples*))
                       :key #'pathname-name :test #'string=)))
    (true (< 20 (length files)))
    (dolist (path files)
      (false (search ":rule (documentation" (alexandria:read-file-into-string path)) "~a" (pathname-name path)))))

(defun pal-signed-data (signer-serial &key (indefinite nil))
  "A signature's PKCS#7 data whose one signer is Aroha Ngata, by issuer (Kea CA) and SIGNER-SERIAL, holding her
certificate (serial 7) and after it Kea CA's own (serial 1); the outer item of undetermined length with
INDEFINITE, as BER writes it."
  (flet ((name (cn) (pal-der #x30 (pal-der #x31 (pal-der #x30 (pal-der 6 #x55 4 3) (pal-der #x0C cn)))))
         (certificate (serial subject issuer)
           (pal-der #x30 (pal-der #x30 (pal-der #xA0 (pal-der 2 2)) (pal-der 2 serial) (pal-der #x30)
                                  (pal-der #x30 (pal-der #x31 (pal-der #x30 (pal-der 6 #x55 4 3)
                                                                       (pal-der #x0C issuer))))
                                  (pal-der #x30)
                                  (pal-der #x30 (pal-der #x31 (pal-der #x30 (pal-der 6 #x55 4 3)
                                                                       (pal-der #x0C subject))))
                                  (pal-der #x30))
                    (pal-der #x30) (pal-der 3 0))))
    (let ((contents (pal-octets (pal-der 6 #x2A #x86 #x48 #x86 #xF7 #x0D 1 7 2)
                                (pal-der #xA0 (pal-der #x30 (pal-der 2 1) (pal-der #x31) (pal-der #x30)
                                                       (pal-der #xA0 (certificate 7 "Aroha Ngata" "Kea CA")
                                                                (certificate 1 "Kea CA" "Kea CA"))
                                                       (pal-der #x31 (pal-der #x30 (pal-der 2 1)
                                                                              (pal-der #x30 (name "Kea CA")
                                                                                       (pal-der 2 signer-serial)))))))))
      (if indefinite
          (pal-octets #x30 #x80 contents 0 0 0 0 0 0)
          (pal-der #x30 contents)))))

(define-test (palimpsest palimpsest-signer-named-not-its-issuer)
  ;; a signature holding its signer's certificate and then its authority's: by the signer, found by issuer and
  ;; serial number, never the authority whose certificate comes last; written with undetermined lengths too
  (is equal "Aroha Ngata" (palimpsest::signer-name (pal-signed-data 7)))
  (is equal "Aroha Ngata" (palimpsest::signer-name (pal-signed-data 7 :indefinite t)))
  ;; a signer naming a serial number no certificate has: no one is named
  (false (palimpsest::signer-name (pal-signed-data 9))))

(defmacro pal-with-functions ((&rest definitions) &body body)
  "Run BODY with each palimpsest function of DEFINITIONS, (NAME FUNCTION), put in place of its own, put back after."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved (list ,@(loop for (name) in definitions collect `(fdefinition ',name)))))
       (unwind-protect
            (progn ,@(loop for (name function) in definitions collect `(setf (fdefinition ',name) ,function))
                   ,@body)
         ,@(loop for (name) in definitions for k from 0 collect `(setf (fdefinition ',name) (nth ,k ,saved)))))))

(define-test (palimpsest palimpsest-recoloured-search-bounded)
  ;; a page of 5,000 characters of one size: each is set beside a bounded number of others for a copy recoloured,
  ;; not beside every other
  (let* ((glyphs (loop for k below 5000
                       collect (multiple-value-bind (row column) (floor k 100)
                                 (palimpsest::make-glyph :x0 (* 12 column) :y0 (* 16 row) :x1 (+ 10 (* 12 column))
                                                         :y1 (+ 12 (* 16 row)) :page 0 :picture 1
                                                         :width 14
                                                         :pixels (make-array (* 16 14)
                                                                             :element-type '(unsigned-byte 8))))))
         (correlations 0))
    (pal-with-functions ((palimpsest::glyph-correlation (lambda (a b) (declare (ignore a b)) (incf correlations) 0d0))
                         (palimpsest::regreyed-distance (lambda (&rest arguments) (declare (ignore arguments)) nil))
                         (palimpsest::shapeless-glyph-p (lambda (g) (declare (ignore g)) nil))
                         (palimpsest::rule-piece-p (lambda (&rest arguments) (declare (ignore arguments)) nil)))
      (palimpsest::join-regreyed-copies (palimpsest::make-copy-joins glyphs) glyphs 12 10))
    (true (<= correlations (* palimpsest::*glyph-peers-most* 5000)) "~:d correlations" correlations)))

;;; A form printed in two inks

(defun pal-ring-glyph (x y darkness shift seed)
  "A made-up o printed in an ink of DARKNESS, how far (RED GREEN BLUE) it takes
each colour down from the paper's 235, its ink's box 12 by 12 at X Y of
picture 1, the ring drawn SHIFT of a pixel right and down of the box's middle
as a print falls between pixels. Each pixel's ink is laid up to a quarter
heavier or lighter than drawn, as each print takes its ink its own way, and
each colour strays by a grey level either way, both seeded by SEED. Its grey
is the colours' weighted sum, and its colours are kept as a picture in
colours keeps them."
  (let* ((side 16) (grey (make-array (* side side) :element-type '(unsigned-byte 8)))
         (channels (loop repeat 3 collect (make-array (* side side) :element-type '(unsigned-byte 8)))))
    (flet ((next (n)
             (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
             (mod (ash seed -16) n)))
      (dotimes (j side)
        (dotimes (i side)
          (let ((cover (* (/ (loop for sy below 4
                                   sum (loop for sx below 4
                                             count (<= 3.2 (sqrt (+ (expt (- (+ i (/ (+ sx 0.5) 4)) 8 shift) 2)
                                                                    (expt (- (+ j (/ (+ sy 0.5) 4)) 8 shift) 2)))
                                                       5.6)))
                             16)
                          (+ 3/4 (/ (next 51) 100))))
                (at (+ (* j side) i)))
            (loop for cut in channels for dark in darkness
                  do (setf (aref cut at) (max 0 (round (+ (- 235 (* dark cover)) -1 (next 3))))))
            (setf (aref grey at) (round (+ (* 0.299 (aref (first channels) at)) (* 0.587 (aref (second channels) at))
                                           (* 0.114 (aref (third channels) at)))))))))
    (palimpsest::make-glyph :x0 x :y0 y :x1 (+ x 12) :y1 (+ y 12) :page 0 :picture 1 :width side :pixels grey
                            :channels channels :alike 40)))

(define-test (palimpsest palimpsest-two-inks-not-recoloured)
  ;; a form whose labels are printed in red and whose filling is in black, the same letter in both, each print
  ;; falling between pixels its own way: no two named as a character recoloured. The red ink is faint in red,
  ;; and a line through that colour alone is steep and makes two honest prints in red look as near as a copy.
  (let* ((glyphs (loop for k below 60
                       collect (pal-ring-glyph (* 16 (mod k 20)) (* 20 (floor k 20))
                                               (if (zerop (mod k 3)) '(50 200 190) '(200 200 200))
                                               (/ (mod (* 7 k) 10) 10) (+ 11 k))))
         (joins (palimpsest::make-copy-joins glyphs)))
    (pal-with-functions ((palimpsest::shapeless-glyph-p (lambda (g) (declare (ignore g)) nil))
                         (palimpsest::rule-piece-p (lambda (&rest arguments) (declare (ignore arguments)) nil)))
      (palimpsest::join-regreyed-copies joins glyphs 12 12))
    (false (loop for g in glyphs thereis (eq :regreyed (gethash g (palimpsest::copy-joins-why joins)))))))

(define-test (palimpsest palimpsest-scan-limits-checked-as-made)
  ;; a picture of more words than allowed is given up as soon as one too many is measured, and once more
  ;; characters than allowed are cut no more are
  (let ((measured 0) (cut 0)
        (measure (fdefinition 'palimpsest::measure-word)) (glyphs (fdefinition 'palimpsest::word-glyphs)))
    (pal-with-functions ((palimpsest::measure-word (lambda (&rest arguments) (incf measured) (apply measure arguments)))
                         (palimpsest::word-glyphs (lambda (&rest arguments)
                                                    (let ((made (apply glyphs arguments)))
                                                      (incf cut (length made))
                                                      made))))
      (let ((palimpsest::*scan-words-most* 5))
        (false (palimpsest::picture-words (pal-scan) 600 500))
        (is = 6 measured))
      (let ((palimpsest::*scan-characters-most* 10))
        (true (palimpsest::picture-words (pal-scan) 600 500))
        (true (< cut 100) "~:d characters cut" cut)))))

(define-test (palimpsest palimpsest-one-page-pdfs-read)
  ;; the page standing for a picture file and the page a fax or JBIG2 picture is drawn on read as PDFs of one page
  ;; drawing their picture
  (flet ((picture-entry (octets key)
           (let* ((pdf (palimpsest::read-pdf octets :name "test.pdf")) (revision (palimpsest::last-revision pdf)))
             (is = 1 (length (palimpsest::pdf-revisions pdf)))
             (is = 1 (length (palimpsest::pages-of pdf revision)))
             (values (palimpsest::deref pdf revision (palimpsest::pdf-get (palimpsest::pdf-indirect-get
                                                                            (palimpsest::revision-object pdf revision 4)
                                                                            :value)
                                                                           key))
                     pdf revision))))
    (is eql 600 (picture-entry (palimpsest::picture-page-octets 600 500) "/Width"))
    (multiple-value-bind (mask pdf revision)
        (picture-entry (palimpsest::bilevel-pdf 40 30 "/JBIG2Decode" '() (pal-octets 1 2 3) (pal-octets 4 5))
                       "/ImageMask")
      (true (palimpsest::pdf-true-p mask))
      (is equalp (pal-octets 4 5) (palimpsest::stream-data pdf (palimpsest::revision-object pdf revision 6))))))

(define-test (palimpsest palimpsest-compared-files-stay-in-their-directory)
  ;; a .pdf in the directory that is a link to a file elsewhere is not read, wherever it leads
  (with-test-directory (outside)
    (with-test-directory (dir)
      (flet ((put (directory name)
               (let ((path (namestring (merge-pathnames name directory))))
                 (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
                   (write-sequence (pal-edited) out))
                 path)))
        (put dir "a.pdf")
        (put dir "b.pdf")
        (sb-posix:symlink (put outside "elsewhere.pdf") (namestring (merge-pathnames "c.pdf" dir)))
        (is equal '("a.pdf" "b.pdf") (mapcar #'file-namestring (palimpsest::directory-pdfs (namestring dir))))))))

(define-test (palimpsest palimpsest-help-words-as-shown)
  ;; the list of files compared names its command without a stray backslash; drawing the objects says how
  ;; the words unlike the rest are framed as the key at the page's foot does
  (let ((doc (documentation 'palimpsest::palimpsest-compared-mode 'type)))
    (true doc)
    (false (search "\\(" (or doc "")) "~a" doc))
  (let ((doc (documentation 'palimpsest::palimpsest-draw-objects 'function)))
    (true doc)
    (false (search "in red" (or doc "")))
    (true (search "framed by their strength" (or doc "")))))

(define-test (palimpsest palimpsest-copied-paper-rule-says-what-is-searched)
  ;; the rule says pictures once saved as a JPEG and kept without loss since are searched, not that they are not
  (let ((copied (pal-finding (nth-value 2 (pal-read (pal-scanned (pal-cloned (pal-noisy-scan :sigma 2)
                                                                             300 462 256 168 30 26))))
                             :look "alike pixel for pixel")))
    (true copied)
    (when copied
      (pal-has (palimpsest::finding-rule copied) "those once saved as a JPEG too")
      (false (search "never saved as a JPEG" (palimpsest::finding-rule copied))))))

(define-test (palimpsest palimpsest-groups-without-stack)
  ;; things joined in one chain of 500,000 make one group, with no call nested in another for each step
  (let ((sets (palimpsest::make-disjoint-sets 500000)))
    (loop for k from 499999 above 0 do (palimpsest::join-sets sets k (1- k)))
    (is = 0 (palimpsest::set-root sets 499999))
    (is = 1 (length (palimpsest::set-groups sets))))
  ;; groups in the order of their first things, each in its things' order
  (let ((sets (palimpsest::make-disjoint-sets 6)))
    (palimpsest::join-sets sets 4 1)
    (palimpsest::join-sets sets 5 2)
    (palimpsest::join-sets sets 2 4)
    (is equal '((0) (1 2 4 5) (3)) (palimpsest::set-groups sets))
    (is equal '((:a) (:b :c :e :f) (:d)) (palimpsest::set-groups sets (vector :a :b :c :d :e :f)))))

(define-test (palimpsest palimpsest-cases-compared-by-way)
  ;; 2,000 fingerprints of 20 ways of editing, one way each: only those of one way are set beside one another
  (let ((calls 0) (alike (fdefinition 'palimpsest::cases-alike-p))
        (prints (loop for k below 2000
                      collect (list (format nil "/cases/~4,'0d-fingerprint.txt" k) (format nil "~d.pdf" k)
                                    (list (list :method (format nil "way-~d" (mod k 20))))))))
    (pal-with-functions ((palimpsest::cases-alike-p (lambda (a b) (incf calls) (funcall alike a b))))
      (is = 2000 (length (palimpsest::group-fingerprints prints))))
    (true (<= calls (* 20 (/ (* 100 99) 2))) "~:d pairs set beside each other" calls)))

;;; A questioned file beside known-genuine files

(defun pal-issued (lines &key (producer "Kea Ledger 4.2") (font "Helvetica") (size 10) (comment '(226 227 207 211)))
  "A one-page PDF as an issuer's system writes it: LINES one under another in FONT at SIZE, its producer PRODUCER,
the comment under its first line the octets COMMENT."
  (let ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                    '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                    (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] ~
                                                         /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
                                    (list 4 :stream ""
                                          (format nil "~{~a~^~%~}"
                                                  (loop for line in lines for y downfrom 760 by 16
                                                        collect (format nil "BT /F1 ~d Tf 72 ~d Td (~a) Tj ET"
                                                                        size y line))))
                                    (cons 5 (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /~a >>" font))
                                    (cons 6 (format nil "<< /Producer (~a) /Creator (Ledger) >>" producer))))))
    (replace octets comment :start1 10)
    octets))

(defun pal-beside (octets &rest references)
  "The findings of the reference check on OCTETS set beside REFERENCES, each the octets of a known-genuine file."
  (let ((palimpsest::*references*
          (loop for reference in references for k from 1
                collect (palimpsest::read-pdf reference :name (format nil "genuine-~d.pdf" k)))))
    (remove :references (nth-value 2 (pal-read octets)) :key #'palimpsest::finding-check :test-not #'eq)))

(define-test (palimpsest palimpsest-beside-genuine-files)
  (let ((first (pal-issued '("Invoice 4017" "Total due 1,250.00")))
        (second (pal-issued '("Invoice 4018" "Total due 980.00")))
        (third (pal-issued '("Invoice 4021" "Total due 77.10"))))
    ;; without genuine files the check is not run, and says why
    (multiple-value-bind (findings outcomes)
        (palimpsest::all-findings (palimpsest::read-pdf first :name "a.pdf") '())
      (false (find :references findings :key #'palimpsest::finding-check))
      (is equal "no known-genuine files given" (cdr (assoc :references outcomes))))
    ;; a genuine file beside two others of its issuer: nothing said
    (is equal '() (mapcar #'palimpsest::finding-words (pal-beside third first second)))
    ;; the issuer's name kept and the file put together otherwise: worth a look, never strong, since a file
    ;; only written again stands apart alike; beside two that agree it says what they do and what it does
    (let* ((rebuilt (pal-issued '("Invoice 4021" "Total due 9,077.10") :comment '(191 247 162 254)))
           (findings (pal-beside rebuilt first second))
           (kept (pal-finding findings :look "names the same producer and creator as 2 of the known-genuine")))
      (false (pal-finding findings :strong "known-genuine"))
      (true kept "~s" (mapcar #'palimpsest::finding-words findings))
      (when kept
        (pal-has (palimpsest::finding-words kept)
                 "theirs: PDF 1.7, the comment E2E3CFD3 under it; this file's: PDF 1.7, the comment BFF7A2FE under it")
        (pal-has (palimpsest::finding-words kept) "A software update or another office would change the name")
        (pal-has (palimpsest::finding-words kept) "not that anything in it was changed")
        (is eq :other-program (palimpsest::finding-method kept))
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence kept)) "genuine-1.pdf, genuine-2.pdf"))
      (let ((one (pal-beside rebuilt first)))
        (false (pal-finding one :strong "known-genuine"))
        (true (pal-finding one :look "One genuine file is little to judge by"))))
    ;; a producer that names no version: an update would not show in its name, so worth a look at most
    (let ((findings (pal-beside (pal-issued '("Invoice 4021") :producer "Kea Scan" :comment '(191 247 162 254))
                                (pal-issued '("Invoice 4017") :producer "Kea Scan")
                                (pal-issued '("Invoice 4018") :producer "Kea Scan"))))
      (false (pal-finding findings :strong "known-genuine"))
      (true (pal-finding findings :look "The producer names no version")))
    ;; another program named: worth a look, with the honest causes; the same program in another version: for
    ;; the record, worth a look when the file is put together otherwise too, never strong
    (let ((findings (pal-beside (pal-issued '("Invoice 4021" "Total due 9,077.10") :producer "Skia/PDF m157"
                                                                                  :comment '(191 247 162 254))
                                first second)))
      (false (pal-finding findings :strong "known-genuine"))
      (let ((look (pal-finding findings :look "Another program wrote this file")))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when look
          (pal-has (palimpsest::finding-words look)
                   "all name \"Kea Ledger 4.2\" as producer; this file names \"Skia/PDF m157\"")
          (pal-has (palimpsest::finding-words look) "another office with a system of its own")
          (pal-has (palimpsest::finding-words look) "it also differs in the PDF version and the comment"))))
    (let ((findings (pal-beside (pal-issued '("Invoice 4021" "Total due 77.10") :producer "Kea Ledger 4.3")
                                first second)))
      (false (pal-finding findings :strong "known-genuine"))
      (false (pal-finding findings :look "known-genuine"))
      (true (pal-finding findings :record "a software update at the issuer")))
    (let ((findings (pal-beside (pal-issued '("Invoice 4021" "Total due 77.10") :producer "Kea Ledger 4.3"
                                                                               :comment '(191 247 162 254))
                                first second)))
      (false (pal-finding findings :strong "known-genuine"))
      (true (pal-finding findings :look "An update seldom changes how a program puts its files together")))
    ;; a figure typed in at a size and in a font no genuine file uses: each named with its place
    (let* ((typed (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                      (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] ~
                                                           /Resources << /Font << /F1 5 0 R /F2 7 0 R >> >> ~
                                                           /Contents 4 0 R >>"))
                                      (list 4 :stream ""
                                            (format nil "BT /F1 10 Tf 72 760 Td (Invoice 4021) Tj ET~%~
                                                         BT /F2 11 Tf 72 744 Td (Total due 9,077.10) Tj ET"))
                                      '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                                      '(6 . "<< /Producer (Kea Ledger 4.2) /Creator (Ledger) >>")
                                      '(7 . "<< /Type /Font /Subtype /Type1 /BaseFont /Courier >>"))))
           (findings (pal-beside typed first second))
           (font (pal-finding findings :look "in the 2 known-genuine files, Helvetica; in this file, Courier"))
           (size (pal-finding findings :look "in the 2 known-genuine files, 10.0 pt; in this file, 11.0 pt")))
      (true font "~s" (mapcar #'palimpsest::finding-words findings))
      (true size)
      (when font
        (is eql 1 (palimpsest::finding-page font))
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details font))
                 "page 1: text \"Total due 9,077.10\" at x 72, y 744")))
    ;; text that sits off the grid the genuine files share
    (let* ((moved (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                      (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] ~
                                                           /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
                                      (list 4 :stream "" (format nil "BT /F1 10 Tf 72 760 Td (Invoice 4021) Tj ET~%~
                                                                      BT /F1 10 Tf 303 744 Td (9,077.10) Tj ET"))
                                      '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                                      '(6 . "<< /Producer (Kea Ledger 4.2) /Creator (Ledger) >>"))))
           (grid (pal-finding (pal-beside moved first second) :look "1 of 2 runs of text off that grid")))
      (true grid)
      (when grid
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details grid)) "text \"9,077.10\" at x 303, y 744")))
    ;; the habits themselves, and how a file stands apart in one
    (let ((habits (palimpsest::file-habits (palimpsest::read-pdf first :name "a.pdf"))))
      (is equal "Kea Ledger 4.2" (palimpsest::habit-value habits :producer))
      (is equal "a table" (palimpsest::habit-value habits :index))
      (is equal "saved once" (palimpsest::habit-value habits :saves))
      (is equal '("595 by 842 pt") (palimpsest::habit-value habits :page-sizes))
      (is equal '("BT" "ET" "Td" "Tf" "Tj") (palimpsest::habit-value habits :operators))
      (is equal '("Type1, not embedded") (palimpsest::habit-value habits :fonts-kept)))
    (is equal "Kea Ledger" (palimpsest::producer-name "Kea Ledger 4.2"))
    (is equal '(1) (palimpsest::off-grid '((72 . 120) (300 . 340)) '(((72 . 99) (410 . 480)))))
    ;; references that disagree among themselves give nothing to stand apart from
    (false (palimpsest::habit-departure :header '((:header "a")) '(((:header "b")) ((:header "c")))))
    (is equal '("b" "a") (palimpsest::habit-departure :header '((:header "a")) '(((:header "b")) ((:header "b")))))))

(define-test (palimpsest palimpsest-genuine-files-read)
  ;; the files given are read; one that is no PDF, and the examined file itself, are left out with why
  (with-test-directory (dir)
    (flet ((file (name octets)
             (let ((path (namestring (merge-pathnames name dir))))
               (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
                 (write-sequence octets out))
               path)))
      (let* ((questioned (pal-issued '("Invoice 4021" "Total due 9,077.10") :comment '(191 247 162 254)))
             (paths (list (file "a.pdf" (pal-issued '("Invoice 4017" "Total due 1,250.00")))
                          (file "b.pdf" (pal-issued '("Invoice 4018" "Total due 980.00")))
                          (file "notes.txt" (pal-octets "not a PDF"))
                          (file "same.pdf" questioned))))
        (multiple-value-bind (read left) (palimpsest::read-references paths (tuatara:octets-sha256 questioned))
          (is equal '("a.pdf" "b.pdf") (mapcar #'palimpsest::pdf-name read))
          (is equal '(("notes.txt" "not a PDF") ("same.pdf" "the file examined itself")) left))
        (is equal (subseq paths 0 2)
            (subseq (palimpsest::reference-paths (namestring dir)) 0 2))
        ;; examined beside them as the work shut in examines it: the finding and the files named in the report
        (let* ((report (namestring (ensure-directories-exist (merge-pathnames "report/" dir))))
               (analysis (palimpsest::examine-file (file "questioned.pdf" questioned) report :references paths))
               (text (format nil "~{~a~}" (mapcar (lambda (piece) (if (consp piece) (car piece) piece))
                                                  (palimpsest::report-pieces analysis)))))
          (true (pal-finding (palimpsest::analysis-findings analysis) :look "names the same producer and creator"))
          (pal-has text "Set beside")
          (pal-has text "notes.txt, left out: not a PDF")
          (pal-has text "Beside known-genuine files")))))
  ;; examined shut in as C-c p g examines it, the genuine files read in the sandbox beside it: the same report
  (let ((questioned (pal-issued '("Invoice 4021" "Total due 9,077.10") :comment '(191 247 162 254)))
        (beside (list (cons "a.pdf" (pal-issued '("Invoice 4017" "Total due 1,250.00")))
                      (cons "b.pdf" (pal-issued '("Invoice 4018" "Total due 980.00"))))))
    (let ((here (pal-reported questioned "questioned.pdf" :beside beside))
          (shut (pal-reported questioned "questioned.pdf" :beside beside :shut-in t)))
      (false (palimpsest::analysis-stopped shut) "~a" (palimpsest::analysis-stopped shut))
      (dolist (analysis (list here shut))
        (let ((text (first (palimpsest::analysis-shown analysis))))
          (pal-has text "Set beside")
          (pal-has text "b.pdf")
          (pal-has text "names the same producer and creator as 2 of the known-genuine files"))))))

;;; Figures that no longer agree

(define-test (palimpsest palimpsest-weekday-beside-a-date)
  (is equal '((1 46093 0 21)) (palimpsest::wrong-weekdays "Monday, 14 March 2026"))
  (false (palimpsest::wrong-weekdays "Saturday, 14 March 2026"))
  (false (palimpsest::wrong-weekdays "lördagen den 14 mars 2026"))
  (true (palimpsest::wrong-weekdays "fredag den 14 mars 2026"))
  (true (palimpsest::wrong-weekdays "Paid on 2026-03-14 (Friday)"))
  (false (palimpsest::wrong-weekdays "Paid on 2026-03-14 (Sat)"))
  ;; a date whose day and month could be swapped is not judged; a weekday with no date beside it is none
  (false (palimpsest::wrong-weekdays "Monday 3/4/2026"))
  (false (palimpsest::wrong-weekdays "Closed on Monday. 14 March 2026 is the last day"))
  (let* ((findings (nth-value 2 (pal-read (pal-lines-page "Receipt" "Tuesday 14 March 2026" "Total 21.60"))))
         (found (pal-finding findings :look "2026-03-14 was a Saturday, not a Tuesday")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (is eq :figure-unchecked (palimpsest::finding-method found))
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) "at x 72, y 684")))
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "Receipt" "Saturday 14 March 2026"))) nil "was a")))

(define-test (palimpsest palimpsest-running-balance)
  (let ((rows '("Balance brought forward 642.88" "02 Jan Co-op 23.18 619.70" "02 Jan Council tax 142.00 477.70"
                "03 Jan Rail ticket 18.60 459.10" "04 Jan Transfer in 150.00 609.10" "05 Jan Pharmacy 7.45 601.65"
                "06 Jan Broadband 32.99 568.66" "Closing balance: 568.66")))
    (false (palimpsest::broken-balances rows))
    ;; an amount raised, its balance left: the row named; a balance alone changed; the closing balance changed
    (is equal '((4 :row 45910 115000))
        (palimpsest::broken-balances (substitute "04 Jan Transfer in 1,150.00 609.10" (nth 4 rows) rows :test #'equal)))
    (is equal '((4 :balance 45910 15000))
        (palimpsest::broken-balances (substitute "04 Jan Transfer in 150.00 1,609.10" (nth 4 rows) rows :test #'equal)))
    (is equal '((7 :closing 56866 nil))
        (palimpsest::broken-balances (substitute "Closing balance: 1,568.66" (nth 7 rows) rows :test #'equal)))
    ;; a line that is no part of the table is passed over, and three rows are no statement
    (false (palimpsest::broken-balances (append (subseq rows 0 4) '("Fees this month 1.50 3.00") (subseq rows 4))))
    (false (palimpsest::broken-balances '("Opening balance 500.00" "Card 71.44 428.56" "Salary 2,213.04 2,641.60"
                                          "Rent 950.00 1,700.00" "Total paid out 1,021.44")))
    (let* ((changed (substitute "04 Jan Transfer in 1,150.00 609.10" (nth 4 rows) rows :test #'equal))
           (findings (nth-value 2 (pal-read (apply #'pal-lines-page changed))))
           (found (pal-finding findings :look "the running balance breaks at \"04 Jan Transfer in 1,150.00 609.10\"")))
      (true found "~s" (mapcar #'palimpsest::finding-words findings))
      (when found
        (pal-has (palimpsest::finding-words found) "the balance before it, 459,10, and its amount, 1 150,00")
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) "at x 72, y 636")))
    (false (pal-finding (nth-value 2 (pal-read (apply #'pal-lines-page rows))) nil "running balance"))
    (true (pal-finding (nth-value 2 (pal-read (apply #'pal-lines-page
                                                     (substitute "Closing balance: 1,568.66" (nth 7 rows) rows
                                                                 :test #'equal))))
                       :look "the closing balance stated, \"Closing balance: 1,568.66\", is not the last row's"))))

(define-test (palimpsest palimpsest-columns-and-net-pay)
  ;; a table's cells and the amounts with their labels
  (let* ((mark (palimpsest::make-mark :kind :text :text "x" :x0 0 :x1 10))
         (text (format nil "Basic salary~c2,875.00~cIncome tax~c422.40" #\Tab #\Tab #\Tab))
         (cells (palimpsest::line-cells text (make-array (length text) :initial-element mark))))
    (is equal '("Basic salary" "2,875.00" "Income tax" "422.40") (mapcar #'first cells))
    (is equal '(("basic salary" 287500 nil) ("income tax" 42240 t))
        (mapcar (lambda (a) (list (first a) (second a) (fourth a))) (palimpsest::labelled-amounts cells)))
    (is equal '(("closing balance" 93360))
        (mapcar (lambda (a) (subseq a 0 2))
                (palimpsest::labelled-amounts (list (list "Closing balance: 933.60" mark)))))
    ;; a line a printer joined into one run: each amount with the words since the amount before it
    (is equal '(("gross pay" 276607 nil) ("total deductions" 61926 t))
        (mapcar (lambda (a) (list (first a) (second a) (fourth a)))
                (palimpsest::labelled-amounts (list (list "Gross pay 2,766.07 Total deductions 619.26" mark))))))
  ;; a column inside the table whose total the amounts above it do not make
  (flet ((column (&rest rows)
           (loop for (label value) in rows
                 collect (list (list label value (palimpsest::make-mark :kind :text :x1 300) nil)
                               (list "tax" 100 (palimpsest::make-mark :kind :text :x1 500) t)))))
    (let ((columns (palimpsest::inner-columns (column '("basic salary" 297500) '("overtime" 16172)
                                                      '("shift allowance" 12000) '("total gross pay" 315672)))))
      (is = 1 (length columns))
      (is equal '((3 325672 (0 1 2)))
          (palimpsest::unsummed-rows (mapcar (lambda (row) (subseq row 0 3)) (first columns)))))
    (false (palimpsest::inner-columns (column '("a" 100) '("b" 200) '("total" 300)))))
  ;; lines a printer joined into one run each: the columns told by the amounts' order in their lines
  (let* ((lines (loop for (label value) in '(("basic pay" 254167) ("overtime" 38840) ("mileage" 3600)
                                              ("gross pay" 276607))
                      for x1 in '(544.4 541.4 543.4 541.9)
                      collect (let ((mark (palimpsest::make-mark :kind :text :x1 x1)))
                                (list (list label value mark nil) (list "tax" 100 mark t)))))
         (columns (palimpsest::inner-columns lines)))
    (is = 1 (length columns))
    (is equal '((3 296607 (0 1 2)))
        (palimpsest::unsummed-rows (mapcar (lambda (row) (subseq row 0 3)) (first columns)))))
  ;; gross pay adds up its payments
  (is equal '((3 276607 (0 1 2)))
      (palimpsest::unsummed-totals '("Basic pay 2,541.67" "Overtime 188.40" "Mileage 36.00" "Gross pay 2,966.07")))
  ;; net pay against gross and deductions
  (let* ((lines '("Total gross pay 3,156.72" "Total deductions 765.68" "NET PAY 2,991.04"))
         (findings (nth-value 2 (pal-read (apply #'pal-lines-page lines))))
         (found (pal-finding findings :look "the net pay stated, 2 991,04, is not the gross pay of 3 156,72 less")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found) "which make 2 391,04")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) "\"NET PAY 2,991.04\"")))
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "Total gross pay 3,156.72" "Total deductions 765.68"
                                                             "NET PAY 2,391.04")))
                      nil "net pay stated")))

(define-test (palimpsest palimpsest-figures-of-the-document)
  ;; a total stated twice with two amounts, and a sentence naming a sum the document does not state
  (let* ((sentence "We will collect 86.46 by Direct Debit on or after 28 days")
         (findings (nth-value 2 (pal-read (pal-lines-page "Subtotal 82.34" "VAT at 5% 4.12" "Total to pay 36.46"
                                                          sentence))))
         (found (pal-finding findings :look "A sentence on page 1 names 86,46 to be paid")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found) "while it states its total to pay as 36,46")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) "page 1: \"Total to pay 36.46\"")))
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "Subtotal 82.34" "VAT at 5% 4.12" "Total to pay 86.46"
                                                             "We will collect 86.46 by Direct Debit in 28 days")))
                      nil "names 86,46"))
  ;; a statement's row naming a direct debit is no sentence: its amount is the row's own
  (let* ((rows '(("02 Jan Direct debit, Council tax office" nil "142.00" "477.70")
                 ("03 Jan Card payment, Rail ticket office" nil "18.60" "459.10")))
         (findings (nth-value 2 (pal-read (pal-priced-rows rows '("Closing balance" "459.10"))))))
    (false (pal-finding findings nil "A sentence on page") "~s" (mapcar #'palimpsest::finding-words findings)))
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "Closing balance 459.10"
                                                             "02 Jan Direct debit to the Council tax office 142.00")))
                      nil "A sentence on page"))
  (let ((findings (nth-value 2 (pal-read (pal-lines-page "Invoice" "Amount due 1,250.00" "Remittance advice"
                                                         "Amount due 1,520.00")))))
    (true (pal-finding findings :look "The document states its amount due as 1 250,00 and as 1 520,00")
          "~s" (mapcar #'palimpsest::finding-words findings)))
  (false (pal-finding (nth-value 2 (pal-read (pal-lines-page "Amount due 1,250.00" "Remittance advice"
                                                             "Amount due 1,250.00")))
                      nil "The document states its"))
  ;; a total carried to the next page and brought forward as another
  (flet ((two-pages (carried brought)
           (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                               '(2 . "<< /Type /Pages /Kids [3 0 R 7 0 R] /Count 2 >>")
                               (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources ~
                                                    << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
                               (list 4 :stream "" (format nil "BT /F1 11 Tf 72 700 Td (Timber 412.50) Tj ET~%~
                                                               BT /F1 11 Tf 72 100 Td (Carried forward ~a) Tj ET"
                                                          carried))
                               '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                               '(6 . "<< /Producer (Kea Writer) >>")
                               (cons 7 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources ~
                                                    << /Font << /F1 5 0 R >> >> /Contents 8 0 R >>"))
                               (list 8 :stream "" (format nil "BT /F1 11 Tf 72 700 Td (Brought forward ~a) Tj ET~%~
                                                               BT /F1 11 Tf 72 684 Td (Labour 300.00) Tj ET"
                                                          brought))))))
    (let* ((findings (nth-value 2 (pal-read (two-pages "412.50" "512.50"))))
           (found (pal-finding findings :look "Page 1 carries 412,50 forward and page 2 brings 512,50 forward")))
      (true found "~s" (mapcar #'palimpsest::finding-words findings))
      (when found
        (is eql 2 (palimpsest::finding-page found))
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) "page 2: \"Brought forward 512.50\"")))
    (false (pal-finding (nth-value 2 (pal-read (two-pages "412.50" "412.50"))) nil "carries")))
  ;; a statement over two pages, its closing balance alone on the second and changed
  (flet ((statement (closing)
           (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                               '(2 . "<< /Type /Pages /Kids [3 0 R 7 0 R] /Count 2 >>")
                               (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources ~
                                                    << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>"))
                               (list 4 :stream ""
                                     (format nil "~{~a~^~%~}"
                                             (loop for row in '("Balance brought forward 642.88"
                                                                "02 Jan Co-op 23.18 619.70" "02 Jan Tax 142.00 477.70"
                                                                "03 Jan Rail 18.60 459.10" "04 Jan Giro 150.00 609.10"
                                                                "05 Jan Pharmacy 7.45 601.65")
                                                   for y downfrom 700 by 16
                                                   collect (format nil "BT /F1 11 Tf 72 ~d Td (~a) Tj ET" y row))))
                               '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                               '(6 . "<< /Producer (Kea Writer) >>")
                               (cons 7 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources ~
                                                    << /Font << /F1 5 0 R >> >> /Contents 8 0 R >>"))
                               (list 8 :stream "" (format nil "BT /F1 11 Tf 72 700 Td (Closing balance: ~a) Tj ET"
                                                          closing))))))
    (let* ((findings (nth-value 2 (pal-read (statement "1,601.65"))))
           (found (pal-finding findings :look "On page 2 the closing balance stated, \"Closing balance: 1,601")))
      (true found "~s" (mapcar #'palimpsest::finding-words findings))
      (when found
        ;; the figure's own place within the line that holds it
        (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details found)) ", the figure at x ")
        (pal-has (palimpsest::finding-words found) "is not the last row's balance, 601,65")
        (pal-has (palimpsest::finding-words found) "The balance before it stands on the page before")
        (is eql 2 (palimpsest::finding-page found))))
    (false (pal-finding (nth-value 2 (pal-read (statement "601.65"))) nil "closing balance stated"))))

(define-test (palimpsest palimpsest-more-check-digits)
  ;; a card number, a plusgiro number, a creditor reference and a KID whose checks fail
  (let* ((findings (nth-value 2 (pal-read (pal-lines-page "Card number 4539 1488 0343 6468" "Plusgiro 12 34 56-8"
                                                          "Payment reference RF18 5390 0754 7035" "KID-nummer 2345677"
                                                          "Account GB82 WEST 1234 5698 7654 33"))))
         (found (pal-finding findings :look "whose check fails")))
    (true found "~s" (mapcar #'palimpsest::finding-words findings))
    (when found
      (pal-has (palimpsest::finding-words found) "the card number 4539148803436468, whose check digit would be 7")
      (pal-has (palimpsest::finding-words found) "the plusgiro number 123456-8")
      (pal-has (palimpsest::finding-words found) "the creditor reference RF18539007547035")
      (pal-has (palimpsest::finding-words found) "the KID 2345677")
      (pal-has (palimpsest::finding-words found) "the IBAN GB82WEST12345698765433")))
  ;; the same numbers with their checks holding, and numbers after the same labels that are none of these
  (let ((findings (nth-value 2 (pal-read (pal-lines-page "Card number 4539 1488 0343 6467" "Plusgiro 12 34 56-6"
                                                         "Payment reference RF18 5390 0754 7034" "KID-nummer 2345676"
                                                         "Account GB82 WEST 1234 5698 7654 32" "Account 31926819"
                                                         "Reference INV-2026-0418" "Card ending 4417"
                                                         "Account number SLE-55120438")))))
    (false (pal-finding findings nil "check fails") "~s" (mapcar #'palimpsest::finding-words findings)))
  (true (palimpsest::modulus-11-p "2345676"))
  (false (palimpsest::modulus-11-p "2345677")))
