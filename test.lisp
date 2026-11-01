;;;; test.lisp --- The palimpsest package: a PDF's saves taken apart, and the signs of its editing found.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:tuatara.tests)

(define-test palimpsest
  :description "The palimpsest package: PDFs written here by hand (an original and its incremental updates,
cross-reference and object streams with predictors, white boxes over text, invisible text, signatures that stop
short, a shadowed object, mixed font subsets, metadata that disagrees, data after %%EOF) read from their octets
alone; every revision found, compared and written out with a log naming the offsets and octets of each sign;
hostile files (loops, huge counts, offsets out of range, a stream that unpacks without end) read as far as they
make sense, never hanging or failing; the original never changed; the report and the object view.")

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
                                (format s "~10,'0d ~5,'0d ~a~c~c" (if (= (first e) 0) 0 (second e)) (if (= (first e) 0) 65535 0) (third e) #\Return #\Newline))
                              (setf entries (nthcdr (length run) entries))))
                   (format s "trailer~%<< /Size ~d ~a~@[ /Prev ~d~] >>~%startxref~%~d~%%%EOF~%"
                           (max size (if (plusp (length base)) 20 0)) trailer (and (plusp (length base)) (pal-last-startxref base)) xref))))
      (pal-octets out text))))

(defparameter *pal-original-objects*
  '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
    (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
    (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
    (4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F1 12 Tf 72 680 Td (Total: 100) Tj ET")
    (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>")
    (6 . "<< /Producer (Kea Writer 1.0) /Creator (Kea Writer) /CreationDate (D:20260101090000+13'00') /ModDate (D:20260101090000+13'00') >>"))
  "The objects of the original invoice the tests change.")

(defun pal-original ()
  "The original invoice: one page, its total 100."
  (pal-save #() *pal-original-objects*))

(defun pal-edited ()
  "The invoice changed by a later save: its total 900, saved by an online editor."
  (pal-save (pal-original)
            '((4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F1 12 Tf 72 680 Td (Total: 900) Tj ET")
              (6 . "<< /Producer (iLovePDF) /Creator (Kea Writer) /CreationDate (D:20260101090000+13'00') /ModDate (D:20260105120000+13'00') >>"))))

(defun pal-read (octets)
  "OCTETS read as a PDF, with its revisions compared and every sign found, as three values: the PDF, the accounts and the findings."
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
  "Deflate data, fixed codes, of a zero then REPEATS copies of 258 octets back one: tiny, unpacking to REPEATS times 258 octets."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (buffer 0) (count 0))
    (labels ((bits (value n) (dotimes (k n) (setf buffer (logior buffer (ash (ldb (byte 1 k) value) count))) (incf count)
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
    (true (pal-finding findings :look "The file was changed after it was first saved"))
    (true (pal-finding findings :look "Page 1 was changed in revision 2"))
    (true (pal-finding findings :look "names iLovePDF"))
    (true (pal-finding findings :look "names another program in /Producer"))))

(define-test (palimpsest palimpsest-cross-reference-streams)
  ;; objects packed in a Flate object stream, a cross-reference stream with a PNG predictor
  (let* ((packed-objects '((1 . "<< /Type /Catalog /Pages 2 0 R >>")
                           (2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                           (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R >>")
                           (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")))
         (bodies (mapcar (lambda (o) (format nil "~a " (cdr o))) packed-objects))
         (header (format nil "~{~a ~}" (loop for o in packed-objects for k from 0
                                             collect (format nil "~d ~d" (car o) (reduce #'+ (subseq bodies 0 k) :key #'length)))))
         (data (pal-octets header (format nil "~{~a~}" bodies)))
         (out (pal-octets "%PDF-1.5" 10))
         (places '()))
    (push (cons 4 (length out)) places)
    (setf out (pal-octets out (pal-object 4 '(:stream "" "BT /F1 10 Tf 50 50 Td (Packed hello) Tj ET"))))
    (push (cons 6 (length out)) places)
    (setf out (pal-octets out (pal-object 6 (list :stream (format nil "/Type /ObjStm /N 4 /First ~d /Filter /FlateDecode" (length header))
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
      (setf out (pal-octets out (pal-object 7 (list :stream "/Type /XRef /Size 8 /W [1 2 1] /Root 1 0 R /Filter /FlateDecode /DecodeParms << /Predictor 12 /Columns 4 >>"
                                                    (pal-zlib-stored predicted)))
                            (format nil "startxref~%~d~%%%EOF~%" xref-at))))
    (multiple-value-bind (pdf) (pal-read out)
      (let ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
        (is eq :stream (palimpsest::revision-xref-kind revision))
        (is eq :packed (palimpsest::entry-kind (palimpsest::lookup pdf revision 3)))
        (let ((pages (palimpsest::pages-of pdf revision)))
          (is = 1 (length pages))
          (is equal '("Packed hello") (palimpsest::text-lines (palimpsest::marks-of pdf revision (first pages)))))))))

(define-test (palimpsest palimpsest-decoders)
  ;; a stream that would unpack without end is stopped at the limit, said so
  (multiple-value-bind (data why) (tuatara:pdf-stream-octets '(("/Filter" . "/FlateDecode")) (pal-zlib-bomb 4000) :limit (* 256 1024))
    (false data)
    (pal-has why "more than")))

;;; What pages draw

(defun pal-one-page (content &key (fonts "/F1 5 0 R") (more '()))
  "A one-page PDF drawing CONTENT with FONTS, MORE objects beside."
  (pal-save #() (append (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                              '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                              (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << ~a >> >> /Contents 4 0 R >>" fonts))
                              (list 4 :stream "" content)
                              '(5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                              '(6 . "<< /Producer (Kea Writer) >>"))
                        more)))

(define-test (palimpsest palimpsest-content-signs)
  ;; a white box drawn over text: the text named, the operators in the log's evidence
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET 1 1 1 rg 70 695 100 20 re f"))
    (declare (ignore accounts))
    (let ((covered (pal-finding findings :strong "covered by a white shape")))
      (true covered)
      (pal-has (palimpsest::finding-words covered) "Secret 42")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence covered)) "1 1 1 rg")
      (pal-has (format nil "~{~a~%~}" (palimpsest::finding-evidence covered)) "70 695 100 20 re f")
      (true pdf)))
  ;; invisible, tiny and white text, text off the page
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 3 Tr 72 600 Td (Hidden words) Tj 0 Tr ET BT /F1 0.5 Tf 72 500 Td (Tiny print) Tj ET BT 1 1 1 rg /F1 12 Tf 72 400 Td (White words) Tj 0 g ET BT /F1 12 Tf 900 900 Td (Far away) Tj ET"))
    (declare (ignore pdf accounts))
    (true (pal-finding findings nil "holds invisible text: \"Hidden words\""))
    (true (pal-finding findings :look "too small to read"))
    (true (pal-finding findings nil "white text on a white page"))
    (true (pal-finding findings :look "lies outside the page")))
  ;; a line drawn character by character, figures in another font
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           "BT /F1 12 Tf 72 700 Td (The quick brown fox) Tj ET BT /F1 12 Tf 72 680 Td (jumps over the lazy dog) Tj ET "
                                           "BT /F1 12 Tf 72 660 Td (Amount due today) Tj ET BT /F1 12 Tf 72 640 Td (Thank you for your business in 2026) Tj ET "
                                           "BT /F1 12 Tf 72 620 Td (T) Tj (o) Tj (t) Tj (a) Tj (l) Tj (d) Tj (u) Tj (e) Tj (n) Tj (o) Tj (w) Tj ET "
                                           "BT /F1 12 Tf 72 600 Td (Paid on account:) Tj /F2 12 Tf 180 0 Td (950) Tj ET")
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
                              :more (list '(7 . "<< /Type /Font /Subtype /Type0 /BaseFont /ABCDEF+Arial /Encoding /Identity-H /DescendantFonts [9 0 R] /ToUnicode 10 0 R >>")
                                          '(8 . "<< /Type /Font /Subtype /TrueType /BaseFont /GHIJKL+Arial >>")
                                          '(9 . "<< /Type /Font /Subtype /CIDFontType2 /BaseFont /ABCDEF+Arial /W [1 [500 600]] >>")
                                          (list 10 :stream "" "/CIDInit /ProcSet findresource begin 1 begincodespacerange <0000> <FFFF> endcodespacerange 2 beginbfchar <0001> <004B> <0002> <0065> endbfchar end"))))
    (declare (ignore accounts))
    (true (pal-finding findings :look "The font Arial is in the file as 2 subsets (ABCDEF+Arial, GHIJKL+Arial)"))
    (let* ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (is equal '("Ke" "Second") (palimpsest::text-lines (palimpsest::marks-of pdf revision (first (palimpsest::pages-of pdf revision))))))))

(define-test (palimpsest palimpsest-content-appended)
  ;; a later save lays a second content stream over the page
  (let* ((original (pal-original))
         (edited (pal-save original
                           '((3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 7 0 R] >>")
                             (7 :stream "" "1 1 1 rg 70 675 120 20 re f BT /F1 12 Tf 72 680 Td (Total: 50) Tj ET")))))
    (multiple-value-bind (pdf accounts findings) (pal-read edited)
      (declare (ignore pdf))
      (true (pal-finding findings :strong "one of them appended in a later save"))
      (true (pal-finding findings :strong "covered by a white shape drawn over it, added in a later save"))
      (let ((change (first (palimpsest::account-changes (second accounts)))))
        (true (find "Total: 50" (palimpsest::page-change-added change) :key #'palimpsest::mark-text :test #'equal))))))

;;; Metadata, ends

(define-test (palimpsest palimpsest-metadata-and-ends)
  (let* ((xmp (format nil "<?xpacket begin='' id='W5M0MpCehiHzreSzNTczkc9d'?><x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'>~
<rdf:Description xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmlns:xmpMM='http://ns.adobe.com/xap/1.0/mm/' xmlns:stEvt='http://ns.adobe.com/xap/1.0/sType/ResourceEvent#' ~
xmp:CreateDate='2020-03-01T09:00:00+13:00' xmp:CreatorTool='Kea Writer' xmpMM:DocumentID='uuid:kea-1' xmpMM:OriginalDocumentID='uuid:other-9'>~
<xmpMM:History><rdf:Seq><rdf:li stEvt:action='created' stEvt:when='2020-03-01T09:00:00+13:00' stEvt:softwareAgent='Kea Writer'/>~
<rdf:li stEvt:action='saved' stEvt:when='2026-01-05T12:00:00+13:00' stEvt:softwareAgent='Sejda PDF'/></rdf:Seq></xmpMM:History>~
</rdf:Description></rdf:RDF></x:xmpmeta><?xpacket end='w'?>"))
         (octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                     '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                     '(6 . "<< /CreationDate (D:20260301090000+13'00') /ModDate (D:20250101090000+13'00') /Creator (Kea Writer) >>")
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

;;; Signatures

(defun pal-signed (&key (short 0))
  "The invoice signed in its first save, its /ByteRange covering it all but SHORT octets less."
  (let* ((placeholder "0000000000")
         (base (pal-save #() (append (butlast *pal-original-objects*)
                                     (list '(6 . "<< /Producer (Kea Writer 1.0) >>")
                                           (cons 8 (format nil "<< /Type /Sig /Filter /Adobe.PPKLite /SubFilter /adbe.pkcs7.detached /Name (Kea Signer) /M (D:20260102100000+13'00') /ByteRange [0 ~a ~a ~a] /Contents <~a> >>"
                                                           placeholder placeholder placeholder (make-string 64 :initial-element #\0)))
                                           (list 9 :stream "" "BT /F1 12 Tf 72 300 Td (Shadow total: 1) Tj ET")))))
         (text (map 'string #'code-char base))
         (open (position #\< text :start (search "/Contents" text)))
         (close (1+ (position #\> text :start open)))
         (range (format nil "[0 ~10,'0d ~10,'0d ~10,'0d]" open close (- (length base) close short)))
         (at (search "[0 0000000000" text)))
    (pal-octets (subseq base 0 at) range (subseq base (+ at (length range))))))

(define-test (palimpsest palimpsest-signatures)
  ;; signed, then a later save changes the page and brings an object hidden in the signed part into use
  (let ((after (pal-save (pal-signed)
                         '((3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 9 0 R] >>")))))
    (multiple-value-bind (pdf accounts findings) (pal-read after)
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "An approval signature (object 8), dated 2026-01-02"))
      (true (pal-finding findings :strong "after the signature in object 8, changes what the document shows"))
      (true (pal-finding findings :strong "Object 9 was hidden, unused, in the part the signature in object 8 covers"))
      (false (pal-finding findings :strong "stops short"))))
  ;; an approval signature, then a note: annotations after it are allowed
  (let ((noted (pal-save (pal-signed)
                         '((3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R /Annots [10 0 R] >>")
                           (10 . "<< /Type /Annot /Subtype /Text /Rect [72 600 92 620] /Contents (Checked, R Waimate) >>")))))
    (multiple-value-bind (pdf accounts findings) (pal-read noted)
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "do not allow"))
      (true (pal-finding findings :look "after the signature in object 8, adds or changes annotations."))))
  ;; a /ByteRange that stops short of where its save ends
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-signed :short 10))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "stops short"))))

;;; Hostile files

(define-test (palimpsest palimpsest-hostile-files)
  (let ((start (get-internal-real-time)))
    (dolist (octets (list
                     ;; a page tree in a circle, a /Prev naming its own section
                     (pal-save #() '((1 . "<< /Type /Catalog /Pages 2 0 R >>") (2 . "<< /Type /Pages /Kids [2 0 R 3 0 R] /Count 1 >>")
                                     (3 . "<< /Type /Page /Parent 2 0 R /Contents 4 0 R >>") (4 :stream "" "q q q Q BT ET")))
                     (pal-octets "%PDF-1.4" 10 "1 0 obj << /Type /Catalog /Pages 1 0 R >> endobj" 10 "xref" 10 "0 999999999" 10
                                 "0000000009 00000 n" 13 10 "trailer << /Root 1 0 R /Prev 20 >>" 10 "startxref" 10 "20" 10 "%%EOF" 10)
                     ;; offsets out of range, a startxref pointing past the end
                     (pal-octets "%PDF-1.4" 10 "xref" 10 "0 2" 10 "0000000000 65535 f" 13 10 "9999999999 00000 n" 13 10
                                 "trailer << /Root 1 0 R /Size 2 >>" 10 "startxref" 10 "123456789" 10 "%%EOF" 10)
                     ;; an object stream claiming a billion objects, arrays nested deep, no %%EOF
                     (pal-octets "%PDF-1.5" 10 "1 0 obj << /Type /ObjStm /N 1000000000 /First 3 /Length 5 >> stream" 10 "1 0 2" 10 "endstream endobj" 10
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
                                              (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 ~d 792] /Resources << /Font << /F1 ~d 0 R >> >> >>"
                                                      size (+ 11 (* 2 k))))
                                collect (cons (+ 11 (* 2 k)) (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /~a >>" font)))))))

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

;;; The revisions written, their logs, the report

(define-test (palimpsest palimpsest-revisions-and-logs)
  (with-test-directory (dir)
    (let* ((octets (pal-edited))
           (path (namestring (ensure-directories-exist (merge-pathnames "evidence/invoice.pdf" dir))))
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
      (let ((stamp (tuatara.io:path-stamp path))
            (job (palimpsest::make-palimpsest-job nil path folder)))
        (palimpsest::examine job)
        (let* ((analysis (palimpsest::palimpsest-job-analysis job))
               (files (palimpsest::analysis-files analysis))
               (pdf (palimpsest::analysis-pdf analysis))
               (second-revision (aref (palimpsest::pdf-revisions pdf) 1)))
          (is = 2 (length files))
          ;; each revision its octets up to its %%EOF line, read only, in a folder for you alone
          (destructuring-bind (file log sum) (first files)
            (is equalp (subseq octets 0 (palimpsest::revision-end (aref (palimpsest::pdf-revisions pdf) 0))) (tuatara.io:file-octets file))
            (is = #o400 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp file))))
            (is = #o700 (logand #o777 (tuatara.io:stamp-mode (tuatara.io:path-stamp folder))))
            (true (search "/palimpsest/" file))
            (true sum)
            (pal-has (uiop:read-file-string log) "revision-1.pdf SHA-256: "))
          (let ((log (uiop:read-file-string (second (second files)))))
            ;; how the revision was found, with its offsets in hex and decimal
            (pal-has log (format nil "Its %%EOF is at ~a" (palimpsest::offset-words (palimpsest::revision-eof second-revision))))
            (pal-has log (format nil "Its trailer's /Prev is ~d" (palimpsest::revision-xref-at (aref (palimpsest::pdf-revisions pdf) 0))))
            (pal-has log "the cross-reference section of revision 1 (the original)")
            (pal-has log (format nil "This revision is the source file's octets from 0x00000000 to ~a" (palimpsest::hex-offset (1- (length octets)))))
            ;; each sign with its rule, the octets it rests on and the steps to see it
            (pal-has log "Worth a look: /Producer names iLovePDF")
            (pal-has log "2F 50 72 6F 64 75 63 65 72")
            (pal-has log "text: /Producer (iLovePDF)")
            (pal-has log "Steps to see it by hand:")
            ;; what changed, both places of each object, and the text found in the decoded stream
            (pal-has log (format nil "object 4 now at ~a, before at ~a"
                                 (palimpsest::offset-words (palimpsest::pdf-indirect-at (palimpsest::revision-object pdf second-revision 4)))
                                 (palimpsest::offset-words (palimpsest::pdf-indirect-at (palimpsest::revision-object pdf (aref (palimpsest::pdf-revisions pdf) 0) 4)))))
            (pal-has log "text - Total: 100")
            (pal-has log "text + Total: 900")
            (pal-has log "(Total: 900) Tj")
            ;; the same file, the same log but its date line
            (let ((again (palimpsest::revision-log analysis second-revision (third (second files)))))
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
          (let ((text (palimpsest::object-text analysis second-revision 4)))
            (pal-has text "Object 4 0 begins at")
            (pal-has text "(Total: 900) Tj"))
          (pal-has (palimpsest::object-text analysis second-revision 4 :hex t) "In hex: *Palimpsest: invoice.pdf object 4 stream, revision 2*")
          (is equalp (pal-octets "BT /F1") (subseq (palimpsest::object-stream-shown analysis second-revision 4) 0 6))
          ;; saved where asked, each copy's sum its original's; never beside the evidence, in the cache, or over a file
          (pal-has (palimpsest::save-refusal analysis (namestring (merge-pathnames "evidence/" dir))) "the folder of the PDF examined")
          ;; nor beside it through a link to its folder
          (let ((link (namestring (merge-pathnames "evidence-link" dir))))
            (tuatara.io:make-symbolic-link (string-right-trim "/" (namestring (merge-pathnames "evidence/" dir))) link)
            (pal-has (palimpsest::save-refusal analysis (concatenate 'string link "/")) "the folder of the PDF examined")
            (tuatara.io:remove-file link))
          (pal-has (palimpsest::save-refusal analysis folder) "within palimpsest's own folder in the cache")
          (let* ((kept (namestring (merge-pathnames "kept/" dir)))
                 (saved (palimpsest::save-copies analysis kept)))
            (is equal '("revision-1.pdf" "revision-1.log" "revision-2.pdf" "revision-2.log" "invoice-revision-report.txt" "invoice-fingerprint.txt") (mapcar #'first saved))
            (loop for (name sum) in saved
                  for source in (list (first (first files)) (second (first files)) (first (second files)) (second (second files)))
                  do (is string= (tuatara:file-sha256 source) sum)
                     (is string= sum (tuatara:file-sha256 (tuatara.io:path-join kept name))))
            (is equalp (tuatara.io:file-octets (first (second files))) (tuatara.io:file-octets (tuatara.io:path-join kept "revision-2.pdf")))
            (let ((report (uiop:read-file-string (tuatara.io:path-join kept "invoice-revision-report.txt"))))
              (pal-has report "Palimpsest: invoice.pdf")
              (pal-has report (format nil "Saved to ~a" kept))
              (pal-has report (format nil "~a  ~arevision-2.log" (second (fourth saved)) kept)))
            (is string= kept (palimpsest::analysis-saved analysis))
            (pal-has (palimpsest::save-refusal analysis kept) "revision-1.pdf, revision-1.log, revision-2.pdf, revision-2.log, invoice-revision-report.txt, invoice-fingerprint.txt are there already"))
          ;; the PDF itself never touched
          (is equalp octets (tuatara.io:file-octets path))
          (is = (tuatara.io:stamp-mtime stamp) (tuatara.io:stamp-mtime (tuatara.io:path-stamp path))))
        (tuatara.io:delete-tree folder)))))

(define-test (palimpsest palimpsest-evidence-not-remembered)
  ;; the PDF examined and the folder of cases grouped are evidence: neither becomes the path
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
      (tuatara.frontend.fake:fake-queue-keys f (format nil "M-x ~a RET C-a C-k ~a RET" (rimu-typed "palimpsest") (rimu-typed path)))
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
        (let ((folder (tuatara:buffer-private-folder buffer)))
          (true (probe-file (tuatara.io:path-join folder "revision-2.log")))
          ;; closing it with nothing saved asks first: n keeps it, y closes it and deletes what it wrote
          (flet ((close-answering (answer)
                   (let ((f (tuatara.frontend.fake:make-fake-frontend :width 140 :height 40)))
                     (tuatara.frontend.fake:fake-queue-keys f (format nil "M-x ~a RET ~a RET" (rimu-typed "kill-buffer") answer))
                     (tuatara.command:run-editor f :setup #'tuatara.defaults:install-defaults :buffer buffer)
                     f)))
            (close-answering "n")
            (true (tuatara.buffer:live-buffer-p buffer))
            (true (probe-file folder))
            (close-answering "y")
            (false (tuatara.buffer:live-buffer-p buffer)))
          (loop repeat 200 while (probe-file folder) do (sleep 0.02))
          (false (probe-file folder))))))
  (true (fboundp 'palimpsest::palimpsest))
  (is eq 'palimpsest::palimpsest-show-object
      (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'palimpsest::palimpsest-mode) (tuatara.frontend:parse-key-sequence "o")))
  (is eq 'palimpsest::palimpsest-save
      (tuatara.command:lookup-key (tuatara.modes:mode-keymap 'palimpsest::palimpsest-mode) (tuatara.frontend:parse-key-sequence "s"))))

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
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/")))
           (again-folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets o))
      (unwind-protect
          (let ((job (palimpsest::make-palimpsest-job nil path folder)))
            (palimpsest::examine job)
            (let* ((analysis (palimpsest::palimpsest-job-analysis job))
                   (first-end (palimpsest::revision-end (aref (palimpsest::pdf-revisions (palimpsest::analysis-pdf analysis)) 0))))
              (true (palimpsest::write-revision analysis 1 target))
              ;; the report alone, under the name it is offered
              (is string= "invoice-revision-report.txt" (palimpsest::report-file-name analysis))
              (let ((report (concatenate 'string out "invoice-revision-report.txt")))
                (setf (palimpsest::analysis-shown analysis) (multiple-value-list (palimpsest::pieces-text (palimpsest::report-pieces analysis))))
                (true (palimpsest::write-report analysis report))
                (pal-has (uiop:read-file-string report) "* Summary"))
              (is equalp (subseq octets 0 first-end) (tuatara.io:file-octets target))
              (pal-has (uiop:read-file-string (concatenate 'string out "invoice-revision-1.log")) "Palimpsest log of revision 1")
              (pal-has (palimpsest::revision-refusal analysis target) "there already")
              ;; never beside the PDF examined, the PDF itself untouched
              (pal-has (palimpsest::revision-refusal analysis (namestring (merge-pathnames "evidence/invoice-revision-1.pdf" dir))) "the folder of the PDF examined")
              (is equalp octets (tuatara.io:file-octets path))
              ;; the version written holds one save: nothing before it
              (let ((again (palimpsest::make-palimpsest-job nil target again-folder)))
                (palimpsest::examine again)
                (is = 1 (length (palimpsest::analysis-files (palimpsest::palimpsest-job-analysis again))))
                (false (palimpsest::revision-files (palimpsest::palimpsest-job-analysis again) 2)))))
        ;; its private folders go with it
        (tuatara.io:delete-tree folder)
        (tuatara.io:delete-tree again-folder)))))

(define-test (palimpsest palimpsest-revision-meant)
  ;; in a revision opened from the report, the revision meant is that one
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "invoice.pdf" dir)))
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/")))
           (tuatara.command:*editor* (nth-value 1 (run-keys "" :text ""))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) o))
      (let ((job (palimpsest::make-palimpsest-job nil path folder))
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
            (tuatara.io:delete-tree folder))))))
  ;; in the report, the revision whose part point is in, told by the report's own
  ;; headings: a PDF that writes one into what it says of itself does not move it
  (with-test-directory (dir)
    (let* ((path (namestring (merge-pathnames "invoice.pdf" dir)))
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/")))
           (tuatara.command:*editor* (nth-value 1 (run-keys "" :text ""))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (pal-save (pal-original) (list (cons 6 (format nil "<< /Producer (Kea~%** Revision 1: 0x0 to 0x1, 1 octets~%) >>")))) o))
      (let ((job (palimpsest::make-palimpsest-job nil path folder))
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
          (tuatara.io:delete-tree folder))))))

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
  ;; a page naming one stream that unpacks to 1 MB three hundred times: joined only as far as one stream may decode to, and said
  (let ((octets (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                    '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                    (cons 3 (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Contents [~{~a~^ ~}] >>"
                                                    (loop repeat 300 collect "7 0 R")))
                                    (list 7 :stream "/Filter /FlateDecode" (pal-zlib-bomb 4000)))))
        (tuatara-user:*palimpsest-largest-stream* (* 2 1024 1024)))
    (let ((pdf (palimpsest::read-pdf octets)) (problems nil))
      (multiple-value-bind (revision page) (pal-first-page pdf)
        (true (< (pal-consed (lambda () (setf problems (nth-value 1 (palimpsest::page-marks pdf revision page)))))
                 (* 32 1024 1024)))
        (true (find-if (lambda (p) (search "content streams together decode to more than" p)) problems))))))

(define-test (palimpsest palimpsest-decoded-in-all-bounded)
  ;; a form unpacking to 1 MB drawn a hundred times: what is decoded and read in all held to *palimpsest-most-decoded*, and noted
  (let ((octets (pal-page-of "/MediaBox [0 0 612 792] /Resources << /XObject << /X1 7 0 R >> >> /Contents 4 0 R"
                             (format nil "~{~a~^ ~}" (loop repeat 100 collect "/X1 Do"))
                             :more (list (list 7 :stream "/Type /XObject /Subtype /Form /BBox [0 0 10 10] /Filter /FlateDecode" (pal-zlib-bomb 4000)))))
        (tuatara-user:*palimpsest-most-decoded* (* 8 1024 1024)))
    (let ((pdf (palimpsest::read-pdf octets)))
      (multiple-value-bind (revision page) (pal-first-page pdf)
        (palimpsest::page-marks pdf revision page))
      (true (find-if (lambda (n) (search "(*palimpsest-most-decoded*)" n)) (palimpsest::pdf-notes pdf)))
      (true (<= (getf (palimpsest::pdf-cache pdf) :decoded-read 0) (* 10 1024 1024))))))

(define-test (palimpsest palimpsest-objects-scanned-in-linear-time)
  ;; finding the objects of the body takes time in proportion to their number
  (flet ((seconds-per-object (count)
           (let* ((octets (pal-octets "%PDF-1.4" 10 (with-output-to-string (s) (dotimes (k count) (format s "~d 0 obj null endobj~%" (1+ k))))))
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
  (let* ((xmp "<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmp:CreatorTool='Kea Writer'/></rdf:RDF></x:xmpmeta>")
         (original (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Metadata 7 0 R >>")
                                       '(2 . "<< /Type /Pages /Kids [] /Count 0 >>")
                                       (list 7 :stream "/Type /Metadata /Subtype /XML" xmp))))
         (pdf (palimpsest::read-pdf (pal-save original '((6 . "<< /Producer (Kea Writer) >>")))))
         (first-tree (nth-value 1 (palimpsest::xmp-of pdf (aref (palimpsest::pdf-revisions pdf) 0)))))
    (true first-tree)
    (is eq first-tree (nth-value 1 (palimpsest::xmp-of pdf (aref (palimpsest::pdf-revisions pdf) 1))))))

(defun pal-pictures (count)
  "A one-page PDF placing COUNT pictures alike, and one more of another bit depth and resolution, object 10."
  (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /XObject << ~{/Im~d ~:*~d 0 R ~} >> >> /Contents 4 0 R"
                       (loop for k from 10 to (+ 10 count) collect k))
               (format nil "~{q 72 0 0 72 0 0 cm /Im~d Do Q ~}" (loop for k from 10 to (+ 10 count) collect k))
               :more (loop for k from 10 to (+ 10 count)
                           collect (list k :stream (format nil "/Type /XObject /Subtype /Image /Width ~d /Height 100 /ColorSpace /DeviceGray /BitsPerComponent ~d"
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
    (true (pal-finding findings :look "(object 10) on page 1 has a bit depth (1)"))
    (true (pal-finding findings :look "(object 10) on page 1 is placed at about 1000 pixels"))
    (is = 2 (length findings)))
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
                    (palimpsest::make-glyph :x0 10 :y0 10 :x1 (+ 10 w) :y1 (+ 10 h) :page (next pages) :ink (+ 1000 (next inks)))))))

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
                                          (let ((da (abs (- (palimpsest::glyph-ink a) ink))) (db (abs (- (palimpsest::glyph-ink b) ink))))
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
        (true (<= large-octets (max 64 (* 3/2 small-octets))) "~,1f octets of garbage for each character among 4,000, ~,1f among 500"
              large-octets small-octets)))))

(defun pal-rows (count)
  "A one-page PDF of COUNT rows, each a line of text and a black box beside it, apart."
  (pal-page-of (format nil "/MediaBox [0 0 612 ~d] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R" (* 20 (1+ count)))
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
                             "BT /F1 12 Tf 72 700 Td (Kea 7) Tj ET 1 1 1 rg 70 695 100 20 re f 0 0 1 rg 70 495 100 20 re f 1 1 1 rg BT /F1 12 Tf 72 500 Td (Seen) Tj ET"))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape"))
    (false (pal-finding findings nil "white text on a white page"))))

(define-test (palimpsest palimpsest-odd-structures-noted)
  ;; a cross-reference stream whose /W is not an array: noted, the file still examined
  (let* ((head (pal-octets "%PDF-1.5" 10 "1 0 obj << /Type /Catalog >> endobj" 10))
         (at (length head))
         (octets (pal-octets head "2 0 obj << /Type /XRef /W 5 /Size 3 /Root 1 0 R /Length 3 >> stream" 10 "abc" 10 "endstream endobj" 10
                             (format nil "startxref~%~d~%%%EOF~%" at))))
    (multiple-value-bind (pdf accounts findings) (pal-read octets)
      (declare (ignore accounts))
      (true (plusp (length (palimpsest::pdf-revisions pdf))))
      (true (listp findings))))
  ;; a file of its header alone
  (is string= "1.4" (palimpsest::pdf-version (palimpsest::read-pdf (pal-octets "%PDF-1.4")))))

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
                                      '((6 . "<< /Producer (Kea Writer 1.0) /CreationDate (D:20260101090000-13'00') >>")))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "a time zone no place uses")))
  ;; a page of no size is not taken for a scan of itself
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-page-of "/MediaBox [0 0 0 0] /Resources << /Font << /F1 5 0 R >> /XObject << /Im1 7 0 R >> >> /Contents 4 0 R"
                             "q 100 0 0 100 0 0 cm /Im1 Do Q BT 3 Tr /F1 12 Tf 10 10 Td (Hidden) Tj ET"
                             :more '((7 :stream "/Type /XObject /Subtype /Image /Width 10 /Height 10 /ColorSpace /DeviceGray /BitsPerComponent 8" "x"))))
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
  ;; saving, writing a revision and writing the report refuse the PDF's folder and palimpsest's own, links followed;
  ;; each offers a place outside them; the workings let go of once the report is made
  (with-test-directory (dir)
    (let* ((path (namestring (ensure-directories-exist (merge-pathnames "evidence/invoice.pdf" dir))))
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/")))
           (to-cache (namestring (merge-pathnames "to-cache" dir)))
           (to-evidence (namestring (merge-pathnames "to-evidence" dir))))
      (with-open-file (o path :direction :output :element-type '(unsigned-byte 8)) (write-sequence (pal-edited) o))
      (tuatara.io:make-symbolic-link (string-right-trim "/" folder) to-cache)
      (tuatara.io:make-symbolic-link (string-right-trim "/" (namestring (merge-pathnames "evidence/" dir))) to-evidence)
      (unwind-protect
           (let ((job (palimpsest::make-palimpsest-job nil path folder)))
             (palimpsest::examine job)
             (let* ((analysis (palimpsest::palimpsest-job-analysis job))
                    (pdf (palimpsest::analysis-pdf analysis)))
               (is = 0 (hash-table-count (palimpsest::pdf-decoded pdf)))
               (false (getf (palimpsest::pdf-cache pdf) :marks))
               (dolist (place (list (namestring (merge-pathnames "evidence/" dir)) (concatenate 'string to-evidence "/")))
                 (pal-has (palimpsest::save-refusal analysis place) "the folder of the PDF examined")
                 (pal-has (palimpsest::revision-refusal analysis (concatenate 'string place "invoice-revision-1.pdf")) "the folder of the PDF examined")
                 (pal-has (palimpsest::folder-refusal analysis place) "the folder of the PDF examined"))
               ;; nor a folder within the evidence's, there or yet to be made
               (pal-has (palimpsest::save-refusal analysis (format nil "~aevidence/reports/" (namestring dir))) "within the folder of the PDF examined")
               (pal-has (palimpsest::save-refusal analysis (concatenate 'string to-evidence "/reports/")) "within the folder of the PDF examined")
               (pal-has (palimpsest::save-refusal analysis (concatenate 'string to-cache "/")) "within palimpsest's own folder in the cache")
               (pal-has (palimpsest::revision-refusal analysis (concatenate 'string to-cache "/invoice-revision-1.pdf")) "within palimpsest's own folder in the cache")
               (is string= "~/invoice-revision-report.txt" (palimpsest::offered-path analysis "invoice-revision-report.txt"))))
        (tuatara.io:remove-file to-cache)
        (tuatara.io:remove-file to-evidence)
        (tuatara.io:delete-tree folder)))))

;;; Findings that read a page, an action or a save rightly worded

(define-test (palimpsest palimpsest-text-fitted-into-a-gap)
  ;; a line whose characters are moved apart, the rest of the page not: named, and the page's other checks kept
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders Ltd) Tj ET BT /F1 12 Tf 72 680 Td (Thank you for your business) Tj ET "
                                           "BT /F1 12 Tf 72 660 Td [(T) -400 (o) -400 (t) -400 (a) -400 (l) -400 (:) -400 (9) -400 (0) -400 (0)] TJ ET "
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
                                (3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R /Annots [7 0 R] >>")
                                (4 :stream "" "BT /F1 12 Tf 72 700 Td (Pay online) Tj ET")
                                (5 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>")
                                (6 . "<< /Producer (Kea Writer) >>")
                                (7 . "<< /Type /Annot /Subtype /Link /Rect [72 690 300 710] /A << /S /URI /URI (https://example.org/pay) >> >>")
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
    (true (find "In revision 1, page 1's content stream, object 9, is not in the file" (palimpsest::pdf-notes pdf) :test #'string=))))

(define-test (palimpsest palimpsest-findings-worded)
  ;; dates that disagree, in the plural
  (let ((xmp "<x:xmpmeta xmlns:x='adobe:ns:meta/'><rdf:RDF xmlns:rdf='http://www.w3.org/1999/02/22-rdf-syntax-ns#'><rdf:Description xmlns:xmp='http://ns.adobe.com/xap/1.0/' xmp:ModifyDate='2026-01-01T09:00:00+13:00'/></rdf:RDF></x:xmpmeta>"))
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
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (pal-octets (format nil "%PDF-1.4~%1 0 obj~%<< /Type /Catalog /Pages 2 0 R >>~%endobj~%2 0 obj~%<< /Type /Pages /Kids [] /Count 0 >>~%endobj~%trailer~%<< /Root 1 0 R >>~%%%EOF~%")) out))
      (let ((job (palimpsest::make-palimpsest-job nil path folder)))
        (palimpsest::examine job)
        (let* ((analysis (palimpsest::palimpsest-job-analysis job))
               (report (palimpsest::pieces-text (palimpsest::report-pieces analysis)))
               (problem (find-if (lambda (f) (search "startxref" (palimpsest::finding-words f))) (palimpsest::analysis-findings analysis))))
          (pal-has report "Trailer     /Root object 1")
          (true problem)
          (pal-has (format nil "~{~a~%~}" (palimpsest::finding-steps problem)) "no startxref stands before it")))
      (tuatara.io:delete-tree folder))))

(define-test (palimpsest palimpsest-page-numbers-bounded)
  ;; matrices of the largest whole numbers a file may write, again and again: read at once, the text after them still found
  (flet ((page-text (operator count)
           (let* ((pdf (pal-read (pal-one-page (format nil "~v@{~a ~:*~}BT /F1 12 Tf 72 700 Td (Still read) Tj ET" count operator))))
                  (last (palimpsest::last-revision pdf))
                  (marks (palimpsest::page-marks pdf last (first (palimpsest::pages-of pdf last)))))
             (values (find "Still read" marks :key #'palimpsest::mark-text :test #'equal) pdf))))
    (let ((start (get-internal-real-time)))
      (multiple-value-bind (mark pdf) (page-text "99999999999999999999999999999999 0 0 99999999999999999999999999999999 0 0 cm" 1000)
        (true mark)
        (true (typep (palimpsest::mark-x0 mark) 'double-float))
        (false (find-if (lambda (n) (search "could not" n)) (palimpsest::pdf-notes pdf))))
      (true (< (- (get-internal-real-time) start) (* 2 internal-time-units-per-second))
            "~,2f s to read 1,000 matrices of large numbers" (/ (- (get-internal-real-time) start) internal-time-units-per-second)))
    ;; numbers with a point, which no double holds once multiplied a few times over
    (true (page-text "9999999999999999999999999999.9 0 0 9999999999999999999999999999.9 0 0 cm" 50))))

(define-test (palimpsest palimpsest-page-stops-when-asked)
  ;; a page's operators followed in a job asked to stop: it stops there
  (let ((tuatara.runtime::*job* (palimpsest::make-palimpsest-job nil "/nonexistent/a.pdf" "/nonexistent/"))
        (seen 0))
    (setf (car (tuatara.runtime:job-cancel tuatara.runtime::*job*)) t)
    (fail (palimpsest::content-tokens (pal-octets (format nil "~v@{~a ~:*~}" 10000 "0 0 m"))
                                      (lambda (&rest operator) (declare (ignore operator)) (incf seen)))
          'tuatara.runtime:job-stopped)
    (true (< seen 10000))))

(define-test (palimpsest palimpsest-kept-text-shows-controls)
  ;; a /Producer holding a terminal's clipboard and title sequences: the log and report kept show them, never hold them
  (with-test-directory (dir)
    (let* ((producer (format nil "Kea~c]52;c;aGk=~cWriter~c]0;title~c" (code-char 27) (code-char 7) (code-char 27) (code-char 7)))
           (octets (pal-save (pal-original) (list (cons 6 (format nil "<< /Producer (~a) /Creator (Kea Writer) >>" producer)))))
           (path (namestring (merge-pathnames "invoice.pdf" dir)))
           (folder (tuatara.defaults::new-private-folder (tuatara.defaults::private-cache-directory "palimpsest/"))))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8)) (write-sequence octets out))
      (let ((job (palimpsest::make-palimpsest-job nil path folder)))
        (palimpsest::examine job)
        (let ((analysis (palimpsest::palimpsest-job-analysis job))
              (target (namestring (merge-pathnames "report.txt" dir))))
          (palimpsest::write-report analysis target)
          (dolist (text (list (uiop:read-file-string (second (second (palimpsest::analysis-files analysis))))
                              (uiop:read-file-string target)))
            (pal-has text "Kea^[]52;c;aGk=^GWriter")
            (false (find-if (lambda (c) (member (char-code c) '(7 27))) text)))))
      (tuatara.io:delete-tree folder))))

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
           (pal-one-page (format nil "BT /F1 12 Tf 72 720 Td (Invoice for Kea Builders Ltd) Tj ET BT ~a/F1 12 Tf 72 700 Td (Amount payable to Kea Builders Ltd) Tj ~a( by Friday) Tj ~aET"
                                 before odd after))))
    (loop for (odd before after words detail)
            in '(("/F1 11 Tf " "" "" "in another size than the line around it" "\"by Friday\" at 11.0 pt, its line at 12.0 pt (4 @73 font 5)")
                 ("2 Tr " "" "0 Tr " "drawn bolder or lighter" "\"by Friday\" filled and outlined, its line filled (4 @73 font 5)")
                 ("90 Tz " "" "100 Tz " "set narrower or wider" "\"by Friday\" at 90 % of its width, its line at 100 % (4 @73 font 5)")
                 ("0.6 Tc " "" "0 Tc " "characters spaced unlike" "\"by Friday\" with 0.05 of its size after each character, its line 0.00 (4 @73 font 5)")
                 ("0 g " "0.2 g " "" "in another black" "\"by Friday\" in grey 0.00, its line in grey 0.20 (4 @79 font 5)"))
          do (multiple-value-bind (pdf accounts findings) (pal-read (line odd before after))
               (declare (ignore pdf accounts))
               (let ((finding (pal-finding findings :look words)))
                 (true finding "~a: ~s" words (mapcar #'palimpsest::finding-words findings))
                 (when finding (is equal (list detail) (palimpsest::finding-details finding)))))))
  ;; set alike, a superscript, words spread by word spacing, a bold font of the family: nothing said
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           "BT /F1 12 Tf 72 720 Td (Invoice for Kea Builders Ltd) Tj ET "
                                           "BT /F1 12 Tf 72 700 Td (Amount payable to Kea Builders) Tj /F1 7 Tf 2 Ts (note) Tj 0 Ts ET "
                                           "BT /F1 12 Tf 2 Tw 72 680 Td (Paid in full on the day) Tj ET "
                                           "BT /F1 12 Tf 72 660 Td (Thank you for your business) Tj /F2 12 Tf ( Kea) Tj ET")
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
                      '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 7 0 R >> >> /Contents 4 0 R >>")
                      '(4 :stream "" "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET")
                      '(6 . "<< /Producer (Kea Writer) /CreationDate (D:20260101090000Z) >>")
                      '(7 . "<< /Type /Font /Subtype /TrueType /BaseFont /ABCDEF+ArialMT /FontDescriptor 8 0 R >>")
                      '(8 . "<< /Type /FontDescriptor /FontName /ABCDEF+ArialMT /FontFile2 9 0 R >>")
                      (list 9 :stream "" (pal-font-file changed)))))

(define-test (palimpsest palimpsest-fonts-named-and-dated)
  ;; one family named as two programs name it on one page; one program's names for its styles are not
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F2 12 Tf 72 680 Td (Total: 900) Tj ET"
                              :fonts "/F1 7 0 R /F2 8 0 R"
                              :more '((7 . "<< /Type /Font /Subtype /TrueType /BaseFont /ArialMT >>")
                                      (8 . "<< /Type /Font /Subtype /TrueType /BaseFont /Arial,Bold >>"))))
    (declare (ignore pdf accounts))
    (let ((named (pal-finding findings :look "On page 1 one font is named the ways two programs name it (ArialMT, Arial,Bold)")))
      (true named "~s" (mapcar #'palimpsest::finding-words findings))
      (when named (is equal '("F1 as ArialMT" "F2 as Arial,Bold") (palimpsest::finding-details named)))))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Invoice for Kea Builders) Tj ET BT /F2 12 Tf 72 680 Td (Total: 900) Tj ET"
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

(define-test (palimpsest palimpsest-annotations-compared)
  ;; a later save that only lays annotations over the page, its content untouched: the page set beside the
  ;; one before shows what was laid over it, as it shows text
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-save (pal-original)
                          '((3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents 4 0 R /Annots [14 0 R 15 0 R 16 0 R] >>")
                            (14 . "<< /Type /Annot /Subtype /Square /Rect [70 675 230 693] /C [1 1 1] /IC [1 1 1] /F 4 >>")
                            (15 . "<< /Type /Annot /Subtype /FreeText /Rect [72 677 230 693] /Contents (Total: $3,250.00) /DA (/Helv 12 Tf 0 g) /F 4 >>")
                            (16 . "<< /Type /Annot /Subtype /Stamp /Rect [380 700 520 760] /Name /Approved /Contents (APPROVED) /F 4 >>"))))
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
  "The grey octets of a scanned page, 600 by 500 pixels: 12 lines of 6 words,
each word strokes 3 pixels wide whose core is grey 60 and whose edges fade
through 140, on paper of grey 235 straying by up to 10; with PASTED, the
third word of the fifth line crisp, grey 0 with no fading, on clean paper of
grey 250; with LOWERED, that word 4 pixels lower than its line; TILT, the
page's slope, its rows falling that many pixels for each pixel across."
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

(defun pal-scanned (grey &key (width 600) (height 500))
  "A one-page PDF of the scanned page GREY, raw grey pixels laid over all of the page."
  (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                      '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] /Resources << /XObject << /Im1 7 0 R >> >> /Contents 4 0 R >>")
                      '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                      '(6 . "<< /Producer (Kea Scan) >>")
                      (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width ~d /Height ~d /ColorSpace /DeviceGray /BitsPerComponent 8" width height) grey))))

(define-test (palimpsest palimpsest-scanned-word-pasted)
  ;; a word laid crisp and black on clean paper over a scan is named, with its place on the page; the scan untouched, nothing
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :pasted t)))
    (declare (ignore pdf accounts))
    (let ((pasted (pal-finding findings :look "has 1 word unlike the rest of it")))
      (true pasted "~s" (mapcar #'palimpsest::finding-words findings))
      (when pasted
        (pal-has (palimpsest::finding-words pasted) "darker ink")
        (pal-has (palimpsest::finding-words pasted) "cleaner paper")
        (pal-has (first (palimpsest::finding-details pasted)) "pixels 210,172 to")
        (pal-has (first (palimpsest::finding-details pasted)) "ink grey 0, the page's 60"))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))))

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
    (false (pal-finding findings nil "characters of the scanned page") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; the characters keep cuts of their own, not the page: 648 strokes, 22 by 9 pixels each, of a page of 300,000
  (multiple-value-bind (words tilt glyphs) (palimpsest::picture-words (pal-grained (pal-scan)) 600 500)
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
  ;; comparison reaches; a copy with its grey levels moved found by the comparison, nearer than prints; the
  ;; other prints of the same stroke no copies, nor a mark of one ink without grain printed the same twice
  (let* ((a (pal-grained (pal-scan)))
         (b (pal-grained (pal-scan) 4242)))
    (pal-copy-box a a 114 58 233 172)
    (pal-copy-box a a 19 96 344 286 t)
    (pal-copy-box a b 415 362 550 400)
    ;; a mark of one ink, no grain within it, printed the same on both pages
    (dolist (grey (list a b))
      (dotimes (y 8) (dotimes (x 4) (setf (aref grey (+ (* (+ 255 y) 600) 92 x)) 0))))
    (let ((glyphs (loop for (grey page picture) in (list (list a 0 7) (list b 5 8))
                        append (let ((glyphs (nth-value 2 (palimpsest::picture-words grey 600 500))))
                                 (dolist (g glyphs glyphs) (setf (palimpsest::glyph-page g) page (palimpsest::glyph-picture g) picture))))))
      (multiple-value-bind (copies alike) (palimpsest::copied-glyphs glyphs)
        (flet ((group (page x y)
                 (find-if (lambda (c) (find-if (lambda (g) (and (= page (palimpsest::glyph-page g)) (= x (palimpsest::glyph-x0 g)) (= y (palimpsest::glyph-y0 g))))
                                               (first c)))
                          copies))
               (places (c) (sort (mapcar (lambda (g) (list (palimpsest::glyph-page g) (palimpsest::glyph-x0 g) (palimpsest::glyph-y0 g))) (first c))
                                 #'< :key #'second)))
          (true (and alike (>= alike 4)) "~s" alike)
          (is = 3 (length copies) "~s" (mapcar #'places copies))
          (is equal '((0 114 58) (0 233 172)) (places (group 0 114 58)))
          (is eql 0 (second (group 0 114 58)))
          (is equal '((0 415 362) (5 550 400)) (places (group 5 550 400)))
          (is eql 0 (second (group 5 550 400)))
          (is equal '((0 19 96) (0 344 286)) (places (group 0 19 96)))
          (true (and (group 0 19 96) (< 0 (second (group 0 19 96)) (/ alike 2))) "~s" (group 0 19 96))
          (false (group 0 92 255))
          (let ((marks (remove-if-not (lambda (g) (and (= 92 (palimpsest::glyph-x0 g)) (= 255 (palimpsest::glyph-y0 g)))) glyphs)))
            (is = 2 (length marks))
            (true (palimpsest::same-pixels-p (first marks) (second marks)))
            (true (palimpsest::glyph-bare (first marks))))))))
  ;; the same pixels told by every grey level, not by the hash alone: one level apart in each column, no copies
  (let* ((grey (pal-grained (pal-scan)))
         (glyphs (nth-value 2 (palimpsest::picture-words (pal-copy-box grey grey 114 58 233 172) 600 500)))
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

(define-test (palimpsest palimpsest-likeness-alone-worth-a-look)
  ;; a stroke laid over another with a third of its grey levels moved by one, on a page kept without loss:
  ;; found by its likeness, far nearer than prints, worth a look and never a strong sign; laid pixel for
  ;; pixel, strong
  (let ((page (pal-grained (pal-scan))))
    (pal-copy-box page page 19 96 344 286)
    ;; every third pixel of the copy a grey level lighter: no strip the same, its ink nearer than prints are
    (dotimes (y 18)
      (dotimes (x 5)
        (when (zerop (mod (+ (* 5 y) x) 3))
          (let ((at (+ (* (+ 286 y) 600) 344 x))) (setf (aref page at) (min 255 (1+ (aref page at))))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "characters of the scanned page") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :look "2 characters of the scanned page are nearer one another than prints")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  (let ((page (pal-grained (pal-scan))))
    (pal-copy-box page page 19 96 344 286)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

;; JPEG's luminance table, as its standard gives it, row by row
(defparameter *pal-jpeg-table*
  #(16 11 10 16 24 40 51 61  12 12 14 19 26 58 60 55  14 13 16 24 40 57 69 56  14 17 22 29 51 87 80 62
    18 22 37 56 68 109 103 77  24 35 55 64 81 104 113 92  49 64 78 87 103 121 120 101  72 92 95 98 112 100 103 99)
  "The luminance table of the JPEG standard, row by row, that PAL-JPEG-SAVED scales.")

(defun pal-jpeg-saved (grey x0 y0 w h &key (quality 75) (width 600))
  "GREY (WIDTH wide), with the region from X0 Y0, W by H pixels, saved as
a grey JPEG of QUALITY on a grid of its own and opened again, as a JPEG
keeps it: each block of 8 by 8 from X0 Y0 turned into its frequencies,
each divided by the standard's table scaled for QUALITY, made whole and
multiplied again, and turned back into whole grey levels; a block past the
region's edge filled out with the region's last pixels. GREY, changed."
  (let ((c (make-array '(8 8))) (scale (if (< quality 50) (floor 5000 quality) (- 200 (* 2 quality)))))
    (dotimes (u 8) (dotimes (x 8) (setf (aref c u x) (* (if (zerop u) (sqrt 1/8) 1/2) (cos (/ (* (+ (* 2 x) 1) u pi) 16))))))
    (loop for by from 0 below h by 8
          do (loop for bx from 0 below w by 8
                   do (let ((block (make-array '(8 8))) (f (make-array '(8 8))))
                        (dotimes (y 8)
                          (dotimes (x 8)
                            (setf (aref block y x) (- (aref grey (+ (* (+ y0 (min (+ by y) (1- h))) width) x0 (min (+ bx x) (1- w)))) 128))))
                        (dotimes (v 8)
                          (dotimes (u 8)
                            (let ((q (max 1 (min 255 (floor (+ (* (aref *pal-jpeg-table* (+ (* v 8) u)) scale) 50) 100)))))
                              (setf (aref f v u) (* q (round (loop for y below 8 sum (loop for x below 8 sum (* (aref c v y) (aref c u x) (aref block y x)))) q))))))
                        (dotimes (y 8)
                          (dotimes (x 8)
                            (when (and (< (+ by y) h) (< (+ bx x) w))
                              (setf (aref grey (+ (* (+ y0 by y) width) x0 bx x))
                                    (max 0 (min 255 (round (+ 128 (loop for v below 8 sum (loop for u below 8 sum (* (aref c v y) (aref c u x) (aref f v u))))))))))))))))
  grey)

(defun pal-noisy-scan (&key (sigma 2) (ink-sigma sigma) (paper 235) (core 60) (edge 140) bold (seed 4242))
  "The grey octets of a scanned page as PAL-SCAN draws it, 600 by 500
pixels, with a scanner's grain of SIGMA grey levels (Gaussian, seeded by
SEED) on its paper and INK-SIGMA on its ink; PAPER, CORE and EDGE its
greys; with BOLD, the first line set in four strokes BOLD pixels wide a
word, as a heading is."
  (let ((g (make-array (* 600 500) :initial-element paper)) (random (sb-ext:seed-random-state seed)))
    (dotimes (line 12)
      (dotimes (word 6)
        (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 95))))
          (if (and bold (zerop line))
              (dotimes (k 4)
                (let ((x0 (+ left (* k 18))))
                  (loop for y from top repeat 18
                        do (loop for dx from -1 to bold do (setf (aref g (+ (* y 600) x0 dx)) (if (or (= dx -1) (= dx bold)) edge core))))))
              (dotimes (stroke 9)
                (let ((x0 (+ left (* stroke 8))))
                  (loop for y from top repeat 18
                        do (loop for dx from -1 to 3 do (setf (aref g (+ (* y 600) x0 dx)) (if (<= 0 dx 2) core edge))))))))))
    (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
      (dotimes (i (length g) out)
        (let ((s (if (< (aref g i) 100) ink-sigma sigma)))
          (setf (aref out i)
                (max 0 (min 255 (round (+ (aref g i)
                                          (if (zerop s) 0
                                              (* s (sqrt (* -2 (log (- 1 (random 1d0 random))))) (cos (* 2 pi (random 1d0 random)))))))))))))))

(defun pal-blurred (grey x0 y0 x1 y1 &optional (times 2))
  "GREY (600 by 500) out of focus from X0 Y0 to X1 Y1: each pixel the mean
of the 9 about it, TIMES over. GREY, changed."
  (dotimes (n times grey)
    (let ((c (copy-seq grey)))
      (loop for y from (max 1 y0) below (min 499 y1)
            do (loop for x from (max 1 x0) below (min 599 x1)
                     do (setf (aref grey (+ (* y 600) x))
                              (round (loop for dy from -1 to 1 sum (loop for dx from -1 to 1 sum (aref c (+ (* (+ y dy) 600) x dx)))) 9)))))))

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
                    (max 0 (min 255 (round (+ (* (- 1 fx) (- 1 fy) (aref grey (+ (* y0 600) x0))) (* fx (- 1 fy) (aref grey (+ (* y0 600) x0 1)))
                                              (* (- 1 fx) fy (aref grey (+ (* (1+ y0) 600) x0))) (* fx fy (aref grey (+ (* (1+ y0) 600) x0 1)))))))
                    235)))))))

(defun pal-pieces (grey &key jpeg)
  "The regions JPEG-PIECES names on the scanned page GREY (600 by 500),
kept as a JPEG when JPEG is true."
  (let ((glyphs (nth-value 2 (palimpsest::picture-words grey 600 500))))
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
  ;; a word of a scan kept without loss saved as a JPEG on its own grid and laid back: named, a strong sign,
  ;; with its place, at a scanner's grain from 1.5 to 7 grey levels, quality 75 and 90
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (let ((piece (pal-finding findings :strong "carries JPEG blocks of its own")))
      (true piece "~s" (mapcar #'palimpsest::finding-words findings))
      (when piece
        (pal-has (palimpsest::finding-words piece) "A region of page 1, about")
        (pal-has (palimpsest::finding-words piece) "a piece saved apart and laid in")
        (pal-has (first (palimpsest::finding-details piece)) "of picture 7), 9 characters, 9 with a block at column 7, row 1")
        (is eq :piece-laid-in (palimpsest::finding-method piece)))))
  (dolist (quality '(75 90))
    (dolist (sigma '(1.5 2 3 7))
      (let ((pieces (pal-pieces (pal-jpeg-saved (pal-noisy-scan :sigma sigma) 207 169 76 24 :quality quality))))
        (true (find :strong pieces :key #'palimpsest::block-piece-strength) "quality ~d, grain ~a: ~s" quality sigma pieces)
        (true (every (lambda (p) (and (<= 207 (palimpsest::block-piece-x0 p)) (<= (palimpsest::block-piece-x1 p) 283))) pieces)))))
  ;; a single character so saved is not told from a smooth one
  (false (pal-pieces (pal-jpeg-saved (pal-noisy-scan :sigma 3) 207 170 10 22)))
  ;; a scan saved as a JPEG, then a word of it cut out, saved as a JPEG on its own grid and laid off the
  ;; page's grid: named, worth a look
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-word-relaid (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500))))
    (declare (ignore pdf accounts))
    (let ((piece (pal-finding findings :look "the JPEG blocks the rest of the scan carries on its grid")))
      (true piece "~s" (mapcar #'palimpsest::finding-words findings))
      (when piece (pal-has (first (palimpsest::finding-details piece)) "its blocks on the scan's grid with")))
    (false (pal-finding findings nil "of its own")))
  (dolist (quality '(75 90))
    (dolist (sigma '(1.5 3 7))
      (true (pal-pieces (pal-word-relaid (pal-jpeg-saved (pal-noisy-scan :sigma sigma) 0 0 600 500 :quality quality) :quality quality))
            "quality ~d, grain ~a" quality sigma)))
  ;; a picture kept as a JPEG is told by its filter, alone or last of a chain
  (let ((pdf (palimpsest::read-pdf (pal-scanned (pal-scan)) :name "test.pdf")))
    (false (palimpsest::picture-jpeg-p pdf (palimpsest::last-revision pdf) 7)))
  (dolist (filter '("/DCTDecode" "[/FlateDecode /DCTDecode]"))
    (let ((pdf (palimpsest::read-pdf (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R >>")
                                                         '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                                         '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] /Resources << /XObject << /Im1 7 0 R >> >> /Contents 4 0 R >>")
                                                         '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                                         '(6 . "<< /Producer (Kea Scan) >>")
                                                         (list 7 :stream (format nil "/Type /XObject /Subtype /Image /Width 8 /Height 8 /ColorSpace /DeviceGray /BitsPerComponent 8 /Filter ~a" filter)
                                                               (make-array 16 :element-type '(unsigned-byte 8) :initial-element 0))))
                                     :name "test.pdf")))
      (true (palimpsest::picture-jpeg-p pdf (palimpsest::last-revision pdf) 7) "~a" filter)))
  ;; a page drawn and kept without loss, its paper white, with a picture in it saved as a JPEG: its blocks
  ;; the only ones measured, the rest passed over as white, so never a strong sign
  (dolist (quality '(50 75))
    (dolist (box '((113 17 76 26) (113 17 170 26) (113 17 170 100)))
      (destructuring-bind (x y w h) box
        (false (find :strong (pal-pieces (pal-jpeg-saved (pal-noisy-scan :sigma 0 :paper 255 :core 30 :edge 130) x y w h :quality quality))
                     :key #'palimpsest::block-piece-strength)
               "quality ~d, ~a" quality box))))
  ;; the frequencies counted as a JPEG sets them: a block of grain keeps them, the same block saved as a JPEG does not
  (let* ((grey (pal-grained (make-array (* 600 500) :element-type '(unsigned-byte 8) :initial-element 200)))
         (glyph (palimpsest::make-glyph :pixels (subseq grey 0 (* 12 600)) :width 600 :x0 2 :y0 2 :x1 10 :y1 10)))
    (true (< (reduce #'max (palimpsest::glyph-block-scores glyph)) 14))
    (pal-jpeg-saved grey 0 0 600 12)
    (setf (palimpsest::glyph-pixels glyph) (subseq grey 0 (* 12 600)))
    (is = 28 (aref (palimpsest::glyph-block-scores glyph) 0))))

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
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 110 15 195 45) (format nil "a word out of focus, grain ~a" sigma))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 110 15 195 45 1) (format nil "a word a little out of focus, grain ~a" sigma))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 0 15 600 45) (format nil "a line out of focus, grain ~a" sigma))
      (none (pal-blurred (pal-noisy-scan :sigma sigma) 300 250 600 500) (format nil "a quarter out of focus, grain ~a" sigma)))
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
      (loop for y from 169 below 193 do (loop for x from 207 below 283 do (setf (aref grey (+ (* y 600) x)) (if (< (aref crisp (+ (* y 600) x)) 128) 0 255))))
      (none grey "a crisp word on a JPEG")))
  ;; the scans of the other tests, untouched and saved as JPEGs, through the whole examination
  (dolist (grey (list (pal-grained (pal-scan)) (pal-scan) (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500)
                      (pal-jpeg-saved (pal-grained (pal-scan)) 0 0 600 500 :quality 90)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "JPEG blocks") "~s" (mapcar #'palimpsest::finding-words findings)))))
;;; Pages in heavier type and drawn into, for the grain of characters

(defun pal-typed (grey line &key (words '(0 1 2 3 4 5)) erased (ink 60) clean)
  "GREY, a scanned page (PAL-SCAN), with the WORDS (from 0) of LINE (from
0) drawn again as a program draws text into a picture: the old word
covered with the page's own paper (from below its lines), each stroke 3
pixels of one grey INK, smoothed as a text tool smooths where a stroke
falls between pixels, a pixel of 150 to its right on every other row, no
grain; with CLEAN, on paper of grey 250 and without the smoothing. The
words ERASED of the line covered with paper alone. GREY."
  (flet ((cover (word)
           (let ((top (+ 20 (* line 38))) (left (+ 20 (* word 95))))
             (loop for y from (- top 3) below (+ top 21) for k from 0
                   do (loop for x from (- left 3) below (+ left 73)
                            do (setf (aref grey (+ (* y 600) x)) (if clean 250 (aref grey (+ (* (+ 467 k) 600) x)))))))))
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

(defun pal-stamped (&key (sigma 6) uniform (blur (not uniform)) (line 10) (words '(3)) (level 76) (ink 60) bold (lowered 0) width height strokes pitch (seed 99))
  "The grey octets of an honest scanned page of PAL-SCAN's lines, blurred by
a 3 by 3 mean with BLUR (without UNIFORM unless told), then given grain:
Gaussian of SIGMA grey levels, or with UNIFORM, up to SIGMA either way; the
WORDS of LINE a stamp, a coloured ink or a heavy ink the scanner clipped,
its strokes' cores one grey LEVEL after the grain, or with LEVEL (GREY .
SPREAD) that grey jittering by SPREAD as a scanner's clipping does; with
LEVEL NIL, printed in another INK with grain as the rest, as a typewriter
or a second pass prints; wider with BOLD (5 pixels, 7 a word), or WIDTH by
HEIGHT, STROKES a word PITCH apart; LOWERED that many pixels below the
line (above, when less than 0)."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0))
        (flat (make-array (* 600 500) :initial-element nil)) (s seed))
    (labels ((uniform () (setf s (mod (+ (* s 1103515245) 12345) 2147483648)) (/ (+ 1 s) 2147483649d0))
             (gauss () (* (sqrt (* -2 (log (uniform)))) (cos (* 2 pi (uniform))))))
      (dotimes (l 12)
        (dotimes (word 6)
          (let* ((stamp (and (= l line) (member word words))) (top (+ 20 (* l 38) (if stamp lowered 0))) (left (+ 20 (* word 95)))
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
                                  (/ (loop for dy from -1 to 1 sum (loop for dx from -1 to 1 sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9))))))
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
  (let ((alike (nth-value 2 (palimpsest::scans-read pdf (aref (palimpsest::pdf-revisions pdf) 0)))))
    (and alike (>= alike 4) alike)))

(define-test (palimpsest palimpsest-marks-without-grain)
  ;; two marks of one ink, no grain within them, the same pixel for pixel: no copies, but marks worth a look
  (let ((grey (pal-grained (pal-scan))))
    (dolist (at '((92 . 255) (282 . 329)))
      (dotimes (y 8) (dotimes (x 4) (setf (aref grey (+ (* (+ (cdr at) y) 600) (car at) x)) 0))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned grey))
      (declare (ignore pdf accounts))
      (let ((marks (pal-finding findings :look "2 marks on page 1 are the same pixel for pixel, with no grain of their own")))
        (true marks "~s" (mapcar #'palimpsest::finding-words findings))
        (when marks
          (is eq :typed-into-scan (palimpsest::finding-method marks))
          (pal-has (first (palimpsest::finding-details marks)) "pixels 92,255 to 96,263 of picture 7")
          (pal-has (first (palimpsest::finding-details marks)) "pixels 282,329 to 286,337 of picture 7")))
      (false (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings nil "of the scanned page has no grain") "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; a word drawn into the scan in the page's own ink, on the page's own paper, its edges smoothed: no grain of
  ;; its own, worth a look, the way named, not for its ink or paper
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-typed (pal-grained (pal-scan)) 10 :words '(3))))
    (declare (ignore pdf accounts))
    (let ((typed (pal-finding findings :look "On page 1, a word of the scanned page has no grain of its own: drawn into the scan by a program, or in bold or heavier type, or ink, a stamp or coloured ink the scanner clipped.")))
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
  (loop for (name . page) in (append (list (cons "bold line" (pal-varied :grain 6 :width 6 :height 30 :strokes 6 :pitch 10))
                                           (cons "heavier line" (pal-varied :grain 6 :width 8 :height 34 :ink 40 :strokes 6 :pitch 12))
                                           (cons "bold words" (pal-varied :grain 6 :width 6 :height 30 :strokes 4 :pitch 10))
                                           (cons "clipped line" (pal-varied :ink 0)))
                                     (loop for (sigma uniform) in '((5 nil) (8 nil) (6 t) (12 t))
                                           append (loop for level in '(29 76 100)
                                                        append (loop for (how bold lowered) in '(("plain" nil 0) ("lowered" nil 5) ("bold" t 0) ("bold, lowered" t 5))
                                                                     collect (cons (format nil "stamp ~d ~a, grain ~d~:[~; uniform~]" level how sigma uniform)
                                                                                   (pal-stamped :sigma sigma :uniform uniform :level level :bold bold :lowered lowered))))))
        do (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
             (declare (ignore accounts))
             (true (pal-alike pdf) "~a: characters set beside each other" name)
             (false (pal-finding findings :strong "no grain of") "~a: ~s" name (mapcar #'palimpsest::finding-words findings))
             ;; nor copies: characters alike for their cores of one grey alone
             (false (pal-finding findings :strong "copies of one another") "~a: ~s" name (mapcar #'palimpsest::finding-words findings))))
  ;; honest ink the scanner clipped to a grey or two that jitters, as clipping does: a heading at 1 jittering
  ;; by 0.7, a body line at 2 by 0.7 or 0.3, a typewriter word at 3 by 1 set lower, stamps at 29 by 0.3 and
  ;; 0.8, plain and lowered; each at uniform grain of 6 and 12 and Gaussian of 5 and 8: no strong sign at all
  (loop for (name . args) in '(("heading 1 by 0.7" :words (0 1 2 3 4 5) :width 6 :height 30 :strokes 6 :pitch 10 :level (1 . 0.7))
                              ("body line 2 by 0.7" :words (0 1 2 3 4 5) :level (2 . 0.7))
                              ("body line 2 by 0.3" :words (0 1 2 3 4 5) :level (2 . 0.3))
                              ("typewriter word 3 by 1, lower" :level (3 . 1.0) :lowered 4)
                              ("stamp 29 by 0.3" :level (29 . 0.3))
                              ("stamp 29 by 0.8, lower" :level (29 . 0.8) :lowered 5))
        do (loop for (sigma uniform) in '((6 t) (12 t) (5 nil) (8 nil))
                 do (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (apply #'pal-stamped :sigma sigma :uniform uniform args)))
                      (declare (ignore accounts))
                      (true (pal-alike pdf) "~a, grain ~d: characters set beside each other" name sigma)
                      (false (find :strong findings :key #'palimpsest::finding-strength) "~a, grain ~d~:[~; uniform~]: ~s"
                             name sigma uniform (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq))))))
  ;; honest words printed in another pass, thin strokes 2 pixels wide set 5 or 7 pixels lower or 5 higher: a
  ;; typewriter word in ink 30, two in ink 60, a thin print in ink 80; at Gaussian grain of 5 and 8 and
  ;; uniform of 6 and 12: no strong sign (their narrow strokes read as cleaner paper within the word's box)
  (loop for (name . args) in '(("typewriter word" :width 2 :height 16 :ink 30)
                              ("typewriter words" :words (3 4) :width 2 :height 16 :ink 60)
                              ("thin print" :width 2 :height 14 :ink 80))
        do (loop for lowered in '(5 7 -5)
                 do (loop for (sigma uniform) in '((5 nil) (8 nil) (6 t) (12 t))
                          do (multiple-value-bind (pdf accounts findings)
                                 (pal-read (pal-scanned (apply #'pal-stamped :sigma sigma :uniform uniform :blur t :level nil :lowered lowered args)))
                               (declare (ignore accounts))
                               (true (pal-alike pdf) "~a ~d, grain ~d: characters set beside each other" name lowered sigma)
                               (false (find :strong findings :key #'palimpsest::finding-strength) "~a ~d, grain ~d~:[~; uniform~]: ~s"
                                      name lowered sigma uniform
                                      (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))))))
  ;; the scan untouched, with grain: nothing; a page without grain anywhere, a word drawn on it too: nothing told
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-grained (pal-scan))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "grain of") "~s" (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-typed (pal-scan) 10 :words '(3))))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "of the scanned page has no grain") "~s" (mapcar #'palimpsest::finding-words findings))
    ;; and the analyst told nothing was judged there, and why
    (let ((record (pal-finding findings :record "Copies and grain not checked on page 1: the scan's grain is too faint to tell prints apart.")))
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
      (false (pal-finding findings nil "nearer one another than prints") "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; real copies on such a page still found: a stroke of the body copied over another, a strong sign; a
  ;; stroke of the clipped line copied over another, its core one grey, marks worth a look
  (let ((page (pal-varied :ink 0)))
    (pal-copy-box page page 114 58 233 172)
    (pal-copy-box page page (+ 20 95 -1) (+ 20 (* 10 38)) (+ 20 (* 3 95) (* 2 8) -1) (+ 20 (* 10 38)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "2 characters of the scanned page are copies of one another"))
            (look (pal-finding findings :look "2 marks on page 1 are the same pixel for pixel, with no grain of their own")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        (when strong (pal-has (first (palimpsest::finding-details strong)) "pixels 114,58 to 119,76 of picture 7"))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when look (pal-has (first (palimpsest::finding-details look)) "pixels 114,400 to 119,418 of picture 7")))))
  ;; a heading the scanner clipped to black: its characters not judged, said so in the report and drawn so
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-clipped-heading (pal-varied :width 6 :height 30 :strokes 6 :pitch 10 :ink 0))))
    (declare (ignore accounts))
    (let ((record (pal-finding findings :record "36 characters not judged: too few pixels left once the ink the scanner clipped is set aside, on page 1.")))
      (true record "~s" (mapcar #'palimpsest::finding-words findings))
      (when record (pal-has (first (palimpsest::finding-details record)) "page 1 at x ")))
    (false (pal-finding findings nil "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings))
    (let ((revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (multiple-value-bind (scans copies alike pieces marks bare not-judged) (palimpsest::scans-read pdf revision)
        (declare (ignore alike pieces marks bare))
        (is = 36 (length not-judged))
        (let ((drawn (palimpsest::overlay-operators pdf revision (palimpsest::scan-picture-page (first scans))
                                                    :scans scans :copies copies :not-judged not-judged)))
          (pal-has drawn "(not judged: clipped) Tj")
          (pal-has drawn "[0.4 0.8] 0 d")
          (pal-has drawn "(Dotted grey: not judged, too few pixels left once the ink the scanner clipped is set aside. Grey hatch: clipped ink set aside.) Tj")))))
  ;; the marks of a clipped page hatched where their clipped ink was set aside
  (let ((page (pal-varied :ink 0)))
    (pal-copy-box page page (+ 20 95 -1) (+ 20 (* 10 38)) (+ 20 (* 3 95) (* 2 8) -1) (+ 20 (* 10 38)))
    (let* ((pdf (pal-read (pal-scanned page))) (revision (aref (palimpsest::pdf-revisions pdf) 0)))
      (multiple-value-bind (scans copies alike pieces marks) (palimpsest::scans-read pdf revision)
        (declare (ignore alike pieces))
        (true marks)
        (pal-has (palimpsest::overlay-operators pdf revision (palimpsest::scan-picture-page (first scans))
                                                :scans scans :copies copies :bare (mapcar #'list marks))
                 "q 0.6 0.6 0.6 RG 0.1 w ")))))

;;; Letters the blur joins

(defun pal-joined (&key (sigma 6) uniform (blur t) (width 3) (height 18) (pitch 8) (ink 60) (neck 120) dark varied clipped (seed 99))
  "The grey octets of an honest scanned page of PAL-SCAN's lines, 600 by 500
pixels, of words of nine strokes, the third and fourth and the seventh and
eighth of each joined at a neck as letters set tight are by a scanner's
blur: three rows of NECK grey bridging the paper between the two, from
their second row. With DARK, the lines of that list (from 0) with a kerned
pair or a ligature in each word: its first two strokes joined by a bridge
of the ink itself, the third joined to them at a neck. With VARIED, every
two strokes joined but the last, no two pairs alike, as no two pairs of
letters set tight are: each pair's neck two rows of a grey 20 darker than NECK at a height of
its own, its second
stroke in an ink up to 60 lighter, up to 2 rows shorter and set up to 2
rows lower. Strokes WIDTH wide and
HEIGHT tall with edges of 140, PITCH apart, of INK; blurred by a 3 by 3 mean with BLUR, then
given grain: Gaussian of SIGMA grey levels, or with UNIFORM, up to SIGMA
either way; with CLIPPED, every pixel darker than it made it, as a scanner
clips."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0)) (s seed))
    (labels ((uniform () (setf s (mod (+ (* s 1103515245) 12345) 2147483648)) (/ (+ 1 s) 2147483649d0))
             (gauss () (* (sqrt (* -2 (log (uniform)))) (cos (* 2 pi (uniform)))))
             (bridge (x0 top value &optional (from 1) (rows 3))
               (loop for y from (+ top from) repeat rows
                     do (loop for x from (+ x0 width 1) to (+ x0 pitch -2) do (setf (aref grey (+ (* y 600) x)) (float value 1d0))))))
      (dotimes (l 12)
        (dotimes (word 6)
          (let ((top (+ 20 (* l 38))) (left (+ 20 (* word 95))))
            (dotimes (stroke 9)
              (let ((x0 (+ left (* stroke pitch))))
                (let* ((v (+ (floor stroke 2) (* 4 word) (* 24 l)))
                       (second (and varied (oddp stroke) (< stroke 8))))
                  (loop for y from (+ top (if second (mod (floor v 96) 3) 0)) repeat (- height (if second (mod (floor v 32) 3) 0))
                        do (loop for dx from -1 to width
                                 do (setf (aref grey (+ (* y 600) x0 dx))
                                          (if (<= 0 dx (1- width)) (float (+ ink (if second (* 20 (mod (floor v 8) 4)) 0)) 1d0) 140d0))))
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
                                  (/ (loop for dy from -1 to 1 sum (loop for dx from -1 to 1 sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9))))))
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
  (let* ((glyphs (nth-value 2 (palimpsest::picture-words (pal-joined) 600 500)))
         (first-word (remove-if-not (lambda (g) (and (< (palimpsest::glyph-x0 g) 100) (< (palimpsest::glyph-y0 g) 40))) glyphs)))
    (is = 648 (length glyphs))
    (is = 288 (count :sure glyphs :key #'palimpsest::glyph-split))
    (is = 360 (count nil glyphs :key #'palimpsest::glyph-split))
    (is = 9 (length first-word))
    (dolist (neck '(41 73))
      (false (find-if (lambda (g) (<= (palimpsest::glyph-x0 g) neck (1- (palimpsest::glyph-x1 g)))) first-word)
             "the neck at ~d in a piece: ~s" neck
             (mapcar (lambda (g) (list (palimpsest::glyph-x0 g) (palimpsest::glyph-x1 g))) first-word))))
  ;; kerned pairs: the two joined by their core left together, an unsure piece; the next two cut off at their necks
  (let ((glyphs (nth-value 2 (palimpsest::picture-words (pal-joined :dark '(0 1 2 3 4 5 6 7 8 9 10 11)) 600 500))))
    (is = 576 (length glyphs))
    (is = 72 (count-if (lambda (g) (and (eq :unsure (palimpsest::glyph-split g)) (>= (- (palimpsest::glyph-x1 g) (palimpsest::glyph-x0 g)) 13))) glyphs))
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
        (false (pal-finding findings :look "copies of one another") "~s" (mapcar #'palimpsest::finding-words findings)))))
  ;; a kerned pair laid over another, saved again on the way: alike, but the pieces' cut unsure, worth a
  ;; look and no more
  (let ((page (pal-joined :sigma 12 :uniform t :dark '(3 7))))
    (pal-laid page (+ 20 95 -3) (+ 20 (* 3 38) -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 17 22 t)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s" (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
      (let ((look (pal-finding findings :look "nearer one another than prints of one character are")))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when look (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details look)) "no neck found to cut it at"))))))

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
                 do (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (apply #'pal-joined :sigma sigma :uniform uniform args)))
                      (declare (ignore pdf accounts))
                      (false (find :strong findings :key #'palimpsest::finding-strength) "~a, grain ~a~:[~; uniform~]: ~s"
                             name sigma uniform (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
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
                                append (list (cons page (format nil "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] /Resources << /XObject << /Im1 ~d 0 R >> >> /Contents ~d 0 R >>" (+ page 2) (+ page 1)))
                                             (list (+ page 1) :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                             (list (+ page 2) :stream "/Type /XObject /Subtype /Image /Width 600 /Height 500 /ColorSpace /DeviceGray /BitsPerComponent 8" grey)))))))

(defun pal-few-lines (grey lines)
  "GREY, a page of 12 lines (PAL-JOINED), with all but its first LINES lines covered with paper. GREY."
  (loop for i from (* 600 (+ 20 (* lines 38) -4)) below (length grey) do (setf (aref grey i) 235))
  grey)

(define-test (palimpsest palimpsest-pages-judged-apart)
  ;; a file put together from a grainy scan and a page of a few lines with hardly any grain: the second page's
  ;; honest prints, alike for want of grain, are not judged by the first page's grain; it is said unchecked
  (let ((first (pal-joined :sigma 6 :uniform t)) (second (pal-few-lines (pal-joined :sigma 0.5 :seed 7) 3)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore pdf accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s" (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
      (true (pal-finding findings :record "Copies and grain not checked on page 2: the scan's grain is too faint to tell prints apart.")
            "~s" (mapcar #'palimpsest::finding-words findings)))
    ;; a stroke copied on the grainy page still found there
    (pal-laid first (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 9 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page are copies of one another")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-page-glyphs (grey &optional (page 0) (picture 7))
  "The characters of the scanned page GREY (600 by 500), on PAGE of PICTURE, their grain measured (GRAIN-LEVELS)."
  (let ((glyphs (nth-value 2 (palimpsest::picture-words grey 600 500))))
    (dolist (g glyphs) (setf (palimpsest::glyph-page g) page (palimpsest::glyph-picture g) picture))
    (palimpsest::grain-levels glyphs)))

(define-test (palimpsest palimpsest-strips-on-a-jpeg-grid)
  ;; a stroke laid a whole number of JPEG blocks from its source: on a page kept as a JPEG, a block the
  ;; same there is marked as on the grid, worth a look; a block off the grid, or a page kept without loss, not
  (flet ((pairs (dy dct)
           (let ((page (pal-grained (pal-scan))))
             (pal-copy-box page page 114 58 122 (+ 58 dy))
             (palimpsest::same-strips (pal-page-glyphs page) :dct dct))))
    (let ((on (pairs 152 '(7))) (off (pairs 190 '(7))) (lossless (pairs 152 '())))
      (is = 1 (length on)) (is = 1 (length off)) (is = 1 (length lossless))
      (true (fourth (first on)))
      (false (fourth (first off)))
      (false (fourth (first lossless)))
      ;; the block's ink not clipped counted beside its size
      (true (<= 20 (third (third (first on)))))))
  ;; strips whose core has no grain of its own are not set beside each other, though every print of a stroke
  ;; drawn without grain is the same there and far more than 64 share one number
  (let ((glyphs (nth-value 2 (palimpsest::picture-words (pal-scan) 600 500))))
    (dolist (g glyphs) (setf (palimpsest::glyph-page g) 0 (palimpsest::glyph-picture g) 7 (palimpsest::glyph-typical g) 17))
    (is = 0 (length (palimpsest::same-strips glyphs))))
  ;; past the most strips a character may give, those left out are counted, never dropped unsaid
  (let* ((wide 3200) (grey (make-array (* 14 (+ wide 4)) :element-type '(unsigned-byte 8))) (seed 5))
    (dotimes (i (length grey))
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (if (<= 3 (floor i (+ wide 4)) 10) (+ 40 (mod (ash seed -16) 40)) 235)))
    (let ((g (palimpsest::glyph-measured (palimpsest::make-glyph :pixels grey :width (+ wide 4) :x0 2 :y0 2 :x1 (+ wide 2) :y1 12 :ink 5000 :page 0 :picture 7 :typical 17))))
      (true (plusp (nth-value 1 (palimpsest::same-strips (list g g)))) "strips counted as left out"))))

(define-test (palimpsest palimpsest-many-copies-of-one-character)
  ;; one stroke laid over 70 others: every copy found by its strips, the same pixel for pixel, a strong sign;
  ;; no detail without its block's size
  (let ((page (pal-grained (pal-scan))))
    (loop for (line word stroke) in (append (loop for line from 1 to 11 append (loop for word below 6 collect (list line word 1)))
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
  (let ((glyphs (nth-value 2 (palimpsest::picture-words (pal-joined :neck 60) 600 500))))
    (is = 144 (count :unsure glyphs :key #'palimpsest::glyph-split)))
  ;; a stroke laid the same pixel for pixel over another, and the same stroke, saved again on the way, over one
  ;; of a pair the blur joins: the first two a strong sign; the piece alike by its likeness alone worth a look,
  ;; never in the strong group
  (let ((page (pal-joined :sigma 12 :uniform t)))
    (pal-laid page (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 5 38) -2) 9 22)
    (pal-laid page (+ 20 95 16 -3) (+ 20 38 -2) (+ 20 (* 2 95) 48 -3) (+ 20 (* 8 38) -2) 9 22 t)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "2 characters of the scanned page are copies of one another"))
            (look (pal-finding findings :look "nearer one another than prints of one character are")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        (true look "~s" (mapcar #'palimpsest::finding-words findings))
        (when strong (false (search "cut at a neck" (first (palimpsest::finding-details strong)))))
        (when look (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details look)) "cut at a neck"))))))

(defun pal-ledger (&key (sigma 1.75) (quality 50) (seed 31))
  "The grey octets of an honest page, 600 by 500 pixels, kept as a JPEG of
QUALITY: six lines of print above whose ink varies pixel by pixel, as toner does,
and below a clean ledger of figures, each two strokes, a comma (3 by 6
pixels) and a stroke 8 pixels apart, set on whole pixels 64 apart across
and 32 down, all on the JPEG's grid of 8 by 8 pixels; blurred by a 3 by 3 mean, then given Gaussian grain of SIGMA."
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
                     do (stroke x y 14 60d0) (stroke (+ x 8) y 14 60d0) (stroke (+ x 16) (+ y 8) 6 60d0) (stroke (+ x 24) y 14 60d0)))
      (let ((copy (copy-seq grey)))
        (loop for y from 1 below 499
              do (loop for x from 1 below 599
                       do (setf (aref grey (+ (* y 600) x))
                                (/ (loop for dy from -1 to 1 sum (loop for dx from -1 to 1 sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9)))))
      (let ((out (make-array (* 600 500) :element-type '(unsigned-byte 8))))
        (dotimes (i (length out)) (setf (aref out i) (max 0 (min 255 (round (+ (aref grey i) (* sigma (gauss))))))))
        (pal-jpeg-saved out 0 0 600 500 :quality quality)))))

(define-test (palimpsest palimpsest-jpeg-likeness-worth-a-look)
  ;; a page kept as a JPEG, prints of varied toner above and a clean ledger on the JPEG's grid below: the
  ;; ledger's figures and commas come alike, but on a page kept as a JPEG likeness alone is never a strong sign
  (dolist (sigma '(1.75 2.5))
    (let* ((glyphs (pal-page-glyphs (pal-ledger :sigma sigma)))
           (copies (palimpsest::copied-glyphs glyphs :dct '(7))))
      (false (find :strong copies :key #'fifth) "grain ~a: ~d strong groups, of differences ~a" sigma
             (count :strong copies :key #'fifth) (mapcar #'second (remove :look copies :key #'fifth)))
      (true (every (lambda (c) (or (eq (fifth c) :look) (third c))) copies)))))

(define-test (palimpsest palimpsest-large-mark-measured)
  ;; a mark of more than 65,535 pixels, a line run together as a tight page set small and blurred can make,
  ;; is measured as any other
  (let* ((w 400) (h 200) (pixels (make-array (* (+ w 4) (+ h 4)) :element-type '(unsigned-byte 8) :initial-element 40)))
    (true (palimpsest::glyph-measured (palimpsest::make-glyph :pixels pixels :width (+ w 4) :x0 2 :y0 2 :x1 (+ w 2) :y1 (+ h 2) :ink (* w h))))))

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
         (page (pal-stamped-on (pal-stamped-on (pal-few-lines (pal-joined :sigma 6 :uniform t) 6) stamp 40 300) stamp 330 400)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (let ((strong (pal-finding findings :strong "10 characters of the scanned page are copies of one another")))
        (true strong "~s" (mapcar #'palimpsest::finding-words findings))
        ;; letters taller than the page's type: a stamp laid by software named as one honest cause
        (when strong (pal-has (palimpsest::finding-words strong) "or a stamp, a logo or a signature laid on the pages by software"))
        (when strong
          (dolist (x '(60 80 100 120 140))
            (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details strong)) (format nil "pixels ~d,312 to " x))
            (pal-has (format nil "~{~a~%~}" (palimpsest::finding-details strong)) (format nil "pixels ~d,412 to " (+ x 290)))))))))

(define-test (palimpsest palimpsest-mark-at-one-place-on-every-page)
  ;; a character laid at the same place on both pages of a file: a strong sign, a mark laid by software named
  ;; among its causes; a character copied elsewhere on a page: no such cause
  (let ((first (pal-joined :sigma 6 :uniform t)) (second (pal-joined :sigma 6 :uniform t :seed 5)))
    (dotimes (y 22) (dotimes (x 9) (setf (aref second (+ (* (+ 56 y) 600) 112 x)) (aref first (+ (* (+ 56 y) 600) 112 x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list first second)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned pages are copies of one another, the grain of their ink too: copied within the scan, or a stamp, a logo or a signature laid on the pages by software.")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  (let ((page (pal-joined :sigma 6 :uniform t)))
    (pal-laid page (+ 20 95 -3) (+ 20 38 -2) (+ 20 (* 4 95) -3) (+ 20 (* 7 38) -2) 9 22)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "copied within the scan, or typed onto it.") "~s" (mapcar #'palimpsest::finding-words findings))
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
  (let ((glyphs (nth-value 2 (palimpsest::picture-words (pal-ruled (pal-joined)) 600 500))))
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
             "~s" (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq)))
      (true (pal-finding findings :record "not checked on page 2") "~s" (mapcar #'palimpsest::finding-words findings))
      (let* ((revision (aref (palimpsest::pdf-revisions pdf) 0))
             (scans (palimpsest::scan-pictures pdf revision))
             (glyphs (loop for scan in scans append (palimpsest::scan-picture-glyphs scan)))
             (drawn (remove-if-not (lambda (s) (< (palimpsest::scan-picture-paper s) 0.5)) scans)))
        (is = 1 (length drawn))
        (palimpsest::copied-glyphs glyphs :drawn (list (palimpsest::mark-object (palimpsest::scan-picture-mark (first drawn)))))
        (true (every #'palimpsest::glyph-bare (remove-if-not (lambda (g) (= 1 (palimpsest::glyph-page g))) glyphs)))))))

(define-test (palimpsest palimpsest-smoothed-region-worth-a-look)
  ;; a region of a grainy scan smoothed by the scanner's software: its prints, near alike for the grain
  ;; smoothed away, worth a look at most, never a strong sign
  (let ((page (pal-joined :sigma 6 :uniform t)))
    (pal-blurred page 0 50 600 140 3)
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (false (find :strong findings :key #'palimpsest::finding-strength)
             "~s" (mapcar #'palimpsest::finding-words (remove :strong findings :key #'palimpsest::finding-strength :test-not #'eq))))))

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
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list (pal-joined :sigma 6 :uniform t) dark)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "Copies and grain not checked on page 2: no words could be read from its picture.")
            "~s" (mapcar #'palimpsest::finding-words findings))))
  (let ((page (pal-joined :sigma 6 :uniform t)))
    ;; a hatching of dots between lines 4 to 12 runs them into one band, too tall to be a line
    (loop for y from (+ 20 (* 3 38)) below 480
          do (loop for x from 20 below 580 when (zerop (mod (+ x y) 3)) do (setf (aref page (+ (* y 600) x)) 60)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned page))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "Copies and grain checked on ") "~s" (mapcar #'palimpsest::finding-words findings)))))

(defun pal-close-lines (&key (pitch 26) (lines 12) (sigma 6) bars (seed 61))
  "The grey octets of an honest scanned page, 600 by 500 pixels, of LINES
lines PITCH rows apart, each of 6 words of 9 strokes 3 wide and 14 tall,
the second stroke of each word rising 8 rows higher and the sixth falling 8
lower, as ascenders and descenders do: at a PITCH under 30 those of one line
touch those of the next. With BARS, ten change bars 2 pixels wide and 100 tall down the
right margin. Grain up to SIGMA either way."
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
        (loop for y from (+ 10 (* b 4)) repeat 100 do (loop for x from (+ 560 (* b 3)) repeat 2 do (setf (aref grey (+ (* y 600) x)) 50)))))
    (dotimes (i (length grey) grey)
      (setf s (mod (+ (* s 1103515245) 12345) 2147483648))
      (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) (- sigma) (mod (ash s -16) (1+ (* 2 sigma))))))))))

(define-test (palimpsest palimpsest-touching-lines-read)
  ;; lines set so close that ascenders and descenders touch: cut apart where they meet, and read
  (let ((glyphs (nth-value 2 (palimpsest::picture-words (pal-close-lines) 600 500))))
    (true (<= 640 (length glyphs) 648) "~d characters" (length glyphs)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-close-lines)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "not checked") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-unread-pages-told-why)
  ;; lines run together by a hatching between them that no cut can part, change bars beside them: said to
  ;; run together, not to be lost among rules
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (let ((page (pal-close-lines :bars t)))
                               (loop for y from 10 below 340
                                     do (loop for x from 15 below 550 when (zerop (mod (+ x y) 3)) do (setf (aref page (+ (* y 600) x)) 60)))
                               page)))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :record "Copies and grain not checked on page 1: its lines run together, too close to be told apart.")
          "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; two pages of no words: their pictures
  (let ((dark (make-array (* 600 500) :element-type '(unsigned-byte 8))))
    (dotimes (i (length dark)) (setf (aref dark i) (if (< (mod i 600) 300) 40 230)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list dark (copy-seq dark))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :record "Copies and grain not checked on pages 1 and 2: no words could be read from their pictures.")
            "~s" (mapcar #'palimpsest::finding-words findings)))))

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
    (let ((glyphs (nth-value 2 (palimpsest::picture-words page 600 500))))
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

(define-test (palimpsest palimpsest-columns-of-different-leading)
  ;; two columns of lines that touch, set at different leading, so that the lines of one fall between those of
  ;; the other: each column's lines found apart, and read
  (let ((page (pal-close-lines :pitch 26)) (other (pal-close-lines :pitch 23 :seed 62)))
    (dotimes (y 500)
      (loop for x from 280 below 600
            do (setf (aref page (+ (* y 600) x)) (if (< x 380) (aref page (+ (* y 600) 279)) (aref other (+ (* y 600) x))))))
    (let ((glyphs (nth-value 2 (palimpsest::picture-words page 600 500))))
      (true (<= 450 (length glyphs) 540) "~d characters" (length glyphs)))))

(defun pal-ledger-page (&key (sigma 6) (seed 71))
  "The grey octets of an honest scanned page, 600 by 500 pixels: six lines of
dense print (PAL-JOINED's) above, and below six rows of a ledger, each of
five amounts of three figures drawn as a 0 is, 8 pixels wide and 12 tall,
10 apart, their top and bottom
strokes 2 rows deep and their sides 2 pixels wide, so that their top and
bottom rows hold more ink than their middle. Grain up to SIGMA either way."
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

(define-test (palimpsest palimpsest-ledger-rows-not-cut)
  ;; a ledger below dense print: its rows of figures, inkier at their top and bottom than in their middle, are
  ;; no two lines and are never cut through; an amount of three figures laid over another is found, a strong sign
  (let ((glyphs (nth-value 2 (palimpsest::picture-words (pal-ledger-page) 600 500))))
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
           (coerce (palimpsest::raw-grey (coerce data '(simple-array (unsigned-byte 8) (*))) width 1 bits kind table invert) 'list)))
    (is equal '(0 255 128) (grey '(0 255 128) 3 8 :grey))
    (is equal '(76 150 29) (grey '(255 0 0  0 255 0  0 0 255) 3 8 :rgb))
    (is equal '(255 0) (grey '(0 0 0 0  0 0 0 255) 2 8 :cmyk))
    (is equal '(0 255 0 255 0 255 0 255) (grey '(#b01010101) 8 1 :grey))
    (is equal '(255 0 255 0 255 0 255 0) (grey '(#b01010101) 8 1 :grey nil t))
    (is equal '(76 255) (grey '(0 1) 2 8 :indexed (list :rgb 1 (coerce '(255 0 0 255 255 255) '(simple-array (unsigned-byte 8) (*)))))))
  ;; too short for its size: none
  (false (palimpsest::raw-grey (make-array 2 :element-type '(unsigned-byte 8)) 3 1 8 :grey nil nil)))

(define-test (palimpsest palimpsest-text-placed-off)
  ;; a figure a little above its line; a superscript, smaller, left alone
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (concatenate 'string
                                           "BT /F1 12 Tf 72 700 Td (Total due for fence and gate: $) Tj ET BT /F1 12 Tf 250 700.6 Td (8,250.00) Tj ET "
                                           "BT /F1 12 Tf 72 680 Td (Paid in full on the day it was due) Tj /F1 7 Tf 3 Ts (note) Tj 0 Ts ET")))
    (declare (ignore pdf accounts))
    (let ((above (pal-finding findings :look "sits a little above or below")))
      (true above "~s" (mapcar #'palimpsest::finding-words findings))
      (when above (is equal '("\"8,250.00\" 0.60 pt above its line (4 @76 font 5)") (palimpsest::finding-details above)))))
  ;; a line starting a little right of the margin four others share; an indent of 18 points left alone
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "~{BT /F1 12 Tf ~a ~d Td (~a) Tj ET ~}"
                                      '(72 700 "Invoice for Kea Builders" 72 680 "To Tui Gardens Ltd" 72.6 660 "Fence repair, 12 m"
                                        72 640 "New gate and latch" 72 620 "Total: $1,250.00" 90 600 "Paid with thanks"))))
    (declare (ignore pdf accounts))
    (let ((off (pal-finding findings :look "set a little off the edge")))
      (true off "~s" (mapcar #'palimpsest::finding-words findings))
      (when off (is equal '("\"Fence repair, 12 m\" starts at x 72.60, 4 other lines at 72.00 (4 @119 font 5)") (palimpsest::finding-details off)))))
  ;; an amount a little off a right-aligned column, its font giving its widths; with no widths, ends are not judged
  (flet ((column (widths)
           (let ((content (with-output-to-string (s)
                            (loop for (item amount off) in '(("Posts" "480" 0) ("Rails" "360" 0) ("Gate" "210" 0.6) ("Labour" "200" 0) ("Total" "1250" 0))
                                  for y downfrom 700 by 20
                                  do (format s "BT /F1 12 Tf 72 ~d Td (~a) Tj ET BT /F1 12 Tf ~,2f ~d Td (~a) Tj ET "
                                             y item (+ (- 500 (* (length amount) 556/1000 12)) off) y amount)))))
             (pal-one-page content :fonts "/F1 7 0 R"
                                   :more (list (cons 7 (format nil "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica~@[ /FirstChar 48 /LastChar 57 /Widths [~{~d~^ ~}]~] >>"
                                                               (and widths (make-list 10 :initial-element 556)))))))))
    (multiple-value-bind (pdf accounts findings) (pal-read (column t))
      (declare (ignore pdf accounts))
      (let ((off (pal-finding findings :look "set a little off the edge")))
        (true off "~s" (mapcar #'palimpsest::finding-words findings))
        (when off (is equal '("\"Gate 210\" ends at x 500.60, 4 other lines at 500.00 (4 @201 font 7)") (palimpsest::finding-details off)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (column nil))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "set a little off the edge")))))

(define-test (palimpsest palimpsest-scanned-word-placed-off)
  ;; a word of a scan 4 pixels below its line is named; a whole page scanned crooked is not
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :lowered t)))
    (declare (ignore pdf accounts))
    (let ((lower (pal-finding findings :look "a lower baseline")))
      (true lower "~s" (mapcar #'palimpsest::finding-words findings))
      (when lower (pal-has (first (palimpsest::finding-details lower)) "its baseline 4.0 pixels below its line's"))))
  ;; a word both pasted and lowered on a scan with grain: two measures apart agree, a strong sign
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (let ((page (pal-grained (pal-scan :lowered t))) (crisp (pal-scan :pasted t :lowered t)))
                               ;; the word laid crisp on clean paper after the page was scanned
                               (loop for y from 169 below 197
                                     do (loop for x from 207 below 283 do (setf (aref page (+ (* y 600) x)) (aref crisp (+ (* y 600) x)))))
                               page)))
    (declare (ignore pdf accounts))
    (let ((both (pal-finding findings :strong "both in how it looks and where it sits")))
      (true both "~s" (mapcar #'palimpsest::finding-words findings))
      (when both
        (pal-has (palimpsest::finding-words both) "darker ink")
        (pal-has (palimpsest::finding-words both) "a lower baseline")))
    (false (pal-finding findings :look "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :tilt 0.012)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings nil "unlike the rest of it") "~s" (mapcar #'palimpsest::finding-words findings))))

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
    (true (pal-finding findings :record "the scan's grain is too faint to tell prints apart") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :strong "carries JPEG blocks of its own") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; the same on a picture drawn by a program, its paper and its ink each one grey, as a screenshot's are:
  ;; worth a look at most
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned (pal-jpeg-saved (pal-noisy-scan :sigma 0) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "JPEG blocks") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-pieces-laid-on-every-page)
  ;; a region with JPEG blocks of its own at the same place on both pages, as a logo laid by an office's
  ;; software: strong still, the honest cause named; on one page alone, not
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-scanned-pages (list (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)
                                         (pal-jpeg-saved (pal-grained (pal-scan) 4242) 207 169 76 24))))
    (declare (ignore pdf accounts))
    (let ((laid (remove-if-not (lambda (f) (and (eq :strong (palimpsest::finding-strength f))
                                                (search "carries JPEG blocks of its own" (palimpsest::finding-words f))))
                               findings)))
      (is = 2 (length laid) "~s" (mapcar #'palimpsest::finding-words findings))
      (dolist (f laid)
        (pal-has (palimpsest::finding-words f) "or a stamp, a logo or a signature laid on the pages by software")
        (pal-has (palimpsest::finding-rule f) "Whether such a mark belongs there is for the examiner to judge"))))
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-jpeg-saved (pal-grained (pal-scan)) 207 169 76 24)))
    (declare (ignore pdf accounts))
    (let ((piece (pal-finding findings :strong "carries JPEG blocks of its own")))
      (true piece)
      (when piece (false (search "a stamp, a logo" (palimpsest::finding-words piece)))))))

(define-test (palimpsest palimpsest-picture-shown-twice)
  ;; one picture kept twice in a file, the same octet for octet, shown on two pages: no copies, both pages
  ;; checked; a stroke laid over another in it is still a strong sign
  (let ((page (pal-grained (pal-scan))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list page (copy-seq page))))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "characters of the scanned page") "~s" (mapcar #'palimpsest::finding-words findings))
      (false (pal-finding findings :record "not checked") "~s" (mapcar #'palimpsest::finding-words findings)))
    (loop for y below 22
          do (loop for x below 8
                   do (setf (aref page (+ (* (+ 20 (* 4 38) -2 y) 600) (+ 20 (* 2 95) (* 3 8) -2) x))
                            (aref page (+ (* (+ 20 38 -2 y) 600) (+ 20 95 -2) x)))))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned-pages (list page (copy-seq page))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "2 characters of the scanned page") "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; the same octets drawn otherwise, inverted by /Decode, are another picture: read for themselves
  (let* ((page (pal-grained (pal-scan)))
         (octets (map '(vector (unsigned-byte 8)) #'char-code
                      (cl-ppcre:regex-replace "(?s)(/Width 600 /Height 500 /ColorSpace /DeviceGray /BitsPerComponent 8)(.*?/Width 600 /Height 500 /ColorSpace /DeviceGray /BitsPerComponent 8)"
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
          (true (find-if (lambda (l) (and (search "4 @" l) (search "font 5" l) (not (search "@13 " l)))) lines) "~s" lines))))
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
               (pal-page-of "/MediaBox [0 0 612 792] /Resources << /XObject << /Im10 10 0 R /Im11 11 0 R >> >> /Contents 4 0 R"
                            "q 10 0 0 10 0 0 cm /Im10 Do Q q 500 0 0 500 0 0 cm /Im10 Do Q q 10 0 0 10 0 0 cm /Im11 Do Q"
                            :more (loop for k in '(10 11)
                                        collect (list k :stream "/Type /XObject /Subtype /Image /Width 2 /Height 2 /ColorSpace /DeviceGray /BitsPerComponent 8" "abcd")))))
         (revision (palimpsest::last-revision pdf)))
    (is equal '(10 11) (mapcar (lambda (p) (palimpsest::mark-object (first p))) (palimpsest::pictures-of pdf revision)))
    (is equal '((10 500)) (mapcar (lambda (p) (list (palimpsest::mark-object (first p)) (round (palimpsest::mark-x1 (first p)))))
                                  (palimpsest::pictures-of pdf revision :keep (lambda (page m) (declare (ignore page))
                                                                                 (> (palimpsest::mark-x1 m) 100)))))))

(defun pal-office-page (content &key (producer "Acrobat Distiller 10.1.2 (Windows)") (creator "PScript5.dll Version 5.2.2")
                                     (id "/ID [<0a1b2c3d> <0a1b2c3d>]") tagged trailer)
  "A one-page PDF drawing CONTENT, its document information naming PRODUCER and CREATOR, its trailer
giving ID: /F1 a font kept in the file (Calibri), /F2 Helvetica-Bold, not kept, /F3 another font kept
in the file (Liberation Serif); TAGGED, a tagged PDF
whose structure tree names the page's marked content 0, 1 and 2; TRAILER, more for the trailer."
  (pal-save #() (list (if tagged
                          '(1 . "<< /Type /Catalog /Pages 2 0 R /StructTreeRoot 11 0 R /MarkInfo << /Marked true >> >>")
                          '(1 . "<< /Type /Catalog /Pages 2 0 R >>"))
                      '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                      '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R /F2 10 0 R /F3 13 0 R >> >> /Contents 4 0 R >>")
                      (list 4 :stream "" content)
                      '(5 . "<< /Type /Font /Subtype /TrueType /BaseFont /ABCDEF+Calibri /FirstChar 32 /LastChar 126 /FontDescriptor 8 0 R >>")
                      (cons 6 (format nil "<< /Producer (~a) /Creator (~a) >>" producer creator))
                      '(8 . "<< /Type /FontDescriptor /FontName /ABCDEF+Calibri /Flags 32 /FontFile2 9 0 R >>")
                      '(9 :stream "" "kept font")
                      '(10 . "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica-Bold /Encoding /WinAnsiEncoding >>")
                      '(11 . "<< /Type /StructTreeRoot /K 12 0 R >>")
                      '(12 . "<< /Type /StructElem /S /P /Pg 3 0 R /K [0 1 2] >>")
                      '(13 . "<< /Type /Font /Subtype /TrueType /BaseFont /GHIJKL+LiberationSerif /FirstChar 32 /LastChar 126 /FontDescriptor 14 0 R >>")
                      '(14 . "<< /Type /FontDescriptor /FontName /GHIJKL+LiberationSerif /Flags 34 /FontFile2 15 0 R >>")
                      '(15 :stream "" "kept font"))
            :trailer (format nil "/Root 1 0 R /Info 6 0 R~@[ ~a~]~@[ ~a~]" id trailer)))

(define-test (palimpsest palimpsest-text-box-drawn-as-made)
  ;; an office program's text box, white with its own text in the sheet's font, over a cell's text, all in the
  ;; one stream of the first save, its /ID as made: worth a look, with its honest cause; Distiller names no
  ;; PDF editor
  (let ((box "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET"))
    (is equal '("Acrobat Distiller") (mapcar #'second (palimpsest::tools-in "Acrobat Distiller 10.1.2 (Windows)")))
    (is equal '("Adobe Acrobat" "Acrobat Distiller")
        (mapcar #'second (palimpsest::tools-in "Acrobat Distiller 10.1.2 (Windows); Adobe Acrobat Pro DC")))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page box))
      (declare (ignore pdf accounts))
      (false (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))
      (true (pal-finding findings :look "drawn with the text as the file was made"))
      (false (pal-finding findings nil "names Adobe Acrobat")))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page box :id nil))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :look "drawn with the text as the file was made") "~s" (mapcar #'palimpsest::finding-words findings)))
    ;; the same with a PDF editor named, by a program not known to make PDFs, or with the /ID written again: strong
    (loop for (producer creator id) in '(("pypdf" "PScript5.dll Version 5.2.2" "/ID [<0a1b2c3d> <0a1b2c3d>]")
                                         ("Kea Writer" "Kea Writer" "/ID [<0a1b2c3d> <0a1b2c3d>]")
                                         ("Acrobat Distiller 10.1.2 (Windows)" "PScript5.dll Version 5.2.2" "/ID [<0a1b2c3d> <99887766>]"))
          do (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page box :producer producer :creator creator :id id))
               (declare (ignore pdf accounts))
               (true (pal-finding findings :strong "covered by a white shape") "~s ~s" producer (mapcar #'palimpsest::finding-words findings)))))
  ;; drawn after the page's outermost q ... Q block the hidden text is in: strong; within it, worth a look
  (dolist (case '(("q BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET Q 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET" :strong)
                  ("q BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET Q" :look)))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page (first case)))
      (declare (ignore pdf accounts))
      (true (pal-finding findings (second case) "covered by a white shape") "~s ~s" (second case) (mapcar #'palimpsest::finding-words findings))))
  ;; a file LibreOffice made carries /DocChecksum in its trailer, the MD5 of the file up to its
  ;; trailer: without it, or with one that does not hold, written again or changed, strong
  (let* ((content "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET")
         (made (pal-office-page content :producer "LibreOffice 7.6" :creator "Writer"))
         (sum (palimpsest::md5-hex made :end (search (pal-octets "trailer") made))))
    (dolist (case (list (list nil :strong) (list (format nil "/DocChecksum /~a" sum) :look)
                        (list "/DocChecksum /CBF78CB392F8D620019445DBEF0F0037" :strong)))
      (multiple-value-bind (pdf accounts findings)
          (pal-read (pal-office-page content :producer "LibreOffice 7.6" :creator "Writer" :trailer (first case)))
        (declare (ignore pdf accounts))
        (true (pal-finding findings (second case) "covered by a white shape") "~s ~s" case (mapcar #'palimpsest::finding-words findings)))))
  ;; MD5 as RFC 1321 gives it
  (is string= "D41D8CD98F00B204E9800998ECF8427E" (palimpsest::md5-hex (pal-octets "")))
  (is string= "9E107D9D372BB6826BD81D3542A419D6" (palimpsest::md5-hex (pal-octets "The quick brown fox jumps over the lazy dog")))
  (is string= "57EDF4A22BE3C955AC49DA2E2107B67A"
      (palimpsest::md5-hex (pal-octets "12345678901234567890123456789012345678901234567890123456789012345678901234567890")))
  ;; a program that writes PDFs anew named: strong
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET"
                                 :creator "PScript5.dll Version 5.2.2; pikepdf 8.15"))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a tagged page: the box and its text within the marked content its structure tree names, worth a look;
  ;; the same box and text appended untagged, strong
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page "/P <</MCID 0>> BDC BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET EMC /P <</MCID 1>> BDC 1 1 1 rg 70 695 110 18 re f EMC /P <</MCID 2>> BDC 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET EMC"
                                 :tagged t))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :look "drawn with the text as the file was made") "~s" (mapcar #'palimpsest::finding-words findings)))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page "/P <</MCID 0>> BDC BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET EMC 1 1 1 rg 70 695 110 18 re f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET"
                                 :tagged t))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings)))
  ;; a box whose text is in a font not kept in the file, as an editor adds one, and a box with nothing of its
  ;; own on it, though drawn as the file was made: strong
  (dolist (content '("BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g BT /F2 12 Tf 72 703 Td (Data Size in Bytes) Tj ET"
                     "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page content))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-figure-in-a-line-covered)
  ;; a white box over one figure of a long line, as an online editor changes it: the line partly covered,
  ;; the figure named; a box over a whole run, as before
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Price NZ$ 184,500.00 plus GST, being the total of the prices in Schedule 1 of this Agreement.) Tj ET 1 1 1 rg 130 697 64 14 re f"))
    (declare (ignore pdf accounts))
    (let ((partly (pal-finding findings :strong "partly covered by a white shape")))
      (true partly "~s" (mapcar #'palimpsest::finding-words findings))
      (when partly
        (pal-has (palimpsest::finding-words partly) "184,500.00\" of \"Price NZ$ 184,500.00 plus GST")))))

;;; Covers drawn as paths

(defparameter *pal-white-paths*
  '(("a box with rounded corners" "1 1 1 rg 72 694 m 134 694 l 138 694 140 696 140 700 c 140 712 l 140 716 138 718 134 718 c 72 718 l 68 718 66 716 66 712 c 66 700 l 66 696 68 694 72 694 c h f")
    ("a rectangle drawn as LibreOffice draws one" "1 1 1 rg 100 694 m 66 694 l 66 718 l 140 718 l 140 694 l 100 694 l h f")
    ("two squares filled by the nonzero rule" "1 1 1 rg 60 690 90 40 re 66 696 78 28 re f"))
  "White shapes drawn as paths over \"Secret 42\" (72 to 126 across, 700 to 712 up), each with what it is.")

(define-test (palimpsest palimpsest-path-covers)
  ;; a white shape drawn as a path, its curves and lines, over text: strong, as a rectangle is
  (loop for (what path) in *pal-white-paths*
        do (multiple-value-bind (pdf accounts findings) (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET ~a" path)))
             (declare (ignore pdf accounts))
             (let ((covered (pal-finding findings :strong "covered by a white shape")))
               (true covered "~a: ~s" what (mapcar #'palimpsest::finding-words findings))
               (when covered (pal-has (palimpsest::finding-words covered) "Secret 42")))))
  ;; its box over the text, but the text in a notch of it, or in a hole the even-odd rule leaves:
  ;; not covered
  (dolist (path '("1 1 1 rg 60 690 m 150 690 l 150 730 l 140 730 l 140 696 l 66 696 l 66 730 l 60 730 l h f"
                  "1 1 1 rg 60 690 90 40 re 66 696 78 28 re f*"))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET ~a" path)))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "covered by a") "~s: ~s" path (mapcar #'palimpsest::finding-words findings))))
  ;; an oval over one figure of a line: that figure named, the letters it only grazes not
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page "BT /F1 12 Tf 72 700 Td (Price NZ$ 184,500.00 plus GST) Tj ET 1 1 1 rg 196 704 m 196 709.5 180.8 714 162 714 c 143.2 714 128 709.5 128 704 c 128 698.5 143.2 694 162 694 c 180.8 694 196 698.5 196 704 c h f"))
    (declare (ignore pdf accounts))
    (let ((partly (pal-finding findings :strong "partly covered by a white shape")))
      (true partly "~s" (mapcar #'palimpsest::finding-words findings))
      (when partly (pal-has (palimpsest::finding-words partly) "\"184,500.00\" of")))))

;; a rounded box from X Y, W across and H up, its corners of radius R, as a path
(defun pal-rounded (x y w h r)
  "The operators drawing a box from X Y, W across and H up, its corners rounded to radius R."
  (let ((c (* r 0.5523)))
    (format nil "~,3f ~,3f m ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c ~,3f ~,3f l ~,3f ~,3f ~,3f ~,3f ~,3f ~,3f c h "
            (+ x r) y (- (+ x w) r) y (+ (- (+ x w) r) c) y (+ x w) (- (+ y r) c) (+ x w) (+ y r)
            (+ x w) (- (+ y h) r) (+ x w) (+ (- (+ y h) r) c) (+ (- (+ x w) r) c) (+ y h) (- (+ x w) r) (+ y h)
            (+ x r) (+ y h) (- (+ x r) c) (+ y h) x (+ (- (+ y h) r) c) x (- (+ y h) r)
            x (+ y r) x (- (+ y r) c) (- (+ x r) c) y (+ x r) y)))

(define-test (palimpsest palimpsest-covers-in-the-text-frame)
  ;; a rising line: a panel in the empty corner of its upright box hides nothing of it; a panel over
  ;; it hides it
  (let ((line "BT /F1 12 Tf 0.5 0.866 -0.866 0.5 72 600 Tm (Received 1,250.00 on account) Tj ET "))
    (multiple-value-bind (pdf accounts findings) (pal-read (pal-one-page (format nil "~a1 1 1 rg ~af" line (pal-rounded 138 604 30 94 4))))
      (declare (ignore pdf accounts))
      (false (pal-finding findings nil "covered by a") "~s" (mapcar #'palimpsest::finding-words findings)))
    (multiple-value-bind (pdf accounts findings)
        (pal-read (pal-one-page (format nil "~aq 0.5 0.866 -0.866 0.5 72 600 cm 1 1 1 rg ~af Q" line (pal-rounded -4 -4 180 18 4))))
      (declare (ignore pdf accounts))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))))
  ;; text set up the page under a panel: covered
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-one-page (format nil "BT /F1 12 Tf 0 1 -1 0 300 500 Tm (Vertical note 1,250.00) Tj ET 1 1 1 rg ~af" (pal-rounded 284 494 22 148 4))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-covers-by-their-outline-wholly)
  (flet ((findings-of (shape)
           (nth-value 2 (pal-read (pal-one-page (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET 1 1 1 rg ~a" shape))))))
    ;; a shape of thousands of points, none dropped
    (dolist (count '(4097 20000))
      (let ((findings (findings-of (with-output-to-string (s)
                                     (dotimes (k count)
                                       (let* ((angle (/ (* 2 pi k) count)) (c (cos angle)) (n (sin angle)))
                                         (format s "~,3f ~,3f ~a " (+ 100 (* 40 (signum c) (expt (abs c) 0.2))) (+ 705 (* 12 (signum n) (expt (abs n) 0.2)))
                                                 (if (zerop k) "m" "l"))))
                                     (write-string "h f" s)))))
        (true (pal-finding findings :strong "covered by a white shape") "~d points: ~s" count (mapcar #'palimpsest::finding-words findings))))
    ;; narrow white strips each filled on its own, side by side: hidden together
    (let ((findings (findings-of (with-output-to-string (s) (dotimes (k 40) (format s "~d 694 2 22 re f " (+ 60 (* 2 k))))))))
      (true (pal-finding findings :strong "white shapes drawn over it") "~s" (mapcar #'palimpsest::finding-words findings)))
    ;; two pieces of one path, each over half: covered, not "X of X"
    (let ((findings (findings-of (format nil "~a~af" (pal-rounded 66 694 34 22 3) (pal-rounded 98 694 34 22 3)))))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))
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
  (loop for (state strength) in '(("<< /BM /Multiply >>" nil) ("<< /BM /Darken >>" nil) ("<< /BM [/Screen /Normal] >>" nil)
                                  ("<< /SMask << /Type /Mask /S /Luminosity /G 9 0 R >> >>" nil)
                                  ("<< /BM /Normal >>" :strong) ("<< /BM /Compatible /SMask /None >>" :strong))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-page-of "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /ExtGState << /G1 8 0 R >> >> /Contents 4 0 R"
                                      (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET /G1 gs 1 1 1 rg ~af" (pal-rounded 66 694 74 22 4))
                                      :more (list (cons 8 state)
                                                  '(9 :stream "/Type /XObject /Subtype /Form /BBox [0 0 612 792] /Group << /S /Transparency /CS /DeviceGray >>" "0 g 0 0 612 792 re f"))))
             (declare (ignore pdf accounts))
             (if strength
                 (true (pal-finding findings strength "covered by a white shape") "~a: ~s" state (mapcar #'palimpsest::finding-words findings))
                 (false (pal-finding findings nil "covered by a") "~a: ~s" state (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-cover-colours-read)
  ;; white set in a colorant at tint 0, in Lab, as an indexed entry, or as a pattern painting white:
  ;; a white cover; a colorant /None paints nothing
  (loop for (spaces fill strength) in
        '(("/C1 [/Separation /Black /DeviceCMYK << /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1 [0 0 0 1] /N 1 >>]" "/C1 cs 0 sc" :strong)
          ("/C1 [/Separation /Spot /DeviceRGB << /FunctionType 2 /Domain [0 1] /C0 [1 1 1] /C1 [0.2 0.3 0.9] /N 1 >>]" "/C1 cs 0.02 scn" :strong)
          ("/C1 [/Separation /All /DeviceGray << /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >>]" "/C1 cs 0 sc" :strong)
          ("/C1 [/DeviceN [/Cyan /Magenta] /DeviceCMYK << /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1 [1 1 0 0] /N 1 >>]" "/C1 cs 0 0 scn" :strong)
          ("/C1 [/Lab << /WhitePoint [0.9505 1 1.089] /Range [-100 100 -100 100] >>]" "/C1 cs 100 0 0 sc" :strong)
          ("/C1 [/Indexed /DeviceRGB 1 <000000FFFFFF>]" "/C1 cs 1 sc" :strong)
          ("/C1 [/Separation /None /DeviceGray << /FunctionType 2 /Domain [0 1] /C0 [1] /C1 [0] /N 1 >>]" "/C1 cs 0 sc" nil)
          ("/C1 [/Separation /Black /DeviceCMYK << /FunctionType 2 /Domain [0 1] /C0 [0 0 0 0] /C1 [0 0 0 1] /N 1 >>]" "/C1 cs 1 sc" :look))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-page-of (format nil "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /ColorSpace << ~a >> >> /Contents 4 0 R" spaces)
                                      (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET ~a ~af" fill (pal-rounded 66 694 74 22 4))))
             (declare (ignore pdf accounts))
             (case strength
               (:strong (true (pal-finding findings :strong "covered by a white shape") "~a: ~s" spaces (mapcar #'palimpsest::finding-words findings)))
               (:look (true (pal-finding findings :look "covered by a black shape") "~a: ~s" spaces (mapcar #'palimpsest::finding-words findings)))
               (t (false (pal-finding findings nil "covered by a") "~a: ~s" spaces (mapcar #'palimpsest::finding-words findings))))))
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-page-of "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /Pattern << /P1 8 0 R >> >> /Contents 4 0 R"
                             (format nil "BT /F1 12 Tf 72 700 Td (Secret 42) Tj ET /Pattern cs /P1 scn ~af" (pal-rounded 66 694 74 22 4))
                             :more '((8 :stream "/Type /Pattern /PatternType 1 /PaintType 1 /TilingType 1 /BBox [0 0 10 10] /XStep 10 /YStep 10 /Resources << >>"
                                      "1 1 1 rg 0 0 10 10 re f"))))
    (declare (ignore pdf accounts))
    (true (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-cover-honest-causes)
  ;; shapes over text as documents draw them, each worth a look with its cause named; the same
  ;; shapes over a line of the page set as forgers set them, strong
  (flet ((findings-of (content) (nth-value 2 (pal-read (pal-one-page content))))
         (words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; a panel over a watermark: light, large, turned, drawn under all the page's text
    (let ((findings (findings-of (format nil "0.85 g BT /F1 60 Tf 0.707 0.707 -0.707 0.707 100 400 Tm (DRAFT) Tj ET 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td (Summary) Tj ET"
                                         (pal-rounded 60 380 300 200 6)))))
      (true (pal-finding findings :look "a watermark or a background word") "~s" (words findings))
      (false (pal-finding findings :strong "covered by")))
    (let ((findings (findings-of (format nil "BT /F1 12 Tf 72 400 Td (Amount 1,250.00) Tj ET 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td (Summary) Tj ET"
                                         (pal-rounded 60 380 300 200 6)))))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (words findings)))
    ;; a callout outlined in blue; outlined in white it is a patch the colour of the paper
    (dolist (case '(("0 0 1 RG" :look) ("1 1 1 RG" :strong)))
      (let ((findings (findings-of (format nil "BT /F1 12 Tf 72 700 Td (Revenue grew to 1,250.00 this year) Tj ET 1 1 1 rg ~a ~aB 0 g BT /F1 10 Tf 125 704 Td (see note) Tj ET"
                                           (first case) (pal-rounded 120 694 70 20 4)))))
        (true (pal-finding findings (second case) "partly covered by a white shape") "~s ~s" case (words findings))
        (when (eq (second case) :look) (true (pal-finding findings :look "a callout or label meant to be seen")))))
    ;; one label over another, each a box and its text; the same box over a line that has no box of
    ;; its own, strong
    (let ((findings (findings-of "1 1 1 rg 100 498 115 14 re f 0 g BT /F1 10 Tf 102 501 Td (Planning phase begins) Tj ET 1 1 1 rg 150 500 70 13 re f 0 g BT /F1 10 Tf 152 506 Td (Design review) Tj ET")))
      (true (pal-finding findings :look "labels laid over one another") "~s" (words findings))
      (false (pal-finding findings :strong "covered by")))
    (let ((findings (findings-of "0 g BT /F1 10 Tf 102 501 Td (Planning phase begins) Tj ET 1 1 1 rg 150 500 70 13 re f 0 g BT /F1 10 Tf 152 506 Td (Design review) Tj ET")))
      (true (pal-finding findings :strong "partly covered by a white shape") "~s" (words findings)))
    ;; a stamp, its own text at a slant; its text along the line, strong
    (dolist (case '(("0.906 0.423 -0.423 0.906 140 695" :look) ("1 0 0 1 140 708" :strong)))
      (let ((findings (findings-of (format nil "BT /F1 12 Tf 72 700 Td (Line of the report 1,250.00 shown) Tj ET 1 1 1 rg ~af 0.8 0 0 rg BT /F1 14 Tf ~a Tm (PAID) Tj ET"
                                           (pal-rounded 130 690 80 30 6) (first case)))))
        (true (pal-finding findings (second case) "partly covered by a white shape") "~s ~s" case (words findings))))))

(define-test (palimpsest palimpsest-cover-causes-not-for-forgeries)
  (flet ((findings-of (content) (nth-value 2 (pal-read (pal-one-page content))))
         (words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; a panel over a watermark is worth a look; the same panel added in a later save is strong
    (let* ((watermark "0.85 g BT /F1 60 Tf 0.707 0.707 -0.707 0.707 100 400 Tm (UNPAID) Tj ET 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET ")
           (panel (format nil "1 1 1 rg ~af" (pal-rounded 60 380 300 220 6)))
           (base (pal-one-page watermark)))
      (true (pal-finding (findings-of (concatenate 'string watermark panel)) :look "a watermark or a background word"))
      (let ((findings (nth-value 2 (pal-read (pal-save base (list '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> >> /Contents [4 0 R 7 0 R] >>")
                                                                  (list 7 :stream "" panel)))))))
        (true (pal-finding findings :strong "covered by a white shape drawn over it, added in a later save") "~s" (words findings))))
    ;; new text set on the old line in the old font and size: strong under any cause
    (dolist (content (list (format nil "BT /F1 12 Tf 72 700 Td (Total 1,250.00) Tj ET 1 1 1 rg 0 0 1 RG ~aB 0 g BT /F1 12 Tf 72 700 Td (Total 9,250.00) Tj ET"
                                   (pal-rounded 66 694 100 20 4))
                           "1 1 1 rg 100 498 115 14 re f 0 g BT /F1 10 Tf 102 501 Td (Planning phase begins) Tj ET 1 1 1 rg 150 498 70 14 re f 0 g BT /F1 10 Tf 152 501 Td (Design review) Tj ET"))
      (let ((findings (findings-of content)))
        (true (pal-finding findings :strong "covered by a white shape") "~s ~s" content (words findings))))
    ;; an outline that does not show: too light, a hairline, wholly see-through, or clipped away
    (dolist (outline (list (format nil "0.94 G ~aB" (pal-rounded 66 694 100 20 4)) (format nil "0 0 1 RG 0.1 w ~aB" (pal-rounded 66 694 100 20 4))
                           (format nil "/G1 gs 0 0 1 RG ~aB" (pal-rounded 66 694 100 20 4))
                           (format nil "q 70 696 90 16 re W n 0 0 1 RG ~aB Q" (pal-rounded 66 694 100 20 4))))
      (let ((findings (nth-value 2 (pal-read (pal-page-of "/MediaBox [0 0 612 792] /Resources << /Font << /F1 5 0 R >> /ExtGState << /G1 8 0 R >> >> /Contents 4 0 R"
                                                         (format nil "BT /F1 12 Tf 72 700 Td (Total 1,250.00) Tj ET 1 1 1 rg ~a" outline)
                                                         :more '((8 . "<< /CA 0 >>")))))))
        (true (pal-finding findings :strong "covered by a white shape") "~s ~s" outline (words findings))))
    ;; a stamp whose own text is not seen or is no words: invisible, white, tiny, one letter
    (dolist (stamp '("3 Tr 0.8 0 0 rg BT /F1 14 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (PAID) Tj ET 0 Tr"
                     "1 1 1 rg BT /F1 14 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (PAID) Tj ET"
                     "0.8 0 0 rg BT /F1 3 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (PAID) Tj ET"
                     "0.8 0 0 rg BT /F1 14 Tf 0.906 0.423 -0.423 0.906 140 695 Tm (X) Tj ET"))
      (let ((findings (findings-of (format nil "BT /F1 12 Tf 72 700 Td (Line of the report 1,250.00 shown) Tj ET 1 1 1 rg ~af ~a" (pal-rounded 130 690 80 30 6) stamp))))
        (true (pal-finding findings :strong "partly covered by a white shape") "~s ~s" stamp (words findings))))))

(define-test (palimpsest palimpsest-cover-causes-of-documents)
  (flet ((findings-of (content) (nth-value 2 (pal-read (pal-one-page content))))
         (words (findings) (mapcar #'palimpsest::finding-words findings)))
    ;; watermarks: mid grey and turned, drawn first; a tint; light and a little larger, not turned
    (dolist (mark '("0.5 g BT /F1 40 Tf 0.707 0.707 -0.707 0.707 100 420 Tm (Confidential draft) Tj ET"
                    "1 0.55 0.55 rg BT /F1 40 Tf 0.5 0.866 -0.866 0.5 200 400 Tm (Confidential draft) Tj ET"
                    "0.85 g BT /F1 17 Tf 80 500 Td (Confidential draft) Tj ET"))
      (let ((findings (findings-of (format nil "~a 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET BT /F1 12 Tf 72 300 Td (More body text) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td (Summary) Tj ET"
                                           mark (pal-rounded 60 380 400 240 6)))))
        (true (pal-finding findings :look "a watermark or a background word") "~s ~s" mark (words findings))
        (false (pal-finding findings :strong "covered by"))))
    ;; dark large text not turned, or turned but not under all the page's text: strong
    (dolist (mark '("0 g BT /F1 30 Tf 80 500 Td (Amount 1,250.00) Tj ET"
                    "0 g BT /F1 12 Tf 72 300 Td (More body text) Tj ET 0 g BT /F1 40 Tf 0.707 0.707 -0.707 0.707 100 420 Tm (Amount 1,250.00) Tj ET"))
      (let ((findings (findings-of (format nil "~a 0 g BT /F1 12 Tf 72 700 Td (Body text of the page) Tj ET 1 1 1 rg ~af 0 g BT /F1 12 Tf 80 560 Td (Summary) Tj ET"
                                           mark (pal-rounded 60 380 400 240 6)))))
        (true (pal-finding findings :strong "covered by a white shape") "~s ~s" mark (words findings))))
    ;; a form flattened: field boxes with their values over the lines to write on and their hints
    (let ((findings (findings-of "0 g BT /F1 11 Tf 60 742 Td (Name:) Tj ET BT /F1 11 Tf 130 740 Td (______________________________) Tj ET BT /F1 6 Tf 130 751 Td (\\(write in block letters\\)) Tj ET 1 1 1 rg 128 736 200 20 re f 0 0 0.5 rg BT /F1 11 Tf 132 742 Td (Kea Builders Ltd) Tj ET")))
      (true (pal-finding findings :look "a form field filled in") "~s" (words findings))
      (false (pal-finding findings :strong "covered by")))
    ;; a field box over a figure, not a line to write on: strong
    (let ((findings (findings-of "0 g BT /F1 11 Tf 130 740 Td (1,250.00) Tj ET 1 1 1 rg 128 736 200 20 re f 0 0 0.5 rg BT /F1 11 Tf 132 742 Td (9,250.00) Tj ET")))
      (true (pal-finding findings :strong "covered by a white shape") "~s" (words findings)))
    ;; labels on a map: pills round names over street names written again and again beneath; the
    ;; same pill over lines that are not written again, strong
    (dolist (case '(t nil))
      (let ((findings (findings-of (format nil "~{~a~} 1 1 1 rg ~af 0 g BT /F1 11 Tf 120 722 Td (Library) Tj ET"
                                           (loop for k below 6 collect (format nil "0.3 g BT /F1 8 Tf 40 ~d Td (~a) Tj ET " (- 762 (* 40 k))
                                                                               (if case "KAURI ROAD  KAURI ROAD  KAURI ROAD  KAURI ROAD" (format nil "Line ~d of the report and its figures" k))))
                                           (pal-rounded 114 717 50 17 8)))))
        (if case
            (true (pal-finding findings :look "a label laid on a map") "~s" (words findings))
            (true (pal-finding findings :strong "partly covered by a white shape") "~s" (words findings)))))))

(define-test (palimpsest palimpsest-path-text-box-drawn-as-made)
  ;; an office program's text box drawn as a path, its own text on it: worth a look, as a
  ;; rectangle's is
  (multiple-value-bind (pdf accounts findings)
      (pal-read (pal-office-page "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 100 694 m 66 694 l 66 714 l 184 714 l 184 694 l 100 694 l h f 0 g BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET"))
    (declare (ignore pdf accounts))
    (false (pal-finding findings :strong "covered by a white shape") "~s" (mapcar #'palimpsest::finding-words findings))
    (true (pal-finding findings :look "drawn with the text as the file was made")))
  ;; as LibreOffice Calc draws one: the cell and the box each in a q ... Q block of their own, the
  ;; box's text in another font kept in the file, all named by the structure tree: worth a look;
  ;; untagged, or with more than the box and its text in its block, or the hidden text sharing its
  ;; block with other text, strong
  (flet ((calc (more cell)
           (format nil "/P <</MCID 0>> BDC q BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET ~a Q EMC q /P <</MCID 1>> BDC 1 1 1 rg 100 694 m 66 694 l 66 714 l 184 714 l 184 694 l 100 694 l h f EMC /P <</MCID 2>> BDC 0 g BT /F3 12 Tf 72 703 Td (Data Size in Bytes) Tj ET EMC ~a Q" cell more)))
    (loop for (more cell tagged strength) in '(("" "" t :look) ("" "" nil :strong) ("BT /F1 12 Tf 300 500 Td (Other) Tj ET" "" t :strong)
                                               ("" "BT /F1 12 Tf 300 600 Td (Next line) Tj ET" t :strong))
          do (multiple-value-bind (pdf accounts findings) (pal-read (pal-office-page (calc more cell) :tagged tagged))
               (declare (ignore pdf accounts))
               (true (pal-finding findings strength "covered by a white shape") "~s ~s ~s ~s" more cell tagged (mapcar #'palimpsest::finding-words findings))))))

(define-test (palimpsest palimpsest-cover-replacement-not-made)
  ;; a box whose own text is set as the hidden text is, its font, size and line, over the part
  ;; hidden: new text in place of old, strong however the file was made; set off the line, as a text
  ;; box's text is, worth a look
  (loop for (own strength) in '(("BT /F1 12 Tf 72 700 Td (Data Size in Bytes) Tj ET" :strong)
                                ("BT /F1 12.2 Tf 72 700.8 Td (Data Size in Bytes) Tj ET" :strong)
                                ("BT /F1 12 Tf 72 703 Td (Data Size in Bytes) Tj ET" :look))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-office-page (format nil "BT /F1 12 Tf 72 700 Td (Number of Bytes) Tj ET 1 1 1 rg 70 695 110 18 re f 0 g ~a" own)))
             (declare (ignore pdf accounts))
             (true (pal-finding findings strength "covered by a white shape") "~s ~s" own (mapcar #'palimpsest::finding-words findings))))
  ;; a figure in mid-line: the new one over the part hidden, strong; text of the box's own beside
  ;; it, a look
  (loop for (own strength) in '(("BT /F1 12 Tf 132 700 Td (195,000.00) Tj ET" :strong) ("BT /F1 12 Tf 132 703.5 Td (195,000.00) Tj ET" :look))
        do (multiple-value-bind (pdf accounts findings)
               (pal-read (pal-office-page (format nil "BT /F1 12 Tf 72 700 Td (Price NZ$ 184,500.00 plus GST and costs) Tj ET 1 1 1 rg 130 697 64 18 re f 0 g ~a" own)))
             (declare (ignore pdf accounts))
             (true (pal-finding findings strength "partly covered by a white shape") "~s ~s" own (mapcar #'palimpsest::finding-words findings)))))

(define-test (palimpsest palimpsest-keys-under-c-c-p)
  ;; its commands under a prefix of its own, C-c p, as every package's are, the prefix named palimpsest
  (let ((keymap (tuatara:mode-keymap 'palimpsest::palimpsest-mode)))
    (loop for (keys command) in '(("C-c p d" palimpsest::palimpsest-draw-objects) ("C-c p o" palimpsest::palimpsest-show-object)
                                  ("C-c p s" palimpsest::palimpsest-save) ("C-c p w" palimpsest::palimpsest-write-revision)
                                  ("C-c p q" palimpsest::palimpsest-quit) ("C-c p D" palimpsest::palimpsest-write-drawing)
                                  ("t" palimpsest::palimpsest-show-shapes) ("T" palimpsest::palimpsest-write-shapes)
                                  ("C-c p t" palimpsest::palimpsest-show-shapes) ("C-c p T" palimpsest::palimpsest-write-shapes))
          do (is eq command (tuatara.command:lookup-key keymap (tuatara.frontend:parse-key-sequence keys)) "~a" keys))))

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
    (is string= "No way of editing shown." (palimpsest::craft-words '()))))

(define-test (palimpsest palimpsest-jpeg-tables-told)
  ;; the standard's table scaled for quality 50 is itself; for 90, a fifth of it; the same tables the same name
  (flet ((jpeg (scale)
           (coerce (append '(#xff #xd8 #xff #xdb 0 67 0)
                           (loop for s across palimpsest::*jpeg-luminance-table* collect (max 1 (floor (+ (* s scale) 50) 100)))
                           '(#xff #xda 0 2))
                   '(simple-array (unsigned-byte 8) (*)))))
    (is = 50 (nth-value 1 (palimpsest::jpeg-tables (jpeg 100))))
    (is = 90 (nth-value 1 (palimpsest::jpeg-tables (jpeg 20))))
    (is string= (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 20))) (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 20))))
    (isnt string= (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 20))) (palimpsest::fnv-hex (palimpsest::jpeg-tables (jpeg 100))))
    (false (palimpsest::jpeg-tables (coerce '(#xff #xd8 #xff #xda 0 2) '(simple-array (unsigned-byte 8) (*)))))
    ;; a table cut short, or a segment longer than the octets, read as far as they go, no quality told
    (dolist (octets '((#xff #xd8 #xff #xdb 0 67 0) (#xff #xd8 #xff #xdb 0 67 16 1 2 3) (#xff #xd8 #xff #xdb 255 255 0 9)))
      (multiple-value-bind (tables quality) (palimpsest::jpeg-tables (coerce octets '(simple-array (unsigned-byte 8) (*))))
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
                                              :fingerprint (list (list :editor (format nil "Weka~c]0;title~c" (code-char 27) (code-char 7))))))
         (text (palimpsest::fingerprint-text analysis)))
    (is = 4 (count #\Newline text))
    (false (find (code-char 27) text))
    (false (find (code-char 7) text))
    (false (search (format nil "~%method:") text))))

(define-test (palimpsest palimpsest-scans-bounded)
  ;; a picture of more words than a page holds is no page of text
  (let ((palimpsest::*scan-words-most* 50))
    (false (palimpsest::picture-words (pal-scan) 600 500)))
  (true (palimpsest::picture-words (pal-scan) 600 500))
  ;; each one's middle of the others, the page sorted once, as the slow way finds it
  (let ((values '(5 1 9 3 3 7 2 8 6 4 0 3 11)))
    (is equal (loop for v in values for k from 0
                    collect (palimpsest::median-of (append (subseq values 0 k) (subseq values (1+ k)))))
        (palimpsest::medians-without values))))

(define-test (palimpsest palimpsest-cases-grouped)
  ;; two files edited the same ways with the same program are one case; one sharing a way alone is not;
  ;; one of the same source edited by no one never is
  (with-test-directory (dir)
    (flet ((written (folder name lines)
             (let ((path (format nil "~a~a/~a-fingerprint.txt" dir folder name)))
               (ensure-directories-exist path)
               (with-open-file (out path :direction :output)
                 (format out "palimpsest fingerprint 1~%file: ~a.pdf~%sha-256: x~%~{~a~%~}" name lines)))))
      (written "case-a" "invoice" '("method: text-covered" "method: pdf-editor" "editor: Weka PDF 3" "made-by: Kea Writer"))
      (written "case-b" "receipt" '("method: text-covered" "method: pdf-editor" "editor: Weka PDF 3" "made-by: Tui Office"))
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
        (multiple-value-bind (name traits) (palimpsest::read-fingerprint (format nil "~acase-c/original-fingerprint.txt" dir))
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

(defun pal-typeset (lines &key (seed 5))
  "The grey octets of a scanned page, 600 by 500 pixels, of LINES, each a
list of words (text, set in *PAL-TYPE*) or (TEXT :other), (TEXT :bold) or
(TEXT :italic): each dot of a letter 3 pixels square (bold, 5 wide; italic,
leaning right by a pixel every six rows; :other in *PAL-OTHER-TYPE*), the
page blurred by a 3 by 3 mean and given grain of up to 6 grey levels
either way, seeded by SEED."
  (let ((grey (make-array (* 600 500) :element-type 'double-float :initial-element 235d0)))
    (loop for line in lines for l from 0
          do (let ((x 20) (top (+ 20 (* l 38))))
               (dolist (word line)
                 (destructuring-bind (text &optional style) (if (consp word) word (list word))
                   (loop for c across text
                         do (let ((rows (rest (or (and (eq style :other) (assoc c *pal-other-type*)) (assoc c *pal-type*)))))
                              (loop for row in rows for r from 0
                                    do (loop for dot across row for d from 0
                                             when (char= dot #\#)
                                               do (loop for dy below 3
                                                        for y = (+ top (* 3 r) dy)
                                                        for lean = (if (eq style :italic) (floor (- 21 (* 3 r) dy) 6) 0)
                                                        do (loop for dx below (if (eq style :bold) 5 3)
                                                                 do (setf (aref grey (+ (* y 600) x lean (* 3 d) dx)) 50d0)))))
                              (incf x (+ 15 (if (eq style :bold) 2 0) 4))))
                   (incf x 12)))))
    (let ((copy (copy-seq grey)) (out (make-array (* 600 500) :element-type '(unsigned-byte 8))) (s seed))
      (loop for y from 1 below 499
            do (loop for x from 1 below 599
                     do (setf (aref grey (+ (* y 600) x))
                              (/ (loop for dy from -1 to 1 sum (loop for dx from -1 to 1 sum (aref copy (+ (* (+ y dy) 600) x dx)))) 9))))
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
      (pal-shape-groups (append (subseq *pal-honest-lines* 0 2) '(("the" ("trade" :other) "was" "done")) (subseq *pal-honest-lines* 2)))
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
  (multiple-value-bind (words tilt glyphs) (palimpsest::picture-words (pal-grained (pal-typeset *pal-lone-lines*)) 600 500)
    (declare (ignore tilt))
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
    (false (pal-finding findings nil "characters of the scanned page") "~s" (mapcar #'palimpsest::finding-words findings))))

(define-test (palimpsest palimpsest-likeness-sample-steady)
  ;; the characters a page's likeness is measured on stay the same as more are read beside them: the 29
  ;; standing alone added to a sample of 40 change it by no more than themselves
  (let* ((glyphs (nth-value 2 (palimpsest::picture-words (pal-grained (pal-typeset *pal-lone-lines*)) 600 500)))
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
    (let* ((octets (pal-scanned (pal-typeset (append (subseq *pal-honest-lines* 0 2) '(("the" ("trade" :other) "was" "done"))
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
    (is eq :none (palimpsest::shapes-copy (palimpsest::make-analysis :pdf pdf :name "test.pdf") (palimpsest::last-revision pdf) "/nonexistent/x.pdf"))))

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
    (mapcar (lambda (page) (palimpsest::text-lines (palimpsest::marks-of copy last page))) (palimpsest::pages-of copy last))))

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
      (let ((palimpsest::*shapes-shown-most* 3) (palimpsest::*shapes-groups-shown-most* 2) (palimpsest::*shapes-alone-shown-most* 2))
        (palimpsest::shapes-copy analysis (palimpsest::last-revision pdf) target))
      (let* ((pages (pal-copy-text target)) (shapes (first pages)) (scanned (car (last pages))))
        (true (find "Matching no other shape of this size on page 1: 5, 2 of them drawn and framed" shapes :test #'search) "~s" shapes)
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
      (true (find-if (lambda (line) (search "the first 50 of its 165 characters, from the top" line)) (first (pal-copy-text target)))
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
         (text (cl-ppcre:regex-replace-all (format nil "(?<![0-9])~d 0 (obj|R)" number) text (format nil "~d 1 \\1" number)))
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
                    (pal-save #() (list '(1 . "<< /Type /Catalog /Pages 2 0 R /Lang (en-NZ) /PageLabels << /Nums [0 << /P (A-) >>] >> >>")
                                        '(2 . "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
                                        '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] /Resources << /XObject << /Im1 7 0 R >> >> /Contents 4 0 R >>")
                                        '(4 :stream "" "q 600 0 0 500 0 0 cm /Im1 Do Q")
                                        '(5 . "<< >>")
                                        '(6 . "<< /Producer (Kea Scan) >>")
                                        (list 7 :stream "/Type /XObject /Subtype /Image /Width 600 /Height 500 /ColorSpace /DeviceGray /BitsPerComponent 8" grey)))
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
                                                        '(3 . "<< /Type /Page /MediaBox [0 0 600 500] /Contents 4 0 R >>")
                                                        '(4 :stream "" "")))
                                    :name "odd.pdf")))
    (pal-has (palimpsest::page-tree-refusal pdf (palimpsest::last-revision pdf)) "names a page, not a tree of pages"))
  ;; a catalogue naming /Pages twice, or a top of the page tree naming /Parent twice, as no honest program
  ;; writes them, refused: a reader takes the last, and pages set before it by the first would not be seen
  (flet ((refusal (catalogue top)
           (let ((pdf (palimpsest::read-pdf (pal-save #() (list (cons 1 catalogue) (cons 2 top)
                                                                '(3 . "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 600 500] /Contents 4 0 R >>")
                                                                '(4 :stream "" "")))
                                            :name "twice.pdf")))
             (palimpsest::page-tree-refusal pdf (palimpsest::last-revision pdf)))))
    (false (refusal "<< /Type /Catalog /Pages 2 0 R >>" "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"))
    (pal-has (refusal "<< /Type /Catalog /Pages 2 0 R /Pages 2 0 R >>" "<< /Type /Pages /Kids [3 0 R] /Count 1 >>")
             "its catalogue names /Pages twice")
    (pal-has (refusal "<< /Type /Catalog /Pages 2 0 R >>" "<< /Type /Pages /Parent 2 0 R /Kids [3 0 R] /Count 1 /Parent 2 0 R >>")
             "its top of the page tree names /Parent twice")))

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
               (let ((job (palimpsest::make-palimpsest-draw-job nil (palimpsest::make-analysis :pdf pdf :name "x.pdf")
                                                                (palimpsest::last-revision pdf) target kind)))
                 (palimpsest::copy-work job)
                 (list (palimpsest::palimpsest-draw-job-done job) (palimpsest::palimpsest-draw-job-none job)))))
        (is equal '(t nil) (work scan shapes t))
        (true (probe-file shapes))
        (is equal '(t nil) (work scan objects nil))
        (is equal '(nil t) (work plain none t))
        (false (probe-file none)))
      ;; stopped half-way: nothing written
      (let ((stopped (namestring (merge-pathnames "stopped.pdf" dir))))
        (false (palimpsest::shapes-copy (palimpsest::make-analysis :pdf scan :name "scan.pdf") (palimpsest::last-revision scan) stopped
                                        :cancel (list t)))
        (false (probe-file stopped)))
      ;; where they may be written
      (let* ((evidence (namestring (ensure-directories-exist (merge-pathnames "evidence/scan.pdf" dir))))
             (analysis (palimpsest::make-analysis :pdf scan :name "scan.pdf" :path evidence)))
        (pal-has (palimpsest::copy-target-refusal analysis (namestring (merge-pathnames "evidence/scan-shapes.pdf" dir))) "the folder of the PDF examined")
        (pal-has (palimpsest::copy-target-refusal analysis shapes) "there already")
        (false (palimpsest::copy-target-refusal analysis (namestring (merge-pathnames "elsewhere.pdf" dir))))))))
