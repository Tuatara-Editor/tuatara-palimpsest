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
             (let ((start (get-internal-real-time)))
               (loop repeat 20 do (palimpsest::check-images pdf revision))
               (/ (max 1 (- (get-internal-real-time) start)) count)))))
    (let ((small (per-picture 400)) (large (per-picture 1600)))
      (true (< large (* 2 small)) "~,2f times as long for each picture among 1,600 as among 400" (/ large small)))))

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
          (when crisp
            (loop for y from (- top 3) below (+ top 21)
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

(defun pal-grained (grey)
  "GREY, a scanned page (PAL-SCAN), with grain over all of it, ink too: each
pixel straying by up to 12, as no two prints of a stroke are alike."
  (let ((seed 777))
    (dotimes (i (length grey) grey)
      (setf seed (mod (+ (* seed 1103515245) 12345) 2147483648))
      (setf (aref grey i) (max 0 (min 255 (+ (aref grey i) -12 (mod (ash seed -16) 25))))))))

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
  ;; a word both pasted and lowered: two measures apart agree, a strong sign
  (multiple-value-bind (pdf accounts findings) (pal-read (pal-scanned (pal-scan :pasted t :lowered t)))
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

(define-test (palimpsest palimpsest-keys-under-c-c-p)
  ;; its commands under a prefix of its own, C-c p, as every package's are, the prefix named palimpsest
  (let ((keymap (tuatara:mode-keymap 'palimpsest::palimpsest-mode)))
    (loop for (keys command) in '(("C-c p d" palimpsest::palimpsest-draw-objects) ("C-c p o" palimpsest::palimpsest-show-object)
                                  ("C-c p s" palimpsest::palimpsest-save) ("C-c p w" palimpsest::palimpsest-write-revision)
                                  ("C-c p q" palimpsest::palimpsest-quit) ("C-c p D" palimpsest::palimpsest-write-drawing))
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
