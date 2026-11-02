;;;; pdf.lisp --- A PDF read from its octets alone: its saves, cross-reference sections, objects and streams.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A PDF changed after it was made keeps what it was: each save appends the
;;;; objects it changes, a cross-reference section saying where they are, a
;;;; trailer pointing back to the section before, and an %%EOF line. Here the
;;;; file is read once, whole, as octets: every object written in it found by
;;;; scanning its body, every save (revision) found by its %%EOF and read by
;;;; its cross-reference section (a table, a stream, or both), and the
;;;; objects each revision holds told apart from those of the revision before.
;;;; Objects and their streams are read by Tuatara's PDF reader (Flate, LZW,
;;;; ASCII hex and 85, run length, with their predictors), each stream held
;;;; to a size; the data of pictures is never decoded. Nothing in the file
;;;; is run, and the file is never written: whatever it holds that does not
;;;; make sense is noted, never an error.

(in-package #:palimpsest)

(define-package-setting *palimpsest-largest-file* (* 512 1024 1024)
  "The most octets a PDF may have to be examined: 512 MB as standard. The file
is read whole, into memory, once.")

(define-package-setting *palimpsest-largest-stream* (* 64 1024 1024)
  "The most octets one stream of a PDF (a page's content, an object stream, a
cross-reference stream) may decode to, and a page's content streams together:
64 MB as standard. A stream that would decode to more is read no further, nor
the streams of a page past it, and the report says so.")

(define-package-setting *palimpsest-most-decoded* (* 8 1024 1024 1024)
  "The most octets of streams decoded and of page content read while one PDF
is examined, all together: 8 GB as standard, a stream decoded again or a page
read again counted again. Past it nothing more is decoded or read, and the
report says so.")

(define-package-setting *palimpsest-most-objects* 1000000
  "The most objects of a PDF read, and the most entries one cross-reference
section may list: a million as standard. Past them the rest is passed over,
and the report says so.")

(defparameter *most-entries* 5000000
  "Cross-reference entries read from a PDF at most, all its sections together:
saves that each name a section of a million entries read no further than this.")

(define-package-setting *palimpsest-most-revisions* 500
  "The most saves (revisions) of a PDF taken apart: 500 as standard. A file
saved more often has the rest listed as not read.")

(defparameter *decoded-cache-limit* (* 256 1024 1024)
  "The most octets of decoded streams kept while a PDF is examined, so that a
stream shared by many revisions is decoded once.")

;;; Octets

(defun digit-p (c)
  "True when octet C is a decimal digit."
  (<= 48 c 57))

(defun hex-offset (n)
  "N, an offset into a file, in hex as the report writes it: 0x0001A2F0."
  (format nil "0x~8,'0x" n))

;;; MD5, as some programs that make PDFs mark them with

(defparameter *md5-shifts*
  #(7 12 17 22 7 12 17 22 7 12 17 22 7 12 17 22 5 9 14 20 5 9 14 20 5 9 14 20 5 9 14 20
    4 11 16 23 4 11 16 23 4 11 16 23 4 11 16 23 6 10 15 21 6 10 15 21 6 10 15 21 6 10 15 21)
  "How far each of MD5's 64 steps turns its word left.")

(defparameter *md5-constants*
  (let ((v (make-array 64 :element-type '(unsigned-byte 32))))
    (dotimes (i 64 v) (setf (aref v i) (floor (* (abs (sin (float (1+ i) 1d0))) 4294967296)))))
  "MD5's 64 constants: the integer part of the sine of each step's number, times 2^32.")

(defun md5-hex (octets &key (start 0) (end (length octets)))
  "The MD5 digest (RFC 1321) of OCTETS from START to END, in upper-case hex."
  (declare (type (simple-array (unsigned-byte 8) (*)) octets) (type fixnum start end))
  (let* ((length (- end start))
         (padded (* 64 (ceiling (+ length 9) 64)))
         (tail (make-array (- padded (* 64 (floor length 64))) :element-type '(unsigned-byte 8) :initial-element 0))
         (whole (* 64 (floor length 64)))
         (words (make-array 16 :element-type '(unsigned-byte 32)))
         (a0 #x67452301) (b0 #xefcdab89) (c0 #x98badcfe) (d0 #x10325476)
         (shifts *md5-shifts*) (constants *md5-constants*))
    (declare (type (unsigned-byte 32) a0 b0 c0 d0) (type (simple-array (unsigned-byte 32) (64)) constants))
    ;; the last part of the octets, with a 1 bit, 0 bits and the length in bits after it
    (replace tail octets :start2 (+ start whole) :end2 end)
    (setf (aref tail (- length whole)) #x80)
    (let ((bits (* 8 length)))
      (dotimes (k 8) (setf (aref tail (+ (- (length tail) 8) k)) (ldb (byte 8 (* 8 k)) bits))))
    (flet ((block-at (source at)
             (declare (type (simple-array (unsigned-byte 8) (*)) source) (type fixnum at))
             (dotimes (k 16)
               (let ((i (+ at (* 4 k))))
                 (setf (aref words k) (logior (aref source i) (ash (aref source (+ i 1)) 8)
                                              (ash (aref source (+ i 2)) 16) (ash (aref source (+ i 3)) 24)))))
             (let ((a a0) (b b0) (c c0) (d d0))
               (declare (type (unsigned-byte 32) a b c d))
               (dotimes (i 64)
                 (multiple-value-bind (f g)
                     (cond ((< i 16) (values (logior (logand b c) (logand (logxor b #xffffffff) d)) i))
                           ((< i 32) (values (logior (logand d b) (logand (logxor d #xffffffff) c)) (mod (+ (* 5 i) 1) 16)))
                           ((< i 48) (values (logxor b c d) (mod (+ (* 3 i) 5) 16)))
                           (t (values (logxor c (logior b (logxor d #xffffffff))) (mod (* 7 i) 16))))
                   (let* ((sum (ldb (byte 32 0) (+ a f (aref constants i) (aref words g))))
                          (shift (svref shifts i))
                          (turned (ldb (byte 32 0) (logior (ash sum shift) (ash sum (- shift 32))))))
                     (setf a d d c c b b (ldb (byte 32 0) (+ b turned))))))
               (setf a0 (ldb (byte 32 0) (+ a0 a)) b0 (ldb (byte 32 0) (+ b0 b))
                     c0 (ldb (byte 32 0) (+ c0 c)) d0 (ldb (byte 32 0) (+ d0 d))))))
      (loop for at from start below (+ start whole) by 64 do (block-at octets at))
      (loop for at from 0 below (length tail) by 64 do (block-at tail at)))
    (format nil "~{~2,'0X~}" (loop for word in (list a0 b0 c0 d0) append (loop for k below 4 collect (ldb (byte 8 (* 8 k)) word))))))

;;; PDF values, as Tuatara's reader gives them

(defun name-p (value name)
  "True when VALUE is the name NAME (written with its /)."
  (and (stringp value) (string= value name)))

;;; The file as found by scanning it

(defstruct (entry (:constructor make-entry (kind place generation &optional index)) (:copier nil))
  "What a cross-reference section says of an object: KIND :used (at offset
PLACE), :packed (in the object stream numbered PLACE, at INDEX) or :free;
GENERATION its generation number."
  kind place generation index)

(setf (documentation 'entry-p 'function) "True when OBJECT is an ENTRY.")

(defstruct (revision (:copier nil))
  "A save of a PDF: INDEX from 0 (the original); START and END, its octets in
the file (END just past its %%EOF line); EOF, where its %%EOF is; XREF-AT,
where its cross-reference section is, XREF-KIND :table, :stream, :hybrid or
:none; TRAILER, its trailer dictionary; PREV, where its trailer says the
section before is; ENTRIES, what its section says of each object number (a
hash table); PROBLEMS, what does not make sense in it, in words; ADDED,
CHANGED, SAME and FREED, the object numbers it adds, writes with a new body,
writes again unchanged and frees; PAGES, its pages (PAGE), read on asking;
ROLES, what each object is to the document as it then stood (read on
asking); LINEARISED, true for the first save of a file prepared for the web;
LATEST, true for the last save, the file as it is now;
STARTXREF-AT, where its startxref keyword is, and STARTXREF, the offset it
names; TRAILER-AT, where its trailer is (its trailer keyword, or its
cross-reference stream's object)."
  index start end eof xref-at (xref-kind :none) trailer prev startxref-at startxref trailer-at (entries (make-hash-table)) (problems '())
  (added '()) (changed '()) (same '()) (freed '()) (pages :unread) (roles nil) linearised latest)

(setf (documentation 'revision-p 'function) "True when OBJECT is a REVISION.")

(defstruct (pdf (:copier nil))
  "A PDF read for examining: OCTETS, the whole file; PATH and NAME; HEADER,
where %PDF- begins, VERSION as it says; FOUND, the objects its body holds
as scanning finds them (a vector of PDF-INDIRECT, in order); REVISIONS,
a vector of REVISION, the original first; OBJECTS, the objects read by
offset; DECODED, streams decoded; ENCRYPTED, true when its trailer names
an encryption; NOTES, what could not be read, in words; CACHE, what is
worked out once while it is examined (fonts, what pages draw), a property
list."
  octets path name (header 0) version (found #()) (revisions #()) (cache '())
  (objects (make-hash-table)) (decoded (make-hash-table :test 'eq)) (decoded-size 0)
  (packed (make-hash-table :test 'eq)) encrypted (notes '()))

(setf (documentation 'pdf-p 'function) "True when OBJECT is a PDF.")

(defvar *depth* 0
  "How deep references are being followed while an object is read: held
below 64, so that a file whose objects refer to one another in a circle
is read no further.")

(defun note (pdf format &rest arguments)
  "Note in PDF what could not be read of it (FORMAT with ARGUMENTS), once."
  (let ((words (apply #'format nil format arguments)))
    (pushnew words (pdf-notes pdf) :test #'string=)))

(defun scan-body (pdf &optional (progress (constantly nil)))
  "Find every object written in PDF's body, in the order written, with the
places of their streams (PDF-FOUND); PROGRESS is called with the offset
reached now and then."
  (let ((found '()) (objects 0))
    (map-pdf-written-objects
     (lambda (object)
       (push object found)
       (setf (gethash (pdf-indirect-get object :at) (pdf-objects pdf)) object)
       (when (zerop (mod (incf objects) 4096)) (funcall progress (pdf-indirect-get object :at)))
       (when (> objects *palimpsest-most-objects*)
         (note pdf "It holds more than ~:d objects (*palimpsest-most-objects*): the rest were not read"
               *palimpsest-most-objects*)
         t))
     (pdf-octets pdf))
    (setf (pdf-found pdf) (coerce (nreverse found) 'vector))))

;;; Streams decoded

(defun spend-decoded (pdf count)
  "Count COUNT more octets decoded or read as page content in PDF against
*PALIMPSEST-MOST-DECODED*: true when they are within it; NIL, not counted,
when they would pass it, and that is noted in PDF."
  (if (<= (+ (getf (pdf-cache pdf) :decoded-read 0) count) *palimpsest-most-decoded*)
      (incf (getf (pdf-cache pdf) :decoded-read 0) count)
      (progn (note pdf "Its streams decode to more than ~:d octets in all, a stream decoded again and a page read again counted again (*palimpsest-most-decoded*): the rest were not read"
                   *palimpsest-most-decoded*)
             nil)))

(defun stream-data (pdf object &key (limit *palimpsest-largest-stream*))
  "The decoded octets of OBJECT's stream (a PDF-INDIRECT), as two values: the octets
\(the picture data itself for a picture, undecoded), and NIL, the name of
the picture's filter, or why it could not be decoded, in words. NIL for an
object with no stream. Kept, so that a stream is decoded once."
  (when (pdf-indirect-get object :stream-start)
    (let ((kept (gethash object (pdf-decoded pdf))))
      (if kept
          (values (car kept) (cdr kept))
          (let ((raw (raw-stream pdf object))
                (dict (pdf-indirect-get object :value)))
            (multiple-value-bind (data why)
                (cond ((and (pdf-encrypted pdf) (not (pdf-get dict "/F")) (not (name-p (pdf-get dict "/Type") "/XRef")))
                       (values raw "it is encrypted"))
                      ((not (spend-decoded pdf 0))
                       (values nil "not decoded: the file's streams decode to too much in all"))
                      (t (multiple-value-bind (data why) (pdf-stream-octets dict raw :limit limit)
                           (when data (incf (getf (pdf-cache pdf) :decoded-read 0) (length data)))
                           (values data why))))
              (when (and data (< (+ (pdf-decoded-size pdf) (length data)) *decoded-cache-limit*))
                (incf (pdf-decoded-size pdf) (length data))
                (setf (gethash object (pdf-decoded pdf)) (cons data why)))
              (values data why)))))))

(defun forget-workings (pdf)
  "Let go of what PDF keeps while it is examined so as not to work it out
again: its decoded streams, object streams, the marks its pages draw and its
fonts. What is asked for later is read again."
  (setf (pdf-decoded pdf) (make-hash-table :test 'eq) (pdf-decoded-size pdf) 0
        (pdf-packed pdf) (make-hash-table :test 'eq))
  (dolist (key '(:marks :marks-queue :marks-kept :fonts))
    (remf (pdf-cache pdf) key)))

(defun raw-stream (pdf object &optional most)
  "The octets of OBJECT's stream as the file holds them, not decoded, MOST of
them when given; NIL for one with none."
  (let ((start (pdf-indirect-get object :stream-start)))
    (and start (subseq (pdf-octets pdf) start (if most
                                                    (min (pdf-indirect-get object :stream-end) (+ start most))
                                                    (pdf-indirect-get object :stream-end))))))

;;; Objects

(defun object-at-offset (pdf at)
  "The object PDF's file holds at AT (a PDF-INDIRECT), read once; NIL when none begins there."
  (when (and (integerp at) (<= 0 at) (< at (length (pdf-octets pdf))))
    (multiple-value-bind (kept there) (gethash at (pdf-objects pdf))
      (if there
          kept
          (setf (gethash at (pdf-objects pdf)) (pdf-indirect-object-at (pdf-octets pdf) at))))))

(defun packed-objects (pdf stream)
  "What the object stream STREAM (a PDF-INDIRECT) holds, as two values: a vector of
\(NUMBER . OFFSET), each object's number and where it is in the decoded
octets, and those octets; NIL when it cannot be read."
  (let ((kept (gethash stream (pdf-packed pdf))))
    (if kept
        (values (car kept) (cdr kept))
        (let* ((data (stream-data pdf stream))
               (pairs (and data (pdf-object-stream-offsets (pdf-indirect-get stream :value) data))))
          (when pairs
            (setf (gethash stream (pdf-packed pdf)) (cons pairs data))
            (values pairs data))))))

(defun packed-object (pdf stream index number)
  "The object NUMBER the object stream STREAM (a PDF-INDIRECT) holds, at INDEX
among them or, when another is there, wherever it is; a PDF-INDIRECT, or NIL."
  (multiple-value-bind (pairs data) (packed-objects pdf stream)
    (and pairs (pdf-packed-object data pairs number :index index :in (pdf-indirect-get stream :number)))))

(defun saves-by-number (pdf)
  "For each object number, the indexes of the revisions of PDF whose
cross-reference sections name it, in order (a hash table of vectors); worked
out once for PDF's revisions as they are now."
  (let ((kept (getf (pdf-cache pdf) :saves-by-number)))
    (if (and kept (eq (car kept) (pdf-revisions pdf)))
        (cdr kept)
        (let ((table (make-hash-table)) (revisions (pdf-revisions pdf)))
          (loop for r from (1- (length revisions)) downto 0
                do (job-check-stop)
                   (maphash (lambda (number entry)
                              (declare (ignore entry))
                              (push r (gethash number table)))
                            (revision-entries (aref revisions r))))
          (maphash (lambda (number saves) (setf (gethash number table) (coerce saves 'simple-vector))) table)
          (setf (getf (pdf-cache pdf) :saves-by-number) (cons revisions table))
          table))))

(defun lookup (pdf revision number)
  "What the cross-reference sections of REVISION and those before say of
the object NUMBER, the latest first (an ENTRY), and the REVISION saying it;
NIL when none does."
  (let ((saves (gethash number (saves-by-number pdf)))
        (index (revision-index revision)))
    (when saves
      ;; the last save at or before INDEX naming NUMBER
      (let ((low 0) (high (length saves)))
        (loop while (< low high)
              do (let ((middle (floor (+ low high) 2)))
                   (if (<= (svref saves middle) index) (setf low (1+ middle)) (setf high middle))))
        (when (plusp low)
          (let ((r (aref (pdf-revisions pdf) (svref saves (1- low)))))
            (values (gethash number (revision-entries r)) r)))))))

(defun found-object (pdf number limit)
  "The last object NUMBER written in PDF's body before the offset LIMIT, as scanning found it; NIL for none."
  (let ((index (or (getf (pdf-cache pdf) :by-number)
                   (let ((table (make-hash-table)))
                     (loop for object across (pdf-found pdf) do (push object (gethash (pdf-indirect-get object :number) table)))
                     (setf (getf (pdf-cache pdf) :by-number) table)))))
    ;; each number's objects, the last written first
    (find-if (lambda (object) (< (pdf-indirect-get object :at) limit)) (gethash number index))))

(defun revision-object (pdf revision number)
  "The object NUMBER as REVISION has it (a PDF-INDIRECT): where its cross-reference
says, else, when that holds no such object, the last of that number written
before REVISION ends; NIL when it has none, or it is free."
  (when (and (integerp number) (< *depth* 64))
    (let ((*depth* (1+ *depth*)))
      (multiple-value-bind (entry) (lookup pdf revision number)
        (case (and entry (entry-kind entry))
          (:used (let ((object (object-at-offset pdf (entry-place entry))))
                   (if (and object (eql (pdf-indirect-get object :number) number))
                       object
                       (found-object pdf number (revision-end revision)))))
          (:packed (let ((stream (revision-object pdf revision (entry-place entry))))
                     (and stream (null (pdf-indirect-get stream :packed-in))
                          (packed-object pdf stream (entry-index entry) number))))
          (:free nil)
          (t (and (eq (revision-xref-kind revision) :none) (found-object pdf number (revision-end revision)))))))))

(defun deref (pdf revision value)
  "VALUE with a reference followed, in REVISION, to the value of what it names."
  (if (pdf-ref-p value)
      (let ((object (revision-object pdf revision (second value))))
        (and object (pdf-indirect-get object :value)))
      value))

(defun dref (pdf revision dict key)
  "The value of KEY in DICT, a reference followed in REVISION."
  (deref pdf revision (pdf-get dict key)))

;;; Cross-reference sections

(defun read-xref-table (pdf at)
  "The cross-reference table of PDF at AT (where xref is), as three values:
its entries (a hash table of number to ENTRY), its trailer, and what does not
make sense in it, in words; NIL when there is no table at AT."
  (let ((entries (make-hash-table)) (problems '()) (total 0))
    (multiple-value-bind (ending trailer where)
        (map-pdf-cross-reference-table
         (lambda (number offset generation kind)
           (cond ((> (incf total) *palimpsest-most-objects*)
                  (push "its cross-reference table lists more entries than are read (*palimpsest-most-objects*)"
                        problems)
                  t)
                 ((gethash number entries)
                  (push (format nil "object ~d is listed twice in its cross-reference table" number) problems)
                  nil)
                 (t (setf (gethash number entries) (make-entry kind offset generation))
                    nil)))
         (pdf-octets pdf) at)
      (when ending
        (case ending
          (:cut-short (push (format nil "its cross-reference table at ~a is cut short" (hex-offset at)) problems))
          (:unreadable (push (format nil "an entry of its cross-reference table at ~a cannot be read" (hex-offset where))
                             problems)))
        (values entries trailer (nreverse problems))))))

(defun read-xref-stream (pdf object)
  "The entries of OBJECT, a cross-reference stream (a PDF-INDIRECT), as a hash table
of number to ENTRY, and what does not make sense in it, in words, as two
values; NIL when it is no such stream or cannot be decoded."
  (let ((dict (pdf-indirect-get object :value)))
    (when (name-p (pdf-get dict "/Type") "/XRef")
      (multiple-value-bind (data why) (stream-data pdf object)
        (if (or (null data) why)
            (values nil (list (format nil "its cross-reference stream, object ~d, ~a" (pdf-indirect-get object :number) (or why "is empty"))))
            (let* ((number (pdf-indirect-get object :number)) (entries (make-hash-table)) (problems '()) (total 0)
                   (ending (map-pdf-cross-reference-stream
                            (lambda (entry-number type second third)
                              (if (> (incf total) *palimpsest-most-objects*)
                                  (progn (push "its cross-reference stream lists more entries than are read (*palimpsest-most-objects*)"
                                               problems)
                                         t)
                                  (progn (unless (gethash entry-number entries)
                                           (setf (gethash entry-number entries)
                                                 (case type
                                                   (0 (make-entry :free second third))
                                                   (1 (make-entry :used second third))
                                                   (2 (make-entry :packed second 0 third))
                                                   (t (make-entry :free 0 0)))))
                                         nil)))
                            dict data)))
              (case ending
                ((nil) (values nil (list (format nil "its cross-reference stream, object ~d, gives no field widths it can be read by"
                                                 number))))
                (:short (push (format nil "its cross-reference stream, object ~d, is shorter than its index says" number) problems)
                        (values entries (nreverse problems)))
                (t (values entries (nreverse problems))))))))))

(defun read-section (pdf at)
  "The cross-reference section of PDF at AT, as five values: its entries,
its trailer, its kind (:table, :stream, :hybrid), where it really is and
what does not make sense in it; NIL when there is none at AT, nor near it.
Read once for each place, its entries shared by every save naming it; once
the PDF's sections have given *MOST-ENTRIES* entries in all, no more is
read, and that is said."
  (let ((sections (or (getf (pdf-cache pdf) :sections)
                      (setf (getf (pdf-cache pdf) :sections) (make-hash-table)))))
    (values-list
     (or (gethash at sections)
         (setf (gethash at sections)
               (if (>= (or (getf (pdf-cache pdf) :entries-read) 0) *most-entries*)
                   (list nil nil nil nil
                         (list (format nil "its cross-reference section was not read: the file's sections already hold ~:d entries" *most-entries*)))
                   (let ((read (multiple-value-list (read-section-anew pdf at))))
                     (when (first read)
                       (incf (getf (pdf-cache pdf) :entries-read 0) (hash-table-count (first read))))
                     read)))))))

(defun read-section-anew (pdf at)
  "The cross-reference section of PDF at AT as READ-SECTION gives it, read from its octets."
  (let ((octets (pdf-octets pdf)))
    (flet ((try (place)
             (multiple-value-bind (entries trailer problems) (read-xref-table pdf place)
               (if entries
                   (let ((hidden (pdf-get trailer "/XRefStm")))
                     (when (integerp hidden)
                       (let ((object (object-at-offset pdf hidden)))
                         (multiple-value-bind (more more-problems) (and object (read-xref-stream pdf object))
                           (setf problems (append problems more-problems))
                           (when more
                             (maphash (lambda (number entry)
                                        (let ((old (gethash number entries)))
                                          (when (or (null old) (eq (entry-kind old) :free))
                                            (setf (gethash number entries) entry))))
                                      more)))))
                     (return-from read-section-anew (values entries trailer (if (integerp hidden) :hybrid :table) place problems)))
                   (let ((object (object-at-offset pdf place)))
                     (when (and object (name-p (pdf-get (pdf-indirect-get object :value) "/Type") "/XRef"))
                       (multiple-value-bind (entries problems) (read-xref-stream pdf object)
                         (return-from read-section-anew
                           (values (or entries (make-hash-table)) (pdf-indirect-get object :value) :stream place problems)))))))))
      (when (and (integerp at) (< -1 at (length octets)))
        (try at)
        ;; a section a little before or after where it is said to be: found, and said
        (loop for delta from 1 to 64
              do (dolist (place (list (- at delta) (+ at delta)))
                   (when (and (< -1 place (length octets))
                              (or (octets-at-p octets "xref" place)
                                  (and (digit-p (aref octets place)) (or (zerop place) (not (digit-p (aref octets (1- place))))))))
                     (try place))))))))

;;; Revisions

(defun linearisation (pdf)
  "The linearisation dictionary of PDF (a file prepared for the web), or NIL:
its first object, when that says /Linearized."
  (let ((first (find-if (lambda (o) (> (pdf-indirect-get o :at) (pdf-header pdf))) (pdf-found pdf))))
    (and first (pdf-get (pdf-indirect-get first :value) "/Linearized") (pdf-indirect-get first :value))))

(defun web-parts-disagree (pdf first-entries first-at rest-entries end named)
  "Why the two parts of a save of PDF prepared for the web (linearised)
disagree, each in words, or NIL when they agree as their makers write them:
the linearisation dictionary's /L the length of the file to the end of that
save, END; no object number in use in both the first page's section
\(FIRST-ENTRIES, at FIRST-AT) and the rest's (REST-ENTRIES); and the
startxref before the save's last %%EOF naming the first page's section
\(NAMED). Parts that disagree can show readers another document than the
two read together."
  (let ((length (pdf-get (linearisation pdf) "/L")) (both '()))
    (maphash (lambda (number entry)
               (let ((other (gethash number rest-entries)))
                 (when (and (plusp number) (not (eq (entry-kind entry) :free)) other (not (eq (entry-kind other) :free)))
                   (push number both))))
             first-entries)
    (setf both (sort both #'<))
    (remove nil (list (unless (eql length end)
                        (format nil "its linearisation dictionary's /L says ~:[nothing that can be read~;~:*~:d octets~], but the save ends at ~:d"
                                (and (integerp length) length) end))
                      (when both
                        (format nil "object~p ~{~d~^, ~} ~:*~:*~[~;is~:;are~]~* in use in the sections of both"
                                (length both) (subseq both 0 (min 20 (length both)))))
                      (unless (eql named first-at)
                        (format nil "the startxref before its last %%EOF names ~:[nothing that can be read~;~:*~a~], not the first page's section at ~a"
                                (and named (hex-offset named)) (hex-offset first-at)))))))

(defun web-disagreement-words (reasons)
  "What is not as it should be in a save prepared for the web whose parts
disagree for REASONS (WEB-PARTS-DISAGREE), in words."
  (format nil "the two parts of a save prepared for the web disagree: ~{~a~^; ~}" reasons))

(defun section-revision (pdf index start eof end &key first-page after-first-page)
  "The REVISION of PDF ending with the %%EOF at EOF (its line ending at END),
from START, read by the cross-reference section its startxref names.
FIRST-PAGE, true for the first save of a file prepared for the web
\(linearised): the section written for its first page, whose startxref
readers pass over and its maker writes as 0, its trailer's /Prev naming the
section of the rest of the file. AFTER-FIRST-PAGE, for the save after that
one, is its REVISION: this save's section is the one its /Prev names, and
this save's startxref may name either; when it names the first page's,
that section's objects come first, as readers take them. A file prepared
for the web written as one save, its first page's section naming the rest's
by /Prev within the save, is read through both, the first page's objects
first, as readers do; it is LINEARISED, and when its two parts disagree
\(WEB-PARTS-DISAGREE) that is said."
  (multiple-value-bind (named startxref-at) (pdf-startxref (pdf-octets pdf) :start start :end eof)
    (let* ((octets (pdf-octets pdf))
           (revision (make-revision :index index :start start :end end :eof eof))
           (rest-of-file (and after-first-page
                              (let ((prev (revision-prev after-first-page)))
                                (and prev (<= start prev) (< prev eof)
                                     (member named (list prev (revision-xref-at after-first-page)))
                                     prev))))
           (sought (or rest-of-file named)))
      (multiple-value-bind (entries trailer kind place problems) (and sought (read-section pdf sought))
        (when (and entries sought (/= place sought))
          (push (if rest-of-file
                    (format nil "the first page's trailer names ~a by /Prev, but this save's cross-reference section is at ~a" (hex-offset sought) (hex-offset place))
                    (format nil "its startxref names ~a, but its cross-reference section is at ~a" (hex-offset named) (hex-offset place)))
                problems))
        (unless entries
          ;; none where it is said to be: the last one written in the save, else none at all
          (let ((table (loop for at = (octets-search (load-time-value (string-octets "xref")) octets :start start :end eof :from-end t)
                               then (octets-search (load-time-value (string-octets "xref")) octets :start start :end at :from-end t)
                             while (and at (>= at 5) (octets-at-p octets "start" (- at 5)))
                             finally (return at))))
            (when table
              (multiple-value-setq (entries trailer kind place problems) (read-section pdf table))))
          (unless entries
            (let ((stream (find-if (lambda (o) (and (<= start (pdf-indirect-get o :at) eof) (name-p (pdf-get (pdf-indirect-get o :value) "/Type") "/XRef")))
                                   (pdf-found pdf) :from-end t)))
              (when stream
                (multiple-value-setq (entries trailer kind place problems) (read-section pdf (pdf-indirect-get stream :at))))))
          (unless (and first-page entries (eql named 0)
                       (let ((prev (pdf-get trailer "/Prev"))) (and (integerp prev) (>= prev end))))
            (push (cond (entries (format nil "~:[it has no startxref~;~:*its startxref names ~a, where no cross-reference section is~]; the one at ~a was read"
                                         (and named (hex-offset named)) (hex-offset place)))
                        (t (format nil "~:[it has no startxref~;~:*its startxref names ~a, where no cross-reference section is~], and no section was found: its objects were found by scanning"
                                   (and named (hex-offset named)))))
                  problems)))
        ;; the rest of a file prepared for the web whose last startxref names the first page's section: that
        ;; section's objects first, as readers take them
        (when (and rest-of-file entries (eql named (revision-xref-at after-first-page)))
          (let ((both (make-hash-table :size (hash-table-count entries))))
            (maphash (lambda (number entry) (setf (gethash number both) entry)) entries)
            (maphash (lambda (number entry) (setf (gethash number both) entry)) (revision-entries after-first-page))
            (setf entries both)))
        ;; a file prepared for the web written as one save: the first page's section and the rest's it names
        (let ((prev (and entries (not after-first-page) (zerop index) (linearisation pdf) (pdf-get trailer "/Prev"))))
          (when (and (integerp prev) (< place prev) (< prev eof))
            (let ((rest (nth-value 0 (read-section pdf prev))))
              (when rest
                (let ((reasons (web-parts-disagree pdf entries place rest end named))
                      (both (make-hash-table :size (+ (hash-table-count entries) (hash-table-count rest)))))
                  (setf (getf (pdf-cache pdf) :first-page-entries) entries)
                  (maphash (lambda (number entry) (setf (gethash number both) entry)) rest)
                  (maphash (lambda (number entry) (setf (gethash number both) entry)) entries)
                  (setf entries both
                        (revision-linearised revision) t)
                  (when reasons (push (web-disagreement-words reasons) problems)))))))
        (unless trailer
          (let ((at (octets-search (load-time-value (string-octets "trailer")) octets :start start :end eof :from-end t)))
            (when at (setf trailer (let ((d (pdf-object-at octets (+ at 7)))) (and (pdf-dictionary-p d) d))))))
        (setf (revision-xref-at revision) place
              (revision-startxref-at revision) startxref-at
              (revision-startxref revision) named
              (revision-trailer-at revision) (case kind
                                               (:stream place)
                                               ((:table :hybrid) (octets-search (load-time-value (string-octets "trailer")) octets :start place :end eof))
                                               (t (octets-search (load-time-value (string-octets "trailer")) octets :start start :end eof :from-end t)))
              (revision-xref-kind revision) (or kind :none)
              (revision-trailer revision) trailer
              (revision-prev revision) (let ((p (pdf-get trailer "/Prev"))) (and (integerp p) p))
              (revision-problems revision) problems)
        (if entries
            (setf (revision-entries revision) entries)
            ;; no section: each object written in the save, the last of each number
            (loop for object across (pdf-found pdf)
                  when (and (<= start (pdf-indirect-get object :at)) (< (pdf-indirect-get object :at) end))
                    do (setf (gethash (pdf-indirect-get object :number) (revision-entries revision))
                             (make-entry :used (pdf-indirect-get object :at) (pdf-indirect-get object :generation)))))
        revision))))

(defun merge-revisions (first second)
  "FIRST and SECOND, the two sections of a file prepared for the web, as one
revision: the octets of both, the entries of both (FIRST's first), FIRST's
trailer with what SECOND's adds; FIRST's entries copied first, as a section's
entries may be shared with another save (READ-SECTION)."
  (let ((entries (make-hash-table :size (hash-table-count (revision-entries first)))))
    (maphash (lambda (number entry) (setf (gethash number entries) entry)) (revision-entries first))
    (setf (revision-entries first) entries))
  (maphash (lambda (number entry)
             (unless (gethash number (revision-entries first))
               (setf (gethash number (revision-entries first)) entry)))
           (revision-entries second))
  (setf (revision-end first) (revision-end second)
        (revision-eof first) (revision-eof second)
        (revision-linearised first) t
        (revision-trailer first) (append (revision-trailer first)
                                         (remove-if (lambda (pair) (assoc (car pair) (revision-trailer first) :test #'string=))
                                                    (revision-trailer second)))
        (revision-problems first) (append (revision-problems first) (revision-problems second)))
  first)

(defun objects-alike-p (pdf a b)
  "True when the objects A and B (each a PDF-INDIRECT) are written alike: their
octets in the file the same, or, both in object streams, their values."
  (let ((packed-a (pdf-indirect-get a :packed-in)) (packed-b (pdf-indirect-get b :packed-in)))
    (cond ((and packed-a packed-b)
           (string= (princ-to-string (pdf-indirect-get a :value)) (princ-to-string (pdf-indirect-get b :value))))
          ((or packed-a packed-b) nil)
          (t (let* ((octets (pdf-octets pdf)) (n (length octets))
                    (a0 (pdf-indirect-get a :at)) (a1 (min n (pdf-indirect-get a :end)))
                    (b0 (pdf-indirect-get b :at)) (b1 (min n (pdf-indirect-get b :end))))
               (and (= (- a1 a0) (- b1 b0))
                    (not (mismatch octets octets :start1 a0 :end1 a1 :start2 b0 :end2 b1))))))))

(defun note-changes (pdf revision)
  "Work out what REVISION adds, changes, writes again unchanged and frees,
from the revision before it; the original adds all it holds."
  (let ((before (and (plusp (revision-index revision)) (aref (pdf-revisions pdf) (1- (revision-index revision)))))
        (added '()) (changed '()) (same '()) (freed '()))
    (maphash (lambda (number entry)
               (unless (zerop number)
                 (let ((old (and before (lookup pdf before number))))
                   (if (eq (entry-kind entry) :free)
                       (when (and old (not (eq (entry-kind old) :free))) (push number freed))
                       (if (or (null old) (eq (entry-kind old) :free))
                           (push number added)
                           (let ((new-object (revision-object pdf revision number))
                                 (old-object (revision-object pdf before number)))
                             (if (and new-object old-object (objects-alike-p pdf new-object old-object))
                                 (push number same)
                                 (push number changed))))))))
             (revision-entries revision))
    (setf (revision-added revision) (sort added #'<) (revision-changed revision) (sort changed #'<)
          (revision-same revision) (sort same #'<) (revision-freed revision) (sort freed #'<))))

(defun read-pdf (octets &key path name (progress (constantly nil)))
  "PDF's structure read from OCTETS, the whole file (from PATH, called NAME):
its objects found by scanning, its saves (revisions) from the first, each with
its cross-reference section and what it changes. PROGRESS is called with a
number from 0 to 1 as it goes."
  (let ((pdf (make-pdf :octets octets :path path :name name)))
    (let ((header (pdf-header-at octets)))
      (setf (pdf-header pdf) (or header 0)
            (pdf-version pdf) (and header (let ((end (position-if (lambda (c) (or (pdf-white-p c) (= c 37))) octets :start (+ header 5))))
                                            (octets-latin-1 octets :start (+ header 5) :end (min (or end (+ header 9)) (+ header 9) (length octets))))))
      (unless header (note pdf "It does not begin with %PDF- in its first 1,024 octets: it may not be a PDF at all")))
    (let ((*pdf-most-items* *palimpsest-most-objects*))
      (scan-body pdf (lambda (at) (funcall progress (* 0.6 (/ at (max 1 (length octets)))))))
      (let* ((ends (multiple-value-bind (lines doubtful) (pdf-eof-lines octets (pdf-found pdf))
                     (setf (getf (pdf-cache pdf) :eofs-in-unended-streams) doubtful)
                     lines))
             (revisions '()) (start 0))
        (when (> (length ends) *palimpsest-most-revisions*)
          (note pdf "It was saved ~:d times; only the first ~:d saves were taken apart (*palimpsest-most-revisions*)"
                (length ends) *palimpsest-most-revisions*)
          (setf ends (subseq ends 0 *palimpsest-most-revisions*)))
        (loop with web = (linearisation pdf)
              for (eof . end) in ends for k from 0
              do (job-check-stop)
                 (push (section-revision pdf k start eof end :first-page (and web (= k 0))
                                                             :after-first-page (and web (= k 1) (first revisions)))
                       revisions)
                 (setf start end)
                 (funcall progress (+ 0.6 (* 0.2 (/ (1+ k) (length ends))))))
        (setf revisions (nreverse revisions))
        (when (null revisions)
          (note pdf "It has no %%EOF line: it was cut short, or is no PDF; its objects were read as one save")
          (setf revisions (list (section-revision pdf 0 0 (length octets) (length octets)))))
        ;; a file prepared for the web: its first two sections are one save, when its two parts agree
        (when (and (linearisation pdf) (rest revisions)
                   (eql (revision-prev (first revisions)) (revision-xref-at (second revisions))))
          (let* ((first (first revisions)) (second (second revisions))
                 (reasons (web-parts-disagree pdf (revision-entries first) (revision-xref-at first)
                                              (nth-value 0 (read-section pdf (revision-prev first)))
                                              (revision-end second) (revision-startxref second))))
            (if reasons
                (push (web-disagreement-words reasons) (revision-problems first))
                (progn
                  (setf (getf (pdf-cache pdf) :first-page-entries) (revision-entries first))
                  (setf revisions (cons (merge-revisions first second) (cddr revisions)))))))
        (setf (pdf-revisions pdf) (coerce revisions 'vector))
        (loop for revision across (pdf-revisions pdf) for k from 0
              do (setf (revision-index revision) k
                       (revision-latest revision) (= k (1- (length (pdf-revisions pdf)))))))
      (setf (pdf-encrypted pdf) (some (lambda (r) (pdf-get (revision-trailer r) "/Encrypt")) (pdf-revisions pdf)))
      (loop for revision across (pdf-revisions pdf)
            do (job-check-stop)
               (note-changes pdf revision)))
    (funcall progress 0.85)
    pdf))

(defun last-revision (pdf)
  "The latest save of PDF, as it is shown."
  (let ((revisions (pdf-revisions pdf))) (aref revisions (1- (length revisions)))))

(defun revision-of-offset (pdf at)
  "The revision of PDF whose octets hold the offset AT, or NIL."
  (find-if (lambda (r) (and (<= (revision-start r) at) (< at (revision-end r)))) (pdf-revisions pdf)))

(defun written-in (pdf revision number)
  "The revision in which the object NUMBER, as REVISION has it, was written:
the one whose octets hold it, or its object stream; NIL when there is none."
  (let ((object (revision-object pdf revision number)))
    (when object
      (if (pdf-indirect-get object :packed-in)
          (let ((stream (revision-object pdf revision (pdf-indirect-get object :packed-in))))
            (and stream (revision-of-offset pdf (pdf-indirect-get stream :at))))
          (revision-of-offset pdf (pdf-indirect-get object :at))))))

(defun live-numbers (pdf revision)
  "The numbers of the objects REVISION holds, not free, in order."
  (let ((seen (make-hash-table)) (live '()))
    (loop for r from (revision-index revision) downto 0
          do (maphash (lambda (number entry)
                        (unless (or (zerop number) (gethash number seen))
                          (setf (gethash number seen) t)
                          (unless (eq (entry-kind entry) :free) (push number live))))
                      (revision-entries (aref (pdf-revisions pdf) r))))
    (sort live #'<)))

(defun references-in (value)
  "The object numbers VALUE refers to, at any depth."
  (let ((found '()))
    (labels ((walk (v depth)
               (cond ((> depth 40))
                     ((pdf-ref-p v) (push (second v) found))
                     ((consp v)
                      ;; along the list, a dictionary's (KEY . VALUE) whose value is a reference among its tails
                      (loop for rest = v then (cdr rest)
                            do (cond ((pdf-ref-p rest) (push (second rest) found) (return))
                                     ((consp rest) (walk (car rest) (1+ depth)))
                                     (t (return))))))))
      (walk value 0))
    found))

(defun reachable (pdf revision &key (from nil from-p) skip)
  "The numbers of the objects REVISION's trailer leads to (or the values
FROM leads to), following every reference but those under the keys SKIP
names, as a hash table."
  (let ((seen (make-hash-table)) (queue (references-in (if from-p from (remove "/Prev" (revision-trailer revision) :key #'car :test #'string=)))))
    (loop while queue
          do (let ((number (pop queue)))
               (unless (gethash number seen)
                 (setf (gethash number seen) t)
                 (let ((object (revision-object pdf revision number)))
                   (when object
                     (let ((value (pdf-indirect-get object :value)))
                       (dolist (n (references-in (if (and skip (pdf-dictionary-p value))
                                                     (remove-if (lambda (pair) (member (car pair) skip :test #'string=)) value)
                                                     value)))
                         (unless (gethash n seen) (push n queue)))))))))
    seen))
