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
as scanning finds them (a vector of PDF-INDIRECT, in order); STREAMS, where each
stream found lies, for telling an %%EOF in one from a real one; REVISIONS,
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
places of their streams (PDF-FOUND, PDF-STREAMS); PROGRESS is called with
the offset reached now and then."
  (let* ((octets (pdf-octets pdf)) (n (length octets)) (found '()) (count 0) (objects 0) (i 0)
         (mark (load-time-value (string-octets "obj"))))
    (loop
      (let ((p (search mark octets :start2 i)))
        (unless p (return))
        (when (zerop (mod (incf count) 4096)) (funcall progress p) (job-check-stop))
        (let ((at (pdf-object-header-before octets p))
              (next (+ p 3)))
          (when (and at (or (>= (+ p 3) n) (pdf-delimiter-p (aref octets (+ p 3)))))
            (let ((object (ignore-errors (pdf-indirect-object-at octets at))))
              (when object
                (push object found)
                (setf (gethash at (pdf-objects pdf)) object)
                (setf next (max next (pdf-indirect-end object)))
                (when (> (incf objects) *palimpsest-most-objects*)
                  (note pdf "It holds more than ~:d objects (*palimpsest-most-objects*): the rest were not read" *palimpsest-most-objects*)
                  (return)))))
          (setf i next))))
    (setf (pdf-found pdf) (coerce (nreverse found) 'vector))))

(defun inside-stream-p (pdf at)
  "True when AT, an offset of PDF's file, falls within the octets of a stream found by scanning."
  (let* ((found (pdf-found pdf)) (low 0) (high (length found)))
    (loop while (< low high)
          do (let ((middle (floor (+ low high) 2)))
               (if (<= (pdf-indirect-at (aref found middle)) at) (setf low (1+ middle)) (setf high middle))))
    (let ((object (and (plusp low) (aref found (1- low)))))
      (and object (pdf-indirect-stream-start object) (<= (pdf-indirect-stream-start object) at) (< at (pdf-indirect-stream-end object))))))

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
  (when (pdf-indirect-stream-start object)
    (let ((kept (gethash object (pdf-decoded pdf))))
      (if kept
          (values (car kept) (cdr kept))
          (let ((raw (raw-stream pdf object))
                (dict (pdf-indirect-value object)))
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
  (let ((start (pdf-indirect-stream-start object)))
    (and start (subseq (pdf-octets pdf) start (if most
                                                    (min (pdf-indirect-stream-end object) (+ start most))
                                                    (pdf-indirect-stream-end object))))))

;;; Objects

(defun object-at-offset (pdf at)
  "The object PDF's file holds at AT (a PDF-INDIRECT), read once; NIL when none begins there."
  (when (and (integerp at) (<= 0 at) (< at (length (pdf-octets pdf))))
    (multiple-value-bind (kept there) (gethash at (pdf-objects pdf))
      (if there
          kept
          (setf (gethash at (pdf-objects pdf)) (ignore-errors (pdf-indirect-object-at (pdf-octets pdf) at)))))))

(defun packed-objects (pdf stream)
  "What the object stream STREAM (a PDF-INDIRECT) holds, as two values: a vector of
\(NUMBER . OFFSET), each object's number and where it is in the decoded
octets, and those octets; NIL when it cannot be read."
  (let ((kept (gethash stream (pdf-packed pdf))))
    (if kept
        (values (car kept) (cdr kept))
        (let* ((data (stream-data pdf stream))
               (pairs (and data (pdf-object-stream-offsets (pdf-indirect-value stream) data))))
          (when pairs
            (setf (gethash stream (pdf-packed pdf)) (cons pairs data))
            (values pairs data))))))

(defun packed-object (pdf stream index number)
  "The object NUMBER the object stream STREAM (a PDF-INDIRECT) holds, at INDEX
among them or, when another is there, wherever it is; a PDF-INDIRECT, or NIL."
  (multiple-value-bind (pairs data) (packed-objects pdf stream)
    (and pairs (pdf-packed-object data pairs number :index index :in (pdf-indirect-number stream)))))

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
                     (loop for object across (pdf-found pdf) do (push object (gethash (pdf-indirect-number object) table)))
                     (setf (getf (pdf-cache pdf) :by-number) table)))))
    ;; each number's objects, the last written first
    (find-if (lambda (object) (< (pdf-indirect-at object) limit)) (gethash number index))))

(defun revision-object (pdf revision number)
  "The object NUMBER as REVISION has it (a PDF-INDIRECT): where its cross-reference
says, else, when that holds no such object, the last of that number written
before REVISION ends; NIL when it has none, or it is free."
  (when (and (integerp number) (< *depth* 64))
    (let ((*depth* (1+ *depth*)))
      (multiple-value-bind (entry) (lookup pdf revision number)
        (case (and entry (entry-kind entry))
          (:used (let ((object (object-at-offset pdf (entry-place entry))))
                   (if (and object (eql (pdf-indirect-number object) number))
                       object
                       (found-object pdf number (revision-end revision)))))
          (:packed (let ((stream (revision-object pdf revision (entry-place entry))))
                     (and stream (null (pdf-indirect-packed-in stream))
                          (packed-object pdf stream (entry-index entry) number))))
          (:free nil)
          (t (and (eq (revision-xref-kind revision) :none) (found-object pdf number (revision-end revision)))))))))

(defun deref (pdf revision value)
  "VALUE with a reference followed, in REVISION, to the value of what it names."
  (if (pdf-ref-p value)
      (let ((object (revision-object pdf revision (second value))))
        (and object (pdf-indirect-value object)))
      value))

(defun dref (pdf revision dict key)
  "The value of KEY in DICT, a reference followed in REVISION."
  (deref pdf revision (pdf-get dict key)))

;;; Cross-reference sections

(defun read-xref-table (pdf at)
  "The cross-reference table of PDF at AT (where xref is), as three values:
its entries (a hash table of number to ENTRY), its trailer, and what does not
make sense in it, in words; NIL when there is no table at AT."
  (let* ((octets (pdf-octets pdf)) (n (length octets)) (entries (make-hash-table)) (problems '()) (total 0)
         (i (pdf-skip-space octets at)))
    (when (octets-at-p octets "xref" i)
      (setf i (+ i 4))
      (block sections
        (loop
          (setf i (pdf-skip-space octets i))
          (when (or (>= i n) (octets-at-p octets "trailer" i)) (return))
          (multiple-value-bind (start after) (pdf-integer-at octets i)
            (multiple-value-bind (count after) (and start (pdf-integer-at octets after))
              (unless count (push (format nil "its cross-reference table at ~a is cut short" (hex-offset at)) problems) (return))
              (setf i after)
              (dotimes (k count)
                (multiple-value-bind (offset after) (pdf-integer-at octets i)
                  (multiple-value-bind (generation after) (and offset (pdf-integer-at octets after))
                    (let ((kind (and generation (pdf-skip-space octets after))))
                      (unless (and kind (< kind n) (member (aref octets kind) '(102 110)))
                        (push (format nil "an entry of its cross-reference table at ~a cannot be read" (hex-offset i)) problems)
                        (return-from sections))
                      (when (> (incf total) *palimpsest-most-objects*)
                        (push "its cross-reference table lists more entries than are read (*palimpsest-most-objects*)" problems)
                        (return-from sections))
                      (let ((number (+ start k)))
                        (if (gethash number entries)
                            (push (format nil "object ~d is listed twice in its cross-reference table" number) problems)
                            (setf (gethash number entries)
                                  (make-entry (if (= (aref octets kind) 110) :used :free) offset generation))))
                      (setf i (1+ kind))))))))))
      (let ((trailer (and (octets-at-p octets "trailer" i) (pdf-object-at octets (+ i 7)))))
        (values entries (and (pdf-dictionary-p trailer) trailer) (nreverse problems))))))

(defun read-xref-stream (pdf object)
  "The entries of OBJECT, a cross-reference stream (a PDF-INDIRECT), as a hash table
of number to ENTRY, and what does not make sense in it, in words, as two
values; NIL when it is no such stream or cannot be decoded."
  (let ((dict (pdf-indirect-value object)))
    (when (name-p (pdf-get dict "/Type") "/XRef")
      (multiple-value-bind (data why) (stream-data pdf object)
        (if (or (null data) why)
            (values nil (list (format nil "its cross-reference stream, object ~d, ~a" (pdf-indirect-number object) (or why "is empty"))))
            (let* ((widths (pdf-get dict "/W"))
                   (size (pdf-get dict "/Size"))
                   (index (or (pdf-get dict "/Index") (and (integerp size) (list 0 size))))
                   (entries (make-hash-table)) (problems '()) (at 0) (total 0))
              (if (not (and (listp widths) (= (length widths) 3) (every (lambda (w) (and (integerp w) (<= 0 w 8))) widths)
                            (plusp (reduce #'+ widths))))
                  (values nil (list (format nil "its cross-reference stream, object ~d, gives no field widths it can be read by" (pdf-indirect-number object))))
                  (let ((row (reduce #'+ widths)))
                    (flet ((field (k standard)
                             (let ((w (nth k widths)))
                               (if (zerop w)
                                   standard
                                   (prog1 (loop with v = 0 for j from at below (+ at w) do (setf v (+ (* v 256) (aref data j))) finally (return v))
                                     (incf at w))))))
                      (loop named sections
                            for (start count) on index by #'cddr
                            while (and (integerp start) (integerp count))
                            do (dotimes (k count)
                                 (when (> (+ at row) (length data))
                                   (push (format nil "its cross-reference stream, object ~d, is shorter than its index says" (pdf-indirect-number object)) problems)
                                   (return-from sections))
                                 (when (> (incf total) *palimpsest-most-objects*)
                                   (push "its cross-reference stream lists more entries than are read (*palimpsest-most-objects*)" problems)
                                   (return-from sections))
                                 (let* ((type (field 0 1)) (second (field 1 0)) (third (field 2 0)) (number (+ start k)))
                                   (unless (gethash number entries)
                                     (setf (gethash number entries)
                                           (case type
                                             (0 (make-entry :free second third))
                                             (1 (make-entry :used second third))
                                             (2 (make-entry :packed second 0 third))
                                             (t (make-entry :free 0 0)))))))))
                    (values entries (nreverse problems))))))))))

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
                     (when (and object (name-p (pdf-get (pdf-indirect-value object) "/Type") "/XRef"))
                       (multiple-value-bind (entries problems) (read-xref-stream pdf object)
                         (return-from read-section-anew
                           (values (or entries (make-hash-table)) (pdf-indirect-value object) :stream place problems)))))))))
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

(defun eof-ends (pdf)
  "The %%EOF markers of PDF's file that end a save (none within a stream), each
\(EOF . END): where it is, and where its line ends."
  (let ((octets (pdf-octets pdf)) (ends '()))
    (loop for at = (search (load-time-value (string-octets "%%EOF")) octets) then (search (load-time-value (string-octets "%%EOF")) octets :start2 (+ at 5))
          while at
          unless (inside-stream-p pdf at)
            do (push (cons at (pdf-line-end-after octets (+ at 5))) ends))
    (nreverse ends)))

(defun startxref-before (octets from to)
  "The offset the last startxref between FROM and TO in OCTETS names, or NIL."
  (let ((at (search (load-time-value (string-octets "startxref")) octets :start2 from :end2 to :from-end t)))
    (and at (pdf-integer-at octets (+ at 9) to))))

(defun linearisation (pdf)
  "The linearisation dictionary of PDF (a file prepared for the web), or NIL:
its first object, when that says /Linearized."
  (let ((first (find-if (lambda (o) (> (pdf-indirect-at o) (pdf-header pdf))) (pdf-found pdf))))
    (and first (pdf-get (pdf-indirect-value first) "/Linearized") (pdf-indirect-value first))))

(defun section-revision (pdf index start eof end)
  "The REVISION of PDF ending with the %%EOF at EOF (its line ending at END),
from START, read by the cross-reference section its startxref names."
  (let* ((octets (pdf-octets pdf))
         (revision (make-revision :index index :start start :end end :eof eof))
         (startxref-at (search (load-time-value (string-octets "startxref")) octets :start2 start :end2 eof :from-end t))
         (named (startxref-before octets start eof)))
    (multiple-value-bind (entries trailer kind place problems) (and named (read-section pdf named))
      (when (and entries named (/= place named))
        (push (format nil "its startxref names ~a, but its cross-reference section is at ~a" (hex-offset named) (hex-offset place)) problems))
      (unless entries
        ;; none where it is said to be: the last one written in the save, else none at all
        (let ((table (search (load-time-value (string-octets "xref")) octets :start2 start :end2 eof :from-end t)))
          (when (and table (not (and (> table 0) (= (aref octets (1- table)) 116)))) ; not startxref
            (multiple-value-setq (entries trailer kind place problems) (read-section pdf table))))
        (unless entries
          (let ((stream (find-if (lambda (o) (and (<= start (pdf-indirect-at o) eof) (name-p (pdf-get (pdf-indirect-value o) "/Type") "/XRef")))
                                 (pdf-found pdf) :from-end t)))
            (when stream
              (multiple-value-setq (entries trailer kind place problems) (read-section pdf (pdf-indirect-at stream))))))
        (push (cond (entries (format nil "~:[it has no startxref~;~:*its startxref names ~a, where no cross-reference section is~]; the one at ~a was read"
                                     (and named (hex-offset named)) (hex-offset place)))
                    (t (format nil "~:[it has no startxref~;~:*its startxref names ~a, where no cross-reference section is~], and no section was found: its objects were found by scanning"
                               (and named (hex-offset named)))))
              problems))
      (unless trailer
        (let ((at (search (load-time-value (string-octets "trailer")) octets :start2 start :end2 eof :from-end t)))
          (when at (setf trailer (let ((d (pdf-object-at octets (+ at 7)))) (and (pdf-dictionary-p d) d))))))
      (setf (revision-xref-at revision) place
            (revision-startxref-at revision) startxref-at
            (revision-startxref revision) named
            (revision-trailer-at revision) (case kind
                                             (:stream place)
                                             ((:table :hybrid) (search (load-time-value (string-octets "trailer")) octets :start2 place :end2 eof))
                                             (t (search (load-time-value (string-octets "trailer")) octets :start2 start :end2 eof :from-end t)))
            (revision-xref-kind revision) (or kind :none)
            (revision-trailer revision) trailer
            (revision-prev revision) (let ((p (pdf-get trailer "/Prev"))) (and (integerp p) p))
            (revision-problems revision) problems)
      (if entries
          (setf (revision-entries revision) entries)
          ;; no section: each object written in the save, the last of each number
          (loop for object across (pdf-found pdf)
                when (and (<= start (pdf-indirect-at object)) (< (pdf-indirect-at object) end))
                  do (setf (gethash (pdf-indirect-number object) (revision-entries revision))
                           (make-entry :used (pdf-indirect-at object) (pdf-indirect-generation object)))))
      revision)))

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
  (let ((packed-a (pdf-indirect-packed-in a)) (packed-b (pdf-indirect-packed-in b)))
    (cond ((and packed-a packed-b)
           (string= (princ-to-string (pdf-indirect-value a)) (princ-to-string (pdf-indirect-value b))))
          ((or packed-a packed-b) nil)
          (t (let* ((octets (pdf-octets pdf)) (n (length octets))
                    (a0 (pdf-indirect-at a)) (a1 (min n (pdf-indirect-end a)))
                    (b0 (pdf-indirect-at b)) (b1 (min n (pdf-indirect-end b))))
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
      (let* ((ends (eof-ends pdf))
             (revisions '()) (start 0))
        (when (> (length ends) *palimpsest-most-revisions*)
          (note pdf "It was saved ~:d times; only the first ~:d saves were taken apart (*palimpsest-most-revisions*)"
                (length ends) *palimpsest-most-revisions*)
          (setf ends (subseq ends 0 *palimpsest-most-revisions*)))
        (loop for (eof . end) in ends for k from 0
              do (job-check-stop)
                 (push (section-revision pdf k start eof end) revisions)
                 (setf start end)
                 (funcall progress (+ 0.6 (* 0.2 (/ (1+ k) (length ends))))))
        (setf revisions (nreverse revisions))
        (when (null revisions)
          (note pdf "It has no %%EOF line: it was cut short, or is no PDF; its objects were read as one save")
          (setf revisions (list (section-revision pdf 0 0 (length octets) (length octets)))))
        ;; a file prepared for the web: its first two sections are one save
        (when (and (linearisation pdf) (rest revisions)
                   (eql (revision-prev (first revisions)) (revision-xref-at (second revisions))))
          (setf revisions (cons (merge-revisions (first revisions) (second revisions)) (cddr revisions))))
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
      (if (pdf-indirect-packed-in object)
          (let ((stream (revision-object pdf revision (pdf-indirect-packed-in object))))
            (and stream (revision-of-offset pdf (pdf-indirect-at stream))))
          (revision-of-offset pdf (pdf-indirect-at object))))))

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
                     (let ((value (pdf-indirect-value object)))
                       (dolist (n (references-in (if (and skip (pdf-dictionary-p value))
                                                     (remove-if (lambda (pair) (member (car pair) skip :test #'string=)) value)
                                                     value)))
                         (unless (gethash n seen) (push n queue)))))))))
    seen))
