;;;; credentials.lisp --- What a file says of how it was made: content credentials and image generators' marks.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A picture or a PDF may say it was made by an image generator: in its
;;;; content credentials (C2PA, a signed record of who made it and how, kept
;;;; in JUMBF boxes of CBOR), in the digital source type of its XMP, in the
;;;; text a generator writes into a PNG, or in the name of the program. All
;;;; of it is read as data, never run, each length and count checked against
;;;; what is left and against a limit; signatures are named as written, not
;;;; verified. Most generated pictures say none of this, so a picture that
;;;; says nothing is not thereby taken from life.

(in-package #:palimpsest)

(define-condition unreadable-credentials (error)
  ((why :initarg :why :reader unreadable-credentials-why))
  (:report (lambda (condition stream) (write-string (unreadable-credentials-why condition) stream)))
  (:documentation "Content credentials that cannot be read as far as they say: WHY, in words."))

(defun credentials-refused (format &rest arguments)
  "Give up reading content credentials, saying why by FORMAT and ARGUMENTS."
  (error 'unreadable-credentials :why (apply #'format nil format arguments)))

;;; CBOR, the compact data of a C2PA claim

(defparameter *cbor-depth-most* 24
  "The deepest CBOR items are read nested in one another: a claim or an
assertion nests a few levels, a file made to run the reader out of stack
many thousands.")

(defparameter *cbor-items-most* 200000
  "The most CBOR items read from one file's content credentials, all their
boxes together; past them the rest is not read.")

(defparameter *cbor-kept-most* (* 4 1024 1024)
  "The most octets of text and byte strings kept from one file's content
credentials, all their boxes together. A text longer than *CBOR-TEXT-MOST*
is kept cut, and a byte string longer than *CBOR-BYTES-MOST* not kept.")

(defparameter *cbor-text-most* 4096
  "The most octets of one CBOR text kept, the rest left out.")

(defparameter *cbor-bytes-most* (* 256 1024)
  "The most octets of one CBOR byte string kept; a longer one (a picture
kept in an assertion) is read past, its length kept.")

(defun cbor-budget ()
  "A new budget for reading one file's content credentials: the CBOR items
and kept octets left (*CBOR-ITEMS-MOST*, *CBOR-KEPT-MOST*), as a list both
readers count down."
  (list *cbor-items-most* *cbor-kept-most*))

(defun cbor-read (octets &key (start 0) (end (length octets)) (budget (cbor-budget)))
  "The CBOR data item in OCTETS from START below END, as two values: the item
and where what follows it begins. A number is an integer; a text a string;
a byte string an octet vector, or (:LONG-BYTES LENGTH) past
*CBOR-BYTES-MOST*; an array a simple vector; a map (:MAP (KEY . VALUE) ...);
a tagged item (:TAG NUMBER ITEM); false, true, null and undefined
:FALSE, :TRUE, :NULL and :UNDEFINED; a floating-point number :FLOAT and any
other simple value :SIMPLE. Items of undetermined length are read to their
break. BUDGET, the items and kept octets left (CBOR-BUDGET), is counted
down, each piece of a string of undetermined length counted as an item.
Signals UNREADABLE-CREDENTIALS when an item runs past END, nests
deeper than *CBOR-DEPTH-MOST*, gives a count larger than the octets left
could hold, a map gives a key twice, or the budget runs out."
  (declare (type octets octets) (type fixnum start end))
  (let ((at start))
    (declare (type fixnum at))
    (labels ((octet ()
               (when (>= at end) (credentials-refused "a CBOR item runs past the end"))
               (prog1 (aref octets at) (incf at)))
             (argument (info)
               (cond ((< info 24) info)
                     ((<= info 27)
                      (let ((n (ash 1 (- info 24))))
                        (when (> (+ at n) end) (credentials-refused "a CBOR length runs past the end"))
                        (prog1 (octets-integer octets at n :big-endian t) (incf at n))))
                     ((= info 31) nil)
                     (t (credentials-refused "a CBOR item uses a reserved length (~d)" info))))
             (fits (count)
               ;; every item takes an octet at least
               (when (> count (- end at))
                 (credentials-refused "a CBOR count (~:d) is larger than the octets left" count)))
             (spend (octets)
               (when (minusp (decf (second budget) octets))
                 (credentials-refused "the content credentials keep more text than ~:d octets" *cbor-kept-most*)))
             (break-p ()
               (when (>= at end) (credentials-refused "a CBOR item of undetermined length has no end"))
               (when (= (aref octets at) #xFF) (incf at) t))
             (pieces (major n)
               ;; a string's parts as (START . END): one, or the definite pieces of one of undetermined length
               (if n
                   (progn (fits n) (prog1 (list (cons at (+ at n))) (incf at n)))
                   (loop until (break-p)
                         collect (let ((initial (octet)))
                                   (when (minusp (decf (first budget)))
                                     (credentials-refused "the content credentials hold more than ~:d CBOR items"
                                                          *cbor-items-most*))
                                   (unless (= (ash initial -5) major)
                                     (credentials-refused "a CBOR string of undetermined length holds another item"))
                                   (let ((n (argument (logand initial 31))))
                                     (unless n (credentials-refused "a CBOR string piece is itself undetermined"))
                                     (fits n)
                                     (prog1 (cons at (+ at n)) (incf at n)))))))
             (joined (ranges most)
               (let* ((total (loop for (s . e) in ranges sum (- e s)))
                      (out (make-array (min total most) :element-type '(unsigned-byte 8)))
                      (filled 0))
                 (loop for (s . e) in ranges
                       while (< filled (length out))
                       do (let ((take (min (- e s) (- (length out) filled))))
                            (replace out octets :start1 filled :start2 s :end2 (+ s take))
                            (incf filled take)))
                 (spend (length out))
                 (values out total)))
             (key-identity (key from to)
               ;; what tells two keys apart: a number by its value, a string by its whole contents and its kind,
               ;; anything else by its octets as written
               (let ((major (ash (aref octets from) -5)))
                 (case major
                   ((0 1) key)
                   ((2 3) (let ((contents (cbor-string-contents octets from)))
                            (spend (length contents))
                            (cons major contents)))
                   (t (spend (- to from))
                      (cons :written (map 'string #'code-char (subseq octets from to)))))))
             (item (depth)
               (when (> depth *cbor-depth-most*)
                 (credentials-refused "CBOR items nest deeper than ~d" *cbor-depth-most*))
               (when (minusp (decf (first budget)))
                 (credentials-refused "the content credentials hold more than ~:d CBOR items" *cbor-items-most*))
               (let* ((initial (octet)) (major (ash initial -5)) (info (logand initial 31))
                      (n (if (= major 7) info (argument info))))
                 (case major
                   (0 (or n (credentials-refused "a CBOR number has no value")))
                   (1 (if n (- -1 n) (credentials-refused "a CBOR number has no value")))
                   (2 (let ((ranges (pieces 2 n)))
                        (if (> (loop for (s . e) in ranges sum (- e s)) *cbor-bytes-most*)
                            (list :long-bytes (loop for (s . e) in ranges sum (- e s)))
                            (values (joined ranges *cbor-bytes-most*)))))
                   (3 (let ((text (joined (pieces 3 n) *cbor-text-most*)))
                        (octets-string text :replacement #\?)))
                   (4 (if n
                          (progn (fits n) (coerce (loop repeat n collect (item (1+ depth))) 'simple-vector))
                          (coerce (loop until (break-p) collect (item (1+ depth))) 'simple-vector)))
                   (5 (let ((keys (make-hash-table :test 'equal)))
                        (flet ((entry ()
                                 ;; C2PA's CBOR is deterministic: a key given twice could say two things at once
                                 (let* ((from at) (key (item (1+ depth))) (identity (key-identity key from at)))
                                   (when (gethash identity keys)
                                     (credentials-refused "a CBOR map gives the key ~a twice"
                                                          (if (stringp key)
                                                              (shorten-string key 60 :one-line t)
                                                              "of another kind")))
                                   (setf (gethash identity keys) t)
                                   (cons key (item (1+ depth))))))
                          (cons :map (if n
                                         (progn (fits (* 2 n)) (loop repeat n collect (entry)))
                                         (loop until (break-p) collect (entry)))))))
                   (6 (unless n (credentials-refused "a CBOR tag has no number"))
                    (list :tag n (item (1+ depth))))
                   (t (case info
                        (20 :false) (21 :true) (22 :null) (23 :undefined)
                        ((25 26 27) (let ((size (ash 1 (- info 24))))
                                      (when (> (+ at size) end) (credentials-refused "a CBOR number runs past the end"))
                                      (incf at size)
                                      :float))
                        (24 (octet) :simple)
                        (31 (credentials-refused "a CBOR break stands outside any item"))
                        ((28 29 30) (credentials-refused "a CBOR item uses a reserved length (~d)" info))
                        (t :simple)))))))
      (values (item 0) at))))

(defun cbor-string-contents (octets at)
  "The contents of the CBOR byte or text string at AT in OCTETS, already read
whole by CBOR-READ, every piece of one of undetermined length joined, as a
string of one character for each octet."
  (flet ((head (at)
           ;; the length a head at AT gives and where its contents begin
           (let ((info (logand (aref octets at) 31)))
             (if (< info 24)
                 (values info (1+ at))
                 (let ((n (ash 1 (- info 24))))
                   (values (octets-integer octets (1+ at) n :big-endian t) (+ at 1 n)))))))
    (with-output-to-string (out)
      (if (= (logand (aref octets at) 31) 31)
          (let ((piece (1+ at)))
            (loop until (= (aref octets piece) #xFF)
                  do (multiple-value-bind (length from) (head piece)
                       (loop for k from from below (+ from length) do (write-char (code-char (aref octets k)) out))
                       (setf piece (+ from length)))))
          (multiple-value-bind (length from) (head at)
            (loop for k from from below (+ from length) do (write-char (code-char (aref octets k)) out)))))))

(defun cbor-get (item key)
  "The value of KEY, a string or an integer, in ITEM when it is a CBOR map
as CBOR-READ gives one; NIL otherwise."
  (and (consp item) (eq (first item) :map)
       (cdr (assoc key (rest item) :test #'equal))))

(defun cbor-text (item)
  "ITEM when it is a non-empty string, on one line and shortened to 200
columns as a finding names it; else NIL."
  (and (stringp item) (plusp (length item)) (shorten-string item 200 :one-line t)))

;;; JUMBF boxes

(defparameter *jumbf-depth-most* 8
  "The deepest JUMBF boxes are read nested: C2PA nests four levels.")

(defparameter *jumbf-boxes-most* 10000
  "The most JUMBF boxes read from one file's content credentials.")

(defparameter *jumbf-type-suffix* '(#x00 #x11 #x00 #x10 #x80 #x00 #x00 #xAA #x00 #x38 #x9B #x71)
  "The last twelve octets of the type of a JUMBF box whose first four name
its kind in letters (c2pa, c2ma, cbor and so on), as ISO and C2PA write
them.")

(defstruct (jumbf (:constructor make-jumbf (kind label children)) (:copier nil) (:predicate nil))
  "A JUMBF superbox: KIND, the four letters its description box's type
names (\"c2pa\" for a manifest store, \"c2ma\" a manifest, \"c2cl\" a
claim, \"cbor\" CBOR content), or NIL for a type of another form; LABEL,
its label or NIL; CHILDREN, the boxes it holds after its description, each
a JUMBF or (TYPE START END), TYPE the box's four letters and START and END
where its contents lie."
  kind label children)

(defun jumbf-read (octets start end &key (budget (list *jumbf-boxes-most*)) (depth 0))
  "The boxes in OCTETS from START below END, each a JUMBF for a superbox
\(jumb) or (TYPE START END) for any other. BUDGET, a list holding how many
boxes may yet be read, is counted down. Signals UNREADABLE-CREDENTIALS when
a box runs past END or is shorter than its header, boxes nest deeper than
*JUMBF-DEPTH-MOST*, or more than the budget are found."
  (when (> depth *jumbf-depth-most*) (credentials-refused "JUMBF boxes nest deeper than ~d" *jumbf-depth-most*))
  (let ((at start) (out '()))
    (loop while (< at end)
          do (when (minusp (decf (first budget)))
               (credentials-refused "the content credentials hold more than ~:d boxes" *jumbf-boxes-most*))
             (when (> (+ at 8) end) (credentials-refused "a JUMBF box header runs past the end"))
             (let* ((size (octets-integer octets at 4 :big-endian t))
                    (type (octets-latin-1 octets :start (+ at 4) :end (+ at 8)))
                    (header (if (= size 1) 16 8))
                    (size (case size
                            (0 (- end at))
                            (1 (or (and (<= (+ at 16) end) (octets-integer octets (+ at 8) 8 :big-endian t))
                                   (credentials-refused "a JUMBF box's long size runs past the end")))
                            (t size)))
                    (box-end (+ at size)))
               (when (or (< size header) (> box-end end))
                 (credentials-refused "a JUMBF box (~a, ~:d octets) runs past the end or is shorter than its header"
                                      (escape-controls type) size))
               (push (if (string= type "jumb")
                         (jumbf-superbox octets (+ at header) box-end budget depth)
                         (list type (+ at header) box-end))
                     out)
               (setf at box-end)))
    (nreverse out)))

(defun jumbf-superbox (octets start end budget depth)
  "The JUMBF superbox whose contents lie in OCTETS from START below END, its
description box first (JUMBF-READ)."
  (let* ((boxes (jumbf-read octets start end :budget budget :depth (1+ depth)))
         (description (first boxes)))
    (unless (and (consp description) (string= (first description) "jumd")
                 (>= (- (third description) (second description)) 17))
      (credentials-refused "a JUMBF superbox has no description box"))
    (destructuring-bind (from to) (rest description)
      (let* ((kind (and (octets-at-p octets *jumbf-type-suffix* (+ from 4))
                        (octets-latin-1 octets :start from :end (+ from 4))))
             (toggles (aref octets (+ from 16)))
             (label (and (logtest toggles 2)
                         (let ((nul (position 0 octets :start (+ from 17) :end to)))
                           (octets-string octets :start (+ from 17) :end (min (or nul to) (+ from 17 *cbor-text-most*))
                                                 :replacement #\?)))))
        (make-jumbf kind label (rest boxes))))))

(defun jumbf-child (box kind)
  "The first superbox BOX holds whose kind is KIND, or NIL."
  (find-if (lambda (c) (and (jumbf-p c) (equal (jumbf-kind c) kind))) (jumbf-children box)))

(defun jumbf-p (item)
  "True when ITEM is a JUMBF superbox as JUMBF-READ gives one."
  (typep item 'jumbf))

(defun jumbf-cbor (octets box budget)
  "The CBOR item of BOX's first CBOR content box, read from OCTETS within
BUDGET (CBOR-READ); NIL when it has none."
  (let ((content (find-if (lambda (c) (and (consp c) (string= (first c) "cbor"))) (jumbf-children box))))
    (and content (values (cbor-read octets :start (second content) :end (third content) :budget budget)))))

;;; Certificates, as written

(defun der-item (octets at end)
  "The DER item of OCTETS at AT, within END, as three values: its tag, where
its contents begin and where they end; NIL when it does not fit."
  (when (< (1+ at) end)
    (let ((tag (aref octets at)) (first (aref octets (1+ at))))
      (multiple-value-bind (length from)
          (cond ((< first #x80) (values first (+ at 2)))
                ((<= 1 (- first #x80) 4)
                 (let ((n (- first #x80)))
                   (and (<= (+ at 2 n) end) (values (octets-integer octets (+ at 2) n :big-endian t) (+ at 2 n)))))
                (t nil))
        (and length (<= (+ from length) end) (values tag from (+ from length)))))))

(defun der-items (octets start end)
  "The DER items of OCTETS from START below END, each (TAG FROM TO), at most 64."
  (loop with at = start
        repeat 64
        while (< at end)
        for (tag from to) = (multiple-value-list (der-item octets at end))
        while tag
        collect (list tag from to)
        do (setf at to)))

(defparameter *der-name-parts*
  '(((#x55 #x04 #x03) . "CN") ((#x55 #x04 #x0A) . "O") ((#x55 #x04 #x0B) . "OU") ((#x55 #x04 #x06) . "C")
    ((#x55 #x04 #x07) . "L") ((#x55 #x04 #x08) . "ST")
    ((#x2A #x86 #x48 #x86 #xF7 #x0D #x01 #x09 #x01) . "E"))
  "The parts of a certificate's names written by their short names: common
name (CN), organisation (O), unit (OU), country (C), locality (L), state
\(ST), email address (E), each by the octets of its object identifier.")

(defun der-name (octets start end)
  "The X.509 name in OCTETS from START below END in words, its parts in the
order written, each SHORT=VALUE (*DER-NAME-PARTS*; another part by its
object identifier in dots), on one line and shortened to 300 columns."
  (shorten-string
   (format nil "~{~a~^, ~}"
          (loop for (nil set-from set-to) in (der-items octets start end)
                append (loop for (nil from to) in (der-items octets set-from set-to)
                             for ((nil oid-from oid-to) (string-tag value-from value-to)) = (der-items octets from to)
                             when (and oid-to value-to)
                               collect (let ((oid (coerce (subseq octets oid-from
                                                                  (min oid-to (+ oid-from *oid-octets-most* 1)))
                                                          'list)))
                                         (format nil "~a=~a"
                                                 (or (cdr (assoc oid *der-name-parts* :test #'equal))
                                                     (oid-words oid))
                                                 (if (= string-tag #x1E)
                                                     (utf-16-text octets :start value-from :end value-to :big-endian t)
                                                     (octets-string octets :start value-from
                                                                           :end (min value-to (+ value-from 256))
                                                                           :replacement #\?)))))))
   300 :one-line t))

(defparameter *oid-octets-most* 32
  "The most octets of an object identifier read for its name in dots; a
longer one, which no certificate needs, is cut and ends in ...")

(defun oid-words (octets)
  "The object identifier whose DER contents are OCTETS, a list, in dots:
2.5.4.3; at most *OID-OCTETS-MOST* of them read, ... after when there are
more."
  (let ((parts '()) (value 0) (cut (> (length octets) *oid-octets-most*)))
    (dolist (o (subseq octets 0 (min (length octets) *oid-octets-most*)))
      (setf value (+ (* value 128) (logand o 127)))
      (unless (logtest o 128) (push value parts) (setf value 0)))
    (setf parts (nreverse parts))
    (if parts
        (format nil "~{~d~^.~}~:[~;...~]" (list* (min 2 (floor (first parts) 40))
                                               (- (first parts) (* 40 (min 2 (floor (first parts) 40))))
                                               (rest parts))
                cut)
        (if cut "..." "?"))))

(defun certificate-names (octets)
  "The subject and the issuer of the X.509 certificate whose DER is OCTETS,
in words (DER-NAME), as two values; NIL when it is not one."
  (multiple-value-bind (tag from to) (der-item octets 0 (length octets))
    (when (eql tag #x30)
      (multiple-value-bind (tag from to) (der-item octets from to)
        (when (eql tag #x30)
          (let* ((items (der-items octets from to))
                 ;; the version, when written, comes first in a box of its own
                 (items (if (eql (first (first items)) #xA0) (rest items) items)))
            (destructuring-bind (&optional serial algorithm issuer validity subject &rest more) items
              (declare (ignore serial algorithm validity more))
              (when (and (eql (first issuer) #x30) (eql (first subject) #x30))
                (values (der-name octets (second subject) (third subject))
                        (der-name octets (second issuer) (third issuer)))))))))))

(defun signature-certificate (signature budget)
  "The signer's certificate of SIGNATURE, a COSE_Sign1 as CBOR-READ gives it,
as an octet vector, read within BUDGET: the first of its x5chain (header 33)
in its protected or its unprotected headers; NIL when it names none."
  (let* ((sign1 (if (and (consp signature) (eq (first signature) :tag)) (third signature) signature))
         (protected (and (simple-vector-p sign1) (> (length sign1) 1) (svref sign1 0)))
         (headers (list (and (typep protected 'octets) (plusp (length protected))
                             (values (cbor-read protected :budget budget)))
                        (and (simple-vector-p sign1) (> (length sign1) 1) (svref sign1 1)))))
    (loop for header in headers
          for chain = (or (cbor-get header 33) (cbor-get header "x5chain"))
          for first = (if (simple-vector-p chain) (and (plusp (length chain)) (svref chain 0)) chain)
          when (typep first 'octets) return first)))

;;; Content credentials

(defstruct (credential (:copier nil) (:predicate nil))
  "One manifest of a file's content credentials (C2PA), as written and not
verified. LABEL is its label (urn:uuid:...). GENERATOR, the program that
wrote its claim; TITLE and FORMAT, the file it speaks of. ACTIONS, what it
says was done, each (ACTION SOURCE-TYPE AGENT): the action (c2pa.created,
c2pa.edited), its digital source type as IPTC names it (trainedAlgorithmicMedia)
or NIL, and the program that did it or NIL. INGREDIENTS, the files it was
made from, each (TITLE RELATIONSHIP). SIGNER and ISSUER, the subjects of
the signing certificate and of its issuer as written, or NIL. PROBLEM, why
some of it could not be read, or NIL. ACTIVE, true for the manifest of the
file itself, the last of the last store; the others are of the files it was
made from. STORE is the number, from 1, of the store that keeps it, when a
file keeps more than one."
  label generator title format (actions '()) (ingredients '()) signer issuer problem active (store 1))

(defun source-type-name (text)
  "The name of the IPTC digital source type TEXT, its address's last part:
trainedAlgorithmicMedia for http://cv.iptc.org/newscodes/digitalsourcetype/trainedAlgorithmicMedia."
  (and text (let ((slash (position #\/ text :from-end t))) (if slash (subseq text (1+ slash)) text))))

(defun agent-text (agent)
  "The program an action names, AGENT a text or a map with a name and a version, in words; or NIL."
  (or (cbor-text agent)
      (let ((name (cbor-text (cbor-get agent "name"))))
        (and name (format nil "~a~@[ ~a~]" name (cbor-text (cbor-get agent "version")))))))

(defun claim-generator (claim)
  "The program that wrote CLAIM, a C2PA claim as CBOR-READ gives it, in words:
its claim_generator, else the names of its claim_generator_info."
  (or (cbor-text (cbor-get claim "claim_generator"))
      (let ((info (cbor-get claim "claim_generator_info")))
        (cond ((simple-vector-p info) (format nil "~{~a~^, ~}" (remove nil (map 'list #'agent-text info))))
              (info (agent-text info))))))

(defun manifest-credential (octets manifest budget)
  "The CREDENTIAL MANIFEST, a JUMBF superbox of OCTETS, holds, read within
BUDGET. Its claim, each assertion and its signature are read each on its
own: one that cannot be read leaves the others, and the first problem is
kept."
  (let ((credential (make-credential :label (cbor-text (jumbf-label manifest))))
        (claim (jumbf-child manifest "c2cl")) (store (jumbf-child manifest "c2as"))
        (signature (jumbf-child manifest "c2cs")))
    (macrolet ((part (&body body)
                 `(handler-case (progn ,@body)
                    (unreadable-credentials (e)
                      (unless (credential-problem credential)
                        (setf (credential-problem credential) (unreadable-credentials-why e)))))))
      (part (let ((claim (and claim (jumbf-cbor octets claim budget))))
              (setf (credential-generator credential) (claim-generator claim)
                    (credential-title credential) (cbor-text (cbor-get claim "dc:title"))
                    (credential-format credential) (cbor-text (cbor-get claim "dc:format")))))
      (dolist (assertion (and store (jumbf-children store)))
        (when (jumbf-p assertion)
          (part (assertion-read octets assertion budget credential))))
      (setf (credential-actions credential) (nreverse (credential-actions credential))
            (credential-ingredients credential) (nreverse (credential-ingredients credential)))
      (part (let ((certificate (and signature (signature-certificate (jumbf-cbor octets signature budget) budget))))
              (when certificate
                (multiple-value-bind (subject issuer) (certificate-names certificate)
                  (setf (credential-signer credential) subject (credential-issuer credential) issuer))))))
    credential))

(defun assertion-read (octets assertion budget credential)
  "Push onto CREDENTIAL what ASSERTION, a JUMBF superbox of OCTETS, says,
read within BUDGET: the actions of c2pa.actions, the file of an ingredient,
or the digital source type any other names, as IPTC's own metadata does."
  (let ((label (or (jumbf-label assertion) "")))
    (cond ((uiop:string-prefix-p "c2pa.actions" label)
           (let ((actions (cbor-get (jumbf-cbor octets assertion budget) "actions")))
             (when (simple-vector-p actions)
               (loop for action across actions
                     for parameters = (cbor-get action "parameters")
                     do (push (list (or (cbor-text (cbor-get action "action")) "?")
                                    (source-type-name (cbor-text (or (cbor-get action "digitalSourceType")
                                                                     (cbor-get parameters "digitalSourceType"))))
                                    (agent-text (cbor-get action "softwareAgent")))
                              (credential-actions credential))))))
          ((uiop:string-prefix-p "c2pa.ingredient" label)
           (let ((value (jumbf-cbor octets assertion budget)))
             (push (list (or (cbor-text (cbor-get value "dc:title")) (cbor-text (cbor-get value "title")) "?")
                         (cbor-text (cbor-get value "relationship")))
                   (credential-ingredients credential))))
          (t
           (let* ((content (find-if #'consp (jumbf-children assertion)))
                  (mark (string-octets "digitalsourcetype/"))
                  (at (and content (member (first content) '("json" "cbor") :test #'string=)
                           (octets-search mark octets :start (second content) :end (third content)))))
             (when at
               (let* ((from (+ at (length mark)))
                      (to (or (position-if-not #'alpha-char-p octets :start from :end (third content) :key #'code-char)
                              (third content))))
                 (push (list (cbor-text label) (octets-latin-1 octets :start from :end (min to (+ from 64))) nil)
                       (credential-actions credential)))))))))

(defun credentials-in (octets &optional (ranges (list (cons 0 (length octets)))))
  "The CREDENTIALs of every C2PA manifest store in OCTETS within RANGES, each
\(START . END), a JUMBF superbox of kind c2pa among the boxes there: each
manifest one, its STORE the store's number from 1. The last manifest of the
last store is the active one, as C2PA reads them. NIL when there is no
store; a CREDENTIAL with only its PROBLEM for a range that cannot be read.
Reading stops at the limits of *CBOR-ITEMS-MOST*, *CBOR-KEPT-MOST* and
*JUMBF-BOXES-MOST*, all the stores together; stores larger than
*CREDENTIALS-STORE-MOST* in all are not read, and that is their problem."
  (let ((budget (cbor-budget)) (boxes (list *jumbf-boxes-most*)) (number 0) (out '()))
    (handler-case
        (let ((size (loop for (start . end) in ranges sum (- end start))))
          (when (> size *credentials-store-most*)
            (credentials-refused "they take ~:d octets, more than ~:d (*credentials-store-most*)"
                                 size *credentials-store-most*))
          (loop for (start . end) in ranges
                do (handler-case
                       (dolist (store (jumbf-read octets start end :budget boxes))
                         (when (and (jumbf-p store) (equal (jumbf-kind store) "c2pa"))
                           (incf number)
                           (let ((manifests (remove-if-not (lambda (c) (and (jumbf-p c)
                                                                            (member (jumbf-kind c) '("c2ma" "c2um")
                                                                                    :test #'equal)))
                                                           (jumbf-children store))))
                             (if manifests
                                 (dolist (m manifests)
                                   (let ((credential (manifest-credential octets m budget)))
                                     (setf (credential-store credential) number)
                                     (push credential out)))
                                 (push (make-credential :problem "its manifest store holds no manifest"
                                                        :store number)
                                       out)))))
                     (unreadable-credentials (e)
                       (push (make-credential :problem (unreadable-credentials-why e)) out)))))
      (unreadable-credentials (e) (push (make-credential :problem (unreadable-credentials-why e)) out)))
    (when out (setf (credential-active (first out)) t))
    (nreverse out)))

(defparameter *credentials-store-most* (* 64 1024 1024)
  "The most octets of a file's content credentials read: a store said to be
larger is not read, and that is said.")

(defun jpeg-credential-store (octets)
  "The octets of the JUMBF boxes the JPEG in OCTETS keeps in its APP11
segments, as C2PA and JPEG XT keep them: the segments of each box, told by
the number of the box, put back together in the order of their sequence
numbers, the first whole and the rest without the header each repeats.
NIL when it keeps none; past *CREDENTIALS-STORE-MOST* in all, the rest left
out. A second value says, in words, which sequence numbers of a box were
repeated, only the first of each kept, or are missing."
  (let ((pieces '()) (total 0))
    (jpeg-segments octets
                   (lambda (marker start end)
                     (when (and (= marker #xEB) (>= (- end start) 16) (octets-at-p octets "JP" start)
                                (octets-at-p octets "jumb" (+ start 12)))
                       (let* ((box (octets-integer octets (+ start 2) 2 :big-endian t))
                              (sequence (octets-integer octets (+ start 4) 4 :big-endian t))
                              (header (if (eql (octets-integer octets (+ start 8) 4 :big-endian t) 1) 16 8))
                              (from (if (= sequence 1) (+ start 8) (min end (+ start 8 header)))))
                         (when (<= (incf total (- end from)) *credentials-store-most*)
                           (push (list box sequence from end) pieces))))
                     nil))
    (when pieces
      (let* ((sorted (stable-sort (nreverse pieces) (lambda (a b) (or (< (first a) (first b))
                                                                      (and (= (first a) (first b))
                                                                           (< (second a) (second b)))))))
             (problems '())
             (sorted (loop for (piece . more) on sorted
                           for (box sequence) = piece
                           for next = (first more)
                           for previous = nil then kept
                           for kept = piece
                           when (and (or (null previous) (/= box (first previous))) (/= sequence 1))
                             do (pushnew (format nil "box ~d lacks sequence 1" box) problems :test #'string=)
                           if (and previous (= box (first previous)) (= sequence (second previous)))
                             do (pushnew (format nil "box ~d repeats sequence ~d" box sequence) problems
                                         :test #'string=)
                           else collect piece
                           when (and next (= box (first next)) (> (second next) (1+ sequence)))
                             do (push (format nil "box ~d lacks sequence ~d" box (1+ sequence)) problems)))
             (out (make-array (loop for (nil nil from end) in sorted sum (- end from))
                              :element-type '(unsigned-byte 8)))
             (filled 0))
        (loop for (nil nil from end) in sorted
              do (replace out octets :start1 filled :start2 from :end2 end)
                 (incf filled (- end from)))
        (values out (and problems (format nil "its APP11 segments do not join up: ~{~a~^, ~}"
                                          (reverse problems))))))))

(defun png-credential-stores (octets)
  "Where the content credentials of the PNG in OCTETS lie, its caBX chunks,
each (START . END); NIL when it has none. At most 100,000 chunks are looked
at."
  (let ((at 8) (out '()))
    (loop repeat 100000
          while (<= (+ at 8) (length octets))
          do (let* ((length (octets-integer octets at 4 :big-endian t)) (data (+ at 8)) (end (+ data length)))
               (when (or (> end (length octets)) (octets-at-p octets "IEND" (+ at 4))) (return))
               (when (octets-at-p octets "caBX" (+ at 4)) (push (cons data end) out))
               (setf at (+ end 4))))
    (nreverse out)))

(defun tiff-credential-store (octets)
  "Where the content credentials of the TIFF in OCTETS lie, the value of tag
52545 of its first directory of tags, as C2PA keeps them, as two values,
its start and end; NIL when it has none. At most 1,000 tags are looked at."
  (let ((big (octets-at-p octets "MM")))
    (when (or big (octets-at-p octets "II"))
      (let* ((ifd (octets-integer octets 4 4 :big-endian big))
             (count (and ifd (octets-integer octets ifd 2 :big-endian big))))
        (when count
          (loop for k below (min count 1000)
                for entry = (+ ifd 2 (* 12 k))
                for tag = (octets-integer octets entry 2 :big-endian big)
                while tag
                when (= tag 52545)
                  do (let ((length (octets-integer octets (+ entry 4) 4 :big-endian big))
                           (offset (octets-integer octets (+ entry 8) 4 :big-endian big)))
                       (return (and length offset (> length 4) (<= (+ offset length) (length octets))
                                    (values offset (+ offset length)))))))))))

(defun picture-credentials (octets kind)
  "The CREDENTIALs of the picture file of KIND (:JPEG, :PNG, :TIFF) whose
contents are OCTETS (CREDENTIALS-IN); NIL when it keeps none."
  (case kind
    (:jpeg (multiple-value-bind (store problem) (jpeg-credential-store octets)
             (let ((credentials (and store (credentials-in store))))
               (if problem (cons (make-credential :problem problem) credentials) credentials))))
    (:png (let ((ranges (png-credential-stores octets))) (and ranges (credentials-in octets ranges))))
    (:tiff (multiple-value-bind (start end) (tiff-credential-store octets)
             (and start (credentials-in octets (list (cons start end))))))))

;;; Generators' marks

(defparameter *digital-source-types*
  '(("trainedAlgorithmicMedia" :generated "made by a model trained on other pictures, an image generator")
    ("compositeWithTrainedAlgorithmicMedia" :generated "put together with parts an image generator made")
    ("algorithmicMedia" :program "made by a program from nothing taken from life, as a fractal is")
    ("compositeSynthetic" nil "put together from parts, some made by a program")
    ("digitalCapture" nil "taken by a camera or scanner")
    ("computationalCapture" nil "taken by a camera that worked on the picture as it took it, as phones do")
    ("negativeFilm" nil "scanned from a negative")
    ("positiveFilm" nil "scanned from a slide")
    ("print" nil "scanned from a print")
    ("humanEdits" nil "changed by a person")
    ("minorHumanEdits" nil "changed a little by a person")
    ("compositeCapture" nil "put together from pictures taken")
    ("composite" nil "put together from several pictures")
    ("algorithmicallyEnhanced" nil "made clearer by a program, sharpened or denoised")
    ("digitalArt" nil "drawn by a person on a computer")
    ("digitalCreation" nil "drawn by a person on a computer")
    ("dataDrivenMedia" nil "drawn from data")
    ("virtualRecording" nil "recorded from a scene a computer drew")
    ("screenCapture" nil "taken from a screen"))
  "The digital source types IPTC names, which content credentials and XMP
use to say how a picture was made, each (NAME KIND WORDS): KIND :GENERATED
for those that say an image generator made it or a part of it, :PROGRAM for
one that says a program made it from nothing taken from life, WORDS what the
type means.")

(defun source-type-words (name)
  "The digital source type NAME in words: its name and what it means, and as
a second value :GENERATOR when it says an image generator made the picture
or a part of it, :PROGRAM when a program made it from nothing taken from
life, else NIL (*DIGITAL-SOURCE-TYPES*)."
  (let ((known (assoc name *digital-source-types* :test #'string-equal)))
    (values (if known (format nil "~a (~a)" (first known) (third known)) (format nil "~a" name))
            (and known (case (second known) (:generated :generator) (:program :program))))))

(defparameter *generator-texts*
  '(("parameters" ("Steps:" "Sampler:" "Seed:" "CFG scale:" "Negative prompt:")
     "Stable Diffusion's settings, as AUTOMATIC1111, Forge and Fooocus write them")
    ("prompt" ("class_type") "ComfyUI's prompt")
    ("workflow" ("\"nodes\"" "\"links\"") "ComfyUI's workflow")
    ("invokeai_metadata" () "InvokeAI's settings")
    ("invokeai_graph" () "InvokeAI's workflow")
    ("sd-metadata" () "InvokeAI's settings")
    ("Dream" () "InvokeAI's settings")
    ("sui_image_params" () "SwarmUI's settings")
    ("fooocus_scheme" () "Fooocus's settings")
    ("Comment" ("\"steps\"" "\"sampler\"" "\"n_samples\"") "NovelAI's settings"))
  "The texts image generators write into the PNGs they make, each (KEYWORD
MARKS WORDS): the text's keyword, words one of which the text holds (none
when the keyword alone tells), and what it is. A text cut short at the
note's length before its marks still counts for \"parameters\", whose
settings follow the prompt (*GENERATOR-TEXT-CUT*).")

(defparameter *generator-text-cut* 400
  "How many characters of a PNG text called parameters, at least, count as
Stable Diffusion's settings without their marks: the note keeps 500 of a
PNG's text (*METADATA-VALUE-MOST*), and a long prompt pushes the settings
past them. Honest pictures seen keep no text of that name.")

(defparameter *generator-sizes*
  '((1024 1024) (1024 1536) (1536 1024) (1024 1792) (1792 1024) (1344 768) (768 1344) (832 1216) (1216 832)
    (896 1152) (1152 896) (1536 640) (640 1536) (2048 2048))
  "Picture sizes in pixels, across and down, that image generators often
make; many other pictures have them too, so a size is named only beside
another sign, never on its own.")

(defun generator-size-p (width height)
  "True when WIDTH by HEIGHT pixels is a size image generators often make (*GENERATOR-SIZES*)."
  (and (member (list width height) *generator-sizes* :test #'equal) t))

(defparameter *generator-program-labels* '("software" "creatortool" "history" "producer")
  "Words in the label of a field that names the program a file was made
with, where an image generator's name is looked for: EXIF's Software, XMP's
CreatorTool and edit history, a PDF's Producer. Comments and descriptions
are passed over, as their words are anyone's.")

(defun generator-marks (fields &key credentials)
  "What FIELDS, a file's metadata as PICTURE-FIELDS gives it, say of an
image generator, as two values: the marks that say one made it, each (KIND
WORDS), KIND :GENERATOR or :PROGRAM as SOURCE-TYPE-WORDS tells them, and
the facts recorded only, each in words. Marks: a digital source type of an
image generator (*DIGITAL-SOURCE-TYPES*), a generator's own text in a PNG
\(*GENERATOR-TEXTS*), a field naming the programs it went through that
names an image generator (*GENERATOR-PROGRAM-LABELS*, *PDF-TOOLS*), or a
PNG's text Source, where NovelAI names its model, that does. Facts: any
other digital source type, and the content credentials its XMP points to
\(dcterms:provenance), kept elsewhere and not fetched, or within the file
itself, said only when the file holds none (CREDENTIALS false), as when
they were taken out."
  (let ((marks '()) (facts '()))
    (loop for (heading label value) in fields
          for lower = (string-downcase label)
          do (cond ((uiop:string-suffix-p lower "digitalsourcetype")
                    (multiple-value-bind (words kind) (source-type-words (source-type-name value))
                      (if kind
                          (pushnew (list kind (format nil "its ~a says ~a" label words)) marks :test #'equal)
                          (pushnew (format nil "its ~a says ~a" label words) facts :test #'string=))))
                   ((and (uiop:string-suffix-p lower "provenance") (uiop:string-prefix-p "self#jumbf" value))
                    (unless credentials
                      (pushnew (format nil "its ~a points to content credentials within the file (~a), which it does ~
                                            not hold: taken out, or never written"
                                       label (shorten-string value 120 :one-line t))
                               facts :test #'string=)))
                   ((uiop:string-suffix-p lower "provenance")
                    (pushnew (format nil "its ~a points to content credentials kept elsewhere, at ~a, not fetched"
                                     label (shorten-string value 120 :one-line t))
                             facts :test #'string=))
                   ((and (string= heading "PNG text")
                         (let ((text (assoc label *generator-texts* :test #'string=)))
                           (and text (or (null (second text))
                                         (some (lambda (mark) (search mark value)) (second text))
                                         (and (string= label "parameters") (>= (length value) *generator-text-cut*))))))
                    (pushnew (list :generator (format nil "its PNG text ~s holds ~a" label
                                                      (third (assoc label *generator-texts* :test #'string=))))
                             marks :test #'equal))
                   ((or (some (lambda (word) (search word lower)) *generator-program-labels*)
                        (and (string= heading "PNG text") (string= label "Source")))
                    (dolist (tool (tools-in value :generators t))
                      (when (eq (third tool) :generator)
                        (pushnew (list :generator (format nil "its ~a names ~a (~a)" label (second tool) (fourth tool)))
                                 marks :test #'equal))))))
    (values (nreverse marks) (nreverse facts))))

(defun credential-generated (credential)
  "What CREDENTIAL says of an image generator, each (KIND WORDS) as
GENERATOR-MARKS gives them: each action whose digital source type is an
image generator's or a program's (SOURCE-TYPE-WORDS), and the program that
wrote its claim or did an action when it is an image generator
\(*PDF-TOOLS*)."
  (append (loop for (action source agent) in (credential-actions credential)
                for kind = (and source (nth-value 1 (source-type-words source)))
                when kind
                  collect (list kind (format nil "~a, digital source type ~a~@[, by ~a~]"
                                             action (source-type-words source) agent)))
          (loop for program in (remove-duplicates
                                (remove nil (cons (credential-generator credential)
                                                  (mapcar #'third (credential-actions credential))))
                                :test #'string=)
                append (loop for tool in (tools-in program :generators t)
                             when (eq (third tool) :generator)
                               collect (list :generator (format nil "written by ~a, which names ~a (~a)"
                                                                program (second tool) (fourth tool)))))))

(defun credential-details (credential)
  "Lines of the report for CREDENTIAL: what it says, as written."
  (let ((name (if (credential-active credential) "the file's own" "of a file it was made from")))
    (append (list (format nil "Content credentials (~a~:[~;, store ~d~]~@[, ~a~])" name
                          (> (credential-store credential) 1) (credential-store credential)
                          (credential-label credential)))
            (and (credential-generator credential)
                 (list (format nil "  written by: ~a" (credential-generator credential))))
            (and (credential-title credential)
                 (list (format nil "  of: ~a~@[ (~a)~]" (credential-title credential) (credential-format credential))))
            (loop for (action source agent) in (credential-actions credential)
                  repeat 50
                  collect (format nil "  done: ~a~@[, digital source type ~a~]~@[, by ~a~]"
                                  action (and source (source-type-words source)) agent))
            (loop for (title relationship) in (credential-ingredients credential)
                  repeat 50
                  collect (format nil "  made from: ~a~@[ (~a)~]" title relationship))
            (list (format nil "  ~a" (credential-signature-words credential)))
            (and (credential-problem credential)
                 (list (format nil "  not read wholly: ~a" (credential-problem credential)))))))

(defun credential-signature-words (credential)
  "Who CREDENTIAL says signed it, as written, and that the signature is not verified."
  (if (credential-signer credential)
      (format nil "signed by ~a~@[, its certificate issued by ~a~], not verified"
              (credential-signer credential) (credential-issuer credential))
      "no signer's certificate read, and nothing verified"))

(defparameter *credentials-detailed* 20
  "The most manifests of a file's content credentials whose lines a finding
gives in its details, the rest counted: a store can chain a great many.")

(defun making-findings (what credentials marks facts &key size details revision page objects evidence)
  "The findings on what WHAT (\"the picture\", \"the PDF\") says of how it was
made: CREDENTIALS, its content credentials (CREDENTIAL); MARKS and FACTS,
what its metadata says (GENERATOR-MARKS). Worth a look, when the
credentials or the marks say an image generator made it, all of them in
one finding, with SIZE, (WIDTH HEIGHT), named beside them when generators
often make it (GENERATOR-SIZE-P). It says how the file was made, not that
it is false, and is never strong: the credentials are not verified, and
most generated pictures carry no such sign. For the record, credentials
that say nothing of a generator, what could not be read of them, and the
FACTS. DETAILS go before every finding's own; REVISION, PAGE, OBJECTS and
EVIDENCE are given to each."
  (let* ((said (append (loop for c in credentials
                             append (loop for (kind words) in (credential-generated c)
                                          collect (list kind (credential-active c)
                                                        (format nil "~:[the content credentials of a file it was ~
                                                                     made from say~;its content credentials (C2PA) ~
                                                                     say~]: ~a"
                                                                (credential-active c) words))))
                       (loop for (kind words) in marks collect (list kind t words))))
         (generated (mapcar #'third said))
         (own (remove nil said :key #'second))
         (headline (cond ((find :generator own :key #'first) "it was made by an image generator")
                         (own "it was made by a program, not taken from life")
                         ((find :generator said :key #'first) "a part of it was made by an image generator")
                         (t "a part of it was made by a program, not taken from life")))
         (all-details (append details
                              (loop for c in credentials repeat *credentials-detailed* append (credential-details c))
                              (and (> (length credentials) *credentials-detailed*)
                                   (list (format nil "and ~:d manifests more"
                                                 (- (length credentials) *credentials-detailed*))))
                              (mapcar (lambda (f) (format nil "~a~a" (char-upcase (char f 0)) (subseq f 1))) facts)))
         (opening (format nil "~a~a" (char-upcase (char what 0)) (subseq what 1)))
         (out '()))
    (flet ((add (strength words rule &optional method)
             (push (found strength words :rule rule :details all-details :revision revision :page page
                                         :objects objects :evidence evidence :method method)
                   out)))
      (when generated
        (add :look (format nil "~a says ~a: ~{~a~^; ~}.~@[ Its size, ~
                                ~{~d by ~d~} pixels, is one image generators often make, as do many other ~
                                pictures.~] It tells how the file was made, not that the document is false~:[~;; ~
                                content credentials are signed, but the signature is not verified here~]."
                           opening headline generated (and size (apply #'generator-size-p size) size)
                           (some #'credential-generated credentials))
             (concatenate 'string "A picture may say how it was made: in content credentials (C2PA), whose actions "
                                  "and digital source type name an image generator, in the digital source type of "
                                  "its XMP, or in the text a generator writes. Read as data and never run; most "
                                  "generated pictures say none of this.")
             :generated))
      (dolist (c credentials)
        (unless (or (credential-generated c) (not (credential-active c))
                    (and (credential-problem c) (null (credential-actions c))))
          (add :record (format nil "~a holds content credentials (C2PA)~@[ written by ~a~] that say nothing ~
                                    of an image generator~@[: ~{~a~^, ~}~]; ~a."
                               opening (credential-generator c)
                               (let ((done (remove-duplicates (mapcar #'first (credential-actions c))
                                                              :test #'string= :from-end t)))
                                 (and done (subseq done 0 (min 10 (length done)))))
                               (credential-signature-words c))
               (concatenate 'string "Content credentials (C2PA) are a signed record of who made a file and how; "
                                    "read as written, the signature not verified.")))
        (when (credential-problem c)
          (add :record (format nil "The content credentials of ~a could not be read wholly: ~a."
                               what (credential-problem c))
               "Content credentials are read as far as they make sense, within limits of size, count and depth.")))
      (let ((stores (reduce #'max credentials :key #'credential-store :initial-value 0)))
        (when (> stores 1)
          (add :record (format nil "~a holds ~d stores of content credentials, where C2PA writes one: the last ~
                                    manifest of the last is read as its own, as C2PA reads them, and every ~
                                    manifest is listed in the details."
                               opening stores)
               "A file keeps one store of content credentials; each one found is read.")))
      (when (and facts (not generated))
        (add :record (format nil "~a says how it was made: ~{~a~^; ~}." opening facts)
             "The digital source type IPTC names, and a pointer to content credentials, are read as written.")))
    (nreverse out)))

(defun check-picture-making (file)
  "What FILE, a PICTURE-FILE, says of how it was made: its content
credentials and an image generator's marks (MAKING-FINDINGS)."
  (multiple-value-bind (marks facts) (generator-marks (picture-fields (picture-file-sections file))
                                                     :credentials (picture-file-credentials file))
    (making-findings "the picture" (picture-file-credentials file) marks facts
                     :size (and (plusp (picture-file-width file))
                                (list (picture-file-width file) (picture-file-height file))))))

(defun pdf-credential-stores (pdf revision)
  "The content credentials REVISION of PDF keeps in files embedded in it, as
C2PA keeps them in a PDF (an associated file, its relationship
C2PA_Manifest), each (CREDENTIALS NUMBER): the CREDENTIALs of a stream
whose decoded octets are a C2PA manifest store, and the stream's object
number. Every embedded file is looked at, however it is named."
  (let ((seen (make-hash-table)) (out '()))
    (dolist (n (live-numbers pdf revision) (nreverse out))
      (job-check-stop)
      (let* ((object (revision-object pdf revision n)) (value (and object (pdf-indirect-get object :value))))
        (when (pdf-dictionary-p value)
          (let ((streams (cond ((name-p (pdf-get value "/Type") "/EmbeddedFile") (list n))
                               ((pdf-get value "/EF")
                                (let ((ef (dref pdf revision value "/EF")))
                                  (loop for key in '("/F" "/UF")
                                        for ref = (pdf-get ef key)
                                        when (pdf-ref-p ref) collect (second ref)))))))
            (dolist (number streams)
              (unless (gethash number seen)
                (setf (gethash number seen) t)
                (let* ((stream (revision-object pdf revision number))
                       (data (and stream (stream-data pdf stream :limit *credentials-store-most*))))
                  (when (and data (octets-at-p data "jumb" 4) (octets-search (string-octets "c2pa") data :end 64))
                    (push (list (credentials-in data) number) out)))))))))))

(defun jpeg-head (pdf object)
  "The octets of the JPEG OBJECT's stream holds as the file keeps them, as
far as they hold what it says of itself: the segments before its picture
data, read 64 KB at first and sixteen times more each time those run past
what was read, to *CREDENTIALS-STORE-MOST*; the whole stream when shorter."
  (loop for most = 65536 then (* most 16)
        for head = (raw-stream pdf object most)
        for last = 2
        do (when (or (null head) (< (length head) most) (>= most *credentials-store-most*)) (return head))
           (jpeg-segments head (lambda (marker start end) (declare (ignore marker start)) (setf last end) nil))
           (when (< (+ last 8) (length head)) (return head))))

(defun pdf-picture-making (pdf revision)
  "The findings on what the JPEG pictures REVISION of PDF draws say of how
they were made, each picture's own (MAKING-FINDINGS)."
  (let ((out '()))
    (loop for (m page) in (pictures-of pdf revision)
          for filters = (mapcar #'car (pdf-stream-filters (mark-actual m)))
          when (and filters (jpeg-filter-p (car (last filters))))
            do (job-check-stop)
               (guarding-item (out "the picture ~a (object ~d)" (mark-text m) (mark-object m))
                 (let* ((object (revision-object pdf revision (mark-object m)))
                        (data (and object (if (rest filters) (stream-data pdf object) (jpeg-head pdf object))))
                        (credentials (and data (picture-credentials data :jpeg))))
                   (when data
                     (multiple-value-bind (marks facts)
                         (generator-marks (picture-fields (handler-case (octets-metadata data :jpeg)
                                                            ((or error storage-condition) () '())))
                                          :credentials credentials)
                       (let ((width (pdf-get (mark-actual m) "/Width")) (height (pdf-get (mark-actual m) "/Height")))
                         (setf out (append (reverse (making-findings
                                                     (format nil "the picture ~a (object ~d) on page ~d"
                                                             (mark-text m) (mark-object m) (1+ (page-index page)))
                                                     credentials marks facts
                                                     :size (and (integerp width) (integerp height) (list width height))
                                                     :revision (revision-index revision)
                                                     :page (1+ (page-index page))
                                                     :objects (list (mark-object m))
                                                     :evidence (nth-value 0 (object-evidence pdf revision
                                                                                             (mark-object m)))))
                                           out))))))))
    (nreverse out)))

(defun check-made-by-generator (pdf revision)
  "What PDF says, as REVISION has it, of how it and its pictures were made:
content credentials in files embedded in it, the digital source type of its
XMP, and the JPEG pictures' own credentials and marks (MAKING-FINDINGS)."
  (append (loop for (credentials number) in (pdf-credential-stores pdf revision)
                append (making-findings "the PDF" credentials '() '()
                                        :revision (revision-index revision) :objects (list number)
                                        :evidence (nth-value 0 (object-evidence pdf revision number))))
          (multiple-value-bind (fields tree number) (xmp-of pdf revision)
            (declare (ignore tree))
            (multiple-value-bind (marks facts) (generator-marks (loop for (name . value) in fields
                                                                      when (stringp value)
                                                                        collect (list "XMP" name value)))
              (making-findings "the PDF" '() marks facts :revision (revision-index revision)
                                                         :objects (and number (list number)))))
          (pdf-picture-making pdf revision)))
