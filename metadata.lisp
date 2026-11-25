;;;; metadata.lisp --- What a PDF says of itself: its document information and XMP, set against what it is.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:palimpsest)

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
                                :method :metadata-edited :kind :info-xmp)
                         out))))
    (nreverse out)))

(defun clock-slip-p (gap made-zone changed-zone)
  "True when a file said to be changed GAP seconds before it was made, the
two dates written in the zones MADE-ZONE and CHANGED-ZONE (minutes from
UTC, NIL for none), can be an honest program's slip: the zones differ or
one is missing and GAP is 14 hours at most, as a time zone written wrong
leaves; GAP is an hour at most, as a clock moved for summer leaves; or GAP
is within a quarter of an hour of 12 hours, as the hour of a twelve-hour
clock written without its half of the day leaves."
  (or (and (or (null made-zone) (null changed-zone) (/= made-zone changed-zone)) (<= gap (* 14 3600)))
      (<= gap 3600)
      (<= (abs (- gap (* 12 3600))) 900)))

(defun metadata-date-findings (reading where created changed created-evidence changed-evidence)
  "The findings of the dates CREATED and CHANGED that WHERE gives, with
their evidence: changed more than a minute before made, worth a look when
an honest program's clock can have slipped so (CLOCK-SLIP-P), else
strong; a date more than a day in the future, a time zone no place uses,
made and changed in two zones."
  (let ((revision (revision-index (metadata-reading-last reading))) (now (get-universal-time)) (out '()))
    (multiple-value-bind (ct cz) (date-of created)
      (multiple-value-bind (mt mz) (date-of changed)
        (when (and ct mt (< mt (- ct 60)))
          (let ((by-hand (not (clock-slip-p (- ct mt) cz mz))))
            (push (found (if by-hand :strong :look)
                         (format nil "In ~a the file was last changed (~a) before it was made (~a): a date was ~
                                      set by hand~:[, or a clock or a time zone written wrong~;~]."
                                 where (utc-time-words mt) (utc-time-words ct) by-hand)
                         :rule "A file cannot be changed before it was made."
                         :revision revision :evidence (append created-evidence changed-evidence)
                         :method :metadata-edited :kind :dates-before-made)
                  out)))
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
      (push (found :record (format nil "The XMP metadata says the document was derived from another: ~a."
                                   (cdr derived))
                   :rule "xmpMM:DerivedFrom names the document a file was made from."
                   :revision (revision-index last) :evidence (xmp-evidence reading "DerivedFrom")
                   :method :other-program)
            out))
    (nreverse out)))

(defun xmp-toolkit (tree)
  "The toolkit an XMP TREE says wrote it (the x:xmptk of its xmpmeta
element), or NIL."
  (loop for node in (xml-find-all tree "xmpmeta" :local t)
        thereis (loop for (name . value) in (second node)
                      when (and (stringp value) (string= (xml-unprefixed name) "xmptk") (plusp (length value)))
                        return value)))

(defun metadata-toolkit (reading)
  "The finding that READING's XMP was written by a toolkit that edits files
\(an :editor of *PDF-TOOLS*, ExifTool say) which neither its producer nor
its creator names: the XMP was written anew after the file was made. NIL
for none."
  (let* ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
         (toolkit (and (metadata-reading-tree reading) (xmp-toolkit (metadata-reading-tree reading))))
         (tool (find :editor (tools-in toolkit) :key #'third))
         (named (format nil "~@[~a~] ~@[~a~]" (info-text pdf last "/Producer") (info-text pdf last "/Creator"))))
    (when (and tool (not (find (second tool) (tools-in named) :key #'second :test #'string=)))
      (found :look (format nil "The XMP metadata was written by ~a (~a), ~a, which the file names neither as its ~
                                producer nor as its creator: what the file says of itself was written anew after it ~
                                was made."
                           (second tool) toolkit (fourth tool))
             :rule (format nil "A program that makes a PDF writes its XMP with its own toolkit, named in the packet's ~
                                x:xmptk; a packet written by a program that edits metadata was put there afterwards. ~
                                Archives and publishers add titles and keywords so too.")
             :revision (revision-index last)
             :objects (remove nil (list (metadata-reading-xmp-number reading)))
             :evidence (and (metadata-reading-xmp-number reading)
                            (nth-value 0 (object-evidence pdf last (metadata-reading-xmp-number reading))))
             :method :metadata-edited))))

(defun metadata-programs (reading)
  "The findings of the programs READING's producer and creator name: each
that edits PDFs, and Acrobat saving again what another program made."
  (let* ((pdf (metadata-reading-pdf reading)) (last (metadata-reading-last reading))
         (producer (info-text pdf last "/Producer")) (creator (info-text pdf last "/Creator"))
         (out '()))
    ;; each program that edits PDFs once, with every field naming it
    (let ((named '()))
      (loop for (place revision text) in (program-names pdf)
            when (eq revision last)
              do (dolist (tool (tools-in text))
                   (when (eq (third tool) :editor)
                     (let ((entry (assoc (second tool) named :test #'string=)))
                       (if entry
                           (pushnew place (cddr entry) :test #'string=)
                           (push (list (second tool) (fourth tool) place) named))))))
      (loop for (name what . places) in (reverse named)
            for places-in-order = (reverse places)
            do (push (found :record (format nil "~{~a~#[~; and ~:;, ~]~} name~:[s~;~] ~a, ~a: the file passed ~
                                                 through a program that edits PDFs."
                                            places-in-order (rest places-in-order) name what)
                            :rule (format nil "The programs a file names in its producer and creator, as ~
                                               *pdf-tools* lists them, tell how it was made and changed. Honest ~
                                               files pass through such programs too, a scan's text recognition in ~
                                               Acrobat among them, so it is for the record, each program once.")
                            :revision (revision-index last)
                            :evidence (loop for place in places-in-order
                                            append (if (char= #\/ (char place 0))
                                                       (info-evidence reading place)
                                                       (xmp-evidence reading (xml-unprefixed (subseq place 4)))))
                            :method :pdf-editor)
                     out)))
    (when (and producer creator
               (let ((producer (string-downcase producer)))
                 (and (search "acrobat" producer) (not (search "distiller" producer))
                      (not (search "pdfmaker" producer))))
               (not (search "acrobat" (string-downcase creator))))
      (push (found :record (format nil "It was made by ~a and saved again by ~a." creator producer)
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
                       :revision (revision-index revision) :method :metadata-removed :kind :metadata-removed)
                out))))
    (when (and (xmp-of pdf before) (not (xmp-of pdf revision)))
      (push (found :look (format nil "~@(~a~) removes the XMP metadata." (revision-name revision))
                   :rule (format nil "A save that drops the catalogue's /Metadata has removed what the file said of ~
                                      itself.")
                   :revision (revision-index revision) :method :metadata-removed :kind :metadata-removed)
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
                                       (file-date-order reading)
                                       (metadata-history reading)
                                       (metadata-programs reading)
                                       (list (metadata-toolkit reading) (maker-mark-finding pdf))
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
