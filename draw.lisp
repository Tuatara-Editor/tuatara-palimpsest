;;;; draw.lisp --- A copy of a revision with each thing a page draws framed and named by its object.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; The copy is the revision as it was, to its %%EOF, and one save more that
;;;; lays a drawing over each page: a frame round each line of text, shape,
;;;; picture and annotation, and beside it the number of the object that
;;;; draws it, each content stream in a colour of its own. It is written to
;;;; the report's own folder, never beside the PDF examined, and shown in
;;;; Tuatara's PDF view beside the report.

(in-package #:palimpsest)

;;; PDF values written

(defun pdf-name-text (name)
  "NAME, text with a / first as PDF-OBJECT-AT reads a name, written as a PDF
writes it: every character but the plain ones as # and its two hex digits,
so that no delimiter, space or other character in it ends it or begins
something else."
  (with-output-to-string (s)
    (write-char #\/ s)
    (loop for c across (subseq name 1)
          for code = (logand (char-code c) 255)
          do (if (and (< 32 code 127) (not (find c "()<>[]{}/%#")))
                 (write-char c s)
                 (format s "#~2,'0x" code)))))

(defun pdf-literal-text (text)
  "TEXT written as a PDF literal string: in brackets, those and the
backslash escaped, every character but the printable ones as \\ and its
three octal digits."
  (with-output-to-string (s)
    (write-char #\( s)
    (loop for c across text
          do (cond ((find c "()\\") (write-char #\\ s) (write-char c s))
                   ((<= 32 (char-code c) 126) (write-char c s))
                   (t (format s "\\~3,'0o" (logand (char-code c) 255)))))
    (write-char #\) s)))

(defun pdf-value-text (value)
  "VALUE, as PDF-OBJECT-AT reads one, written as a PDF writes it: a name (text
with a / first, PDF-NAME-TEXT), true, false and null as words, other text as
a literal string (PDF-LITERAL-TEXT), a number, a reference, an array or a
dictionary."
  (cond ((pdf-ref-p value) (format nil "~d ~d R" (second value) (or (third value) 0)))
        ((pdf-dictionary-p value)
         (format nil "<< ~{~a~^ ~} >>" (loop for (key . v) in value collect (format nil "~a ~a" (pdf-name-text key) (pdf-value-text v)))))
        ((integerp value) (format nil "~d" value))
        ((realp value) (let ((text (format nil "~,4f" value))) (string-right-trim "." (string-right-trim "0" text))))
        ((stringp value)
         (cond ((and (plusp (length value)) (char= (char value 0) #\/)) (pdf-name-text value))
               ((member value '("true" "false" "null") :test #'string=) value)
               (t (pdf-literal-text value))))
        ((null value) "null")
        ((listp value) (format nil "[~{~a~^ ~}]" (mapcar #'pdf-value-text value)))
        (t "null")))

;;; The drawing laid over a page

(defparameter *draw-colours*
  '((0.85 0.1 0.1) (0.1 0.35 0.85) (0.1 0.6 0.2) (0.8 0.45 0.0) (0.55 0.15 0.7) (0.0 0.55 0.6) (0.6 0.4 0.2))
  "The colours the objects drawing a page are framed in, one after another:
each content stream, form and picture its own.")

(defun overlay-operators (pdf revision page &key scans copies)
  "The operators of the drawing laid over PAGE of REVISION: a frame round each
line of text (each part of it a stream or form of its own draws), each shape
and each picture, and round each annotation, with the number of the object
that draws it beside it: for text, also where in that stream's decoded
octets its operators begin (@312) and the font objects it is set in. On a
scanned picture, in red, its words unlike the rest and its characters
copied, from SCANS (SCAN-PICTURES) and COPIES (COPIED-GLYPHS)."
  (let ((colours (make-hash-table)) (next 0) (out (make-string-output-stream)) (labels-placed '()))
    (labels ((colour (number)
               (or (gethash number colours)
                   (setf (gethash number colours) (nth (mod (1- (incf next)) (length *draw-colours*)) *draw-colours*))))
             (frame (x0 y0 x1 y1 label colour &key dashed)
               ;; kept on the page: a frame at its edge drawn just inside it
               (destructuring-bind (px0 py0 px1 py1) (or (page-crop page) (page-media page))
                 (setf x0 (max x0 (+ px0 1)) y0 (max y0 (+ py0 1)) x1 (min x1 (- px1 1)) y1 (min y1 (- py1 1))))
               (destructuring-bind (r g b) colour
                 (format out "q ~,3f ~,3f ~,3f RG 0.6 w ~:[[] 0~;[2 1] 0~] d ~,2f ~,2f ~,2f ~,2f re S Q~%"
                         r g b dashed x0 y0 (- x1 x0) (- y1 y0))
                 ;; the label on white, above the frame's top left (inside it when that would be off the
                 ;; page), moved right past any label already there
                 (let* ((width (+ 2 (* 6 0.56 (length label)))) (x x0)
                        (y (if (> (+ y1 8) (fourth (or (page-crop page) (page-media page)))) (- y1 8) (+ y1 0.5))))
                   (loop while (some (lambda (placed) (destructuring-bind (px py pw) placed
                                                        (and (< (abs (- py y)) 7) (< x (+ px pw)) (< px (+ x width)))))
                                     labels-placed)
                         do (incf x 3))
                   (push (list x y width) labels-placed)
                   (format out "q 1 1 1 rg ~,2f ~,2f ~,2f 7 re f ~,3f ~,3f ~,3f rg BT /PalimpsestLabel 6 Tf ~,2f ~,2f Td ~a Tj ET Q~%"
                           x y width r g b (+ x 1) (+ y 1.5) (pdf-literal-text label))))))
      (let ((marks (marks-of pdf revision page)))
        ;; text, line by line: each stretch of it one stream or form draws without a gap wider than one
        ;; and a half times its size (a column apart) framed on its own, named by the stream, where in its
        ;; decoded octets the stretch's operators begin, and the font objects it is set in
        (dolist (line (page-lines-of marks))
          (let ((parts '()))
            (dolist (m line)
              (when (mark-source m)
                (let ((part (first parts)))
                  (if (and part (eql (first part) (mark-source m))
                           (<= (- (mark-x0 m) (fourth part)) (* 1.5 (max 1 (mark-size m)))))
                      (destructuring-bind (x0 y0 x1 y1 from fonts) (cdr part)
                        (setf (cdr part) (list (min x0 (mark-x0 m)) (min y0 (mark-y0 m)) (max x1 (mark-x1 m)) (max y1 (mark-y1 m))
                                               (if (and from (mark-from m)) (min from (mark-from m)) (or from (mark-from m)))
                                               (let ((n (and (mark-font m) (font-number (mark-font m)))))
                                                 (if n (adjoin n fonts) fonts)))))
                      (push (list (mark-source m) (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (mark-from m)
                                  (let ((n (and (mark-font m) (font-number (mark-font m))))) (and n (list n))))
                            parts)))))
            (loop for (number x0 y0 x1 y1 from fonts) in (reverse parts)
                  do (frame (- x0 0.5) (- y0 1.5) (+ x1 0.5) y1
                            (place-label number from fonts)
                            (colour number)))))
        ;; shapes of some size, and pictures
        (loop for m across marks
              do (case (mark-kind m)
                   (:fill (when (and (mark-source m) (> (- (mark-x1 m) (mark-x0 m)) 4) (> (- (mark-y1 m) (mark-y0 m)) 4))
                            (frame (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (format nil "~d shape" (mark-source m))
                                   (colour (mark-source m)) :dashed t)))
                   (:image (when (mark-object m)
                             (frame (mark-x0 m) (mark-y0 m) (mark-x1 m) (mark-y1 m) (format nil "~d picture" (mark-object m))
                                    (colour (mark-object m)))
                             ;; on a scan, the words unlike the rest, in red, with how they are unlike
                             (let ((scan (find-if (lambda (scan) (and (eq (scan-picture-page scan) page)
                                                                      (eql (mark-object (scan-picture-mark scan)) (mark-object m))))
                                                  scans)))
                               (when scan
                                 (let ((width (scan-picture-width scan)) (height (scan-picture-height scan)))
                                   (loop for (word . ways) in (scan-picture-odd scan)
                                         do (destructuring-bind (x0 y0 x1 y1) (word-page-box m word width height (scan-picture-tilt scan))
                                              (frame (- x0 1) (- y0 1) (+ x1 1) (+ y1 1)
                                                     (format nil "~{~a~^, ~}" (unlike-words-words ways)) '(0.9 0.0 0.0))))
                                   (loop for (group) in copies
                                         do (dolist (glyph group)
                                              (when (and (eql (glyph-picture glyph) (mark-object m)) (eql (glyph-page glyph) (page-index page)))
                                                (destructuring-bind (x0 y0 x1 y1) (glyph-page-box m glyph width height)
                                                  (frame (- x0 0.5) (- y0 0.5) (+ x1 0.5) (+ y1 0.5) "copy" '(0.9 0.0 0.0))))))))))))))
      ;; annotations, laid over the page
      (dolist (value (page-annots page))
        (let* ((annot (deref pdf revision value))
               (subtype (and (pdf-dictionary-p annot) (pdf-get annot "/Subtype")))
               (box (and (pdf-dictionary-p annot) (box-of (deref pdf revision (pdf-get annot "/Rect"))))))
          (when (and box (stringp subtype) (not (string= subtype "/Popup")))
            (destructuring-bind (x0 y0 x1 y1) box
              (frame x0 y0 x1 y1 (format nil "~@[~d ~]~a" (and (pdf-ref-p value) (second value)) (subseq subtype 1))
                     '(0.9 0.3 0.6) :dashed t))))))
    (get-output-stream-string out)))

;;; The copy

(defun marked-copy (analysis revision target &key (cancel (list nil)))
  "Write to TARGET the revision REVISION of ANALYSIS's PDF with one save more
laying the drawing of OVERLAY-OPERATORS over each of its pages; the page's
own drawing kept apart between q and Q, and document information of its own
saying what the copy is (the revision's own in the save before). Returns TARGET; an error saying why
it cannot be made (an encrypted file, whose added streams would be read as
encrypted too)."
  (let* ((pdf (analysis-pdf analysis))
         (trailer (revision-trailer revision)))
    (when (pdf-get trailer "/Encrypt")
      (error "the PDF is encrypted: a drawing added to it would be read as encrypted too"))
    (let* ((octets (pdf-octets pdf))
           (end (revision-end revision))
           (next (1+ (loop for r across (pdf-revisions pdf)
                           maximize (loop for n being the hash-keys of (revision-entries r) maximize n))))
           (objects '())
           (font next) (open-state (+ next 1)) (close-state (+ next 2)) (info (+ next 3))
           (scans (nth-value 0 (scans-read pdf revision)))
           (copies (nth-value 1 (scans-read pdf revision))))
      (setf next (+ next 4))
      (flet ((add (number text) (push (cons number text) objects))
             (stream-text (data) (format nil "<< /Length ~d >>~%stream~%~a~%endstream" (length data) data)))
        (add font "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>")
        ;; the copy says what it is: the revision's own information is in the save before
        (add info (pdf-value-text (list (cons "/Title" (format nil "~a, ~a, each object it draws framed and named"
                                                               (analysis-name analysis) (revision-name revision)))
                                        (cons "/Producer" "palimpsest, a package of Tuatara"))))
        (add open-state (stream-text "q"))
        (add close-state (stream-text "Q"))
        (dolist (page (pages-of pdf revision))
          (when (car cancel) (return-from marked-copy nil))
          (job-check-stop)
          (when (page-number page)
            (let* ((overlay next)
                   (dict (page-dict page))
                   (contents (let ((c (pdf-get dict "/Contents"))) (if (pdf-ref-p c) (list c) (and (listp c) (remove-if-not #'pdf-ref-p c)))))
                   (resources (let ((r (deref pdf revision (page-resources page)))) (if (pdf-dictionary-p r) r '())))
                   (fonts (let ((f (deref pdf revision (pdf-get resources "/Font")))) (if (pdf-dictionary-p f) f '())))
                   (new-resources (acons "/Font" (append (remove "/PalimpsestLabel" fonts :key #'car :test #'string=)
                                                         (list (cons "/PalimpsestLabel" (list :ref font 0))))
                                         (remove "/Font" resources :key #'car :test #'string=)))
                   (new-dict (append (remove-if (lambda (pair) (member (car pair) '("/Contents" "/Resources") :test #'string=)) dict)
                                     (list (cons "/Resources" new-resources)
                                           (cons "/Contents" (append (list (list :ref open-state 0)) contents
                                                                     (list (list :ref close-state 0) (list :ref overlay 0))))))))
              (incf next)
              (add overlay (stream-text (overlay-operators pdf revision page :scans scans :copies copies)))
              (add (page-number page) (pdf-value-text new-dict)))))
        ;; the objects, a cross-reference table for them, a trailer going back to the revision's: the
        ;; save added after the revision's octets, the PDF itself copied once, into the copy written
        (let ((added (make-array 4096 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
              (offsets '()))
          ;; written as Latin-1, an octet a character, as PDF-OBJECT-AT read it
          (flet ((put (text) (loop for c across text
                                   do (vector-push-extend (let ((code (char-code c))) (if (< code 256) code 63)) added)))
                 (at () (+ end (fill-pointer added))))
            (unless (member (aref octets (1- end)) '(10 13)) (put (string #\Newline)))
            (dolist (object (reverse objects))
              (push (cons (car object) (at)) offsets)
              (put (format nil "~d 0 obj~%~a~%endobj~%" (car object) (cdr object))))
            (let ((xref (at)))
              (put (format nil "xref~%0 1~%0000000000 65535 f~c~c" #\Return #\Newline))
              (dolist (entry (sort (copy-list offsets) #'< :key #'car))
                (put (format nil "~d 1~%~10,'0d 00000 n~c~c" (car entry) (cdr entry) #\Return #\Newline)))
              (put (format nil "trailer~%<< /Size ~d ~{~a~^ ~} /Prev ~d >>~%startxref~%~d~%%%EOF~%"
                           next
                           (cons (format nil "/Info ~d 0 R" info)
                                 (loop for key in '("/Root" "/ID")
                                       for value = (pdf-get trailer key)
                                       when value collect (format nil "~a ~a" (pdf-name-text key) (pdf-value-text value))))
                           (revision-startxref revision) xref))))
          (ensure-directories target)
          (let ((out (make-array (+ end (fill-pointer added)) :element-type '(unsigned-byte 8))))
            (replace out octets :end2 end)
            (replace out added :start1 end)
            (write-new-file target out))
          target)))))

;;; The command

(defun report-drawing-window (report)
  "The window showing a copy REPORT drew (PALIMPSEST-DRAW-OBJECTS), when one does."
  (let ((folder (let ((analysis (buffer-local report 'analysis))) (and analysis (analysis-folder analysis)))))
    (and folder
         (loop for buffer in (buffer-list)
               for file = (shown-file buffer)
               thereis (and file (path-within-p file folder)
                            (search "objects-revision-" (native-file-name file))
                            (buffer-window buffer))))))

(define-command palimpsest-quit ()
  "In a report: close the copy with its objects drawn when it is shown
\(\<palimpsest-mode>\[palimpsest-draw-objects]); else put the window back as \[quit-window] does, the report
kept, shown again by \[palimpsest] on the same PDF."
  (let ((drawing (report-drawing-window (current-buffer))))
    (if drawing
        (leave-window drawing)
        (quit-window))))

(defstruct (palimpsest-draw-job (:include job) (:constructor make-palimpsest-draw-job (buffer analysis revision target)) (:copier nil))
  "Writing a copy of REVISION of ANALYSIS's PDF with its objects framed and
named, as TARGET, for BUFFER, the report, a job; DONE, true once written;
KEPT, true for a copy written where asked, to keep, rather than shown."
  analysis revision target done kept)

(setf (documentation 'palimpsest-draw-job-p 'function) "True when OBJECT is a PALIMPSEST-DRAW-JOB.")

(define-job-words palimpsest-draw-job
  :doing "Drawing the objects"
  :description (format nil "Framing the objects of ~a" (native-file-name (palimpsest-draw-job-target job)))
  :verb "draw the objects"
  :left-words "no copy shown")

(defun shown-beside (path)
  "The window showing the copy at PATH, when one does."
  (let ((buffer (find-if (lambda (b) (equal (shown-file b) path)) (buffer-list))))
    (and buffer (buffer-window buffer))))

(defun show-beside (path)
  "Show the PDF at PATH in Tuatara's PDF view in a window of its own down the
right, made for it, which \\`q' there closes; the report kept where it is,
selected. Shown already, it is left where it is."
  (pdf-view-visit path (lambda (buffer)
                         (unless (buffer-window buffer)
                           (or (display-buffer-at buffer :right)
                               (display-buffer buffer :other t))))))

(defmethod job-take-in ((job palimpsest-draw-job))
  "Show the copy written beside the report, and say what it shows; or, for one
written where asked (PALIMPSEST-WRITE-DRAWING), say where; or why it was not made."
  (cond ((not (palimpsest-draw-job-done job))
         (message "The objects were not drawn: ~a" (job-ending-words job)))
        ((palimpsest-draw-job-kept job)
         (message "Wrote ~a: ~a's ~a with each thing its pages draw framed and named by its object"
                  (abbreviated-path (palimpsest-draw-job-target job)) (analysis-name (palimpsest-draw-job-analysis job))
                  (revision-name (palimpsest-draw-job-revision job))))
        (t
         (show-beside (palimpsest-draw-job-target job))
         (message "~a's ~a with each thing its pages draw framed, the number of the object drawing it beside it: each content stream a colour of its own, annotations dashed"
                  (analysis-name (palimpsest-draw-job-analysis job)) (revision-name (palimpsest-draw-job-revision job))))))

(define-command palimpsest-draw-objects ()
  "Show beside the report, in a window of its own that \\`q' there closes, a
copy of the PDF with each thing its pages draw
framed and named by the object that draws it: every line of text (each part
of a line a content stream or form of its own draws, framed apart), every
shape and picture, every annotation; each content stream a colour of its
own, so that one a later save added stands out. The revision whose part of
the report point is in, else the latest. The copy is that revision with one
save more, the drawing, written to the report's own folder for you alone,
never beside the PDF examined, drawn afresh each time; the PDF itself is
never changed. On a scanned page, the words unlike the rest (as the
findings name them) are framed in red with how they are unlike. Shown
already, \\<palimpsest-mode>\\[palimpsest-draw-objects] closes it again; the report stays selected throughout."
  (let* ((report (current-buffer))
         (analysis (buffer-local report 'analysis)))
    (if (null analysis)
        (message "Not a report of palimpsest: ~a there first" (substitute-command-keys "\\[palimpsest]"))
        (let* ((pdf (analysis-pdf analysis))
               (revision (or (report-revision-at analysis) (last-revision pdf)))
               (target (path-join (analysis-folder analysis) (format nil "objects-revision-~d.pdf" (1+ (revision-index revision))))))
          (cond ((shown-beside target)
                 ;; shown already: d puts it away again
                 (leave-window (shown-beside target)))
                (t
                 ;; drawn afresh each time, never a copy an earlier palimpsest drew, nor its view kept
                 (dolist (buffer (buffer-list))
                   (when (equal (shown-file buffer) target) (kill-buffer buffer)))
                 (when (path-kind target) (remove-file target))
              (start-job-thread (make-palimpsest-draw-job report analysis revision target)
                                (lambda (job)
                                  ;; done only when the copy was written, not stopped short
                                  (setf (palimpsest-draw-job-done job)
                                        (and (marked-copy analysis revision target :cancel (job-cancel job)) t)))
                                :name "tuatara palimpsest objects")))))))

(define-command palimpsest-write-drawing ()
  "Write the copy with each object its pages draw framed and named
\(\\<palimpsest-mode>\\[palimpsest-draw-objects]) to a PDF of its own, to keep or show to others: of the
revision whose part of the report point is in, else the latest. Asked
where, offered in the folder last saved to, else in the home folder; never
beside the PDF examined nor in the package's own folder, nothing written
over. The copy is drawn afresh, the PDF examined never changed."
  (let* ((report (current-buffer))
         (analysis (buffer-local report 'analysis)))
    (if (null analysis)
        (message "Not a report of palimpsest: ~a there first" (substitute-command-keys "\\[palimpsest]"))
        (let* ((revision (or (report-revision-at analysis) (last-revision (analysis-pdf analysis))))
               (offered (offered-path analysis (format nil "~a-objects-revision-~d.pdf" (native-file-stem (analysis-name analysis))
                                                       (1+ (revision-index revision)))))
               (target (read-new-native-path (format nil "Write the objects of ~a to: " (revision-name revision)) :initial offered)))
          (when target
            (let ((refusal (or (folder-refusal analysis (parent-path target))
                               (and (path-kind target :follow nil)
                                    (format nil "~a is there already: nothing is written over" (native-file-name target))))))
              (if refusal
                  (message "Not written to ~a: ~a" (abbreviated-path target) refusal)
                  (let ((job (make-palimpsest-draw-job report analysis revision target)))
                    (setf (palimpsest-draw-job-kept job) t)
                    (start-job-thread job
                                      (lambda (job)
                                        (setf (palimpsest-draw-job-done job)
                                              (and (marked-copy analysis revision target :cancel (job-cancel job)) t)))
                                      :name "tuatara palimpsest objects written")))))))))
