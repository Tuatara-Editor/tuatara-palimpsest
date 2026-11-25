;;;; repeats.lisp --- A signature, a stamp or a seal the same pixel for pixel in two places, in one file or several.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; No hand signs its name twice alike, and no stamp presses its ink twice
;;;; alike: each signature and each stamp on paper differs from the others in
;;;; its strokes, and a scanner lays its grain over every one. A signature or
;;;; a stamp that is the same in two places was copied from one to the other.
;;;; Pictures a page draws are set beside one another by their octets, and the
;;;; marks of scanned pages larger than their type by their pixels, the pairs
;;;; alike in coarse parts first and only those laid over each other pixel for
;;;; pixel. The same is done for the pictures and marks of several files.

(in-package #:palimpsest)

;;; Pictures drawn in more than one place

(defparameter *repeated-picture-least* '(16 24 12)
  "How large a picture must be to be set beside the others drawn the same
\(REPEATED-PICTURES): as (PIXELS WIDE HIGH), PIXELS the fewest it has across
and down, WIDE and HIGH the fewest points it is drawn across and down. A
bullet, a box to tick or a rule kept as a picture is smaller.")

(defparameter *repeated-pictures-listed* 20
  "The most pictures drawn in more than one place a finding lists.")

(defun picture-places (pdf revision)
  "The pictures the pages of REVISION of PDF draw that are no scans
\(SCAN-PICTURE-P) and are as large as *REPEATED-PICTURE-LEAST* says, each
time one is drawn, as (PAGE MARK IDENTITY), IDENTITY what it draws
\(PICTURE-IDENTITY), worked out once for each object. Pictures hidden in a
layer that is off, or clipped away whole, are left out."
  (let ((identities (make-hash-table)))
    (destructuring-bind (pixels wide high) *repeated-picture-least*
      (loop for page in (pages-of pdf revision)
            append (loop for m across (marks-of pdf revision page)
                         when (and (eq (mark-kind m) :image) (mark-object m) (not (mark-strips m))
                                   (not (mark-hidden m)) (not (eq (mark-clipped m) :whole))
                                   (>= (mark-width m) wide) (>= (mark-height m) high)
                                   (not (scan-picture-p page m)))
                           append (let* ((object (revision-object pdf revision (mark-object m)))
                                         (dict (and object (pdf-indirect-get object :value)))
                                         (across (and (pdf-dictionary-p dict)
                                                      (deref pdf revision (pdf-get dict "/Width"))))
                                         (down (and across (deref pdf revision (pdf-get dict "/Height")))))
                                    (when (and (integerp across) (integerp down) (>= across pixels) (>= down pixels))
                                      (let ((identity (multiple-value-bind (known found)
                                                          (gethash (mark-object m) identities)
                                                        (if found
                                                            known
                                                            (setf (gethash (mark-object m) identities)
                                                                  (picture-identity pdf revision (mark-object m)))))))
                                        (and identity (list (list page m identity)))))))))))

(defun place-of-drawing (page mark)
  "Where MARK is drawn on PAGE, (PAGE-INDEX X Y), its corner rounded to the
point, by which two drawings of one picture are told to be in one place."
  (list (page-index page) (round (mark-x0 mark)) (round (mark-y0 mark))))

(defun picture-groups (pdf revision)
  "The pictures of REVISION of PDF that are no scans (PICTURE-PLACES), the
drawings of each that draw the same (PICTURE-IDENTITY) gathered into one
group, whether drawn by one object or by several: a list of groups, each a
list of (PAGE MARK IDENTITY), one for each place, in the order of the pages;
the groups of the most places first."
  (let ((by-identity (make-hash-table :test 'equal)) (groups '()))
    (dolist (entry (picture-places pdf revision))
      (push entry (gethash (third entry) by-identity)))
    (loop for entries being the hash-values of by-identity
          do (push (remove-duplicates (reverse entries) :key (lambda (e) (place-of-drawing (first e) (second e)))
                                                        :test #'equal :from-end t)
                   groups))
    (stable-sort (sort groups #'< :key (lambda (g) (page-index (first (first g))))) #'> :key #'length)))

(defun repeated-pictures (pdf revision)
  "The groups of PICTURE-GROUPS of REVISION of PDF drawn in more than one place."
  (remove-if-not #'rest (picture-groups pdf revision)))

(defun picture-group-words (group)
  "A group of drawings of one picture (REPEATED-PICTURES) in words: the
picture, its size in pixels, the objects keeping it when more than one, and
each place it is drawn."
  (let* ((m (second (first group)))
         (objects (remove-duplicates (mapcar (lambda (e) (mark-object (second e))) group))))
    (format nil "~a (object ~d)~@[, ~a~]~@[, kept as ~a~]: ~{~a~^; ~}"
            (mark-text m) (mark-object m)
            (let ((dict (mark-actual m)))
              (and (pdf-dictionary-p dict) (integerp (pdf-get dict "/Width")) (integerp (pdf-get dict "/Height"))
                   (format nil "~d by ~d pixels" (pdf-get dict "/Width") (pdf-get dict "/Height"))))
            (and (rest objects) (format nil "~d objects, ~{~d~^, ~}" (length objects) objects))
            (loop for (page mark) in group
                  collect (format nil "page ~d at x ~d, y ~d" (1+ (page-index page)) (round (mark-x0 mark))
                                  (round (mark-y0 mark)))))))

(defun repeated-pictures-finding (pdf revision groups)
  "The finding, for the record, that the pictures of GROUPS (REPEATED-PICTURES)
of REVISION of PDF are each drawn in more than one place."
  (let ((objects (remove-duplicates (loop for g in groups append (mapcar (lambda (e) (mark-object (second e))) g)))))
    (found :record
           (format nil "~:d picture~:p ~:*~[~;is~:;are~] each drawn in more than one place, the same octet for octet: ~
                        a signature, a stamp, a seal or a logo laid on as a picture, as an office's letters and ~
                        forms honestly do, or one copied where it should differ."
                   (length groups))
           :rule (concatenate 'string "No hand signs twice alike and no stamp presses twice alike, but a picture is "
                              "the same every time it is drawn. Pictures that are no scans, 16 by 16 pixels or more "
                              "and drawn 24 by 12 points or more, are set beside one another by their octets and "
                              "the entries that say how they are drawn; each kept the same in two places or more, "
                              "by one object or by several, is listed with where it is drawn. Whether the "
                              "signatures should differ, as two signers' do, is for the examiner to judge.")
           :revision (revision-index revision)
           :page (1+ (page-index (first (first (first groups)))))
           :tally (list (length groups) "picture")
           :objects objects
           :details (append (loop for g in groups repeat *repeated-pictures-listed* collect (picture-group-words g))
                            (and (> (length groups) *repeated-pictures-listed*)
                                 (list (format nil "and ~:d more" (- (length groups) *repeated-pictures-listed*)))))
           :evidence (pictures-evidence pdf revision (subseq objects 0 (min 6 (length objects))))
           :method :picture-pasted)))

;;; Marks set beside one another

(defparameter *same-mark-ink-least* '(0.97 . 0.97)
  "The least share of the pixels inked in either of two marks laid over each
other that must be inked in both for them to be the same (SAME-MARK-P), as
\(KEPT-WITHOUT-LOSS . AFTER-A-JPEG). A copy shares all of its ink, and after
a JPEG's rounding of quality 75 or better 97% or more. Two scans of one print
whose pixels fall a fifth of a pixel apart on its strokes, or turned a
quarter of a degree, share 92% to 94%, and half a pixel or half a degree
apart 80% to 85%; two signatures by one hand far less. Crisp printed marks
of honest scans kept as JPEGs, a form's large letters or a heading's logo on
each page, came to 95% and 96%.")

(defparameter *same-mark-spread-most* '(1.0 . 15.0)
  "The most the grey levels of two marks laid over each other may differ, over
the pixels inked in both, their mean difference taken out (the spread,
SAME-MARK-P), for them to be the same, as (KEPT-WITHOUT-LOSS . AFTER-A-JPEG).
A copy kept without loss is the same pixel for pixel, or the same but for how
it was laid on (multiplied with the paper: a grey level or less), where two
scans of one print stray by the scanner's grain, 2 grey levels and more. A
copy saved as a JPEG of quality 75 strays by about 6 to 9, 12 when both
places were (21 at quality 50, which is missed); two scans of one print whose
pixels fall a fifth of a pixel apart on its strokes, or turned a quarter of
a degree, stray by 24 to 25, and printed marks of honest scans alike in
their share of ink up to 23.")

(defparameter *mark-coarse-most* 640
  "The most the coarse parts of two marks (INK-MARK-COARSE) may differ, summed,
for them to be laid over each other pixel by pixel (ALIKE-MARKS).")

(defparameter *mark-tries-each* 4
  "How many marks, the nearest in their coarse parts, each mark is laid over
pixel by pixel at most (ALIKE-MARKS).")

(defparameter *mark-tries-most* 4000
  "The most pairs of marks laid over each other pixel by pixel in one search
\(ALIKE-MARKS); the rest are counted as not tried.")

(defparameter *mark-pixels-read-most* (* 2000 1000 1000)
  "The most pixels the pairs of marks laid over each other in one search may
read among them (ALIKE-MARKS, MARK-LIKENESS-COST), a few seconds' work; the
pairs past it are counted as not tried.")

(defparameter *mark-candidates-most* 64
  "The most marks of about its size each mark is set beside by its coarse
parts (ALIKE-MARKS); a page of many marks of one size (a logo on each of
hundreds of pages) would make that search grow as the square of their
number. The rest are counted as not tried.")

(defun mark-likeness (a b &key (step 1) (dx 0) (dy 0) (reach 3))
  "How alike the ink of marks A and B is laid over each other, darker than the
lighter of their levels of ink taken for both, B moved by DX and DY, as four
values: the share of the pixels inked in either that are inked in both, the
spread of their grey difference over the pixels inked in both (its root
mean square, its mean taken out), the mean difference, and the shift as (DX
. DY). STEP above 1 reads every STEPth row and column alone. With REACH above
0 the shift is the one that lays the most of their ink over each other: the
best of laying the first corners of their boxes together, their last
corners or the middles of their ink is found on every fourth row and
column, then the shifts within REACH pixels of it so, then those a pixel
about the best on every second row, and the best of all is read whole."
  (when (plusp reach)
    (flet ((best (shifts every)
             (let ((best -1.0) (at '(0 . 0)))
               (dolist (shift (remove-duplicates shifts :test #'equal) at)
                 (let ((share (mark-likeness a b :step every :dx (car shift) :dy (cdr shift) :reach 0)))
                   (when (> share best) (setf best share at shift)))))))
      (let* ((centre (best (list '(0 . 0)
                                 (cons (- (ink-mark-x1 b) (ink-mark-x0 b) (- (ink-mark-x1 a) (ink-mark-x0 a)))
                                       (- (ink-mark-y1 b) (ink-mark-y0 b) (- (ink-mark-y1 a) (ink-mark-y0 a))))
                                 (cons (round (- (car (ink-mark-middle b)) (car (ink-mark-middle a))))
                                       (round (- (cdr (ink-mark-middle b)) (cdr (ink-mark-middle a))))))
                           4))
             (near (flet ((around (at r)
                            (loop for y from (- (cdr at) r) to (+ (cdr at) r)
                                  append (loop for x from (- (car at) r) to (+ (car at) r) collect (cons x y)))))
                     (best (around (best (around centre reach) 4) 1) 2))))
        (return-from mark-likeness (mark-likeness a b :step step :dx (car near) :dy (cdr near) :reach 0)))))
  (job-check-stop)
  (let* ((pa (ink-mark-pixels a)) (pb (ink-mark-pixels b))
         (wa (ink-mark-across a)) (wb (ink-mark-across b)) (ha (floor (length pa) wa)) (hb (floor (length pb) wb))
         (level (min (ink-mark-level a) (ink-mark-level b)))
         (either 0) (both 0) (n 0) (sum 0) (squares 0))
    (declare (type (simple-array (unsigned-byte 8) (*)) pa pb)
             (type fixnum wa wb ha hb level either both n sum squares step dx dy))
    (loop for j of-type fixnum from 1 below (1- ha) by step
          for jb of-type fixnum = (+ j dy)
          do (loop for i of-type fixnum from 1 below (1- wa) by step
                   for ib of-type fixnum = (+ i dx)
                   do (let* ((ga (aref pa (+ (* j wa) i)))
                             (gb (if (and (< -1 ib wb) (< -1 jb hb)) (aref pb (+ (* jb wb) ib)) 255))
                             (ink-a (< ga level)) (ink-b (< gb level)))
                        (declare (optimize speed))
                        (when (or ink-a ink-b)
                          (incf either)
                          (when (and ink-a ink-b)
                            (incf both)
                            (let ((d (- ga gb))) (incf n) (incf sum d) (incf squares (* d d))))))))
    (let ((mean (if (plusp n) (/ sum n) 0)))
      (values (if (plusp either) (float (/ both either) 1.0) 0.0)
              (if (plusp n) (sqrt (max 0.0 (float (- (/ squares n) (* mean mean)) 1.0))) 0.0)
              (float mean 1.0)
              (cons dx dy)))))

(defun mark-likeness-cost (mark)
  "About how many pixels laying MARK over another (MARK-LIKENESS) reads: its
box read some nine times over, at every fourth and every second row and
whole."
  (* 9 (length (ink-mark-pixels mark))))

(defun same-scan-place-p (a b)
  "True when the marks A and B lie in one place of one scan kept twice, not a
mark copied: the paper round them (INK-MARK-RING), where both reach, is alike
where it holds enough to tell by. Kept without loss, its grain is enough: a
grey level or more of it, the same within a grey level and half the grain.
After a JPEG, which smooths grain away, it must hold ink, straying by 8 grey
levels or more, and the two must stray by a quarter of that or less. A
signature copied with the paper about it seldom reaches so far, and the
paper of two scans differs."
  (let ((ra (ink-mark-ring a)) (rb (ink-mark-ring b)) (n 0) (sum 0) (squares 0) (own 0) (own-squares 0))
    (dotimes (k (length ra))
      (let ((ga (aref ra k)) (gb (aref rb k)))
        (when (and (>= ga 0) (>= gb 0))
          (let ((d (- ga gb)))
            (incf n) (incf sum d) (incf squares (* d d)) (incf own ga) (incf own-squares (* ga ga))))))
    (and (>= n (floor (length ra) 4))
         (let ((spread (sqrt (max 0 (- (/ squares n) (expt (/ sum n) 2)))))
               (detail (sqrt (max 0 (- (/ own-squares n) (expt (/ own n) 2))))))
           (if (or (ink-mark-lossy a) (ink-mark-lossy b))
               (and (>= detail 8) (<= spread (/ detail 4)))
               (and (>= detail 1) (<= spread (min 1 (/ detail 2)))))))))

(defun same-mark-p (a b)
  "Whether marks A and B are the same picture, as two values: true when, laid
over each other at the shift that suits them best (MARK-LIKENESS), the share
of their ink inked in both is *SAME-MARK-INK-LEAST* or more and the spread
of their grey difference *SAME-MARK-SPREAD-MOST* or less, the looser of each
when either was kept as a JPEG, and they are not one place of one scan kept
twice (SAME-SCAN-PLACE-P); and their likeness, as (SHARE SPREAD MEAN). On a
page whose grain was not judged (SCAN-PICTURE-JUDGED: its prints of a
character come out too alike to tell apart, as a page drawn by a program and
given grain does), two prints of one shape come out alike too, and the marks
must be the same in every pixel inked in both, 99% of the ink of either
inked in both. A mark of no scan, read alone, is taken as judged."
  (multiple-value-bind (share spread mean) (mark-likeness a b)
    (let ((lossy (or (ink-mark-lossy a) (ink-mark-lossy b)))
          (judged (every (lambda (m) (let ((scan (ink-mark-scan m))) (or (null scan) (scan-picture-judged scan))))
                         (list a b))))
      (values (and (if judged
                       (and (>= share (if lossy (cdr *same-mark-ink-least*) (car *same-mark-ink-least*)))
                            (<= spread (if lossy (cdr *same-mark-spread-most*) (car *same-mark-spread-most*))))
                       (and (>= share 0.99) (zerop spread)))
                   (not (same-scan-place-p a b)))
              (list share spread mean)))))

(defun coarse-distance (a b)
  "How far apart the coarse parts of marks A and B are, summed (INK-MARK-COARSE)."
  (let ((ca (ink-mark-coarse a)) (cb (ink-mark-coarse b)))
    (loop for k below 64 sum (abs (- (aref ca k) (aref cb k))))))

(defun mark-near-p (a b)
  "True when marks A and B may be the same and are to be laid over each other:
their boxes of ink within 4 pixels or a fiftieth of each other's size, their
ink within a fifth, their coarse parts within *MARK-COARSE-MOST*. The faint
ends of a stroke, near the level of ink, may reach a few pixels further on one
page than on another."
  (flet ((near (p q) (<= (abs (- p q)) (max 4 (ceiling (max p q) 50)))))
    (and (near (- (ink-mark-x1 a) (ink-mark-x0 a)) (- (ink-mark-x1 b) (ink-mark-x0 b)))
         (near (- (ink-mark-y1 a) (ink-mark-y0 a)) (- (ink-mark-y1 b) (ink-mark-y0 b)))
         (<= (* 5 (abs (- (ink-mark-ink a) (ink-mark-ink b)))) (max (ink-mark-ink a) (ink-mark-ink b)))
         (<= (coarse-distance a b) *mark-coarse-most*))))

(defun alike-marks (marks apart)
  "The pairs of MARKS (INK-MARKs) that are the same picture (SAME-MARK-P), APART
called with two marks true for those that may be named, each (A B SHARE
SPREAD MEAN), and how many pairs were left untried past the bounds as a
second value (those past the nearest of each are not counted). The
marks are kept by the size of their box, and each is set beside the first
*MARK-CANDIDATES-MOST* of about its size (MARK-NEAR-P) and laid over the
*MARK-TRIES-EACH* nearest in their coarse parts, *MARK-TRIES-MOST* pairs and
*MARK-PIXELS-READ-MOST* pixels read in all."
  (let ((by-size (make-hash-table :test 'equal)) (tried (make-hash-table :test 'equal)) (pairs '()) (left 0)
        (tries 0) (read 0) (index (make-hash-table :test 'eq)))
    (flet ((size-key (m dx dy)
             ;; steps of 4% of the size and 64 pixels more, so that sizes MARK-NEAR-P holds near are a step apart
             (flet ((size-step (size) (floor (log (+ size 64) 1.04))))
               (list (+ dx (size-step (- (ink-mark-x1 m) (ink-mark-x0 m))))
                     (+ dy (size-step (- (ink-mark-y1 m) (ink-mark-y0 m)))))))
           (in-order (a b) (if (<= (gethash a index) (gethash b index)) (values a b) (values b a))))
      (loop for m in marks for k from 0
            do (setf (gethash m index) k) (push m (gethash (size-key m 0 0) by-size)))
      (dolist (a marks)
        (job-check-stop)
        (let ((seen 0) (near '()))
          (loop for dx from -1 to 1
                do (loop for dy from -1 to 1
                         do (dolist (b (gethash (size-key a dx dy) by-size))
                              (when (and (not (eq a b)) (funcall apart a b))
                                (cond ((< seen *mark-candidates-most*)
                                       (incf seen)
                                       (when (mark-near-p a b) (push (cons (coarse-distance a b) b) near)))
                                      ((> (gethash b index) (gethash a index)) (incf left)))))))
          (setf near (sort near #'< :key #'car))
          (loop for (nil . b) in near repeat *mark-tries-each*
                for key = (let ((i (gethash a index)) (j (gethash b index))) (cons (min i j) (max i j)))
                unless (gethash key tried)
                  do (setf (gethash key tried) t)
                     (if (or (>= tries *mark-tries-most*)
                             (> (+ read (mark-likeness-cost a)) *mark-pixels-read-most*))
                         (incf left)
                         (progn (incf tries)
                                (incf read (mark-likeness-cost a))
                                (multiple-value-bind (same likeness) (same-mark-p a b)
                                  (when same
                                    (multiple-value-bind (first second) (in-order a b)
                                      (push (list* first second likeness) pairs))))))))))
    (values (nreverse pairs) left)))

(defun marks-laid-out-p (a b)
  "True when marks A and B are on one scanned picture and their boxes meet,
or stand in one row or one column, their first corners level or above each
other within a pixel: the boxes, figures and large letters of a printed form
or a page set by a program, printed alike, not a signature copied by hand."
  (and (eq (ink-mark-scan a) (ink-mark-scan b))
       (or (<= (abs (- (ink-mark-x0 a) (ink-mark-x0 b))) 1)
           (<= (abs (- (ink-mark-y0 a) (ink-mark-y0 b))) 1)
           (and (< (max (ink-mark-x0 a) (ink-mark-x0 b)) (min (ink-mark-x1 a) (ink-mark-x1 b)))
                (< (max (ink-mark-y0 a) (ink-mark-y0 b)) (min (ink-mark-y1 a) (ink-mark-y1 b)))))))

(defun mark-place-words (mark)
  "Where MARK, an INK-MARK, is, in words: its file when it has one, its page
and its place on it (PIXELS-PLACE), or the place it keeps."
  (let ((scan (ink-mark-scan mark)))
    (format nil "~@[~a, ~]~a" (and (ink-mark-source mark) (escape-controls (ink-mark-source mark)))
            (or (ink-mark-place mark)
                (format nil "page ~d ~a" (1+ (page-index (scan-picture-page scan)))
                        (pixels-place (scan-picture-mark scan) (ink-mark-x0 mark) (ink-mark-y0 mark) (ink-mark-x1 mark)
                                      (ink-mark-y1 mark) (scan-picture-width scan) (scan-picture-height scan)))))))

(defun mark-pair-words (pair)
  "A pair of marks the same (ALIKE-MARKS), (A B SHARE SPREAD MEAN), in words:
both places, their size and how alike they are."
  (destructuring-bind (a b share spread mean) pair
    (format nil "~a and ~a: ~d by ~d pixels, ~:d of them ink, ~d% of the ink of either inked in both, ~
                 ~:[their grey levels straying by ~,1f~;the same grey in every pixel of their ink~*~]~@[, ~a~]~
                 ~:[~;, kept as a JPEG~]"
            (mark-place-words a) (mark-place-words b)
            (- (ink-mark-x1 a) (ink-mark-x0 a)) (- (ink-mark-y1 a) (ink-mark-y0 a)) (ink-mark-ink a)
            (floor (* 100 share)) (and (zerop spread) (zerop mean)) spread
            (and (>= (abs mean) 0.5)
                 (format nil "~,1f grey levels ~:[lighter~;darker~] in all" (abs mean) (minusp mean)))
            (or (ink-mark-lossy a) (ink-mark-lossy b)))))

(defparameter *mark-pairs-listed* 20
  "The most pairs of marks the same a finding lists.")

(defun same-marks-finding (pdf revision pairs left)
  "The finding, worth a look, that PAIRS of marks of the scanned pages of
REVISION of PDF are the same (ALIKE-MARKS), LEFT pairs not tried."
  (let* ((scans (remove-duplicates (loop for (a b) in pairs collect (ink-mark-scan a) collect (ink-mark-scan b))))
         (objects (remove-duplicates (loop for s in scans append (ends-objects (scan-picture-mark s)))))
         (pages (sort (remove-duplicates (mapcar (lambda (s) (1+ (page-index (scan-picture-page s)))) scans)) #'<)))
    (found :look
           (same-mark-finding-words pairs (length pages))
           :rule (format nil "No hand signs twice alike and no stamp presses its ink twice alike, and a scanner lays ~
                              its own grain over each print. The marks of the scanned pages twice as tall and as ~
                              wide as their type or more, once the words and the rules are taken out, are set ~
                              beside one another: those of about one size and alike in coarse parts are laid over ~
                              each other pixel by pixel, at the best shift within 3 pixels of laying their ~
                              corners or the middles of their ink together. Two are named the same when ~d% of ~
                              the pixels inked in either are inked in both and their grey levels there, their ~
                              mean difference taken out, stray by ~,1f grey levels or less, on pictures kept without ~
                              loss; on a picture kept as a JPEG, ~d% and ~,1f grey levels. Two places of one scan ~
                              kept twice, whose paper round them is as alike, are not named. A stamp an office's ~
                              software lays on its scans, or a facsimile signature a program prints, is the same ~
                              every time too; whether the marks should differ is for the examiner to judge."
                         (round (* 100 (car *same-mark-ink-least*))) (car *same-mark-spread-most*)
                         (round (* 100 (cdr *same-mark-ink-least*))) (cdr *same-mark-spread-most*))
           :revision (revision-index revision) :page (first pages) :objects objects
           :tally (list (length pairs) "pair" pages)
           :details (append (loop for p in pairs repeat *mark-pairs-listed* collect (mark-pair-words p))
                            (and (> (length pairs) *mark-pairs-listed*)
                                 (list (format nil "and ~:d pairs more" (- (length pairs) *mark-pairs-listed*))))
                            (and (plusp left)
                                 (list (format nil "~:d pairs of marks of about one size were not laid over each ~
                                                    other, past the most tried" left))))
           :evidence (pictures-evidence pdf revision (subseq objects 0 (min 6 (length objects))))
           :method :picture-pasted :kind :marks-twice)))

(defun file-marks (pdf revision scans)
  "The marks of SCANS, the SCAN-PICTUREs of REVISION of PDF, to set beside one
another: those of the pictures kept once, read as scans (not drawn by a
program) and not kept as JBIG2, whose encoder may draw alike shapes from one
symbol."
  (loop for scan in scans
        unless (or (scan-picture-twin scan) (scan-picture-drawn scan) (scan-picture-rendered scan)
                   (not (listp (scan-picture-marks scan)))
                   (some (lambda (n) (member "/JBIG2Decode" (picture-filters pdf revision n) :test #'equal))
                         (mark-objects (scan-picture-mark scan))))
          append (scan-picture-marks scan)))

(defun same-mark-finding-words (pairs pages)
  "What the finding of PAIRS of marks the same says, the marks on PAGES pages."
  (format nil "~:d pair~:p of marks on the scanned page~p, as large as a signature or a stamp, ~:[is~;are~] one ~
               picture in two places, the same pixel for pixel or but for a JPEG's rounding: copied from one place ~
               to the other, as no hand signs and no stamp presses twice alike."
          (length pairs) pages (rest pairs)))

(defun check-repeats (pdf revision)
  "The pictures of REVISION of PDF drawn in more than one place, for the record
\(REPEATED-PICTURES), and the marks of its scanned pages as large as a
signature or a stamp that are the same pixel for pixel in two places, worth a
look (ALIKE-MARKS)."
  (let ((out '()) (groups (repeated-pictures pdf revision)))
    (when groups (push (repeated-pictures-finding pdf revision groups) out))
    (let ((scans (scan-reading-scans (scans-read pdf revision))))
      (dolist (scan scans)
        (when (typep (scan-picture-marks scan) 'condition)
          (push (unfinished (format nil "the large marks of page ~d" (1+ (page-index (scan-picture-page scan))))
                            (scan-picture-marks scan))
                out)))
      (let ((let-go (loop for scan in scans
                          when (eq (scan-picture-marks scan) :let-go)
                            collect (1+ (page-index (scan-picture-page scan))))))
        (when let-go
          (push (found :record (format nil "The marks as large as a signature or a stamp of ~:d scanned page~:p ~
                                            (~{~d~^, ~}) were not set beside the others: the file's marks would ~
                                            keep more than ~:d pixels."
                                       (length let-go) (remove-duplicates let-go) *marks-pixels-in-all*)
                       :rule (concatenate 'string "The pixels the marks of a file keep are bounded; past the "
                                          "bound a page's marks are let go.")
                       :revision (revision-index revision) :page (first let-go))
                out)))
      (multiple-value-bind (pairs left)
          (alike-marks (file-marks pdf revision scans) (lambda (a b) (not (marks-laid-out-p a b))))
        (cond (pairs (push (same-marks-finding pdf revision pairs left) out))
              ((plusp left)
               (push (found :record (format nil "~:d pair~:p of marks as large as a signature or a stamp ~
                                                 ~:*~[were~;was~:;were~] not laid over each other, past the most ~
                                                 tried: the file was not wholly searched for one copied."
                                            left)
                            :rule "The pairs of marks laid over each other, and the pixels they read, are bounded."
                            :revision (revision-index revision))
                     out)))))
    (nreverse out)))

;;; Pictures and marks across files

(defparameter *compared-files-most* 200
  "The most PDFs of a directory \\[palimpsest-compare-files] sets beside one another.")

(defparameter *compared-marks-pixels-most* (* 400 1000 1000)
  "The most pixels the marks of all the files \\[palimpsest-compare-files] sets
beside one another may keep among them; the marks of a file that would take
them past it are not kept, and the list says so.")

(defstruct (compared-file (:copier nil) (:predicate nil))
  "A PDF set beside others (FILES-COMPARED): NAME and PATH, or for one read
shut in its place among those compared, from 0; PICTURES, its
pictures that are no scans, each (IDENTITY WORDS), IDENTITY what the picture
draws (PICTURE-IDENTITY) and WORDS where it is drawn (PICTURE-GROUP-WORDS);
MARKS, its scanned marks as large as a signature
or a stamp (INK-MARKs); WHY, in words, why it could not be read or its marks
were not kept, or NIL."
  name path pictures marks why)

(defun compared-file (path &optional (room *compared-marks-pixels-most*))
  "The PDF at PATH read for setting beside others, as a COMPARED-FILE: its
pictures (PICTURE-GROUPS) and the marks of its scanned pages (FILE-MARKS),
those let go when their pixels pass ROOM. Its workings are let go after."
  (let ((name (native-file-name path)))
    (handler-case
        (let* ((pdf (read-pdf (source-octets path) :path path :name name))
               (revision (last-revision pdf))
               (file (make-compared-file
                      :name name :path path
                      :pictures (loop for group in (picture-groups pdf revision)
                                      collect (list (third (first group)) (picture-group-words group)))
                      :marks (file-marks pdf revision (scan-reading-scans (scans-read pdf revision)))))
               (kept (make-hash-table :test 'eq)))
          (forget-workings pdf)
          (dolist (m (compared-file-marks file))
            ;; what is needed of its scan alone, not its characters, words and traces
            (let ((scan (ink-mark-scan m)))
              (setf (ink-mark-source m) name
                    (ink-mark-scan m) (or (gethash scan kept)
                                          (setf (gethash scan kept)
                                                (make-scan-picture :page (scan-picture-page scan)
                                                                   :mark (scan-picture-mark scan)
                                                                   :width (scan-picture-width scan)
                                                                   :height (scan-picture-height scan)
                                                                   :judged (scan-picture-judged scan)))))))
          (when (null (pages-of pdf revision))
            (setf (compared-file-why file) "it holds no page that could be read"))
          (when (> (loop for m in (compared-file-marks file) sum (length (ink-mark-pixels m))) room)
            (setf (compared-file-marks file) '()
                  (compared-file-why file) "its marks were not kept, past *compared-marks-pixels-most*"))
          file)
      ((or error storage-condition) (condition)
        (make-compared-file :name name :path path
                            :why (format nil "it could not be read: ~a"
                                         (shorten-string (princ-to-string condition) 200 :one-line t)))))))

(defun files-compared (files)
  "FILES, COMPARED-FILEs, set beside one another, as three values: the
pictures in more than one of them, each a list of (FILE WORDS) for each file
that holds it; the pairs of marks of two files the same (ALIKE-MARKS); and how
many pairs were left untried."
  (let ((sums (make-hash-table :test 'equal)))
    (dolist (file files)
      (loop for (sum words) in (compared-file-pictures file)
            do (push (list file words) (gethash sum sums))))
    (multiple-value-bind (pairs left)
        (alike-marks (loop for file in files append (compared-file-marks file))
                     (lambda (a b) (not (equal (ink-mark-source a) (ink-mark-source b)))))
      (values (sort (loop for held being the hash-values of sums
                          when (rest (remove-duplicates held :key #'first))
                            collect (reverse held))
                    #'> :key #'length)
              pairs left))))

(defun compared-files (paths)
  "The PDFs at PATHS read in this process and set beside one another, as four
values: the COMPARED-FILEs, then what FILES-COMPARED gives."
  (let ((files '()) (room *compared-marks-pixels-most*))
    (dolist (path paths)
      (let ((file (compared-file path room)))
        (decf room (loop for m in (compared-file-marks file) sum (length (ink-mark-pixels m))))
        (push file files)))
    (setf files (nreverse files))
    (multiple-value-call #'values files (files-compared files))))

(defun mark-data (mark)
  "MARK, an INK-MARK of a file read for setting beside others, as data, its
pixels left out: its box, ACROSS, LEVEL, INK, its middle as two fractions
\(numerator and denominator each), COARSE, RING, LOSSY, where it is in words,
whether its scan was judged, and how many pixels it has."
  (destructuring-bind (x . y) (ink-mark-middle mark)
    (list (ink-mark-x0 mark) (ink-mark-y0 mark) (ink-mark-x1 mark) (ink-mark-y1 mark) (ink-mark-across mark)
          (ink-mark-level mark) (ink-mark-ink mark)
          (numerator (rational x)) (denominator (rational x)) (numerator (rational y)) (denominator (rational y))
          (coerce (ink-mark-coarse mark) 'list) (coerce (ink-mark-ring mark) 'list) (and (ink-mark-lossy mark) t)
          (mark-place-words mark) (and (scan-picture-judged (ink-mark-scan mark)) t)
          (length (ink-mark-pixels mark)))))

(defun data-mark (data pixels source)
  "The INK-MARK DATA stands for (MARK-DATA), PIXELS its pixels, SOURCE the name
of its file; NIL when DATA does not fit them, its pixels not rows of ACROSS."
  (destructuring-bind (&optional x0 y0 x1 y1 across level ink xn xd yn yd coarse ring lossy place judged count
                       &rest more)
      data
    (declare (ignore more))
    (when (and (every #'integerp (list x0 y0 x1 y1 across level ink xn xd yn yd count))
               (plusp xd) (plusp yd) (plusp across) (= count (length pixels)) (zerop (mod count across))
               (listp coarse) (= 64 (length coarse)) (every (lambda (c) (typep c '(unsigned-byte 8))) coarse)
               (listp ring) (every (lambda (r) (typep r '(signed-byte 16))) ring)
               (stringp place))
      (make-ink-mark :scan (make-scan-picture :judged judged) :x0 x0 :y0 y0 :x1 x1 :y1 y1 :pixels pixels
                     :across across :level level :ink ink
                     :middle (cons (float (/ xn xd) 1.0) (float (/ yn yd) 1.0))
                     :coarse (coerce coarse '(simple-array (unsigned-byte 8) (*)))
                     :ring (coerce ring '(simple-array (signed-byte 16) (*)))
                     :lossy lossy :source source :place place))))

(defun compared-shut-in (paths directory &key settings room)
  "Read the PDF, the first of PATHS, for setting beside others, as the work
shut in does (COMPARED-FILE, its marks kept within ROOM pixels), and say what
was read as data: its pictures, each (IDENTITY WORDS), IDENTITY printed;
its marks (MARK-DATA),
the pixels of the Kth written to mark-K in DIRECTORY; why it could not be
read or its marks were not kept, or NIL; and the pixels of its marks in all. SETTINGS as SHUT-IN carries them."
  (call-with-settings
   settings
   (lambda ()
     (let ((file (compared-file (first paths) room)))
       (loop for m in (compared-file-marks file) for k from 0
             do (write-new-file (path-join directory (format nil "mark-~d" k)) (ink-mark-pixels m)))
       (list (loop for (identity words) in (compared-file-pictures file)
                   ;; the same picture is the same words in every file
                   collect (list (with-standard-io-syntax (prin1-to-string identity)) words))
             (mapcar (lambda (m) (setf (ink-mark-source m) nil) (mark-data m)) (compared-file-marks file))
             (compared-file-why file)
             (loop for m in (compared-file-marks file) sum (length (ink-mark-pixels m))))))))

(defun compare-shut-in (paths directory &key settings entries words (past 0))
  "Set beside one another the PDFs ENTRIES says were read, each (NAME DATA),
DATA what COMPARED-SHUT-IN said of it, whose marks' pixels are under the
first of PATHS, in a directory for each numbered from 0, as the work shut in
does (FILES-COMPARED), the pixels read within *COMPARED-MARKS-PIXELS-MOST*.
Returns the pieces of the list (COMPARED-PIECES, WORDS naming the directory
compared, PAST the PDFs found and not compared; PIECES-DATA), and how many files, pictures in more than one, and
pairs of marks the same. SETTINGS as SHUT-IN carries them."
  (call-with-settings
   settings
   (lambda ()
     (let* ((room *compared-marks-pixels-most*)
            (files (loop for (name data) in entries for k from 0
                         collect (destructuring-bind (&optional pictures marks why &rest more) data
                                   (declare (ignore more))
                                   (let ((place (path-join (first paths) (format nil "~d/" k))))
                                     (make-compared-file
                                      :name name :path k
                                      :pictures (remove-if-not (lambda (p) (and (listp p) (stringp (first p))
                                                                                (stringp (second p))))
                                                               pictures)
                                      :marks (loop for data in marks for j from 0
                                                   for pixels = (shut-in-octets place (format nil "mark-~d" j)
                                                                                (max 1 room))
                                                   for mark = (and pixels (data-mark data pixels name))
                                                   when mark collect mark and do (decf room (length pixels)))
                                      :why (and (stringp why) why)))))))
       (multiple-value-bind (pictures pairs left) (files-compared files)
         (list (pieces-data (compared-pieces words files pictures pairs left past) directory)
               (list (length files) (length pictures) (length pairs))))))))

(defun compared-pieces (directory files pictures pairs left &optional (past 0))
  "The pieces of the list of what the PDFs of DIRECTORY, a directory's name as
it is shown, share (FILES-COMPARED): FILES, the PICTURES in more than one of
them, the PAIRS of marks the same, LEFT pairs untried. PAST, how many more
PDFs were found and not compared (*COMPARED-FILES-MOST*), is said. Each file's name is a
button, its action (:VISIT N), N its PATH, the place it has among those
compared."
  (collecting-pieces (put)
    (flet ((file-link (file)
             (put (escape-controls (compared-file-name file)) :link (list :visit (compared-file-path file)))))
      (put (format nil "Palimpsest: pictures and marks compared under ~a~%" (escape-controls directory))
           :heading)
      (put (format nil "~:d PDF~:p: ~:d picture~:p in more than one, ~:d pair~:p of marks as large as a signature or a ~
                        stamp the same in two. No hand signs twice alike and no stamp presses twice alike; a picture ~
                        is the same every time it is drawn, as an office's logo or a signature it keeps as a picture ~
                        honestly is. Whether these should differ is for you to judge.~%"
                   (length files) (length pictures) (length pairs)))
      (when (plusp past)
        (put (format nil "Only the first ~:d PDFs were compared (*compared-files-most*); ~:d more found were not.~%"
                     (length files) past)
             :list-note))
      (when pictures
        (put (format nil "~%* The same picture, octet for octet, in more than one file~%") :heading)
        (loop for held in pictures for k from 1
              do (put (format nil "  ~d. In ~:d files:~%" k (length (remove-duplicates held :key #'first)))
                      :list-label)
                 (loop for (file words) in held
                       do (put "    ")
                          (file-link file)
                          (put (format nil ": ~a~%" (escape-controls words)) :list-note))))
      (when pairs
        (put (format nil "~%* Marks the same pixel for pixel in two files~%") :heading)
        (dolist (pair pairs)
          (put (format nil "  ~a~%" (mark-pair-words pair)) :list-note))
        (when (plusp left)
          (put (format nil "  ~:d pairs of marks of about one size were not laid over each other, past the most ~
                            tried~%" left)
               :list-note)))
      (let ((apart (remove-if-not #'compared-file-why files)))
        (when apart
          (put (format nil "~%* Not wholly compared~%") :heading)
          (dolist (file apart)
            (put "  ")
            (file-link file)
            (put (format nil ": ~a~%" (escape-controls (compared-file-why file))) :list-note)))))))

(define-major-mode palimpsest-compared-mode special-mode
  "The pictures and the marks as large as a signature or a stamp that the
PDFs of a directory share, as palimpsest set them beside one another
\(\\[palimpsest-compare-files]).
\\<palimpsest-compared-mode>\\[push-button] on a file's name opens it; \\[quit-window] leaves."
  :lighter "Palimpsest compared" :interactive nil)

(defstruct (palimpsest-compare-job (:include job) (:constructor make-palimpsest-compare-job (buffer directory))
                                   (:copier nil))
  "Setting beside one another the PDFs of DIRECTORY, for BUFFER, a job;
PATHS, the PDFs found there (DIRECTORY-PDFS); FOUND, how many pictures and
pairs of marks they share; SHOWN, the list's text, faces and buttons, made
in the background (PIECES-TEXT); PROGRESS, the share of the files read."
  directory paths found shown (progress 0))

(setf (documentation 'palimpsest-compare-job-p 'function) "True when OBJECT is a PALIMPSEST-COMPARE-JOB.")

(define-job-words palimpsest-compare-job
  :doing "Comparing the files"
  :description (format nil "Setting beside one another the pictures of the PDFs under ~a"
                       (palimpsest-compare-job-directory job))
  :verb "compare the files"
  :left-words "nothing shown")

(defmethod job-percent ((job palimpsest-compare-job))
  "How far setting the files beside one another has got."
  (running-percent (round (* 1000 (palimpsest-compare-job-progress job))) 1000))

(defmethod job-take-in ((job palimpsest-compare-job))
  "Show what the files share, or say the directory holds too few PDFs."
  (let ((paths (palimpsest-compare-job-paths job)))
    (if (null (rest paths))
        (message "~a holds ~:[no PDF~;one PDF alone~]: two or more are needed"
                 (abbreviated-path (palimpsest-compare-job-directory job)) paths)
        (progn
          (show-made-list "*Palimpsest: compared*" 'palimpsest-compared-mode (palimpsest-compare-job-shown job))
          (destructuring-bind (files pictures pairs) (palimpsest-compare-job-found job)
            (message "~:d PDF~:p: ~:d picture~:p in more than one, ~:d pair~:p of marks the same in two"
                     files pictures pairs))))))

(defun directory-pdfs (directory)
  "The PDFs of DIRECTORY, not those of the directories under it: its regular
files named .pdf (in any case), sorted by name, as paths. An entry that is a
link is left out, wherever it leads, so that nothing outside the directory
is read."
  (loop for (name nil nil own) in (directory-entries directory)
        when (and (eq own :regular) (> (length name) 4) (string-equal ".pdf" name :start2 (- (length name) 4)))
          collect (path-join directory name)))

(defun compare-work (job directory)
  "What a job setting beside one another the PDFs of DIRECTORY does
\(PALIMPSEST-COMPARE-JOB): each PDF read in a Tuatara of its own shut in
\(COMPARED-SHUT-IN), one at a time, and what they said set beside one another
in one more (COMPARE-SHUT-IN), which makes the list; nothing of the PDFs is
read in the editor."
  (let ((paths (directory-pdfs directory)))
    (setf (palimpsest-compare-job-paths job) paths)
    (when (rest paths)
      (let ((paths (subseq paths 0 (min (length paths) *compared-files-most*))) (room *compared-marks-pixels-most*))
        (with-work-directory (marks)
          (with-work-directory (list)
            (let ((entries
                    (loop for path in paths for k from 1
                          collect (let ((place (path-join marks (format nil "~d/" (1- k))))
                                        (name (native-file-name path)))
                                    (make-directory place #o700)
                                    (prog1 (list name
                                                 (handler-case
                                                     (let ((data (shut-in 'compared-shut-in (list path) place
                                                                          (format nil "Reading ~a" name)
                                                                          :arguments (list :room (max 0 room)))))
                                                       (let ((kept (and (listp data) (fourth data))))
                                                         (when (integerp kept) (decf room kept)))
                                                       data)
                                                   (error (condition)
                                                     (job-check-stop)
                                                     (list nil nil (format nil "it could not be read: ~a"
                                                                           (shorten-string (princ-to-string condition)
                                                                                           200 :one-line t))))))
                                      (setf (palimpsest-compare-job-progress job) (/ k (1+ (length paths)))))))))
              (destructuring-bind (&optional pieces found &rest more)
                  (shut-in 'compare-shut-in (list marks) list "Comparing the files"
                           :arguments (list :entries entries :words (abbreviated-path directory)
                                            :past (- (length (palimpsest-compare-job-paths job)) (length paths))))
                (declare (ignore more))
                (setf (palimpsest-compare-job-found job)
                      (if (and (listp found) (= 3 (length found)) (every #'integerp found)) found (list 0 0 0))
                      (palimpsest-compare-job-shown job)
                      (multiple-value-list
                       (pieces-text (data-pieces (and (listp pieces) pieces) list
                                                 (lambda (action)
                                                   (let ((path (and (string= (first action) "visit")
                                                                    (integerp (second action))
                                                                    (nth (second action) paths))))
                                                     (and path (lambda () (visit-path path)))))))))))))))))

(define-command palimpsest-compare-files ()
  "Set beside one another the PDFs of a directory asked for, to find a
signature, a stamp or a seal the same in two of them. Their pictures that
are no scans are set beside one another by their octets, and the marks of
their scanned pages as large as a signature or a stamp pixel by pixel, as
\\[palimpsest] does within one file. Listed are the
pictures in more than one file and the pairs of marks of two files the same.
No hand signs twice alike and no stamp presses twice alike, but an office's
logo, or a signature it keeps as a picture, is honestly the same in every
letter: whether they should differ is for you to judge. The files are only
read, as many as *compared-files-most* of them; read in the background, as
long as examining each of them takes."
  (let ((buffer (current-buffer)))
    (start-directory-job "Compare the pictures and marks of the PDFs in directory: "
                         (lambda (directory) (make-palimpsest-compare-job buffer directory))
                         #'compare-work "tuatara palimpsest compare")))
