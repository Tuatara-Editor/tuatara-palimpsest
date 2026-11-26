;;;; page-checks.lisp --- What a PDF's pages draw: text covered, set apart, added or hidden.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:palimpsest)

;;; What the pages draw

(defun font-family (font)
  "The family of FONT: its name up to a comma or hyphen (Calibri for Calibri,Italic and Calibri-Bold)."
  (let* ((base (font-base font)) (cut (position-if (lambda (c) (find c ",-")) base)))
    (if (and cut (plusp cut)) (subseq base 0 cut) base)))

(defparameter *white-text-shows-under* 90
  "The most lightness (LIGHTNESS) a shading may have for white text on it to
be seen: a grey of 0.94, all but white, is 96; the pale red, blue and
yellow ends of the shadings that bars and banners are drawn with 60 to
96, the white labels on them seen on those of 60 to 80.")

(defun shapes-words (count colour)
  "COUNT filled shapes of COLOUR, in words: a white shape, 2 white shapes; a
shading of a colour not known, for shapes of a colour not known, as only
shadings are."
  (cond ((null colour) (format nil "~:[a shading~;~:*~d shadings~] of a colour not known" (and (> count 1) count)))
        ((> count 1) (format nil "~d ~a shapes" count (colour-words colour)))
        (t (format nil "a ~a shape" (colour-words colour)))))

(defun white-p* (colour)
  "True when COLOUR is white, or nearly: red, green and blue each 0.95 or
more, so that a white written as 0.98 by a program that rounds is white."
  (and colour (every (lambda (c) (>= c 0.95)) colour)))

(defun centre-inside-p (mark box-mark)
  "True when the middle of MARK lies within BOX-MARK's box."
  (inside-box-p (list (mark-x0 box-mark) (mark-y0 box-mark) (mark-x1 box-mark) (mark-y1 box-mark))
                (/ (+ (mark-x0 mark) (mark-x1 mark)) 2) (/ (+ (mark-y0 mark) (mark-y1 mark)) 2)))

(defun centre-height (mark)
  "The height of the middle of MARK's box."
  (/ (+ (mark-y0 mark) (mark-y1 mark)) 2))

(defun by-centre-height (marks)
  "MARKS (a list) as a vector sorted by the heights of their middles, and a
vector of those heights, as two values, for CENTRED-WITHIN."
  (let ((sorted (sort (coerce marks 'vector) #'< :key #'centre-height)))
    (values sorted (map 'vector #'centre-height sorted))))

(defun centred-within (sorted heights box-mark)
  "The marks of SORTED (BY-CENTRE-HEIGHT, with its HEIGHTS) whose middle lies
within BOX-MARK's box, give or take half a point, as CENTRE-INSIDE-P tells;
only those at its height are looked at."
  (loop for k from (sorted-position-after heights (- (mark-y0 box-mark) 0.5) :below t)
          below (sorted-position-after heights (+ (mark-y1 box-mark) 0.5))
        for mark = (aref sorted k)
        when (centre-inside-p mark box-mark) collect mark))

(defun area (mark)
  "The area of MARK's box, in square points."
  (* (max 0 (mark-width mark)) (max 0 (mark-height mark))))

(defun later-source-p (pdf revision mark)
  "True when the stream drawing MARK was written in a later save than the original."
  (let ((w (and (mark-source mark) (written-in pdf revision (mark-source mark))))) (and w (plusp (revision-index w)))))

(defun mark-characters (mark)
  "How many characters other than spaces MARK shows."
  (count #\Space (mark-text mark) :test-not #'char=))

(defun line-usual (marks key)
  "The value of KEY (a function of a mark) that most of MARKS' characters
have, and how many characters have it; values within a hundredth of each
other counted as one."
  (let ((counts '()))
    (dolist (m marks)
      (let* ((value (funcall key m))
             (entry (find-if (lambda (e) (if (and (realp value) (realp (car e)))
                                             (<= (abs (- value (car e))) 0.01)
                                             (equal value (car e))))
                             counts)))
        (if entry
            (incf (cdr entry) (mark-characters m))
            (push (cons value (mark-characters m)) counts))))
    (let ((best (reduce (lambda (a b) (if (> (cdr b) (cdr a)) b a)) counts :initial-value (cons nil 0))))
      (values (car best) (cdr best)))))

(defparameter *line-unlike-least* 3
  "The fewest letters or figures a run of text must show for it to be told
apart from the rest of its line by its size, weight, setting or ink.")

(defun dark-p (colour)
  "True when COLOUR is a black or a dark grey, as text is printed in."
  (and colour (every (lambda (c) (<= c 0.4)) colour)))

(defun render-mode-words (mode)
  "How text in render MODE is drawn, in words."
  (case mode (0 "filled") (1 "outlined") (2 "filled and outlined") (t (format nil "in render mode ~d" mode))))

(defun colour-figures (colour)
  "COLOUR, (red green blue) each 0 to 1, as figures to two places, where
COLOUR-WORDS names it: grey 0.13 when the three are one, else rgb 0.10 0.10
0.12; so that two blacks a hundredth or two apart read apart."
  (if (= (first colour) (second colour) (third colour))
      (format nil "grey ~,2f" (first colour))
      (format nil "rgb ~{~,2f~^ ~}" colour)))

(defparameter *baseline-least* 0.2
  "The least, in points, a run of text set above or below the rest of its
line, or a line set off an edge others share, is told apart by: programs set
the runs of a line on one baseline and lines on one edge to a hundredth.")

(defun line-unlike (line)
  "The runs of text in LINE set unlike the rest of it, a list of (KIND MARK
OWN USUAL). KIND is :size (its height in points), :weight (its render mode,
filled and outlined drawing bolder) or :setting (how much narrower or wider
its characters are set). KIND is :spacing (the space after each character,
in sizes), :ink (its colour, both dark) or :baseline (its baseline, in
points from the page's foot, a run as large as the line's set above or
below the rest by more than *BASELINE-LEAST*). Only a run of
*LINE-UNLIKE-LEAST* letters or figures, in the same font family as most of
the line, among at least twice as many characters set the usual way is told
apart; a run set unlike the rest by a few hundredths is not, nor a
superscript (a size under 70 % of the line's)."
  (let* ((shown (remove-if
                  (lambda (m) (or (invisible-text-p m) (null (mark-font m)) (white-p* (mark-colour m))))
                  line))
         (family (line-usual shown (lambda (m) (font-family (mark-font m)))))
         (same (remove-if-not (lambda (m) (equal (font-family (mark-font m)) family)) shown))
         (out '()))
    (flet ((odd (kind key unlike-p &optional (among same))
             (multiple-value-bind (usual many) (line-usual among key)
               (dolist (m among)
                 (let ((own (funcall key m)))
                   (when (and usual (>= (count-if #'alphanumericp (mark-text m)) *line-unlike-least*)
                              (>= many (* 2 (mark-characters m)))
                              (funcall unlike-p own usual))
                     (push (list kind m own usual) out)))))))
      (odd :size #'mark-size
           (lambda (own usual) (and (plusp usual) (< 0.7 (/ own usual) 1.43) (> (abs (- (/ own usual) 1)) 0.04))))
      (odd :weight #'mark-mode (lambda (own usual) (and (member own '(0 1 2)) (member usual '(0 1 2)) (/= own usual))))
      (odd :setting #'mark-squeeze (lambda (own usual) (and (plusp usual) (> (abs (- (/ own usual) 1)) 0.03))))
      (odd :spacing #'mark-spacing (lambda (own usual) (> (abs (- own usual)) 0.03)))
      (odd :ink #'mark-colour (lambda (own usual) (and (dark-p own) (dark-p usual)
                                                         (>
                                                           (reduce #'max
                                                                   (mapcar (lambda (a b) (abs (- a b))) own usual))
                                                           0.02)))
           (remove-if-not (lambda (m) (dark-p (mark-colour m))) shown))
      ;; a superscript or subscript, smaller and moved on purpose, is not set beside the line
      (let ((size (line-usual same #'mark-size)))
        (odd :baseline #'mark-y0 (lambda (own usual) (> (abs (- own usual)) *baseline-least*))
             (remove-if-not (lambda (m) (and size (plusp size) (>= (/ (mark-size m) size) 0.85))) same))))
    (nreverse out)))

(defun lines-off-edges (lines)
  "The lines of LINES (PAGE-LINES-OF) set a little off an edge three or more
other lines of about their size share, each (SIDE LINE OWN USUAL MANY).
SIDE is :left for where it starts, :right where it ends (only when every
font of the line gives its widths, so that its end is known). OWN is its
edge, USUAL the shared one, MANY how many lines share it. Off means by more
than *BASELINE-LEAST* and less than 3 points: further is an indent. Lines
of another size (a heading, a header, a footnote) are another style, set to
their own edges, and are not set beside each other."
  (let ((out '()))
    (flet ((size-of (line) (median-of (mapcar #'mark-size line)))
           (alike (a b) (and (plusp a) (plusp b) (<= (/ (abs (- a b)) (max a b)) 0.1))))
      (flet ((edges (side edge-of usable-p)
               (let* ((usable (remove-if-not (lambda (line) (and (funcall usable-p line)
                                                                  (>=
                                                                    (count-if #'alphanumericp
                                                                              (format nil "~{~a~}"
                                                                                      (mapcar #'mark-text line)))
                                                                    3)))
                                             lines))
                      (shared '()))
                 ;; the edges three or more lines of a size share, to a hundredth: each (EDGE SIZE COUNT)
                 (dolist (line usable)
                   (let* ((edge (funcall edge-of line)) (size (size-of line))
                          (entry (find-if
                                   (lambda (e) (and (<= (abs (- (first e) edge)) 0.01) (alike (second e) size)))
                                   shared)))
                     (if entry (incf (third entry)) (push (list edge size 1) shared))))
                 (setf shared (remove-if (lambda (e) (< (third e) 3)) shared))
                 (dolist (line usable)
                   (let* ((edge (funcall edge-of line)) (size (size-of line))
                          (near (find-if
                                  (lambda (e)
                                    (and (alike (second e) size) (< *baseline-least* (abs (- (first e) edge)) 3)))
                                  shared)))
                     (when (and near
                                (not (find-if
                                       (lambda (e) (and (alike (second e) size) (<= (abs (- (first e) edge)) 0.01)))
                                       shared)))
                       (push (list side line edge (first near) (third near)) out)))))))
        (edges :left (lambda (line) (mark-x0 (first line))) (constantly t))
        (edges :right (lambda (line) (mark-x1 (car (last line))))
               (lambda (line)
                 (every (lambda (m) (and (mark-font m) (plusp (hash-table-count (font-widths (mark-font m))))))
                        line)))))
    (nreverse out)))

(defstruct (page-check (:constructor make-page-check (pdf revision page marks texts number scan)) (:copier nil)
                       (:predicate nil))
  "What the checks of one page's content read and what they find: the PDF
and REVISION, the PAGE, the MARKS it draws and the TEXTS among them, its
NUMBER counting from 1, the picture SCAN covering most of it or NIL, and
the FINDINGS so far, the latest first."
  pdf revision page marks texts number scan (findings '()))

(defun page-found (check strength words rule marks-shown &rest keys)
  "Add to CHECK a finding of STRENGTH saying WORDS by RULE, of the marks
MARKS-SHOWN: the evidence of the first four of them, the steps to the
first, the objects drawing them. KEYS go to FOUND."
  (let ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check)))
    (push (apply #'found strength words :rule rule :revision (revision-index revision) :page (page-check-number check)
                 :evidence (loop for m in marks-shown
                                 repeat 4
                                 append (mark-evidence pdf revision m (mark-words page m)))
                 :steps (loop for m in marks-shown
                              repeat 1
                              append (nth-value 1 (mark-evidence pdf revision m)))
                 :objects (remove-duplicates (remove nil (mapcar #'mark-source marks-shown)))
                 keys)
          (page-check-findings check))))

(defun marks-text (marks length)
  "The text of MARKS one after another, a space between, cut to LENGTH
characters on one line."
  (shorten-string (format nil "~{~a~^ ~}" (mapcar #'mark-text marks)) length :one-line t))

(defun cover-cause (check shapes runs within parts framed counts angle size)
  "The honest cause the drawing shows for SHAPES covering RUNS, text drawn
before them: :background or :turned for a panel over a watermark, :outlined,
:labels, :stamp, :form or :map; NIL for none, or when the shapes carry new
text set in place of the old, or when the file is not as its maker wrote
it. WITHIN gives the marks within a shape, PARTS the parts of the runs
hidden, FRAMED the page's text runs whose frames are known, COUNTS how many
of them write each text (TEXT-COUNTS), ANGLE and SIZE their usual turn and
size."
  (let ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (marks (page-check-marks check))
        (hidden-parts (or parts (mapcar (lambda (run) (list run 0 1)) runs))))
    (flet ((later-text-p (s test)
             (some (lambda (m) (and (> (mark-order m) (mark-order s)) (funcall test m))) (funcall within s))))
      (cond ((some (lambda (s) (later-text-p s (lambda (m) (some (lambda (h) (replacement-p m h)) hidden-parts))))
                   shapes)
             nil)
            ;; only for a file as its maker wrote it, as for a text box drawn as made
            ((not (file-as-written-p pdf revision)) nil)
            ;; a panel over a watermark carries text of its own, as panels in documents do
            ((and (every (lambda (run) (background-run-p run framed angle size counts)) runs)
                  (every (lambda (s) (later-text-p s (lambda (m) (own-text-shows-p m s nil)))) shapes))
             (if (every (lambda (run) (eq (background-run-p run framed angle size counts) :light)) runs)
                 :background
                 :turned))
            ((some #'outline-shows-p shapes) :outlined)
            ((every (lambda (run) (some (lambda (s) (label-p run s marks)) shapes)) runs) :labels)
            ((every (lambda (run) (every (lambda (s) (stamp-p run s marks)) shapes)) runs) :stamp)
            ((every (lambda (s) (form-field-p runs s marks)) shapes) :form)
            ((every (lambda (run) (every (lambda (s) (map-label-p run s marks framed counts)) shapes)) runs) :map)))))

(defparameter *cover-cause-words*
  '((:background . "; the text beneath is a watermark or a background word, light and under all the page's text, ~
                    turned, large or written again and again, under a panel with text of its own: panels lie over ~
                    such words in documents as made, as they do when a watermark is hidden")
    (:turned . "; the text beneath is drawn before all the page's text and turned ~d degrees from it, as a watermark ~
                is, under a panel with text of its own: panels lie over such words in documents as made, as they do ~
                when a watermark is hidden")
    (:outlined . "; the shape is outlined in a colour seen against the page, a callout or label meant to be seen, not ~
                  a patch the colour of the paper")
    (:labels . "; labels laid over one another as a drawing lays them, each a box with its text, the text beneath a ~
                label of its own")
    (:stamp . "; a stamp or note laid across the text, its own text turned against the line beneath, not set along it ~
               as new text in place of old is")
    (:form . "; a form field filled in, as a form flattened into its page draws one: a box with its value over the ~
              line to write on and the hint beside it")
    (:map . "; a label laid on a map, a pill round its own name over names written again and again beneath, laid ~
             down first"))
  "What each honest cause COVER-CAUSE names says, after the finding's words:
format controls, the one of :turned taking the angle in degrees.")

(defun cover-rule (made cause kind)
  "The rule of a finding of shapes over text: MADE when drawn as the file was
made, CAUSE the honest cause shown, KIND :covered when the text lies wholly
under the shapes."
  (cond (made (format nil "A filled shape drawn after text, over it, hides that text from the eye but not from the ~
                           file. This one was drawn by the same content stream as the text, with text of its own on it ~
                           as a text box has, in a font kept in the file, in the file's first save, its /ID as its ~
                           maker wrote it, by a program that makes PDFs from documents and with no PDF editor named: ~
                           the document as its program printed it had the shape over the text."))
        (cause (format nil "A filled shape drawn after text, over it, hides that text from the eye but not from the ~
                            file; how this one lies, as the finding names it, is how documents are drawn as made, so ~
                            it is worth a look, not a strong sign."))
        ((eq kind :covered) (format nil "A filled shape drawn after text, over it, hides that text from the eye but ~
                                         not from the file: every character of it lies wholly under the shape, tested ~
                                         along its own baseline."))
        (t (format nil "A filled shape drawn after text, over a part of it, hides that part from the eye but not from ~
                        the file: the way an editor changes one figure of a line."))))

(defun check-covered-text (check)
  "Add to CHECK the text its page draws that filled shapes drawn after it
cover (PAGE-COVERS), wholly or in part: a strong sign when a shape is white
and nothing the drawing shows explains it (DRAWN-AS-MADE-P, COVER-CAUSE),
else worth a look."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check))
         (p (page-check-number check))
         (framed (remove-if-not #'mark-frame (page-check-texts check)))
         (counts (text-counts framed)))
    (multiple-value-bind (entries within) (page-covers (page-check-marks check))
      (multiple-value-bind (angle size) (page-text-usual framed)
        (loop for (kind shapes runs part hidden) in entries
              do (let* ((colour (mark-colour (first shapes)))
                        (black (and colour (every (lambda (c) (<= c 0.05)) colour)))
                        (later (some (lambda (s) (later-source-p pdf revision s)) shapes))
                        (parts (and (eq kind :partly)
                                    (let ((run (first runs)) (first (position t hidden))
                                          (last (position t hidden :from-end t)))
                                      (list (list run (aref (mark-stops run) (* 2 first))
                                                  (aref (mark-stops run) (1+ (* 2 last))))))))
                        (made (every (lambda (s) (drawn-as-made-p pdf revision s runs (funcall within s) page parts))
                                     shapes))
                        ;; an honest cause the drawing itself shows none for a shape added later
                        (cause (and (not (or made later))
                                    (cover-cause check shapes runs within parts framed counts angle size)))
                        (strength (if (and (white-p* colour) (not made) (not cause)) :strong :look))
                        (why (cond (made (format nil "; drawn with the text as the file was made: a shape over text in ~
                                                      the program the document was written in, as a text box over a ~
                                                      spreadsheet's cell is, or a change made there before the PDF was ~
                                                      made"))
                                   (cause (format nil (cdr (assoc cause *cover-cause-words*))
                                                  (and (eq cause :turned)
                                                       (round (reduce #'min runs
                                                                      :key (lambda (run)
                                                                             (nth-value 1 (background-run-p
                                                                                           run framed angle size
                                                                                           counts))))))))
                                   (t "")))
                        (rule (cover-rule made cause kind)))
                   (if (eq kind :covered)
                       (page-found check strength
                                   (format nil "Text on page ~d is covered by ~a drawn over it~:[~;, added in a later ~
                                                save~]: ~s~:[~; (blacked out: the text is still in the file)~]~a."
                                           p (shapes-words (length shapes) colour) later
                                           (marks-text runs 120) black why)
                                   rule (append shapes runs) :method :text-covered)
                       (let ((run (first runs)))
                         (page-found check strength
                                     (format nil "Text on page ~d is partly covered by ~a drawn over it~:[~;, added in ~
                                                  a later save~]: ~s of ~s~a."
                                             p (shapes-words (length shapes) colour) later part
                                             (shorten-string (mark-text run) 100 :one-line t) why)
                                     rule (append shapes runs)
                                     :details (let ((place (mark-place run)))
                                                (and place (list (format nil "the line is drawn at ~a" place))))
                                     :method :text-covered)))))))))

(defparameter *hidden-letters-least* 3
  "The fewest letters and figures text never seen must hold to be worth a
look (HIDDEN-STRENGTH). Fewer are specks of a text recognition layer, a
mark left by a program, or text of no characters at all, and are noted for
the record. See doc/measures.org.")

(defun hidden-strength (marks strength)
  "How strong a finding of the text MARKS never seen is: STRENGTH, but for
the record when it would be worth a look and MARKS hold fewer than
*HIDDEN-LETTERS-LEAST* letters and figures in all."
  (if (and (eq strength :look)
           (< (loop for m in marks sum (count-if #'alphanumericp (mark-text m))) *hidden-letters-least*))
      :record
      strength))

(defun check-unseen-text (check)
  "Add to CHECK the text its page draws that is never seen: invisible (on a
scan, the text recognition of it, for the record), clipped away, outside
the page, or in a layer that is off."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check))
         (p (page-check-number check)) (texts (page-check-texts check))
         (invisible (remove-if-not #'invisible-text-p texts))
         (clipped (remove-if-not #'mark-clipped texts))
         (box (or (page-crop page) (page-media page)))
         (outside (remove-if (lambda (m)
                               (inside-box-p box (/ (+ (mark-x0 m) (mark-x1 m)) 2) (/ (+ (mark-y0 m) (mark-y1 m)) 2) 1))
                             texts))
         (hidden (remove-if-not #'mark-hidden texts)))
    (cond ((null invisible))
          ((page-check-scan check)
           (page-found check :record
                       (format nil "Page ~d is a picture with invisible text over it, as text recognition (OCR) ~
                                    leaves." p)
                       (format nil "Invisible text (render mode 3) over a picture of the whole page is how scanned ~
                                    pages are made searchable.")
                       (list (first invisible))))
          (t
           (page-found check (if (some (lambda (m) (later-source-p pdf revision m)) invisible)
                                 :look
                                 (hidden-strength invisible :look))
                       (format nil "Page ~d holds invisible text: ~s." p (marks-text invisible 120))
                       "Text in render mode 3 or 7 is never drawn: found by searching and copying, never seen."
                       invisible :method :hidden-text :kind :hidden)))
    (when clipped
      (page-found check (hidden-strength clipped :look)
                  (format nil "Text on page ~d lies outside what the page clips to, so it is never seen: ~s."
                          p (marks-text clipped 120))
                  "Text drawn outside the clipping path is cut away." clipped :method :hidden-text :kind :hidden))
    (when outside
      (page-found check (hidden-strength outside :look)
                  (format nil "Text on page ~d lies outside the page: ~s." p (marks-text outside 120))
                  "Text drawn outside the page's crop box (else its media box) is never shown."
                  outside :method :hidden-text :kind :hidden))
    (when hidden
      (page-found check (hidden-strength hidden :look)
                  (format nil "Text on page ~d is in a layer that is off: ~s." p (marks-text hidden 120))
                  "Content tagged with an optional content group that is off as the file opens is not shown."
                  hidden :method :hidden-text :kind :hidden))))

(defun white-text-unseen (check)
  "The white text CHECK's page draws, not invisible, that nothing drawn
before it beneath shows up: no filled shape or picture not white, nor a
shading dark enough that white shows on it."
  (let* ((white (remove-if-not (lambda (m) (and (not (invisible-text-p m)) (white-p* (mark-colour m))))
                               (page-check-texts check)))
         (seen (make-hash-table :test 'eq)))
    (multiple-value-bind (white-by-height white-heights) (by-centre-height white)
      (loop for under across (page-check-marks check)
            for shading = (equal (mark-text under) "shading")
            when (and (member (mark-kind under) '(:fill :image)) (not (white-p* (mark-colour under)))
                      ;; a shading of a colour not known, or one so light white is lost on it, shows nothing for
                      ;; certain
                      (or (not shading)
                          (and (mark-colour under) (< (lightness (mark-colour under)) *white-text-shows-under*))))
              do (dolist (m (centred-within white-by-height white-heights under))
                   (when (and (< (mark-order under) (mark-order m))
                              ;; a shading paints only where it reaches, not its whole box
                              (or (not shading) (null (mark-outline under))
                                  (covered-point-p (list (make-coverage under))
                                                   (float (/ (+ (mark-x0 m) (mark-x1 m)) 2) 1d0)
                                                   (float (/ (+ (mark-y0 m) (mark-y1 m)) 2) 1d0))))
                     (setf (gethash m seen) t)))))
    (remove-if (lambda (m) (gethash m seen)) white)))

(defun check-faint-text (check)
  "Add to CHECK the white text its page draws on white (WHITE-TEXT-UNSEEN),
and its text under a point tall."
  (let ((p (page-check-number check))
        (white (white-text-unseen check))
        (tiny (remove-if-not (lambda (m) (< 0 (mark-size m) 1)) (page-check-texts check))))
    (when white
      (page-found check
                  (let ((pdf (page-check-pdf check)) (revision (page-check-revision check)))
                    (if (some (lambda (m) (later-source-p pdf revision m)) white) :look (hidden-strength white :look)))
                  (format nil "Page ~d holds white text on a white page, unseen: ~s." p (marks-text white 120))
                  "Text the colour of the page beneath it cannot be seen." white :method :hidden-text :kind :hidden))
    (when tiny
      (page-found check (hidden-strength tiny :look)
                  (format nil "Page ~d holds text too small to read (under 1 pt): ~s." p (marks-text tiny 120))
                  "Text smaller than a point cannot be read on the page." tiny :method :hidden-text :kind :hidden))))

(defun check-content-streams (check)
  "Add to CHECK, when its page draws from more than one content stream, a
finding saying so: worth a look when one was appended in a later save,
else for the record."
  (let* ((pdf (page-check-pdf check)) (revision (page-check-revision check)) (page (page-check-page check))
         (p (page-check-number check)) (contents (page-contents page)))
    (when (> (length contents) 1)
      (let* ((written (mapcar (lambda (c) (let ((w (written-in pdf revision c))) (if w (revision-index w) 0)))
                              contents))
             (late (> (reduce #'max written) (reduce #'min written))))
        (push (found (if late :look :record)
                     (format nil "Page ~d draws from ~d content streams~:[~;, one of them appended in a later save: ~
                                  something was laid over the page~]." p (length contents) late)
                     :rule (format nil "A page's /Contents may be an array of streams drawn one after another; a ~
                                        stream added in a later save draws over what was there.")
                     :revision (revision-index revision) :page p :objects contents
                     :evidence (loop for c in contents for w in written
                                     append (cons (format nil "content stream object ~d, written in revision ~d" c
                                                          (1+ w))
                                                  (nth-value 0 (object-evidence pdf revision c))))
                     :method :laid-over)
              (page-check-findings check))))))

(defun font-family-counts (texts &key figures)
  "A table of the font families TEXTS are set in: how many characters each
sets; with FIGURES, T for each family setting a figure."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (m texts table)
      (when (mark-font m)
        (if figures
            (when (some #'digit-char-p (mark-text m))
              (setf (gethash (font-family (mark-font m)) table) t))
            (incf (gethash (font-family (mark-font m)) table 0) (length (mark-text m))))))))

(defparameter *style-words* '("bold" "italic" "oblique" "black" "heavy" "light" "medium" "semi" "demi" "cond" "narrow"
                              "mono" "typewriter" "ocr" "slant")
  "Words in a font's name that say it is a style of a family, set to stand
out: bold, italic, condensed, monospaced, a font for machine reading.")

(defun styled-apart-p (font usual)
  "True when FONT is a style of the family of USUAL, or a face made to stand
out beside it, as a program sets a figure on purpose: FONT's name holds a
word of *STYLE-WORDS* that USUAL's lacks, or both are Computer Modern fonts, which name their styles in
letters (CMR10, CMBX10, CMTI9, CMTT12). A figure retyped to pass for its
line's own is in a plain face that looks like the line's."
  (let ((own (string-downcase (font-base font))) (other (string-downcase (font-base usual))))
    (or (some (lambda (word) (and (search word own) (not (search word other)))) *style-words*)
        (and (uiop:string-prefix-p "cm" own) (uiop:string-prefix-p "cm" other)))))

(defun figure-passes-p (run line)
  "True when RUN, a run of text of LINE in another font than the rest of it,
is set to pass for the line's own: within 15 % of the size most of the
line's other characters have, no more than a tenth of that size off their
baseline, and in a face not styled apart from the font most of them are in
\(STYLED-APART-P). A superscript, a footnote's number or a label is smaller
or raised; a figure set off on purpose is bold, italic or monospaced."
  (let* ((others (remove run line))
         (size (and others (line-usual others #'mark-size)))
         (baseline (and others (line-usual others #'mark-y0)))
         (usual (and others (line-usual (remove nil others :key #'mark-font) #'mark-font))))
    (and size baseline usual (plusp size)
         (< 0.85 (/ (mark-size run) size) 1.18)
         (<= (abs (- (mark-y0 run) baseline)) (* 0.1 size))
         (not (styled-apart-p (mark-font run) usual)))))

(defun line-not-kept (line unkept-characters)
  "The runs of LINE, runs of text on one baseline, that hold a figure and are
set in a font not kept in the file, where the line's other text, three
characters or more, is set in fonts kept in it, and the fonts not kept set
40 characters of the page or fewer in all (UNKEPT-CHARACTERS): a number or
a date written by a program that does not embed, not a program leaving out
the same fonts everywhere."
  (flet ((kept-p (m) (and (mark-font m) (font-embedded (mark-font m)))))
    (when (and (<= unkept-characters 40)
               (>= (reduce #'+ (remove-if-not #'kept-p line) :key #'mark-characters) 3))
      (loop for m in line
            when (and (mark-font m) (not (kept-p m)) (plusp (length (font-base (mark-font m))))
                      (some #'digit-char-p (mark-text m)))
              collect m))))

(defun line-retyped (line font-characters fonts-with-figures)
  "The runs of LINE that are figures in a font of their own, each (MARK
MAIN), MAIN being the family most of the line is set in, when that family
has figures of its own on the page (FONTS-WITH-FIGURES) and the run's font
sets 40 characters of the page or fewer (FONT-CHARACTERS)."
  (let* ((counts (font-family-counts line))
         (total (loop for v being the hash-values of counts sum v))
         (main (loop with best = nil with most = 0 for k being the hash-keys of counts using (hash-value v)
                     when (or (> v most) (and (= v most) best (string< k best)))
                       do (setf best k most v)
                     finally (return best))))
    (when (and main (>= (gethash main counts) (* 0.7 total)) (gethash main fonts-with-figures)
               (>= (count-if #'alphanumericp
                             (format nil "~{~a~}" (mapcar #'mark-text
                                                          (remove main line
                                                                  :key (lambda (m)
                                                                         (and (mark-font m)
                                                                              (font-family (mark-font m))))
                                                                  :test-not #'equal))))
                   (* 0.5 (gethash main counts))))
      (loop for m in line
            for text = (string-trim " " (mark-text m))
            when (and (mark-font m) (string/= (font-family (mark-font m)) main)
                      (plusp (length text)) (<= (length text) 12)
                      (every (lambda (c) (or (digit-char-p c) (find c ".,:/$€£%()+- "))) text)
                      (>= (count-if #'digit-char-p text) (* 0.5 (length (remove #\Space text))))
                      (<= (gethash (font-family (mark-font m)) font-characters 0) 40))
              collect (list m main)))))

(defun lines-set-apart (texts lines)
  "Three values, of the LINES of a page drawing TEXTS: the lines drawn a
character at a time where the rest of the page is not, those whose
characters are spaced apart one by one where the rest is not, and the runs
that are figures retyped in a font of their own (LINE-RETYPED)."
  (let* ((shows (reduce #'+ texts :key #'mark-shows)) (singles (reduce #'+ texts :key #'mark-singles))
         (page-characters (reduce #'+ texts :key #'mark-characters))
         (page-gaps (reduce #'+ texts :key #'mark-wide))
         (font-characters (font-family-counts texts))
         (fonts-with-figures (font-family-counts texts :figures t))
         (one-by-one '()) (spaced '()) (retyped '()))
    (dolist (line lines)
      (let ((line-shows (reduce #'+ line :key #'mark-shows)) (line-singles (reduce #'+ line :key #'mark-singles))
            (line-characters (reduce #'+ line :key #'mark-characters)) (line-gaps (reduce #'+ line :key #'mark-wide)))
        (when (and (>= line-shows 5)
                   (>= line-characters 10)
                   (>= line-singles (* 0.8 line-shows))
                   (>= (- shows line-shows) 5)
                   (< (- singles line-singles) (* 0.3 (- shows line-shows))))
          (push line one-by-one))
        (when (and (>= line-characters 4)
                   (>= line-gaps (* 0.6 line-characters))
                   (>= (- page-characters line-characters) 20)
                   (< (- page-gaps line-gaps) (* 0.25 (- page-characters line-characters))))
          (push line spaced))
        (setf retyped (revappend (line-retyped line font-characters fonts-with-figures) retyped))))
    (values (nreverse one-by-one) (nreverse spaced) (nreverse retyped))))

(defparameter *unlike-run-words*
  '((:size "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] in another size than the line around ~:*~[~;it~:;them~], ~
            in the same font: pasted, or retyped to fit."
     "A program sets a line in one size; a few words of the same font a little smaller or larger than their line were ~
      set apart from it."
     "~s at ~,1f pt, its line at ~,1f pt")
    (:weight "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] drawn bolder or lighter than the line around ~
              ~:*~[~;it~:;them~]: made to look bold, or pasted."
     "Bold text is set in a bold font; text both filled and outlined (render mode 2) beside text only filled was drawn ~
      heavier another way."
     "~s ~a, its line ~a")
    (:setting "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] set narrower or wider than the line around ~
               ~:*~[~;it~:;them~]: squeezed or stretched to fit a gap."
     "Characters squeezed or stretched (a horizontal scale, a text matrix narrowed) unlike the rest of their line were ~
      fitted into a space."
     "~s at ~d % of its width, its line at ~d %")
    (:spacing "On page ~d, ~:d run~:p of text ~:*~[~;has~:;have~] ~:*~[~;its~:;their~] characters spaced unlike the ~
               line around ~:*~[~;it~:;them~]: fitted into a gap."
     "Space added after each character (character spacing) in a few words of a line, not the rest, spreads or packs ~
      them to fit."
     "~s with ~,2f of its size after each character, its line ~,2f")
    (:ink "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] in another black than the line around ~:*~[~;it~:;them~]: ~
           pasted from another document, or retyped."
     "A program sets a line's text in one colour; a black or dark grey a little unlike the rest of the line came from ~
      elsewhere."
     "~s in ~a, its line in ~a")
    (:baseline "On page ~d, ~:d run~:p of text ~:*~[~;sits~:;sit~] a little above or below the line around ~
                ~:*~[~;it~:;them~]: placed by hand."
     "A program sets the runs of a line on one baseline; a run of the same size a fraction of a point above or below ~
      it was put there apart."
     "~s ~,2f pt ~:[below~;above~] its line"))
  "For each way a run of text is unlike its line (LINE-UNLIKE), in order:
the words of the finding (a format control of the page and the count), its
rule, and the format control of each run's detail.")

(defun unlike-run-detail (kind control text own usual)
  "The detail of a run of TEXT unlike its line in the way KIND: CONTROL (of
*UNLIKE-RUN-WORDS*) given the run's value OWN and the line's USUAL one, as
each kind shows them."
  (ecase kind
    (:size (format nil control text own usual))
    (:weight (format nil control text (render-mode-words own) (render-mode-words usual)))
    (:setting (format nil control text (round (* 100 own)) (round (* 100 usual))))
    (:spacing (format nil control text own usual))
    (:ink (format nil control text (colour-figures own) (colour-figures usual)))
    (:baseline (format nil control text (abs (- own usual)) (> own usual)))))

(defun check-unlike-runs (check unlike &optional (place (constantly nil)))
  "Add to CHECK, for each way runs of text are unlike their line (UNLIKE,
from LINE-UNLIKE), one finding of those runs; those of them PLACE (a
function of a run) gives :PROSE, in running text or boilerplate, one finding
for the record (PLACE-WORTH-CHANGING)."
  (loop for (kind words rule control) in *unlike-run-words*
        for all = (remove kind unlike :key #'first :test-not #'eq)
        for prose = (remove-if-not (lambda (run) (eq (funcall place (second run)) :prose)) all)
        do (loop for runs in (list (set-difference all prose :test #'eq) prose)
                 for noted in '(nil t)
                 when runs
                   do (page-found check (if noted :record :look)
                                  (format nil "~?~:[~; It lies in running text or boilerplate, where a change would ~
                                               serve no one: for the record.~]"
                                          words (list (page-check-number check) (length runs)) noted)
                                  (format nil rule)
                                  (mapcar #'second runs)
                                  :kind :layout
                                  :place (cond (noted :prose)
                                               ((some (lambda (run) (eq (funcall place (second run)) :field)) runs)
                                                :field))
                                  :method (ecase kind
                                            ((:size :setting :spacing) :fitted)
                                            ((:weight :ink) :retyped)
                                            (:baseline :placed-by-hand))
                                  :details (loop for (nil m own usual) in runs
                                                 collect (format nil "~a~@[ (~a)~]"
                                                                 (unlike-run-detail kind control
                                                                                    (shorten-string (mark-text m) 60
                                                                                                    :one-line t)
                                                                                    own usual)
                                                                 (mark-place m)))))))

(defun check-text-set-apart (check)
  "Add to CHECK the lines of text its page draws that were set apart from the
rest: drawn a character at a time, spaced one by one, figures in a font of
their own, runs unlike their line, lines a little off the edge the lines
around them keep. Only text that shows is judged: invisible text
\(INVISIBLE-TEXT-P), as text recognition lays over a scan, is set as its
program sets it, and is named, when it is no recognition layer, by
CHECK-UNSEEN-TEXT."
  (let* ((p (page-check-number check))
         (shown (remove-if #'invisible-text-p (page-check-texts check)))
         (lines (page-lines-of shown))
         (unlike (loop for line in lines append (line-unlike line)))
         ;; what each run's line is to a forger
         (places (let ((table (make-hash-table :test 'eq)))
                   (dolist (line lines table)
                     (let ((place (place-worth-changing (values (line-text line)))))
                       (dolist (m line) (setf (gethash m table) place))))))
         (place (lambda (m) (gethash m places))))
    (flet ((line-text (line) (marks-text line 100)))
      (multiple-value-bind (one-by-one spaced retyped) (lines-set-apart shown lines)
        (when one-by-one
          (page-found check :look
                      (format nil "~:d line~:p on page ~d ~:*~:*~[~;is~:;are~] drawn a character at a time, unlike ~
                                   the rest of the page: retyped, or added by another program."
                              (length one-by-one) p)
                      (format nil "Programs draw a line of text in a few runs; a line drawn character by character ~
                                   where the rest is not was written another way.")
                      (apply #'append one-by-one)
                      :details (mapcar #'line-text one-by-one) :method :retyped :kind :layout))
        (when spaced
          (page-found check :look
                      (format nil "~:d line~:p on page ~d ~:*~:*~[~;is~:;are~] spaced oddly, ~:*~[~;its~:;their~] ~
                                   characters set apart one by one: fitted into a gap."
                              (length spaced) p)
                      (format nil "Characters set apart by moving the text on, nearly one by one, where the rest of ~
                                   the page is not, are how text is fitted into a gap.")
                      (apply #'append spaced)
                      :details (mapcar #'line-text spaced) :method :fitted :kind :layout))
        (let ((passing (remove-if-not (lambda (r) (figure-passes-p (first r) (find (first r) lines :test #'member)))
                                      retyped)))
          (loop for runs in (list passing (set-difference retyped passing :test #'eq))
                for pass in '(t nil)
                when runs
                  do (page-found check :look
                                 (format nil "On page ~d, ~:d figure~:p ~:*~[~;is~:;are~] in another font than the ~
                                              line around ~:*~[~;it~:;them~]~:[~;, at the line's size and on its ~
                                              baseline~]: retyped."
                                         p (length runs) pass)
                                 (format nil "Figures in a font used nowhere else on the page, in a line whose own ~
                                              font has figures of its own, were set apart from it, as a retyped number ~
                                              is.~
                                              ~:[~; A superscript or a label is set smaller or raised; a figure at ~
                                              its line's size on its baseline is set to pass for the line's own.~]"
                                         pass)
                                 (mapcar #'first runs)
                                 :details (loop for (m main) in runs
                                                collect (format nil "~s in ~a, its line in ~a~@[ (~a)~]"
                                                                (mark-text m) (font-base (mark-font m)) main
                                                                (mark-place m)))
                                 :method :retyped :kind (if pass :figure-passing :layout) :place :field)))
        (let* ((characters (reduce #'+ (remove-if (lambda (m) (or (null (mark-font m))
                                                                   (font-embedded (mark-font m))))
                                                         shown)
                                   :key #'mark-characters))
               (unkept (loop for line in lines append (line-not-kept line characters))))
          (when unkept
            (page-found check :look
                        (format nil "On page ~d, ~:d run~:p of text ~:*~[~;is~:;are~] set in a font not kept in the ~
                                     file (~{~a~^, ~}), in ~:*~:*~[~;a line~:;lines~]~* whose other text is set in ~
                                     fonts kept in it: put in by another program."
                                p (length unkept)
                                (remove-duplicates (mapcar (lambda (m) (font-base (mark-font m))) unkept)
                                                   :test #'string=))
                        (format nil "A program that keeps the fonts of a page in the file keeps them all, or leaves ~
                                     out the same few everywhere; a few words of a line in a font left out, where ~
                                     the line's other text is in fonts kept, were set by a program that does not ~
                                     embed, as tools that write over a redaction do.")
                        unkept
                        :details (loop for m in unkept
                                       collect (format nil "~s in ~a~@[ (~a)~]"
                                                       (mark-text m) (font-base (mark-font m)) (mark-place m)))
                        :method :other-program))))
      (check-unlike-runs check unlike place)
      (let* ((all (lines-off-edges lines))
             (prose (remove-if-not (lambda (o) (eq (funcall place (first (second o))) :prose)) all)))
        (loop for off in (list (set-difference all prose :test #'eq) prose)
              for noted in '(nil t)
              when off
                do (page-found check (if noted :record :look)
                               (format nil "On page ~d, ~:d line~:p of text ~:*~[~;is~:;are~] set a little off the ~
                                            edge the lines around ~:*~[~;it~:;them~] keep: placed by hand.~:[~; It ~
                                            lies in running text or boilerplate, where a change would serve no one: ~
                                            for the record.~]"
                                       p (length off) noted)
                               (format nil "A program starts the lines of a block, and ends right-aligned ones, on one ~
                                            edge to a hundredth of a point; a line a fraction of a point off it was ~
                                            placed apart.")
                               (loop for (nil line) in off append line)
                               :details (loop for (side line own usual many) in off
                                              collect (format nil "~s ~:[ends~;starts~] at x ~,2f, ~d other lines at ~
                                                                   ~,2f~@[ (~a)~]"
                                                              (line-text line) (eq side :left) own many usual
                                                              (mark-place (if (eq side :left)
                                                                              (first line)
                                                                              (car (last line))))))
                               :method :placed-by-hand :kind :layout
                               :place (cond (noted :prose)
                                            ((some (lambda (o) (eq (funcall place (first (second o))) :field)) off)
                                             :field))))))))

(defun check-tagged-text (check)
  "Add to CHECK each stretch of text its page draws whose tag says it stands
for other words than show (its /ActualText or /Alt), spaces left aside."
  (let ((groups '()))
    (dolist (m (page-check-texts check))
      (when (mark-actual m)
        (if (and groups (eq (car (first groups)) (mark-actual m)))
            (push m (cdr (first groups)))
            (push (list (mark-actual m) m) groups))))
    (flet ((bare (text) (remove-if (lambda (c) (member (char-code c) '(9 10 13 32 160))) text)))
      (dolist (group groups)
        (let ((shown (format nil "~{~a~}" (mapcar #'mark-text (reverse (cdr group))))))
          (unless (string-equal (bare shown) (bare (car group)))
            (page-found check :look
                        (format nil "On page ~d the text shown, ~s, is tagged as standing for ~s."
                                (page-check-number check)
                                (shorten-string shown 80 :one-line t) (shorten-string (car group) 80 :one-line t))
                        (format nil "A tag's /ActualText or /Alt says what the text it marks stands for; read aloud ~
                                     and copied, it replaces what shows.")
                        (reverse (cdr group)) :method :retyped :kind :layout)))))))

(defun check-typed-on-scan (check)
  "Add to CHECK, when its page is one picture, the visible text drawn over
that picture."
  (let ((scan (page-check-scan check)))
    (when scan
      (let ((visible (remove-if (lambda (m) (or (invisible-text-p m) (< (mark-order m) (mark-order scan))))
                                (page-check-texts check))))
        (when visible
          (page-found check :look
                      (format nil "Page ~d is one picture with text drawn over it: ~s; typed onto a scan."
                              (page-check-number check) (marks-text visible 120))
                      (format nil "A page that is a picture of a whole page holds no text of its own; visible text ~
                                   over it was laid on afterwards.")
                      (cons scan visible) :method :typed-on-scan :kind :typed-on-scan))))))

(defparameter *page-content-checks*
  '((check-covered-text "text covered") (check-unseen-text "text unseen") (check-faint-text "faint text")
    (check-content-streams "content appended") (check-text-set-apart "text set apart")
    (check-drawn-pitch "text drawn a character at a time") (check-column-fonts "fonts of a column")
    (check-tagged-text "tagged text") (check-typed-on-scan "text typed on a scan")
    (check-chat-traces "chatbot traces") (check-figures "check digits and totals")
    (check-text-dates "dates of the page"))
  "The checks of one page's content (CHECK-PAGE-CONTENT) in the order they
run, each (FUNCTION WORDS): FUNCTION, given the PAGE-CHECK; WORDS, what it
looks for, named when it cannot finish.")

(defparameter *layout-habit-most* 2
  "The most lines, runs and figures of text set apart from the rest a file's
pages may name in all for each to stay worth a look (LAYOUT-HABIT); a file
naming more sets text so by habit, and they join one note for the record.
See doc/measures.org.")

(defun layout-finding-p (finding)
  "True when FINDING, of the pages' content, names text set apart from the
rest by how it is set, in a way programs set text too (its kind is
:LAYOUT): placed off its line or edge, fitted, or retyped. A figure in
another font set to pass for its line's own (FIGURE-PASSES-P), and a cell
in another font at its column's size, are of the kind :FIGURE-PASSING and
are not among them."
  (and (eq (finding-strength finding) :look) (eq (finding-kind finding) :layout)
       (member (finding-method finding) '(:placed-by-hand :fitted :retyped))))

(defun layout-habit (findings)
  "FINDINGS of a file's pages' content (CHECK-PAGE-CONTENT), those of text
set apart by how it is set (LAYOUT-FINDING-P) put together in one finding
for the record when they name more than *LAYOUT-HABIT-MOST* lines, runs or
figures in all: then it is how the file's writer sets its text, the count
and the pages said once, each finding's words and details kept in it.
Figures in another font set to pass for their line's own (the kind
:FIGURE-PASSING) are not counted among them, and are put with them only
when they stand on more than one page of the file: a forger sets a few
figures of one page again, a program sets its section numbers and years in
another font on page after page."
  (flet ((named (findings) (reduce #'+ findings :key (lambda (f) (max 1 (length (finding-details f)))))))
    (let* ((set-apart (remove-if-not #'layout-finding-p findings))
           (passing (remove-if-not (lambda (f) (eq (finding-kind f) :figure-passing)) findings))
           (layout (append (and (> (named set-apart) *layout-habit-most*) set-apart)
                           (and (rest (remove-duplicates (mapcar #'finding-page passing))) passing)))
           (count (named layout)))
      (if (null layout)
          findings
          (let ((pages (sort (remove-duplicates (remove nil (mapcar #'finding-page layout))) #'<)))
            (append (remove-if (lambda (f) (member f layout :test #'eq)) findings)
                    (list (found :record
                               (format nil "The file sets ~:d lines, runs or figures of its text apart from the rest, ~
                                            on ~a: how the program that wrote it sets text, for the record."
                                       count (pages-words pages :ranges t))
                               :rule (format nil "A program sets the runs of a line on one baseline and in one size, ~
                                                  and lines on one edge, to a hundredth of a point; a run or a line ~
                                                  set apart was set another way. A forger changes a figure or a ~
                                                  line, but a file that sets more than ~d so is set by a program ~
                                                  that does: a manual, a font sample or a table. The findings are ~
                                                  kept here, each with its place."
                                             *layout-habit-most*)
                               :revision (finding-revision (first layout)) :page (first pages)
                               :objects (remove-duplicates (loop for f in layout append (finding-objects f)))
                               :details (loop for f in layout
                                              collect (format nil "page ~d: ~a" (finding-page f) (finding-words f))
                                              append (finding-details f))
                               :evidence (loop for f in layout for e = (finding-evidence f)
                                               repeat 4 append e)))))))))

(defun check-page-content (pdf revision page)
  "The signs in what PAGE of REVISION draws: text covered by shapes,
invisible, clipped away, outside the page, white or tiny text, content
appended later, text drawn a character at a time or in a font of its own,
figures placed unlike their font's widths, cells of a table set apart from
their column, tagged text unlike what shows, a page that is one picture
with text over it, words a chatbot leaves in text pasted from it, numbers
whose check digit fails, totals that do not add up and dates later than the
file. A picture covering
85 % of the page or more is a scan, and invisible text over it the text
recognition of it, for the record. Each check runs apart: one that fails on
what the file holds adds that it could not finish and leaves the findings of
the others standing."
  (job-check-stop)
  (let* ((marks (marks-of pdf revision page))
         (page-area (* (- (third (page-media page)) (first (page-media page)))
                       (- (fourth (page-media page)) (second (page-media page)))))
         (scan (and (plusp page-area)
                    (find-if (lambda (m) (and (eq (mark-kind m) :image) (>= (area m) (* 0.85 page-area)))) marks)))
         (check (make-page-check pdf revision page marks
                                 (remove-if-not (lambda (m) (eq (mark-kind m) :text)) (coerce marks 'list))
                                 (1+ (page-index page)) scan)))
    (loop for (each words) in *page-content-checks*
          do (guarding-item ((page-check-findings check) "~a on page ~d" words (page-check-number check))
               (funcall each check)))
    (page-check-findings check)))

(defun font-maker-key (base)
  "BASE, a font's name without its subset tag, as two values: what is the same
for every maker's name of its family (arial for ArialMT, Arial,Bold and
Arial-BoldMT; liberationsans for LiberationSans and Liberation Sans
Regular), and how the name was made: \"MT\" when it ends so, as Monotype
names its fonts (ArialMT, Arial-BoldMT, TimesNewRomanPSMT), \"words\" when
it is the font's full name in words with spaces, as a program writes that
embeds a font it found on the machine under the name the font gives itself
(Liberation Sans Regular), else \"\" (Arial, Arial,Bold, LiberationSans)."
  (let* ((words (and (find #\Space base) t))
         (base (if words
                   (let ((joined (remove #\Space base)))
                     (if (uiop:string-suffix-p (string-downcase joined) "regular")
                         (subseq joined 0 (- (length joined) 7))
                         joined))
                   base))
         (cut (position-if (lambda (c) (find c ",-")) base))
         (key (string-downcase (subseq base 0 (or cut (length base))))))
    (dolist (suffix '("psmt" "mt" "ps"))
      (when (and (> (length key) (+ 2 (length suffix))) (uiop:string-suffix-p key suffix))
        (setf key (subseq key 0 (- (length key) (length suffix))))
        (return)))
    (values key (cond (words "words") ((uiop:string-suffix-p (string-downcase base) "mt") "MT") (t "")))))

(defconstant +seconds-1900-to-1904+ (* 4 365 86400)
  "Seconds from 1900 to 1904, where a TrueType font's dates count from.")

(defun font-file-dates (pdf revision font)
  "When FONT's file kept in the PDF says it was made and last changed, as two
universal times, from the head table of a TrueType or OpenType font; NIL for
none, a font file of another kind (a Type 1 or bare CFF font has no dates),
or dates that cannot be (before 1985 or after now)."
  (let* ((data (font-program pdf revision font))
         (at (sfnt-table data "head")))
    (when (and at (<= (+ at 36) (length data)))
      (flet ((u64 (k) (loop for i below 8 sum (ash (aref data (+ k i)) (* 8 (- 7 i))))))
        (let ((made (+ (u64 (+ at 20)) +seconds-1900-to-1904+))
              (changed (+ (u64 (+ at 28)) +seconds-1900-to-1904+))
              (earliest (encode-universal-time 0 0 0 1 1 1985 0)))
          (when (<= earliest changed (get-universal-time))
            (values (and (<= earliest made (get-universal-time)) made) changed)))))))

(defun plain-text-p (text)
  "True when TEXT holds a letter or figure and nothing but the letters A to Z
and a to z, figures, spaces and the marks of amounts and dates: what every
subset of a font for Latin text has room for in its first encoding."
  (and (some #'alphanumericp text)
       (every (lambda (c) (or (char<= #\a c #\z) (char<= #\A c #\Z) (char<= #\0 c #\9) (find c " .,:;/-()%$+")))
              text)))

(defun subsets-set-into-lines (pdf revision base)
  "The runs of plain text (PLAIN-TEXT-P) REVISION's pages show that are set
in one subset of the font BASE and stand in a line with plain text in another
subset of it kept in the same form (FONT-SUBTYPE): each (PAGE-NUMBER WORDS),
WORDS naming the run and its place (MARK-WORDS); of two subsets in a line,
the runs of the one that sets fewer characters there. A program that splits
one font over several subsets does so for characters its first encoding has
no room for (a Type 1 font of more than 256 glyphs, a simple and a composite
font); plain letters and figures in a second subset, in a line of the first,
were embedded apart. NIL for none."
  (loop for page in (pages-of pdf revision)
        for texts = (remove-if-not (lambda (m)
                                     (and (eq (mark-kind m) :text) (mark-font m) (font-subset (mark-font m))
                                          (not (invisible-text-p m)) (string= (font-base (mark-font m)) base)
                                          (plain-text-p (mark-text m))))
                                   (coerce (marks-of pdf revision page) 'list))
        append (loop for line in (page-lines-of texts)
                     append (flet ((characters (subset)
                                     (loop for m in line
                                           when (string= (font-subset (mark-font m)) subset)
                                             sum (mark-characters m))))
                              (loop for m in line
                                    for font = (mark-font m)
                                    when (some (lambda (o)
                                                 (let ((other (mark-font o)))
                                                   (and (string/= (font-subset other) (font-subset font))
                                                        (equal (font-subtype other) (font-subtype font))
                                                        (< (characters (font-subset font))
                                                           (characters (font-subset other))))))
                                               line)
                                      collect (list (1+ (page-index page)) (mark-words page m)))))))

(defun runs-of-least-font (pdf revision page bases &optional (most 8))
  "Lines for a finding's details naming the runs of text PAGE of REVISION
shows in the fonts named BASES (names without subset tags) but the one that
sets the most characters there, each with its place (MARK-WORDS), MOST of
them at most: the text the other program set. NIL when none of them shows
text, or two set the most alike."
  (let ((runs (make-hash-table :test 'equal)))
    (loop for m across (marks-of pdf revision page)
          when (and (eq (mark-kind m) :text) (mark-font m) (not (invisible-text-p m))
                    (member (font-base (mark-font m)) bases :test #'string=)
                    (plusp (length (string-trim " " (mark-text m)))))
            do (push m (gethash (font-base (mark-font m)) runs)))
    (let ((counts (sort (loop for base in (remove-duplicates bases :test #'string=)
                              collect (cons (reduce #'+ (gethash base runs) :key #'mark-characters) base))
                        #'> :key #'car)))
      (when (and (rest counts) (> (car (first counts)) (car (second counts))))
        (loop for m in (loop for (nil . base) in (rest counts) append (reverse (gethash base runs)))
              repeat most
              collect (mark-words page m))))))

(defun numbered-tag-p (tag)
  "True when TAG, a font subset's six letters, is one a program gives that
numbers its subsets from BAAAAA on, as LibreOffice and OpenOffice do
(BAAAAA, CAAAAA ... ZAAAAA, ABAAAA): its last four letters are A. A tag of
letters drawn at random is one in half a million times."
  (and (= (length tag) 6) (every (lambda (c) (char= c #\A)) (subseq tag 2))))

(defparameter *maker-subset-tags*
  '(("libreoffice" numbered-tag-p "BAAAAA, CAAAAA and on") ("openoffice" numbered-tag-p "BAAAAA, CAAAAA and on"))
  "How programs that make PDFs tag the font subsets they embed, each (WORDS
TEST HOW): a file naming WORDS as its producer whose pages use a subset
tagged otherwise than TEST, a function of the tag, allows was not all
written by that program; HOW says the program's way in words. See
doc/measures.org.")

(defun subset-tag-findings (pdf revision faces)
  "The findings of how the fonts of REVISION's pages are tagged, FACES being a
table of each font name without its tag to its uses, each (FONT PAGE-NUMBER
NAME OBJECT-NUMBER): subsets tagged as the producer the file names does not tag
its own (*MAKER-SUBSET-TAGS*), and a numbered tag (NUMBERED-TAG-P) given
twice, to two fonts or to two copies of one, which a program numbering its
subsets does only in separate runs: pages joined from more than one file."
  (let ((producer (string-downcase (or (info-text pdf revision "/Producer") "")))
        (tagged (make-hash-table :test 'equal))
        (out '()))
    (loop for base being the hash-keys of faces using (hash-value uses)
          do (loop for (font page nil number) in uses
                   when (and (font-subset font) number)
                     do (pushnew (list base page number (font-file font))
                                 (gethash (font-subset font) tagged)
                                 :test #'equal)))
    (let ((tags (sort (loop for tag being the hash-keys of tagged collect tag) #'string<)))
      (loop for (words test how) in *maker-subset-tags*
            for other = (and (search words producer) (remove-if test tags))
            when other
              do (push (found :look
                              (format nil "The file names ~a as its producer, but ~:d of the font subsets its pages ~
                                           use ~:*~[~;is~:;are~] not tagged as that program tags its own (~{~a~^, ~}): ~
                                           the pages were written by another program, and the producer's name kept ~
                                           or set."
                                      (info-text pdf revision "/Producer") (length other)
                                      (loop for tag in other repeat 6
                                            collect (format nil "~a+~a" tag (first (first (gethash tag tagged))))))
                              :rule (format nil "A program tags each font subset it embeds in a way of its own (~a for ~
                                                 this one); a file that names it as producer and holds subsets tagged ~
                                                 otherwise had its pages written by another program."
                                            how)
                              :revision (revision-index revision)
                              :objects (loop for tag in other append (mapcar #'third (gethash tag tagged)))
                              :evidence (append (let ((number (nth-value 1 (info-of pdf revision))))
                                                  (and number (key-evidence pdf revision number "/Producer")))
                                                (loop for tag in other repeat 4
                                                      append (key-evidence pdf revision
                                                                           (third (first (gethash tag tagged)))
                                                                           "/BaseFont")))
                              :method :metadata-edited)
                       out)
                 (return))
      (dolist (tag tags)
        (let* ((uses (sort (copy-list (gethash tag tagged)) #'< :key #'second))
               (first-use (first uses))
               (same-p (lambda (use)
                         (and (string= (first use) (first first-use)) (equal (fourth use) (fourth first-use)))))
               (others (remove-if same-p uses)))
          (when (and (numbered-tag-p tag) others)
            (let* ((own-pages (remove-duplicates (mapcar #'second (remove-if-not same-p uses))))
                   (other-pages (remove-duplicates (mapcar #'second others)))
                   (apart (if (< (length own-pages) (length other-pages)) own-pages other-pages)))
              (push (found :look
                           (format nil "The subset tag ~a is given twice, to ~a on ~a and to ~a on ~a: a program that ~
                                        numbers its subsets gives each number once in a file, so the pages were ~
                                        written in separate runs and joined, or a page was put in from another file."
                                   tag (first first-use) (pages-words own-pages :ranges t)
                                   (first (first others)) (pages-words other-pages :ranges t))
                           :rule (concatenate 'string "LibreOffice and OpenOffice number the font subsets of a file "
                                                      "BAAAAA, CAAAAA and on, each once; the same number before two "
                                                      "fonts, or before two copies of one font kept apart, comes from "
                                                      "two exports.")
                           :revision (revision-index revision) :page (first apart)
                           :details (loop for (base page number) in uses
                                          collect (format nil "page ~d: ~a+~a (object ~d)" page tag base number))
                           :objects (mapcar #'third uses)
                           :evidence (loop for use in (list first-use (first others))
                                           append (key-evidence pdf revision (third use) "/BaseFont"))
                           :method :pages-changed)
                    out)
              (return))))))
    out))

(defun check-fonts (pdf revision)
  "One face in several subsets, one face both embedded and not, one family
named by two makers on a page, and a font kept in the file changed after the
document was made: more than a day after, so that a font dated in another
time zone than the document is not named."
  (let ((faces (make-hash-table :test 'equal)) (out '()))
    ;; one family named two ways on a page
    (dolist (page (pages-of pdf revision))
      (let ((spellings (make-hash-table :test 'equal)))
        (loop for (name . value) in (page-font-entries pdf revision page)
              do (let ((font (font-of pdf revision value)))
                   (when (plusp (length (font-base font)))
                     (multiple-value-bind (key spelling) (font-maker-key (font-base font))
                       (pushnew (list spelling (font-base font) name value) (gethash key spellings)
                                :test #'equal :key #'first)))))
        (loop for key in (sort (loop for k being the hash-keys of spellings collect k) #'string<)
              for uses = (reverse (gethash key spellings))
              when (> (length uses) 1)
                do (push (found :look (format nil "On page ~d one font is named the ways two programs name it (~{~a~^, ~
                                                   ~}): text set by another program."
                                              (1+ (page-index page)) (mapcar #'second uses))
                                :rule (concatenate 'string "A program names a font one way throughout (ArialMT, "
                                                           "Arial-BoldMT); the same family named another way (Arial, "
                                                           "Arial,Bold) on the same page was written by another.")
                                :revision (revision-index revision) :page (1+ (page-index page))
                                :details (append (loop for (nil base name) in uses
                                                       collect (format nil "~a as ~a" (subseq name 1) base))
                                                 (runs-of-least-font pdf revision page (mapcar #'second uses)))
                                :objects (remove nil
                                                 (mapcar
                                                   (lambda (u) (and (pdf-ref-p (fourth u)) (second (fourth u))))
                                                   uses))
                                :evidence (loop for u in uses
                                                when (pdf-ref-p (fourth u))
                                                append (key-evidence pdf revision (second (fourth u)) "/BaseFont"))
                                :method :other-program)
                         out))))
    ;; a font kept in the file changed after the document was made
    (let ((made (date-of (info-text pdf revision "/CreationDate"))) (seen (make-hash-table :test 'equal)))
      (when made
        (dolist (page (pages-of pdf revision))
          (loop for (nil . value) in (page-font-entries pdf revision page)
                do (let ((font (font-of pdf revision value)))
                     (when (and (font-file font) (not (gethash (font-file font) seen)))
                       (setf (gethash (font-file font) seen) t)
                       (let ((changed (nth-value 1 (font-file-dates pdf revision font))))
                         (when (and changed (> changed (+ made 86400)))
                           (push (found :look (format nil "The font ~a kept in the file was last changed ~a, after ~
                                                             the document was made (~a): it was put in later."
                                                        (font-base font) (utc-time-words changed) (utc-time-words made))
                                        :rule (concatenate 'string "A program embeds a font as it writes the document, "
                                                                   "and many date the subset they make then; a font "
                                                                   "changed after the document's creation date was "
                                                                   "embedded by a later edit, or that date was set "
                                                                   "back.")
                                        :revision (revision-index revision) :page (1+ (page-index page))
                                        :objects (list (second (font-file font)))
                                        :evidence (append (let ((number (nth-value 1 (info-of pdf revision))))
                                                            (and number
                                                                 (key-evidence pdf revision number "/CreationDate")))
                                                          (nth-value 0
                                                                     (object-evidence
                                                                       pdf
                                                                       revision
                                                                       (second (font-file font)))))
                                        :method :font-added)
                                 out)))))))))
    (dolist (page (pages-of pdf revision))
      (loop for (name . value) in (page-font-entries pdf revision page)
            do (let ((font (font-of pdf revision value)))
                 (when (plusp (length (font-base font)))
                   (push (list font (1+ (page-index page)) name (pdf-ref-p value)) (gethash (font-base font) faces))))))
    (dolist (base (sort (loop for k being the hash-keys of faces collect k) #'string<))
      (let* ((uses (reverse (gethash base faces)))
             (subsets (remove-duplicates
                        (remove nil (mapcar (lambda (u) (font-subset (first u))) uses))
                        :test #'string=))
             (embedded (remove-duplicates (mapcar (lambda (u) (font-embedded (first u))) uses))))
        (flet ((evidence ()
                 (loop for (font nil nil number) in (remove-duplicates uses :key #'fourth) repeat 6
                       when number
                       append (or (key-evidence pdf revision number "/BaseFont")
                                  (nth-value 0 (object-evidence pdf revision number))))))
          (when (> (length subsets) 1)
            (let ((set-in (subsets-set-into-lines pdf revision base)))
              (push (found (if set-in :look :record)
                           (format nil "The font ~a is in the file as ~d subsets (~{~a+~a~^, ~})~:[: embedded part by ~
                                        part, as office programs, form designers and programs that print to PDF ~
                                        embed a font, or pages joined. No line sets plain words or figures in two ~
                                        of them: for the record.~;, and ~:*~:d run~:p of plain words or figures ~
                                        ~:*~[~;stands~:;stand~] in a line set in another of them: set into the ~
                                        line at another time or by another program.~]"
                                   base (length subsets) (loop for s in subsets append (list s base))
                                   (and set-in (length set-in)))
                           :rule (concatenate 'string "A program embeds a font as subsets tagged ABCDEF+Name, and may "
                                                      "split one font over several, each for other text or other "
                                                      "characters its first encoding has no room for. Plain letters "
                                                      "and figures of a line in a second subset of the same form "
                                                      "were embedded apart.")
                           :revision (revision-index revision)
                           :page (and set-in (first (first set-in)))
                           :details (append (loop for (font page name) in uses
                                                  collect (format nil "page ~d: ~a as ~:[~a~;~:*~a+~a~]"
                                                                  page
                                                                  (subseq name 1)
                                                                  (font-subset font)
                                                                  base))
                                            (loop for (number words) in set-in
                                                  collect (format nil "page ~d: ~a" number words)))
                           :objects (remove nil (remove-duplicates (mapcar #'fourth uses)))
                           :evidence (evidence) :method :font-added :kind (and set-in :font-subsets))
                    out)))
          (when (> (length embedded) 1)
            (push (found (if
                           (= 1
                              (length (remove-duplicates
                                        (mapcar (lambda (u) (font-subtype (first u))) uses)
                                        :test #'equal)))
                           :look :record)
                         (format nil "The font ~a is embedded on some pages and not on others: text added by another ~
                                      program, or ~:*~a written two ways by one (a simple and a composite font)." base)
                         :rule "One program writing one document embeds a font everywhere or nowhere."
                         :revision (revision-index revision)
                         :details (loop for (font page name) in uses
                                        collect (format nil "page ~d: ~a ~:[not embedded~;embedded~]"
                                                        page
                                                        (subseq name 1)
                                                        (font-embedded font)))
                         :objects (remove nil (remove-duplicates (mapcar #'fourth uses)))
                         :evidence (evidence) :method :font-added)
                  out)))))
    (append (subset-tag-findings pdf revision faces) out)))
