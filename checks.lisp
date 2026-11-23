;;;; checks.lisp --- Every check run on a file, the row each has in the report's Checks, and what each looks for.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; A check looks for one way of editing a file that a reader of the report
;;;; knows by name. Each is defined once here, in the order they run and the
;;;; report lists them: its label, the explanation its label opens, the kinds
;;;; of file it is run on, why it is not run on a file it has nothing to read
;;;; in, and the parts it runs. Every finding a check makes carries its key,
;;;; so that the report sums them up by check.

(in-package #:palimpsest)

(defstruct (check (:constructor make-check (key label explanation on reason parts)) (:copier nil))
  "One check, a row of the report's Checks: KEY, a keyword naming it; LABEL,
the row's name; EXPLANATION, for the reader of the report, the way of
editing it looks for, what that leaves, how the check finds it and what
honest files can show alike; ON, the kinds of file it is run on (:pdf,
:picture); REASON, a function of the PDF, its last revision, its accounts
and the picture file (NIL for a PDF) giving why the check is not run on
that file, in words, or NIL; PARTS, functions of the same and a progress
function, each giving findings."
  key label explanation on reason parts)

(setf (documentation 'check-p 'function) "True when OBJECT is a CHECK.")

(defvar *checks* '()
  "Every check, in the order they run and the report's Checks lists them (DEFINE-CHECK).")

(defun register-check (check)
  "Put CHECK in *CHECKS*: in the place of the check of its key, or last."
  (let ((place (member (check-key check) *checks* :key #'check-key)))
    (if place
        (setf (car place) check)
        (setf *checks* (append *checks* (list check))))
    (check-key check)))

(defmacro define-check (key label explanation (&key (on '(:pdf :picture)) unless) &body parts)
  "Define the check KEY, its row in the report's Checks named LABEL,
EXPLANATION what its label opens (CHECK). ON, the kinds of file it is run
on, :pdf and :picture, both unless given. UNLESS, a form giving why the
check is not run on a file, in words, or NIL when it is. Each of PARTS, a
form giving findings, is run on its own: one that fails leaves the others'
findings standing. The forms are evaluated with PDF, LAST (its last
revision), ACCOUNTS (its revisions compared), FILE (the PICTURE-FILE, NIL
for a PDF) and, in PARTS, PROGRESS (a function of the fraction of the part
done) bound."
  (let ((names '(pdf last accounts file)))
    `(register-check
      (make-check ,key ,label ,explanation ',on
                  (lambda ,names (declare (ignorable ,@names)) ,unless)
                  (list ,@(loop for part in parts
                                collect `(lambda (,@names progress)
                                           (declare (ignorable ,@names progress))
                                           ,part)))))))

(defun find-check (key)
  "The check of KEY in *CHECKS*, or NIL."
  (find key *checks* :key #'check-key))

(defun check-name (check)
  "CHECK's label as it reads within a sentence: its first letter small, unless its first word is a short form (JPEG)."
  (let ((label (check-label check)))
    (if (and (> (length label) 1) (lower-case-p (char label 1)))
        (string-downcase label :end 1)
        label)))

;;; Why a check is not run

(defun scanned-reason (pdf last)
  "\"no scanned pages\" when no page of LAST, PDF's last revision, draws a
scanned picture (SCAN-MARKS); NIL when one does."
  (kept-for-pdf (pdf :scanned-reason last)
    (and (null (scan-marks pdf last)) "no scanned pages")))

(defun text-layer-reason (pdf last)
  "\"no text layer\" when no page of LAST, PDF's last revision, names a font; NIL when one does."
  (and (notany (lambda (page) (page-font-entries pdf last page)) (pages-of pdf last)) "no text layer"))

(defun pictures-reason (pdf last)
  "\"no pictures\" when no page of LAST, PDF's last revision, draws a picture; NIL when one does."
  (and (null (pictures-of pdf last)) "no pictures"))

(defun jpeg-reason (pdf last file)
  "Why the JPEG checks are not run: for FILE, a picture file, \"not a JPEG\"
when it is none; for PDF, \"no JPEG pictures\" when LAST, its last
revision, draws none; NIL otherwise."
  (if file
      (and (not (eq (picture-file-kind file) :jpeg)) "not a JPEG")
      (and (null (jpeg-pictures pdf last)) "no JPEG pictures")))

;;; Running them

(defun run-check (check pdf last accounts file progress)
  "The findings of CHECK on PDF (LAST its last revision, ACCOUNTS its
revisions compared, FILE the picture file it stands for or NIL), each
marked with CHECK's key; and as a second value how it went: NIL when it
ran, why it was not run in words, or :UNFINISHED when it, or a part of it,
failed on what the file holds or ran out of stack or memory, the finding
saying so (UNFINISHED) among its findings. PROGRESS is called with the
fraction of CHECK done."
  (let ((findings '()) (outcome nil) (parts (check-parts check)))
    (flet ((failed (condition)
             (setf outcome :unfinished)
             (push (unfinished (check-name check) condition) findings)))
      (handler-case (setf outcome (funcall (check-reason check) pdf last accounts file))
        ((or error storage-condition) (e) (failed e)))
      (unless outcome
        (loop for part in parts for k from 0
              do (job-check-stop)
                 (let ((k k))
                   (handler-case
                       (setf findings (revappend (funcall part pdf last accounts file
                                                          (lambda (x) (funcall progress (/ (+ k x) (length parts)))))
                                                 findings))
                     ((or error storage-condition) (e) (failed e)))))))
    (dolist (f findings) (setf (finding-check f) (check-key check)))
    (values (nreverse findings) outcome)))

(defun all-findings (pdf accounts &key file (progress (constantly nil)))
  "Every sign found in PDF, ACCOUNTS being its revisions compared, by every
check of *CHECKS* run on a PDF; with FILE, a picture file examined as PDF,
its one page, by every check run on a picture instead. The findings come
the strongest first, then by revision and page, each placed in a revision
\(the latest when it is of the file as a whole); and as a second value
each of those checks' outcomes in order, (KEY . OUTCOME) as RUN-CHECK
gives it. PROGRESS is called with a number from 0 to 1."
  (let* ((last (last-revision pdf))
         (checks (remove-if-not (lambda (check) (member (if file :picture :pdf) (check-on check))) *checks*))
         (outcomes '())
         (findings (loop for check in checks for k from 0
                         append (let ((k k))
                                  (multiple-value-bind (found outcome)
                                      (run-check check pdf last accounts file
                                                 (lambda (x) (funcall progress (/ (+ k x) (length checks)))))
                                    (push (cons (check-key check) outcome) outcomes)
                                    found)))))
    (funcall progress 1)
    (values (sorted-findings findings last) (nreverse outcomes))))

;;; The checks

(define-check :saves "Later saves"
  "A PDF can be changed without writing it anew: the program adds what it
changes at the end of the file, a later save, and leaves the original
before it. Most editing of a finished PDF leaves such a save behind. The
check counts the saves the file keeps and names each page a later save
changes, so that the revisions can be set side by side. Honest files have
later saves too: a form filled in, a signature added, a reader's notes or
a viewer that saves on closing all write one."
  (:on (:pdf))
  (check-saves pdf accounts))

(define-check :cross-references "Cross-reference sections"
  "Each save ends in a cross-reference section, the file's index of where
each object begins, and a trailer naming the section before it. Editing a
file by hand, or with a tool that patches it in place, leaves an index that
no longer fits: entries pointing beside the object they name, a section
naming no earlier one, a save indexed in another way than the first, or a
file prepared for fast web viewing changed after. The check reads every
section against the octets of the file. Some honest programs leave empty
slots in their index, and many store later saves in compressed object
streams; those are named worth a look or for the record only."
  (:on (:pdf))
  (check-revision-problems pdf)
  (check-offsets pdf)
  (check-index-kinds pdf)
  (check-packed-changes pdf))

(define-check :objects "Objects freed, unused or written twice"
  "A PDF is made of numbered objects. Removing or replacing something frees
objects, or leaves them in the file with nothing using them; an edit done
in haste writes an object twice in one save or reuses a number without
raising its generation. The old objects keep what was there before. The
check follows every reference from the document's root and lists what
nothing reaches, what is freed and what is written twice. Programs that
save carelessly also leave unused objects, so these tell where to look
rather than prove an edit."
  (:on (:pdf))
  (check-generations pdf)
  (check-leftovers pdf)
  (check-twice pdf))

(define-check :file-ends "Data before and after the file"
  "A reader of a PDF looks for its first line and its last %%EOF and skips
anything outside them. Data appended after the last save, or put before
the file begins, is a place to hide a message or an earlier version. The
check reads what lies beyond those ends and says what it is. A file sent
by mail or copied from a disc can carry a few honest octets of padding
there, which are named as such."
  (:on (:pdf))
  (check-ends pdf))

(define-check :hidden-white "White space hiding data"
  "Between a PDF's objects any amount of white space may stand, and a
reader skips it. A program hiding a message can write it as a pattern of
spaces, tabs and line ends there. The check looks for long runs of white
space within the saves that mix their kinds as such a code writes them.
Writers pad with runs of one kind; a run of spaces between two line ends
is not named."
  (:on (:pdf))
  (check-hidden-white pdf))

(define-check :hidden-saves "Saves hidden in streams"
  "A stream holds a page's drawing, a picture or a font, and its stated
length tells a reader where it ends. A save can be hidden inside a stream
whose length is written to swallow it, or a stream left open so that a
reader passes over the save that follows. The check sets each stream's
stated length beside where it really ends, and looks for %%EOF lines and
object headers inside the data of streams that are not embedded files.
A PDF kept inside another as an attachment holds those honestly, and is
not named."
  (:on (:pdf))
  (check-unended-streams pdf)
  (check-lying-streams pdf)
  (check-save-signs pdf)
  (check-lengths pdf))

(define-check :comments "Comments between the objects"
  "Lines beginning with % between a PDF's objects are comments, which no
reader shows. Some editing tools write their name there, and some people
hide notes in them. The check lists the comments and the programs they
name. Several honest writers leave a comment of their own as well."
  (:on (:pdf))
  (check-comments pdf))

(define-check :metadata "Metadata and dates"
  "A file says of itself which program made it and when it was made and
changed: in a PDF, its document information and its XMP metadata; in a
picture, its EXIF, XMP and other fields. An editor writes its own name
and a new date, or forgets to change one of two places that should agree.
The check reads every field, sets the dates against each other and against
today, and names programs that edit files. Dates differ honestly by time
zone, and a picture turned or cropped in a phone's gallery is saved again
with a new date, so these are worth a look at most unless they cannot be
true."
  ()
  (if file (check-picture-metadata file) (check-metadata pdf)))

(define-check :page-content "What the pages draw"
  "A page is drawn by a list of instructions. Editing text in a PDF often
leaves the old text under a white box, makes it invisible, clips it away,
moves it off the page, or adds new text drawn a character at a time in a
font of its own. The check reads every page's instructions and looks for
text covered, hidden or clipped, text appended later, figures placed unlike
their font's widths, table cells set apart from their column, tagged text
unlike what shows, and words a chatbot leaves in pasted text. Some honest
programs also draw a character at a time or leave text under shapes, so
most of these are worth a look rather than strong; a file that sets more
than two lines, runs or figures apart sets its text so throughout, and they
are said once for the record."
  (:on (:pdf) :unless (and (null (pages-of pdf last)) "no pages"))
  (let* ((pages (pages-of pdf last)) (count (length pages)) (out '()))
    (loop for page in pages for k from 1
          do (job-check-stop)
             (funcall progress (/ k count))
             (guarding-item (out "page ~d" k)
               (setf out (revappend (check-page-content pdf last page) out))))
    (layout-habit (nreverse out))))

(define-check :fonts "Fonts and widths"
  "Text typed into a finished PDF has to use a font, and the editor often
brings its own copy: a second subset of the same face, the face both
embedded and not, a family named the way another program names it, or
the same font with other widths for the same characters. The check sets
the fonts of the pages against each other. A document joined from parts
made by different programs shows some of the same, so these are worth a
look at most."
  (:on (:pdf) :unless (text-layer-reason pdf last))
  (check-fonts pdf last)
  (check-font-widths pdf last))

(define-check :font-maps "Text that copies as other text"
  "A font maps each code to the shape it draws and, apart from that, to the
letter it stands for when text is copied or searched. A figure changed by
editing the font's map rather than the page shows one number and copies as
another. The check compares the two maps of every font a page draws with.
Old or badly made fonts can disagree honestly, so this is worth a look."
  (:on (:pdf) :unless (text-layer-reason pdf last))
  (check-font-maps pdf last))

(define-check :pictures "Pictures unlike the others"
  "A picture pasted into a document, such as a signature or a logo, often
differs from the rest in how it is stored, its colours or the program that
made it, or was added in a later save. The check sets the pictures of the
file against each other and reads what JPEG pictures say of the programs
that made them. Documents assembled honestly from many sources mix their
pictures too, so a picture kept otherwise is for the record, and one added
in a later save worth a look."
  (:on (:pdf) :unless (pictures-reason pdf last))
  (check-images pdf last))

(define-check :scans-read "Scanned pages read"
  "The checks of the scanned pages read their pixels. This row says what
could not be read or judged, so that no page goes unexamined without its
saying so: pictures in a form not read here or too large, pages whose
characters are too alike or too few to tell prints from copies, bands of
lines run together, characters with too little ink left once clipped ink
is set aside. Nothing here is a sign of editing; it bounds what the other
rows can say."
  (:unless (scanned-reason pdf last))
  (check-scans-read pdf last))

(define-check :rendered-pages "Pages drawn by a program"
  "A scanned page carries the grain of paper and a scanner's blur. A page
made by opening a document in a picture editor, or by printing it as a
picture, is drawn by a program from text and has neither. The check looks
for whole pages of perfectly flat paper whose text edges are smoothed into
half-tones. A screenshot, a page printed to a picture by an app, or a scan
whose paper was whitened until it is one grey look the same, so this is
worth a look."
  (:unless (scanned-reason pdf last))
  (check-rendered-pages pdf last))

(define-check :unlike-words "Words unlike their neighbours"
  "A word pasted or typed into a scanned page rarely matches the words
around it: its ink is darker or lighter, its edges sharper or softer, its
paper cleaner, or it sits off the line or turned against it. The check
measures each word against those beside it and names those that stand
apart, more so those apart both in how they look and in where they sit.
Real scans have uneven ink, stains and folds, so these are worth a look at
most; a word off its line by its baseline alone by more than 5 pixels,
a word in running text or boilerplate, and a block of lines set apart
together, as headings and shaded rows are on honest pages, are for the
record."
  (:unless (scanned-reason pdf last))
  (check-unlike-words pdf last))

(define-check :copied-characters "Characters copied, grain and all"
  "To change a figure on a scanned page, an editor copies another character
of the page over it, so that the ink and grain match. But a scanner never
prints a character twice alike: each print differs in its grain and in
where the blur falls on the pixels. The check sets every character beside
the others of its shape and names those that share a block of pixels the
same, grey level for grey level, with the grain of a print, and those
copied and then saved again as a JPEG, which stay nearer each other than
prints come. Near pairs of pieces, pairs in running text or boilerplate
and the pairs of a page naming more than five are for the record.
A stamp or a signature laid on each page by software is the
same every time honestly, and clean prints on a page kept as a JPEG can
come near each other by chance; those are named worth a look."
  (:unless (scanned-reason pdf last))
  (check-copied-characters pdf last))

(define-check :bare-marks "Characters the same pixel for pixel"
  "Characters drawn into a picture by a program, as a picture editor's text
tool or an office's stamp software draws them, come out the same pixel
for pixel every time, and marks so drawn have no grain of their own. The
check names characters that repeat exactly where a program could have
drawn them: in an ink or at an angle unlike the page's text, far lighter
than it, or on a page whose prints come out alike (a JPEG kept without
loss, few grey levels, black and white smoothed, faint grain); and marks
without grain that repeat exactly. Honest stamps, light printed forms and
a scanner that clips dark ink to one grey make prints alike in the same
way, so this is worth a look."
  (:unless (scanned-reason pdf last))
  (check-bare-marks pdf last))

(define-check :typed-text "Text typed into the pixels"
  "Text typed onto a scanned page with a picture editor's text tool is drawn
by the program in one flat ink, while every character a scanner prints
strays by some grey levels however wide its strokes. The check names words
and lines whose characters have no grain of their own. Bold or heavier
type, a stamp, coloured ink or a scanner that clips dark ink can look the
same, so this is worth a look."
  (:unless (scanned-reason pdf last))
  (check-typed-text pdf last))

(define-check :text-layer "Text layer against the pixels"
  "A scanned PDF often carries an invisible text layer, made by reading the
page, so that it can be searched. When the picture is edited and the layer
is not, the two disagree: the layer holds words over blank paper, or the
picture holds words the layer lacks. The check sets each page's layer
against its pixels. A layer read badly by the scanner's software disagrees
honestly, so this is worth a look at most."
  (:on (:pdf) :unless (or (scanned-reason pdf last) (text-layer-reason pdf last)))
  (check-text-layer pdf last))

(define-check :jpeg-grid "Pieces off the JPEG grid"
  "A JPEG stores a picture in blocks of 8 by 8 pixels on a grid. A piece cut
from one picture, saved as a JPEG of its own and laid into another keeps
its own blocks, off the page's grid; a piece laid in after the page was
saved lacks the blocks the rest carries. The check measures each region's
blocks against the page's grid. A region is strong only when another sign
agrees on it, since honest software that lays a logo or a stamp on a scan
leaves the same; one lacking the page's blocks that no other sign meets is
for the record."
  (:unless (scanned-reason pdf last))
  (check-scan-pieces pdf last))

(define-check :jpeg-traces "Regions without the JPEG's traces"
  "Every part of a page saved as a JPEG carries the traces of that save.
Paint laid over the page afterwards, or a region pasted from a picture
never saved so, lacks them; a page joined from strips can have one strip
saved with other tables than the rest. The check measures the traces tile
by tile and names regions that lack them or were painted flat. Very smooth
paper, or a scanner that sets its quality strip by strip, can look the
same, so these are worth a look at most."
  (:unless (scanned-reason pdf last))
  (check-local-traces pdf last))

(define-check :copied-paper "Paper copied over paper"
  "A clone stamp or a content-aware fill removes a word by copying paper from
elsewhere on the page over it. The paper brings its grain along, and no
two patches of a scanner's paper are ever alike pixel for pixel. The check
searches pictures kept without loss for patches of paper near the words
that match other paper of the page exactly at one shift. A form's printed
pattern repeating at one shift matches too, so this is worth a look."
  (:unless (scanned-reason pdf last))
  (check-copied-paper pdf last))

(define-check :generator-grid "An image generator's pattern"
  "An image generator draws its picture from one eight times smaller, and
its decoder leaves a faint pattern repeating every 8 by 8 pixels that
changes along rows and columns at once. A scanner, a camera and a JPEG
leave none such. The check measures that pattern in the flat parts of
pictures kept without loss. A picture tiled with a faint pattern of that
size, or a JPEG saved twice on grids out of step, carries one too, so this
is worth a look."
  (:unless (scanned-reason pdf last))
  (check-generator-grid pdf last))

(define-check :repeats "Pictures and marks repeated"
  "No hand signs twice alike and no stamp presses twice alike. A signature
or a stamp copied from one place to another is the same in both. The check
lists pictures drawn in more than one place and lays the large marks of
the scanned pages, those the size of a signature or a stamp, over each
other pixel by pixel. Offices lay the same stamp or logo on their letters
honestly, so whether the marks should differ is for the examiner to judge."
  (:unless (pictures-reason pdf last))
  (check-repeats pdf last))

(define-check :resolution "Scan resolution"
  "How sharp the edges of the ink are tells the resolution a page was really
scanned at, whatever its file says. A page made larger, or pieced together
from scans of different resolutions, shows edges softer than its pixels.
The check measures the edges of each scanned page and names the
resolution, or says it was made larger. A page blurred, out of focus or
printed soft cannot be told from one made larger, so this is for the
record."
  (:unless (scanned-reason pdf last))
  (check-resolution pdf last))

(define-check :jbig2 "Pages kept as JBIG2 symbols"
  "JBIG2 can store a black and white page as a dictionary of character
shapes, drawing each character from the shape it is nearest. Such a
scanner can put one figure in the place of another, and makes prints of a
character the same everywhere, which the copy checks then cannot judge.
The check reads only the segment headers and names the pages so kept.
This is for the record."
  (:on (:pdf) :unless (scanned-reason pdf last))
  (check-jbig2 pdf last))

(define-check :saved-twice "A JPEG saved twice"
  "A JPEG opened in a picture editor and saved again is rounded a second
time with other steps, and its frequencies no longer fall evenly over the
steps of its last save. The check measures how far they stray and names
the tables the JPEG uses, those of a common library at a quality or a
device's own. A scanner or phone app that saves twice, a viewer that
turned the picture, or an honest crop does the same, and it names no
place, so it is for the record."
  (:on (:picture) :unless (jpeg-reason pdf last file))
  (check-saved-twice file))

(define-check :jpeg-headers "JPEG headers and thumbnails"
  "A camera or scanner writes its own make of JPEG header and a small
thumbnail of the picture in its EXIF. A picture editor writes another
header, and an edit made after the thumbnail leaves the thumbnail showing
the picture as it was. The check sets the header beside the device the
EXIF names and the thumbnail beside the picture. A picture turned with its
old thumbnail kept, or a device unknown here, can differ honestly."
  (:unless (jpeg-reason pdf last file))
  (if file (check-jpeg-header-file file) (check-jpeg-headers pdf last)))

(define-check :making "Content credentials and generator marks"
  "Some programs record how a picture or a document was made: content
credentials (C2PA), the digital source type in XMP, and the marks image
generators write. The check reads them, as data and nothing more, and
names a picture an image generator says it made, or a later edit the
credentials record. Credentials can be stripped, so their absence says
nothing."
  ()
  (if file (check-picture-making file) (check-made-by-generator pdf last)))

(define-check :annotations "Annotations"
  "Annotations are laid over a page rather than drawn in it: notes,
stamps, form widgets and boxes. An annotation that looks like the page, a
white box over a figure with a new figure on it, can change what a reader
sees without touching the page. The check names annotations that look like
the page, were added later, are hidden, or are marked for redaction and
never applied. Honest reviewers add notes and stamps too."
  (:on (:pdf))
  (check-annotations pdf last))

(define-check :forms "Form fields"
  "A form's fields hold values apart from the page. The check names fields
whose values change from one save to the next, and XFA forms, which can
show other content than the PDF's own pages. Filling a form in honestly
changes its values too; the saves show when and what."
  (:on (:pdf))
  (check-forms pdf))

(define-check :layers "Layers hidden as the file opens"
  "A PDF can hold layers that are off when it opens, so their content is
in the file but not shown. Text or a picture can be kept there unseen, or
an old version left under a new one. The check names the layers off as the
file opens. Drawings and maps hide layers honestly."
  (:on (:pdf))
  (check-layers pdf last))

(define-check :signatures "Signatures"
  "A digital signature covers the octets of the file up to its save; what
later saves change is outside it. The check reads what each signature
claims, whether its octets cover its whole save, what later saves change,
and objects hidden in the signed part and brought into use later. Signing
more than once, or filling in a form after signing, adds later saves
honestly."
  (:on (:pdf))
  (check-signatures pdf))

(define-check :pages "Pages added, removed or moved"
  "Pages can be put in, taken out or put in another order in a later save,
and page labels changed to hide it. A page brought in from another
document often differs in size or make from the rest. The check sets each
save's pages against the save before, and the pages against each other.
Documents joined honestly from several files mix their pages too."
  (:on (:pdf))
  (check-pages pdf))

(define-check :actions "Actions and embedded files"
  "A PDF can carry JavaScript, actions run on opening, programs to launch,
addresses to visit and files kept inside it. None of these is run here;
they are named. They can change what a reader sees or hide a document
inside the document. Many honest forms carry scripts and links."
  (:on (:pdf))
  (check-actions pdf last))

(define-check :encryption "Encryption"
  "Encryption limits who can open a PDF and what they may do with it.
Encryption added in a later save, or permissions changed, tell of a file
handled after it was made. The check names the encryption of each save
and its permissions. Many honest files are encrypted against printing or
copying from the start."
  (:on (:pdf))
  (check-encryption pdf))
