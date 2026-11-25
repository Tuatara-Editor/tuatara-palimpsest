;;;; producers.lisp --- The programs that make and edit PDFs, and what each leaves in a file.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:palimpsest)

;;; Tools that make and edit PDFs

(defparameter *pdf-tools*
  '(("ilovepdf" "iLovePDF" :editor "an online service for editing PDFs")
    ("smallpdf" "Smallpdf" :editor "an online service for editing PDFs")
    ("sejda" "Sejda" :editor "a PDF editor, online and on the desktop")
    ("pdfescape" "PDFescape" :editor "an online PDF editor")
    ("pdf-xchange" "PDF-XChange" :editor "a PDF editor")
    ("foxit" "Foxit" :editor "a PDF reader and editor")
    ("nitro" "Nitro" :editor "a PDF editor")
    ("pdfelement" "PDFelement" :editor "a PDF editor (Wondershare)")
    ("wondershare" "Wondershare" :editor "the maker of the PDFelement editor")
    ("soda pdf" "Soda PDF" :editor "a PDF editor")
    ("pdftk" "pdftk" :editor "a program that joins, splits and changes PDFs")
    ("qpdf" "qpdf" :editor "a program that rewrites PDFs")
    ("%qdf-" "qpdf's QDF form" :editor "the form qpdf writes a file in for changing it by hand in a text editor")
    ("exiftool" "ExifTool" :editor "a program that reads and rewrites what files say of themselves")
    ("pypdf" "PyPDF" :editor "a library for changing PDFs from Python")
    ("pdfsam" "PDFsam" :editor "a program that joins and splits PDFs")
    ("pdf24" "PDF24" :editor "a set of PDF tools, online and on the desktop")
    ("xodo" "Xodo" :editor "a PDF editor")
    ("pdf expert" "PDF Expert" :editor "a PDF editor")
    ("canva" "Canva" :editor "an online design service")
    ("inkscape" "Inkscape" :editor "a drawing program that opens and saves PDFs")
    ("photoshop" "Adobe Photoshop" :editor "a picture editor")
    ("gimp" "GIMP" :editor "a picture editor")
    ("mutool" "MuPDF tools" :editor "a program that rewrites PDFs")
    ("bluebeam" "Bluebeam" :editor "a PDF editor")
    ("power pdf" "Kofax Power PDF" :editor "a PDF editor")
    ("acrobat" "Adobe Acrobat" :editor "Adobe's PDF editor")
    ("ghostscript" "Ghostscript" :maker "a program that makes and rewrites PDFs")
    ("libreoffice" "LibreOffice" :maker "an office suite")
    ("openoffice" "OpenOffice" :maker "an office suite")
    ("microsoft: print to pdf" "Microsoft Print to PDF" :maker "Windows' printing to PDF")
    ("quartz" "Quartz" :maker "macOS's own PDF writer, used by Preview and printing")
    ("preview" "Preview" :editor "macOS's viewer, which can change PDFs")
    ("itext" "iText" :maker "a library that makes and changes PDFs")
    ("reportlab" "ReportLab" :maker "a library that makes PDFs")
    ("aspose" "Aspose" :maker "a library that makes and changes PDFs")
    ("pdfium" "PDFium" :maker "the PDF library of Chrome")
    ("skia" "Skia" :maker "the drawing library of Chrome, which prints to PDF")
    ("distiller" "Acrobat Distiller" :maker "Adobe's program that makes PDFs from print files")
    ("pdfmaker" "Acrobat PDFMaker" :maker "Adobe's add-in that makes PDFs from office documents")
    ("microsoft" "Microsoft Office" :maker "an office program")
    ("docusign" "DocuSign" :maker "a signing service")
    ("paint.net" "Paint.NET" :editor "a picture editor")
    ("affinity photo" "Affinity Photo" :editor "a picture editor")
    ("pixelmator" "Pixelmator" :editor "a picture editor")
    ("krita" "Krita" :editor "a painting program and picture editor")
    ("paintshop" "PaintShop Pro" :editor "a picture editor")
    ("lightroom" "Adobe Lightroom" :editor "a photograph editor")
    ("snapseed" "Snapseed" :editor "a photograph editor for phones")
    ("picsart" "Picsart" :editor "a picture editor for phones")
    ("pixlr" "Pixlr" :editor "an online picture editor")
    ("photopea" "Photopea" :editor "an online picture editor")
    ("imagemagick" "ImageMagick" :maker "a program that converts and changes pictures")
    ("graphicsmagick" "GraphicsMagick" :maker "a program that converts and changes pictures")
    ("midjourney" "Midjourney" :generator "an image generator")
    ("dall-e" "DALL-E" :generator "OpenAI's image generator")
    ("dall·e" "DALL-E" :generator "OpenAI's image generator")
    ("chatgpt" "ChatGPT" :generator "OpenAI's chat service, which makes pictures")
    ("firefly" "Adobe Firefly" :generator "Adobe's image generator")
    ("stable diffusion" "Stable Diffusion" :generator "an image generator")
    ("stablediffusion" "Stable Diffusion" :generator "an image generator")
    ("comfyui" "ComfyUI" :generator "a program that runs image generators")
    ("invokeai" "InvokeAI" :generator "a program that runs image generators")
    ("automatic1111" "AUTOMATIC1111" :generator "a program that runs image generators")
    ("fooocus" "Fooocus" :generator "a program that runs image generators")
    ("novelai" "NovelAI" :generator "an image generator")
    ("dreamstudio" "DreamStudio" :generator "an image generator")
    ("ideogram" "Ideogram" :generator "an image generator")
    ("leonardo.ai" "Leonardo.Ai" :generator "an image generator")
    ("nightcafe" "NightCafe" :generator "an image generator")
    ("craiyon" "Craiyon" :generator "an image generator")
    ("bing image creator" "Bing Image Creator" :generator "Microsoft's image generator")
    ("microsoft designer" "Microsoft Designer" :generator "Microsoft's design service, which makes pictures")
    ("google imagen" "Google Imagen" :generator "Google's image generator")
    ("black forest labs" "FLUX" :generator "an image generator")
    ("flux.1" "FLUX" :generator "an image generator")
    ("diffusionbee" "DiffusionBee" :generator "a program that runs image generators")
    ("draw things" "Draw Things" :generator "a program that runs image generators"))
  "Programs that make or change PDFs and pictures, as they name themselves:
each (TEXT NAME KIND WORDS), TEXT what they write (in lower case), KIND
:editor for one used to change PDFs or pictures, :maker for one that makes
them, :generator for an image generator or a program that runs one.")

(defun tools-in (text &key generators)
  "The entries of *PDF-TOOLS* TEXT names, the first for each name; none for
NIL. An entry named only within another named entry's name is that one's:
\"Acrobat Distiller\" names Distiller, not Adobe Acrobat; \"Acrobat
Distiller, then Adobe Acrobat Pro\" names both. Image generators only with
GENERATORS, for a field that names a program: their names are words of
everyday text too (draw things, firefly)."
  (when text
    (let* ((text (string-downcase text))
           (named (remove-duplicates (remove-if-not (lambda (tool)
                                                      (and (or generators (not (eq (third tool) :generator)))
                                                           (search (first tool) text)))
                                                    *pdf-tools*)
                                     :key #'second :test #'string= :from-end t)))
      (flet ((places (words)
               (loop for at = (search words text) then (search words text :start2 (1+ at))
                     while at collect at)))
        (remove-if (lambda (tool)
                     (let ((own (first tool)))
                       (every (lambda (at)
                                (some (lambda (other)
                                        (let ((name (string-downcase (second other))))
                                          (and (not (eq other tool))
                                               (some (lambda (from)
                                                       (<= from at (+ at (length own)) (+ from (length name))))
                                                     (places name)))))
                                      named))
                              (places own))))
                   named)))))

(defun program-names (pdf)
  "The programs each save of PDF names, from the first save, each (PLACE
REVISION TEXT): PLACE is where, \"/Producer\" or \"/Creator\" of its document
information, or \"XMP Producer\" or \"XMP CreatorTool\" of its XMP."
  (kept-for-pdf (pdf :program-names)
    (loop for r across (pdf-revisions pdf)
          append (let ((fields (xmp-of pdf r)))
                   (loop for (place text) in (list (list "/Producer" (info-text pdf r "/Producer"))
                                                   (list "/Creator" (info-text pdf r "/Creator"))
                                                   (list "XMP Producer" (and fields (xmp-get fields "Producer")))
                                                   (list "XMP CreatorTool" (and fields (xmp-get fields "CreatorTool"))))
                         when (stringp text) collect (list place r text))))))

(defun file-tools (pdf)
  "The entries of *PDF-TOOLS* that any save of PDF names in its document
information's producer or creator, or its XMP's producer or creator tool."
  (kept-for-pdf (pdf :file-tools)
    (remove-duplicates (loop for (nil nil text) in (program-names pdf) append (tools-in text))
                       :key #'second :test #'string=)))

(defun ids-alike-p (pdf revision)
  "True when REVISION's trailer gives no /ID, or gives its two parts alike,
as a program writes them when it makes a file; a program writing the file
again changes the second."
  (let ((id (pdf-array-items (pdf-get (revision-trailer revision) "/ID"))))
    (or (/= (length id) 2) (not (stringp (first id))) (not (stringp (second id)))
        (string= (first id) (second id)))))

(defparameter *pdf-rewriters*
  '("mupdf" "mutool" "qpdf" "pikepdf" "pypdf" "itext" "pdfbox" "hexapdf" "pdf-lib" "pdfsharp" "pdftk" "ghostscript"
    "libharu" "podofo" "reportlab" "fpdf" "tcpdf" "pdfkit" "pdfcpu" "unipdf" "aspose" "spire.pdf" "apryse" "pdftron")
  "Words naming a program or library that can write a PDF anew from another,
as found in lower case in a file's producer, creator or comments. Any of
them named takes away that a shape over text was drawn as the file was made
\(DRAWN-AS-MADE-P). A file rewritten by one of them may have had a shape
added before it was written.")

(defun rewriter-named-p (pdf)
  "True when a save of PDF names, in its producer or creator, its XMP's
producer or creator tool, or a comment between its objects
\(FILE-COMMENTS), a program of *PDF-REWRITERS*."
  (kept-for-pdf (pdf :rewriter-named)
    (let ((texts (append (mapcar #'third (program-names pdf)) (mapcar #'third (file-comments pdf)))))
      (some (lambda (text)
              (and (stringp text)
                   (let ((text (string-downcase text))) (some (lambda (w) (search w text)) *pdf-rewriters*))))
            texts))))

(defparameter *maker-trailer-marks*
  '(("libreoffice" "/DocChecksum") ("openoffice" "/DocChecksum"))
  "Marks a program that makes PDFs writes in the trailer of every file it
makes, each (WORDS KEY): a file whose first save names WORDS as its
producer and whose trailer lacks KEY, or whose KEY does not hold
\(MAKER-MARK-HOLDS-P), was written again by another program or changed
after. LibreOffice and OpenOffice write /DocChecksum, the MD5 digest of the
file's octets up to its trailer.")

(defun maker-mark-holds-p (pdf key)
  "True when the mark KEY in the trailer of PDF's first save holds: for
/DocChecksum, the MD5 digest (OCTETS-MD5) of the file's octets from its start
up to the keyword trailer after the first save's cross-reference table."
  (let* ((first (aref (pdf-revisions pdf) 0))
         (value (pdf-get (revision-trailer first) key)))
    (cond ((null value) nil)
          ((string= key "/DocChecksum")
           (let* ((octets (pdf-octets pdf))
                  (from (or (revision-xref-at first) 0))
                  (at (and (integerp from) (<= 0 from (length octets))
                           (octets-search (load-time-value (string-octets "trailer")) octets :start from))))
             (and at (stringp value) (= (length value) 33)
                  (string-equal (subseq value 1)
                                (let ((cache (pdf-cache pdf)))
                                  (or (getf cache :doc-checksum)
                                      (setf (getf (pdf-cache pdf) :doc-checksum) (octets-md5 octets :end at))))))))
          (t t))))

(defun maker-marks-missing-p (pdf)
  "True when PDF's first save names a program of *MAKER-TRAILER-MARKS* as
its producer and its trailer lacks that program's mark, or the mark does
not hold (MAKER-MARK-HOLDS-P)."
  (kept-for-pdf (pdf :maker-marks-missing)
    (let* ((first (aref (pdf-revisions pdf) 0))
           (producer (string-downcase (or (info-text pdf first "/Producer") ""))))
      (loop for (words key) in *maker-trailer-marks*
            thereis (and (search words producer) (not (maker-mark-holds-p pdf key)))))))

(defun maker-mark-finding (pdf)
  "The finding of a mark of *MAKER-TRAILER-MARKS* that PDF's first save
carries and that does not hold (MAKER-MARK-HOLDS-P), in a file naming that
program as its producer: strong when the two parts of the save's /ID are
alike, as the program writes them, for then no program wrote the file again
and its octets were changed where they lay; for the record when they
differ, as a program that writes a file again leaves the mark and changes
the ID. NIL when the mark holds, is not there, or another program is named."
  (let* ((first (aref (pdf-revisions pdf) 0))
         (producer (string-downcase (or (info-text pdf first "/Producer") "")))
         (key (loop for (words key) in *maker-trailer-marks*
                    when (and (search words producer) (pdf-get (revision-trailer first) key)
                              (not (maker-mark-holds-p pdf key)))
                      return key)))
    (when key
      (let ((in-place (and (ids-alike-p pdf first) (pdf-get (revision-trailer first) "/ID"))))
        (found (if in-place :strong :record)
               (format nil "The file's first save carries ~a, the checksum ~a writes of every octet before its ~
                            trailer, and the octets no longer give it~:[: a program wrote the file again and kept ~
                            the old checksum, as the two unlike parts of its /ID say too.~;, while the two parts of ~
                            its /ID are alike, as that program writes them: the file was changed where it lay ~
                            after it was written, by no program that says so.~]"
                       key (info-text pdf first "/Producer") in-place)
               :rule (concatenate 'string "LibreOffice and OpenOffice write the MD5 digest of the file's octets, "
                                          "from its start to its trailer, into the trailer as /DocChecksum. A "
                                          "program that writes the file again changes the second part of /ID; a "
                                          "checksum that fails beside two like parts is a file changed in place.")
               :revision 0
               :evidence (list (format nil "the trailer of the first save, at ~a, holds ~a"
                                       (offset-words (or (revision-trailer-at first) 0)) key))
               :method (and in-place :structure-by-hand))))))

(defun structure-mcids (pdf revision page)
  "Whether REVISION is a tagged PDF (its catalogue has a structure tree or
says it is marked), and the numbers of PAGE's marked content its structure
tree names (/MCID, directly or as a marked-content reference), in a table;
the tree followed 100,000 entries at most. Worked out once for each
revision and page."
  (let ((kept (kept-for-pdf (pdf :structure-mcids) (make-hash-table :test 'equal)))
        (key (cons (revision-index revision) (page-index page))))
    (values-list
     (or (gethash key kept)
         (setf (gethash key kept)
               (multiple-value-list
                 (let* ((catalogue (catalogue pdf revision))
                        (root (and catalogue (deref pdf revision (pdf-get catalogue "/StructTreeRoot"))))
                        (marked (let ((m (deref pdf revision (pdf-get catalogue "/MarkInfo"))))
                                  (pdf-true-p (pdf-get m "/Marked"))))
                        (out (make-hash-table)) (seen (make-hash-table)) (budget 100000)
                        (number (page-number page)))
                   (labels ((walk (value pg)
                              (when (plusp (decf budget))
                                (cond ((pdf-ref-p value)
                                       (unless (gethash (second value) seen)
                                         (setf (gethash (second value) seen) t)
                                         (walk (deref pdf revision value) pg)))
                                      ((integerp value) (when (and pg number (eql pg number))
                                                          (setf (gethash value out) t)))
                                      ((pdf-dictionary-p value)
                                       (let ((pg (or (pdf-ref-p (pdf-get value "/Pg")) pg)))
                                         (let ((mcid (pdf-get value "/MCID")))
                                           (when (and (integerp mcid) pg number (eql pg number))
                                             (setf (gethash mcid out) t)))
                                         (walk (pdf-get value "/K") pg)))
                                      ((consp value) (dolist (item (pdf-array-items value)) (walk item pg)))))))
                     (when root (walk (pdf-get root "/K") nil)))
                   (values (or (and root t) marked) out))))))))

(defparameter *replacement-size* 0.05
  "How near in size, as a share of the hidden text's, the own text of a box
over text is taken to be set as the hidden text is (REPLACEMENT-P), room for
a size rounded where it is written.")

(defparameter *replacement-baseline* 1.0
  "How near, in points, the baseline of a box's own text is taken to lie to
the hidden text's line (REPLACEMENT-P): new text set over old lies on the old
line, where an office program sets a text box's own text inside its
margins, further off.")

(defun replacement-p (own hidden)
  "True when the run of text OWN is set as the text HIDDEN is, each (RUN FROM
TO), a run with the part of it, as fractions along its baseline, that is
OWN's or hidden. It has the same font family, a size within
*REPLACEMENT-SIZE* of it, its baseline within *REPLACEMENT-BASELINE* points
of its line, and the middle of OWN within the hidden part along it. This is
new text set in place of old, as an editor changing a figure does."
  (destructuring-bind (run from to) hidden
    (let ((frame (mark-frame run)) (own-frame (mark-frame own)))
      (and frame own-frame (mark-font own) (mark-font run)
           (equal (font-family (mark-font own)) (font-family (mark-font run)))
           (< (abs (- (mark-size own) (mark-size run))) (* *replacement-size* (max (mark-size run) 1)))
           (let* ((ax (aref frame 2)) (ay (aref frame 3)) (length (sqrt (+ (* ax ax) (* ay ay)))))
             (and (plusp length)
                  (let ((nx (- (/ ay length))) (ny (/ ax length))
                        (dx (- (aref own-frame 0) (aref frame 0))) (dy (- (aref own-frame 1) (aref frame 1))))
                    (<= (abs (+ (* dx nx) (* dy ny))) *replacement-baseline*))
                  (multiple-value-bind (mx my) (run-middle own)
                    (let ((along (/ (+ (* (- mx (aref frame 0)) ax) (* (- my (aref frame 1)) ay)) (* length length))))
                      (<= from along to)))))))))

(defun file-as-written-p (pdf revision)
  "True when REVISION of PDF reads as the program that wrote it left it: its
/ID as its maker wrote it (IDS-ALIKE-P), no program named that writes PDFs
anew, its comments too (REWRITER-NAMED-P), its maker's trailer marks there
and holding (MAKER-MARKS-MISSING-P), and no program named that edits PDFs
\(FILE-TOOLS)."
  (and (ids-alike-p pdf revision)
       (not (rewriter-named-p pdf))
       (not (maker-marks-missing-p pdf))
       (not (find :editor (file-tools pdf) :key #'third))))

(defun box-tagging (pdf revision page box own)
  "How the structure tree of PAGE in REVISION of PDF names the box BOX and
its own text OWN: :TAGGED when the page is tagged and the tree names the
marked content of each (STRUCTURE-MCIDS), :UNTAGGED for an untagged page or
none given, NIL when the page is tagged and one of them is outside it, as a
box appended to a page is."
  (if page
      (multiple-value-bind (tagged mcids) (structure-mcids pdf revision page)
        (cond ((not tagged) :untagged)
              ((every (lambda (m) (and (mark-mcid m) (gethash (mark-mcid m) mcids))) (cons box own)) :tagged)))
      :untagged))

(defun block-alone-p (mark marks keep-p)
  "True when MARK is drawn in an outermost q ... Q block (MARK-BLOCK) holding
no other of MARKS drawn by its content stream but those KEEP-P is true of."
  (and (mark-block mark)
       (every (lambda (other)
                (or (eq other mark)
                    (funcall keep-p other)
                    (not (eql (mark-source other) (mark-source mark)))
                    (not (eql (mark-block other) (mark-block mark)))))
              marks)))

(defun cells-alone-p (box own covered marks)
  "True when the box BOX and its own text OWN are drawn in an outermost
q ... Q block of their own, over the text COVERED each in a block of its own
too, among the page's MARKS, as LibreOffice Calc draws each cell and each
shape."
  (and (every (lambda (m) (eql (mark-block m) (mark-block box))) own)
       (block-alone-p box marks (lambda (m) (member m own)))
       (every (lambda (c) (block-alone-p c marks (lambda (m) (not (eq (mark-kind m) :text))))) covered)))

(defun own-text-as-made-p (own covered hidden alone)
  "True when OWN, a box's own text, is set in fonts kept in the file of the
family of the text COVERED (FONT-FAMILY), in any font kept in the file when
ALONE (CELLS-ALONE-P), and none of it is set as the hidden text is over the
part hidden (REPLACEMENT-P, HIDDEN giving each covered run with the part of
it hidden, (RUN FROM TO)): that is new text in place of old, not a text
box's own."
  (let ((families (loop for m in covered when (mark-font m) collect (font-family (mark-font m)))))
    (and (every (lambda (m) (and (mark-font m) (font-embedded (mark-font m))
                                 (or alone (member (font-family (mark-font m)) families :test #'equal))))
                own)
         (notany (lambda (m) (some (lambda (h) (replacement-p m h)) hidden)) own))))

(defun drawn-as-made-p (pdf revision box covered within &optional page hidden)
  "True when the filled shape BOX was drawn over the text COVERED as the PDF
was made, not laid on it later: a text box, as an office program prints one
with a white fill over a spreadsheet's cell. It has text of its own drawn on
it after it, of WITHIN, the texts whose middle it covers, set as a text box's
own is (OWN-TEXT-AS-MADE-P, HIDDEN giving the parts of COVERED hidden). BOX,
all of COVERED and its own text are drawn by one content stream, written in
the original save, within the outermost q ... Q block the hidden text is
drawn in (MARK-BLOCK). The file reads as its maker wrote it
\(FILE-AS-WRITTEN-P) and names a program that makes PDFs. On a tagged PAGE
the structure tree names the box and its text (BOX-TAGGING); there they may
be drawn in a block of their own over covered text in blocks of its own
\(CELLS-ALONE-P), and set in another font kept in the file. A box laid over
text by a PDF editor fails one of these: it comes in a later save, in a
content stream of its own, with an editor named, the /ID written again, or
its text in a font of the editor's own. A forger who passes them all still
leaves the finding naming the text hidden under the box, worth a look."
  (let* ((own (remove-if-not
                (lambda (m) (and (> (mark-order m) (mark-order box)) (eql (mark-source m) (mark-source box))))
                within))
         (hidden (or hidden (mapcar (lambda (run) (list run 0 1)) covered)))
         (tagging (box-tagging pdf revision page box own))
         (alone (and (eq tagging :tagged) (cells-alone-p box own covered (marks-of pdf revision page))))
         (blocks (remove nil (mapcar #'mark-block covered))))
    (and own (mark-source box) tagging (not (later-source-p pdf revision box))
         (own-text-as-made-p own covered hidden alone)
         (file-as-written-p pdf revision)
         (find :maker (file-tools pdf) :key #'third)
         (or (null blocks) alone
             (every (lambda (m) (and (mark-block m) (member (mark-block m) blocks))) (cons box own)))
         (every (lambda (m) (eql (mark-source m) (mark-source box))) covered)
         t)))
