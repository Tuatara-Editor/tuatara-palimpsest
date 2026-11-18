# palimpsest

A PDF taken apart into every save it holds, from the original to the latest
edit, and searched for every sign of editing or forgery that examiners of
documents know. A picture (PNG, JPEG or TIFF), as a scanner, a phone or a
picture editor saves one, is examined as one scanned page. One of the Tuatara
project's packages, not part of the editor.

Everything is read from the file's octets, by the package itself: no other
program reads the PDF, nothing in it is run (no JavaScript, no actions, no
launching), nothing it names is fetched, and the file is never changed. What
the package writes (each revision as a PDF of its own, and its log) goes to a
directory of its own in Tuatara's cache directory, for you alone, never beside
the evidence.

## Loading it, step by step

1. **Install what it needs.** Nothing to read PDFs; bubblewrap, ImageMagick and
   poppler to read scanned pages and to draw pages (`d`):

   | System | Command |
   |---|---|
   | Fedora | `sudo dnf install bubblewrap ImageMagick poppler-utils` |
   | Ubuntu, Debian | `sudo apt install bubblewrap imagemagick poppler-utils` |
   | Arch, Omarchy | `sudo pacman -S bubblewrap imagemagick poppler` |

2. **Open your init file**: in Tuatara press `C-x C-f`, type
   `~/.config/tuatara/init.lisp` and press `RET`.
3. **Add these lines** to it (the first line only once, whichever of the
   project's packages you load):

   ```lisp
   (load-package "palimpsest" :source "https://github.com/Tuatara-Editor/tuatara-palimpsest")
   ```

4. **Save** with `C-x C-s`, **quit** with `C-x C-c`, and **start Tuatara
   again**. Answer `y` when it asks whether to load palimpsest; it asks once.
5. **Check it**: open a PDF and run `M-x palimpsest`.

## Using it

`M-x palimpsest` examines the PDF or picture the current buffer shows (a
PDF's or picture's view, a visited file, the file on rimu's line), or asks for
one (always, with `C-u`). The report, `*Palimpsest: NAME.pdf*`, is made in the
background: `P` is lit in the mode line, `C-x j` says how far it has got, and
the editor never waits.

The report has three parts:

1. **Summary**: the file's size and SHA-256, its PDF version, how many
   revisions it holds, the verdict, how many findings of each strength, its
   dates and the programs it names.
2. **Findings**, strongest first, one line each with what it means, the
   revision, page and objects it is in, and its details under it:
   - *Strong signs of editing*: what an honest program does not leave.
   - *Worth a look*: what editing leaves, and some honest programs too.
   - *For the record*: what the file holds that an examiner will want to know.
3. **Revisions**, from the original to the latest: where each begins and
   ends in the file (in hex), its cross-reference section and trailer, the
   dates and program it claims, the objects it adds, changes and frees, the
   pages it touches, each page's text set line by line beside the revision
   before, and what each page draws that it did not (text, filled shapes,
   pictures, with where on the page, in points from its bottom left).

A picture file has no revisions: its summary gives its kind and size in
pixels instead, and its findings are those of a scanned page (words unlike
the rest, characters copied, pieces with JPEG blocks of their own, regions
short of the JPEG traces the rest carries, paper painted over, paper copied
from elsewhere on it, a signature or a stamp the same in two places), what the
picture says of itself,
and, for a JPEG, whether it was saved twice and whether its header and
thumbnail agree with the device its EXIF names. A place
on it is given in points from its bottom left as on a PDF's page, a point to
a pixel, and in pixels from its top left. `d` and `D` draw the page that
stands for the picture with the signs found on it framed (the same frames,
labels and key as on a scanned page of a PDF, no frames for objects), written as
`NAME-drawing.pdf` in the report's own directory by `d` and where you ask by
`D`, never beside the picture; the picture is read again from its file for it,
and refused if it has changed since it was examined. The keys that act on
revisions and objects (`o`, `w`, `t`, `T`) say there are none; `s` saves the
report and the fingerprint.

`M-x palimpsest-compare-files` sets the PDFs of a directory beside one
another (those named `.pdf`, not those of the directories under it, at most
200), to find a signature, a stamp or a seal the same in two of them. It
lists each picture that is no scan kept octet for octet in more than one
file, with where each file draws it, and each pair of marks of two files'
scanned pages, as large as a signature or a stamp, that are one picture
(read as within one file, below). The files are only read; it runs in the
background and takes as long as examining each of them. A company's logo,
or a signature it keeps as a picture, is honestly the same in every letter:
whether they should differ is for you to judge.

## Keys

| Where                                 | Key            | What it does                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| ---                                   | ---            | ---                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| The report                            | `RET`          | On `revision-N.pdf`, opens that revision in Tuatara's PDF view; on `revision-N.log`, opens its log; on an object's number, shows the object.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| The report                            | `TAB`, `S-TAB` | Go from one link to the next.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| The report                            | `s`            | Saves the revisions, their logs and the report (`M-x palimpsest-save`): see below.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |
| The report                            | `o`            | Shows an object: where it is in the file, its dictionary as written, its stream decoded, as text, or in hex in a buffer of its own when it is not text (in hex too with `C-u o`); `RET` on that buffer's name shows it. Asks for its number, offering the one at point. Read in the background, the report usable meanwhile.                                                                                                                                                                                                                                                                                                                                                                  |
| The report, a revision opened from it | `w`            | Writes a revision as a PDF of its own, to show to others, its log beside it, and opens it (`M-x palimpsest-write-revision`): the revision whose part of the report the cursor is in, or the revision you are looking at; else it asks which, offering the one before the latest.                                                                                                                                                                                                                                                                                                                                                                                                              |
| The report                            | `d`            | Shows beside the report a copy of the PDF with each thing its pages draw framed and named by its object, and on a scanned page the signs found framed by their strength (`M-x palimpsest-draw-objects`; `q` there, or `d` again, closes it). On a picture file, the page that stands for the picture with only its scan signs framed (strong in red, worth a look in blue dashed, characters not judged in dark green dotted, clipped ink hatched grey, the key at its foot), written to the report's own directory as `NAME-drawing.pdf`, never beside the picture. |
| The report                            | `D`            | Writes that drawing to a PDF of its own, asked where, never beside the PDF or picture examined (`M-x palimpsest-write-drawing`); for a picture file offered as `NAME-drawing.pdf`. |
| The report                            | `C-x C-w`      | Writes the report alone as text, offered as `NAME-revision-report.txt` in the directory last saved to, else in your home directory (`M-x palimpsest-write-report`); never beside the PDF examined nor in the package's directory, nothing written over.                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| The report                            | `t`            | Shows beside the report the shapes of the characters of the PDF's scanned pages (`M-x palimpsest-show-shapes`, also `C-c p t`), for when a scan shows no copies but something still feels off: each page's characters of one size side by side, grouped by their shape (made black and white, scaled to their lines), those that match no other shape on their page last, each with its line and place in it. No finding: palimpsest reads no text and claims nothing of them; bold, italic, headings, figures and forms filled in differ honestly too. Each character leads to its place on its scanned page, which follow the pages of shapes. Characters not judged (clipped by other ink) are framed dotted, as `d` frames them, and labelled "not judged: clipped", with the key line, both where they are set and on the scanned page. Made in the background; `t` again closes it. |
| The report                            | `T`            | Writes those pages of shapes to a PDF of its own, to keep or show (`M-x palimpsest-write-shapes`, also `C-c p T`), asked where as `C-x C-w` asks.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| Either                                | `q`            | Leaves its window; the report stays, to come back to.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| The report                            | `C-x k`        | Closes it, and the revisions and logs opened from it, and deletes its own copies of them; when they were not saved it asks first: `Revisions and logs not saved: close and delete them? (y or n)`.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            |

## Each revision written out, with its log

Each revision is written to the package's directory as `revision-N.pdf`: the
file's octets from its start to the end of that revision's `%%EOF` line, as
the file stood when that revision was saved. Beside it, `revision-N.log`
says, so that an examiner can do it all again with a hex editor and nothing
else:

- the source file's name, size and SHA-256, when the log was written and by
  which version of the package;
- how the revision was found: the offsets (hex and decimal) of its `%%EOF`,
  its `startxref` and the offset it names, its cross-reference section and
  trailer (the trailer's octets in hex and as text), the `/Prev` it names
  and whose section that is, where its octets were cut, and the SHA-256 of
  `revision-N.pdf` to check the cut by;
- every sign found in it: the rule that found it, the evidence (object
  numbers, the offsets of the objects and of the octets that matter, those
  octets in hex and as text: a `/ModDate`, the operators
  `1 1 1 rg 70 695 100 20 re f` at an offset of a decoded content stream,
  the font names `ABCDEF+Arial` and `GHIJKL+Arial`), and the steps to see it
  by hand (go to offset X; the object begins `12 0 obj`; its stream is
  `/FlateDecode`, so many octets from offset Y; inflate them; at offset Z of
  the inflated octets...);
- the objects it adds, changes and frees, with both places of each changed
  object;
- what changed from the revision before: each page's text and drawing, with
  the offsets in the decoded content streams they came from.

The same file gives the same log, but for the line saying when it was
written, so that two examiners' logs can be compared.

### Keeping them

The package's own directory is cleared: the report's copies go when it is
closed, and anything left there after a day. To keep the work, `s` in the report
(or `M-x palimpsest-save` there) asks for a directory, made when it is not
there, and copies every `revision-N.pdf` and `revision-N.log` into it, with the
report itself as `NAME-revision-report.txt`. Each copy's SHA-256 is checked
against its original; the PDFs and logs are copied octet for octet, and the
report ends with where each copy is and its SHA-256. The copying runs in the
background. It refuses the PDF's own directory (nothing is written beside the
evidence) and the package's directory in the cache, and never writes over a
file: when any of these names is in the directory already, nothing is copied and
the report says which are there. The summary then says where the work was saved,
and closing the report no longer asks.

### One version to show

The `revision-N.pdf` links open the report's own copies, to look at; they go
with the report. To hand one version to someone else, `w` writes it wherever you
say (offered as `NAME-revision-N.pdf` in the directory last saved to, else in
your home directory), with its log beside it as `NAME-revision-N.log`, each
copy's SHA-256 checked, and opens it. Which revision: the one whose part of the
report (under `* Revisions`) the cursor is in; in a revision you opened from the
report, that one; elsewhere in the report it asks, offering the one before the
latest. The PDF examined is never changed; nothing is written over, nor beside
the PDF examined, nor into the package's directory. The PDF written is the file
as it stood after that save, so palimpsest run on it finds the saves up to it.

## What each finding means

### How the file is put together

| Finding | What it means |
|---|---|
| The file was changed after it was first saved | It holds incremental updates: each later save's objects follow the first's, each with its own cross-reference section and `%%EOF`. Signing and filling forms do this too; the revisions show what each changed. |
| Page N was changed in revision M | A later save changed what a page draws: its text (line by line) and drawing (item by item) are compared with the save before. |
| A cross-reference entry does not point at its object | The offsets the index gives are wrong: the file was edited by hand, or repaired (strong). When, in the first save, the objects the entries name are written nowhere in the file, they are empty slots that cairo and Quartz leave in their index, and the finding is worth a look. An entry that misses an object written elsewhere, or any wrong entry in a later save, stays strong. |
| startxref names an offset where no section is | The same, for the whole index. |
| Objects freed, numbers used again without a higher generation | What a save removed is still in the file; reusing a number without raising its generation is not what writers do. |
| Objects no longer used | Objects no reference reaches: leftovers of earlier content, or objects kept to be used later (see shadow attacks). |
| Objects written but listed by no section | Hidden from every reader, or left by an edit. |
| An object written twice in one save | Written by hand or by a careless program. |
| Data after the last `%%EOF` or before `%PDF-` | Hidden data, a save cut short, or a file made to be read as two kinds. A few lines of comments alone (up to 1 KB, each beginning with `%`) are worth a look: a printing system's banner pages carry their instructions so. One octet repeated (padding) is worth a look; 64 octets or more mixing kinds of white space (NULs, tabs, form feeds, spaces, line ends) are strong, the kinds named and counted. |
| White space mixing its kinds within the file | 64 octets or more of white space outside the streams' data (between objects, after a stream's data, between the trailer and `startxref`, between `startxref` and `%%EOF`), mixing two kinds besides line ends, eight or more of the second, are strong, as after the last `%%EOF`, the kinds named and counted. Line ends of any kind, what strings and comments hold, and the data of a stream whose end is not where its `/Length` says are left out. Of some 13,900 honest PDFs none held such a run; writers pad with one kind, and end and indent lines as they please. |
| A save's cross-reference of another kind than the original's, a hybrid file, a linearised file changed | Saved by another program than made it. |
| Comments between objects | Notes programs leave, sometimes naming themselves. |
| A stream's `/Length` disagrees with where `endstream` is | Changed by hand, or by a program that did not write the length again. |

### What the file says of itself

| Finding | What it means |
|---|---|
| Document information and XMP disagree | One was changed without the other: dates, producing and creating programs, title. |
| Last changed before it was made, a date in the future, a time zone no place uses | A date set by hand, or a clock set wrong. Changed before made by a gap an honest clock can leave is worth a look: dates in two zones up to 14 hours apart, an hour or less, or about 12 hours (a twelve-hour clock); any other gap is strong. |
| An edit history in the XMP | Each save a program kept, and the program that saved it. |
| Made from another document | XMP's `OriginalDocumentID` or `DerivedFrom` names another document. |
| A program that edits PDFs is named | The producer, creator or XMP names a PDF editor or online service (iLovePDF, Smallpdf, Sejda, PDFescape, PDF-XChange, Foxit, Nitro, PDFelement, Soda PDF, pdftk, qpdf, PyPDF, PDFsam, Acrobat after another program, Preview, Canva...). |
| A save names another program, removes metadata, gives another first `/ID` | Each save was made by whatever program it names; clearing metadata, or another document's ID, is done on purpose. |
| The file's dates out of order between saves | For the record: a save giving another creation date than the save before (more than a minute apart, zones allowed for), a save last changed more than a minute before the save before it, or the XMP's `MetadataDate` more than a minute before its `CreateDate`. A program that sets the creation date anew on each save, or a clock that slipped, gives the same. |

### A picture file

| Finding | What it means |
|---|---|
| What the picture says of itself | For the record: the device (EXIF's make and model), the programs and the dates it names, and every field of its EXIF, XMP, PNG text and time, JPEG comments and TIFF tags, read as data. |
| A picture editor is named | Worth a look: EXIF's `Software`, XMP's `CreatorTool` or edit history, or a comment ("Created with GIMP") names GIMP, Photoshop, Paint.NET, Affinity Photo, Pixelmator, Krita, Lightroom, Snapseed, Picsart, Pixlr, Photopea and the like, the more so beside a camera's or scanner's own name. An honest crop, turn or lightening is saved so too, and some scanner and phone apps name such a program. |
| Made and changed at different times | Worth a look: when the picture says it was made (`DateTimeOriginal`, `xmp:CreateDate`) and last changed (`DateTime`, `xmp:ModifyDate`, a PNG's `tIME`) lie more than a minute apart, or more than 26 hours when one names no zone. A picture turned in a phone's gallery or saved again by a viewer gives that too. |
| A JPEG saved twice | Worth a look: its frequencies fall unevenly over its steps, some all but empty, as a second save with other steps leaves them. A scanner or phone app that saves twice, a viewer that turned it, an honest crop do the same. Measured only where enough of its blocks hold no clipped pixel; a second save at the same or a coarser quality is often not told. |
| The JPEG's tables | For the record: libjpeg's standard table at a quality, as most programs write, or a table of its own, as many cameras and scanners keep. |
| A device named, a program's header | Worth a look: the EXIF names a camera, phone or scanner (`Make`, `Model`), no picture editor is named, and the header is one a program writes: Adobe's APP14 segment, or a frame other than the baseline one (progressive). Saved again by a program that kept the EXIF. A device's own app that saves again, a gallery, messaging or scanning app, or an honest crop or turn does the same. With an editor named, or a header as a device writes it, the segments, a JFIF segment before the EXIF (which the EXIF standard does not allow, but scanner drivers and phones write as programs built on libjpeg do), frame, sampling, Huffman tables and steps are given for the record. The same for a JPEG picture in a PDF. |
| An EXIF thumbnail unlike its picture | Worth a look: the small copy a device keeps in the EXIF, set beside the picture made as small in each of its eight turns and mirrorings, with a lightening or a change of contrast fitted away, is unlike it as a whole (correlation under 0.8), or differs in one cell (a tenth of the picture across) by 20 grey levels or more and 0.4 or more of the spread there: the picture was changed after the thumbnail was made and the old one kept. A thumbnail up to 1024 pixels across or down is read, made no more than 320 across first; a larger one is for the record only. An honest crop or straightening by a program that keeps the old thumbnail does the same; a program that writes a new thumbnail leaves no sign. The place, and the octets to cut the thumbnail out by, are given. Measured on one camera's pictures, 55 honest ones (31 as the camera saved them, 24 saved again whole, turned, lighter or with less contrast) gave 0.966 or more and no cell past 0.22; regions a sixth of the picture across laid in were told in 16 of 18, a sixteenth across in 1 of 6. The same for a JPEG picture in a PDF. |
| Made by an image generator | Worth a look, never strong: the picture says an image generator made it or a part of it. Its content credentials (C2PA, in a JPEG's APP11 segments, a PNG's `caBX` chunk or a TIFF's tag 52545) have an action whose digital source type is `trainedAlgorithmicMedia` or `compositeWithTrainedAlgorithmicMedia`, or name a generator as the program (`algorithmicMedia` alone is said as made by a program, as a fractal is; a generator named only in the credentials of a file it was made from, as a part made so); or its XMP's `Iptc4xmpExt:DigitalSourceType` says so; or a PNG keeps a generator's own text (Stable Diffusion's `parameters`, ComfyUI's `prompt` and `workflow`, InvokeAI's, SwarmUI's, NovelAI's settings); or `Software`, `CreatorTool`, an edit history or a PNG's `Source` names Midjourney, DALL-E, Adobe Firefly, Stable Diffusion, NovelAI and the like. Names in comments and descriptions are passed over. A file keeping more than one store of credentials has every store read, the last manifest of the last taken as its own, and that is recorded. The picture's size is named beside it when generators often make it (1024 by 1024, 832 by 1216...), never alone. It tells how the picture was made, not that the document is false. The credentials' signer and its certificate's issuer are named as written, and the signature is not verified. |
| Pixels as an image generator draws them | Worth a look, never strong: the flat parts of a picture kept without loss (a PNG or TIFF, or a scanned page a PDF keeps without loss) carry a faint pattern repeating every 8 by 8 pixels and changing along rows and columns at once, as the decoder of an image generator leaves it, faint against the picture's own noise. Paper too smooth to weigh by (its noise smoothed away by a JPEG), or too little of it, is not measured. A JPEG's blocks and a scanner's sensor leave patterns along whole rows and columns, which are not counted; a dither or a half-tone screen is far stronger, and is not taken for one. A picture once saved as a JPEG is not measured, as two JPEG saves out of step make such a pattern; one whose first save left no trace to tell, or a picture tiled with a faint pattern of that size, can still be named. Pictures kept as JPEGs, or as JPEGs in a TIFF, are not measured: a JPEG save, or a print scanned again, takes the pattern away. A picture made larger after it was generated is seldom named. |
| Content credentials that name no generator | For the record: what they say (the program, the actions and their digital source types, the files it was made from, the signer), as written and not verified. A pointer to credentials kept elsewhere (`dcterms:provenance`) is recorded, not fetched. Most generated pictures carry no credentials or marks at all, so none found says nothing. |

### What the pages draw

| Finding | What it means |
|---|---|
| Text covered by a shape drawn over it | A white shape of any outline (strong) or another colour (a black one is a redaction drawn over text, the text still in the file): the covered text is named. Worth a look, its cause named, for an office program's text box drawn as the file was made, a panel over a watermark, an outlined callout, labels over labels and a stamp at a slant. |
| Invisible text | Text in render mode 3 or 7; over a picture of the whole page it is a scan's text recognition, recorded and set against the picture (below). |
| Figures or letters that copy as others than they show | Worth a look, never strong: a font's encoding names a code's glyph as one figure or letter (by `/Differences`, a name such as `seven` or `uni0037`, or by the WinAnsi or MacRoman encoding of a font not marked symbolic and, when it is a TrueType font kept in the file, holding a cmap from Unicode; a subset with only a cmap of its own codes is drawn by its codes, whatever encoding it names) while its ToUnicode map gives another figure or letter for it. The page shows one amount, and copying, searching and programs that read the text get another. Each font is named with its codes, the page and the text as it shows and as it copies. Ligatures, symbols mapped to private places, a letter in the other case and maps of several characters are passed over, as honest programs write them. Glyph names kept only inside the font program (a TrueType `post` table, a CFF charset) are not read, so a font of two-octet codes, or one with no encoding given, as LibreOffice and many others write them, is not checked. |
| Text clipped away, outside the page, in a layer that is off | Text in the file that is never seen. |
| White text on a white page, tiny text | Text no one can see or read. |
| A page drawn from several content streams, one appended later | Something was laid over the page by a later save. |
| A line drawn a character at a time, or spaced oddly | Unlike the rest of the page: retyped, or fitted into a gap. |
| Figures in another font than their line | A retyped number. |
| One font in several subsets, embedded on some pages and not others | Text set at different times or by different programs, or pages joined. |
| A figure placed unlike its own font's widths | Worth a look, never strong: four or more characters of a figure in a row, two different pairs or more among them (one pair kerned again and again, as Helvetica kerns "1 1", is not named), each moved on by 30 thousandths of the size or more, the same way, beyond what the font's `/Widths` give (TJ numbers, or a move of its own before each character), where at least 90 % of the other steps within words on the line keep to the widths. A program places each character where the one before ends and moves the text on only to kern a pair or spread the spaces; a figure placed for other widths was set again with another font, or at the positions of the figures it replaced. Text in fonts that give no widths (the standard fonts written without them) and fonts placed unlike their widths all over the page are left out. On the honest run of 977 files (62 PDFs written by programs, 70 real documents, 845 honest scans) and 85 more born-digital PDFs (LibreOffice, LaTeX, Skia) none was named. |
| One font name with two sets of widths | Worth a look: two embeddings of one font (two subset tags, or two font files), written in different saves of the file, that give other widths to two or more of the same characters, not figures alone: two versions of the font, from two machines or programs, or a stand-in for it, as when a later save sets text again. The text each draws is quoted. Left alone: one embedding written with two encodings (small capitals, old-style figures), two embeddings written in one save (a font split by cairo or Skia, OpenType alternates, two documents merged) and figures alone differing (tabular and proportional figures). |
| A cell of a table set apart from its column | Worth a look: a figure in a column of four or more (figures a size or more apart from the words beside them, their right edges or decimal points within 0.3 points) set in another font, size or dark ink than every other figure of the column, those alike, and than the rest of its row. Left alone: a row set apart as a whole (a bold total, a header), the column's own family in bold or italic (a best value set off), red negatives and highlights, a cell spaced apart unlike its column (a font's table of glyphs), a column of single figures, and fonts that give no widths. Of the same honest files none is named. |
| Tagged text unlike what shows | `/ActualText` or `/Alt` says the text stands for something else. |
| A page that is one picture with text over it | Text typed onto a scan. |
| Words a chatbot leaves | Worth a look, never strong: the page's text (its text recognition over a scan too) keeps what a chatbot writes about itself ("As an AI language model", "as of my knowledge cutoff", "som en AI-språkmodell"), the labels of its page copied with an answer ("Regenerate response", "ChatGPT said:"), its citation marks (`oaicite`, `†source`) or the tracking part of the links it gives (`utm_source=chatgpt.com`), each quoted with its page. Whole phrases of a fixed list are matched, a phrase broken over two lines too. A bracketed place left to fill ("[Insert date]", "[Your name]", "[Infoga datum]") is named as well, and said to be what an office template leaves too. A page that quotes a chatbot keeps the same words. |
| A number whose check digit fails | Worth a look, never strong: a Swedish organisation number (also inside a VAT number, SE and its ten digits and 01), personal identity or coordination number, bankgiro number or OCR payment reference whose last digit is not the check digit by the modulus 10 method, or an IBAN that does not give remainder 1 modulo 97, each named with its page, the digit it should end in and the line it stands in. A number is taken only right after its label (Org.nr, Personnummer, Bankgiro or BG, OCR, IBAN), written as such numbers are (556036-0793, 811218-9876, 5050-1055, 1234 5678 97, SE45 5000 0000 0583 9825 7466), a label ending one line with its number on the next under the label or after it (or alone on that line); digits are not grouped across a gap between columns, and a ten-digit invoice or customer number is passed over. A personal number whose date cannot be is not taken, nor an IBAN of a length its country does not use. A number read from a scan's text recognition is named for the record only, as text recognition may misread or drop a digit; the picture is not compared. Where a font draws other figures than copying gives, numbers are checked as drawn too, as a reader sees them. A slip in the issuer's own system gives the same sign. |
| A net and VAT that do not make the total | Worth a look, never strong (for the record when read from a scan's text recognition): a page stating one net (Netto, Summa exkl. moms, Momsunderlag, Net), one VAT (Moms, VAT) and totals (Att betala, Summa, Totalt, Total, Amount due), each at the end of a line labelled so, of which no total is the net and VAT together within 51 öre (öre rounded to whole kronor). Only amounts with two decimals or written 1 250:- are read, their label holding no other figure than a rate. A subtotal is not taken for the net, as fees may follow it. A page with another amount between the net and VAT and the last total (a fee, a sum paid), of more than one net or VAT (several rates, several invoices), a VAT over 27 % of the net, or a line naming a weight, hours, a count, a discount or a sum paid is not checked. The lines of an invoice are not added up. |
| A date in the text later than the file | Worth a look, never strong: the date the page gives as the document's own (right after Fakturadatum, Kvittodatum, Dokumentdatum, Utfärdad, Invoice date, Receipt date, Date of issue, Issued, Dated, or Datum and Date with no word before them, the date beside the label or under it when the label ends its line or heads a column; or, in the upper half of the first page, a letter's date line, alone or after a place: Stockholm den 15 mars 2024) is later than every date the file gives of itself (the document information's `/CreationDate` and `/ModDate` in every save, the XMP's `CreateDate`, `ModifyDate` and `MetadataDate`), on the latest day any time zone had then: 14 hours ahead of a date that names its zone, 26 hours ahead of one that names none. Dates are read only where the day and month cannot be swapped: 2024-03-15 (also with a time after it, 2024-03-15T10:00), 15 mars 2024, March 15, 2024, 15.03.2024, 15/3/2024 or 3/15/2024, never 05/03/2024 or a year of two figures; a due date, a delivery date, a period (2024-03-01 – 2024-03-31) and a file date before 1993 are passed over. For the record: a date given only by Datum or Date, as agendas, invitations and tickets are often dated ahead of the day they are written; a date after the file was made but not after its last change; and a date read by a scan's text recognition (which may misread a figure), also when it is after the day the scanned picture's EXIF says it was made. A document dated ahead on purpose gives the same sign. |

### Pictures, annotations, forms, layers

| Finding | What it means |
|---|---|
| A picture with a compression, colour space, bit depth or resolution unlike the others | Pasted in from elsewhere: a signature, stamp or logo. |
| A picture added in a later save | Put on the page after it was made. |
| A JPEG picture keeping metadata of its own | It was made or edited elsewhere; its EXIF and comments are listed, read as data only. |
| A picture made after the file | For the record: a JPEG picture's EXIF says it was made (`DateTimeOriginal`, `DateTimeDigitized`) after every date the file gives, by more than a minute when both name their zone, else by more than 26 hours. Put in later, or a camera's or scanner's clock set wrong, as many are. |
| A document or picture made by an image generator | Worth a look, as for a picture file: content credentials kept in an attached file (relationship `C2PA_Manifest`; every attached file is looked at), the XMP's digital source type, or a JPEG picture's own credentials and marks say an image generator made it. Content credentials that name none are recorded. |
| The resolution a scanned page was scanned at | For the record only: its pixels to the inch from the size the page draws it at, what its JPEG says beside them, and how much detail they hold from the width of its ink's edges. A page whose edges are as sharp as a scan's at the resolution of its pixels was scanned at it, made smaller from a finer scan, or made larger from a lower resolution, as said. A page whose edges are wider than a scan's at the resolution of its pixels holds the detail of a lower resolution: made larger, blurred, or scanned by a scanner whose optics are softer than its pixels, which the edges cannot tell apart. A page made larger by repeating its pixels is named so. A black and white page, or one with too few edges of ink, is said to tell nothing. It adds about 0.06 s a million pixels to reading a scanned page: 0.18 s for an A4 page at 150 dpi, 0.53 s at 300, 1.8 s at 600. |
| Words or characters of a scanned page unlike their line | Worth a look: a word whose ink, edges or paper differ from the words beside it, or that sits off its line or is turned against it, was pasted, printed over the scan or moved; a stamp, a form filled in and a line printed in another pass do the same. Each character is set beside its line too: its foot, read to a fraction of a pixel, its shape's own fall below the line taken out (told by four or more prints of its shape on the page), against the straight line through up to five neighbours on each side. One further off than 0.25 pixels and 11.9 times as far as the page's characters typically lie is named, a character set higher or lower, with its offset in pixels and its place: a digit or a letter pasted by hand into a figure or a word about a pixel off. Of 100 characters pasted by hand 0.3 to 2 pixels off their line, 55 could be judged and 11 were named, all of them a pixel off or more; none of 33 pasted in place. On 1,001 honest pictures it named 662 of 173,216 words, about one in 260. |
| Characters of a black and white page the same as others, all moved alike | Worth a look: on a page of one bit a pixel, kept as fax (CCITT) or made black and white, three characters or more from one line the same pixel for pixel as as many others, each the same distance away: a word or a figure copied within the page. A character alone the same as another tells nothing there, as honest prints come out alike; a page kept as JBIG2 is not searched, as its coder prints characters of one shape from one print. |
| A scanned page kept as JBIG2 with symbol dictionaries | For the record: the picture's JBIG2 segments include a text region that refers to a symbol dictionary, so its coder printed every character it matched to a stored shape from that one print. Identical characters there are its design, and any character may have been swapped for one that looks like it as the page was scanned (Xerox WorkCentre copiers turned 6 into 8 in 2013), so neither characters alike nor the digits read on the page can be trusted. Only the segment headers are read, by the lengths they give, never the picture; a page of generic regions alone, or a text region its coder says is lossless, is not named, nor a page of an encrypted file, whose streams are not decrypted. |
| A region of a scanned page short of its JPEG traces | Worth a look: a page once saved as a JPEG carries that save's traces in every block of 8 by 8 pixels. Kept without loss since, its frequencies lie near whole numbers; saved as a JPEG again with finer steps, some steps of its lowest frequencies are left all but empty. A region that lacks them was laid in after the first save: a word typed again over healed paper, a stamp, a signature or cloned paper, however well its look, ink and grain match. A part of the page a program drew after the save, as a scanner's own note, does the same. A second save at the same or a coarser quality leaves no such trace, and a page never saved as a JPEG has none to lack. It adds 0.2 to 0.3 s to a page at 300 dpi, up to 1.2 s for a JPEG whose grid of blocks must be searched for. |
| Paper painted over in a scanned page | Worth a look: a region of blank paper, 3 tiles of 16 by 16 pixels or more where text runs (words on both sides, or above and below), keeping a twentieth or less of the spread the page's paper keeps: a white box, a box or a brush of one colour laid over words in a picture editor. Told only on a page whose paper keeps a grey level of grain and whose smooth paper lies in few pieces; a page whose smooth paper lies all over it is said, for the record, not to have been judged. A region mostly white on paper nearly white is not taken. A note a program drew, an office's blanking out, correction fluid, a glossy label or a flash's glare gives the same sign. It adds about 0.04 s to a page of 1240 by 1754 pixels. |
| Paper copied from elsewhere on a scanned page | Worth a look, never strong: a patch of grained paper near the words alike pixel for pixel with paper elsewhere on the same page, as a clone stamp laid over old characters or a content-aware fill leaves it. Blocks of 8 by 8 pixels of paper holding no ink and no clipped white are keyed coarsely and set beside one another pixel by pixel; 16 blocks or more matching at one shift of 8 pixels or more make a region, and both places are named with the shift between them. Which of the two is the copy is not told. Searched only on a page kept without loss (one saved as a JPEG before the paper was copied is searched too), whose paper keeps a grey level of grain: a JPEG rounds flat paper to the same few values in its blocks, which then repeat at shifts of whole blocks, so shifts of a whole number of blocks both ways are left out everywhere. A page holding more than 3 million such blocks (some pages at 600 dpi) is said, for the record, not to have been searched. A form's printed pattern repeating at one shift gives the same sign. It adds about 0.5 s to a page at 300 dpi, up to 1 s with grained paper all over it. |
| A picture drawn in more than one place | For the record: a picture that is no scan (16 by 16 pixels or more, drawn 24 by 12 points or more) kept the same octet for octet, and drawn alike, in two places or more, by one object or by several, each place listed. A signature, a stamp or a seal pasted in as a picture is the same every time it is drawn; so is an office's logo or the signature it keeps for its letters, honestly. Whether the signatures should differ, as two signers' do, is for the examiner to judge. |
| A signature or a stamp of a scanned page the same in two places | Worth a look: no hand signs twice alike, no stamp presses twice alike, and a scanner lays its grain over each print. The ink of each scanned page, once its words and its rules (runs six times the type's height long and a fifth of the page) are taken out, is joined into marks where it lies within a cell of 4 pixels; marks twice as tall and as wide as the type or more, no taller than a third of the page, no wider than two thirds, half their cells ink or less (not a photograph), up to 16 a page, are kept. Marks of about one size and alike in 8 by 8 coarse parts, the 4 nearest of each, are laid over each other at the best shift within 3 pixels of laying their corners or middles together, darker than the lighter of their pages' levels of ink. Named when 97% of the pixels inked in either are inked in both and their greys there stray by a grey level or less (97% and 15 grey levels on a page kept as a JPEG, or once saved as one), unless the paper round them, 64 pixels out, is as alike (one scan kept twice), or two on one page stand in one row or one column to the pixel (a form's boxes, a page set by a program). On a page whose grain was not judged (its prints of a character too alike to tell apart, as a page drawn by a program and given grain), only marks the same in every pixel inked in both, 99% of their ink shared, are named. Within one file, between pages or on one page; across files with `M-x palimpsest-compare-files`. Characters smaller than that are the copied characters' finding's. |
| A scan's text layer that disagrees with its picture | Worth a look, never strong: a scanned page made searchable keeps the words text recognition (OCR) read as invisible text over its picture. Each word of that layer is set on the picture's pixels. A word of two letters or more and 10 pixels high or more with no ink in its box or within a tenth of its height about it (ink a third of the way from the paper to the page's level of ink, so that grey type counts), or a word of ink the layer passes over standing within 1.5 times the layer's type height of layer words on both sides, is named, with the layer's text and its place: the picture was changed after its text was recognised, a word painted over or one put in. On 30,940 words of honest layers over 60 scanned pages neither was found. A figure changed in place keeps its ink and is not told; nor is a layer whose words were changed and not moved. A page kept in layers (a background picture and the masks of its text) is not judged where another picture lies over the layer's words, nor at all when the picture is drawn through a mask of its own (`/SMask`, `/Mask`), nor a page whose paper is not 40 grey levels lighter than its ink. |
| Characters copied, then the page saved again as a JPEG with its own steps | Worth a look: on a page kept as a JPEG, a character that is, to a grey level or so, what a save with the page's own steps makes of another print of its shape laid over it. A JPEG saved again with the same steps comes back as it was, so a character copied from elsewhere on the page comes out as that save makes the copy. The nearest print, moved by up to a pixel and laid with two pixels of paper round it, is saved with the page's table and decoded again; the character is named when it lies half as far or less from that as from the print as it lies and within a quarter of how alike the page's prints are, when none of the other prints of its shape comes so near, and when of 60 characters of its page set beside their nearest print the same way no more than a fiftieth do: where the steps are coarse beside the grain, or prints were drawn alike, any print laid over another comes back as it, and the page is not judged so. Characters of 100 pixels of ink or more only, not prints a whole number of blocks apart. A copy saved by an encoder that rounds otherwise than the trial save, at middling quality, may be missed. The trial saves add about 0.02 seconds to a dense page of 3,500 characters. |
| Text boxes, stamps, shapes, ink and redactions as annotations | Laid over the page and able to pass for it; strong when added later. A redaction marked but never applied leaves what it covers. |
| A hidden annotation | Its flags keep it from view. |
| A form field changed between saves | What the form says was changed. |
| An XFA form | What shows depends on the program reading it. |
| Layers off as the file opens | Content in them is not shown. |

### Signatures

| Finding | What it means |
|---|---|
| A signature | Who it names, when, why and how (`/SubFilter`), approval or certification. It is not checked cryptographically: what is reported is what it claims and what octets it covers. |
| A `/ByteRange` that stops short, leaves out more than its value, or covers another save than its own | The signature does not vouch for its whole revision. |
| A save after a signature that changes what is shown | Strong: the signature does not vouch for what is shown. Annotations, form values, signatures and validation data are reported as allowed or not by the certification's `/P`. |
| A shadow attack | An object in the signed octets that the signed document does not use, brought into use by a later save (hide, replace, hide and replace). |

### Pages, actions, files, encryption

| Finding | What it means |
|---|---|
| Pages added, removed or put in another order | Between two saves. |
| A page of another size, rotation or fonts than all the others | Put in from another document. |
| Page labels changed | The page numbers a reader shows were changed. |
| Actions | JavaScript, actions on opening, programs launched, addresses, forms sent: named, never run. |
| Embedded files | Named, with their sizes, never opened. |
| Encryption | Its kind and permissions; added in a later save, worth a look. |

## Settings

| Setting | Standard | Values | What it is |
|---|---|---|---|
| `*palimpsest-largest-file*` | 512 MB | a whole number, 1 or more | The most a PDF may hold to be examined; it is read whole, into memory. |
| `*palimpsest-largest-stream*` | 64 MB | a whole number, 1 or more | The most one stream may decode to; one that would decode to more is read no further, and the report says so. |
| `*palimpsest-most-decoded*` | 8 GB | a whole number, 1 or more | The most of streams decoded and of page content read in all while one PDF is examined, a stream decoded again counted again; past it no more is read, and the report says so. A page's content streams together are held to `*palimpsest-largest-stream*` as well. |
| `*palimpsest-most-objects*` | 1,000,000 | a whole number, 1 or more | The most objects read, and the most entries one cross-reference section may list. |
| `*palimpsest-most-revisions*` | 500 | a whole number, 1 or more | The most saves taken apart. |
| `*palimpsest-largest-written*` | 2 GB | a whole number, 1 or more | The most the revisions written out may take together; past it, or when the drive would keep less than 1 GB free, they are listed as not written. |
| `*palimpsest-largest-shown*` | 1 MB | a whole number, 1 or more | The most of an object's stream `o` shows. |
| `*palimpsest-scan-pixels*` | 60 million | a whole number, 1 or more | The most pixels a picture may have for its words to be measured. |
| `*palimpsest-strong-colour*` | `"#e60000"`, red | a colour: `"#rrggbb"`, a name such as `:red`, or a number of the 256 colours | The colour a strong sign is framed in on a scanned picture drawn by `d`, and its line of the key. |
| `*palimpsest-look-colour*` | `"#1a4ce6"`, blue | a colour, as above | The colour a sign worth a look is framed in. |
| `*palimpsest-not-judged-colour*` | `"#007326"`, dark green | a colour, as above | The colour a character not judged is framed in. A setting holding no colour keeps the standard one. |
| `*palimpsest-strong-line*` | `:whole` | `:whole`, `:dashed` or `:dotted` | How the frame of a strong sign is drawn, and its piece in the key. |
| `*palimpsest-look-line*` | `:dashed` | as above | How the frame of a sign worth a look is drawn. |
| `*palimpsest-not-judged-line*` | `:dotted` | as above | How the frame of a character not judged is drawn. |

Set them in the package's `:config`:

```lisp
(load-package "palimpsest" :source "https://github.com/Tuatara-Editor/tuatara-palimpsest"
  :config
  ;; (setf *palimpsest-largest-file* (* 512 1024 1024))
  ;; (setf *palimpsest-largest-stream* (* 64 1024 1024))
  ;; (setf *palimpsest-most-decoded* (* 8 1024 1024 1024))
  ;; (setf *palimpsest-most-objects* 1000000)
  ;; (setf *palimpsest-most-revisions* 500)
  ;; (setf *palimpsest-largest-written* (* 2 1024 1024 1024))
  ;; (setf *palimpsest-largest-shown* (* 1024 1024))
  ;; (setf *palimpsest-strong-colour* "#e60000")
  ;; (setf *palimpsest-look-colour* "#1a4ce6")
  ;; (setf *palimpsest-not-judged-colour* "#007326")
  ;; (setf *palimpsest-strong-line* :whole)
  ;; (setf *palimpsest-look-line* :dashed)
  ;; (setf *palimpsest-not-judged-line* :dotted)
  )
```

## Limits

- Encrypted files: their strings and streams are not decrypted, so their
  pages' content, fonts and pictures are not read; how the file is put
  together, its saves and its cross-references are.
- Signatures are not verified: no certificate or digest is checked. The
  signer's name is read from the signature's own data, as data.
- Content credentials (C2PA) are read, not verified: verifying them would
  mean checking the COSE signature over the claim, the certificate chain
  against the C2PA trust list, and the hashes that bind the claim to the
  file. Their JUMBF boxes and CBOR are read within limits of depth (8 boxes,
  24 items), count (10,000 boxes, 200,000 items, each piece of a string
  counted) and size (64 MB of credentials, 4 MB of text kept, 32 octets of
  an object identifier); a CBOR map giving a key twice is refused; past them the rest is not read, and the
  report says why. A WebP or HEIF picture is not examined, so its
  credentials are not read.
- A scanned page's picture is turned into pixels to be examined: one kept
  without loss is read by the package, a JPEG or a JPEG 2000 by Tuatara's own
  decoder in its sandbox (ImageMagick or GraphicsMagick under bubblewrap),
  never in the editor itself. A page kept as narrow strips drawn one under
  the next (of one kind and way up, abutting on the page) is joined into one
  picture first and named "the scanned page joined from N strips"; places
  are given in the strip that holds them. Strips that cannot be joined (two
  pictures at one strip's foot, too many pixels, more than 128 JPEG strips,
  JPEG blocks on no one grid) are read one by one as before, or named as not
  read; JPEG strips saved with other tables than the rest are worth a look.
  A picture kept as fax (CCITT) or JBIG2 is read by Poppler's pdftoppm in
  the sandbox, from a PDF of that picture alone. One that cannot be read is
  named for the record, and nothing is claimed of it.
- A scanned page is read line by line. A logo, a seal or a signature beside
  a few lines, whose ink joins their rows, is read apart from them where
  paper runs down between; a band no such gap parts is left unread, and the
  report says so. Text far lighter than the page's level of ink (thin type
  photographed in poor light) is not read at all, and a figure alone in a
  table's cell is read as characters, not judged as a word. Of 266 places
  pasted by hand into 19 scanned documents, a word or a character is read at
  249. A character's foot is judged only where its shape has four prints on
  the page and it has three neighbours on its line: 144 of those places.
- A black and white page (kept one bit a pixel, as fax, JBIG2 and image
  masks are, holding four grey levels or fewer) holds no grey: its words are
  read and judged by how they are turned, but not by their ink, edges or
  paper, nor by where they sit, which one bit reads too roughly (honest fax
  pages at 200 dpi set words a pixel or two off their lines), and neither copies
  of a character nor characters without grain are told on it, as honest
  prints of a character come out the same pixel for pixel there (up to 89
  pairs a page on the honest fax pages, 180 to 338 on JBIG2 pages). Only
  a run of three or more characters copied together is named. A word typed
  again in a matching face is not told on such a page.
- A JPEG's header is set beside the device its EXIF names by signs only
  programs write (Adobe's segment, a progressive frame); JFIF before the
  EXIF is given for the record, as scanner drivers and phones write it too. No table of devices' own steps is kept: the one published survey
  (Kee, Johnson and Farid, 2011) keeps its database private, and many devices
  write libjpeg's standard steps, so steps and Huffman tables are given for
  the record only, and a device that writes one of those signs itself is
  not known from one that does not. The EXIF is read within its segment,
  at most 200 entries a directory, each offset checked; its thumbnail, at
  most 64 KB lying within the EXIF, beginning as a JPEG does and at most
  1024 pixels across and down by its frame, is turned into pixels in the
  sandbox made no more than 320 across, and set beside the picture at
  that size, so a phone's 512 by 384 costs what a 320 by 240 does; a
  frame claiming more is refused for the record before it is decoded. A
  thumbnail shows an edit only when it is large in it: a region a sixteenth of the picture across is mostly missed.
  The two pictures are not drawn side by side; the octets to cut the
  thumbnail out by are given instead.
- A picture file is turned into pixels the same way, turned as its EXIF says
  it is held; a TIFF of several pages is read at its first. A page that is
  one picture drawn by a program (a PDF opened in a picture editor and saved
  again, its text turned to pixels) is named so, worth a look: its blank
  paper is one grey and its text's edges are smoothed. A scan whose paper was
  whitened all over looks the same, and the finding says so.
- A picture an image generator made is told by its pixels only when it is kept
  without loss and at the size it was made: the pattern its decoder leaves every
  8 by 8 pixels is gone after a JPEG save, a print scanned again, or a picture
  made larger. Most generated pictures that reach a document are so changed, so
  a picture with no such finding is not thereby taken from life. Most office
  scans are not measured at all: their paper is made white to the last grey, and
  only paper short of white has the flat, unclipped pixels the pattern is read
  in. So a missing finding says nothing. A dither or half-tone screen on grey
  paper repeating every 8 or 4 pixels is far stronger than a decoder's pattern
  and is not named; one made faint enough to pass for it would be. Generated
  text that does not repeat as type does, blur and grain across a page, and a
  camera's own traces were measured and do not tell generated pages from honest
  ones (the numbers are in doc/measures.org).
- Text written with a chatbot is named only by the words it leaves behind when
  pasted and not read through. The style of the writing (words chatbots favour,
  sentences of even length) is not weighed: published detectors that weigh it
  name honest writers too often, above all those writing in a second language,
  and none has been measured on Swedish. Nothing in a PDF's structure names a
  chatbot either: text from one is saved by the word processor or browser it is
  pasted into. So a page without this finding may still hold such text.
- Check digits and totals are read from the page's text: the text a
  born-digital page draws, or the text recognition a scan carries. A scan
  without text recognition is not read for them. Plusgiro numbers are not
  checked, for want of a published account of their check digit; the length
  digit some OCR references carry is not checked either, as the bankgiro
  allows references without one. The lines of an invoice are not added up,
  nor the VAT set against its rate.
- Dates in the text are read in Swedish and English only, and only the
  document's own date by its label or a letter's date line: a date whose day
  and month could be swapped (05/03/2024), with a year of two figures, or
  written in other words (the fifteenth of March) is not read, nor a due date
  or a period. A date earlier than the file is never a sign, as a copy saved
  again later is honest; an invoice made out ahead of its date gives the sign
  honestly. A scan's text recognition may misread a figure, so its dates are
  for the record only. A date line written with figures alone (2017-11-20 on
  a line of its own) is not taken, as a due date set apart looks the same.
- A stamp drawn by software is told from copies of the page's characters by
  the grain of its ink and of the paper within its figures. In a thin face
  the ink's core is mostly the stroke's smoothed edge, so the paper decides;
  on a scan whose paper was whitened it cannot, and such a stamp on a page
  whose ink strays by 6 grey levels or more is named copies, a strong sign.
  Real scanners leave 1 to 3 grey levels, where it is worth a look at most.
- A copy of a page's character recoloured and turned, its grain made fainter
  with its ink, is found by the grain of the paper it carries, its source
  sought turned up to 15 degrees either way, but only where it holds 16
  pixels or more of paper two pixels from its ink: round letters and figures
  of 40 pixels or more. Copies in body type, copies turned 10 degrees or
  more, and copies cut out of their paper are missed this way, and are named
  as a stamp drawn by software when they lie in one; one recoloured lighter
  than a faint mark and turned is not read at all. A copy whose letter has no
  other print on the page to set it beside is never taken by its grain: a
  false strong sign would be worse than the miss.
- Paper painted over is told only where the page's paper keeps its grain: a
  grey level of spread in the middle tile, and smooth paper in no more than
  2 pieces for each thousand tiles. A JPEG of middling quality rounds the
  grain of plain paper away, and honest pages so smooth held 4.6 to 60 pieces
  a thousand, painted pages 1.6 at most: they are not judged, and the report says so. Paint at the end of
  a line, with words on one side only, is passed over as a margin is. A box
  over one or two figures (under 3 tiles) is missed. Paper patched from
  paper elsewhere (a clone stamp, a content-aware fill) keeps its grain and
  is not told as paint; it is found as copied paper when the page was kept
  without loss after the copying, and missed after a JPEG save, on
  flat or white paper, and where the patch makes fewer than 16 matching
  blocks (a clone stamp's feathered edges leave only its core alike, so a
  single figure cloned away is often missed). Of 284 made-up boxes and
  brushes over words, 118 were named at their place and none elsewhere.
- A JPEG saved a second time at the same quality leaves no trace of the
  first save: decoded and saved again at the same steps its blocks come back
  as they were, those laid in between and the rest alike (none of either
  changes, on the 27 such forgeries measured). Saved coarser the second
  time, the steps' counts alternate, but printed text makes them alternate as
  far on honest pages saved once (6.2 to 40.4 deviations in the forged
  boxes' places, 3.8 to 43.3 in the forged boxes). A copy saved as a JPEG on
  a grid of its own before it was laid in is lost too once the page is saved
  as a JPEG again: its grid stood 0.55 to 2.71 apart in the region, honest
  regions of the same pages up to 2.87. None of these is told, and a region
  laid in before such a save is not named by its JPEG traces. Nor is one
  told by how its blocks stand up to a trial save at the file's own steps:
  the page's paper and text move that count far more than a save does. A
  character copied from elsewhere on the same page is the exception: its
  source is still there, and a trial save of the source laid over the copy
  gives the copy back (characters copied, then the page saved again).
- A signature or a stamp copied is found only as a picture laid on whole: the
  same pixels, moved by whole pixels, neither scaled nor turned. One copied
  with a scale or a turn, printed and scanned again, laid over other ink (a
  line, a word) so that its mark joins that ink or is cut where words are
  taken out, or on a page kept as JBIG2 or drawn by a program, is missed; so
  is one smaller than twice the type, one copied into the same row or column
  of its page, and one on pages saved as JPEGs of quality 50 or worse. After a
  JPEG save the rule is looser, and two scans of one print (a facsimile
  signature a program prints, a stamp pressed by a machine) whose pixels
  happen to fall on its strokes within a tenth of a pixel, unturned, come as
  alike as a copy and are named. A picture kept twice with other octets (saved
  again in another way) is not set beside its twin as a picture, only as marks
  when it is a scan. The marks of a file keep 40 million pixels at most; each
  mark is set beside 64 of about its size, and 4,000 pairs reading 2,000
  million pixels are laid over each other at most. Past them the rest are let
  go or not tried, and the report says so.
- A page printed and scanned again defeats the signs read in its pixels. The
  printer and the scanner lay new ink, grain, edges and JPEG blocks over the
  whole page, the forged parts and the rest alike, so copied characters,
  retyped words, paper painted over and the traces of earlier saves are gone.
  What the new file says of itself, and what its words say, is all that is
  left to judge.
- Text is read through each font's ToUnicode map, else its encoding (the
  standard encodings and `/Differences`), else shown as the codes
  themselves in hex (`<0041>`). Text in Type 3 fonts with no such map, and
  text drawn as pictures or paths, is not read as text.
- Positions follow the operators: transformations, text matrices and
  spacing, the widths each font gives. Text is covered where the filled
  shapes drawn after it hide each of its characters: the middle of each,
  along its own baseline however the text is turned, lying wholly where the
  shapes of one colour fill, their outlines (curves cut into straight
  pieces) filled row by row, half a point apart, by their fill rules within
  the outlines they are clipped to. Text is clipped by the box of what it
  is clipped to.
- What the pages draw is checked in the latest revision; the earlier
  revisions' pages are compared, each with the one before.
- A certification signature's permissions (`/P`) are weighed against the
  saves after it; a field lock (FieldMDP) is not read on its own.
- A sign is a sign, not proof: honest programs leave some of these too, and
  the report says which signs are stronger.

## Tests

`tools/test.sh` runs them.
They read only PDFs and pictures they write themselves.

## Credits

- ISO 32000 (the PDF specification): how a PDF is put together and read.
- Saves after a signature: Vladislav Mladenov, Christian Mainka, Karsten
  Meyer zu Selhausen, Martin Grothe and Jörg Schwenk, "1 Trillion Dollar
  Refund: How To Spoof PDF Signatures" (ACM CCS 2019).
- The shadow attacks were described by Christian Mainka, Vladislav Mladenov
  and Simon Rohlmann (Ruhr University Bochum, 2020; NDSS 2021).
- A JPEG's block grid and quantisation tables: Hany Farid, *Photo
  Forensics* (MIT Press, 2016); Eric Kee, Micah K. Johnson and Hany Farid,
  "Digital Image Authentication From JPEG Headers" (IEEE TIFS, 2011).
- Copies the same pixel for pixel: Jessica Fridrich, David Soukal and Jan
  Lukáš, "Detection of Copy-Move Forgery in Digital Images" (DFRWS 2003).

The rules for scanned pages and every threshold were made and measured for
palimpsest; the handbook gives each with its reasons.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.

## Working on it

```sh
tools/test.sh
```

runs its tests against a Tuatara source tree: `../tuatara` beside this
repository, or `../../tuatara` beside the directory it is in, unless
`TUATARA_SOURCE` names another. How a package is laid out and which of
Tuatara's names it may use are in Tuatara's
[HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org);
bug reports, ideas and how changes are made in
[CONTRIBUTING.md](CONTRIBUTING.md).
