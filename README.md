# palimpsest

A PDF taken apart into every save it holds, from the original to the latest
edit, and searched for every sign of editing or forgery that examiners of
documents know. One of the Tuatara project's packages, not part of the editor.

Everything is read from the file's octets, by the package itself: no other
program reads the PDF, nothing in it is run (no JavaScript, no actions, no
launching), nothing it names is fetched, and the file is never changed. What
the package writes (each revision as a PDF of its own, and its log) goes to a
folder of its own in Tuatara's cache directory, for you alone, never beside
the evidence.

## Loading it, step by step

1. **Install what it needs.** Nothing to read PDFs; bubblewrap, ImageMagick and poppler to read scanned pages and to draw pages (`d`):

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

`M-x palimpsest` examines the PDF the current buffer shows (a PDF's view, a
visited PDF, the PDF on rimu's line), or asks for one (always, with `C-u`).
The report, `*Palimpsest: NAME.pdf*`, is made in the background: `P` is lit
in the mode line, `C-x j` says how far it has got, and the editor never waits.

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

## Keys

| Where | Key | What it does |
|---|---|---|
| The report | `RET` | On `revision-N.pdf`, opens that revision in Tuatara's PDF view; on `revision-N.log`, opens its log; on an object's number, shows the object. |
| The report | `TAB`, `S-TAB` | Go from one link to the next. |
| The report | `s` | Saves the revisions, their logs and the report (`M-x palimpsest-save`): see below. |
| The report | `o` | Shows an object: where it is in the file, its dictionary as written, its stream decoded, as text, or in hex in a buffer of its own when it is not text (in hex too with `C-u o`); `RET` on that buffer's name shows it. Asks for its number, offering the one at point. |
| The report, a revision opened from it | `w` | Writes a revision as a PDF of its own, to show to others, its log beside it, and opens it (`M-x palimpsest-write-revision`): the revision whose part of the report the cursor is in, or the revision you are looking at; else it asks which, offering the one before the latest. |
| The report | `C-x C-w` | Writes the report alone as text, offered as `NAME-revision-report.txt` in the folder last saved to, else in your home folder (`M-x palimpsest-write-report`); never beside the PDF examined nor in the package's folder, nothing written over. |
| Either | `q` | Leaves its window; the report stays, to come back to. |
| The report | `C-x k` | Closes it, and the revisions and logs opened from it, and deletes its own copies of them; when they were not saved it asks first: `Revisions and logs not saved: close and delete them? (y or n)`. |

## Each revision written out, with its log

Each revision is written to the package's folder as `revision-N.pdf`: the
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

The package's own folder is cleared: the report's copies go when it is
closed, and anything left there after a day. To keep the work, `s` in the
report (or `M-x palimpsest-save` there) asks for a folder, made when it is
not there, and copies every `revision-N.pdf` and `revision-N.log` into it,
with the report itself as `NAME-revision-report.txt`. Each copy's SHA-256 is checked
against its original; the PDFs and logs are copied octet for octet, and
the report ends with where each copy is and its SHA-256. The copying runs in
the background. It refuses the PDF's own folder (nothing is written beside
the evidence) and the package's folder in the cache, and never writes over a
file: when any of these names is in the folder already, nothing is copied
and the report says which are there. The summary then says where the work was
saved, and closing the report no longer asks.

### One version to show

The `revision-N.pdf` links open the report's own copies, to look at; they go
with the report. To hand one version to someone else, `w` writes it wherever
you say (offered as `NAME-revision-N.pdf` in the folder last saved to, else in your home folder), with its log beside it as
`NAME-revision-N.log`, each copy's SHA-256 checked, and opens it. Which
revision: the one whose part of the report (under `* Revisions`) the cursor
is in; in a revision you opened from the report, that one; elsewhere in the
report it asks, offering the one before the latest. The PDF examined is
never changed; nothing is written over, nor beside the PDF examined, nor into the package's folder. The PDF written is the file as it stood after that save, so palimpsest
run on it finds the saves up to it.

## What each finding means

### How the file is put together

| Finding | What it means |
|---|---|
| The file was changed after it was first saved | It holds incremental updates: each later save's objects follow the first's, each with its own cross-reference section and `%%EOF`. Signing and filling forms do this too; the revisions show what each changed. |
| Page N was changed in revision M | A later save changed what a page draws: its text (line by line) and drawing (item by item) are compared with the save before. |
| A cross-reference entry does not point at its object | The offsets the index gives are wrong: the file was edited by hand, or repaired. |
| startxref names an offset where no section is | The same, for the whole index. |
| Objects freed, numbers used again without a higher generation | What a save removed is still in the file; reusing a number without raising its generation is not what writers do. |
| Objects no longer used | Objects no reference reaches: leftovers of earlier content, or objects kept to be used later (see shadow attacks). |
| Objects written but listed by no section | Hidden from every reader, or left by an edit. |
| An object written twice in one save | Written by hand or by a careless program. |
| Data after the last `%%EOF` or before `%PDF-` | Hidden data, a save cut short, or a file made to be read as two kinds. |
| A save's cross-reference of another kind than the original's, a hybrid file, a linearised file changed | Saved by another program than made it. |
| Comments between objects | Notes programs leave, sometimes naming themselves. |
| A stream's `/Length` disagrees with where `endstream` is | Changed by hand, or by a program that did not write the length again. |

### What the file says of itself

| Finding | What it means |
|---|---|
| Document information and XMP disagree | One was changed without the other: dates, producing and creating programs, title. |
| Last changed before it was made, a date in the future, a time zone no place uses | A date set by hand, or a clock set wrong. |
| An edit history in the XMP | Each save a program kept, and the program that saved it. |
| Made from another document | XMP's `OriginalDocumentID` or `DerivedFrom` names another document. |
| A program that edits PDFs is named | The producer, creator or XMP names a PDF editor or online service (iLovePDF, Smallpdf, Sejda, PDFescape, PDF-XChange, Foxit, Nitro, PDFelement, Soda PDF, pdftk, qpdf, PyPDF, PDFsam, Acrobat after another program, Preview, Canva...). |
| A save names another program, removes metadata, gives another first `/ID` | Each save was made by whatever program it names; clearing metadata, or another document's ID, is done on purpose. |

### What the pages draw

| Finding | What it means |
|---|---|
| Text covered by a shape drawn over it | A white box (strong) or another colour (a black one is a redaction drawn over text, the text still in the file): the covered text is named. |
| Invisible text | Text in render mode 3 or 7; over a picture of the whole page it is a scan's text recognition, and only for the record. |
| Text clipped away, outside the page, in a layer that is off | Text in the file that is never seen. |
| White text on a white page, tiny text | Text no one can see or read. |
| A page drawn from several content streams, one appended later | Something was laid over the page by a later save. |
| A line drawn a character at a time, or spaced oddly | Unlike the rest of the page: retyped, or fitted into a gap. |
| Figures in another font than their line | A retyped number. |
| One font in several subsets, embedded on some pages and not others | Text set at different times or by different programs, or pages joined. |
| Tagged text unlike what shows | `/ActualText` or `/Alt` says the text stands for something else. |
| A page that is one picture with text over it | Text typed onto a scan. |

### Pictures, annotations, forms, layers

| Finding | What it means |
|---|---|
| A picture with a compression, colour space, bit depth or resolution unlike the others | Pasted in from elsewhere: a signature, stamp or logo. |
| A picture added in a later save | Put on the page after it was made. |
| A JPEG picture keeping metadata of its own | It was made or edited elsewhere; its EXIF and comments are listed, read as data only. |
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

| Setting | Standard | What it is |
|---|---|---|
| `*palimpsest-largest-file*` | 512 MB | The most a PDF may hold to be examined; it is read whole, into memory. |
| `*palimpsest-largest-stream*` | 64 MB | The most one stream may decode to; one that would decode to more is read no further, and the report says so. |
| `*palimpsest-most-decoded*` | 8 GB | The most of streams decoded and of page content read in all while one PDF is examined, a stream decoded again counted again; past it no more is read, and the report says so. A page's content streams together are held to `*palimpsest-largest-stream*` as well. |
| `*palimpsest-most-objects*` | 1,000,000 | The most objects read, and the most entries one cross-reference section may list. |
| `*palimpsest-most-revisions*` | 500 | The most saves taken apart. |
| `*palimpsest-largest-written*` | 2 GB | The most the revisions written out may take together; past it, or when the drive would keep less than 1 GB free, they are listed as not written. |
| `*palimpsest-largest-shown*` | 1 MB | The most of an object's stream `o` shows. |

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
  )
```

## Limits

- Encrypted files: their strings and streams are not decrypted, so their
  pages' content, fonts and pictures are not read; how the file is put
  together, its saves and its cross-references are.
- Signatures are not verified: no certificate or digest is checked. The
  signer's name is read from the signature's own data, as data.
- Picture data (JPEG, JPEG 2000, JBIG2, CCITT) is never decoded: only its
  dictionary and, for JPEG, the metadata in its own segments.
- Text is read through each font's ToUnicode map, else its encoding (the
  standard encodings and `/Differences`), else shown as the codes
  themselves in hex (`<0041>`). Text in Type 3 fonts with no such map, and
  text drawn as pictures or paths, is not read as text.
- Positions follow the operators: transformations, text matrices and
  spacing, the widths each font gives. A shape over text is found by the
  middle of the text lying within the shape's box, drawn after it; clipping
  is followed as rectangles.
- What the pages draw is checked in the latest revision; the earlier
  revisions' pages are compared, each with the one before.
- A certification signature's permissions (`/P`) are weighed against the
  saves after it; a field lock (FieldMDP) is not read on its own.
- A sign is a sign, not proof: honest programs leave some of these too, and
  the report says which signs are stronger.

## Tests

`tools/test.sh` runs them.
They read only PDFs they write themselves.

## Credits

- ISO 32000 (the PDF specification): how a PDF is put together and read.
- The shadow attacks were described by Christian Mainka, Vladislav Mladenov
  and Simon Rohlmann (Ruhr University Bochum, 2020).

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.

## Working on it

```sh
tools/test.sh
```

runs its tests against a Tuatara source tree: `../tuatara` beside this
repository, or `../../tuatara` beside the folder it is in, unless
`TUATARA_SOURCE` names another. How a package is laid out and which of
Tuatara's names it may use are in Tuatara's
[HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org);
bug reports, ideas and how changes are made in
[CONTRIBUTING.md](CONTRIBUTING.md).
