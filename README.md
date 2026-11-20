# palimpsest

Read the manual:
[tuatara-editor.github.io/tuatara-palimpsest](https://tuatara-editor.github.io/tuatara-palimpsest/).

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

`M-x palimpsest` makes a report in the background: a summary of the file, a
table of every check run, and the findings, strongest first, each with the
page and place it is on. The manual says how to use it, what each check looks
for and what each strength means.

<picture>
  <source media="(prefers-color-scheme: light)" srcset="media/palimpsest-light.gif">
  <img alt="An invoice saved four times: the report of its saves, the original and the latest opened, and the second written out as a PDF of its own" src="media/palimpsest.gif">
</picture>

<picture>
  <source media="(prefers-color-scheme: light)" srcset="media/palimpsest-craft-light.gif">
  <img alt="A contract changed in a PDF editor, levels 1 and 2; the same contract scanned and its pixels edited, level 4; a bank statement whose figures are copies of its own characters, level 3, each copy framed in red" src="media/palimpsest-craft.gif">
</picture>

## What it needs

- Tuatara 0.90.35 or later.
- bubblewrap, ImageMagick and poppler, to read scanned pages and to draw
  pages. Nothing else reads PDFs.

| System | Command |
|---|---|
| Fedora | `sudo dnf install bubblewrap ImageMagick poppler-utils` |
| Ubuntu, Debian | `sudo apt install bubblewrap imagemagick poppler-utils` |
| Arch, Omarchy | `sudo pacman -S bubblewrap imagemagick poppler` |

## Installing it, step by step

1. **Install what it needs**, with the command for your system above.
2. **Open your init file**: in Tuatara press `C-x C-f`, type
   `~/.config/tuatara/init.lisp` and press `RET`.
3. **Add this line** to it:

   ```lisp
   (load-package "palimpsest" :source "https://github.com/Tuatara-Editor/tuatara-palimpsest")
   ```

4. **Save** with `C-x C-s`, **quit** with `C-x C-c`, and **start Tuatara
   again**. Answer `y` when it asks whether to load palimpsest; it asks once.
5. **Check it**: open a PDF and run `M-x palimpsest`.

## On a machine without internet

Tuatara compiles the package itself, so only fetching it needs the internet.
On a machine with internet:

```sh
git clone https://github.com/Tuatara-Editor/tuatara-palimpsest
git -C tuatara-palimpsest verify-tag "$(git -C tuatara-palimpsest describe --tags --abbrev=0)"
```

Copy the directory to the other machine, for instance to
`~/tuatara-packages/tuatara-palimpsest`, install bubblewrap, ImageMagick and
poppler there from the system's own packages, and name the directory in the
init file:

```lisp
(load-package "palimpsest" :source "~/tuatara-packages/tuatara-palimpsest")
```

Carrying over the compiled files as well, so the other machine skips the
compiling, is in Tuatara's manual, chapter 19, "On a machine without
internet".

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
palimpsest; the manual gives each.

## License

GPL-3.0-or-later; see `LICENSE` at the top of this repository.

## Working on it

```sh
tools/test.sh
```

runs its tests against a Tuatara source tree: `../tuatara` beside this
repository, or `../../tuatara` beside the directory it is in, unless
`TUATARA_SOURCE` names another. The tests read only PDFs and pictures they
write themselves. How a package is laid out and which of Tuatara's names it
may use are in Tuatara's
[HACKING.org](https://github.com/Tuatara-Editor/tuatara/blob/main/HACKING.org);
bug reports, ideas and how changes are made in
[CONTRIBUTING.md](CONTRIBUTING.md). The manual's source is in `doc/manual/`;
`tools/manual-html.sh` in a Tuatara tree, given this repository's directory,
makes its pages.
