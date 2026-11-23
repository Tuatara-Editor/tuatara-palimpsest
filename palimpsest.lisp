;;;; palimpsest.lisp --- The palimpsest package: a PDF's saves taken apart, and the signs of its editing found.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;; Needs: Tuatara 0.90.40
;;;;
;;;; One line in the init file loads it: (load-package "palimpsest").

(defpackage #:palimpsest
  (:use #:cl #:tuatara)
  (:documentation "A PDF's saves, from the first to the last, and every sign of its editing, read from its
octets alone."))

(in-package #:palimpsest)

(dolist (file '("groups" "pdf" "shut-in" "content" "covers" "findings" "widths" "pixels" "marks" "unlike" "copies"
                "pieces" "local" "resolution" "methods" "credentials" "generated" "copied" "layers" "chat" "figures"
                "picture" "dates" "header" "report" "draw" "shapes" "repeats" "checks" "install"))
  (load-package-file (format nil "~a.lisp" file)))
