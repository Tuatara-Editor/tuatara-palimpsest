;;;; palimpsest.lisp --- The palimpsest package: a PDF's saves taken apart, and the signs of its editing found.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; One line in the init file loads it: (load-package "palimpsest").

(defpackage #:palimpsest
  (:use #:cl #:tuatara)
  (:documentation "A PDF's saves, from the first to the last, and every sign of its editing, read from its octets alone."))

(in-package #:palimpsest)

(dolist (file '("pdf" "content" "findings" "pixels" "methods" "report" "draw" "install"))
  (load-package-file (format nil "~a.lisp" file)))
