;;;; shut-in.lisp --- Each file read in a Tuatara of its own, shut in, what it found sent back as data.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; palimpsest reads files made to deceive, and some are made to do harm. So
;;;; every piece of work that reads one (examining it, showing one of its
;;;; objects, drawing a copy of it, reading it to set beside others) runs in a
;;;; Tuatara of its own shut in by the sandbox (SANDBOX-CALL): one file to a
;;;; process, read only, with its own memory and time, and one directory it
;;;; writes in. What comes back is data: words, numbers and lists, and the
;;;; files it wrote, read back within limits. The editor builds its buffers
;;;; from that alone. Work that runs out of memory or time ends there and is
;;;; said; the editor goes on.

(in-package #:palimpsest)

(define-package-setting *palimpsest-seconds* 3600
  "The most seconds the work on one file may take: examining it, showing one
of its objects, drawing a copy of it, or reading it to set beside others.
Past it the work is stopped and the report says so.
- a time, 1 second or more: seconds, or words such as \"90s\" or \"2h\"; an
  hour by default."
  :values (seconds 1 *))

(defvar *palimpsest-shut-in* t
  "True to read each file in a Tuatara of its own, shut in (SHUT-IN), as
palimpsest always does; NIL to read it in the editor itself, which only the
package's tests do.")

(defparameter *carried-settings*
  '("largest-file" "largest-stream" "most-decoded" "most-objects" "most-revisions" "largest-written"
    "largest-shown" "scan-pixels" "strong-colour" "look-colour" "not-judged-colour" "strong-line" "look-line"
    "not-judged-line")
  "The settings the work on a file is done by, carried to the Tuatara it runs
in, which reads no init file: each the name of *PALIMPSEST-NAME*, defined in
the files loaded after this one.")

(defun carried-symbols ()
  "The symbols of *CARRIED-SETTINGS*."
  (mapcar (lambda (name) (find-symbol (string-upcase (format nil "*palimpsest-~a*" name)) :tuatara-user))
          *carried-settings*))

(defvar *shut-in-progress* (lambda (value) (sandbox-call-report value))
  "What work run by SHUT-IN tells how far it has got, a function of one value
of words, numbers and lists of them.")

(define-condition check-stopped (error)
  ((words :initarg :words :reader check-stopped-words))
  (:report (lambda (condition stream) (write-string (check-stopped-words condition) stream)))
  (:documentation "The work on a file ran out of memory or time and was stopped; WORDS say so."))

(defun shut-in-memory ()
  "The memory the Tuatara reading one file may take: 1 gigabyte (GB) and
three times *PALIMPSEST-LARGEST-FILE*, which it holds whole."
  (+ (expt 2 30) (* 3 *palimpsest-largest-file*)))

(defun plain-data (value)
  "VALUE, what work on a file returned, with JSON's arrays as lists and its
null as NIL, so that it reads the same whether it came back as JSON or not."
  (cond ((eq value :null) nil)
        ((stringp value) value)
        ((vectorp value) (map 'list #'plain-data value))
        ((consp value) (mapcar #'plain-data value))
        (t value)))

(defun call-with-settings (settings function)
  "Call FUNCTION with each of *CARRIED-SETTINGS* bound to its value in
SETTINGS, the list SHUT-IN carries, in the same order."
  (progv (carried-symbols) settings (funcall function)))

(defun shut-in (function paths directory label &key arguments progress)
  "Run FUNCTION, a symbol naming work on a file, on PATHS (one file, or for
work that reads none, none) and DIRECTORY, which it writes in, as (FUNCTION
PATHS DIRECTORY :SETTINGS S . ARGUMENTS), S the values of *CARRIED-SETTINGS*.
It runs in a Tuatara of its own shut in (SANDBOX-CALL), with SHUT-IN-MEMORY
and *PALIMPSEST-SECONDS*; or, while *PALIMPSEST-SHUT-IN* is NIL, here.
PROGRESS, a function of one value, is given what it tells of how far it has
got. Returns what it returned (PLAIN-DATA). A CHECK-STOPPED, LABEL beginning
its words, when it ran out of memory or time; the job stopped when it was
asked to; any other failure an error saying why."
  (let ((arguments (list* :settings (mapcar #'symbol-value (carried-symbols)) arguments)))
    (if (not *palimpsest-shut-in*)
        (let ((*shut-in-progress* (lambda (value) (when progress (funcall progress (plain-data value))))))
          (plain-data (apply function paths directory arguments)))
        (multiple-value-bind (result failure outcome)
            (sandbox-call function paths directory
                          :arguments arguments :seconds *palimpsest-seconds* :memory (shut-in-memory) :label label
                          :progress (and progress (lambda (value) (funcall progress (plain-data value)))))
          (case outcome
            (:done (plain-data result))
            (:memory (error 'check-stopped
                            :words (format nil "~a stopped: it needed more than ~:d MB of memory" label
                                           (floor (shut-in-memory) (* 1024 1024)))))
            (:timeout (error 'check-stopped
                             :words (format nil "~a stopped: it took more than ~:d seconds" label
                                            *palimpsest-seconds*)))
            (:cancelled (job-check-stop) (error "~a" failure))
            (t (error "~a" failure)))))))

(defun shut-in-octets (directory name limit)
  "The octets of the file NAME that work run by SHUT-IN wrote in DIRECTORY, at
most LIMIT of them; NIL when it wrote none. An error when it is larger."
  (let ((path (path-join directory name)))
    (and (eq :regular (path-kind path :follow nil))
         (file-octets path :limit limit :follow nil))))

(defun work-directory ()
  "A new directory for work on a file to write in, for you alone, deleted by
the caller once read (DELETE-TREE)."
  (new-private-directory (private-cache-directory "palimpsest-work/") :hold nil))

(defmacro with-work-directory ((directory) &body body)
  "Run BODY with DIRECTORY a new directory for work on a file (WORK-DIRECTORY),
deleted with all in it after, however BODY ends."
  `(let ((,directory (work-directory)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-tree ,directory)))))

;;; Pieces of a list sent back as data

(defparameter *piece-faces* '(:link :heading :list-label :list-dim :list-note :warning :strong :diff.plus :diff.minus)
  "The faces the pieces of a report or a list made from a file are shown in;
a face named by work on a file that is not one of them is let go.")

(defun pieces-data (pieces directory)
  "PIECES, as COLLECTING-PIECES makes them, their actions lists of a keyword
and words or numbers (as REPORT-PIECES makes them), as data: their texts
written together into the file pieces.txt in DIRECTORY, as UTF-8, and a list
of (OCTETS FACE ACTION) for each, OCTETS the length of its text written, FACE
and the keyword of ACTION in lower case."
  (flet ((word (thing) (if (symbolp thing) (string-downcase (symbol-name thing)) thing)))
    (let ((octets (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)) (data '()))
      (dolist (piece pieces)
        (multiple-value-bind (text face action)
            (cond ((stringp piece) (values piece nil nil))
                  ((and (consp (cdr piece)) (consp (cddr piece))) (values (first piece) (second piece) (third piece)))
                  (t (values (car piece) (cdr piece) nil)))
          (let ((encoded (string-octets text)))
            (loop for octet across encoded do (vector-push-extend octet octets))
            (push (list (length encoded) (and face (word face)) (and (consp action) (mapcar #'word action))) data))))
      (write-new-file (path-join directory "pieces.txt") (coerce octets '(simple-array (unsigned-byte 8) (*))))
      (nreverse data))))

(defun data-pieces (data directory action-function)
  "The pieces DATA and the file pieces.txt in DIRECTORY stand for (PIECES-DATA),
as COLLECTING-PIECES makes them: each face one of *PIECE-FACES*, else none;
each action what ACTION-FUNCTION, called with the action's words and
numbers, its first the word naming it, makes of it, a function or NIL. What does not
fit is let go; past the text written, nothing more."
  (let ((octets (or (shut-in-octets directory "pieces.txt" (expt 2 30)) #())) (at 0) (pieces '()))
    (loop for entry in data
          for (count face action) = (and (listp entry) entry)
          while (and (integerp count) (<= 0 count (- (length octets) at)))
          do (let* ((text (octets-string octets :start at :end (+ at count)))
                    (face (and (stringp face) (find face *piece-faces* :key #'symbol-name :test #'string-equal)))
                    (made (and (consp action) (stringp (first action))
                               (funcall action-function action))))
               (incf at count)
               (push (cond (made (list text face made)) (face (cons text face)) (t text)) pieces)))
    (nreverse pieces)))
