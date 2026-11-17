;;;; chat.lisp --- Words a chatbot leaves in text pasted from it and not read through.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Text copied from a chatbot sometimes brings along more than the answer:
;;;; the words it writes about itself ("as an AI language model"), the labels
;;;; of the page it was copied from ("Regenerate response"), its citation
;;;; marks, the tracking part of the links it gives, and the bracketed places
;;;; it leaves for a name or a date. Only whole phrases of a fixed list are
;;;; looked for. The style of the words (favourite words, sentences of even
;;;; length) is not weighed: published detectors that do so name honest
;;;; writers, above all those writing in a second language, too often to be
;;;; reported.

(in-package #:palimpsest)

(defparameter *chat-phrases*
  '((:self "as an ai language model" "as a large language model" "as an ai developed by" "i am an ai language model"
     "i'm an ai language model" "as of my knowledge cutoff" "as of my last knowledge update"
     "as of my last training update" "my knowledge cutoff" "som en ai-språkmodell" "som en stor språkmodell"
     "jag är en ai-språkmodell")
    (:interface "regenerate response" "chatgpt said:")
    (:citation "contentreference[oaicite" ":oaicite:" "†source】" "utm_source=chatgpt.com" "utm_source=openai"))
  "The phrases looked for in a page's text, by kind: :SELF, what a chatbot
writes about itself; :INTERFACE, the labels of its page copied along with an
answer; :CITATION, its citation marks and the tracking part of the links it
gives. Lower case, straight quotes, single spaces, as the page's text is
made before the search (CHAT-NORMAL-TEXT).")

(defparameter *chat-placeholders*
  '(("insert " "your " "infoga " "ditt " "din " "ert ")
    "company name" "recipient's name" "recipient name" "recipient" "företagsnamn" "företagets namn"
    "mottagarens namn" "mottagare")
  "The bracketed places left to fill that a chatbot's letters and forms keep:
the first element lists what such a place may begin with (\"[Insert date]\",
\"[Your name]\", \"[Infoga datum]\"), the rest whole places
(\"[Company name]\"). Office templates leave the same, which the finding says.")

(defun chat-normal-text (text)
  "TEXT made ready for the search of *CHAT-PHRASES*: lower case, curly quotes
and apostrophes straight, every run of blanks one space."
  (let ((out (make-string-output-stream)) (blank nil))
    (loop for c across text
          do (cond ((member c '(#\Space #\Tab #\Newline #\Return #\No-break_space))
                    (setf blank t))
                   (t (when blank (write-char #\Space out) (setf blank nil))
                      (write-char (case c
                                    ((#\Left_single_quotation_mark #\Right_single_quotation_mark) #\')
                                    ((#\Left_double_quotation_mark #\Right_double_quotation_mark) #\")
                                    (t (char-downcase c)))
                                  out))))
    (string-trim " " (get-output-stream-string out))))

(defun chat-placeholder-p (inside)
  "True when INSIDE, the text between a pair of square brackets as
CHAT-NORMAL-TEXT makes it, is a place left to fill (*CHAT-PLACEHOLDERS*)."
  (and (< 0 (length inside) 40)
       (or (some (lambda (start) (and (uiop:string-prefix-p start inside) (> (length inside) (length start))))
                 (first *chat-placeholders*))
           (member inside (rest *chat-placeholders*) :test #'string=))))

(defun chat-traces (text)
  "The traces of a chatbot in TEXT, as CHAT-NORMAL-TEXT makes it: a list of
(KIND PHRASE), KIND :self, :interface, :citation or :placeholder, PHRASE as
found in TEXT, lower case, each once, in the order of *CHAT-PHRASES*, the
places left to fill last."
  (let ((found '()))
    (loop for (kind . phrases) in *chat-phrases*
          do (dolist (phrase phrases)
               (when (search phrase text) (push (list kind phrase) found))))
    (loop for open = (position #\[ text) then (position #\[ text :start (1+ open))
          while open
          do (let* ((close (position #\] text :start open :end (min (length text) (+ open 42))))
                    (inside (and close (subseq text (1+ open) close))))
               (when (and inside (chat-placeholder-p inside))
                 (pushnew (list :placeholder (subseq text open (1+ close))) found :test #'equal))))
    (nreverse found)))

(defun check-chat-traces (check)
  "Add to CHECK the traces of a chatbot its page's text keeps (CHAT-TRACES),
worth a look: each line read, and each line with the next, so that a phrase
broken over two lines is found. The finding quotes what was found and
shows the lines it is in."
  (let ((lines (mapcar (lambda (line) (cons (chat-normal-text (format nil "~{~a~^ ~}" (text-lines line))) line))
                       (page-lines-of (page-check-texts check))))
        (traces '()) (shown '()))
    (loop for (here . rest) on lines
          for next = (first rest)
          do (dolist (trace (chat-traces (if next (format nil "~a ~a" (car here) (car next)) (car here))))
               (unless (member trace traces :test #'equal)
                 (push trace traces)
                 (setf shown (union shown (if (search (second trace) (car here))
                                              (cdr here)
                                              (append (cdr here) (cdr next))))))))
    (when traces
      (setf traces (nreverse traces))
      (let ((kinds (remove-duplicates (mapcar #'first traces))))
        (page-found check :look
                    (format nil "Page ~d keeps ~{~a~^, and ~}: ~{~s~^, ~}. ~:[It may be text pasted from a chatbot ~
                                 and not read through, or a page that quotes one~;A place left to fill is left by a ~
                                 chatbot's draft and by an office template alike~]."
                            (page-check-number check)
                            (loop for kind in kinds
                                  collect (ecase kind
                                            (:self "words a chatbot writes about itself")
                                            (:interface "the labels of a chatbot's page")
                                            (:citation "a chatbot's citation marks or link tags")
                                            (:placeholder "a place left to fill")))
                            (mapcar #'second traces)
                            (equal kinds '(:placeholder)))
                    #.(concatenate 'string "Text pasted from a chatbot can keep the words around its answer, "
                                           "its marks, or the places it left to fill.")
                    (sort (copy-list shown) #'> :key #'mark-y0)
                    :method :chatbot-text)))))
