;;;; signatures.lisp --- A PDF's signatures and what was changed after them.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.

(in-package #:palimpsest)

;;; Signatures

(defun signer-name (octets)
  "Who signed, as a signature's PKCS#7 data OCTETS (SignedData) says, read
as data: the common name (CN) of the subject of the certificate its first
signer names, by issuer and serial number or by its key's identifier, or
the whole subject when it gives no CN (DER-NAME-PARTS); NIL when no
certificate it holds is the signer's."
  (multiple-value-bind (tag from to) (der-item octets 0 (length octets))
    (when (eql tag #x30)
      (destructuring-bind (&optional type content &rest more) (der-items octets from to)
        (declare (ignore type more))
        (when (eql (first content) #xA0)
          (multiple-value-bind (tag from to) (der-item octets (second content) (third content))
            (when (eql tag #x30)
              (let* ((items (der-items octets from to))
                     (certificates (find #xA0 items :key #'first))
                     (signers (car (last items)))
                     (signer (and (eql (first signers) #x31)
                                  (first (der-items octets (second signers) (third signers)))))
                     (id (and (eql (first signer) #x30) (second (der-items octets (second signer) (third signer))))))
                (flet ((same-p (a b)
                         (and a b (= (- (third a) (second a)) (- (third b) (second b)))
                              (not (mismatch octets octets :start1 (second a) :end1 (third a)
                                                           :start2 (second b) :end2 (third b))))))
                  (when (and certificates id)
                    (loop with at = (second certificates) and end = (third certificates)
                          while (< at end)
                          do (multiple-value-bind (tag cfrom cto next) (der-item octets at end)
                               (declare (ignore cfrom cto))
                               (unless tag (return nil))
                               (multiple-value-bind (serial issuer subject) (certificate-fields octets at next)
                                 (when (and subject
                                            (case (first id)
                                              ;; issuer and serial number
                                              (#x30 (destructuring-bind (&optional name number &rest more)
                                                        (der-items octets (second id) (third id))
                                                      (declare (ignore more))
                                                      (and (same-p name issuer) (same-p number serial))))
                                              ;; the key's identifier, written in the certificate's extensions
                                              (#x80 (and (< (second id) (third id))
                                                         (search octets octets :start1 (second id) :end1 (third id)
                                                                               :start2 (third subject) :end2 next)))))
                                   (return (let ((parts (der-name-parts octets (second subject) (third subject))))
                                             (or (cdr (assoc "CN" parts :test #'string=))
                                                 (der-name octets (second subject) (third subject)))))))
                               (setf at next)))))))))))))

(defun signatures (pdf revision)
  "The signatures REVISION holds, each (NUMBER DICT FIELD-NAME): every
dictionary with a /ByteRange among its objects."
  (let ((found '()) (fields :unread))
    (dolist (n (live-numbers pdf revision))
      (let* ((object (revision-object pdf revision n)) (value (and object (pdf-indirect-get object :value))))
        (when (and (pdf-dictionary-p value) (listp (pdf-get value "/ByteRange")) (pdf-get value "/ByteRange"))
          (when (eq fields :unread) (setf fields (form-fields pdf revision)))
          (push (list n value (let ((field (find n fields :key (lambda (f) (pdf-ref-p (pdf-get (third f) "/V"))))))
                                (and field (first field))))
                found))))
    (nreverse found)))

(defun change-kind (pdf before after number)
  "What a change to the object NUMBER from BEFORE to AFTER is: :annotation,
:form, :signature, :security (the signatures' validation data) or :content
\(anything else, which changes what the document shows)."
  (let* ((new (deref pdf after (list :ref number 0))) (old (deref pdf before (list :ref number 0)))
         (type (pdf-get new "/Type")) (subtype (pdf-get new "/Subtype")))
    (flet ((without (dict keys) (remove-if (lambda (p) (member (car p) keys :test #'string=)) dict)))
      (cond ((or (name-p type "/Sig") (name-p type "/DocTimeStamp") (pdf-get new "/ByteRange")) :signature)
            ((or (name-p type "/DSS")
                 (pdf-get new "/VRI")
                 (pdf-get new "/Certs")
                 (pdf-get new "/OCSPs")
                 (pdf-get new "/CRLs")) :security)
            ((or (name-p type "/Annot") (and (stringp subtype) (pdf-get new "/Rect") (not (pdf-get new "/FT"))))
             :annotation)
            ((or (pdf-get new "/FT") (pdf-get new "/Fields") (and (pdf-get new "/T") (pdf-get new "/Kids"))) :form)
            ((and (pdf-dictionary-p old) (pdf-dictionary-p new) (name-p type "/Page")
                  (equal (without old '("/Annots")) (without new '("/Annots"))))
             :annotation)
            ((and (pdf-dictionary-p old) (pdf-dictionary-p new) (name-p type "/Catalog")
                  (equal (without old '("/AcroForm" "/DSS" "/Extensions" "/Perms"))
                         (without new '("/AcroForm" "/DSS" "/Extensions" "/Perms"))))
             :form)
            ((let ((roles (roles-of pdf after))) (search "annotation" (or (gethash number roles) ""))) :annotation)
            ((and (name-p type "/XObject") (name-p subtype "/Form") (not (gethash number (roles-of pdf after))))
             :annotation)
            (t :content)))))

(defun check-signatures (pdf)
  "Each signature: what it claims, whether its bytes cover its whole save,
what later saves change, and objects hidden in the signed part brought into
use later."
  (let* ((last (last-revision pdf)) (octets (pdf-octets pdf)) (out '()))
    (dolist (signature (signatures pdf last))
      (guarding-item (out "the signature in object ~d" (first signature))
        (destructuring-bind (number dict field) signature
          (let* ((range (pdf-get dict "/ByteRange"))
                 (written (written-in pdf last number))
                 (signed-end (and (= (length range) 4) (every #'integerp range) (+ (third range) (fourth range))))
                 (covered (and signed-end
                               (find-if (lambda (r)
                                          (and (<= (+ (revision-eof r) 5) signed-end) (<= signed-end (revision-end r))))
                                        (pdf-revisions pdf))))
                 (contents (pdf-get dict "/Contents"))
                 (raw (and (stringp contents) (string-octets-as-read contents)))
                 (reference (find-if (lambda (r) (name-p (pdf-get (deref pdf last r) "/TransformMethod") "/DocMDP"))
                                     (pdf-array-items (dref pdf last dict "/Reference"))))
                 (rights (and (not reference)
                              (find-if (lambda (r) (let ((method (pdf-get (deref pdf last r) "/TransformMethod")))
                                                     (or (name-p method "/UR3") (name-p method "/UR"))))
                                       (pdf-array-items (dref pdf last dict "/Reference")))
                              t))
                 (permission (or (let ((p (pdf-get (dref pdf last (deref pdf last reference) "/TransformParams") "/P")))
                                   (and (integerp p) p))
                                 (and reference 2)))
                 (evidence (append (nth-value 0 (object-evidence pdf last number))
                                   (key-evidence pdf last number "/ByteRange")))
                 (steps (list (format nil "Read /ByteRange ~a in object ~d: the octets from ~a for ~a, and from ~a for ~
                                           ~a, are what was signed."
                                      range number (first range) (second range) (third range) (fourth range)))))
            (push (found :record (format nil "~a signature~@[ in field ~s~] (object ~d)~@[ by ~a~]~@[, dated ~a~]~
                                              ~@[, for ~s~]~@[, ~a~]; it is not checked here, only what it covers."
                                         (cond (reference "A certification") (rights "A usage rights")
                                               (t "An approval"))
                                         field number (and raw (signer-name raw))
                                         (let ((m (pdf-get dict "/M"))) (and (stringp m) (pdf-date-words m)))
                                         (let ((reason (pdf-get dict "/Reason"))) (and (stringp reason) reason))
                                         (let ((s (pdf-get dict "/SubFilter"))) (and (stringp s) (subseq s 1))))
                         :rule (concatenate 'string "A signature dictionary says who signed, when, why and how, and "
                                                    "/ByteRange names the octets it vouches for.")
                         :revision (if written (revision-index written) (revision-index last)) :objects (list number)
                         :evidence evidence :steps steps)
                  out)
            (cond ((not signed-end)
                   (push (found :strong (format nil "The signature in object ~d has a /ByteRange that cannot be read: ~
                                                     it vouches for nothing that can be told." number)
                                :rule "A /ByteRange is four numbers."
                                :revision (revision-index last)
                                :objects (list number)
                                :evidence evidence
                                :method :signature-bypassed)
                         out))
                  (t
                   (let ((gap-ok (and (zerop (first range))
                                      (<= 0 (second range))
                                      (< (second range) (third range))
                                      (< (third range) (length octets))
                                      (= (aref octets (second range)) 60) (= (aref octets (1- (third range))) 62))))
                     (unless gap-ok
                       (push (found :strong (format nil "The signature in object ~d leaves out more than its own ~
                                                         value: its /ByteRange does not begin at 0 or its gap is not ~
                                                         just /Contents." number)
                                    :rule (concatenate 'string "A signature's /ByteRange covers the file from octet 0, "
                                                               "leaving out only the <...> of its own /Contents.")
                                    :revision (revision-index last) :objects (list number)
                                    :evidence (append evidence (and (< -1 (second range) (length octets))
                                                                    (octets-evidence "Where the gap begins"
                                                                                     octets
                                                                                     (second range)
                                                                                     (min (length octets)
                                                                                          (+ (second range) 16)))))
                                    :method :signature-bypassed)
                             out)))
                   (cond ((null covered)
                          (push (found :strong (format nil "The signature in object ~d stops short: its bytes end at ~
                                                            ~a, where no save ends." number (offset-words signed-end))
                                       :rule (concatenate 'string "A signature covers the whole of the save it was "
                                                                  "made in, to the end of its %%EOF line.")
                                       :revision (revision-index last)
                                       :objects (list number)
                                       :evidence evidence
                                       :steps steps
                                       :method :signature-bypassed)
                                out))
                         ((and written (/= (revision-index covered) (revision-index written)))
                          (push (found :strong (format nil "The signature in object ~d, written in ~a, covers only up ~
                                                            to the end of ~a."
                                                       number (revision-name written) (revision-name covered))
                                       :rule "A signature covers the save it is written in."
                                       :revision (revision-index written)
                                       :objects (list number)
                                       :evidence evidence
                                       :steps steps
                                       :method :signature-bypassed)
                                out)))
                   ;; what later saves change
                   (when covered
                     (loop for k from (1+ (revision-index covered)) below (length (pdf-revisions pdf))
                           do (let* ((r (aref (pdf-revisions pdf) k)) (before (aref (pdf-revisions pdf) (1- k)))
                                     (kinds (loop for n in (append (revision-changed r) (revision-added r))
                                                  collect (cons n (change-kind pdf before r n))))
                                     (content (remove :content kinds :key #'cdr :test-not #'eq))
                                     (annotations (remove :annotation kinds :key #'cdr :test-not #'eq))
                                     (forms (remove :form kinds :key #'cdr :test-not #'eq))
                                     (allowed-annotations (or (null permission) (>= permission 3)))
                                     (allowed-forms (or (null permission) (>= permission 2))))
                                (cond (content
                                       (push (found (if rights :record :strong)
                                                    (format nil "~@(~a~), after the ~:[~;usage rights ~]signature in ~
                                                                 object ~d, changes what the document shows (object~p ~
                                                                 ~{~d~^, ~}): ~:[the signature does not vouch for what ~
                                                                 is shown~;usage rights let a reader's program save a ~
                                                                 form filled in, and vouch for nothing it shows~]."
                                                            (revision-name r)
                                                            rights
                                                            number
                                                            (length content)
                                                            (mapcar #'car content)
                                                            rights)
                                                    :rule (concatenate 'string "Saves after a signature may add "
                                                                               "signatures, validation data, form "
                                                                               "values and annotations as its "
                                                                               "permissions allow; any other change "
                                                                               "alters what was signed.")
                                                    :revision k :objects (mapcar #'car content)
                                                    :details (loop for (n) in content
                                                                   repeat 20
                                                                   collect (format nil "object ~d~@[, ~a~]"
                                                                                   n
                                                                                   (role-words pdf r n)))
                                                    :evidence (loop for (n) in content
                                                                    repeat 10
                                                                    append (nth-value 0 (object-evidence pdf r n)))
                                                    :method (unless rights :signature-bypassed))
                                             out))
                                      ((or (and annotations (not allowed-annotations))
                                           (and forms (not allowed-forms))
                                           (and permission (= permission 1) kinds))
                                       (push (found :strong (format nil "~@(~a~), after the signature in object ~d, ~
                                                                         makes changes its permissions (/P ~a) do not ~
                                                                         allow." (revision-name r) number permission)
                                                    :rule (concatenate 'string "A certification signature's /P allows "
                                                                               "no changes (1), form filling and "
                                                                               "signing (2), or those and annotations "
                                                                               "(3).")
                                                    :revision k :objects (mapcar #'car kinds)
                                                    :method :signature-bypassed)
                                             out))
                                      (kinds
                                       (push (found :look (format nil "~@(~a~), after the signature in object ~d, adds ~
                                                                       or changes ~{~a~^, ~}." (revision-name r) number
                                                                  (remove-duplicates
                                                                    (mapcar (lambda (k)
                                                                              (ecase (cdr k)
                                                                                (:annotation "annotations")
                                                                                (:form "form values")
                                                                                (:signature "signatures")
                                                                                (:security "validation data")))
                                                                            kinds)
                                                                    :test #'string=))
                                                    :rule (concatenate 'string "Changes after a signature are allowed "
                                                                               "when they are signatures, validation "
                                                                               "data, form values or annotations.")
                                                    :revision k :objects (mapcar #'car kinds))
                                             out)))))
                     ;; shadow attacks: in the signed part, unused, then used
                     (let ((signed-reach (reachable pdf covered)) (final-reach (reachable pdf last)) (shadows '()))
                       (maphash (lambda (n v) (declare (ignore v))
                                  (unless (gethash n signed-reach)
                                    (let ((w (written-in pdf last n)))
                                      (when (and w
                                                 (<= (revision-index w) (revision-index covered))
                                                 (not (structural-p pdf last n)))
                                        (push n shadows)))))
                                final-reach)
                       (when shadows
                         (setf shadows (sort shadows #'<))
                         (push (found (if rights :record :strong)
                                      (format nil "Object~p ~{~d~^, ~} ~:*~:*~[~;was~:;were~]~* hidden, unused, in the ~
                                                   part the ~:[~;usage rights ~]signature in object ~d covers, and ~
                                                   brought into use by a later save: ~:[a shadow attack~;as a shadow ~
                                                   attack does, though usage rights vouch for nothing shown~]."
                                              (length shadows) shadows rights number rights)
                                      :rule (concatenate 'string "An object in the signed octets that the signed "
                                                                 "document never uses, but a later save does, lets "
                                                                 "what was signed show something else.")
                                      :revision (revision-index last) :objects shadows
                                      :evidence (loop for n in shadows
                                                      repeat 10
                                                      append (nth-value 0 (object-evidence pdf last n)))
                                      :method (unless rights :signature-bypassed))
                               out))))))))))
    out))
