;;;; groups.lisp --- Things joined into groups, each joined to one of its group joining the groups.
;;;; Copyright (C) 2026 Michael A Jones.  GPL-3.0-or-later; see LICENSE.
;;;;
;;;; Blocks of paper beside each other, characters copied from one another,
;;;; files of one case: many checks join things two at a time and then ask
;;;; which group each fell in. Here the things are numbered from 0, and a
;;;; vector gives each the one it was joined under; following it up to one
;;;; joined under itself gives the group's root. Each step of the way is
;;;; halved as it is followed, so that the paths stay short, and no step
;;;; calls itself, so that a chain of a million joins takes no stack.

(in-package #:palimpsest)

(deftype disjoint-sets ()
  "Things numbered from 0 in groups, as MAKE-DISJOINT-SETS makes them."
  '(simple-array (unsigned-byte 32) (*)))

(defun make-disjoint-sets (count)
  "COUNT things, numbered 0 to COUNT - 1, each a group of its own, to be
joined by JOIN-SETS."
  (let ((sets (make-array count :element-type '(unsigned-byte 32))))
    (dotimes (k count sets) (setf (aref sets k) k))))

(declaim (inline set-root join-sets))

(defun set-root (sets k)
  "The root of the group of K in SETS: the same number for all of one group,
the least of them."
  (declare (type disjoint-sets sets) (type fixnum k))
  (loop until (= (aref sets k) k)
        do (setf (aref sets k) (aref sets (aref sets k)) k (aref sets k)))
  k)

(defun join-sets (sets a b)
  "Join the groups of A and B in SETS into one; its root (SET-ROOT)."
  (declare (type disjoint-sets sets))
  (let ((ra (set-root sets a)) (rb (set-root sets b)))
    (cond ((< ra rb) (setf (aref sets rb) ra) ra)
          ((> ra rb) (setf (aref sets ra) rb) rb)
          (t ra))))

(defun set-groups (sets &optional items)
  "The groups of SETS, each a list of its things in their order, the groups
in the order of their first things; each thing K given as the Kth of the
vector ITEMS when given, else as K."
  (declare (type disjoint-sets sets))
  (let ((groups (make-hash-table)) (roots '()))
    (dotimes (k (length sets))
      (let ((root (set-root sets k)))
        (unless (gethash root groups) (push root roots))
        (push (if items (aref items k) k) (gethash root groups))))
    (loop for root in (nreverse roots) collect (nreverse (gethash root groups)))))
