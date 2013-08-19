;;;; utils.lisp --- Utilities

;;; Copyright (C) 2012  Kan-Ru Chen

;;; Author(s): Kan-Ru Chen <kanru@kanru.info>

;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:

;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.

;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;;;; Commentary:

;;; 

;;;; Code:

(in-package #:alienpress)

(defun absolute-pathname-p (pathname)
  (eq :absolute (car (pathname-directory pathname))))

(defun absolute-pathname (pathname &optional (defaults *default-pathname-defaults*))
  "Return the PATHNAME in absolute form."
  (let ((pathname (or pathname defaults)))
    (cond
      ((absolute-pathname-p pathname) pathname)
      (t (merge-pathnames pathname defaults)))))

(defun relative-pathname (pathname &optional (defaults *default-pathname-defaults*))
  "Get relative pathname of PATHNAME based on DEFAULTS directory."
  (let ((orig-dir (pathname-directory (absolute-pathname pathname)))
        (base-dir (pathname-directory (absolute-pathname defaults))))
    (let ((pos (mismatch orig-dir base-dir :test #'equal)))
      (make-pathname :directory (cons :relative (and pos (subseq orig-dir pos)))
                     :defaults pathname))))

(defun hidden-pathname-p (pathname)
  "If a PATHNAME is hidden by the UNIX shell convention.
That is, whether the last component starts with a #\."
  (let ((name (or (pathname-name pathname)
                  (car (last (pathname-directory pathname))))))
    (char= #\. (char name 0))))

;;; utils.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
