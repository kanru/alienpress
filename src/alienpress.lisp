;;;; alienpress.lisp --- Alienpress

;;; Copyright (C) 2012, 2013  Kan-Ru Chen

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

(defvar *verbose* t
  "Enable verbose output if non-nil")

(defun info (control-string &rest args)
  (when *verbose*
    (format t "; ")
    (apply #'format t control-string args)
    (fresh-line)))

(defun compile-site (site)
  (let ((files (mapcar #'make-file (site-source-files site))))
    (mapc #'file-upgrade-type files)
    (mapc #'file-collect-metadata files)
    (mapc (lambda (file)
            (info "compiling file ~A" (file-path file))
            (info "~A written" (file-destdir file site))
            (copy-or-write-file file site)) files))
  (values))

;;; alienpress.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
