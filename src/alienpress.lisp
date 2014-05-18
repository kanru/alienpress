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

(defvar *verbose* 2
  "Enable verbose output if non-nil")

(defun log-i (control-string &rest args)
  (when (>= *verbose* 2)
    (format t "; ")
    (apply #'format t control-string args)
    (fresh-line)))

(defun log-d (control-string &rest args)
  (when (>= *verbose* 3)
    (format t "; ")
    (apply #'format t control-string args)
    (fresh-line)))

(defun compile-site (site &optional force)
  (let ((*current-site* site)
        (*current-file-list* (mapcar #'make-file (site-source-files site))))
    (mapc #'file-upgrade-type *current-file-list*)
    (mapc #'file-collect-metadata *current-file-list*)
    (mapc (lambda (file)
            (cond
              ((or (file-modified-p file site) force)
               (log-i "compiling file ~A" (file-path file))
               (file-render file site)
               (log-i "~A written" (file-dest-path file site)))
              (t
               (log-d "skip ~A" (file-path file)))))
          *current-file-list*)
    (log-i "finished."))
  (values))

(defun load-config-file (path)
  (uiop:with-current-directory (path)
    ;; XXX security
    (load path)))

(defun compile-all-sites (&optional force)
  (mapc #'(lambda (site) (compile-site site force)) *site-list*)
  (values))

;;; alienpress.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
