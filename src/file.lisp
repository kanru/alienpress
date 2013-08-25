;;;; file.lisp --- File base class

;;; Copyright (C) 2013  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@kanru.info>

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

;;;; Code:

(in-package :alienpress)

(defvar *file-type-alist* nil
  "A list of (type . class) for different file types.")

(defclass file ()
  ((path :initarg :path
         :reader file-path)
   (mtime :reader file-mtime)))

(defmethod initialize-instance :after ((instance file) &rest initargs)
  (declare (ignore initargs))
  (with-slots (mtime path)
      instance
    (setf mtime (file-write-date path))))

(defmethod print-object ((object file) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (file-path object) stream)))

(defun make-file (path)
  (make-instance 'file :path path))

(defun file-upgrade-type (file)
  "Upgrade FILE's type according to it's `pathname-type'."
  (let ((type (assoc (pathname-type (file-path file))
                     *file-type-alist* :test #'equalp)))
    (when type
      (change-class file (cdr type)))))

(defun file-name (file)
  (pathname-name (file-path file)))

(defun file-destdir (file site)
  (let ((srcpath (file-path file))
        (srcdir  (site-source-dir site))
        (destdir (site-destdir site)))
    (merge-pathnames (relative-pathname srcpath srcdir) destdir)))

(defgeneric file-collect-metadata (file)
  (:documentation "Collect metadata of FILE."))

(defgeneric copy-or-write-file (file site)
  (:documentation "Write files to their destination."))

(defmethod file-collect-metadata ((file file))
  (values))

(defmethod copy-or-write-file ((file file) site)
  (let ((destdir (file-destdir file site))
        (from (file-path file)))
    (ensure-directories-exist destdir)
    (fad:copy-file from destdir :overwrite t))
  (values))

;;; file.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
