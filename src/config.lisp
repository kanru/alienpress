;;;; config.lisp --- Config handling

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

(defparameter *site-list* nil)

(defclass site ()
  ((name :initarg :name
         :accessor site-name)
   (srcdir :initarg :srcdir
           :initform nil
           :accessor site-srcdir)
   (destdir :initarg :destdir
            :initform nil
            :accessor site-destdir)
   (title :initarg :title
          :accessor title)
   (baseurl :initarg :baseurl
            :accessor site-baseurl)))

(defmethod initialize-instance :after ((instance site) &rest initargs)
  (declare (ignore initargs))
  (ensure-absolute-site-directory instance)
  (push instance *site-list*))

(defmethod print-object ((object site) stream)
  (print-unreadable-object (object stream :type t :identity t)
    (princ (site-name object) stream)))

(defun absolute-pathname-p (pathname)
  (eql :absolute (car (pathname-directory pathname))))

(defun absolute-directory (pathname &optional (default-pathname *default-pathname-defaults*))
  "Return the PATHNAME in absolute form."
  (let ((pathname (pathname (or pathname default-pathname))))
    (cond
      ((absolute-pathname-p pathname) pathname)
      (t (fad:pathname-as-directory
          (merge-pathnames pathname default-pathname))))))

(defun ensure-absolute-site-directory (site)
  "Ensure the directories in SITE is absolute."
  (with-accessors ((srcdir site-srcdir)
                   (destdir site-destdir)) site
    (setf srcdir  (absolute-directory srcdir))
    (setf destdir (absolute-directory destdir))))

(defun site-cache-dir (site)
  (merge-pathnames (make-pathname :directory '(:relative ".alienpress"))
                   (site-srcdir site)))

;;; config.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
