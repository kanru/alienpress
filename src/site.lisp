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
   (sitedir :initarg :sitedir
            :initform nil
            :accessor site-sitedir)
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

(defun ensure-absolute-site-directory (site)
  "Ensure the directories in SITE is absolute."
  (with-accessors ((sitedir site-sitedir)
                   (destdir site-destdir)) site
    (setf sitedir (absolute-pathname sitedir))
    (setf destdir (absolute-pathname destdir))))

(defun site-source-dir (site)
  (merge-pathnames (make-pathname :directory '(:relative "source"))
                   (site-sitedir site)))

(defun site-template-dir (site)
  (merge-pathnames (make-pathname :directory '(:relative "template"))
                   (site-sitedir site)))

(defun site-cache-dir (site)
  (merge-pathnames (make-pathname :directory '(:relative ".alienpress"))
                   (site-sitedir site)))

(defun site-source-files (site)
  "Return a list of files."
  (let (list)
    (labels ((add-file-to-list (pathname)
               (when (pathname-name pathname)
                 (push pathname list))))
      (fad:walk-directory
       (site-source-dir site) #'add-file-to-list
       :directories :breadth-first
       :test (complement #'hidden-pathname-p)))
    list))

;;; config.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End: