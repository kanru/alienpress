;;;; db.lisp --- Metadata Database

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

;;; File

(in-package #:alienpress)

(defclass file-info ()
  ((mtime :initarg :mtime
          :accessor mtime)
   (pathname :initarg :path
             :accessor path))
  (:documentation "File information that needs to be kept."))

(defmethod print-object ((object file-info) stream)
  (prin1 (list `(:pathname ,(path object)
                 :mtime ,(mtime object))) stream))

(defmethod marshal:class-persistant-slots ((class file-info))
  '(mtime pathname))

(defun file-list-of (site)
  (let (list)
    (walk-directory (srcdir site)
                    (lambda (p)
                      (when (not (directory-pathname-p p))
                        (push (make-instance
                               'file-info
                               :mtime (file-write-date p)
                               :path p)
                              list)))
                    :directories :breadth-first
                    :test (lambda (name)
                            (let ((name (or (pathname-name name)
                                            (car (last (pathname-directory name))))))
                              (not (char= #\. (char name 0))))))
    list))

;;; db.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
