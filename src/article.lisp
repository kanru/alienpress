;;;; article.lisp --- Article Foundation

;;; Copyright (C) 2013  Kan-Ru Chen (陳侃如)

;;; Author(s): Kan-Ru Chen (陳侃如) <kanru@isil.kanru.info>

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

(in-package :alienpress)

(defvar *current-article*)

(defun current-article ()
  (when (boundp '*current-article*)
    *current-article*))

(defclass article (file)
  ((title        :accessor article-title)
   (publish-time :accessor article-publish-time)
   (update-time  :accessor article-update-time)
   (uuid         :accessor article-uuid)
   (tags         :accessor article-tags)))

(push '("md" . article) *file-type-alist*)
(push '("mdwn" . article) *file-type-alist*)
(push '("markdown" . article) *file-type-alist*)

(defmethod file-collect-metadata ((file article))
  (with-open-file (in (file-path file))
    (let ((*current-article* file)
          (ast (read-article in)))
      (eval-article-ast ast)))
  (values))

(defmethod copy-or-write-file ((file article) site)
  (let* ((destfile (file-dest-path file site)))
    (ensure-directories-exist destfile)
    (with-open-file (out destfile :if-exists :supersede :direction :output)
      (render-article file out)))
  (values))

(defmethod file-dest-path :around ((file article) site)
  (merge-pathnames (make-pathname :type "html")
                   (call-next-method file site)))

;;; article.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
