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

(defvar *current-file-list* nil
  "File list of current site")

(defvar *current-article*)

(defun current-article ()
  (when (boundp '*current-article*)
    *current-article*))

(defun current-articles ()
  (loop :for article :in *current-file-list*
        :when (typep article 'article)
          :collect article))

(defclass article (file)
  ((title        :accessor article-title)
   (publish-time :accessor article-publish-time)
   (update-time  :accessor article-update-time)
   (uuid         :accessor article-uuid)
   (tags         :accessor article-tags)
   (template     :initform "default"
                 :accessor article-template)))

(push '("md" . article) *file-type-alist*)
(push '("mdwn" . article) *file-type-alist*)
(push '("markdown" . article) *file-type-alist*)

(defmethod file-collect-metadata ((file article))
  (with-open-file (in (file-path file))
    (let ((*current-article* file)
          (ast (read-article in)))
      (eval-article-ast ast :meta)))
  (values))

(defmethod file-render ((file article) site)
  (let* ((destfile (file-dest-path file site))
         (template (article-template-path file site)))
    (ensure-directories-exist destfile)
    (with-open-file (out destfile :if-exists :supersede :direction :output)
      (render-article file template out)))
  (values))

(defmethod file-dest-path :around ((file article) site)
  (merge-pathnames (make-pathname :type "html")
                   (call-next-method file site)))

(defun article-template-path (article site)
  (merge-pathnames (article-template article)
                   (site-template-dir site)))

(defun context-from-article (article)
  (let ((it article))
    `((:title        . ,(article-title it))
      (:publish-time . ,(article-publish-time it))
      (:update-time  . ,(article-update-time it))
      (:uuid         . ,(article-uuid it))
      (:tags         . ,(article-tags it))
      (:content      . nil))))

;;; article.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
