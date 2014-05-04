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

(defun current-articles (&key (exclude (current-article)))
  (loop :for article :in *current-file-list*
        :when (and (not (eq article exclude))
                   (typep article 'article))
          :collect article))

(defclass article (file)
  ((title        :initform "" :accessor article-title)
   (publish-time :initform "" :accessor article-publish-time)
   (update-time  :initform () :accessor article-update-time)
   (uuid         :initform "" :accessor article-uuid)
   (tags         :initform () :accessor article-tags)
   (template     :initform "default"
                 :accessor article-template)
   (type         :initform "article"
                 :accessor article-type)
   (blog-limit   :initform 10 :accessor article-blog-limit)))

(push '("md" . article) *file-type-alist*)
(push '("mdwn" . article) *file-type-alist*)
(push '("markdown" . article) *file-type-alist*)

(defun rfc2822-skip-to-body (&optional (stream *standard-input*))
  (loop :for field = (read-line stream nil)
        :while (and field (string/= field "")))
  (values))

(defun rfc2822-read-body (&optional (stream *standard-input*))
  (rfc2822-skip-to-body stream)
  (let ((*print-pretty* nil))
    (with-output-to-string (datum)
      (let ((buffer (make-array 4096 :element-type 'character)))
        (loop :for bytes-read = (read-sequence buffer stream)
              :do (write-sequence buffer datum :start 0 :end bytes-read)
              :while (= bytes-read 4096))))))

;;; TODO: Read multiple line header
(defun rfc2822-read-headers (&optional (stream *standard-input*))
  (let ((collector nil))
    (loop :for field = (read-line stream nil)
          :while (and field (string/= field ""))
          :do (let* ((colon (position #\: field))
                     (name (subseq field 0 colon))
                     (body (string-trim '(#\Space) (subseq field (1+ colon)))))
                (push (cons (string-downcase name) body) collector)))
    collector))

(defun parse-tags (input)
  (mapcar (lambda (token)
            (string-trim " " token))
          (split-sequence #\, input)))

;;; FIXME: Better metadata handling
(defmethod file-collect-metadata ((file article))
  (with-open-file (in (file-path file))
    (let ((headers (rfc2822-read-headers in)))
      (dolist (field headers)
        (destructuring-bind (name . body) field
          (cond
            ((string= name "title")
             (setf (article-title file) body))
            ((or (string= name "date")
                 (string= name "published"))
             (setf (article-publish-time file) body))
            ((string= name "updated")
             (setf (article-update-time file) body))
            ((or (string= name "uuid")
                 (string= name "id")
                 (string= name "guid"))
             (setf (article-uuid file) body))
            ((string= name "tags")
             (setf (article-tags file) (parse-tags body)))
            ((string= name "template")
             (setf (article-template file) body))
            ((string= name "type")
             (setf (article-type file) body))
            ((string= name "blog-limit")
             (setf (article-blog-limit file)
                   (parse-integer body))))))))
  (values))

(defun markup-to-html (markdown)
  "Translate MARKDOWN string to html."
  (with-output-to-string (datum)
    (markdown:markdown markdown :stream datum)))

(defun apply-template (template context)
  (mustache:render* template context))

(defun template-path (template site)
  (merge-pathnames template
                   (site-template-dir site)))

(defun article-render (article site &optional (stream *standard-output*) template)
  (let* ((template (or (and template
                            (template-path template site))
                       (article-template-path article site)))
         (*current-article* article)
         (content (with-open-file (in (file-path article))
                    (rfc2822-read-body in)))
         (context (context-from-site (current-site)))
         (context (append (context-from-article article) context))
         (context (acons :content (markup-to-html content) context)))
    (when (string= "blog" (article-type article))
      (with-output-to-string (datum)
        (loop :for ar :in (current-articles :exclude article)
              :for index :upto (article-blog-limit article)
              :do (article-render ar (current-site) datum "blog-inline"))
        (setf context (acons :blog-content
                             (get-output-stream-string datum) context))))
    (write-string (apply-template template context) stream))
  (values))

(defmethod file-render ((file article) site)
  (let ((destfile (file-dest-path file site)))
    (ensure-directories-exist destfile)
    (with-open-file (out destfile :if-exists :supersede :direction :output)
      (article-render file site out)))
  (values))

(defmethod file-destdir ((file article) site)
  (let ((srcpath (file-path file))
        (srcdir  (site-source-dir site))
        (destdir (site-destdir site)))
    (if (string= "index" (file-name file))
        (merge-pathnames (relative-pathname srcpath srcdir) destdir)
        (merge-pathnames
         (make-pathname :directory
                        (append (pathname-directory
                                 (relative-pathname srcpath srcdir))
                                (list (file-name file))))
         destdir))))

(defmethod file-dest-path :around ((file article) site)
  (merge-pathnames (make-pathname :type "html")
                   (call-next-method file site)))

(defmethod file-dest-path ((file article) site)
  (let ((destdir (file-destdir file site)))
    (merge-pathnames (make-pathname :name "index"
                                    :type "html")
                     destdir)))

(defun article-template-path (article site)
  (merge-pathnames (article-template article)
                   (site-template-dir site)))

(defun context-from-article (article)
  (let ((it article))
    `((:title        . ,(article-title it))
      (:publish-time . ,(article-publish-time it))
      (:update-time  . ,(or (article-update-time it)
                            (article-publish-time it)))
      (:uuid         . ,(article-uuid it))
      (:tags         . ,(article-tags it))
      (:content      . nil))))

;;; article.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
