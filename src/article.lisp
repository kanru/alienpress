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

(defun articlep (file)
  (and (typep file 'article)
       (not (or (string= "rss" (article-type file))
                (string= "blog" (article-type file))))))

(defun sort-articles (articles)
  (stable-sort articles #'> :key #'article-publish-time))

(defun current-articles ()
  (sort-articles (remove-if-not #'articlep *current-file-list*)))

(defun articles-in-range (article)
  (loop :for ar :in (current-articles)
        :for index :upto (article-blog-limit article)
        :when (and (>= (article-publish-time ar)
                       (article-blog-after article))
                   (< (article-publish-time ar)
                      (article-blog-before article)))
          :collect ar))

(defclass article (file)
  ((title        :initform "" :accessor article-title)
   (publish-time :initform () :accessor article-publish-time)
   (update-time  :initform () :accessor article-update-time)
   (uuid         :initform "" :accessor article-uuid)
   (tags         :initform () :accessor article-tags)
   (template     :initform "default"
                 :accessor article-template)
   (type         :initform "article"
                 :accessor article-type)
   (blog-limit   :initform most-positive-fixnum :accessor article-blog-limit)
   (blog-after   :initform most-negative-fixnum :accessor article-blog-after)
   (blog-before  :initform most-positive-fixnum :accessor article-blog-before)))

(push '("md" . article) *file-type-alist*)
(push '("mdwn" . article) *file-type-alist*)
(push '("markdown" . article) *file-type-alist*)
(push '("rss" . article) *file-type-alist*)

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
             (setf (article-publish-time file)
                   (cl-date-time-parser:parse-date-time body)))
            ((string= name "updated")
             (setf (article-update-time file)
                   (cl-date-time-parser:parse-date-time body)))
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
                   (parse-integer body)))
            ((string= name "blog-after")
             (setf (article-blog-after file)
                   (cl-date-time-parser:parse-date-time body)))
            ((string= name "blog-before")
             (setf (article-blog-before file)
                   (cl-date-time-parser:parse-date-time body))))))))
  (values))

(defmethod file-modified-p ((article article) site)
  (if (or (string= "blog" (article-type article))
          (string= "rss"  (article-type article)))
      (or (call-next-method)
          (some (lambda (ar)
                  (file-modified-p ar site))
                (articles-in-range article)))
      (call-next-method)))

(defun article-self-link (article site)
  (let ((destpath (file-dest-path article site))
        (destdir  (site-destdir site))
        (baseurl (site-baseurl site)))
    (cond
      ((and (string= "index" (file-name article))
            (string= "rss" (article-type article)))
       (concatenate 'string
                    baseurl (namestring
                             (relative-pathname destpath destdir))))
      (t
       (concatenate 'string
                    baseurl (directory-namestring
                             (relative-pathname destpath destdir)))))))

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
         (context (mapcan #'collect-context (list (current-site) article)))
         (context (acons :content (markup-to-html content) context)))
    (when (or (string= "blog" (article-type article))
              (string= "rss"  (article-type article)))
      (with-output-to-string (datum)
        (mapc (lambda (ar)
                (article-render
                 ar (current-site) datum
                 (format nil "~a-inline" (article-type article))))
              (articles-in-range article))
        (setf context (acons :inline-content
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

(defmethod file-dest-path ((file article) site)
  (let ((destdir (file-destdir file site)))
    (if (string= "rss" (article-type file))
        (merge-pathnames (make-pathname :name "index"
                                        :type "rss")
                         destdir)
        (merge-pathnames (make-pathname :name "index"
                                        :type "html")
                         destdir))))

(defun article-template-path (article site)
  (merge-pathnames (article-template article)
                   (site-template-dir site)))

(defgeneric collect-context (object)
  (:documentation "Return an alist of contexts from OBJECT."))

(defmethod collect-context ((object article))
  (let ((it object))
    `((:title        . ,(article-title it))
      (:publish-time . ,(local-time:to-rfc1123-timestring
                         (local-time:universal-to-timestamp
                          (article-publish-time it))))
      (:update-time  . ,(local-time:to-rfc3339-timestring
                         (local-time:universal-to-timestamp
                          (or (article-update-time it)
                              (article-publish-time it)))))
      (:uuid         . ,(article-uuid it))
      (:tags         . ,(article-tags it))
      (:self-link    . ,(article-self-link it (current-site)))
      (:content      . nil))))

(defmethod collect-context ((object site))
  (let ((it object))
    `((:site-name  . ,(site-name it))
      (:site-title . ,(title it))
      (:baseurl    . ,(site-baseurl it)))))

;;; article.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
