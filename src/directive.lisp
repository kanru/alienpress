;;;; directive.lisp --- Directive Foundation

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

;;;; Code:

(in-package :alienpress)

(defparameter *directive-meta-table* (make-hash-table :test #'equalp))
(defparameter *directive-format-table* (make-hash-table :test #'equalp))
(defparameter *directive-tables-alist*
  `((:meta . ,*directive-meta-table*)
    (:format . ,*directive-format-table*)))

(defun add-directive (name function category &rest other-categories)
  "Register a directive to the directive table. Optional categories
could be :meta or :format."
  (let ((categories (cons category other-categories)))
    (loop :for category :in categories
          :for table = (cdr (assoc category *directive-tables-alist*))
          :when table
            :do (setf (gethash (string name) table) function))))

(defun remove-directive (name function category &rest other-categories)
  (declare (ignore name function category other-categories))
  (error "Not implemented"))

(defun find-directive (name category)
  (let ((table (cdr (assoc category *directive-tables-alist*))))
    (when table
      (gethash (string name) table))))

(defun eval-directive (directive-args category)
  (let ((directive (find-directive (first directive-args) category))
        (args (rest directive-args)))
    (if directive
        (format nil "~@[~A~]" (with-output-to-string (*standard-output*)
                                (apply directive args)))
        "")))

;;;; Predefined directives

(defun meta-directive (&rest keyword-args)
  (labels ((parse-tags (input)
             (mapcar (lambda (token)
                       (string-trim " " token))
                     (split-sequence #\, input))))
    (let ((article (current-article)))
      (when article
        (doplist (key value keyword-args)
                 (case key
                   (:title (setf (article-title article) value))
                   (:published (setf (article-publish-time article) value))
                   (:updated (setf (article-update-time article) value))
                   (:id (setf (article-uuid article) value))
                   (:tags (setf (article-tags article) (parse-tags value)))
                   (:template (setf (article-template article) value))
                   (otherwise)))))))
(add-directive "meta" #'meta-directive :meta)

(defun template-path (template site)
  (merge-pathnames template
                   (site-template-dir site)))

(defun blog-inline-directive (&key from to tags limit (template "blog-inline")
                              &allow-other-keys)
  (declare (ignore from to tags))
  (loop :for article :in (current-articles :exclude (current-article))
        :for index :upto limit
        :when (slot-boundp article 'title)
          :do (render-article article (template-path template (current-site)))))
(add-directive "blog-inline" #'blog-inline-directive :format)

;;; directive.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
