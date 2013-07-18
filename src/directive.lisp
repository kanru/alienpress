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

;;; 

;;;; Code:

(in-package :alienpress)

(defvar *directive-table* (make-hash-table :test #'equalp))

(defun register-directive (name function)
  (setf (gethash (string name) *directive-table*)
        function))

(defun find-directive (name)
  (gethash (string name) *directive-table*
           (lambda (&rest args)
             (declare (ignore args))
             (format nil "#<Undefined directive ~A>" name))))

(defun eval-directive (directive-args)
  (let ((directive (find-directive (first directive-args)))
        (args (rest directive-args)))
    (format nil "~@[~A~]" (apply directive args))))

;;;; Predefined directives

(defun meta-directive (&rest keyword-args)
  (labels ((position-skip (chars sequence &key start)
             (position-if (lambda (char) (not (member char chars)))
                          sequence :start start))
           (parse-tags (input)
             (loop :for start := 0 :then (position-skip '(#\, #\Space) input :start (1+ finish))
                   :for finish := (position #\, input :start start)
                   :collect (subseq input start finish)
                   :until (null finish))))
    (let ((article (current-article)))
      (when article
        (loop :for (key value) :on keyword-args :by #'cddr :do
          (case key
            (:title (setf (article-title article) value))
            (:published (setf (article-publish-time article) value))
            (:updated (setf (article-update-time article) value))
            (:id (setf (article-id article) value))
            (:tags (setf (article-tags article) (parse-tags value)))
            (otherwise)))))))
(register-directive "meta" #'meta-directive)

;;; directive.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
