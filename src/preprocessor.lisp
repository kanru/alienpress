;;;; preprocessor.lisp --- Pre-Processor

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

;;;; Commentary:

;;; 

;;;; Code:

(in-package :alienpress)

(defmacro with-temp-package (&body body)
  "Create a temporary package so we don't read into our package."
  `(let ((*package* (make-package (gensym "PACKAGE"))))
     (unwind-protect
          (progn ,@body)
       (delete-package *package*))))

(defun read-article (&optional (stream *standard-input*))
  "Reads article from STREAM then outputs the article's abstract
syntax tree."
  (with-open-stream (collector (make-string-output-stream))
    (labels ((process-char (output)
               (let ((c (read-char stream nil)))
                 (cond
                   ;; Done reading
                   ((null c)
                    (cons (get-output-stream-string collector) output))
                   ;; Embedded lisp code
                   ((and (char= #\( c)
                         (eql #\( (peek-char nil stream nil)))
                    (cons (get-output-stream-string collector)
                          (cons (read-lisp) (process-char output))))
                   ;; All other characters
                   (t
                    (write-char c collector)
                    (process-char output)))))
             (read-lisp ()
               (let ((form (read stream))
                     (c (read-char stream)))
                 (if (char= #\) c)
                     (progn
                       (consume-newlines)
                       form)
                     (error 'end-of-file :stream stream))))
             (consume-newlines ()
               (loop :while (eql #\Newline (peek-char nil stream nil))
                     :do (read-char stream))))
      (with-temp-package
        (process-char nil)))))

(defun eval-article-ast (ast phase)
  "Processes the article's AST and outputs the final? article string."
  (with-output-to-string (out-stream)
    (loop :for node :in ast :do
      (etypecase node
        (string (write-string node out-stream))
        (list (write-string (eval-directive node phase) out-stream))))))

;;; TODO Add hook
(defun markup-to-html (markdown)
  "Translate MARKDOWN string to html."
  (let ((ast (markdown:markdown markdown
                                :stream nil :format :none)))
    (markdown:render-to-stream ast :html nil)))

(defun render-article (article template &optional (stream *standard-output*))
  (with-open-file (in (file-path article))
    (let ((*current-article* article)
          (ast (read-article in)))
      (let* ((content (eval-article-ast ast :format))
             (context (context-from-site (current-site)))
             (context (append (context-from-article article) context))
             (context (acons :content (markup-to-html content) context)))
        (write-string (apply-template template context) stream)))))

(defun apply-template (template context)
  (mustache:render* template context))

;;; preprocessor.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
