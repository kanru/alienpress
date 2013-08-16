;;;; alienpress.lisp --- Alienpress

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

(defvar *verbose* t
  "Enable verbose output if non-nil")

(defun info (control-string &rest args)
  (when *verbose*
    (format t "* ")
    (apply #'format t control-string args)
    (format t "~%")))

(defun compile-site (site)
  (let* ((cache (read-cache "files" site))
         (current (file-list-of site))
         (changed (changed-files cache current)))
    (loop :for file-info :in changed
          :do
             (compile-page site file-info))
    (save-cache current "files" site)))

(defun select-template (site file-info)
  (let ((pathname (make-pathname :name "default"
                                 :type mustache:*default-pathname-type*
                                 :defaults (templatedir site))))
    (info "  select template ~A" pathname)
    (mustache-compile pathname)))

(defun compile-page (site file-info)
  (info "compile page ~a" (path file-info))
  (let* ((tmpl (select-template site file-info))
         (input-file (path file-info))
         (output-file (make-pathname :name (pathname-name input-file)
                                     :directory (output-dir site input-file)
                                     :type "html"))
         (article (read-article input-file))
         (mdwn (markdown:markdown (process-article article)
                                  :stream nil :format :none)))
    (info "  output file ~a" output-file)
    (with-open-file (output output-file :direction :output :if-exists :supersede)
      (let ((mustache:*mustache-output* output))
        (funcall tmpl `((:title . "Test")
                        (:content . ,(markdown:render-to-stream mdwn :html nil))))))))

;;; alienpress.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
