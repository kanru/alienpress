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
(defparameter *recognized-options* nil)

(defclass site ()
  ((name :initarg :name
         :accessor name)
   (title :initarg :title
          :accessor title)
   (srcdir :initarg :srcdir
           :initform nil
           :accessor srcdir)
   (destdir :initarg :destdir
            :initform nil
            :accessor destdir)
   (baseurl :initarg :baseurl
            :accessor baseurl)
   (plugins :initarg :plugins
            :accessor plugins)
   (templatedir :initarg :templatedir
                :initform nil
                :accessor templatedir)
   (basedir :initarg :basedir
            :initform nil
            :accessor basedir)
   (options :initarg :options
            :initform nil
            :accessor options)))

(defmacro defsite (name &body options)
  (when (not (zerop (rem (length options) 2)))
    (error "Odd number of options"))
  (let ((site (gensym)))
    `(let ((,site (make-site ,(string name)
                             ',options)))
       (ensure-site-directory ,site (dirname *load-pathname*))
       (push ,site *site-list*)
       ,site)))

(defun dirname (pathname)
  (make-pathname :directory (pathname-directory pathname)))

(defun absolute-directory-p (pathname)
  (eql :absolute (car (pathname-directory pathname))))

(defun ensure-absolute-directory (pathname &optional default-pathname)
  (if (null pathname)
      default-pathname
      (if (absolute-directory-p pathname)
          pathname
          (pathname-as-directory
           (merge-pathnames pathname default-pathname)))))

(defun ensure-site-directory (site default-pathname)
  (with-accessors ((base basedir)
                   (srcd srcdir)
                   (dest destdir)
                   (tmpl templatedir)) site
    (setf base (ensure-absolute-directory base default-pathname))
    (setf srcd (ensure-absolute-directory srcd base))
    (setf dest (ensure-absolute-directory dest base))
    (setf tmpl (ensure-absolute-directory tmpl base))))

(defun make-site (name options)
  (let ((site (make-instance 'site :name name)))
    (doplist (key val options)
      (let ((slot (intern (string key) #.*package*)))
        (if (slot-exists-p site slot)
            (setf (slot-value site slot) val)
            (progn
              (when (not (member key *recognized-options*))
                (warn "Unrecognized option ~a" key))
              (setf (options site) (acons key val (options site)))))))
    site))

(defun option (site option)
  (assoc-value (options site) option))

(defun cache-dir (site)
  (merge-pathnames #P".alienpress/"
                   (make-pathname :directory (srcdir site))))

;;; config.lisp ends here

;;; Local Variables:
;;; mode: lisp
;;; End:
