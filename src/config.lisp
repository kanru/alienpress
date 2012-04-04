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
           :accessor srcdir)
   (destdir :initarg :destdir
            :accessor destdir)
   (baseurl :initarg :baseurl
            :accessor baseurl)
   (plugins :initarg :plugins
            :accessor plugins)
   (templatedir :initarg :templatedir
                :accessor templatedir)
   (basedir :initarg :basedir
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
       (ensure-dir ,site)
       (pushnew ,site *site-list*)
       ,site)))

(defmacro ensure-dir (site)
  `(progn
     (setf (basedir ,site)
           (pathname-directory
            (or *load-truename* *compile-file-truename*)))
     (let ((srcdir (pathname-directory (pathname-as-directory (srcdir ,site)))))
       (if (eql :relative (car srcdir))
           (setf (srcdir ,site) (append (basedir ,site) (cdr srcdir)))
           (setf (srcdir ,site) srcdir)))
     (let ((destdir (pathname-directory (pathname-as-directory (destdir ,site)))))
       (if (eql :relative (car destdir))
           (setf (destdir ,site) (append (basedir ,site) (cdr destdir)))
           (setf (destdir ,site) destdir)))))

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
