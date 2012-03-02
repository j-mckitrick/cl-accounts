;;;; -*- mode: lisp -*-

(defpackage #:app-config (:export #:*base-directory*))
(defparameter app-config:*base-directory*
  (make-pathname :name nil :type nil :defaults *load-truename*))
  
(defpackage #:accounts-system (:use :cl :asdf))
(in-package #:accounts-system)

(defsystem accounts
  :name "accounts"
  :version "1.0"
  :author "Jonathon McKitrick"
  :description "Accounts web application."
  :components
  ((:module src
            :serial t
            :components ((:file "main")
                         (:file "start"))))
  :depends-on (:hunchentoot :html-template :cl-csv :arnesi :cl-ppcre))
