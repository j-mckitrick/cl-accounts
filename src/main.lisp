;;; -*- mode:lisp; -*-

(in-package #:cl-user)

(defpackage #:accounts
  (:documentation "Hunchentoot web app.")
  (:use
   #:cl #:cl-user #:hunchentoot #:html-template #:cl-ppcre #:arnesi))

(in-package #:accounts)

(defparameter *ignorable-chars* '(#\, #\$))
(defparameter *currency-regex* "^\\$?([.,0-9]*)$")

(defun get-excel-float (cell)
  (parse-float
   (car
    (all-matches-as-strings *currency-regex*
                            (remove-if (lambda (letter) (member letter *ignorable-chars* :test #'eql)) cell)))))

(defun process-transaction-rows (rows)
  (let ((fixed-rows ()))
    (dolist (row rows)
      (when (member (eighth row) '("C" "K" "W") :test #'string=)
        (let ((fixed-row (list (intern (eighth row) :keyword)
                               (get-excel-float (ninth row)))))
          (push fixed-row fixed-rows)))
      (when (member (eighth row) '("E") :test #'string=)
        (let ((fixed-row (list (intern (eighth row) :keyword)
                               (get-excel-float (tenth row)))))
          (push fixed-row fixed-rows))))
    (nreverse fixed-rows)))

(defparameter *parsed-data* ())
(defparameter *columns* `((:col1 "Type" :col2 "Amount")))
(defparameter *rows* nil)
(defparameter *display-fn* nil)

(defun total-rows (rows row-type-keyword)
  (apply #'+ (mapcar #'cadr (remove-if-not (lambda (row) (eql (car row) row-type-keyword)) rows))))

(define-easy-handler
    (handle-home-get :uri "/" :default-request-type :get)
    ()
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template (pathname "index.html")
                             (list
                              :table-p t
                              :headers *columns*
                              :data *rows*
                              :tot-c (total-rows *parsed-data* :c)
                              :tot-k (total-rows *parsed-data* :k)
                              :tot-w (total-rows *parsed-data* :w)
                              :tot-e (total-rows *parsed-data* :e)
                              :cong-start *congregation-funds-start*
                              :cong-end 0
                              :cong-expenses 0
                              :total-start *total-funds-start*
                              :total-in 0
                              :total-out 0
                              :total-end 0))))

(defparameter *congregation-funds-start* 0)
(defparameter *congregation-funds-end* 0)
(defparameter *total-funds-start* 0)
(defparameter *total-funds-end* 0)

(define-easy-handler
	(handle-home-post :uri "/upload" :default-request-type :post)
	()
  (let ((csvfile (post-parameter "csvfile"))
        (congfunds (post-parameter "congfunds"))
        (totalfunds (post-parameter "totalfunds")))
    (when (> (length csvfile) 0)
      (destructuring-bind (path filename content-type) csvfile
        (declare (ignorable filename content-type))
        (setf *csv-data* (import-csv path))
        (setf *parsed-data* (process-transaction-rows (nthcdr 6 *csv-data*)))
        (setf *rows* (mapcar (lambda (row) `(:col1 ,(first row) :col2 ,(second row))) *parsed-data*))))
    (when (> (length congfunds) 0)
      (setf *congregation-funds-start* (parse-float congfunds)))
    (when (> (length totalfunds) 0)
      (setf *total-funds-start* (parse-float totalfunds)))
    (redirect "/")))

(defun import-csv (filename)
  "Read a file as csv and return the rows."
  (with-open-file (stream filename)
    (cl-csv:read-csv stream)))

(defun start-me ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8000 :message-log-destination nil :access-log-destination nil)))
