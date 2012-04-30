;;; -*- mode:lisp; -*-

(in-package #:cl-user)

(defpackage #:accounts
  (:documentation "Hunchentoot web app.")
  (:use
   #:cl #:cl-user #:hunchentoot #:html-template #:cl-ppcre #:arnesi))

(in-package #:accounts)

(declaim (optimize debug))

(defparameter *ignorable-chars* '(#\, #\$))
(defparameter *currency-regex* "^\\$?([.,0-9]*)$")

(defun get-excel-float (cell)
  (let ((raw-cell (car
                   (all-matches-as-strings *currency-regex*
                                           (remove-if (lambda (letter) (member letter *ignorable-chars* :test #'eql)) cell)))))
    (if (> (length raw-cell) 0)
        (parse-float raw-cell)
        0)))

(defun process-transaction-rows (rows)
  "For C, K, W, and A ROWS, capture the RECEPITS IN column.
For E ROWS, capture the CHECKING OUT column."
  (let ((fixed-rows ())
        (i 7))
    (dolist (row rows)
      (format t "Parsing row: ~A~%" i)
      (when (member (eighth row) '("C" "K" "W" "A") :test #'string=)
        (let ((fixed-row (list (intern (eighth row) :keyword)
                               (get-excel-float (ninth row)))))
          (push fixed-row fixed-rows)))
      (when (member (eighth row) '("E") :test #'string=)
        (let ((fixed-row (list (intern (eighth row) :keyword)
                               (get-excel-float (nth 13 row)))))
          (push fixed-row fixed-rows)))
      (incf i))
    (nreverse fixed-rows)))

(defparameter *parsed-data* ())
(defparameter *columns* `((:col1 "Type" :col2 "Amount")))
(defparameter *rows* nil)
(defparameter *display-fn* nil)

(defparameter *total-congregation-contrib* 0)
(defparameter *total-khf* 0)
(defparameter *total-www* 0)
(defparameter *total-wgah* 0)
(defparameter *total-congregation-expenses* 0)

(defun total-rows (rows &rest row-type-keywords)
  (apply #'+ (mapcar #'cadr (remove-if-not (lambda (row) (member (car row) row-type-keywords)) rows))))

(define-easy-handler
    (handle-home-get :uri "/" :default-request-type :get)
    ()
  (with-output-to-string (*default-template-output*)
    (fill-and-print-template (pathname "index.html")
                             (list
                              :table-p t
                              :headers *columns*
                              :data *rows*
                              :tot-c *total-congregation-contrib*
                              :tot-k *total-khf*
                              :tot-w *total-www*
                              :tot-a *total-wgah*
                              :tot-e *total-congregation-expenses*
                              :cong-start *congregation-funds-start*
                              :cong-contributions *total-congregation-contrib*
                              :cong-expenses *total-congregation-expenses*
                              :cong-end *congregation-funds-end*
                              :total-start *total-funds-start*
                              :total-in *total-funds-in*
                              :total-out *total-funds-out*
                              :total-end  (format nil "~4,2f" (/ (round (* *total-funds-end* 100)) 100))))))

(defparameter *congregation-funds-start* 0)
(defparameter *congregation-funds-end* 0)
(defparameter *total-funds-start* 0)
(defparameter *total-funds-end* 0)
(defparameter *total-funds-in* 0)
(defparameter *total-funds-out* 0)

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
        (setf *total-congregation-contrib* (total-rows *parsed-data* :c)
              *total-khf* (total-rows *parsed-data* :k)
              *total-www* (total-rows *parsed-data* :w)
              *total-wgah* (total-rows *parsed-data* :a)
              *total-congregation-expenses* (total-rows *parsed-data* :e))
        (setf *rows* (mapcar (lambda (row) `(:col1 ,(first row) :col2 ,(second row))) *parsed-data*))))
    (when (> (length congfunds) 0)
      (setf *congregation-funds-start* (parse-float congfunds)))
    (when (> (length totalfunds) 0)
      (setf *total-funds-start* (parse-float totalfunds)))
    (when (and *congregation-funds-start*
               *total-funds-start*)
      (setf *congregation-funds-end* (- (+ *congregation-funds-start*
                                           *total-congregation-contrib*)
                                        *total-congregation-expenses*)
            *total-funds-in* (+ *total-congregation-contrib*
                                *total-khf*
                                *total-www*
                                *total-wgah*)
            *total-funds-out* (+ *total-khf*
                                 *total-www*
                                 *total-wgah*
                                 *total-congregation-expenses*)
            *total-funds-end* (- (+ *total-funds-start*
                                    *total-funds-in*)
                                 *total-funds-out*)))
    (redirect "/")))

(defun import-csv (filename)
  "Read a file as csv and return the rows."
  (with-open-file (stream filename)
    (cl-csv:read-csv stream)))

(defun start-me ()
  (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port 8000 :message-log-destination nil :access-log-destination nil)))
