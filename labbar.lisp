;;;; labbar.lisp

(in-package #:labbar)

(defvar *database* "labbar")
(defvar *dbuser* "lisp")
(defvar *dbpassword* "l6dQDFDjWaj4rRs2gaW7Te0TQ2Y")
(defvar *dbhost* "localhost")

(defparameter *acceptor* nil "A variable to hold the hunchentoot acceptor.")

(defun start (&optional (port 80))
  "Start the web server at port, then connect to the database."
  (setf *acceptor* (hunchentoot:start (make-instance 'hunchentoot:easy-acceptor :port port)))
  (postmodern:connect-toplevel *database* *dbuser* *dbpassword* *dbhost*))

(defun stop ()
  "Stop the web server and disconnect from the database."
  (hunchentoot:stop *acceptor*)
  (postmodern:disconnect-toplevel))

(defclass Assignment ()
  ((id :col-type serial
       :reader id)
   (name :col-type string
	 :initarg :name
	 :accessor name)
   (course :col-type string
	   :initarg :course
	   :accessor course)
   (deadline :col-type date
	     :initarg :deadline
	     :accessor deadline)
   (link :col-type string
	 :initarg :link
	 :accessor link)
   (mandatory :col-type string
	      :initarg :mandatory
	      :accessor mandatory)
   (bonus :col-type string
	  :initarg :bonus
	  :accessor bonus)
   (notes :col-type string
	  :initarg :notes
	  :accessor notes))
  (:metaclass postmodern:dao-class)
  (:keys id)
  (:table-name "assignments"))

(defclass user ()
  ((id :col-type serial
       :initarg :id
       :reader user-id)
   (username :col-type string
	     :initarg :username
	     :reader user-username
	     :documentation "The username")
   (password :col-type string
	     :initarg :password
	     :reader user-password
	     :documentation "The password, it is hashed by hash-password with salt nhashes.")
   (salt :col-type string
	 :initarg :salt
	 :reader user-salt
	 :documentation "The salt used to hash the password.")
   (nhashes :col-type int
	    :initarg :nhashes
	    :reader user-nhashes
	    :documentation "The number of times the password was hashed."
	    :initform 10))
  (:metaclass postmodern:dao-class)
  (:keys id)
  (:table-name "users"))

(defun get-all-assignments ()
  "Return a list of all the assignments, in ascending order by the deadline."
  (postmodern:select-dao 'assignment t 'deadline ))

(defun get-assignment (id)
  "Return the first assignment with the given id."
  (first (postmodern:select-dao 'assignment (:= 'id id))))

(defun add-assignment (a)
  "Add the assignment a to the database,
then return the assignment with the correct id."
  (postmodern:insert-dao a))

(defun remove-assignment (id)
  "Remove the assignment with the given id."
  (postmodern:delete-dao (get-assignment id)))

(defun update-assignment (a)
  "Update the assignment."
  (postmodern:update-dao a))

(defun get-user (username)
  (first (postmodern:select-dao 'user (:= 'username username))))

(defun string-to-date (str)
  "str should be on the form YYYY-MM-DD."
  (let ((year (subseq str 0 4))
	(month (subseq str 5 7))
	(day (subseq str 8 10)))
    (encode-universal-time
     0 0 0
     (read-from-string day)
     (read-from-string month)
     (read-from-string year))))

(defun date-to-string (universal-time)
  "Return a string \"YYYY-MM-DD\" from universal-time."
  (multiple-value-bind (s m h day month year) (decode-universal-time universal-time)
    (declare (ignore s m h))
    (when (< month 10) (setf month (format nil "0~a" month)))
    (when (< day 10) (setf day (format nil "0~a" day)))
    (format nil "~a-~a-~a" year month day)))

(dolist (e '(("/css.css" "css.css")
	     ("/datepicker.css" "datepicker.css")
	     ("/bootstrap-datepicker.js" "bootstrap-datepicker.js")
	     ("/iefix.js" "iefix.js")))
  (push (hunchentoot:create-static-file-dispatcher-and-handler (first e) (second e))
	hunchentoot:*dispatch-table*))

(defun get-days-left (time)
  "Return the number of remaining days to the universal-time time.
The number may be zero or negative."
  (let ((seconds-left (- time (get-universal-time))))
    (ceiling (/ seconds-left 3600 24))))

(hunchentoot:define-easy-handler (index :uri "/") ()
  (hunchentoot:redirect "/index.html"))

(defun hash-password (string salt &optional (nhashes 10))
  "Concatenate string and salt and hash the string with sha 256. Then hash the result nhashes times."
  (let ((to-hash (ironclad:ascii-string-to-byte-array (format nil "~a~a" string salt)))
	(dig (ironclad:make-digest :sha256)))
    (dotimes (i nhashes)
      (setf to-hash (ironclad:digest-sequence dig to-hash)))
    (ironclad:byte-array-to-hex-string to-hash)))

(defgeneric user= (u1 u2)
  (:documentation "Return t if the users u1 and u2 are equal."))
(defmethod user= ((u1 user) (u2 user))
  "Two users are equal if they have the same password hash and
the same username. Testing is made with string=."
  (and (not (null u1))
       (not (null u2))
       (string= (user-password u1) (user-password u2))
       (string= (user-username u1) (user-username u2))))

(hunchentoot:define-easy-handler (add-do :uri "/add.do")
    (name course deadline link mandatory bonus notes username password)
  (let* ((admin-user (get-user username))
	 (user (make-instance 'user
			      :salt (user-salt admin-user)
			      :password (hash-password password (user-salt admin-user))
			      :username username)))
    (if (user= admin-user user)
	(progn (add-assignment (make-instance 'assignment
					      :name name
					      :course course
					      :deadline deadline
					      :link link
					      :mandatory mandatory
					      :bonus bonus
					      :notes notes))
	       (hunchentoot:redirect "/add.html?added"))
	(get-page (html (:p :class "text-warning" "Fel användaramn eller lösenord."))))))
