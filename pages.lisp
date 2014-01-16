(in-package #:labbar)

(defun get-page (content)
  (html (:html (:head (:title "Labbar")
		      (:meta :http-equiv "Content-Type" :content "text/html; charset=utf-8")
		      (:meta :name "viewport" :content "target-densitydpi=device-dpi, width=device-width, initial-scale=1.0, maximum-scale=1.0")
		      (:link :rel "stylesheet" :href "//netdna.bootstrapcdn.com/bootstrap/3.0.3/css/bootstrap.min.css")
		      (:link :rel "stylesheet" :href "/datepicker.css")
		      (:link :rel "stylesheet" :href "/css.css")
		      (:script :src "https://code.jquery.com/jquery.js")
		      (:script :src "//netdna.bootstrapcdn.com/bootstrap/3.0.3/js/bootstrap.min.js")
		      (:script "$(function () {$(\".clickable\").click(function() {window.document.location = \"show.html?id=\" + $(this).attr(\"id\");});});")
		      (:script :src "bootstrap-datepicker.js")
		      (:script :src "iefix.js"))
	       (:body (:div :class "container"
			    (:h1 :class "center" "Labbhanterare")
			    (:div :class "col-md-2" "&nbsp;")
			    (:div :class "col-md-8 col-sm-12 col-xs-12"
				  content
				  (:p :class "center foot"
				      "&copy; 2013-2014 Fabian Ström" :br
				      "Observera att informationen kanske inte är korrekt."))
			    (:div :class "col-md-2" "&nbsp;"))))))

(hunchentoot:define-easy-handler (index-html :uri "/index.html") ()
  (let ((assignments (get-all-assignments)))
    (if (null assignments) (html (:p "Det finns ingenting..."))
	(get-page (html (:table :class "table table-hover table-condensed"
				(:tr (:thead (:th "Namn")
					     (:th "Kurs")
					     (:th "Deadline")
					     (:th "Dagar kvar")))
				(:p "Tryck på en rad för mer information. Grönt betyder att det är mer än en vecka kvar till deadline, gult en vecka eller mindre och rött betyder att deadline är idag, till slut betyder grått att deadline är passerad.")
				(with-output-to-string (str)
				  (dolist (a assignments)
				    (let* ((days-left (get-days-left (deadline a)))
					   (class (cond ((< 7 days-left) "success")
							((< 0 days-left) "warning")
							((= 0 days-left) "danger")
							(t "active"))))
				      (generate-html (str)
						     (:tr :class (format nil "clickable ~a" class) :id (id a)
							  (:td (name a))
							  (:td (course a))
							  (:td (date-to-string (deadline a)))
							  (:td (get-days-left (deadline a))))))))))))))

(hunchentoot:define-easy-handler (show-html :uri "/show.html") (id)
  (let ((a (get-assignment id)))
    (if (or (null id) (null a)) (hunchentoot:redirect "/index.html")
	(get-page (html (:h2 (course a) ": " (name a))
			(:table :class "table table-striped table-condensed"
				(:tr (:th "Deadline") (:td (date-to-string (deadline a)) " (" (get-days-left (deadline a)) " dagar kvar)"))
				(:tr (:th "Kurs") (:td (course a)))
				(:tr (:th "Länk") (:td (:a :href (link a) (link a))))
				(:tr (:th "Obligatorisk") (:td (mandatory a)))
				(:tr (:th "Bonus") (:td (bonus a)))
				(:tr (:th "Anteckningar") (:td (notes a))))
			(:a :href "/index.html" "Tillbaka"))))))


(hunchentoot:define-easy-handler (create-new-assignment :uri "/add.html") (added)
  (get-page (html (if added (html (:p :class "text-info" "Labben lades till.")) "")
		  (:form :action "add.do" :method "post"
			 (:div :class "form-group"
			       (:label :for "name" "Namn på labben")
			       (:input :type "text" :class "form-control" :name "name" :id "name" :placeholder "Namn"))
			 (:div :class "form-group"
			       (:label :for "course" "Kurs som labben tillhör")
			       (:input :type "text" :class "form-control" :name "course" :id "course" :placeholder "Kurs"))
			 (:div :class "form-group"
			       (:label :for "deadline" "Deadline")
			       (:input :type "text" :class "form-control" :name "deadline" :id "deadline"
				       :placeholder "ÅÅÅÅ-MM-DD"))
			 (:div :class "form-group"
			       (:label :for "link" "Länk till hemsida")
			       (:input :type "text" :class "form-control" :name "link" :id "link" :placeholder "Länk"))
			 (:div :class "form-group"
			       (:label :for "mandatory" "Obligatorisk?")
			       (:select :name "mandatory" :id "mandatory" :class "form-control"
					(:option :selected "selected" :value "Ja" "Ja")
					(:option :value "Nej" "Nej")))
			 (:div :class "form-group"
			       (:label :for "bonus" "Bonuspoäng")
			       (:input :type "text" :class "form-control" :name "bonus" :id "bonus"
				       :placeholder "Antal bonuspoäng"))
			 (:div :class "form-group"
			       (:label :for "notes" "Anteckningar")
			       (:textarea :class "form-control" :name "notes" :id "notes" :placeholder "Anteckningar"))
			 (:div :class "form-group"
			       (:label :for "password" "Autensiering")
			       (:input :type "text" :class "form-control" :name "username" :id "username"
				       :placeholder "Användarnamn")
			       (:input :type "password" :class "form-control" :name "password" :id "password"
				       :placeholder "Lösenord"))
			 (:input :type "submit" :value "Spara" :class "btn btn-primary"))
		  (:p (:a :href "/index.html" "Tillbaka"))
		  (:script "$(\"#deadline\").datepicker({format: \"yyyy-mm-dd\", weekStart: 1});"))))

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
