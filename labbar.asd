;;;; labbar.asd

(asdf:defsystem #:labbar
  :serial t
  :description "A simple system to handle assignments."
  :author "Fabian Ström fs@fabianstrom.se"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:postmodern
               #:ironclad)
  :components ((:file "package")
	       (:file "html-gen")
               (:file "labbar")
	       (:file "pages")))
