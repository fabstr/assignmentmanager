;;;; labbar.asd

(asdf:defsystem #:assignmentmanager
  :serial t
  :description "A simple system to handle assignments."
  :author "Fabian Ström fs@fabianstrom.se"
  :license "MIT"
  :depends-on (#:hunchentoot
               #:postmodern
               #:ironclad)
  :components ((:file "package")
	       (:file "html-gen")
               (:file "main")
	       (:file "pages")))
