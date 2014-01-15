;;;; labbar.asd

(asdf:defsystem #:labbar
  :serial t
  :description "Describe labbar here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:hunchentoot
               #:postmodern
               #:ironclad)
  :components ((:file "package")
               (:file "labbar")))

