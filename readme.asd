;;;; readme.asd

(asdf:defsystem #:readme
  :description "create a buzzword readme"
  :author "daniel sutton <danielsutton01@gmail.com>"
  :license "Specify license here"
  :serial t
  :components ((:file "package")
               (:file "readme")
               (:file "readme_test"
                      :depends-on ("readme"))))

