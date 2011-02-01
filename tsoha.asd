(defpackage #:tsoha-asd
  (:use #:cl #:asdf))

(in-package :tsoha-asd)

(defsystem tsoha
  :author "Janne Ronkonen"
  :depends-on (:hunchentoot  ;; web-server
               :postmodern   ;; db-bindings
               :cl-ppcre     ;; Perl-compatible regexps
               :lml2)        ;; html-generation
  :components ((:file "packages")
               (:file "pages" :depends-on ("packages"))
               (:file "main" :depends-on ("packages"
                                          "pages"))))



                                     
                                                     


               

