(defpackage #:populate-database
  (:use #:cl))

(in-package #:populate-database)


(db:with-connection 
(loop for (nutrition cal) in '(("carbohydrates" 4)
                               ("protein" 4)
                               ("fat" 9)
                               ("alcohol" 7))
     do
     (pomo:query (format nil "insert into nutrition (name,calories_per_100g) values (~a,~a);"
                         (pomo:sql-escape-string nutrition)
                         cal))))
     