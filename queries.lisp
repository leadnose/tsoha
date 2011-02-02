(in-package #:tsoha-queries)

(defun add-recipe (&key
                   name
                   instructions
                   details)
  "NAME and INSTRUCTIONS are supposed to be strings,
DETAILS is a list of form

   ((INGREDIENT-NAME AMOUNT UNIT) ...)

where INGREDIENT and UNIT are strings and AMOUNT is a number.

When successful, returns RECIPE-ID. 

When not successful, raises an error."

  (db:with-connection
    (pomo:with-transaction ()

      (let ((recipe (make-instance 'db::recipe
                                   :name name
                                   :instructions instructions)))
        (pomo:insert-dao recipe)

        (loop for (ing-name amount unit) in details collect
             (let ((ing (make-instance 'db::ingredient
                                       :name ing-name)))
               (pomo:save-dao ing)
               (pomo:insert-dao (make-instance 'db::recipe-ingredient-unit-amount
                                               :recipe-id (db::id recipe)
                                               :ingredient-id (db::id ing)
                                               :unit-name unit
                                               :amount amount))))
        (db::id recipe)))))
                                           
      


                                  

  