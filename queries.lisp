(in-package #:tsoha-queries)

(defparameter *limit* 25)

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

  (db:with-connection-and-transaction

      (let ((recipe (make-instance 'db::recipe
                                   :name name
                                   :instructions instructions)))

        (pomo:insert-dao recipe)

        (loop for (ing-name amount unit) in details collect
             (let ((ing (make-instance 'db::ingredient
                                       :name ing-name)))
               (ignore-errors 
                   (pomo:insert-dao ing))
               (pomo:insert-dao (make-instance 'db::recipe-ingredient-unit-amount
                                               :recipe-id (db::id recipe)
                                               :ingredient-name (db::name ing)
                                               :unit-name unit
                                               :amount amount))))
        (db::id recipe))))
                                           
      


(defun search-recipe-by-name (name)
  "Returns a list of DB::RECIPEs."
  (db:with-connection 
    (pomo:query-dao 'db::recipe
                    (format nil "select * from recipe where name like ~a;"
                            (pomo:sql-escape-string name)))))


(let ((units (list "grams" "dl" "litres" "ounces" "teaspoon" "kpl" "kilograms")))

  (db:with-connection
    (loop for unit in units do
         (handler-case
             (pomo:insert-dao (make-instance 'db::unit :name unit))
           (cl-postgres-error:unique-violation (e)
             (declare (ignorable e))
             (warn "Unit already exists: ~a" unit)))))

  (defun unit-list ()
    units))


(defun recipe-count ()
  (db:with-connection 
    (caar (pomo:query "select count(*) from recipe;"))))


(defun newest-recipes (&optional (limit *limit*))
  "Returns a list of recipe ids (integers)"
  (mapcar #'car
  (db:with-connection
    (pomo:query (format nil "select id from recipe order by id desc limit ~d" limit)))))




(defun recipe-details (recipe-id)
  (format nil "~{~{~a ~f ~a~%~}~}"
  (db:with-connection
    (pomo:query
     (format nil "select ingredient_name, amount, unit_name from \
recipe_ingredient_unit_amount where recipe_id = ~d;" recipe-id)))))




  

