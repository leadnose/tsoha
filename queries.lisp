(in-package #:tsoha-queries)


(defparameter *default-limit* 25)


(defun add-recipe (&key
                   name
                   instructions
                   description
                   details)
"
NAME and INSTRUCTIONS are supposed to be strings,
DETAILS is a list of form

   ((INGREDIENT-NAME AMOUNT UNIT) ...)

where INGREDIENT and UNIT are strings and AMOUNT is a number.

When successful, returns RECIPE-ID of the newly added recipe. 

When not successful, raises an error.
"

  (db:with-connection-and-transaction
      (let ((recipe (make-instance 'db::recipe
                                   :name name
                                   :description description
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


(defun find-recipe-by-id (recipe-id)
  (db:with-connection 
    (pomo:get-dao 'db::recipe recipe-id)))


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


(defun newest-recipes (&optional (limit *default-limit*))
  "Returns a list of DB::RECIPEs"
  (db:with-connection
    (pomo:query-dao 'db::recipe
                    (format nil "select * from recipe order by id desc limit ~d" limit))))


(defun recipe-details (recipe-id)
  "
Returns a list of lists of form

 ((ingredient-name amount unit-name) ..)

If there is no recipe with RECIPE-ID, returns NIL.

"
  (db:with-connection
    (pomo:query
     (format nil "select ingredient_name, amount, unit_name from \
recipe_ingredient_unit_amount where recipe_id = ~d;" recipe-id))))
