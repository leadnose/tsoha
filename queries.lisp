(in-package #:tsoha-queries)


(defparameter *default-limit* 25)


(defun add-recipe (&key
                   name
                   description
                   instructions
                   details)
  "
NAME and INSTRUCTIONS are supposed to be strings,
DETAILS is a list of form

   ((INGREDIENT-NAME AMOUNT UNIT) ...)

where INGREDIENT and UNIT are strings and AMOUNT is a number.

When successful, returns RECIPE-ID of the newly added recipe. 

When not successful, raises an error.
"
  (db:with-connection
    (let ((recipe (make-instance 'db::recipe
                                 :name name
                                 :description description
                                 :instructions instructions)))
      (pomo:insert-dao recipe)
      (loop for (ing-name amount unit) in details collect
           (let ((ing (make-instance 'db::ingredient
                                     :name ing-name))
                 (un (make-instance 'db::unit
                                    :name unit)))
             ;; these will raise an error if they already exist,
             ;; but that's okay because what we want is that they exist
             (ignore-errors 
               (pomo:insert-dao ing))
             (ignore-errors
               (pomo:insert-dao un))
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
  "Returns a list of DB::RECIPEs whose name is sql-like NAME."
  (db:with-connection 
    (pomo:query-dao 'db::recipe
                    (format nil "select * from recipe where lower(name) like lower(~a);"
                            (pomo:sql-escape-string (format nil "%~a%" name))))))


(defun search-recipe-by-ingredient (ingredient-name)
  "Returns a list of DB::RECIPEs whose details include an ingredient
 whose name is sql-like INGREDIENT-NAME."
  (db:with-connection
    (loop for x in (pomo:query 
                    (format nil "select recipe_id from recipe_ingredient_unit_amount \
                                  where lower(ingredient_name) like lower(~a);"
                            (pomo:sql-escape-string (format nil "%~a%" ingredient-name))))
       collect (find-recipe-by-id (car x)))))



(defun search-recipe-by-description (description)
  (db:with-connection
    (pomo:query-dao 'db::recipe
                    (format nil "select * from recipe where lower(description) like lower(~a);"
                            (pomo:sql-escape-string (format nil "%~a%" description))))))


(defun search-recipe-by-instructions (string)
  (db:with-connection
    (pomo:query-dao 'db::recipe
                    (format nil "select * from recipe where lower(instructions) like lower(~a);"
                            (pomo:sql-escape-string (format nil "%~a%" string))))))


(defun search-recipe-generic (query &key name description ingredients instructions)
  "The &key arguments are booleans which indicate which fields to search for."
  (if (not (or name description ingredients instructions))
      (error "You should supply at least one of NAME, DESCRIPTION, INGREDIENT, INSTRUCTIONS to get any results.")
      (remove-duplicates (append (when name (search-recipe-by-name query))
                                 (when ingredients (search-recipe-by-ingredient query))
                                 (when description (search-recipe-by-description query))
                                 (when instructions (search-recipe-by-instructions query)))
                         :test #'=
                         :key #'db::id)))


(let ((units (list "grams" "dl" "litres" "ounces" "teaspoon" "kpl" "kilograms")))
  (db:with-connection
    (loop for unit in units do
         (handler-case
             (pomo:insert-dao (make-instance 'db::unit :name unit))
           (cl-postgres-error:unique-violation (e)
             (declare (ignorable e))
             (warn "Unit already exists: ~a" unit)))))
  ;; no point in always going to database to get a list of units
  (defun unit-list ()
    units))


(defun recipe-count ()
  (db:with-connection 
    (caar (pomo:query "select count(*) from recipe;"))))


(defun newest-recipes (&key (limit *default-limit*))
  "Returns a list of DB::RECIPEs"
  (db:with-connection
    (pomo:query-dao 'db::recipe
                    (format nil "select * from recipe order by id desc limit ~d" limit))))


(defun recipe-details (recipe-id)
  "
Returns a list of lists of form

 ((ingredient-name amount unit-name) ..)

If there is no recipe with RECIPE-ID, or it has no ingredients,
returns NIL.
"
  (db:with-connection
    (pomo:query
     (format nil "select ingredient_name, amount, unit_name from \
recipe_ingredient_unit_amount where recipe_id = ~d;" recipe-id))))


(defun find-ingredient (name)
  "Returns either a DB::INGREDIENT or NIL."
  (db:with-connection
    (pomo:get-dao 'db::ingredient 
                  name)))


(defun search-ingredient-by-name (name)
  "Returns a list of DB::INGREDIENTs."
  (db:with-connection
    (pomo:query-dao 'db::ingredient
                    (format nil "select * from ingredient where lower(name) like lower(~a);"
                            (pomo:sql-escape-string (format nil "%~a%" name))))))

