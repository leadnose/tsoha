(in-package #:tsoha-db)

;;  create user tsoha_user
;;  alter USER tsoha_user with password 'tsoha_password';
;;  create DATABASE tsoha_db OWNER tsoha_user;


(defparameter *db-spec* '("tsoha_db" "tsoha_user" "tsoha_password" "localhost"))

(defmacro with-new-connection (&body body)
  "Establishes a new database connection for the duration of BODY, and
closes the connection after the BODY exits either normally or
abnormally."
  `(pomo:with-connection *db-spec*
     ,@body))



(defmacro with-connection (&body body)
  "Like WITH-NEW-CONNECTION, but if connection is already established,
doesn't re-establish it." ;;; FIXME
  `(with-new-connection
         ,@body))
       

(defmacro with-transaction (&body body)
  "Establishs a transaction for the duration of the BODY. If body exits
  normally, commit the transaction, if BODY raises an exception, rolls
  the transaction back.

Does NOT establish a connection."
  `(pomo:with-transaction ()
     ,@body))


(defmacro with-connection-and-transaction (&body body)
  "A simple convenience macro that establishes both a connection and a
transaction and handles the commit/rollback and disconnecting."
  `(with-connection
     (with-transaction
       ,@body)))

  
;;;; These classes try to map to the tables as defined in create_tables.sql

(defclass recipe ()
  ((id :reader id
       :col-type integer)
   (name :reader name
         :initarg :name
         :col-type string)
   (instructions :reader instructions
                 :initarg :instructions
                 :col-type string))
  (:metaclass pomo:dao-class)
  (:keys id))


(defmethod recipe-details (recipe-id)
  (db:with-connection
    (pomo:query-dao 'db::recipe-ingredient-unit-amount
                    (format nil "select * from recipe_ingredient_unit_amount where recipe_id = ~d;" recipe-id))))



;(loop for 'describe (recipe-details 1))


(defmethod print-object ((recipe recipe) stream)
  (format stream
          "Name: ~a, instructions: ~a"
          (name recipe)
          (instructions recipe)))
  


(defclass ingredient ()
  ((name :reader name
         :initarg :name
         :col-type string))
  (:metaclass pomo:dao-class)
  (:keys name))


(defclass unit ()
  ((name :reader name
         :initarg :name
         :col-type string))
  (:metaclass pomo:dao-class)
  (:keys name))


(defclass recipe-ingredient-unit-amount ()
  ((recipe-id :reader recipe-id
              :initarg :recipe-id
              :col-type integer)
   (ingredient-name :reader ingredient-name
                  :initarg :ingredient-name
                  :col-type integer)
   (unit-name :reader unit-name
              :initarg :unit-name
              :col-type string)
   (amount :reader amount
           :initarg :amount
           :col-type float))
   (:metaclass pomo:dao-class)
   (:keys recipe-id ingredient-name unit-name))
              
(defclass nutrition ()
  ((name :reader name
         :initarg :name
         :col-type string)
   (calories-per-100g :reader calories-per-100g
                      :initarg :calories-per-100g
                      :col-type float))
   (:metaclass pomo:dao-class)
   (:keys name))


(defclass ingredient-nutrition ()
  ((ingredient-name :reader ingredient-name
                  :initarg :ingredient-name
                  :col-type string)
   (nutrition-name :reader nutrition-name
                   :initarg :nutrition-name
                   :col-type string)
   (grams-per-100g :reader grams-per-100g
                   :initarg grams-per-100g))
  (:metaclass pomo:dao-class)
  (:keys ingredient-name nutrition-name))


               