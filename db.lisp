(in-package #:tsoha-db)


(defparameter *db-spec* '("tsoha" "tsoha" "tsohapassu" "localhost"))

(defmacro with-connection (&body body)
  `(pomo:with-connection *db-spec*
     ,@body))


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

(defclass ingredient ()
  ((id :reader id
       :col-type integer)
   (name :reader name
         :initarg :name
         :col-type string))
  (:metaclass pomo:dao-class)
  (:keys id))


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
   (ingredient-id :reader ingredient-id
                  :initarg :ingredient-id
                  :col-type integer)
   (unit-name :reader unit-name
              :initarg :unit-name
              :col-type string)
   (amount :reader amount
           :initarg :amount
           :col-type float))
   (:metaclass pomo:dao-class)
   (:keys recipe-id ingredient-id unit-name))
              
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
  ((ingredient-id :reader ingredient-id
                  :initarg :ingredient-id
                  :col-type integer)
   (nutrition-name :reader nutrition-name
                   :initarg :nutrition-name
                   :col-type string)
   (grams-per-100g :reader grams-per-100g
                   :initarg grams-per-100g))
  (:metaclass pomo:dao-class)
  (:keys ingredient-id nutrition-name))


               