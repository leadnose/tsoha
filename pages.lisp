(in-package #:tsoha-pages)

(defstruct body 
  (contents nil :type list))

(defun body (list)
  (make-body :contents list))


(defun body* (&rest rest)
  (make-body :contents rest))

(defgeneric format-html (stream object)
  (:method ((stream stream) obj)
    (format stream "~a"
            (tbnl:escape-for-html (format nil "~a" obj))))
  (:method ((stream stream) (body body))
    (loop for item in (body-contents body) do
         (format-html stream item)
         (write-char #\space stream))))


(defparameter *default-title* "Tsoha")

(defparameter *default-style-href* "/style.css")

(defun simple-page (content
                    &key
                    (title *default-title*)
                    (style-href *default-style-href*)))
                    
  (let ((content-str (with-output-to-string (out)
                       (format-html out content))))
    (with-output-to-string (out)
      (lml2:html-print
       `((:html)
         ((:head)
          ((:title) ,title)
          ((:link :rel "stylesheet" :type "text/css" :href ,style-href)))
         ((:body)
          ,content-str))
       out))))
   
      


(defparameter *nav-links*
  '(("/" "home")
    ("/recipe/search" "search recipes")
    ("/recipe/add" "add recipes")))

(defmethod format-html ((stream stream) (nd (eql :navigation-div)))
  (format stream "<div class=~s id=~s>~%" "navigation" "navigation")
  (loop for (url text) in *nav-links* do
       (format stream "<a href=~s>~a</a></br>" url text))
  (format stream "</div>"))


(defun front-page ()
  (let ((recipe-count (queries:recipe-count))
        (newest-recipes (queries:newest-recipes)))
    (simple-page (body* :navigation-div
                        (format nil "Total ~d recipes." recipe-count)
                        newest-recipes))))





(defparameter *ingredient-count* 20
  "Should be enough for everybody.")

(defmethod format-html ((stream stream) (arf (eql :add-recipe-form)))
  (flet ((unit-select (name)
           `((:select :name ,name :id ,name)
             ,@(loop for unit in (queries:unit-list) collect
                    `((:option) ,unit)))))

    (lml2:html-print

     `((:form :method :post :action "/recipe/add/receive")

       ((:label :for "name") "Name of the recipe: ")
       :br
       ((:input :type :text :size 30 :name "name" :id "name"))
       :br
       ((:label :for "instructions") "Instructions: ")
       :br
       ((:textarea :name "instructions" :id "instructions" :cols 80 :rows 25))

       ((:table)
        ((:tr) ((:td) "Ingredient") ((:td) "Amount") ((:td) "Unit"))
        ,@(loop for i from 0 below *ingredient-count* collect
               (let ((ingredient-n (format nil "ingredient~d" i))
                     (amount-n (format nil "amount~d" i))
                     (unit-n (format nil "unit~d" i)))
                 `((:tr)
                   ((:td)
                    ((:input :type :text :name ,ingredient-n :id ,ingredient-n)))
                   ((:td)
                    ((:input :type :text :size 10 :name ,amount-n :id ,amount-n)))
                   ((:td)
                    ,(unit-select unit-n))))))
       
       ((:input :type :submit :value "Add recipe")))

     stream)))




(defun add-recipe ()
  (simple-page (body* :navigation-div
                      :add-recipe-form)))

(defun empty-string-p (s)
  (string= "" (string-trim '(#\space #\newline #\tab) s)))



(defun get-details (&optional
                    (ingredients (loop for i from 0 below *ingredient-count*
                                    collect (tbnl:post-parameter (format nil "ingredient~d" i))))
                    (amounts (loop for i from 0 below *ingredient-count*
                                collect (tbnl:post-parameter (format nil "amount~d" i))))
                    (units (loop for i from 0 below *ingredient-count*
                              collect (tbnl:post-parameter (format nil "unit~d" i)))))

  (when (not (= (length ingredients)
                (length amounts)
                (length units)))
    (error "MULTIFAIL"))

  (loop
     for i in ingredients
     for a in amounts
     for u in units
     when (and (not (empty-string-p i))
               (not (empty-string-p u)))
     collect (list i (parse-integer a) u))) ;; TODO: handle parse-errors


(defun receive-add-recipe ()
  (let ((details (get-details))
        (name (tbnl:post-parameter "name"))
        (instructions (tbnl:post-parameter "instructions")))

    (when (not (and name instructions))
      (error "Faulty POST-parameters, need `name´ and `instructions´"))

    (tbnl:redirect (format nil "/recipe/id/~d"
                           (queries:add-recipe :name name
                                               :instructions instructions
                                               :details details)))))


(defun recipe-by-id ()
  (let ((recipe-id (parse-integer (car (last (ppcre:all-matches-as-strings "[0-9]+" (tbnl:request-uri*)))))))
    (db:with-connection 
      (simple-page
       (body* :navigation-div 
              (pomo:get-dao 'db::recipe recipe-id)
              (queries:recipe-details recipe-id))))))


(defmethod format-html ((stream stream) (srf (eql :search-recipe-form)))
  (lml2:html-print

   `((:form :method :get :action "/recipe/search/results")
     ((:label :for "name") "Name:")
     ((:input :type :text :id "name" :name "name"))
     ((:input :type :submit :value "search")))

   stream))


(defun recipe-search ()
  (simple-page
   (body* :navigation-div
          :search-recipe-form)))

(defun recipe-search-results ()
  (simple-page
   (body* :navigation-div
          (queries:search-recipe-by-name (tbnl:get-parameter "name")))))
