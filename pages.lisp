(in-package #:tsoha-pages)


(defstruct body
  (contents nil :type list))


(defun body (list)
  (make-body :contents list))


(defun body* (&rest rest)
  (make-body :contents rest))


(defgeneric format-html (stream object)
  (:method ((stream stream) obj)
    "Pretty-print and escape for html."
    (format stream "~a"
            (tbnl:escape-for-html (format nil "~a" obj))))
  (:method ((stream stream) (body body))
    "Call FORMAT-HTML on the contents of BODY, separating the items by space."
    (loop for item in (body-contents body) do
         (format-html stream item)
         (write-char #\space stream))))


(defparameter *default-title* "Tsoha")


(defparameter *default-style-href* "/style.css")


(defun simple-page (content
                    &key
                    (title *default-title*)
                    (header-1 title)
                    (style-href *default-style-href*))
  (let ((content-str (with-output-to-string (out)
                       (format-html out content))))
    (with-output-to-string (out)
      (lml2:html-print
       `((:html)
         ((:head)
          ((:title) ,title)
          ((:link :rel "stylesheet" :type "text/css" :href ,style-href)))
         ((:body)
          ((:h1) ,header-1)
          ((:div :class "content" :id "content")
           ,content-str)))
       out))))


(defparameter *nav-links*
  '(("/" "home")
    ("/recipe/search" "search recipes")
    ("/recipe/add" "add recipes")))


(defmethod format-html ((stream stream) (nd (eql :navigation-div)))
  (format stream "<div class=~s id=~s>~%" "navigation" "navigation")
  (loop for (url text) in *nav-links* do
       (format stream "<a href=~s>~a</a>  " url text))
  (format stream "</div>"))


(defclass recipe-listing ()
  ((recipes :initarg :recipes
            :reader recipes)))


(defun recipe-listing (recipe-list)
  (make-instance 'recipe-listing :recipes recipe-list))


(defmethod format-html ((stream stream) (rl recipe-listing))
  (lml2:html-print
   `((:div :class "recipe_listing" :id "recipe_listing")
     ((:table)
      ,@(loop for recipe in (recipes rl)
           collect
             `((:tr)
               ((:td)
                ((:a :href
                     ,(format nil "/recipe/id/~d" (db::id recipe)))
                 ,(db::name recipe)))))))
   stream))



(defun front-page ()
  (let ((recipe-count (queries:recipe-count))
        (newest-recipes (queries:newest-recipes)))
    (simple-page (body* :navigation-div
                        (format nil "Total ~d recipes." recipe-count)
                        (recipe-listing newest-recipes)))))


(defparameter *ingredient-count* 20
  "Should be enough for everybody.")


(defun unit-select (name) ;; this is ugly :(
  `((:select :name ,name :id ,name)
    ,@(loop for unit in (queries:unit-list) collect
           `((:option) ,unit))))


(defmethod format-html ((stream stream) (arf (eql :add-recipe-form)))
  (lml2:html-print
   ` ((:div :class "form" :id "recipe_add_form")
      ((:h2) "Add a new recipe")
      ((:form :method :post :action "/recipe/add/receive")
       ((:label :for "name") "Name: ")
       :br
       ((:input :type :text :size 30 :name "name" :id "name"))
       :br
       ((:label :for "description") "Description: ")
       :br
       ((:textarea :name "description" :id "description" :cols 80 :rows 5))
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
       ((:input :type :submit :value "Add recipe"))))
     stream))


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


(defun strip-crlfs (string)
  "
Returns a new string in which all the
     #\return \#newline
have been replaced with just a #\newline.
"
  (with-output-to-string (out)
    (loop for char across string do
         (unless (char= #\return char)
           (write-char char out)))))


(defun receive-add-recipe ()
  (let ((details (get-details))
        (name (tbnl:post-parameter "name"))
        (description (tbnl:post-parameter "description"))
        (instructions (strip-crlfs (tbnl:post-parameter "instructions"))))

    (when (not (and name instructions))
      (error "Faulty POST-parameters, need `name´ and `instructions´"))

    (tbnl:redirect (format nil "/recipe/id/~d"
                           (queries:add-recipe :name name
                                               :instructions instructions
                                               :description description
                                               :details details)))))


(defmethod format-html ((stream stream) (recipe db::recipe))
  (with-slots ((name db::name) (instructions db::instructions) (id db::id)) recipe
    (let ((details (queries:recipe-details id)))
      (lml2:html-print
       `((:div :class "recipe" :id ,id)
         ((:h2) ,name)
         ((:h3) "Ingredients")
         ((:table)
          ,@(loop for (ing-name amount unit-name) in details
               collect `((:tr)
                         ((:td) ,ing-name)
                         ((:td) ,amount)
                         ((:td) ,unit-name))))
         ((:h3) "Instructions")
         ,(with-output-to-string (out)
                                 (loop for char across (tbnl:escape-for-html instructions) do
                                      (if (char= #\newline char)
                                          (format out "<br>")
                                          (write-char char out)))))
       stream))))


(defun recipe-by-id ()
  (let* ((recipe-id (parse-integer (car (last (ppcre:all-matches-as-strings "[0-9]+" (tbnl:request-uri*))))))
         (recipe (queries:find-recipe-by-id recipe-id)))
    (if (not recipe)
        (setf (tbnl:return-code*) 404) ;; this causes the server to respond with a 404
        (simple-page
         (body* :navigation-div
                recipe)))))


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
          (recipe-listing (queries:search-recipe-by-name (tbnl:get-parameter "name"))))))
