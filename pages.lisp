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



(defun simple-page (content &optional (stream nil))
  "Writes a simple HTML-page to STREAM, or if STREAM is NIL, writes it
to a string and returns the string. The content is formatted using FORMAT-HTML."

  (let ((out (or stream (make-string-output-stream))))
    (format out "~&<html>~%~4t<body>~%~4t")
    (format-html out content)
    (format out "~&~4t</body>~%</html>~%")
    (unless stream
      (get-output-stream-string out))))

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
  (let ((recipe-count (queries:recipe-count)))
    (simple-page (body* :navigation-div
                        (format nil "Total ~d recipes." recipe-count)))))





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

    ;; let the errors fly
    (tbnl:redirect (format nil "/recipe/id/~d"
                           (queries:add-recipe :name name
                                               :instructions instructions
                                               :details details)))))




(defun recipe-by-id ()
  (db:with-connection 
    (simple-page
     (body* :navigation-div 
            (pomo:get-dao 'db::recipe (parse-integer (car (last (ppcre:all-matches-as-strings "[0-9]+" (tbnl:request-uri*))))))))))


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
