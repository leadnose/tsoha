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
  (simple-page (list :navigation-div "nothing here yet")))


(defun unit-list ()
  "this should be made to sync with the db"

  '("grams" "dl" "litres" "ounces" "teaspoon" "kpl"))


(defparameter *ingredient-count* 20
  "Should be enough for everybody.")

(defmethod format-html ((stream stream) (arf (eql :add-recipe-form)))
  (flet ((unit-select (name)
           `((:select :name ,name :id ,name)
             ,@(loop for unit in *unit-list* collect
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
    (error "The lists must be of same length or this ain't gonna work."))

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

    (when (not (and name instructions details))
      (return-from receive-add-recipe (simple-page "FAIL because faulty POST")))

    ;; this is basically the same as try-catch in java/c++/etc
    (handler-case  

        ;; TRY add the recipe, if it's successful (returns normally), then do a
        ;; redirect to it's own page
        (tbnl:redirect (format nil "/recipe/id/~d"
                               (queries:add-recipe :name name
                                                   :instructions instructions
                                                   :details details)))

      ;; CATCH any errors (t is a type that is somewhat like object in java, matches anything)
      (t (err)
        (declare (ignore err))
        (simple-page "FAIL because database")))))


    

