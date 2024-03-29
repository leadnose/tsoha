(in-package :tsoha-main)


(defvar *server*)


(defparameter tbnl:*message-log-pathname* "/home/leadnose/hack/tsoha/error.log")


(defparameter tbnl:*access-log-pathname* "/home/leadnose/hack/tsoha/access.log")


(defparameter tbnl:*show-lisp-errors-p* t)


(defparameter tbnl:*show-lisp-backtraces-p* t)


(defparameter tbnl:*dispatch-table*
  (list

   ;; stylesheet
   (tbnl:create-static-file-dispatcher-and-handler
    "/style.css" #P"/home/leadnose/hack/tsoha/style.css" "text/css")

   ;; the front page
   (tbnl:create-regex-dispatcher
    "^/$" 'pages:front-page)

   ;; add recipes
   (tbnl:create-regex-dispatcher
    "^/recipe/add$" 'pages:add-recipe)

   ;; receive the add
   (tbnl:create-regex-dispatcher
    "^/recipe/add/receive$" 'pages:receive-add-recipe)

   ;; recipe for id
   (tbnl:create-regex-dispatcher
    "^/recipe/id/[0-9]+$" 'pages:recipe-by-id)

   ;; search recipes
   (tbnl:create-regex-dispatcher
    "^/recipe/search$" 'pages:recipe-search)

   ;; show search results
   (tbnl:create-regex-dispatcher
    "^/recipe/search/results$" 'pages:recipe-search-results)

   ;; search ingredients
   (tbnl:create-regex-dispatcher
    "^/ingredient/search$" 'pages:ingredient-search)

   ;; show results for ingredient search
   (tbnl:create-regex-dispatcher
    "^/ingredient/search/results$" 'pages:ingredient-search-results)

   ;; ingredient by name
   (tbnl:create-regex-dispatcher
    "^/ingredient/name/.*" 'pages:ingredient-by-name)
    

   ;; show sources
   (tbnl:create-folder-dispatcher-and-handler "/sources/"
                                              "/home/leadnose/hack/tsoha/"
                                              "text")))




(defun start (&optional (port 8080))
  "Starts the application at given port and returns."

  (when (boundp '*server*)
    (cerror "do nothing" "Application already running.")
    (return-from start))

  (setf *server* (make-instance 'tbnl:acceptor :port port))
  (tbnl:start *server*))


(defun stop ()
  "Stops the application and returns."

  (when (not (boundp '*server*))
    (cerror "do nothing" "Application is not running.")
    (return-from stop))

  (tbnl:stop *server*)
  (makunbound '*server*))




(defun insert-from-file (pathspec)
  (with-open-file (file pathspec)
    (let ((rs (read file)))
      (loop for r in rs do
           (if (queries:search-recipe-by-name (getf r :name))
               (warn "Recipe with name ~s already exists." (getf r :name))
               (apply 'queries:add-recipe r))))))