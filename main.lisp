(in-package :tsoha-main)


;; keep this unbound when the server is not running
(defvar *server*)

(defparameter tbnl:*message-log-pathname* "/home/leadnose/hack/tsoha/error.log")
(defparameter tbnl:*access-log-pathname* "/home/leadnose/hack/tsoha/access.log")


(defparameter tbnl:*dispatch-table*
  (list

   ;; the front page
   (tbnl:create-regex-dispatcher
    "^/$" 'pages:front-page)

   ;; for miscallenous testing
   (tbnl:create-regex-dispatcher
    "^/test$"
    (lambda ()
      (let ((name (or (tbnl:get-parameter "name") "world")))
        (format nil "Hello, ~a" name))))))



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






