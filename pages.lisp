(in-package #:tsoha-pages)


(defgeneric format-html (stream object)
  (:method ((stream stream) obj)
    (format stream "~a"
            (tbnl:escape-for-html (format nil "~a" obj))))
  (:method ((stream stream) (list list))
    (loop for item in list do
         (format-html stream item)
         (write-char #\space stream))))



(defun simple-page (content &optional (stream nil))
  "Writes a simple HTML-page to STREAM, or if STREAM is NIL, writes it to
to a string and returns the string."

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
  (simple-page (list :navigation-div "<&foo>")))