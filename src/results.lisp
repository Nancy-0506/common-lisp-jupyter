(in-package #:jupyter-kernel)

#|

Standard MIME types

|#

(defvar *gif-mime-type* "image/gif")
(defvar *html-mime-type* "text/html")
(defvar *javascript-mime-type* "application/javascript")
(defvar *jpeg-mime-type* "image/jpeg")
(defvar *json-mime-type* "application/json")
(defvar *latex-mime-type* "text/latex")
(defvar *markdown-mime-type* "text/markdown")
(defvar *maxima-mime-type* "text/x-maxima")
(defvar *pdf-mime-type* "application/pdf")
(defvar *ps-mime-type* "application/postscript")
(defvar *plain-text-mime-type* "text/plain")
(defvar *png-mime-type* "image/png")
(defvar *svg-mime-type* "image/svg+xml")


(defun keyword-result-p (code)
  (and (listp code)
       (listp (car code))
       (keywordp (caar code))))

(defun lisp-result-p (code)
  (and (listp code)
       (listp (car code))
       (eq (caar code) ':lisp)))

; (defun mlabel-result-p (code)
;   (and (listp code)
;        (listp (car code))
;        (eq (caar code) 'maxima::displayinput)))

; (defun displayinput-result-p (code)
;   (and (listp code)
;        (listp (car code))
;        (eq (caar code) 'maxima::displayinput)))
;
; (defun mlabel-input-result-p (code)
;   (and (listp code)
;        (listp (car code))
;        (eq (caar code) 'maxima::mlabel)
;        (starts-with-p (string (second code)) (string maxima::$inchar))))

; (defun mtext-result-p (code)
;   (and (listp code)
;          (listp (car code))
;          (eq (caar code) 'maxima::mtext)))
;
; (defun plot-p (value)
;   (and (listp value)
;        (eq (caar value) 'maxima::mlist)
;        (eq (list-length value) 3)
;        (stringp (second value))
;        (stringp (third value))
;        (or (ends-with-p (second value) ".gnuplot")
;            (ends-with-p (second value) ".gnuplot_pipes"))))

(defun sexpr-to-text (value)
  (format nil "~S" value))

; (defun mexpr-to-text (value)
;   (string-trim '(#\Newline)
;                (with-output-to-string (*standard-output*)
;                  (let ((maxima::*alt-display1d* nil)
;                        (maxima::*alt-display2d* nil))
;                    (maxima::displa value)))))

; (defun mexpr-to-latex (value)
;   (let ((env (maxima::get-tex-environment value)))
;     (apply #'concatenate 'string
;            (mapcar #'string
;                    (maxima::tex value
;                                 (list (car env)) (list (cdr env))
;                                 'maxima::mparen 'maxima::mparen)))))

; (defun mexpr-to-maxima (value)
;   (let ((maxima::*display-labels-p* nil))
;     (with-output-to-string (f)
;       (maxima::mgrind value f))))

(defgeneric render (results)
  (:documentation "Render results."))

(defmethod render (res))

(defclass result ()
  ((display :initarg :display
            :initform nil
            :accessor result-display)))

(defclass sexpr-result (result)
  ((value :initarg :value
          :reader sexpr-result-value)))

(defmethod render ((res sexpr-result))
  (jsown:new-js
    (*plain-text-mime-type* (sexpr-to-text (sexpr-result-value res)))))

; (defclass mexpr-result (result)
;   ((value :initarg :value
;           :reader mexpr-result-value)))
;
; (defmethod render ((res mexpr-result))
;   (let ((value (mexpr-result-value res)))
;     (if (mlabel-input-result-p value)
;       (jsown:new-js
;         (*plain-text-mime-type* (mexpr-to-text value))
;         (*maxima-mime-type* (mexpr-to-maxima value)))
;       (jsown:new-js
;         (*plain-text-mime-type* (mexpr-to-text value))
;         (*latex-mime-type* (mexpr-to-latex value))
;         (*maxima-mime-type* (mexpr-to-maxima value))))))

(defclass inline-result (result)
  ((value :initarg :value
          :reader inline-result-value)
   (mime-type :initarg :mime-type
              :reader inline-result-mime-type)))

(defun make-inline-result (value &key (mime-type *plain-text-mime-type*) (display nil) (handle nil))
  (let ((result (make-instance 'inline-result :value value
                                              :mime-type mime-type
                                              :display display)))
    (if (and handle display)
      (progn
        (send-result result)
        t)
      result)))

(defmethod render ((res inline-result))
  (let ((value (inline-result-value res))
        (mime-type (inline-result-mime-type res)))
    (if (equal mime-type *plain-text-mime-type*)
      (jsown:new-js
        (mime-type value))
      (jsown:new-js
        (*plain-text-mime-type* "inline-value")
        (mime-type (if (stringp value)
                       value
                       (cl-base64:usb8-array-to-base64-string value)))))))

(defclass file-result (result)
  ((path :initarg :path
         :reader file-result-path)
   (mime-type :initarg :mime-type
              :initform nil
              :reader file-result-mime-type)))

(defun make-file-result (path &key (mime-type nil) (display nil) (handle nil))
  (let ((result (make-instance 'file-result :path path
                                            :mime-type mime-type
                                            :display display)))
    (if (and handle display)
      (progn
        (send-result result)
        t)
      result)))

(defmethod render ((res file-result))
  (let* ((path (file-result-path res))
         (mime-type (or (file-result-mime-type res) (trivial-mimes:mime path))))
    (if (equal mime-type *plain-text-mime-type*)
      (jsown:new-js
        (mime-type (read-string-file path)))
      (jsown:new-js
        (*plain-text-mime-type* path)
        (mime-type
          (if (or (equal mime-type *svg-mime-type*) (starts-with-p mime-type "text/"))
            (read-string-file path)
            (file-to-base64-string path)))))))

(defclass error-result (result)
  ((ename :initarg :ename
          :reader error-result-ename)
   (evalue :initarg :evalue
           :reader error-result-evalue)
   (quit :initarg :quit
         :initform nil
         :reader error-result-quit)
   (traceback :initarg :traceback
              :initform nil
              :reader error-result-traceback)))

(defun make-error-result (ename evalue &key (quit nil) (traceback nil))
  (make-instance 'error-result :ename ename
                               :evalue evalue
                               :quit quit
                               :traceback traceback))

; (defun make-maxima-result (value &key (display nil) (handle nil))
;   (let ((result (cond ((typep value 'result)
;                         value)
;                       ((eq value 'maxima::maxima-error)
;                         (make-error-result "maxima-error" (second maxima::$error)))
;                       ((lisp-result-p value)
;                         (make-lisp-result (second value)))
;                       ((and (mlabel-result-p value) (typep (third value) 'result))
;                         (third value))
;                       (t
;                         (make-instance 'mexpr-result :value value :display display)))))
;     (if (and handle display)
;       (progn
;         (send-result result)
;         t)
;       result)))

(defun make-lisp-result (value &key (display nil))
  (cond ((typep value 'result)
         value)
        ((not (eq 'no-output value))
         (make-instance 'sexpr-result :value value :display display))))

#|

Jupyter clients generally don't know about the myriad of mime types associated
with TeX/LaTeX and assume that the proper mime type is always text/latex. The
following function will make sure that trivial-mimes database reflects this.

|#

(defun check-mime-db ()
  (iter
    (for ext in '("tex" "latex" "tikz"))
    (setf (gethash ext trivial-mimes:*mime-db*) *latex-mime-type*)))

(check-mime-db)
