;;;; html-gen.lisp

(in-package #:am)

(defmacro with-gensyms ((&rest names) &body body)
  "The with-gensyms macro, derived from Practical Common Lisp."
  `(let ,(loop
	    for n in names
	    collect `(,n  (gensym (format nil "~a-" (string ',n)))))
     ,@body))

(defparameter *format-args* ())

(defun print-attribute-value (attr val stream)
  (if (or (stringp attr) (keywordp attr))
      (progn (format stream " ~a=\"" (string-downcase attr))
	     (if (or (stringp val) (keywordp val))
		 (format stream "~a\"" (string val))
		 (progn (format stream "~~a\"") ;; val is a symbol or a list
			(push val *format-args*))))
      (progn ;; attr is a symbol or a list
	(format stream "~~a=\"")
	(push attr *format-args*)
	(if (or (stringp val) (keywordp val))
	    (format stream "~a\"" (string val))
	    (progn (format stream "~~a\"") ;; val is a symbol or a list
		   (push val *format-args*))))))

(defun print-until-no-keywords (lst stream)
  "Print the attributes. Return the non-attributes."
  (if (not (keywordp (car lst)))
      lst
      (let ((attribute (car lst))
	    (value (cadr lst))
	    (rest (cddr lst)))
	(print-attribute-value attribute value stream)
	(print-until-no-keywords rest stream))))

(defmacro generate-html ((stream &key (indent nil) (indent-level "  ") (trim-whitespace t)
				 (clean-format-args t)) list-of-statements)
  (when (and indent (not (stringp indent))) (setf indent ""))
  (let* ((format-string
	  (with-output-to-string (format-string)
	    (macrolet ((indent-if () `(if indent (format nil "~%~a" indent) "")))
	      (labels ((increase-indent () (when indent (setf indent (format nil "~a~a" indent indent-level))))
		       (decrease-indent () (when indent (setf indent (subseq indent (length indent-level)))))
		       (gen (stmts)
			 (cond ((null stmts))

			       ((and (listp (car stmts)) (keywordp (car (car stmts))))
				(progn (gen (car stmts))
				       (gen (cdr stmts))))

			       ((keywordp (car stmts))
				(progn (format format-string "~a<~a"
					       (indent-if)
					       (string-downcase (car stmts)))
				       (let ((rest (print-until-no-keywords (cdr stmts) format-string)))
					 (format format-string ">")
					 (increase-indent)
					 (gen rest)
					 (decrease-indent)
					 (format format-string "~a</~a>"
						 (indent-if) (string-downcase (car stmts))))))


			       ((stringp (car stmts)) (progn (format format-string "~a~a"
								     (indent-if) (car stmts))
							     (gen (cdr stmts))))

			       (t (progn (format format-string "~a~~a" (indent-if))
					 (push (car stmts) *format-args*)
					 (gen (cdr stmts)))))))
		(gen list-of-statements))))))
    (when trim-whitespace (setf format-string (string-trim '(#\Newline #\Return #\Space) format-string)))
    (let ((args (reverse *format-args*)))
      (when clean-format-args (setf *format-args* ()))
      `(format ,stream ,format-string ,@args))))

(defmacro html (&rest args)
  (with-gensyms (stream)
    `(with-output-to-string (,stream)
       (generate-html (,stream) ,args))))

