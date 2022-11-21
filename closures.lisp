(defmacro! cuter (starter function)
  `(let ((,g!w ,starter))
     (lambda (lst)
       (let* ((rs (mapcar ,function lst))
	      (cr (car rs)))
	 (incf ,g!w cr)))))
(defun scanner (password)
  (let* ((passwrd (coerce password 'list)) (current passwrd))
    (lambda (sentence)
      (let ((trial (coerce sentence 'list)))
	(dolist (i trial)
	  (if current
	      (setf current
		    (if (char= (car current) i)
			(cdr current)
			passwrd))))
	(not current)))))
