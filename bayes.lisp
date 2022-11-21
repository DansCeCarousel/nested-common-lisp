;;;; bayes.lisp

(in-package #:bayes)

(defun find-index (point vector)
  "Find index of an element bigger than the given point"
  (position t (map 'vector
		   (lambda (a) (> a point)) vector)))
(defun generator (function n)
  "Get n samples drawn from a function from 0 to 1"
  (let ((temp (make-array n)))
    (iter (for i below n) (setf (aref temp i) (funcall function (random 1.0))))
    temp))
(defun sampler (n)
  "Get n samples drawn from 0 to 1"
  (generator 'identity n))
(defun gen-fun (function n)
  "Get n samples drawn from a function from 0 to 1"
  (generator function n))	
(defun lrps (lkhood trials lmin)
  "Return a point with likelihood grater than lmin"
  (let* ((proposed-points-loglkhood (gen-fun lkhood trials))
	(successful-points (iter (for i in-vector proposed-points-loglkhood)
			     (when (> i lmin)
			       (collect i into ss))
			     (finally (return ss)))))
    successful-points))     
(defun index-min (vector)
  (let ((index (position (reduce 'min vector) vector)))
    index))
(defun nested-sampling (lkhood niter nlive)
   "Sample nlive, live points. For niter, iterations,find and replace with higher likelihood point"
  (let* ((live-points-loglike (gen-fun lkhood nlive))
	(dead-points-loglike nil)
	(dead-points-volume nil)
	(volume-shell (log (- 1 (exp (/ -1 nlive))))))
    (dotimes (i niter)
      (let*   ((lowest (index-min live-points-loglike))
	      (lmin (aref live-points-loglike lowest)))
        (push volume-shell dead-points-volume)
	(decf volume-shell (/ 1 nlive))
	(push lmin dead-points-loglike)
	(let ((replacement (car (lrps lkhood nlive lmin))))
	  (if replacement
	      (setf (aref live-points-loglike lowest) replacement))))) ;;(print live-points-loglike)))
    (list dead-points-loglike dead-points-volume )))
	
	
(defun evidence-and-posterior-samples (function niter nlive)
  "Returns the evidence, P(data|I)"
  (let* ((q (nested-sampling function niter nlive))
	 (density (car q))
	 (width (mapcar 'exp (second q)))
	 (evidence (reduce '+ (mapcar (lambda (a b) (* a b))
				      density width)))
	 (posterior-samples (mapcar
			     (lambda (a b) (/ (* a b) evidence)) density width)))
    (rtl:with-out-file (posterior "posterior-syamples")
      (princ posterior-samples posterior))
    evidence))
    ;;(list evidence posterior-samples)))
	 
(defun avg (function)
  "Find the average of the evidence over many runs"
  (let ((h nil))
	   (dotimes (i 100)
	     (push (evidence-and-posterior-samples function 400 40) h))
	   (/ (reduce '+ h) 100)))
