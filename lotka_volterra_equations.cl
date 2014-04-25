#|
(defvar lv-alpha .5)
(defvar lv-beta .5)
(defvar lv-gamma .5)
(defvar lv-delta .5)
|#
#|
(defvar lv-alpha 1)
(defvar lv-beta .2)
(defvar lv-gamma .5)
(defvar lv-delta .04)
|#
(defvar lv-alpha 1)
(defvar lv-beta (/ 1 5))
(defvar lv-gamma (/ 1 5))
(defvar lv-delta (/ 1 25))
(defvar integral-epsilon (coerce (/ 1 100000) 'float))

(defun integral (f a b)
	(loop for i from a to b by integral-epsilon summing (* (funcall f i) integral-epsilon))
)

(defun test-integral (num) 
	(mapcar (lambda (x) (integral (lambda (y) y) 0 x)) (loop for i from 1 to num collect i))
)

(defun dprey/dt (numprey numpred)
	(- (* lv-alpha numprey) (* lv-beta numprey numpred))
)

(defun dpred/dt (numprey numpred)
	(- (* lv-delta numprey numpred) (* lv-gamma numpred))
)

(defun update-prey/pred (pair timedelta)
	(let* (
			(old-prey (car pair))
			(old-pred (cdr pair))
			(new-prey (+ old-prey (* timedelta (dprey/dt old-prey old-pred))))
			(new-pred (+ old-pred (* timedelta (dpred/dt old-prey old-pred))))
		)
		(cons new-prey new-pred)
	)
)
(defun run-simulation (&key (initial-state (cons 5 2)) (stepval (/ 1.0 10)) (times 50))
	(do (
			(st initial-state (update-prey/pred st stepval))
			(acc nil (cons st acc))
			(i 0 (+ i 1))
		)
		((>= i times) (reverse acc))
	)
)
(defun output-gnuplot-data (&optional (states (run-simulation)))
	(let ((i 0))
		(dolist (x states)
			(format t "~a ~a ~a~%" (incf i) (car x) (cdr x))
		)
	)
)

#|
(let ((i 0))
	(mapcar
		(lambda (x)
			(format t "~a ~a ~a~%" (incf i) (car x) (cdr x))
		)
		(run-simulation :times 50)
	)
)
|#
