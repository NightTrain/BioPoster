#|
(defvar lv-alpha .5)
(defvar lv-beta .5)
(defvar lv-gamma .5)
(defvar lv-delta .5)
|#
(defvar lv-alpha 1)
(defvar lv-beta .2)
(defvar lv-gamma .5)
(defvar lv-delta .04)
(defvar integral-epsilon (coerce (/ 1 100000) 'float))

(defun integral (f a b)
	(* (loop for i from a to b by integral-epsilon summing (funcall f i)) integral-epsilon)
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
			(new-prey (+ old-prey (dprey/dt old-prey old-pred)))
			(new-pred (+ old-pred (dprey/dt old-prey old-pred)))
		)
		(cons new-prey new-pred)
	)
)
