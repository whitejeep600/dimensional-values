(defmacro set_father (son father)
  `(setf (get ,son 'father) ,father))

(defmacro fau_init (new_symbol)
  `(progn
     (set_father ,new_symbol ,new_symbol)
     (setf (get ,new_symbol 'ratio) 1.0)))

(defun fau_find (s)
  (if (eq s (get s 'father))
      s
      (progn
	(let ((prev_ratio (get (get s 'father) 'ratio))
	      (to_father_ratio (get s 'ratio)))
	      (set_father s (fau_find (get s 'father)))
	      (setf (get s 'ratio) (* to_father_ratio prev_ratio)))
	(get s 'father))))

(defun fau_union (a b a_to_b_ratio)
  (let
      ((a_father (fau_find a))
       (b_father (fau_find b)))
    (set_father (fau_find a) (fau_find b))
    (setf (get a_father 'ratio) (* a_to_b_ratio (/ (get b 'ratio) (get a 'ratio))))))
