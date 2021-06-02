
; two types of entities will be defined in the library.
; the first will be a unit, representing a an elementary unit like: meter, second, kilogram.
; the second will be what the library is actually about: a dimensional value. let us note that
; every sensible dimensinal value is composed of a number, some units multiplied together, and
; possibly a division bar with some more units multiplied together below it. so in practice we
; can have a class with three attributes: the number, the list of units above the division bar
; and the list of units below it. thus 420 m/s^2 would have 420 as the number, m as the first
; list and (s s) as the second list. in some cases one (or both) the lists can be empty,
; for example for 1Hz = 1/s, the first list will be empty.


(defpackage :measures
(:nicknames :miary)
(:use :common-lisp)
(:export :distance :meter :kilometer :mile :time :second :hour :minute :gram :kilogram :ounce :dim :mul :div :is_number :dimv :define-measure :define-rule :define-group :plus :minus :dim))


(in-package measures)

;;; we will use find-and-union to remember which units can be normalized to each other.
;;; so here are some auxiliary functions for that purpose
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
    (set_father a_father b_father)
    (setf (get a_father 'ratio) (* a_to_b_ratio (/ (get b 'ratio) (get a 'ratio))))))


(defun unit_defined (unit)
  (get unit 'unit_defined))


(defun same_union (unit_a unit_b)
  (if (or (eq '/ unit_a) (eq '/ unit_b))
      '()
      (eq (fau_find unit_a) (fau_find unit_b))))


(defmacro define-group (group_name)
  `(if (get ,group_name 'group_defined)
      (write "group already exists. failed")
      (setf (get ,group_name 'group_defined) T)))


(defmacro define-measure (unit group)
  `(if (null (get ,group 'group_defined))
       (write "group does not exist. failed")
       (if (unit_defined ,unit)
	   (write "unit already exists. failed")
	   (progn
	     (fau_init ,unit)
	     (setf (get ,unit 'unit_defined) T)
	     (setf (get ,unit 'unit_group) ,group)))))


(defmacro define-rule (from_unit to_unit multiplier)
  `(if (or (not (unit_defined ,from_unit)) (not (unit_defined ,to_unit)))
      (write "at least one of these measures does not exist. failed")
      (if (same_union ,from_unit ,to_unit)
	  (write "the relationship between the units can already be deduced based on other relationships. failed")
	  (fau_union ,from_unit ,to_unit (/ 1 ,multiplier)))))


(defstruct dimv
  number
  numerator
  denominator)


; these three are auxiliary functions for the dim function.
(defun get_x (arg)
  (assert (numberp arg))
  arg)


(defun get_num (args)
  (if (or (eq '/ (first args)) (null (first args)))
      '()
      (progn
	(assert (unit_defined (first args)))
	(concatenate 'list (list (first args)) (get_num (rest args))))))


(defun get_deno (args)
  (cond
    ((null args)
     '())
    ((eq '/ (first args))
     (rest args))
    (t
     (get_deno (rest args)))))


;;; for parsing a dimensional value from a number and list of units (possibly with a division bar)
(defun dim (number args)
  (make-dimv :number (get_x number)
	    :numerator (get_num args)
	    :denominator (get_deno args)))


;;; auxiliary for the function below.
(defun get_same_type_after_bar (list_arg unit)
  (if (null (first list_arg))
      '(nil nil)
      (if (same_union (first list_arg) unit)
	      (list (first list_arg) nil)
	      (get_same_type_after_bar (rest list_arg) unit))))


;;; returns a list whose first element is a unit contained in list_arg which has the same type as the given unit,
;;; (nil if there is no such unit), and the second element is T iff if no '/' was found between the found unit and
;;; the given unit.
(defun get_same_type (list_arg unit)
  (if (null list_arg)
      '(nil T)
      (if (eq (first list_arg) '/)
	  (get_same_type_after_bar (rest list_arg) unit)
	  (if (same_union (first list_arg) unit)
	      (list (first list_arg) T)
	      (get_same_type (rest list_arg) unit)))))


(defun get_ratio (unit_a unit_b)
  (/ (get unit_a 'ratio) (get unit_b 'ratio)))


(defun get_list_without (list_arg what)
  (if (null list_arg)
      '()
      (if (eq (first list_arg) what)
	  (rest list_arg)
	  (concatenate 'list (list (first list_arg)) (get_list_without (rest list_arg) what)))))

;;; for simplifying units, for example if there are units of the same type in the numerator and denominator.
;;; this function actually gives a pretty messy result - a list of units and numbers together. for example,
;;; when simplifying a division of kilometer by meter, it might leave 1000 meters in the numerator and 'meter'
;;; in the denominator. further functions extract information from the resul of this one.
(defun simplify_list (arg)
  (if (null arg)
      '()
      (if (eq (first arg) '/)
	  (concatenate 'list '(/) (simplify_list (rest arg)))
	  (let
	      ((same_type (get_same_type (rest arg) (first arg))))
	    (if (null (first same_type))
		(concatenate 'list (list (first arg)) (simplify_list (rest arg)))
		(if (and (eq (first arg) (first same_type)) (not (second same_type)))
		    (simplify_list (get_list_without (rest arg) (first same_type)))
		    (if (null (second same_type))
			(concatenate 'list (simplify_list (get_list_without arg (first same_type)))
				     (list (get_ratio (first same_type) (first arg)))
				     (list (first arg)))
			(concatenate 'list (list (get_ratio (first same_type) (first arg)))
				     (list (first arg))
				     (simplify_list (get_list_without arg (first same_type)))))))))))


;;; a list representation of a dimenstional value: the list of units in the numerator followed by '/' and
;;; the list of units from the denominator.
(defun get_list_representation (dimval)
  (concatenate 'list (dimv-numerator dimval) '(/) (dimv-denominator dimval)))


(defun get_multiplier_after_bar (list_arg num)
  (if (null list_arg)
      num
      (if (numberp (first list_arg))
	  (get_multiplier_after_bar (rest list_arg) (/ num (first list_arg)))
	  (get_multiplier_after_bar (rest list_arg) num))))


(defun get_multiplier_before_bar (list_arg num)
  (if (eq '/ (first list_arg))
      (get_multiplier_after_bar list_arg num)
      (if (numberp (first list_arg))
	  (get_multiplier_before_bar (rest list_arg) (* num (first list_arg)))
	  (get_multiplier_before_bar (rest list_arg) num))))


;;; takes a list which represents a dimensional value and returns all the numbers left
;;; in the numerator by the simplify_list function multiplied together and divided by
;;; those left in the denominator multiplied together.
(defun get_multiplier (list_arg)
  (get_multiplier_before_bar list_arg 1))


(defun get_units (list_arg)
  (cond
    ((null list_arg) '())
    ((numberp (first list_arg)) (get_units (rest list_arg)))
    (t (concatenate 'list (list (first list_arg)) (get_units (rest list_arg))))))


;;; returns a list of the units left in the denominator by the simplify_list function.
(defun get_units_after_bar (list_arg)
  (if (eq (first list_arg) '/)
      (get_units (rest list_arg))
      (get_units_after_bar (rest list_arg))))


;;; similar, but for the numerator.
(defun get_units_before_bar (list_arg)
  (cond ((eq (first list_arg) '/) '())
	((numberp (first list_arg)) (get_units_before_bar (rest list_arg)))
	(t (concatenate 'list (list (first list_arg)) (get_units_before_bar (rest list_arg))))))


;;; auxiliary for the simplify function.
(defun simplify_aux (dimval)
  (let ((simplified_list (simplify_list (get_list_representation dimval))))
    (make-dimv :number (* (dimv-number dimval) (get_multiplier simplified_list))
	       :numerator (get_units_before_bar simplified_list)
	       :denominator (get_units_after_bar simplified_list))))


(defun lists_the_same (l1 l2)
  (if (and (null l1) (null l2))
      T
      (if (or (null l1) (null l2))
	  nil
	  (if (not (eq (first l1) (first l2)))
	      nil
	      (lists_the_same (rest l1) (rest l2))))))


(defun dimvals_the_same (dimval1 dimval2)
  (and (eq (dimv-number dimval1) (dimv-number dimval2))
       (lists_the_same (dimv-numerator dimval1) (dimv-numerator dimval2))
       (lists_the_same (dimv-denominator dimval1) (dimv-denominator dimval2))))


;;; simplify while you can.
;;; admittedly, this function is a design failure. I wasn't able to get the simplify_list
;;; function to do its job completely, at least not before the deadline.
(defun simplify (dimval)
  (let ((simplified (simplify_aux dimval)))
    (if (dimvals_the_same simplified dimval)
	dimval
	(simplify simplified))))


(defgeneric mul (arg1 arg2))


(defmethod mul ((arg1 dimv) (arg2 dimv))
  (simplify (make-dimv :number (* (dimv-number arg1) (dimv-number arg2))
		       :numerator (concatenate 'list (dimv-numerator arg1) (dimv-numerator arg2))
		       :denominator (concatenate 'list (dimv-denominator arg1) (dimv-denominator arg2)))))


(defmethod mul ((arg1 dimv) (arg2 number))
  (simplify (make-dimv :number (* (dimv-number arg1) arg2)
		       :numerator (dimv-numerator arg1)
		       :denominator (dimv-denominator arg1))))


(defmethod mul ((arg1 number) (arg2 dimv))
  (simplify (make-dimv :number (* (dimv-number arg2) arg1)
		       :numerator (dimv-numerator arg2)
		       :denominator (dimv-denominator arg2))))


(defun is_number (dimval)
  (let ((simplified (simplify dimval)))
    (and (null (dimv-numerator simplified)) (null (dimv-denominator simplified)))))


; auxiliart for generic dividing
(defgeneric inverse (arg))


(defmethod inverse ((arg dimv))
  (make-dimv :number (/ 1 (dimv-number arg))
	     :numerator (dimv-denominator arg)
	     :denominator (dimv-numerator arg)))


(defmethod inverse ((arg number))
  (/ 1 arg))


(defun div (dimval1 dimval2)
  (mul dimval1 (inverse dimval2)))


(defun get_negative (dimval)
  (make-dimv :number (- (dimv-number dimval))
	     :numerator (dimv-numerator dimval)
	     :denominator (dimv-denominator dimval)))


(defun get_val_with_added_number (dimval to_add)
  (make-dimv :number (+ to_add (dimv-number dimval))
	     :numerator (dimv-numerator dimval)
	     :denominator (dimv-denominator dimval)))


;;; returns a value which has a unit reverse to that of dimval and the number equal to 1.
;;; this is used to reconcile values of different (but compatible) units while adding.
(defun get_rev_unit (dimval)
  (make-dimv :number 1
	     :numerator (dimv-denominator dimval)
	     :denominator (dimv-numerator dimval)))


(defun plus (dimval1 dimval2)
  (if (not (is_number (div dimval1 dimval2)))
      (write "cannot add the values due to a disparity of units")
      (get_val_with_added_number dimval1 (dimv-number (mul dimval2 (get_rev_unit dimval1))))))


(defun minus (dimval1 dimval2)
  (plus dimval1 (get_negative dimval2)))


(define-group 'distance)
(define-measure 'meter 'distance)
(define-measure 'kilometer 'distance)
(define-measure 'mile 'distance)
(define-rule 'kilometer 'meter (/ 1 1000))
(define-rule 'mile 'meter (/ 1 1609))

(define-group 'time)
(define-measure 'second 'time)
(define-measure 'minute 'time)
(define-measure 'hour 'time)
(define-rule 'hour 'minute (/ 1 60))
(define-rule 'minute 'second (/ 1 60))

(define-group 'weight)
(define-measure 'kilogram 'weight)
(define-measure 'ounce 'weight)
(define-measure 'gram 'weight)
(define-rule 'kilogram 'gram (/ 1 1000))
(define-rule 'gram 'ounce 28)

;(defparameter a (dim 420 '(meter / second)))
;(defparameter b (dim 1 '(second)))
;(defparameter c (mul a b))
;(defparameter d (dim 0.1 '(/ second second)))
;(defparameter e (dim 6 '(gram)))
;(defparameter f (mul c (mul d e))) ; now f evaluates to 252.00002 (meters * grams) / second^2 (a quarter of a newton)
;(defparameter g (div 3 f)) ; now g is 0.012 seconds^2 / (grams * meters)
;(defparameter h (dim 10 '(kilometer / hour)))
;(defparameter i (minus a h)) ; now i is about 417 meter / second
