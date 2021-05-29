; todo:
; dodawanie dwoch wielkosci
; dodawanie liczby do wielkosci, jesli wielkosc jest liczba
; mnozenie i dzielenie wielkosci z liczbami
; overloadowanie operatorow dla powyzszych operacji
; package
; komentarze

; two types of entities will be defined in the library.
; the first will be a unit, representing a an elementary unit like: meter, second, kilogram.
; the second will be what the library is actually about: a dimensional value. let us note that
; every sensible dimensinal value is composed of a number, some units multiplied together, and
; possibly a division bar with some more units multiplied together below it. so in practice we
; can have a class with three attributes: the number, the list of units above the division bar
; and the list of units below it. thus 420 m/s^2 would have 420 as the number, m as the first
; list and (s s) as the second list. in some cases one (or both) the lists can be empty,
; for example for 1Hz = 1/s, the first list will be empty.



; (defparameter a (dim 20 '(meter / second)))

;(defpackage :measures
;  (:nicknames :miary)
;  (:use :common-lisp)
;  (:export :distance :meter :kilometer :mile :time :second :hour :minute :c))

;(in-package measures)


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


(defmacro define-group (group_name)
  `(if (get ,group_name 'group_defined)
      (write "group already exists. failed")
      (setf (get ,group_name 'group_defined) T)))


(defun unit_defined (unit)
  (get unit 'unit_defined))


(defmacro define-measure (unit group)
  `(if (null (get ,group 'group_defined))
       (write "group does not exist. failed")
       (if (unit_defined ,unit)
	   (write "unit already exists. failed")
	   (progn
	     (fau_init ,unit)
	     (setf (get ,unit 'unit_defined) T)
	     (setf (get ,unit 'unit_group) ,group)))))


(defun same_union (unit_a unit_b)
  (if (or (eq '/ unit_a) (eq '/ unit_b))
      '()
      (eq (fau_find unit_a) (fau_find unit_b))))


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

; dobra tera to chyba dziala ale trzeba z apostrofami
; na szczescie symbole mozna na luzie zwracac jako wartosci funkcji

(defun get_x (arg)
  (assert (numberp  arg))
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


(defun dim (number args)
  (make-dimv :number (get_x number)
	    :numerator (get_num args)
	    :denominator (get_deno args)))


(defun get_same_type_after_bar (list_arg unit)
  (if (null (first list_arg))
      '(nil nil)
      (if (same_union (first list_arg) unit)
	      (list (first list_arg) nil)
	      (get_same_type_after_bar (rest list_arg) unit))))

; returns a list whose first element is a unit contained in list_arg which has the same type as the given unit,
; (nil if there is no such unit), and the second element is T iff it was found ; todo w tym samym miejscu
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


(defun get_multiplier (list_arg)
  (get_multiplier_before_bar list_arg 1))



(defun get_units (list_arg)
  (cond
    ((null list_arg) '())
    ((numberp (first list_arg)) (get_units (rest list_arg)))
    (t (concatenate 'list (list (first list_arg)) (get_units (rest list_arg))))))


(defun get_units_after_bar (list_arg)
  (if (eq (first list_arg) '/)
      (get_units (rest list_arg))
      (get_units_after_bar (rest list_arg))))


(defun get_units_before_bar (list_arg)
  (cond ((eq (first list_arg) '/) '())
	((numberp (first list_arg)) (get_units_before_bar (rest list_arg)))
	(t (concatenate 'list (list (first list_arg)) (get_units_before_bar (rest list_arg))))))


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


(defun simplify (dimval)
  (let ((simplified (simplify_aux dimval)))
    (if (dimvals_the_same simplified dimval)
	dimval
	(simplify simplified))))


(defun multiply_dimvals (dimval1 dimval2)
  (simplify (make-dimv :number (* (dimv-number dimval1) (dimv-number dimval2))
		       :numerator (concatenate 'list (dimv-numerator dimval1) (dimv-numerator dimval2))
		       :denominator (concatenate 'list (dimv-denominator dimval1) (dimv-denominator dimval2)))))


(defun dimval_is_number (dimval)
  (let ((simplified (simplify dimval)))
    (and (null (dimv-numerator simplified)) (null (dimv-denominator simplified)))))


(defun inverse (dimval)
  (make-dimv :number (/ 1 (dimv-number dimval))
	     :numerator (dimv-denominator dimval)
	     :denominator (dimv-numerator dimval)))


(defun divide_dimvals (dimval1 dimval2)
  (multiply_dimvals dimval1 (inverse dimval2)))


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

(defparameter a (dim 420 '(meter / second)))
(defparameter b (dim 1 '(second)))
(defparameter c (multiply_dimvals a b))
(defparameter d (dim 0.1 '(/ second second)))
(defparameter e (dim 6 '(gram)))
(defparameter f (multiply_dimvals c (multiply_dimvals d e))) ; now f evaluates to 252.00002 meters * grams / second^2 (a quarter of a newton)
