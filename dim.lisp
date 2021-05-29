; generalnie to wszystkie sensowne jednostki sa albo jednostkami podstawowymi, albo mnozeniem, albo ilorazem dwoch mnozen
; mnozenia mozemy trzymac po prostu jako liste jednostek
; alternatywnie jednostki niebedace mnozeniem moga byc lista jednoelementowa, kinda, bo poza wyrazeniami i tak trzeba je trzymac jako
; atomy
; albo wszystkie wielkosci to liczba, lista nad kreska, lista pod kreska

; funkcje:
; dodawanie dwoch wielkosci
; dodawanie liczby do wielkosci, jesli wielkosc jest liczba
; mnozenie i dzielenie wielkosci ze soba
; mnozenie i dzielenie wielkosci z liczbami
; upraszczanie wielkosci
; no i ogolnie to trzeba tez te baze stworzyc

; two types of entities will be defined in the library.
; the first will be a unit, representing a an elementary unit like: meter, second, kilogram.
; the second will be what the library is actually about: a dimensional value. let us note that
; every sensible dimensinal value is composed of a number, some units multiplied together, and
; possibly a division bar with some more units multiplied together below it. so in practice we
; can have a class with three attributes: the number, the list of units above the division bar
; and the list of units below it. thus 420 m/s^2 would have 420 as the number, m as the first
; list and (s s) as the second list. in some cases one (or both) the lists can be empty,
; for example for 1Hz = 1/s, the first list will be empty.

; dobra, tylko tera to o czym pisalem p jurkiewiczowi to wymaga co najmniej jakiegos find and union zeby to hulao

; (defparameter a '(2137 km / h))
; (eq (second a) 'km)
; T

; (defparameter a (dim 20 '(meter / second)))

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

(defstruct dim
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
  (make-dim :number (get_x number)
	    :numerator (get_num args)
	    :denominator (get_deno args)))


(defun get_same_type (list_arg unit)
  (if (null list_arg)
      '()
      (if (same_union (first list_arg) unit)
	  (first list_arg)
	  (get_same_type (rest list_arg) unit))))


(defun get_ratio (unit_a unit_b)
  (/ (get unit_a 'ratio) (get unit_b 'ratio)))


(defun get_list_without (list_arg what)
  (if (null list_arg)
      '()
      (if (eq (first list_arg) what)
	  (rest list_arg)
	  (concatenate 'list (list (first list_arg)) (get_list_without (rest list_arg) what)))))


(defun simplify_single_list (arg)
  (if (null arg)
      '()
      (if (eq (first arg) '/)
	  (concatenate 'list '(/) (simplify_single_list (rest arg)))
	  (let
	      ((same_type (get_same_type (rest arg) (first arg))))
	    (if (null same_type)
		(concatenate 'list (list (first arg)) (simplify_single_list (rest arg)))
		(concatenate 'list (list (get_ratio same_type (first arg))) (list (first arg)) (simplify_single_list (get_list_without arg same_type))))))))


(defun simplify (dimval)
  ; najpierw uprosc licznik ze samym soba, tj usun zduplikowane jednostki
  ; moze tak, zeby uproscic dana liste sama ze soba: dla kazdego kolejnego
  ; symbolu patrzymy czy w reszcie listy jest jakis symbol ktory da sie uproscic.
  ; jezeli tak, to go zastepujemy ilorazem. w ten sposob potencjalnie dostajemy
  ; liste jednostek i liczb, wiec zbieramy hurtem liczby i jednostki.
  ; heh, nawet nie XD mozna te funkcje wywolac dla licznika skonkatenowanego z kreska i mianownikiem,
  ; a potem zebrac do kupy osobno liczby i osobno jednostki
  )

(define-group 'distance)
(define-measure 'meter 'distance)
(define-measure 'kilometer 'distance)
(define-measure 'mile 'distance)
(define-rule 'kilometer 'meter (/ 1 1000))
(define-rule 'mile 'meter (/ 1 1609))
