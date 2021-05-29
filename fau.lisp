(defmacro set_father (son father)
  `(setf (get ,son 'father) ,father))

(defmacro fau_init (new_symbol)
  `(set_father ,new_symbol ,new_symbol)
  `(setf (get ,new_symbol 'ratio) 1.0))

; ustawiamy jako wlasnego ojca to co nam zwroci fau_find i sami to zwracamy
(defun fau_find (s)
  (if (eq s (get s 'father))
      s
      (progn
	; ustaw odpowiednio przelicznik. moze zapamietaj starego ojca i tutaj pomnoz swoj przelicznik przez jego?
	; tak czy inaczej no to chyba zadziala jezeli bedziemy dla kazdego syna pamietac ilu ojcow jest wart xd
	; i zawsze przy zmianie ojca zmieniamy tez te wartosci
	(set_father s (fau_find (get s 'father)))
	(get s 'father))))

(defun fau_union (a b)
  (set_father (fau_find a) (fau_find b)))
