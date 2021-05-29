; generalnie to wszystkie sensowne jednostki sa albo jednostkami podstawowymi, albo mnozeniem, albo ilorazem dwoch mnozen
; mnozenia mozemy trzymac po prostu jako liste jednostek
; alternatywnie jednostki niebedace mnozeniem moga byc lista jednoelementowa, kinda, bo poza wyrazeniami i tak trzeba je trzymac jako
; atomy
; albo wszystkie wielkosci to liczba, lista nad kreska, lista pod kreska
; superklasami sa typy jednostek: odlegosc, czas itd
; subklasami sa jednostki: do odleglosci metr, sazen itd
; stary, wtf? tak sie nie pisze obiektowo

; grupy, jednostki, wielkosci, przeliczniki
; funkcje:
; parsowanie
; dodawanie dwoch wielkosci
; dodawanie liczby do wielkosci, jesli wielkosc jest liczba
; mnozenie i dzielenie wielkosci ze soba
; mnozenie i dzielenie wielkosci z liczbami
; tworzenie nowych grup, jednostek
; upraszczanie wielkosci
; wypisywanie jednostek w jakis sensowny sposob

;(define-group nazwa)
;(define-measure nazwa grupa)
;(define-rule from-measure to-measure multiplier)
 

; symbolami mozna manipulowac podobnie jak to zrobilem w connectivity, zatem jednostki moga byc po prostu symbolami ktorych parametrami
; (setf getf) sa grupy, cos takiego. i stosunek do jednostki podstawowej, jako ktora pamietamy pierwsza podana przez uzytkownika jednostke.


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


(defmacro define-group (group_name)
  `(if (get ,group_name 'group_defined)
      (write "group already exists.")
      (setf (get ,group_name 'group_defined) T)))


(defmacro define-measure (unit group)
  `(if (null (get ,group 'group_defined))
       (write "group does not exist")
       (if (get ,unit 'unit_defined)
	   (write "unit already exists")
	   (progn
	     (setf (get ,unit 'unit_defined) T)
	     (setf (get ,unit 'unit_group) ,group)))))

; dobra tera to chyba dziala ale trzeba z apostrofami
; na szczescie symbole mozna na luzie zwracac jako wartosci funkcji