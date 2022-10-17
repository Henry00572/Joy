;; Implementiert:
;; Zahlen und Ausdrücke in Klammern, Wahrheitswerte
;; pop, dup, swap, rollup, rolldown, rotate
;; pod, dupd, swapd, rollupd, rolldownd, rotated
;; +, -, *, /, rem, div, max, min
;; or, and, not, xor
;; first, rest, size, at, of, concat, cons
;; i, b, map, dip

(defun stack-operation (s j)
  (cond
    ;; Auf den Stack pushen
    ;; Zahlen und Ausdrücke in () werden auf den Stapel gelegt
    ((or (numberp j) (listp j))
	  (cons j s))
	((eq j 'true)
	  (cons 't s))
	((eq j 'false)
	  (cons 'nil s))
	
  ;; Funktionen
  ;; Umsortierungen
    ;; Entfernt das oberste Element
	((eq j 'pop)
	  (cdr s))
	;; Dubpliziert das oberste Element
	((eq j 'dup)
	  (cons (car s) s))
	;; Vertauscht die obersten zwei Elemente
	((eq j 'swap)
	  (cons (cadr s) (cons (car s) (cddr s))))
	;; Vertauscht die obersten drei Elemente zyklisch, das oberste zwei nach unten
	((eq j 'rollup)
	  (append (list (cadr s) (caddr s) (car s)) (cdddr s)))
	;; Vertauscht die obersten drei Elemente zyklisch, das oberste eins nach unten
	((eq j 'rolldown)
	  (append (list (caddr s) (car s) (cadr s)) (cdddr s)))
	;; Vertauscht das oberste mit dem dritten Element
	((eq j 'rotate)
	  (append (list (caddr s) (cadr s) (car s)) (cdddr s)))
  ;; Umsortierungen unter dem obersten Element
	;; Entfernt das zweite Element, wie [pop] dip
	((eq j 'popd)
	  (cons (car s) (cddr s)))
	;; Dupliziert das zweite Element, wie [dup] dip
	((eq j 'dupd)
	  (append (list (car s)(cadr s) (cadr s)) (cddr s)))
	;; Vertauscht das zweite und dritte Element, wie [swap] dip
	((eq j 'swapd)
	  (append (list (car s) (caddr s) (cadr s)) (cdddr s)))
	;; Wie [rollup] dip
	((eq j 'rollupd)
	  (append (list (car s) (caddr s) (cadddr s) (cadr s)) (cddddr s)))
	;; Wie [rolldown] dip
	((eq j 'rolldownd)
	  (append (list (car s) (cadddr s) (cadr s) (caddr s)) (cddddr s)))
	;; Vertauscht das zweite mit dem vierten Element, wie [rotate] dip
	((eq j 'rotated)
	  (append (list (car s) (cadddr s) (caddr s) (cadr s)) (cddddr s)))
  ;; Berechnungen
    ;; Einfache Berechnungen, Operatoren von Joy und Lisp sind gleich
	;; Unäre Operatoren
	((or (eq j 'not))
	  (cons (funcall j (car s)) (cdr s)))
    ((eq j 'xor)
      (cons (and (or (car s) (cadr s)) (not (and (car s) (cadr s)))) (cddr s)))
	  ;; Binäre Operatoren
	((or (eq j '+) (eq j '*) (eq j '-) (eq j '/) (eq j 'rem)
	     (eq j 'max) (eq j 'min) (eq j 'or) (eq j 'and))
	  (cons (funcall j (cadr s) (car s)) (cddr s)))
	((eq j 'div)
	  (append (list (rem (cadr s) (car s)) (floor (cadr s) (car s))) (cddr s)))
	;; Erweiterte Berechnungen
	;; Listenoperationen
	;; Ersetzt eine Liste durch ihr erstes Element
	((eq j 'first)
	  (cons (caar s) (cdr s)))
	;; Ersetzt eine Liste durch ihren Rest
	((eq j 'rest)
	  (cons (cdar s) (cdr s)))
	;; Ersetzt eine Liste durch ihre Länge
	((eq j 'size)
	  (cons (length (car s)) (cdr s)))
	;; Ersetzt eine Liste und einen Index durch das entsprechende Listenelement
	((eq j 'at)
	  (cons (nth (car s) (cadr s)) (cddr s)))
	;; Ersetzt einen Index und eine Liste durch das entsprechende Listenelement
	((eq j 'of)
	  (cons (nth (cadr s) (car s)) (cddr s)))
	;; Hängt die obersten zwei Elemente aneinander
	((eq j 'concat)
	  (cons (append (cadr s) (car s)) (cddr s)))
	;; Erstellt eine cons-Zelle aus den beiden obersten Elementen
	((eq j 'cons)
	  (cons (cons (cadr s) (car s)) (cddr s)))
	((eq j 'uncons)
	  (append (list (cdar s) (caar s)) (cdr s)))
	
  ;; Kombinatoren
	;; i-Kombinator: führt ein Programm auf dem Stapel in Klammern aus
	((eq j 'i)
	  ;;(stack-operation (cdr s) (caar s)))
	  ;; (interpreter (reverse (append (reverse (car s)) (cdr s)))))
	  (reduce #'stack-operation (car s) :initial-value (cdr s)))
	;; b-Kombinator: führt zwei Programme auf dem Stapel in Klammern aus
	((eq j 'b)
	  ;;(stack-operation (stack-operation (cddr s) (caadr s)) (caar s)))
	  ;; (interpreter (reverse (append (reverse (car s))
	  ;;                               (reverse (cadr s))
	  ;;							   (cddr s)))))
	  (reduce #'stack-operation (append (cadr s) (car s)) :initial-value (cddr s)))
	;; map: Wendet eine Funktion in Quotes auf eine Liste darunter an
	((eq j 'map)
	  (cons (reduce #'(lambda (i j)
	                    (append i (reduce #'stack-operation (car s) :initial-value (list j))))
					(cadr s) :initial-value '())
	  (cddr s)))
	;; dip: Führt ein Programm auf dem Stapel ohne das Element direkt darunter aus.
	((eq j 'dip)
	  (cons (cadr s) (reduce #'stack-operation (car s) :initial-value (cddr s))))
	;; Ansonsten bleibt der Stapel unverändert
	(t s)
	  ))
	  
(defun interpreter (j)
  (reduce #'stack-operation j :initial-value '()))