;; Implementiert:
;; Zahlen und Ausdrücke in Klammern
;; pop, dup, swap, concat, cons
;; +, -, *, /
;; i, b, map, dip

(defun stack-operation (s j)
  (cond
    ;; Zahlen und Ausdrücke in () werden auf den Stapel gelegt
    ((or (numberp j) (listp j))
	  (cons j s))
	;; Funktionen
	;; Entfernt das oberste Element
	((eq j 'pop)
	  (cdr s))
	;; Dubpliziert das oberste Element
	((eq j 'dup)
	  (cons (car s) s))
	;; Vertauscht die obersten zwei Elemente
	((eq j 'swap)
	  (cons (cadr s) (cons (car s) (cddr s))))
	;; Hängt die obersten zwei Elemente aneinander
	((eq j 'concat)
	  (cons (append (cadr s) (car s)) (cddr s)))
	;; Erstellt eine cons-Zelle aus den beiden obersten Elementen
	((eq j 'cons)
	  (cons (cons (cadr s) (car s)) (cddr s)))
	;; Grundrechenarten
	((eq j '+)
	  (cons (+ (car s) (cadr s)) (cddr s)))
	((eq j '*)
	  (cons (* (car s) (cadr s)) (cddr s)))
	((eq j '-)
	  (cons (- (car s) (cadr s)) (cddr s)))
	((eq j '*)
	  (cons (/ (car s) (cadr s)) (cddr s)))
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