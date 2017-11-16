(defun SUB-SEQUENCE (SEQ P L)
	(cond	((null SEQ) nil)
			((= L 0) nil)
			((> P 0) (SUB-SEQUENCE (cdr SEQ) (- P 1) L))
			(T (append (SUB-SEQUENCE (cdr SEQ) P (- L 1)) (list (car SEQ))))
	)
)


(defun PALINDROMEP (L)
	(cond	((null L) T)
			(T (and (equal (car L) (car (last L))) (PALINDROMEP (cdr (butlast L)))))
	)
)

(defun remove-dup (e lst)
	(cond	((null lst) nil)
			((not (equal e (car lst))) (append (list (car lst)) (remove-dup e (cdr lst))))
			(T (remove-dup e (cdr lst)))
	)
)

(defun check-repeat (lst)
	(cond	((null lst) nil)
			(T (append (list (car lst)) (check-repeat (remove-dup (car lst) (cdr lst)))))
	)
)

; (print (SUB-SEQUENCE '(a b c d) 1 2))
; (print (SUB-SEQUENCE '(a b c d) 0 3))
; (print (SUB-SEQUENCE '(a b c d) 2 5))
; (print (SUB-SEQUENCE '(a b c d) 10 3))

; (print (PALINDROMEP '(a b c d)))
; (print (PALINDROMEP '(a b c b a)))
; (print (PALINDROMEP '(a b)))
; (print (PALINDROMEP '(a)))

(print (remove-dup 'a '(a b a b a c d d)))
(print (check-repeat '((a b c) (a b c) (a c) (x y z) (x y) (x y))))







