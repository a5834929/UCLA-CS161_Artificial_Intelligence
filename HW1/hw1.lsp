;;	UCLA CS161 Fundamentals of Artificial Intelligence
;;	Andrew Lin 304743326
;;	
;;	1.	Padovan Sequence
;;		The Padovan sequence is defined as PAD(n+1) = PAD(n-1)+PAD(n-2). 
;;		My solution rewrites it as PAD(n) = PAD(n-2)+PAD(n-3) and calculates
;;		the value of PAD(n) recursively with initial values PAD(0)=PAD(1)=PAD(2)=1.
;;
;;	2.	Number of Additions of Padovan Sequence
;;		The number of additions PAD(n) needs is the sum of that of PAD(n-2) and PAD(n-3) plus 1.
;;		Thus, SUMS is defined as SUMS(n) = SUMS(n-2)+SUMS(n-3)+1 with initial values 
;;		SUMS(0)=SUMS(1)=SUMS(2)=0.
;;
;;	3.	Anonymized Tree Representation
;;		Replace the atoms with "?" until encounter nil of a list, then return the anonymized list
;;		recursively.


;;	PAD(N)
;;	N:	requesting the Nth number in the Padovan sequence
;;	return value:	(1)	for N=0,1,2, return 1
;;					(2) return PAD(N-2)+PAD(N-3) otherwise

(defun PAD (N)
	(cond	((< N 3) 1)
			(T (+ (PAD (- N 2)) (PAD (- N 3))))
	)
)

;;	SUMS(N)
;;	N:	requesting the number of additions for PAD(N)
;;	return value:	(1)	for N=0,1,2, return 0
;;					(2) return SUMS(N-2)+SUMS(N-3)+1 otherwise

(defun SUMS (N)
	(cond	((< N 3) 0)
			(T (+ 1 (+ (SUMS (- N 2)) (SUMS (- N 3)))))
	)
)

;;	ANON(TREE)
;;	TREE:	the input tree in data structure of list or atom
;;	return value:	the anonymized tree with leaves represented in "?"

(defun ANON (TREE)
	(cond	((null TREE) nil)
			((atom TREE) '?)
			(T (cons (ANON (car TREE)) (ANON (cdr TREE))))
	)
)

