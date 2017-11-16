;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
)

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp")
)

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
)

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
)

(defun isWall (v)
  (= v wall)
)

(defun isBox (v)
  (= v box)
)

(defun isKeeper (v)
  (= v keeper)
)

(defun isStar (v)
  (= v star)
)

(defun isBoxStar (v)
  (= v boxstar)
)

(defun isKeeperStar (v)
  (= v keeperstar)
)

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (row col)
    (cond 	((null row) nil)
		    (T  (if (or (isKeeper (car row)) (isKeeperStar (car row)))
		        	col
		        	(getKeeperColumn (cdr row) (+ col 1))
		        );end if
		    );end t
    );end cond
)

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
    (cond 	((null s) nil)
	        (T (let ((x (getKeeperColumn (car s) 0)))

		            (if x
					    ;keeper is in this row
					    (list x row)
					    ;otherwise move on
					    (getKeeperPosition (cdr s) (+ row 1))
					);end if
				);end let
		    );end t
    );end cond
);end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
    (cond 	((null L) nil)
	        (T (let ((cur (car L))
					(res (cleanUpList (cdr L))))

			     	(if cur 
				 		(cons cur res)
				  		res
				  	)
		    	);end let
	        );end t
    );end cond
);end 

; reaches goal state if no misplaced boxes left
(defun goal-test (s)
    (= (h1 s) 0)
)

; get square (row, col) from s
(defun get-square (s row col)
	(cond	((or (< row 0) (< col 0)) 1); out of bound
			((or (>= row (length s)) (>= col (length (car s)))) 1); out of bound
			(T (car (nthcdr col (car (nthcdr row s)))))
	)
)

; helper function of set-square
(defun set-square-col (s col v)
	(cond	((not (= col 0)) (cons (car s) (set-square-col (cdr s) (- col 1) v))) 
			(T (cons v (cdr s)))
	)
)

; set square (row, col) to v, and returns the new state
(defun set-square (s row col v)
	(cond	((not (= row 0)) (cons (car s) (set-square (cdr s) (- row 1) col v)))
			(T (cons (set-square-col (car s) col v) (cdr s)))
	)
)

; helper function of getPosition
(defun getColumn (s v row col)
    (cond 	
    		((null (car (nthcdr col s))) nil)
		    ((= v (car (nthcdr col s))) (cons (list row col) (getColumn s v row (+ col 1))))
		    (T (getColumn s v row (+ col 1)))	
    )
)

; returns the positions of a specific square
(defun getPosition (s v row col)
	(cond 	((null (car (nthcdr row s))) nil)
			(T (append 
						(getColumn (car (nthcdr row s)) v row col)
						(getPosition s v (+ row 1) col)
				)
			)
	)
)

; performs moves that generate legal successors
(defun try-move (s row col dir-row dir-col)
	(let* 	((new-row (+ row dir-row))
			 (new-col (+ col dir-col))
			 (nnew-row (+ new-row dir-row))
			 (nnew-col (+ new-col dir-col))
			 (self (get-square s row col))
			 (next (get-square s new-row new-col))
			 (nnext (get-square s nnew-row nnew-col))
			 ; if self is keeper, v1 = blank, else star
			 (v1 (cond	((isKeeper self) blank)
			 			(T star)))
			 ; if next is blank or box, v2 = keepr, else keeperstar
			 (v2 (cond	((or (isBlank next) (isBox next)) keeper)
			 			(T keeperstar)))
			 ; if nnext is blank, v3 = box, else boxstar
			 (v3 (cond	((isBlank nnext) box)
			 			(T boxstar))))

			(cond
				;keeper or keeperStar can move or finds a goal
				((or (isBlank next) (isStar next))
					(list (set-square (set-square s row col v1) new-row new-col v2))
				)
				;keeper or keeperStar finds a box or s boxStar
				;check the next square by the box or boxstar if it's a blank or goal
				((or (isBox next) (isBoxStar next))
					(cond	((or (isBlank nnext) (isStar nnext))
								(list (set-square (set-square (set-square s row col v1) new-row new-col v2) nnew-row nnew-col v3)))
							(T nil)
					)
				)
				;runs into wall
				(T nil)
			)
	)
)

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
(defun next-states (s)
	(let*	((pos (getKeeperPosition s 0))
			(row (cadr pos))
			(col (car pos))
			(result (append (try-move s row col -1 0)	; UP 	(-1,0)
							(try-move s row col 1  0) 	; DOWN 	( 1,0)
							(try-move s row col 0 -1) 	; LEFT	(0,-1)
							(try-move s row col 0  1)))); RIGHT ( 0,1)

			result
	)
)

; sum up all distances
(defun addDist (lst)
	(cond	((null lst) 0)
			(T (+ (car lst) (addDist (cdr lst))))
	)
)

; get max value of a list
(defun getMax (m lst)
	(cond	((null lst) m)
			((> (car lst) m) (getMax (car lst) (cdr lst)))
			(T (getMax m (cdr lst)))
	)
)

; get min value of a list
(defun getMin (m lst)
	(cond	((null lst) m)
			((< (car lst) m) (getMin (car lst) (cdr lst)))
			(T (getMin m (cdr lst)))
	)
)

; calculate manhattan distance between two squares
; if both dimensions are not 0, the keeper needs
; at least one turn around the box to reach the goal
(defun manhattan (v1 v2)
	(let* 	((d1 (- (car v1) (car v2)))
			(d2 (- (cadr v1) (cadr v2)))
			(d 	(+  (cond ((< d1 0) (- d1)) (T d1))
					(cond ((< d2 0) (- d2)) (T d2)))))

			(cond	((and (not (= d1 0)) (not (= d2 0))) (+ d 2))
					(T d)
			)
	)
)

; returns the distance of a box to its nearest goal
(defun helper2 (pos lst)
	(cond	((null lst) nil)
			(T (cons (manhattan pos (car lst)) (helper2 pos (cdr lst))))
	)
)

; calculate manhattan distance of each box to its nearest goal
(defun helper1 (pos lst)
	(cond	((null pos) nil)
			(T 	(let 	((res (helper2 (car pos) lst)))
						(cond 	((null res) nil)
								(T (cons (getMin (car res) res) (helper1 (cdr pos) lst)))
						)
				)
			)
	)
)

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; the number of misplaced boxes in a state.
; This is an admissible heuristic because if there exists a misplaced box, 
; you need at least one push to reach the goal. So h1 will not overestimate
; the actually cost
(defun h1 (s)
	(cond 	((null s) 0)
			(T (+ (count box (car s)) (h1 (cdr s))))
	)
)

; max distance of the keeper to the boxes
(defun h2 (s)
	(let 	((pos (getKeeperPosition s 0)))
			(getMax 0 (helper2 (list (cadr pos) (car pos)) (getPosition s box 0 0)))
	)
)

; the max distance from keeper to boxes, and the sum of nearest distance from each box to goal
(defun h3 (s)
	(let 	((box-list (getPosition s box 0 0))
			(star-list (getPosition s star 0 0)))

			(+ (h2 s) (addDist (helper1 box-list star-list)))
	)
)

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h304743326 (s)
	(h3 s)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; #|
;  | Some predefined problems.
;  | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
;  | Problems are roughly ordered by their difficulties.
;  | For most problems, we also privide 2 additional number per problem:
;  |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
;  |    2) the depth of the optimal solution.
;  | These numbers are located at the comments of the problems. For example, the first problem below 
;  | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
;  | 
;  | Your implementation may not result in the same number of nodes expanded, but it should probably
;  | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
;  | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
;  | for checking whether your heuristic is admissible.
;  |
;  | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
;  | 
;  |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
		   (1 0 3 0 0 1)
		   (1 0 2 0 0 1)
		   (1 1 0 1 1 1)
		   (1 0 0 0 0 1)
		   (1 0 0 0 4 1)
		   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
		   (1 0 0 0 0 0 1) 
		   (1 0 0 0 0 0 1) 
		   (1 0 0 2 1 4 1) 
		   (1 3 0 0 1 0 1)
		   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
		   (1 0 0 0 1 0 0 0 1)
		   (1 0 0 0 2 0 3 4 1)
		   (1 0 0 0 1 0 0 0 1)
		   (1 0 0 0 1 0 0 0 1)
		   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
		   (0 0 0 0 0 1 4)
		   (0 0 0 0 0 0 0)
		   (0 0 1 1 1 0 0)
		   (0 0 1 0 0 0 0)
		   (0 2 1 0 0 0 0)
		   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
		   (1 1 0 0 1 1)
		   (1 0 0 0 0 1)
		   (1 4 2 2 4 1)
		   (1 0 0 0 0 1)
		   (1 1 3 1 1 1)
		   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   	(1 0 0 0 0 0 4 1)
		(1 0 0 0 2 2 3 1)
		(1 0 0 1 0 0 4 1)
		(1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
		(0 0 1 1 1 1 0 0 0 3)
		(0 0 0 0 0 1 0 0 0 0)
		(0 0 0 0 0 1 0 0 1 0)
		(0 0 1 0 0 1 0 0 1 0)
		(0 2 1 0 0 0 0 0 1 0)
		(0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
		(1 4 0 0 4 1)
		(1 0 2 2 0 1)
		(1 2 0 1 0 1)
		(1 3 0 0 4 1)
		(1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
		(1 1 1 0 0 1 1 1 1) 
		(1 0 0 0 0 0 2 0 1) 
		(1 0 1 0 0 1 2 0 1) 
		(1 0 4 0 4 1 3 0 1) 
		(1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
			(1 0 0 0 1 1 0)
			(1 3 2 0 0 1 1)
			(1 1 0 2 0 0 1)
			(0 1 1 0 2 0 1)
			(0 0 1 1 0 0 1)
			(0 0 0 1 1 4 1)
			(0 0 0 0 1 4 1)
			(0 0 0 0 1 4 1)
			(0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
			(1 4 0 0 0 4 1)
			(1 0 2 2 1 0 1)
			(1 0 2 0 1 3 1)
			(1 1 2 0 1 0 1)
			(1 4 0 0 4 0 1)
			(1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
			(1 1 1 1 1 0 0 0 1 1 1 1)
			(1 0 0 0 2 0 0 0 0 0 0 1)
			(1 3 0 0 0 0 0 0 0 0 0 1)
			(1 0 0 0 2 1 1 1 0 0 0 1)
			(1 0 0 0 0 1 0 1 4 0 4 1)
			(1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
			(1 4 0 0 0 0 0 2 0 1)
			(1 0 2 0 0 0 0 0 4 1)
			(1 0 3 0 0 0 0 0 2 1)
			(1 0 0 0 0 0 0 0 0 1)
			(1 0 0 0 0 0 0 0 4 1)
			(1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
			(0 2 1 4 0 0 0)
			(0 2 0 4 0 0 0)	   
			(3 2 1 1 1 0 0)
			(0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
			(1 0 0 0 0 0 1)
			(1 0 0 2 2 0 1)
			(1 0 2 0 2 3 1)
			(1 4 4 1 1 1 1)
			(1 4 4 1 0 0 0)
			(1 1 1 1 0 0 0)))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
			(1 0 0 0 1 0 0 0)
			(1 2 1 0 1 1 1 1)
			(1 4 0 0 0 0 0 1)
			(1 0 0 5 0 5 0 1)
			(1 0 5 0 1 0 1 1)
			(1 1 1 0 3 0 1 0)
			(0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
			(1 3 0 0 1 0 0 0 4 1)
			(1 0 2 0 2 0 0 4 4 1)
			(1 0 2 2 2 1 1 4 4 1)
			(1 0 0 0 0 1 1 4 4 1)
			(1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
			(0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
			(1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
			(0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
			(0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
			(0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
			(0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
			(0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
			(1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
			(0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
			(0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
			(0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
			(0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)
			(0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
			(0 0 0 1 0 0 0 0 1 0 0 0)
			(0 0 0 1 0 0 0 0 1 0 0 0)
			(1 1 1 1 0 0 0 0 1 1 1 1)
			(0 0 0 0 1 0 0 1 0 0 0 0)
			(0 0 0 0 0 0 3 0 0 0 2 0)
			(0 0 0 0 1 0 0 1 0 0 0 4)
			(1 1 1 1 0 0 0 0 1 1 1 1)
			(0 0 0 1 0 0 0 0 1 0 0 0)
			(0 0 0 1 0 0 0 0 1 0 0 0)
			(0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??) no solution
(setq p20 '((0 0 0 1 1 1 1 0 0)
			(1 1 1 1 0 0 1 1 0)
			(1 0 0 0 2 0 0 1 0)
			(1 0 0 5 5 5 0 1 0)
			(1 0 0 4 0 4 0 1 1)
			(1 1 0 5 0 5 0 0 1)
			(0 1 1 5 5 5 0 0 1)
			(0 0 1 0 2 0 1 1 1)
			(0 0 1 0 3 0 1 0 0)
			(0 0 1 1 1 1 1 0 0)))

;(??,??) no solution
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
			(1 1 1 0 0 1 1 1 1 0)
			(1 0 0 2 0 0 0 1 1 0)
			(1 3 2 0 2 0 0 0 1 0)
			(1 1 0 2 0 2 0 0 1 0)
			(0 1 1 0 2 0 2 0 1 0)
			(0 0 1 1 0 2 0 0 1 0)
			(0 0 0 1 1 1 1 0 1 0)
			(0 0 0 0 1 4 1 0 0 1)
			(0 0 0 0 1 4 4 4 0 1)
			(0 0 0 0 1 0 1 4 0 1)
			(0 0 0 0 1 4 4 4 0 1)
			(0 0 0 0 1 1 1 1 1 1)))

;(??,??) no solution
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
			(0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
			(0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
			(0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
			(0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
			(1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
			(1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
			(1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
			(1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
			(0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
			(0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

(setq p23 '((1 1 1 1 1 1)
		   (1 0 0 0 0 1)
		   (1 0 4 2 6 1)
		   (1 1 0 1 1 1)
		   (1 0 0 0 0 1)
		   (1 0 0 0 0 1)
		   (1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; #|
;  | Utility functions for printing states and moves.
;  | You do not need to understand any of the functions below this point.
;  |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (T (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(T (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(T (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (format t "~C~%" #\return)
    (sleep delay)
    );end dolist
  );end defun


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Testing
(load-a-star)
; (time (sokoban p7 #'h0))
; (time (sokoban p7 #'h1))
; (time (sokoban p7 #'h3))
; (time (sokoban p6 #'h3))


