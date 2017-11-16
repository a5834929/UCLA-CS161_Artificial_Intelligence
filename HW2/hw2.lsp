;;	UCLA CS161 Fundamentals of Artificial Intelligence
;;	Andrew Lin 304743326
;;	
;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;; BFS is a function that takes a tree represented in nested lists
;; and outputs the leaves by level-order traversal 
(defun BFS (FRINGE)
	(cond	
		((null FRINGE) nil)
		;; if (car FRINGE) is an atom, put it into the list, and append the result of (cdr FRINGE)
		((atom (car FRINGE)) (cons	(car FRINGE) (BFS (cdr FRINGE))))
		;; if (cdr FRINGE) is null, dp BFS on (car FRINGE) only
		((null (cdr FRINGE)) (BFS (car FRINGE)))
		(T
			(cond	
				;; if (car FRINGE) is a list and (second FRINGE) is an atom, put (second FRINGE) into the list,
				;; and combine (car FRINGE) and (cddr FRINGE) and do BFS
				((atom (second FRINGE)) 
					(cons (second FRINGE) (BFS (append (cons (car FRINGE) nil) (cddr FRINGE)))))
				;; if (car FRINGE) and (second FRINGE) are both lists, combine (car FRINGE) and (second FRINGE)
				;; into one leaf, and append this leaf with (cddr FRINGE)
				;; note that the new leaf should be in one level lower than (cddr FRINGE)
				(T
					(BFS (append (cons (append (car FRINGE) (second FRINGE)) nil) (cddr FRINGE))))
			)
		)
	)
)

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;

;; FINAL-STATE takes a single argument S, the current state, and returns T if it
;; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
	(equal S '(T T T T))
)

;; NEXT-STATE returns the state that results from applying an operator to the
;; current state. It takes three arguments: the current state (S), and which entity
;; to move (A, equal to h for homer only, b for homer with baby, d for homer 
;; with dog, and p for homer with poison). 
;; It returns a list containing the state that results from that move.
;; If applying this operator results in an invalid state (because the dog and baby,
;; or poisoin and baby are left unsupervised on one side of the river), or when the
;; action is impossible (homer is not on the same side as the entity) it returns NIL.
;; NOTE that next-state returns a list containing the successor state (which is
;; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
    (let    ((h (first S))
            (b (second S))
            (d (third S))
            (p (fourth S)))

            (cond
                ;; homer with baby
                ((equal 'b A) 
                             ;; if h==b, negate h and b, and return the state
                    (cond    ((equal h b) (list (list (not h) (not b) d p)))
                             ;; else invalid
                             (T nil)
                    )
                )
                ;; homer with dog
                ((equal 'd A) 
                             ;; if h==d, negate h and d, and return the state
                    (cond    ((and (equal h d) (not (equal b p)))
                                (list (list (not h) b (not d) p)))
                             ;; else invalid
                             (T nil)
                    )
                )
                ;; homer with poison
                ((equal 'p A) 
                             ;; if h==p, negate h and p, and return the state
                    (cond    ((and (equal h p) (not (equal b d)))
                    	        (list (list (not h) b d (not p))))
                             ;; else invalid
                             (T nil)
                    )
                )
                ;; homer alone
                (T     
                             ;; if (b==d or b==p) and b==h invalid
                    (cond    ((and (equal h b) (or (equal b d) (equal b p))) nil)
                             ;; else negate h and return the state
                             (T (list (list (not h) b d p)))
                    )
                )
            )
    )
)

;; SUCC-FN returns all of the possible legal successor states to the current
;; state. It takes a single argument (s), which encodes the current state, and
;; returns a list of each state that can be reached by applying legal operators
;; to the current state.
(defun SUCC-FN (S)
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
)

;; ON-PATH checks whether the current state is on the stack of states visited by
;; this depth-first search. It takes two arguments: the current state (S) and the
;; stack of states visited by DFS (STATES). It returns T if s is a member of
;; states and NIL otherwise.
(defun ON-PATH (S STATES)
    (cond   
         ;; S does not exist in STATES
		 ((null STATES) nil)
         (T 
            (cond    
            	;; S exists in STATES
            	((equal (car STATES) S) T)
                ;; haven't found S
                (T (ON-PATH S (cdr STATES)))
            )
         )
    )
)

;; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
;; states from the initial state to the current state (PATH), and the legal
;; successor states to the last, current state in the PATH (STATES). PATH is a
;; first-in first-out list of states; that is, the first element is the initial
;; state for the current search and the last element is the most recent state
;; explored. MULT-DFS does a depth-first search on each element of STATES in
;; turn. If any of those searches reaches the final state, MULT-DFS returns the
;; complete path from the initial state to the goal state. Otherwise, it returns
;; NIL.
(defun MULT-DFS (STATES PATH)
	(cond
		((null STATES) nil)
		(T
			(let ((dfs-result (DFS (car STATES) PATH)))
				(cond 
					;; visiting repeated state
					((null dfs-result) (MULT-DFS (cdr STATES) PATH))
					;; reach goal state
					(T dfs-result)
				)
			)
		)
	)
)

;; DFS does a depth first search from a given state to the goal state. It
;; takes two arguments: a state (S) and the path from the initial state to S
;; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
;; performs a depth-first search starting at the given state. It returns the path
;; from the initial state to the goal state, if any, or NIL otherwise. DFS is
;; responsible for checking if S is already the goal state, as well as for
;; ensuring that the depth-first search does not revisit a node already on the
;; search path.
(defun DFS (S PATH)
	(cond
		;; reach goal state
		((FINAL-STATE S) (append PATH (list S)))
		;; revisited state
		((ON-PATH S PATH) nil)
		;; unvisited state
		(T (MULT-DFS (SUCC-FN S) (append PATH (list S))))
	)
)


;;;;;;;;;;;;;;
; Test Cases ;
;;;;;;;;;;;;;;

; (print (BFS '(ROOT)))
; (print (BFS '((((L E) F) T))))
; (print (BFS '((R (I (G (H T)))))))
; (print (BFS '(((A (B)) C (D)))))
; (print (BFS '((T (H R E) E))))
; (print (BFS '((A ((C ((E) D)) B)))))
; (print (BFS '((A) ((B)) (((((C))))))))
; (print (BFS '((A ((B (C D)) (E (F)))))))
; (print (BFS '(((A B) (C D) E))))
; (print (BFS '(((A B) (C D) (((E)))))))

; (print (DFS '(nil nil nil nil) nil))
; (print (DFS '(NIL T NIL NIL) '((NIL NIL NIL NIL) (T T NIL NIL))))
; (print (DFS '(T T T NIL) nil))


