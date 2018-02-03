;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their
; header comments.
;
; Do not modify a-star.lsp.
;
; This file also contains many helper functions. You may call any of them in
; your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any
; node. That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your
; heuristic functions. Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on
; memory. So, it may crash on some hard sokoban problems and there is no easy
; fix (unless you buy Allegro). 
;
; Of course, other versions of Lisp may also crash if the problem is too hard,
; but the amount of memory available will be relatively more relaxed. Improving
; the quality of the heuristic will mitigate this problem, as it will allow A*
; to solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition
; (which will affect your score).
;  
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
  (load "hw3.lsp"))

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star))

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

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
  (= v blank))

(defun isWall (v)
  (= v wall))

(defun isBox (v)
  (= v box))

(defun isKeeper (v)
  (= v keeper))

(defun isStar (v)
  (= v star))

(defun isBoxStar (v)
  (= v boxstar))

(defun isKeeperStar (v)
  (= v keeperstar))

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond
    ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
          col
	      (getKeeperColumn (cdr r) (+ col 1))))))

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond
    ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		   ;keeper is in this row
		   (list x row)
		   ;otherwise move on
		   (getKeeperPosition (cdr s) (+ row 1)))))))

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond
    ((null L) nil)
	(t (let ((cur (car L))
		     (res (cleanUpList (cdr L))))
         (if cur 
           (cons cur res)
           res)))))

;
; some math primitives that are built into LISP but we're not allowed to use :(
;

; return the absolute value of n
(defun absolute (n)
  (if (< n 0) (- 0 n) n))

; find the minimum value of a list
; if list is null, return 0
(defun minimum (l &optional (min-sofar (first l)))
  (let ((cur (first l)))
    (cond
      ((null min-sofar) 0)
      ((null l) min-sofar)
      ((< cur min-sofar) (minimum (rest l) cur))
      (t (minimum (rest l) min-sofar)))))


; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  ; we are in a goal state if and only if the number of boxes (excluding boxes
  ; on goals) is zero
  (if (null s) t (and (= (count box (first s)) 0) (goal-test (rest s)))))

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP)
;               (try-move s DOWN)
;               (try-move s LEFT)
;               (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,
; DOWN, LEFT, RIGHT. Any NIL result returned from try-move can be removed by
; cleanUpList.
; 

; gets an element from a particular spot in the game grid
; origin is at the top left of the grid (screen coordinates)
(defun at-location (s pos)
  (let ((x (first pos))
        (y (second pos)))
    (first (nthcdr x (first (nthcdr y s))))))

; takes a list, an index, and value which we want to set that index to
(defun set-nth (l n v)
  (let ((before (butlast l (- (length l) n))) ; get slice before the index
        (after (nthcdr n l))) ; get list slice after the index
    (append before (list v) (rest after)))) ; stick our value in the middle

; sets a position in the grid to a value
(defun set-location (s pos val)
  (let* ((x (first pos))
         (y (second pos))
         (l (first (nthcdr y s))))
    (set-nth s y (set-nth l x val))))

; check if pos is out of bounds, assuming each row has the same length
(defun out-of-bounds (s pos)
  (let ((y-max (length s))
        (x-max (length (first s)))
        (x (first pos))
        (y (second pos)))
    (or (< x 0) (< y 0) (>= x x-max) (>= y y-max))))

; try to push a box in a particular direction, where direction is an (x, y)
; tuple specifying a movement vector centered at the location vector pos.
(defun push-box (s pos dir)
  (let ((at-cur (at-location s pos))
        (next-pos (add-coords pos dir)))
    (if (out-of-bounds s next-pos) nil ; check if box will be out of bounds
      (let* ((at-next (at-location s next-pos))
             (next-item (if (isStar at-next) boxstar box)) ; if we're moving a box onto a star, make the symbol boxstar
             (prev-item (if (isBoxStar at-cur) star blank))) ; if we're moving from boxstar, make the symbol a star
        (cond
          ((or (isWall at-next) (isBox at-next)) nil) ; if there's a wall or box in our way, we can't move
          (t (set-location (set-location s next-pos next-item) pos prev-item))))))) ; otherwise we make our move

(defun push-keeper (s pos dir)
  (let ((at-cur (at-location s pos))
        (next-pos (add-coords pos dir)))
    (if (out-of-bounds s next-pos) nil ; check if keeper will be out of bounds
      (let* ((at-next (at-location s next-pos))
             (next-item (if (isStar at-next) keeperstar keeper)) ; if we're moving the keeper onto a star, make the new symbol keeperstar
             (prev-item (if (isKeeperStar at-cur) star blank)) ; if we're moving the keeper off of a star, make the old symbol a star
             (try-push-box (if (or (isBox at-next) (isBoxStar at-next))
                             (push-box s next-pos dir) ; if there's a box in the way, try pushing it in the same direction as the keeper
                             s)))
        (cond
          ((isWall at-next) nil) ; if there's a wall in the way, do not proceed
          ((null try-push-box) nil) ; if there's an unpushable box in the way, do not proceed
          (t (set-location (set-location try-push-box next-pos next-item) pos prev-item))))))) ; otherwise, make the move

; adds coordinate a to coordinate b, returns coordinate
(defun add-coords (a b)
  (let ((xa (first a))
        (ya (second a))
        (xb (first b))
        (yb (second b)))
    (list (+ xa xb) (+ ya yb))))

; returns absolute manhattan distance between two coordinates
(defun abs-dist (a b)
  (let ((xa (first a))
        (ya (second a))
        (xb (first b))
        (yb (second b)))
    (+ (absolute (- xa xb)) (absolute (- ya yb)))))

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
         (result (list (push-keeper s pos '(0 1))     ; try all possible moves
                       (push-keeper s pos '(1 0))
                       (push-keeper s pos '(0 -1))
                       (push-keeper s pos '(-1 0)))))
    (cleanUpList result))) ; remove results from impossible moves

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0) ; never overstimates, always underestimates

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
  (if (null s) 0 (+ (count box (first s)) (h1 (rest s)))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.

; find all elements on the map that match with 'obj', return a list of their
; coordinates
(defun find-objects (s obj &optional (x 0) (y 0))
  (let* ((row (first s))
         (item (first row))
         (rest-s (cons (rest row) (rest s))))
    (cond
      ((null s) nil) ; we're out of map to check
      ((null row) (find-objects (rest s) obj 0 (+ y 1))) ; we ran out of the current row, advance to next
      ((= item obj) (cons (list x y) (find-objects rest-s obj (+ x 1) y))) ; the object matched, add its coordinate
      (t (find-objects rest-s obj (+ x 1) y))))) ; the object didn't match, continue searching

; efficiently get the values of things surrounding a position
;(defun surroundings (s pos)
;  (let* ((x (first pos))
;         (y (second pos))
;         (max-x (length (first s)))
;         (max-y (length s))
;         (above (- y 1))
;         (below (+ y 1))
;    (list
;      (if (>= above 0) (nthcdr x (nthcdr above s)) nil)
;      (if (<= below max-y) (nthcdr x (nthcdr below y)) nil)
;      (if 

; if this function passes, there are no cornered boxes in the current state
(defun box-check (s boxes)
  (if (null boxes)
    t
    (let* ((box (first boxes))
           (box-x (first box))
           (box-y (second box))
           (up-coord (list box-x (- box-y 1)))
           (down-coord (list box-x (+ box-y 1)))
           (left-coord (list (- box-x 1) box-y))
           (right-coord (list (+ box-x 1) box-y)))
      ; now we look up down left and right, checking if those coordinates are
      ; out of bounds, or blocked by a wall
      (let ((up (if (out-of-bounds s up-coord) nil (isWall (at-location s up-coord))))
            (down (if (out-of-bounds s down-coord) nil (isWall (at-location s down-coord))))
            (left (if (out-of-bounds s left-coord) nil (isWall (at-location s left-coord))))
            (right (if (out-of-bounds s right-coord) nil (isWall (at-location s right-coord)))))
        (cond
          ((null boxes) t)
          ((and left (or up down)) nil) ; check if the box is cornered
          ((and right (or up down)) nil) ; check if the box is cornered
          (t (box-check s (rest boxes)))))))) ; continue checking boxes

; for a particular box, return a list of manhattan distances to all goals in the
; list of goals
(defun distance-to-goals (box goals)
  (let ((goal (first goals)))
    (cond
      ((null goals) nil)
      (t (cons (abs-dist box goal) (distance-to-goals box (rest goals)))))))

; for all boxes and all goals, find the smallest distance from each box to
; a goal, and add it to the heuristic function
(defun box-distance (boxes goals)
  (let ((box (first boxes)))
    (if (or (null boxes) (null goals))
      0
      (+ (minimum (distance-to-goals box goals))
         (box-distance (rest boxes) goals)))))

; if a box is cornered, incur a large penalty as the game is unsolvable in this
; state
; otherwise, return a sum of distance from each box to the goal closest to it
(defun h104494120 (s)
  (let ((boxes (find-objects s box))
        (goals (find-objects s star)))
    (if (not (box-check s boxes))
      2400 ; incur large penalty
      (+ (box-distance boxes goals)
         (minimum (distance-to-goals (getKeeperPosition s 0) goals))))))

(defun test (start)
  (a* start #'goal-test #'next-states #'h104494120))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;
;; Some predefined problems.
;; Each problem can be visualized by calling (printstate <problem>).
;; For example, (printstate p1).
;;
;; Problems are ordered roughly by their difficulties.
;; For most problems, we also privide 2 additional number per problem:
;;    1) # of nodes expanded by A* using our next-states and h0 heuristic.
;;    2) the depth of the optimal solution.
;; These numbers are located at the comments of the problems. For example, the
;; first problem below was solved by 80 nodes expansion of A* and its optimal
;; solution depth is 7.
;; 
;; Your implementation may not result in the same number of nodes expanded, but
;; it should probably give something in the same ballpark. As for the solution
;; depth, any admissible heuristic must make A* return an optimal solution. So,
;; the depths of the optimal solutions provided could be used for checking
;; whether your heuristic is admissible.
;;
;; Warning: some problems toward the end are quite hard and could be impossible
;; to solve without a good heuristic!
;;

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
	    (1 1 1 1 0 0 0)
	    ))

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
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
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

;(??,??)
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

;(??,??)
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

;(??,??)
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 
;; Utility functions for printing states and moves.
;; You do not need to understand any of the functions below this point.
;; 

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1))))
    (cond
      ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT)))))

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond
    ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m)))))) 

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond
    ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))))

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)))

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%"))))

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)))
