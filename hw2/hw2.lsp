;; Rodrigo Valle
;; CS 161 Homework 2

;; question 1
;; depth first search

(defun dfs (tree)
  "Perform depth first search, return list of leaves in order of visit"
  (cond
    ((null tree) nil)         ; tree is empty, return nil
    ((atom tree) (list tree)) ; tree is non-null atom, return list with atom
    (t
      (let ((head (first tree))
            (tail (rest tree)))
        (append (dfs head) (dfs tail)))))) ; evaluate tree depth first


;; question 2
;; iterative depth first earch

(defun dfsl (tree d)
  "Perform depth first search limited to depth d"
  (cond
    ((null tree) nil)          ; base case 1: empty list
    ((atom tree) (list tree))  ; base case 2: leaf
    ((= d 0) nil)              ; base case 3: hit max depth
    (t
      (let ((head (first tree))
            (tail (rest tree)))
        (append (dfsl head (- d 1)) (dfsl tail d)))))) ; descend into head

;; This function exists only to avoid left recursion (and potential stack
;; overflow on large trees with large depth specified). Makes sure that dfsl is
;; called with increasing 'iter' and results are appended to a list in order
;; from smallest to largest 'iter' instead of littering the stack with left
;; recursive function calls.
(defun dfsl-collector (tree iter max-iter)
  "Helper function to collect limited depth first search traversals"
  (let ((traversal (dfsl tree iter)))
    (if (= iter max-iter)
      traversal
      (append traversal (dfsl-collector tree (+ iter 1) max-iter)))))

(defun dfid (tree height)
  "Perform depth first search iterative deepening"
  (dfsl-collector tree 0 height))

;; question 3
;; missionary-cannibal problem

;; These functions implement a depth-first iterative-deepening solver for the
;; missionary-cannibal problem. In this problem, three missionaries and three
;; cannibals are trying to go from the east side of a river to the west side.
;; They have a single boat that can carry two people at a time from one side of
;; the river to the other. There must be at least one person in the boat to
;; cross the river. There can never be more cannibals on one side of the river
;; than missionaries. If there are, the cannibals eat the missionaries.

;; In this implementation, a state is represented by a single list (MISSIONARIES
;; CANNIBALS SIDE). SIDE represents which side the boat is currently on, and is
;; T if it is on the east side and NIL if on the west side. MISSIONARIES and
;; CANNIBALS represent the number of missionaries and cannibals on the same side
;; as the boat. Thus, the initial state for this problem is (3 3 T) (three
;; missionaries, three cannibals, and the boat are all on the east side of the
;; river) and the goal state is (3 3 NIL).

;; The main entry point for this solver is the function ID-DFS, which is called
;; with the initial state to search from and the depth up to which depth-first
;; search will be performed. It returns the complete path from the initial
;; state to the goal state: this path is a list of intermediate problem states.
;; The first element of the path is the initial state and the last element is
;; the goal state. Each intermediate state is the state that results from
;; applying the appropriate operator to the preceding state.

;; To solve the original problem, one would call (ID-DFS '(3 3 T) 0).

;; Examples of calls to some of the helper functions can be found after the
;; code.


;; FINAL-STATE takes a single argument (S), the current state, and returns T if
;; it is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 nil)))

;; NEXT-STATE returns the state that results from applying an operator to the
;; current state. It takes three arguments: the current state (S), a number of
;; missionaries to move (M), and a number of cannibals to move (C). It returns a
;; list containing the state that results from moving that number of
;; missionaries and cannibals from the current side of the river to the other
;; side of the river. If applying this operator results in an invalid state
;; (because there are more cannibals than missionaries on either side of the
;; river, or because it would move more missionaries or cannibals than are on
;; this side of the river) it returns NIL.
;;
;; NOTE that next-state returns a list containing the successor state (which is
;; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let* ((mA (- (first s) m))  ; missionaries on side A after move
         (cA (- (second s) c)) ; cannibals on side A after move
         (mB (- 3 mA))         ; missionaries on side B afte rmove
         (cB (- 3 cA)))        ; cannibals on side B after move
    (cond
      ((> (+ m c) 2) nil)            ; more than 2 people in boat
      ((and (> mA 0) (> cA mA)) nil) ; more C than M on side A, M nonzero
      ((and (> mB 0) (> cB mB)) nil) ; more C than M on side B, M nonzero
      ((or (< cA 0) (< cB 0) (< mA 0) (< mB 0)) nil) ; bounds check
      ((or (> cA 3) (> cB 3) (> mA 3) (> mB 3)) nil) ; bounds check
      (t (list (list mB cB (not (third s))))))))

;; SUCC-FN returns all of the possible legal successor states to the current
;; state. It takes a single argument (S), which encodes the current state, and
;; returns a list of states that can be reached by applying legal operators to
;; the current state.
(defun succ-fn (s)
  (append (next-state s 1 0)
          (next-state s 0 1)
          (next-state s 2 0)
          (next-state s 0 2)
          (next-state s 1 1)))

;; MULT-DFS is a helper function for SINGLE-DFS. It takes three arguments: the
;; path from the initial state to the current state (PATH), the legal successor
;; states to the last state on PATH (STATES), and the depth (DEPTH). PATH is a
;; first-in first-out list of states; that is, the first element is the initial
;; state for the current search and the last element is the most recent state
;; explored. MULT-DFS does a single depth-first iteration to the given depth on
;; each element of STATES in turn. If any of those searches reaches the final
;; state, MULT-DFS returns the complete path from the initial state to the goal
;; state. Otherwise, it returns NIL.
(defun mult-dfs (states path depth)
  (let ((head (first states))
        (tail (rest states)))
    (cond                     ; descend into head if possible
      ((null states) nil)     ; base case: nothing to descend into
      (t (or (single-dfs head path depth) (mult-dfs tail path depth))))))

;; SINGLE-DFS does a single depth-first iteration to the given depth. It takes
;; three arguments: a state (S), the path from the initial state to S (PATH),
;; and the depth (DEPTH). If S is the initial state in our search, PATH should
;; be NIL. It performs a depth-first search starting at the given state. It
;; returns the path from the initial state to the goal state, if any, or NIL
;; otherwise.
(defun single-dfs (s path depth)
  (let ((nextpath (append path (list s)))) ; add state to path
    (cond
      ((final-state s) nextpath) ; return full path with final state
      ((= depth 0) nil)          ; hit max depth
      (t (mult-dfs (succ-fn s) nextpath (- depth 1)))))) ; search next level

;; ID-DFS is the top-level function. It takes two arguments: an initial state
;; (S) and a search depth (DEPTH). ID-DFS performs a series of depth-first
;; iterations, starting from the given depth until a solution is found. It
;; returns the path from the initial state to the goal state. The very first
;; call to ID-DFS should use depth = 0.
(defun id-dfs (s depth)
  (or (single-dfs s () depth) (id-dfs s (+ depth 1))))
  ;; either limited dfs returned an answer, or continue trying with depth++

;; Function execution examples

;; Applying this operator would result in an invalid state, with more cannibals
;; than missionaries on the east side of the river.
;; (next-state '(3 3 t) 1 0) -> NIL

;; Applying this operator would result in one cannibal and zero missionaries on
;; the west side of the river, which is a legal operator. (note that NEXT-STATE
;; returns a LIST of successor states, even when there is only one successor)
;; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

;; SUCC-FN returns all of the legal states that can result from applying
;; operators to the current state.
;; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
;; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))
