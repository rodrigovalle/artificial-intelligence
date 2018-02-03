;;;; TOP LEVEL FUNCTIONS

;; n is the number of variables
;; delta is the CNF
(defun sat? (n delta)
  (real-sat (range 1 n) (print delta)))

;; unassigned is a list of unassigned variables
;; cnf is a list of clauses
(defun real-sat (unassigned cnf &optional (model ()))
  (let ((head (car unassigned))
        (tail (cdr unassigned))
        (unit-literals (unit-propagate cnf)))
        ;(pure-literals (print (get-pure-literals unassigned cnf))))
    (cond
      ((null cnf) (append model unassigned))
      ((empty-clause? cnf) nil)
      (unit-literals (real-sat
                       (remove-all (absolute-all unit-literals) unassigned)
                       (assign-all unit-literals cnf)
                       (append unit-literals model)))
      ;(pure-literals (real-sat
                       ;(remove-all (absolute-all pure-literals) unassigned)
                       ;(assign-all pure-literals cnf)
                       ;(append pure-literals model)))
      (t (or (real-sat tail (assign head cnf) (cons head model))
             (real-sat tail (assign (- head) cnf) (cons (- head) model)))))))


;;;; SEARCH PRUNERS

;; given a CNF, returns a list of variables inside singleton clauses
(defun unit-propagate (cnf &optional (singletons ()))
  (let ((clause (car cnf))
        (tail (cdr cnf)))
    (cond
      ((null cnf) singletons)
      ((= (length clause) 1) (unit-propagate tail (cons (car clause) singletons)))
      (t (unit-propagate tail singletons)))))

;; given a CNF, returns any variable assignments that can be deduced from
;; literals that occur with only one polarity throughout the entire CNF
;(defun get-pure-literals (unassigned cnf)
;  (let ((pure-literal (check-pure (car unassigned) cnf))
;        (tail (cdr unassigned)))
;    (cond
;      ((null unassigned) nil)
;      (pure-literal (cons pure-literal (get-pure-literals tail cnf)))
;      (t (get-pure-literals tail cnf)))))

;(defun check-pure (n cnf)
;  (cond
;    ((null cnf)

;; given a literal n see if it or it's negated version exist in a clause
;; return the clause version
;(defun get-literal (n clause)
;  (let ((head (car clause))
;        (tail (cdr clause)))
;    (cond
;      ((null clause) nil)
;      ((= (absolute head) n) head)
;      (t (get-literal n tail)))))

;; check for an empty list in a CNF
(defun empty-clause? (cnf)
  (let ((clause (car cnf))
        (tail (cdr cnf)))
    (cond
      ((null cnf) nil)
      ((null clause) t)
      (t (empty-clause? tail)))))


;;;; LITERAL ASSIGNMENT UTILITIES

;; map the assign function to a list of literal assignments in sequence
;; except don't use the map function because we can't use that for some reason
(defun assign-all (n-list cnf)
  (let ((n (car n-list))
        (tail (cdr n-list)))
    (if (null n-list) cnf
      (assign-all tail (assign n cnf)))))

;; returns the simplified clause given by assigning n
(defun assign (n cnf &optional (simplified-cnf ()))
  (let ((simplified-clause (assign-clause n (car cnf)))
        (tail (cdr cnf)))
    (cond
      ((null cnf) simplified-cnf)
      ((eq simplified-clause t) (assign n tail simplified-cnf))
      (t (assign n tail (cons simplified-clause simplified-cnf))))))

;; returns true if the clause was satisfied by the assignment n
;; else returns the simplified clause
(defun assign-clause (n clause &optional (simplified ()))
  (let ((literal (car clause)) ; error
        (tail (cdr clause)))
    (cond
      ((null clause) simplified)
      ((= literal n) t)
      ((= literal (- n)) (assign-clause n tail simplified))
      (t (assign-clause n tail (cons literal simplified))))))


;;;; LIST UTILITIES

;; return a list with integer values from [start, stop]
(defun range (start stop)
  (if (= start stop) (list stop)
    (cons start (range (+ start 1) stop))))

;; remove the first element from a list l that matches e
(defun remove-first (e l)
  (let ((cur (car l))
        (tail (cdr l)))
    (cond
      ((null l) nil)
      ((= cur e) tail)
      (t (cons cur (remove-first e tail))))))

;; allow remove-first to take a list of elements
(defun remove-all (le l)
  (let ((cur (car le)))
    (if (null le) l
      (remove-all (cdr le) (remove-first cur l)))))


;;;; MATH UTILITIES

(defun absolute (n)
  (if (< n 0) (- n) n))

(defun absolute-all (l)
  (if (null l) nil
    (cons (absolute (car l)) (absolute-all (cdr l)))))
