;; " These are your father's parentheses
;;   Elegant weapons for a more... civilized age
;;     -- https://xkcd.com/297

(defun tree-contains (n tree)
  "Checks whether n appears in the ordered tree"
  (cond 
    ((numberp tree) (= n tree))
    ((= n (second tree)) t)
    ((< n (second tree)) (tree-contains n (first tree)))
    (t (tree-contains n (third tree)))))

(defun tree-max (tree)
  "Returns the maximum number appearing in the ordered tree"
  (if (numberp tree) tree (tree-max (third tree))))

(defun tree-order (tree)
  "Flattens a given ordered tree"
  (if (numberp tree) (list tree)
    (append
      (tree-order (first tree))
      (tree-order (second tree))
      (tree-order (third tree)))))

(defun sub-list (l start len)
  "Returns a sublist of 'l' starting at 'start' and having length 'len'"
  (cond
    ((= len 0) nil)
    ((= start 0) (cons (first l) (sub-list (rest l) 0 (- len 1))))
    (t (sub-list (rest l) (- start 1) len))))

(defun split-list (l)
  "Returns a list of two lists l1 and l2 such that
    - l is the result of appending l1 and l2
    - length of l2 minus length of l1 is 0 or 1"
  (let* ((len (length l))
         (halflen (/ len 2))
         (halflenm1 (/ (- len 1) 2)))
    (if (evenp len)
      (list (sub-list l 0 halflen) (sub-list l halflen halflen))
      (list (sub-list l 0 halflenm1) (sub-list l halflenm1 (+ halflenm1 1))))))

;; TODO: cant use (max x y), test
(defun btree-height (tree)
  "Given a binary tree, return the height of the tree"
  (if (atom tree) 0
    (t (+ 1 (max (btree-height (first tree)) (btree-height (second tree)))))))

(defun list2btree (leaves)
  "Given a nonempty list of atoms 'leaves', return a binary tree such that
    - The tree leaves are the elements of 'leaves'
    - For any internal (non-leaf) node in the tree, the number of leaves in its
      right branch minus the number of leaves in its left branch is 0 or 1"
  (let* ((split (split-list leaves))
         (l1 (first split))
         (l2 (second split)))
    (if (null l1)
      (car l2)
      (list (list2btree l1) (list2btree l2)))))

(defun btree2list (tree)
  "Given a binary tree, return a list of atoms
   Note: This function is the inverse of list2btree"
  (if (atom tree) (list tree)
    (append (btree2list (first tree)) (btree2list (second tree)))))

(defun is-same (e1 e2)
  "Given two exps e1 and e2 whose atoms are all numbers, check for equality"
  (cond
    ((and (numberp e1) (numberp e2)) (= e1 e2))
    ((and (null e1) (null e2)) t)
    ((and (listp e1) (listp e2))
      (let ((h1 (first e1)) (t1 (rest e1))
            (h2 (first e2)) (t2 (rest e2)))
        (and (is-same h1 h2) (is-same t1 t2))))
    (t nil)))

