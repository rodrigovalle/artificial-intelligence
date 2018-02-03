(defun mult-dfsl (tree depth)
  (if (null tree) nil
    (append (dfsl (first tree) depth)
            (mult-dfsl (rest tree) depth))))

(defun dfsl (tree depth)
  (cond
    ((null tree) nil)
    ((atom tree) (list tree))
    ((= depth 0) nil)
    (t (mult-dfsl tree (- depth 1)))))

(defun dfid (tree height)
  (append (dfid tree (- height 1)
