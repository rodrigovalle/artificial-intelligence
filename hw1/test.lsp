(load "hw1.lsp")

(setq ordered-trees
      '(3
       (1 2 3)
       ((1 2 3) 7 8)
       ((1 2 3) 5 (6 8 (9 10 (11 12 13))))))

(defun test-tree-contains ()
  (let ((tests '((3 (t t t t))
                 (4 (nil nil nil nil))
                 (8 (nil nil t t))
                 (11 (nil nil nil t)))))
    (lambda (tree test)
      (let ((in (first test))
            (out (second test)))
        (equal
          (mapcar
            (lambda (tree-contains (first test) tree) (second test)))

(defun test-tree-max ()
  (let ((tests '(3 3 8 13)))
    (mapcar (lambda (tree ans) (= (tree-max tree) ans)) ordered-trees tests)))

(defun test-tree-order ()
  (let ((tests '((3)
                 (1 2 3)
                 (1 2 3 7 8)
                 (1 2 3 5 6 8 9 10 11 12 13))))
    (mapcar (lambda (tree ans) (equal (tree-order tree) ans)) tests)))

(defun test-sub-list ()
  (let ((tests '((0 3 (a b c))
                 (3 1 (d))
                 (2 0 (nil)))))
    (mapcar
      (lambda (test)
        (equal (sub-list '(a b c d) (first test) (second test)) (third test))
      tests))))

(defun test-split-list ()
  (let ((tests '((() (()()))
                 ((a b c d) ((a b)(c d)))
                 ((a b c d e) ((a b)(c d e)))
                 ((a b c d e f) ((a b c)(d e f))))))
    (mapcar
      (lambda (test)
        (equal (split-list (first test)) (second test)))
      tests)))
