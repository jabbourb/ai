;{{{1 Debugging parameters
(defparameter *dbg-groups* nil)

(defun dbg (group fmt &rest params)
  (when (member group *dbg-groups*)
    (apply #'format *debug-io* fmt params)))
(defun dbg-toggle (group)
  (if (member group *dbg-groups*)
    (setf *dbg-groups* (remove group *dbg-groups*))
    (push group *dbg-groups*)))

(dbg-toggle :search)

;{{{1 Generic functions
(defun tree-search (state goal-p successors combiner)
  "Starting at the given state, traverse the states tree according to
  successor and combiner, looking for one that satisfies goal-p"
  (labels ((tree-search-aux (states)
                            (dbg :search "~&Searching ~a" states)
                            (cond ((null states) nil)
                                  ((funcall goal-p (first states)) (first states))
                                  (t (tree-search-aux
                                       (funcall combiner
                                                (funcall successors (first states))
                                                (rest states)))))))
    (tree-search-aux (list state))))

;{{{1 Specializations
(defun depth-first-search (state goal-p successors)
  (tree-search state goal-p successors #'append))
(defun breadth-first-search (state goal-p successors)
  (tree-search state goal-p successors (lambda (x y) (append y x))))


;{{{1 Test
(defun binary-tree (x) (list (* 2 x) (1+ (* 2 x))))
(defun is (value) #'(lambda (x) (eql x value)))

(depth-first-search 1 (is 1) #'binary-tree)
(breadth-first-search 1 (is 12) #'binary-tree)

; vim: fdm=marker
