(defconstant board-size 8)

(defun make-board()
  (make-array (list board-size board-size)
              :initial-element nil))

(defun cell-x (cell)
  (car cell))

(defun cell-y (cell)
  (cdr cell))

(defun board-cell-at (board pos)
  (aref board (cell-x pos) (cell-y pos)))
(defun (setf board-cell-at) (color board pos)
  (setf (aref board (cell-x pos) (cell-y pos)) color))

(defun horizontal-p (start end)
  (eql (cell-y start) (cell-y end)))

(defun vertical-p (start end)
  (eql (cell-x start) (cell-x end)))

(defun color-inv (color)
  (case color
    (:white :black)
    (:black :white)
    (t nil)))

(defun range (a b)
  "Returns numbers between a and b, exclusive"
  (if (<= a b)
    (loop for x from (1+ a) to (1- b) collect x)
    (range b a)))

(defun intermediate-cells (start end)
  (cond ((horizontal-p start end)
         (mapcar (lambda (x) (cons x (cell-y start))) (range (cell-x start) (cell-x end))))
        ((vertical-p start end)
         (mapcar (lambda (y) (cons (cell-x start) y)) (range (cell-y start) (cell-y end))))
        (t
          (mapcar #'cons (range (cell-x start) (cell-x end)) (range (cell-y start) (cell-y end))))))

(defun flip-cells (board start end color)
  (dolist (c (intermediate-cells start end) board)
    (if (not (null (board-cell-at board c)))
      (setf (board-cell-at board c) color))))

(defun valid-move (board pos color)
  nil)

(defparameter *board* (make-board))
(setf (board-cell-at *board* '(2 . 2)) :white)
(flip-cells *board* '(1 . 1) '(5 . 5) :black)
