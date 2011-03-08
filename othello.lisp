(defconstant board-size 8)

(defun cell-x (cell)
  (car cell))
(defun cell-y (cell)
  (cdr cell))

(defun board-cell-at (board pos)
  (aref board (cell-x pos) (cell-y pos)))
(defun (setf board-cell-at) (color board pos)
  (setf (aref board (cell-x pos) (cell-y pos)) color))

(defun make-board()
  (let ((board (make-array (list board-size board-size)
                           :initial-element nil)))
    (setf (board-cell-at board '(3 . 3)) :white)
    (setf (board-cell-at board '(4 . 4)) :white)
    (setf (board-cell-at board '(3 . 4)) :black)
    (setf (board-cell-at board '(4 . 3)) :black)
    board))

(defun horizontal-p (start end)
  "Tests if both cells are in the same row"
  (eql (cell-y start) (cell-y end)))

(defun vertical-p (start end)
  "Tests if both cells are in the same column"
  (eql (cell-x start) (cell-x end)))

(defun color-inv (color)
  "Returns the opposite color"
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
  "Returns the list of cells between start and end, exclusive"
  (cond ((horizontal-p start end)
         (mapcar (lambda (x) (cons x (cell-y start))) (range (cell-x start) (cell-x end))))
        ((vertical-p start end)
         (mapcar (lambda (y) (cons (cell-x start) y)) (range (cell-y start) (cell-y end))))
        (t
          (mapcar #'cons (range (cell-x start) (cell-x end)) (range (cell-y start) (cell-y end))))))

(defun flip-cells (board start end color)
  "Flip board cells between start and end so that they match the specified color"
  (dolist (c (intermediate-cells start end) board)
    (if (not (null (board-cell-at board c)))
      (setf (board-cell-at board c) color))))

(defun in-bounds (x y)
  "Check if coordinates are inside the board"
  (and (< -1 x board-size)
       (< -1 y board-size)))

(defun cartesian-self (lst)
  "Compute the cartesian product of a set with itself"
  (mapcan (lambda (x) (mapcar (lambda (y) (cons x y)) lst)) lst))

(defun validate-move (board pos color)
  "If the move is valid, return the list of cells that close the bracket in all directions.
  Else return nil."
  (let ((!color (color-inv color)))
    (labels ((valid-move-incr (p step-x step-y acc)
                              (let ((next-x (funcall step-x (cell-x p)))
                                    (next-y (funcall step-y (cell-y p))))
                                (when (in-bounds next-x next-y)
                                  (let* ((next-cell (cons next-x next-y))
                                         (next-color (board-cell-at board next-cell)))
                                    (or
                                      (and (eql next-color color)
                                           (plusp acc) ;At least one piece of the opposite color
                                           next-cell)
                                      (and (eql next-color !color)
                                           (valid-move-incr next-cell step-x step-y (1+ acc)))))))))

      (and (null (board-cell-at board pos))
           (remove nil (mapcar (lambda (steps) (valid-move-incr pos (car steps) (cdr steps) 0))
                               (remove (cons #'identity #'identity)
                                       (cartesian-self (list #'identity
                                                             (lambda (x) (1+ x))
                                                             (lambda (x) (1- x))))
                                       :test #'equal)))))))

(defun do-move (board pos color)
  "Place a piece on the board at the specified position, flipping other
  pieces as required. Returns the board if the move was executed, nil if
  it was invalid."
  (let ((ends (validate-move board pos color)))
    (and (dolist (end ends ends)
           (flip-cells board pos end color))
         (setf (board-cell-at board pos) color)
         board)))

(defun has-valid-move (board color)
  (dotimes (x board-size)
    (dotimes (y board-size)
      (if (validate-move board (cons x y) color)
        (return-from has-valid-move t))))
  nil)

(defun is-game-over (board)
  (not (or (has-valid-move board :black)
           (has-valid-move board :white))))

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro loop-while-not (test)
  `(do ()
     (,test)))

(defun othello ()
  (let ((board (make-board))
        (move nil))
    (labels ((single-turn(color)
               (format t "~A~&~A> " board color)
               (finish-output nil)
               (if (has-valid-move board color)
                 (and
                   (setf move (read))
                   (consp move)
                   (do-move board move color))
                 t)))

      (while (not (is-game-over board))
             (loop-while-not (single-turn :black))
             (loop-while-not (single-turn :white)))
      (princ "Game Over!"))))

;(defparameter *board* (make-board))
;(setf (board-cell-at *board* '(2 . 2)) :white)
;(setf (board-cell-at *board* '(1 . 1)) :black)
;(pprint (do-move *board* '(3 . 3) :black))

(othello)
