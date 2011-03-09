(defconstant board-size 8)
(defparameter *cell-width* 5)

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

(defun cell-string (color)
  (case color
    (:black "B")
    (:white "W")
    (t "")))

(defun board-print (stream board)
  (dotimes (x board-size)
    (format stream " ~v:@<~d~>" *cell-width* x))
  (let ((line-sep (make-string (* board-size (1+ *cell-width*))
                               :initial-element #\-)))
    (dotimes (y board-size)
      (format stream "~& ~A~&~d|" line-sep y)
      (dotimes (x board-size)
        (format stream "~v:@<~a~>|" *cell-width*
                (cell-string (board-cell-at board (cons x y))))))))

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
    (labels ((valid-move-incr(p step-x step-y acc)
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

(defun winner-is (board)
  (labels ((count-pieces(color)
             (let ((cnt 0))
               (dotimes (x board-size cnt)
                 (dotimes (y board-size)
                   (when (eql (board-cell-at board (cons x y)) color)
                     (incf cnt)))))))

    (let ((diff (- (count-pieces :black) (count-pieces :white))))
      (cond ((plusp diff) :black)
            ((minusp diff) :white)
            (t nil)))))

(defmacro while (test &body body)
  `(do ()
     ((not ,test))
     ,@body))

(defmacro do-while-not (vars body test)
  `(do ,(mapcar #'list `,vars)
     (,test)
     ,@body))

(defun othello ()
  (let ((board (make-board)))
    (labels ((single-turn(color)
               (when (has-valid-move board color)
                 (board-print t board)
                 (do-while-not (move)
                               ((format t "~&~A> " color)
                                (finish-output nil)
                                (setf move (read)))
                               (and (consp move)
                                    (do-move board move color))))))

      (while (not (is-game-over board))
             (single-turn :black)
             (single-turn :white))

      (princ "Game Over!")
      (let ((winner (winner-is board)))
        (if (null winner)
          (princ "It is a tie!")
          (format t "And the winner is... ~a!" winner))))))


(othello)
