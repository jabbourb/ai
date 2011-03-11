(defconstant board-size 8)
(defparameter *cell-width* 5)

;{{{1 Low-level primitives
(defun cell-x (cell)
  (car cell))
(defun cell-y (cell)
  (cdr cell))

(defmacro with-gensyms (syms &body body)
  "Bind the given names to gensym-generated symbols"
  `(let ,(mapcar (lambda (sym) `(,sym (gensym))) syms)
     ,@body))

(defmacro do-cells((cell) &body body)
  (with-gensyms(x y)
    `(dotimes (,x board-size)
       (dotimes (,y board-size)
         (let ((,cell (cons ,x ,y)))
           ,@body)))))

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

;{{{1 Pretty printing
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

;{{{1 Functions
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

(defun valid-moves (board color &optional (single nil))
  (let ((moves nil))
    (do-cells (cell)
              (if (validate-move board cell color)
                (if single
                  (return-from valid-moves t)
                  (push cell moves))))
    moves))

(defun count-pieces(board)
  (let ((black 0)
        (white 0))
    (dotimes (x board-size cnt)
      (dotimes (y board-size)
        (case (board-cell-at board (cons x y))
          (:black (incf black))
          (:white (incf white)))))
    (values black white)))

(defun winner-is (board)
  (multiple-value-bind (black white) (count-pieces board)
    (cond ((> black white) :black)
          ((> white black) :white)
          (t nil))))

;{{{1 Strategies
(defmacro do-while-not (vars body test)
  `(do ,(mapcar #'list `,vars)
     (,test t)
     ,@body))

(defun human-player(board color)
  "Ask the player for the move to be taken"
  (board-print t board)
  (do-while-not (move)
                ((format t "~&~A> " color)
                 (finish-output nil)
                 (setf move (read)))
                (and (consp move)
                     (do-move board move color))))

(defun random-elt(lst)
  "Choose a random element from a list"
  (elt lst (random (length lst))))

(defun random-strategy(board color)
  "A bot that selects a random valid move at each turn"
  (do-move board
           (random-elt (valid-moves board color))
           color))

(defun maximize-strategy(board color)
  "A bot that selects the best move at each turn by maximizing the
  difference of pieces to the advantage of the player"
  (flet ((maximize-difference()
           (let ((best-move nil)
                 (best-score 0))
             (do-cells (cell)
                       (let ((ends (validate-move board cell color))
                             (score 0))
                         (dolist (end ends)
                           (incf score (length (intermediate-cells cell end))))
                         (if (> score best-score)
                           (setf best-move cell
                                 best-score score))))
             best-move)))

    (do-move board (maximize-difference) color)))

;{{{1 Main function
(defun check-and-play(board player color)
  (and (valid-moves board color t)
       (funcall player board color)))

(defmacro loop-while (test)
  `(do ()
     ((not ,test))
     nil))

; We can implement this as a function, since we want all the args
; evaluated in order
(defun greedy-or (&rest args)
  (some #'identity args))

(defun othello (p1 p2)
  "Main function"
  (let ((board (make-board)))
    (loop-while (greedy-or (check-and-play board p1 :black)
                           (check-and-play board p2 :white)))

    (board-print t board)
    (terpri)
    (princ "Game Over!")
    (let ((winner (winner-is board)))
      (if (null winner)
        (princ "It is a tie!")
        (format t "And the winner is... ~a!" winner))
      winner)))

;{{{1 Test run
(let ((white 0)
      (black 0))
  (dotimes (n 1000)
    (case (othello #'random-strategy #'maximize-strategy)
      (:white (incf white))
      (:black (incf black))))
  (format t "~&~a:~a~&~a:~a" :black black :white white))

; vim: fdm=marker
