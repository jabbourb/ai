(defpackage :ai.common
  (:use :common-lisp)
  (:export :alpha-beta-searcher))
(in-package :ai.common)

(defmacro select-alpha-beta (a-or-b comp-new-old)
  "Auxiliary function for alpha-beta-searcher (we need alpha/beta as a symbol, hence the macro)"
  `(dolist (move moves ,a-or-b)
     (let ((new-a-or-b (cons move (cdr (alpha-beta-turn (funcall do-move node move move-player)
                                                        orig-player (funcall opponent move-player)
                                                        (1- ply) alpha beta)))))
       (if (,comp-new-old (cdr new-a-or-b) (cdr ,a-or-b))
           (setf ,a-or-b new-a-or-b)))
     (if (<= (cdr beta) (cdr alpha)) (return ,a-or-b))))

(defun alpha-beta-searcher (ply opponent move-generator do-move score)
  "Perform a tree search with alpha-beta pruning.  This is basically a
minimax algorithm with improved performance: knowing that along
another path rooted at the same parent we can get as good as alpha and
as low as beta, we walk through the rest of the tree skipping branches
that can't lower beta or improve alpha.

@ply: the recursion depth
@opponent: a function that takes a player, and returns the opposite
player
@move-generator: a function that takes a node and a player, and
computes all possible moves for that player
@do-move: a function that takes a node, a move and a player, and
returns the child obtained by playing that move. It is up to that
function to make a copy of the node if it modifies it.
@score: a scoring function that takes a node and a player, and
computes the node's score for that player

@return: a function that takes a root node and a player, and computes
the best move for that player."

  (labels ((alpha-beta-turn (node orig-player move-player ply alpha beta)
             "*Note* internally, alpha and beta are stored as conses,
 with the car representing the selected child move, and the cdr
 storing the actual alpha/beta value."
             (let ((moves nil))
               (cond ((or (zerop ply)
                          (null (setf moves (funcall move-generator node move-player))))
                                        ;we select no move at a leaf
                      (cons nil (funcall score node orig-player)))
                     ((eql orig-player move-player)
                                        ;select highest alpha
                      (select-alpha-beta alpha >))
                     (t
                                        ;or lowest beta
                      (select-alpha-beta beta <))))))

    (lambda (root player)
      (car (alpha-beta-turn root player player ply
                            (cons nil most-negative-fixnum)
                            (cons nil most-positive-fixnum))))))
