; A General Problem Solver implementation have-money.
; A GPS tries to find a path between two states in the problem's world
; using a means-ends analysis

(defstruct action
  "An action that can change the actual state of the world.
  - preconds: a set of states that must be satisfied for the action to be applicable
  - add-list: states added when the action is executed
  - del-list: states deleted when the action is executed"
  name preconds add-list del-list)

(defun achieves-goal-p (action goal)
  "Test if executing an action will yield some goal"
  (member goal (action-add-list action)))

(defclass GPS ()
  ((state :initarg :initial
          :accessor GPS-state)
   (actions :initarg :actions
            :accessor GPS-actions)
   (goals :initarg :goals
          :accessor GPS-goals))
  (:documentation
    "The General Problem Solver
    - state: The initial state of the world, as a list of symbols
    - actions: All the actions for the problem at hand
    - goals: The states we aim to fulfill"))

(defun apply-action (state action)
  (union (set-difference state (action-del-list action))
         (action-add-list action)))

(defmethod apply-actions ((gps GPS) actions)
  (dolist (action actions t)
    (print (action-name action))
    (setf (gps-state gps) (apply-action (gps-state gps) action))))

(defmethod solve ((gps GPS))
  "Try to find a valid path in the states space of this GPS in order to get from the initial state to the goal, printing the actions' names as we go"
  ;; No actual modification is made until we are sure the goals are
  ;; achievable; hence the following closures
  (let ((state (gps-state gps))
        (actions ()))
    ;; Start at the goal and try to rewind the chain of actions leading
    ;; to it, until the preconditions are met.
    (labels ((solve-single (goal)
                           (or (member goal state)
                               (some (lambda (action)
                                       (and (achieves-goal-p action goal)
                                            (every #'solve-single (action-preconds action))
                                            (setf state (apply-action state action))
                                            (push action actions)))
                                     (GPS-actions gps)))))

      ;; Solve all the goals sequentially
      (and (every #'solve-single (GPS-goals gps))
           ;;; and make sure that they weren't clobbered in subsequent ones
           (subsetp (gps-goals gps) state)
           ;;; now if all went smoothly, replay the sequence of actions
           (apply-actions gps (nreverse actions))))))


(defparameter *actions*
  (list
    (make-action :name 'drive-son-to-school
                 :preconds '(son-at-home car-works)
                 :add-list '(son-at-school)
                 :del-list '(son-at-home))
    (make-action :name 'shop-installs-battery
                 :preconds '(car-needs-battery shop-knows-problem shop-has-money)
                 :add-list '(car-works))
    (make-action :name 'tell-shop-problem
                 :preconds '(in-communication-with-shop)
                 :add-list '(shop-knows-problem))
    (make-action :name 'telephone-shop
                 :preconds '(know-phone-number)
                 :add-list '(in-communication-with-shop))
    (make-action :name 'look-up-number
                 :preconds '(have-phone-book)
                 :add-list '(know-phone-number))
    (make-action :name 'give-shop-money
                 :preconds '(have-money)
                 :add-list '(shop-has-money)
                 :del-list '(have-money))))

(defparameter *my-gps* (make-instance 'gps
                                      :initial '(son-at-home car-needs-battery have-money have-phone-book)
                                      :goals '(son-at-school have-money)
                                      :actions *actions*))

;Evaluate this to get different solutions.
;Not guaranteed to be a uniform distribution, but who cares :p
(setf (gps-actions *my-gps*) (sort (gps-actions *my-gps*) #'> :key (lambda (x) (declare (ignore x)) (random 1.0))))

(if (solve *my-gps*)
  (print 'solved)
  (print 'failed))
