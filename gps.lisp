; A General Problem Solver implementation.
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
  "The General Problem Solver
  - state: The initial state of the world, as a list of symbols
  - actions: All the actions for the problem at hand
  - goals: The states we aim to fulfill"
  ((state :initarg :initial
          :accessor GPS-state)
   (actions :initarg :actions
            :accessor GPS-actions)
   (goals :initarg :goals
          :accessor GPS-goals)))

(defmethod solve ((gps GPS))
  "Try to find a valid path in the states space of this GPS in order to get from the initial state to the goal, printing the actions' names as we go"
  (labels ((solve-single (goal)
                         (or (member goal (GPS-state gps))
                             (some (lambda (action)
                                     (and (achieves-goal-p action goal)
                                          (every #'solve-single (action-preconds action))
                                          (print (action-name action))))
                                   (GPS-actions gps)))))
    (every #'solve-single (GPS-goals gps))))

(defparameter *my-gps* (make-instance 'gps
                                      :initial '(unhappy hungry)
                                      :goals '(happy full)
                                      :actions (list
                                                 (make-action :name 'eat
                                                              :preconds '(hungry)
                                                              :add-list '(satiated)
                                                              :del-list '(hungry))
                                                 (make-action :name 'drink
                                                              :preconds '(satiated)
                                                              :add-list '(full)
                                                              :del-list '(satiated))
                                                 (make-action :name 'argue
                                                              :preconds '(normal)
                                                              :add-list '(unhappy)
                                                              :del-list '(normal))
                                                 (make-action :name 'play
                                                              :preconds '(unhappy)
                                                              :add-list '(normal)
                                                              :del-list '(unhappy))
                                                 (make-action :name 'chat
                                                              :preconds '(normal)
                                                              :add-list '(happy)
                                                              :del-list '(normal))
                                                 (make-action :name 'dance
                                                              :preconds '(normal)
                                                              :add-list '(happy)
                                                              :del-list '(normal)))))

;Evaluate this to get different solutions.
;Not guaranteed to be a uniform distribution, but who cares :p
(setf (gps-actions *my-gps*) (sort (gps-actions *my-gps*) #'> :key (lambda (x) (random 1.0))))

(solve *my-gps*)
