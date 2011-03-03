(defparameter *words* (make-hash-table :size 10000
                                       :test #'equalp))

(defun read-text (path)
  (with-open-file (str path :direction :input)
    (let ((buf (make-array 100
                           :element-type 'character
                           :fill-pointer 0)))
      (do ((chr (read-char str nil :eof)
                (read-char str nil :eof)))
        ((eql chr :eof))

        (if (or (alpha-char-p chr) (char-equal chr #\'))
          (vector-push-extend chr buf)
          (progn
            (unless (zerop (length buf))
              (read-word (copy-seq buf))
              (setf (fill-pointer buf) 0))))))))

(let ((prev ""))
  (defun read-word (text)
    (let ((pair (assoc text (gethash prev *words*))))
      (if (null pair)
        (push (cons text 1) (gethash prev *words*))
        (incf (cdr pair))))
    (setf prev text)))

(defun get-text (len)
  (let ((buf ()))
    (dotimes (i len (nreverse buf))
      (push (select-word) buf))))

(let ((prev ""))
  (defun select-word ()
    (let* ((choices (gethash prev *words*))
           (i (random (reduce #'+ choices :key #'cdr))))
      (dolist (pair choices)
        (when (minusp (decf i (cdr pair)))
          (return (setf prev (car pair))))))))

(read-text (make-pathname :directory "/home/sic" :name "book.txt"))
(get-text 100)
