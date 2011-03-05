; Memoization related tools

;{{{1 Auxiliary
(defmacro with-gensyms (syms &body body)
  "Bind the given names to gensym-generated symbols"
  `(let ,(mapcar (lambda (sym) `(,sym (gensym))) syms)
     ,@body))

(defmacro decorate-symbol (fn &key prefix suffix)
  "Generate a new symbol by adding a prefix/suffix to the given one"
  `(intern (nstring-upcase (concatenate 'string ,prefix (string ,fn) ,suffix))))

;{{{1 Main dish
(defmacro defmemo (fn args (&key (keygen '#'list) (test '#'equal))
                      &body body)
  "Define a memoized version of a function. By default we generate hash
keys by (list)ing arguments, and look them up using #'equal, which
should be a sane default for multi-args functions. We also define
'fname'-clear-cache as a method to reset the cache."
(with-gensyms (cache key val found-p)
              `(let ((,cache (make-hash-table :test ,test)))
                 (defun ,fn ,args
                   (let ((,key (funcall ,keygen ,@args)))
                     (multiple-value-bind (,val ,found-p) (gethash ,key ,cache)
                       (if ,found-p
                         ,val
                         (setf (gethash ,key ,cache)
                               ,@body)))))
                 (defun ,(decorate-symbol fn :suffix "-clear-cache") ()
                   (clrhash ,cache)))))


;{{{1 Test
(defmacro debug-eval (body)
  "Print the expansion of a macro, then issue the code"
  `(progn
     (pprint (macroexpand-1 ',body))
     ,body))

(debug-eval (defmemo fib (n) (:keygen (lambda (x) x)
                                            :test #'eql)
                     (if (<= n 1) 1
                       (+ (fib (- n 1)) (fib (- n 2))))))

; vim:fdm=marker
