#!/usr/local/bin/sbcl --script

(defparameter *tape* nil)

(let ((id 0))
  (defun get_id ()
    (incf id)
	   id))

(defun hash-keys (hash-table)
  (loop for key being the hash-keys of hash-table collecting key))

(defun unique (list)
 (let ((hash (make-hash-table)))
   (loop for i in list
	 do (setf (gethash i hash) T))
   (hash-keys hash)))

(defstruct Num
  (value nil)
  (id (get_id))
  (fun #'+)
  (fungrads nil)
  (derivative 1)
  (parents nil))

(defmethod forward ((mynum Num) values)
  (multiple-value-bind (v f)
      (funcall (num-fun mynum) values)
    (setf (num-value  mynum) v)
    (setf (num-fungrads  mynum) f))
  (mapcar (lambda (othernum) (push mynum (num-parents othernum))) (unique values)))

(defmethod backwards ((mynum Num))
  (setf (num-derivative mynum)
	(loop for i in (num-parents mynum)
	      summing (* (gethash (num-id mynum) (num-fungrads i)) (num-derivative i)))))

(defun collect (list fun)
  (let ((hash (make-hash-table)))
    (loop for i in list
	  do (if (gethash (funcall fun i) hash)
	      (incf (gethash (funcall fun i) hash))
	      (setf (gethash (funcall fun i) hash) 1)))
    hash))

(defun _plus (rest)
  (values
   (loop for i in rest summing (num-value i))
   (collect rest (lambda (x) (num-id x)))))

(defun get_grad (hash who)
  (reduce (lambda (x y) (* x y))
   (loop for key being the hash-keys of hash
	 collect (if (eq (num-id key) (num-id who))
		     (* (gethash key hash) (expt (num-value key) (- (gethash key hash) 1)))
		     (expt (num-value key) (gethash key hash))))))
		     
(defun get_grads (nums)
  (let ((hash (make-hash-table))
	(collection (collect nums (lambda (x) x))))
    (loop for i in nums
	  do (setf (gethash (num-id i) hash) (get_grad collection i)))
    hash))
		
(defun _times (rest)
  (values
   (reduce (lambda (x y) (* x y)) (mapcar #'num-value rest))
   (get_grads rest)))

(defun _sigmoid (num)
  (let* ((num (car num))
	 (result (/ 1 (+ 1 (exp (* -1 (num-value num))))))
	 (hash (make-hash-table)))
    (values
     result
     (progn
       (setf (gethash (num-id num) hash) (* result (- 1 result)))
       hash))))

(defun apply_fun (fun rest)
  (setf *tape* (append (unique rest) *tape*))
  (let ((newnum (make-num :fun fun)))
    (forward newnum rest)
    newnum))

(defun plus (&rest rest)
  (apply_fun #'_plus rest))

(defun times (&rest rest)
  (apply_fun #'_times rest))

(defun sigmoid (&rest rest)
  (apply_fun #'_sigmoid rest))

(defun grad (&rest rest)
  (loop for i in *tape*
	do (backwards i))
  (setf *tape* nil)
  (mapcar (lambda (x) (setf (num-parents x) nil)) rest)
  (mapcar #'num-derivative rest))

(defun _parse (list)
  (loop for i in list
	collect (if (listp i)
		    (_parse i)
		    (cond
		      ((numberp i) `(make-num :value ,i))
		      ((eq i '+) 'plus)
		      ((eq i '*) 'times)
		      (T i)))))

(defmacro record (list)
  `(num-value ,(_parse list)))

(defparameter *three* (make-num :value 3))
(defparameter *five* (make-num :value 5))

(print
 (record
  (sigmoid (+ 42  (* *three* *three* *five* -1) *five*))))

(print (grad *three* *five*))
(terpri)