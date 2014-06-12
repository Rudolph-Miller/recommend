(defpackage :cl-redis
  (:use :common-lisp)
  (:export rlget
		   rget-keys
		   rllen
		   r-make-r-hash
		   r-make-hash
		   rget-all
		   rget-all-len
		   rrpush
		   rget
		   rset
		   rkey))

(in-package :cl-redis)
;;;position of ruby
(defvar *ruby-pos* "/Users/tomoya/.rbenv/shims/ruby")

(defun redis (command-list &optional (output t))
  (sb-ext:run-program "/usr/local/bin/redis-cli" command-list :output output))

(defun rget (key)
  (redis `("get" ,key)))

(defun rset (key val)
  (redis `("set" ,key ,val)))

(defun rkeys (key)
  (redis `("keys" ,key)))

(defun rrpush (key val)
  (redis `("rpush" ,key ,val)))

(defun redis-rb (command-list)
  (let ((s (make-string-output-stream)))
	(sb-ext:run-program
	  *ruby-pos*
	  (cons ".redis.rb" command-list)
	  :output s)
	(get-output-stream-string s)))

(defun rllen (key)
  (read-from-string (redis-rb `("llen" ,key))))

(defun split (key str)
  (declare (character key)
		   (string str))
  (let ((result nil))
	(declare (list result))
	(loop
	  for pos = (position key str)
	  do (setf result (cons (subseq str 0 pos) result))
	  while pos
	  do (setf str (subseq str (1+ pos))))
	(the list (nreverse result))))

(defun rlget (key)
  (let ((str (remove-if
			   #'(lambda (chr) (or
								 (eql #\" chr)
								 (eql #\[ chr)
								 (eql #\] chr)
								 (eql #\Space chr)
								 (eql #\NewLine chr)
								 (eql #\Tab chr)))
			   (redis-rb `("lget" ,key)))))
			(split #\, str)))

(defun rrpush (key val)
  (redis `("rpush" ,key ,val)))

(defun rget-keys (str)
  (let ((s (make-string-output-stream)))
	(redis `("keys" ,str) s)
	(remove-last (split #\NewLine (get-output-stream-string s)))))

(defun remove-last (lst)
  (let ((rev (reverse lst)))
	(nreverse (cdr rev))))

(defun rget-all ()
  (mapcar #'rlget
		  (rget-keys "*")))

(defun rget-all-len ()
  (mapcar #'(lambda (key)
			  (cons key (rllen key)))
		  (rget-keys "*")))

(defun r-make-hash (&optional key-list)
  (let ((result (make-hash-table :test #'equal))
		(keys (or key-list (rget-keys "*"))))
	(mapc #'(lambda (key)
			  (setf (gethash key result) (rlget key)))
		  keys)
	result))

(defun r-make-r-hash (&optional key-list)
  (let ((result (make-hash-table :test #'equal))
		(keys (or key-list (rget-keys "*"))))
	(mapc #'(lambda (key)
			  (mapc #'(lambda (item)
						(push key (gethash item result)))
					(rlget key)))
		  keys)
	(maphash #'(lambda (key val) (setf (gethash key result) (remove-duplicates val))) result)
	result))

