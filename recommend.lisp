(load "redis")
(defpackage :cl-recommend
	(:use :common-lisp
				:cl-redis))

(in-package :cl-recommend)

;;;from redis -> (list (tag-hash . hash-table) (token-hash . hash-table))
(defun load-redis ()
	(let ((tag-hash (cl-redis:r-make-hash))
				(token-hash (cl-redis:r-make-r-hash)))
		`((tag-hash . ,tag-hash) (token-hash . ,token-hash))))

;;;token token-hash -> tags list
(defun get-token-tags (token token-hash)
	(gethash token token-hash))

;;;tag tag-hash -> tokens list
(defun get-tag-tokens (tag tag-hash)
	(gethash tag tag-hash))

;;;token -> same category tokens list
(defun get-same-category-tokens (token &key tag-hash token-hash)
	(mapcar 
		#'(lambda (tag)
				(cons tag (remove-same-token token (get-tag-tokens tag tag-hash))))
		(get-token-tags token token-hash)))

;;;remove same token from list
(defun remove-same-token (tag lst)
	(remove-if 
		#'(lambda (item)
				(equal item tag))
		lst))

(defun print-hash (hash)
	(maphash #'(lambda (key val)
							 (format t "~a: ~a~%" key val))
					 hash))

(defun expand (token &key tag-hash token-hash)
	(let ((same-tokens (get-same-category-tokens
											 token 
											 :tag-hash tag-hash :token-hash token-hash))
				(tags (make-hash-table :test #'equal)))
		(mapc #'(lambda (lst)
					(mapc #'(lambda (to)
								(mapc #'(lambda (tag)
											(let ((score (gethash tag tags)))
												(if score
													(incf (gethash tag tags))
													(setf (gethash tag tags) 1))))
									(get-token-tags to token-hash)))
						(cdr lst)))
			same-tokens)
		(sort-hash tags)))

(defun sort-hash (hash)
	(let ((lst))
		(maphash 
			#'(lambda (key val)
					(push (cons key val) lst))
			hash)
		(sort lst #'> :key #'cdr )))

(print
	(expand
		"3"
		:tag-hash (cdr (assoc 'tag-hash (load-redis)))
		:token-hash (cdr (assoc 'token-hash (load-redis)))))
