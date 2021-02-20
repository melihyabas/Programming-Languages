(defun read-array (filename) ; Reads 2 numbers from giver filename
  (with-open-file (in filename) ; File opening
    (loop for num = (read in nil)
          until (null num)  ; Collects numbers in an array
          collect num)))

(defun writeList(filename lst) ; Writes given list into file
  (with-open-file (out filename :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "(") ; Add ( for showing it just like a list
    (defvar iter 0)
    (dolist (segment lst)
      (format out "~a" segment) ; Write character to the file
      (if (< iter (- (length lst) 1))
      (format out " "))
      (setq iter (+ 1 iter)))
	(format out ")"))) ; Additionally add a ) at the end of the file

(defun remv (n list) ; Remove an element from the list
  (remove-if (constantly t) list :start (1- n) :count 1))

(defun flattener (nested)
	(if (or (null nested) (atom nested))
		(progn
			(if (null nested)
				(return-from flattener nil)) ; Return empty
			(if (atom nested)
				(return-from flattener (list nested))) ; Return the flattened lsit
		)
		(progn ; Get first element and the other part, and combine them recursively
			(return-from flattener (append (flattener (car nested)) (flattener (cdr nested)))))))

(let ((arr (read-array "nested_list.txt")))
(setq arr (flattener arr))
(writeList "flattened_list.txt" arr))