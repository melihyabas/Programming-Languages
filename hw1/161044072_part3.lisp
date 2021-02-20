(defun giveNum (str) ; Gets an str and converts it to an integer number

	(defvar cntr 0)
	(let ((normalstr  nil)) 
		(loop while (< cntr (length str))

			do(setq normalstr (concatenate 'string normalstr (string (nth cntr str)))) ; Concanate strings
			do(setq cntr (+ 1 cntr)) 
		)
		(setq cntr 0)
		(setq normalstr (parse-integer normalstr))
		(return-from giveNum normalstr)))

(defun collatz (number str)
; While number is not 1, collatz sequence will move on
		(format str "~a: ~a " number number)
		(loop while (> number 1) 
			if(= (mod number 2) 0)
				do(setq number (/ number 2))
				else do(setq number (+ 1 (* 3 number )))
			do(format str "~a " number))

		(format str "~%~%"))

(defun converter (stringNums)	

(with-open-file (str "collatz_outputs.txt" ; Open output file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)	
	(defvar counter 0)
	(defvar aNum '())
	(defvar comb '())
			(defvar emptystr "")

	(loop while (< counter (length stringNums))
		if( not (or (eq #\space (nth counter stringNums)) (eq counter (- (length stringNums) 1))))
			do(push (nth counter stringNums) aNum) ;Gets each number using space character

			else do(setq aNum (reverse aNum))

				(loop for i from 0 to (- (length aNum) 1)
					do(setq emptystr (concatenate 'string emptystr (string (nth i aNum)) ))
				)
				(collatz (parse-integer emptystr) str) ; Calls Collats to create Collatz Sequence of the number
				(setq emptystr nil)

-				(setq aNum nil)

		do(setq counter (+ counter 1)))))

(defvar numArr '())
(setq fp (open "integer_inputs.txt" :direction :input))  ; now read the file
(defvar numC 0)
(defvar c #\x)
	(setq i 0)
	(loop while  (and (< numC 5) (not (eq c #\newline))) 
		do(setq i (+ i 1))
		do(setq c (read-char fp))
		if(eq c #\space) ;to determine the number of words in the paragraph
			do(setq numC (+ numC 1))
		do(push c numArr))

(close fp) 

(setq numArr (reverse numArr))

(converter numArr)