(defun getSubstring (needle haystack &key (test 'char=))
  (search (string needle)
          (string haystack)
          :test test))

(setf predicates '())
(setf facts '())
(setf queries '())

(setf predicates2 nil)
(setf facts2 '())
(setf queries2 '())

(defun changeXIntoString (str word)

	(setf newStr nil)

	(loop for i from 0 to (- (length str) 1)
		do()
		(if (and (>= (char-code (char str i)) 65) (<= (char-code (char str i )) 90))
			(progn
			
				(setf newStr (concatenate 'string (subseq str 0 i) word))
				(loop while (or (and (>= (char-code (char str i)) 65) (<= (char-code (char str i )) 90))
					(and (>= (char-code (char str i)) 97) (<= (char-code (char str i )) 122)))
					do(setf i (+ i 1))

				)
				(setf newStr (concatenate 'string newStr (subseq str i)))
			)
		)
	)

	(if (eq nil (getSubstring "))" newStr))
		(setf newStr (concatenate 'string newStr ")"))
	)
	newStr
)

(defun getQueryContent (query)

	(setf content (subseq query 1))
	
	(setf content (subseq content ( + (getSubstring "(" content) 1)(- (getSubstring ")" content) 1) ) ) 
	
	content
)

(defun check (query)

	(setf bool 0)
	(loop for j from 0 to (- (length facts2) 1)
			do()		
			(if (string= query (nth j facts2))
				(progn
					(return-from check T)
				)
			)
	)

	(setf queryContent (getQueryContent query))
	(loop for i from 0 to (- (length predicates2) 1)
		do()
		(if (string= query (changeXIntoString (nth 0 (nth i predicates2)) content))
			(progn
				(loop for j from 0 to ( - (length (nth 1 (nth i predicates2))) 1)
					do()
					(loop for k from 0 to ( - (length facts2) 1)
						do()
						(if (string= (nth k facts2) (changeXIntoString (nth j (nth 1 (nth i predicates2))) queryContent))
							(progn
								(setq bool (+ 1 bool))
							)
						)						
					)
				)
			)
		)
	)
	(if (eq bool (length predicates2))
		(return-from check T)
		(return-from check nil)
	)

)

(defun parse (strOut)

(let ((in (open "input.txt" :if-does-not-exist nil)))
  (when in
    (loop for line = (read-line in nil)
        while line do 
        	(print line)
        	(format strOut (concatenate 'string line "~%"))

        	(setf line (remove #\space line))
          	(setf line (remove #\“ line))
          	(setf line (remove #\” line))
          	(setf line (remove #\" line))
      	
	        (if (and (eq nil (getSubstring "(()" line)) (eq nil (getSubstring "())" line)) (not (eq nil (getSubstring "((" line))))
	        	(progn
	        		(push line predicates)
	        	)
	        	(progn
	        		(if (not (eq nil (getSubstring "())" line)))
	        			(push line facts)
	        		)
	        		(if (not (eq nil (getSubstring "(()" line)))
	               		(push line queries)
	        		)
	        	)
	        )
	)

	(loop for i from 0 to (- (length facts) 1)
		do(push (substring (nth i facts) 1 (- (length (nth i facts)) 3)) facts2)
	)
	
	(loop for i from 0 to (- (length queries) 1)
		do(push (substring (nth i queries) 3 (- (length (nth i queries)) 1)) queries2)
	)

	(loop for i from 0 to (- (length predicates) 1)
		do(setf flist nil)
		do(setf factslist nil)
		do(setf f  (subseq (nth i predicates) 1 (+ (getSubstring "))" (nth i predicates) ) 2)))
		do(setf index (+ (getSubstring "))" (nth i predicates) ) 2))
		do(push f flist)

		do(setf str (subseq (nth i predicates)(+ (getSubstring "))" (nth i predicates) ) 2) (- (length (nth i predicates)) 1)))
		do(loop
			do( )
			(setf tempStr (subseq str (getSubstring "(" str) (+ (getSubstring "))" str) 2) ))

			(if (not (eq nil (getSubstring "((" str)))
				(setf tempStr (subseq tempStr 1 (- (length tempStr) 1)))
			)

			(push tempStr factslist)
			(setf factslist (reverse factslist))

			(if (< (+ (getSubstring "))" str) 2) (- (length str) 1))
				(setf str (subseq str (+ (getSubstring "))" str) 2)   ))
				(return)
			)


		)
		do(push factslist flist)
		do(setf flist (reverse flist))
		do(push flist predicates2)
		do(setf predicates2 (reverse predicates2))
	)

    (close in)))

	(getQueryContent (nth 0 queries2))

	nil
)

(with-open-file (strOut "output.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
(format strOut "Given list:~%")



(format t "~%Given list:~%~%")

(parse strOut)
(format strOut "~%~%~%Queries:~%")

(format t "~%~%~%Queries:~%")
(loop for i from 0 to (- (length queries2) 1)
	do(format t "~%?~a: ~a" (nth i queries2) (check (nth i queries2)))
	do(format strOut "~%?~a: ~a" (nth i queries2) (check (nth i queries2)))

))