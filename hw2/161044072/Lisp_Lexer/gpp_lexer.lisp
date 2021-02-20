;;########## GPP LEXER ########## Cse341 - Hw1 - Part2 ########## Melih Yabaş - 161044072 ##########;;

;; How to run ?
;; Read-eval-print-loop mode: clisp gpp_lexer.lisp
;; Read from a file:  		  clisp gpp_lexer.lisp filename

(defvar Keywords '( "and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false"))
(defvar TokenKW '("KW_AND" "KW_OR" "KW_NOT" "KW_EQUAL" "KW_LESS" "KW_NIL" "KW_LIST" "KW_APPEND" "KW_CONCAT" "KW_SET" "KW_DEFFUN" "KW_FOR" "KW_IF" "KW_EXIT" "KW_LOAD" "KW_DISP" "KW_TRUE" "KW_FALSE"))
(defvar Operators '( "+" "-" "/" "**" "*" "(" ")" "\"" "\"" ","))
(defvar TokenOP '("OP_PLUS" "OP_MINUS" "OP_DIV" "OP_DBLMULT" "OP_MULT" "OP_OP" "OP_CP" "OP_OC" "OP_CC" "OP_COMMA"))

(defvar Space '( "\n" "\t" " "))
(defvar Allowed '( "(" ")" "\""))	; These characters does not need any space
(defvar opoc 0)

(defvar start 1) ; Condition for tokenizing
(defvar subW) ; Subword


(defun gppinterpreter (&optional filename) ; Starts program in both way
(with-open-file (strTkn "parsed_lisp.txt" ; Open output file
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
	(let (( c 0) (words) )
	(if (eq filename nil)	; Read eval print loop
		(progn
			(loop while (eq c 0)
				do(setq line (read-line))
				if(eq 0 (length line))
					do(return)
				  
				do(setq words (splitTheList line))
				(loop for x in words

					if  (eq start 1)
						do(findToken x strTkn)
					else do(return)
				)
				(setq start 1)
			)
		)
		(progn	; Read from a file
			(let ((in (open (nth 0 filename) :if-does-not-exist nil)))
			  (when in
			    (loop for line = (read-line in nil)
			        while line   
								do(setq words (splitTheList line))
								(loop for x in words
									if  (eq start 1)
										do(findToken x strTkn)
									else do(return)


								)
					do(setq start 1)
			    )


			    (close in)))
		)
	)
)))

(defun findToken(word strTkn)
; This function finds the token of input or gives an error message if it cannot be tokenized.

	(loop for i from 0 to (- (length word) 1)
		do(loop for j from (+ i 1) to  (length word) 
		do(progn
			(setq subW (substring word i j))
		
			;Check whether given input is an operator or not
			(if (eq start 1)
				(progn
					(if (checkWordinList subW Operators)
						(progn
							(if  (not (eq j  (length word) ))
								(progn
									(if (eq #\- (aref subW 0)) ; - could be a sign for a negative value
										(progn
											(if (and (not (isValue (string(aref word j))))(not (checkWordinList subW Allowed) )(eq nil (checkWordinList (aref word j) Allowed)))
												(progn
													(format strTkn "Error: \"~a\" cannot be tokenized~%" word)
													(setq j (length word))
													(setq i (length word))
													(return)
												)
												(progn
													(if (not (isValue (string(aref word j))))
														(progn
															(if (string= subW "\"")
																(progn ; This in an operator
																	
																	(format strTkn "~a~%" (nth (+ (mod opoc 2)(position subW Operators :test #'string=)) TokenOP))

																	(setq opoc (+ 1 opoc))								
																)
																(progn
																	(format strTkn "~a~%" (nth (position subW Operators :test #'string=) TokenOP))

																)
															)
														)
													)
												)
											)
										)
										(progn
										(if (eq #\* (aref subW 0))
											(progn ; '*' character can be exist for 2 times : '**'
												(if (and (not (eq (aref word j) #\*))(eq nil (checkWordinList (aref word j) Allowed)))
													(progn
														(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

														(setq j (length word))
														(setq i (length word))
														(return)
													)
												)
												(if (and (not (eq (aref word j) #\*)) (checkWordinList (aref word j) Allowed))
													(progn
														(format strTkn "OP_MULT~%")
												
													)
												)
												(if (and (>= (length word) (+ i 2))  (eq #\* (aref word (+ i 1))))
													(progn
														(if (eq (length word) (+ i 2))
															(progn
																(format strTkn "OP_DBLMULT~%")

																(setq i (+ i 1))
																(setq j (+ j 1))	
															)
														)


														(if (and (>= (length word) (+ i 3)) (not (checkWordinList (aref word (+ i 2)) Allowed)))
															(progn
																(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

																(setq j (length word))
																(setq i (length word))
																(return)
															)
														)
														(if (and (>= (length word) (+ i 3))  (checkWordinList (aref word (+ i 2)) Allowed))
															(progn
																(format strTkn "OP_DBLMULT~%")
	
																(setq i (+ i 1))
																(setq j (+ j 1))
															)

														)

													)
												)

											)
											(progn	; After this token, only allowed characters can be the next character
												(if (and (not (checkWordinList subW Allowed) )(eq nil (checkWordinList (aref word j) Allowed)))
													(progn
														(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

														(setq j (length word))
														(setq i (length word))
														(return)
													)
													(if (string= subW "\"")
														(progn
															(format strTkn "~a~%" (nth (+ (mod opoc 2)(position subW Operators :test #'string=)) TokenOP))

															(setq opoc (+ 1 opoc))								
														)
														(progn
															(format strTkn "~a~%" (nth (position subW Operators :test #'string=) TokenOP))
					
														)
													)


												)
											)


										)

										)	
									)
								)
								(progn
										(if (string= subW "\"")
											(progn	; quotes have 2 different  names
												(format strTkn "~a~%" (nth (+ (mod opoc 2)(position subW Operators :test #'string=)) TokenOP))
												(setq opoc (+ 1 opoc))								
											)
											(progn
												(format strTkn "~a~%" (nth (position subW Operators :test #'string=) TokenOP))
									
											)
										)
								)
							)	
						)
					)
				)
			)

			;Check whether given input is Keyword or not
			(if (eq start 1)
				(progn
					(if (checkWordinList subW Keywords)
						(progn
							(if   (not (eq j  (length word) ))
								(progn	; After this token, only allowed tokens could be next tokens
									(if (and (eq nil (checkWordinList (aref word j) Allowed)) (checkWordinList (aref word j) Operators))
										(progn
											(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

											(setq j (length word))
											(setq i (length word))
											(return)
										)
										(progn	;This is a keyword
											(if  (eq T (checkWordinList (aref word j) Allowed))
												(progn
													(format strTkn "~a~%" (nth (position subW Keywords :test #'string=) TokenKW))	

													(setq i (+ i (- (length subW) 1)))
													(setq j (+ i 1))	
												)							
											)
										)
									)
								)
								(progn	;This is a keyword

										(format strTkn "~a~%" (nth (position subW Keywords :test #'string=) TokenKW))	
								
										(setq i (+ i (length subW)))
										(setq j (+ i 1))								
								)
							)	
						)
					)
				)
			)
			;Check whether given input is Value or not
			(if (eq start 1)
					(if (isValue subW)
						(progn
			
									(if (eq #\0 (aref subW 0))
										(progn  ; Zero cannot be the first digit of a number

													(let ((temp 1))
													(if (< j (length word))
														(progn ; After this token, only allowed tokens could be next tokens
															(loop while (and (not (eq j  (length word) )) (or (eq T (isLetter (string(aref word i)))) (eq T (isValue (string(aref word i))))))
																do(setq i (+ i 1))
																do(setq j (+ j 1))
																(if (and (eq temp 1)(not(checkWordinList (aref word i) Allowed)))
																	(progn
																		(format strTkn "Error: \"~a\" cannot be tokenized~%" word)
																	(setq j (length word))
																	(setq i (length word))
																	(return)	
																		(setq temp 0)										
																	)
																)												
															)
																(setq i (- i 1))
																(setq j (- j 1))
																(if (eq temp 1)
																	(progn ;This is a value
																		(format strTkn "VALUE~%")	

																	)
																)

														)
														(progn ;This is a value
															(format strTkn "VALUE~%")	

														)
													)
													)


										)
										(progn
											(if  (not (eq j  (length word) ))
															(progn
																(loop while (and (not (eq j  (length word) )) (eq T (isValue (string(aref word i)))))
																	do(setq i (+ i 1))
																	do(setq j (+ j 1))
																); After this token, only allowed tokens could be next tokens
															
																(if (and (eq nil (isValue (string(aref word i)))) (eq nil (checkWordinList (aref word i) Allowed)))
																	(progn	
																		(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

																		(setq j (length word))
																		(setq i (length word))
																		(return)
																	)
																	(progn ; This is a value
																		(format strTkn "VALUE~%")	
																	
																	)
																)
																(if (not(isValue (string (aref word i))))
																	(progn
																		(setq i (- i 1))
																		(setq j (- j 1))	
																	)
																)
						
															)
															(progn
																(if (and (> (length word) 1)(eq j (length word)) (isValue (string (aref word (- j 1)))))
																	(progn
																				(format strTkn "VALUE~%")	

																	)
																)
																(if (> (length word) 1)
																	(progn
																		(if (not (isValue (string (aref word (- j 1)))))
																			(progn
																				(format strTkn "VALUE~%")	

																			)
																		)
																	)
																	(progn
																		(format strTkn "VALUE~%")	

																	)
																)

															)
														)

										)


									)
			
								
							)
						)
					)
			)

			;Check whether given input is Comment or not
			(if (eq start 1)
				(progn
					(if (string= subW ";;")
						(progn
						(format strTkn "COMMENT~%")										
							(setq start 0)

							(setq j (length word))
							(setq i (length word))
							(return) ; After a comment token, all the next inputs are comment in the line
						)
					)
				)
			)

			;Check whether given input is Identifier or not
			(if (eq start 1)
				(progn
					(let ((temp j))
						(if (and (eq 1 (length subW)) (isLetter  subW ))
							(progn
								(loop while(and (< j (length word)) (or (isLetter (string (aref word (- j 1)))) (isValue (string (aref word (- j 1))))))
									do(setq j (+ j 1))
								)
								(setq subW (substring word i j ))

								(if (not (or (isLetter (string (aref subW (- (length subW) 1)))) (isValue (string (aref subW (- (length subW) 1))))))
									(progn
										(setq subW (substring word i (- j 1) ))
										(setq j (- j 1))
									)
								) 
								(setq i (- j 1))
								; An identifier is not a keyword, operator or a value.
								(if (not (checkWordinList subW Keywords))
									(progn
										(if (< j (length word))
											(progn
												(if (or (checkWordinList (aref word j) Allowed) (eq (aref word j) #\;))
													(progn
														(format strTkn "IDENTIFIER~%")										
													)
													(progn
														(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

														(setq j (length word))
														(setq i (length word))
														(return)
													)
												)
											)
											(progn
												(format strTkn "IDENTIFIER~%")										
											)
										)
									)
									(progn
										(setq j temp)
										(setq i (- j 1))
									)
								)


							)
						)
					)
				)
			)
			;Wrong Character
			(if (eq start 1)
				(progn
					(if (eq (length subW) 1)
						(progn ; If it is an unknown character, it cannot be tokenized.
							(if (not (or (string= (string #\tab) subW)(string= ";" subW)(isLetter subW) (isValue subW) (checkWordinList subW Operators)))
								(progn
									(format strTkn "Error: \"~a\" cannot be tokenized~%" word)

									(setq j (length word))
									(setq i (length word))
									(return)								
								)
							)
						)	
					)
					(if  (string= ";" subW) ;This could be a comment or unknown character
						(progn
							(let ((cm 0))
							(if (and (> i 0) (eq #\; (aref word (- i 1))))
								(progn
									(setq cm 1)
								)
							)
		
							(if (and (not (eq i (- (length word) 1) )) (eq #\; (aref word (+ i 1))))
								(progn
									(setq cm 1)
								)
									
							)
							(if (eq cm 0)
								(progn
									(format strTkn "Error: \"~a\" cannot be tokenized~%" word)	
									(setq j (length word))
									(setq i (length word))
									(return)	
								)
							)


						)
							
					))

					(if  (string= "_" subW) ; This must be an identifier or a wrong character
						(progn
							(let ((cm 0))
		
							(if (and (not (eq i (- (length word) 1) )) (or (isLetter (string(aref word (+ i 1)))) (isValue (string(aref word (+ i 1))))) )
								(progn
									(setq cm 1)
								)
									
							)
							(if (eq cm 0)
								(progn
									(format strTkn "Error: \"~a\" cannot be tokenized~%" word)
									(setq j (length word))
									(setq i (length word))
									(return)	
								)
							)


						)
							
					))
				)
			)	
		)
	)
)

(defun isLetter (chr) ; Check whether given input is a letter or not
	(return-from isLetter (or (and (<= (char-code (aref chr 0)) 90) (>= (char-code (aref chr 0)) 65)) (and (<= (char-code (aref chr 0)) 122) (>= (char-code (aref chr 0)) 97))) )
)

(defun isValue (val) ; Check whether given input is a value or not
	(loop for i from 0 to (- (length val) 1)
		if(or (< (char-code (aref val i)) 48) (> (char-code (aref val i)) 57))
			do(return-from isValue nil))
	(return-from isValue T))

(defun checkWordinList(strline aList) ; Checks whether given string is in the list or not
	(if (member strline aList :test #'string=)
		T
		nil
	))

(defun splitTheList (str) ;Splits a string into words of list

	(let ((splt '()) (subStr ))
	(loop for i from 0 to (- (length str) 1)		
		if(not (eq #\Space (aref str i)) )
			do(setf subStr (concatenate 'string subStr (string (aref str i))))
		if(and (eq #\Space (aref str i)) (not (eq nil subStr)) )
			do(push subStr splt)
			(setq subStr nil)
	)
	(push subStr splt)
	(reverse splt))
)

(gppinterpreter *args*) ; Call gpp interpreter with commandline argument