(defun is-prime (number) ; Checks whether a number is prime or not
	(if (< number 2)
		(return-from is-prime nil)) ; Numbers smaller than 2 are not prime.
	(loop for i from 2 to (/ number 2)
	
			if (= (mod number i) 0)  ; If the number divisible by a number except 1 and itself, it is not a prime number.
				do(return-from is-prime nil))
	(return-from is-prime T) ; Returns true, so, it is a prime number
)

(defun is-semi-prime(number) ; Checks whether a number is semi-prime or not

	(setq i 1) ; Numbers for multiplication
	(setq j 1)
	; A semi-prime number is multiplication of 2 prime number 
	(loop while  (<= i (/ number 2)) 
		do(setq i (+ i 1))
		do(loop while (<= j (/ number 2))
			  do(setq j (+ j 1))
			  if (and (eq (is-prime i) T) (eq (is-prime j) T)) ; Check if two numbers are prime or not
			  	if(eq number (* i j)) ; Check if multiplication of these 2 numbers equal to given number or not
			  		do(return-from is-semi-prime T) ; Return true if it is a semi-prime number
		) 
		do(setq j 1))
	(return-from is-semi-prime nil) ; Return nil if it is not a semi-prime number
)

(defun primecrawler (num1 num2) ; Primecrawler tests all numbers in the given gap and checks those number using other functions.
(with-open-file (str "primedistribution.txt"
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
	(loop for a from num1 to (- num2 1)
	  if (eq (is-prime a) T)
	  	do(format str "~d is Prime~%" a )
	  if (eq (is-semi-prime a) T)
	  	do(format str "~d is Semi-prime~%" a ))))

(defun read-array (filename) ; Reads 2 numbers from giver filename
  (with-open-file (in filename) ; File opening
    (loop for num = (read in nil)
          until (null num)  ; Collects numbers in an array
          collect num)))

(let ((arr (read-array "boundries.txt")))

	(if (> (nth 1 arr) (nth 0 arr))
		(primecrawler (nth 0 arr) (nth 1 arr)) ;Calling primecrawler by using array elements
		(format t "Second number must be greater than first one!")
	)
)

