(defclass node () ; Represents a node of the tree
   ((frq :accessor node-frq) ; frequency
      (chr :accessor node-chr) ; character
      (right :accessor node-right)
      (left :accessor node-left)))

(defun file-get-contents (filename)
  (with-open-file (stream filename) ;Read all contents of the file
    (let ((contents (make-string (file-length stream))))
      (read-sequence contents stream)
      contents)))

(defun charfrequency (str)

	(setf chars (make-array '(128))) ; it represents all of the characters
	; 127 -> em dash (8212)
	(loop for i from 0 to (- (length chars) 1)
		do (setf (aref chars i) 0)
	)

	(loop for i from 0 to (- (length str) 1) ; Keep frequences according to ther ascii code
			if(eq (char-code (aref str i)) 8212)
				do( setf (aref chars 127) (+ 1 (aref chars 127)))
			else do( setf (aref chars (char-code (aref str i))) (+ 1 (aref chars (char-code (aref str i)))))
	)

	(return-from charfrequency chars)  ; for tree
)

(defun traverse (root s) ; Traverses the huffman tree
  (if (and (eq nil (node-left root)) (eq nil (node-right root))) 
		(progn ; So, this is a character node
			(push (node-chr root) finalC)
			(push s finalS)
			(return-from traverse)
		)
  )
  (traverse (node-left root) (concatenate 'string s "0 ")) ; Recursively, huffman tree traversing
  (traverse (node-right root) (concatenate 'string s "1 ") ))

(defun removeElement (n list) ; Removes nth element from the list
  (declare
    (type (integer 0) n)
    (type list list))
  (if (or (zerop n) (null list))
    (cdr list)
    (cons (car list) (removeElement (1- n) (cdr list)))))

(defun sortNodes (someNones)
;This is a simple sort algorithm to get 2 node that have smallest data values
	(loop for i from 0 to (- (length someNones) 1)

		do(loop for j from i to (- (length someNones) 1)	

			if(> (node-frq (nth i someNones)) (node-frq (nth j someNones)))
				do(setf temp (make-instance 'node))
				(setf temp (nth i someNones))
				(setf (nth i someNones) (nth j someNones))
				(setf (nth j someNones) temp)))
	(return-from sortNodes someNones))

(defvar charsx '()) ; Characters
(defvar freqs '()) ; Frequencies

(let ((arr (file-get-contents  "paragraph.txt"))) ; Read file
	(defvar charFreq (charfrequency arr)) ; Find the frequencies
)

(loop for i from 0 to 126

	if(not (eq 0 (aref charFreq i))) ; if its frequency equals the zero, so the letter does not exist
		do (push (aref charFreq i) freqs) ; add the frequency
		(push (code-char i) charsx) ; add the character
)

(if(not (eq 0 (aref charFreq 127))) ; This is for em_dash character
	(progn
		(push (code-char 8212)  charsx) 
		(push (aref charFreq 127) freqs)))

(setq charsx (reverse (cdr (reverse charsx)))) ; Delete the null characters from listss
(setq freqs (reverse (cdr (reverse freqs))))

(defvar nodes '())

(loop for i from 0 to (- (length charsx) 1)

	do(setf obj (make-instance 'node)) ;Create objects
	do(setf (node-chr obj) (nth i charsx))	; Fill the objects with our data
	do(setf (node-frq obj) (nth i freqs))
	
	do(setf (node-left obj) '())
	do(setf (node-right obj) '())

	do(push obj nodes)
)

(setf rootNode (make-instance 'node))

(loop while (> (length nodes) 1) ;This loop creates huffman tree, using min-heap algorithm
	do(sortNodes nodes) ; sort nodes according to their frequency
	do(setq n1 (nth 0 nodes))
	(setq nodes (removeElement 0 nodes))
	do(setq n2 (nth 0 nodes))
	(setq nodes (removeElement 0 nodes))

	do(setf new (make-instance 'node)) ; create a new node 
	do(setf (node-frq new) (+ (node-frq n1) (node-frq n2)))

	do(setf (node-left new) n1)
	do(setf (node-right new) n2)

	(setq rootNode new)
	(push new nodes) ; add new node to the list
)

	(defvar finalS '())
	(defvar finalC '()) ;Last versions of characters and frequencies
	(traverse rootNode "" )

	(defvar temps "")
	(defvar tempc "") ;Temprory strings for sorting

	(loop for i from 0 to (- (length finalS) 1) ; Sort the codes according to their length
		do(loop for j from i to (- (length finalS) 1)
				if(> (length (nth i finalS)) (length (nth j finalS)))
					do(setq temps (nth i finalS))
					(setf (nth i finalS) (nth j finalS))
					(setf (nth j finalS) temps)		
					(setq tempc (nth i finalC))
					(setf (nth i finalC) (nth j finalC))
					(setf (nth j finalC) tempc)))	

(defvar aLine "")
(defvar tmpS "")

(with-open-file (str "huffman_codes.txt"	; Create a file to write huffman codes
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)

(loop for i from 0 to (- (length finalS) 1)

	if(not (or (eq (nth i finalC) #\space) (eq (nth i finalC) #\newline)))
		do(setq tmpS (string (nth i finalC)))
	else 
		if(eq (nth i finalC) #\space)
			do(setq tmpS "Space")
		else do(setq tmpS "New Line")
 
	do(setq aLine (concatenate 'string  tmpS ": " (nth i finalS) "~%")) ; Add the neccessary characters
	do(format str aLine) ; Write into the file
	do(setq aLine nil)
	do(setq tmpS nil)
))
