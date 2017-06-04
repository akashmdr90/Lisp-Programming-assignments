;;; useful functions and test cases for LISP natural language parsing program

;;; Set up the parse-obj and word-dict
(setf parse-obj (MAKE-HASH-TABLE))
(setf word-dict (MAKE-HASH-TABLE))

;;; processSentence 
;;;    Parameters:
;;;       sentence - a list of words making a sentence
;;;    Purpose:
;;;       - Sets the parse-obj to contain the sentence
;;;       - Sets the cursor position to zero.
;;;       - Uses resetPartsOfSpeech to reset the values of each part of speech.
;;;       - Prints the sentence
;;;       - Invokes checkSentence to check for valid syntax and prints
;;;         the result.
;;;       - Prints the value for each part of speech
;;;    Notes:
;;;       - Sentences will not contain punctuation since
;;;             -- commas have a meaning in Common LISP associated with backquote
;;;             -- periods are used for dotted pairs
;;;       -  For commas, we will use the symbol COMMA  

(defun processSentence(sentence)
	(PROG (result)
		;;; Save the sentence in parse-obj.
		(putp 'sentence parse-obj sentence) 
		;;; Set the cursor position to 0.
		(putp 'cursor parse-obj 0)
 
		;;; reset the parts of speech to NIL
		(resetPartsOfSpeech parse-obj 'subject 'verb 'prep 'directObject 'indirectObject)
		(resetPartsOfSpeech parse-obj 'subConjunction 'subSubject 'subVerb 'SubPrep 'SubDirectObject 'SubIndirectObject)
		(format T "~% ******************************************************************")
		(format T "~%  ~a" sentence)
		(setf result (checkSentence parse-Obj))
		(format T "~%    checkSentence returned ~a" result)
		(format T "~%    subject= ~a" (getp 'subject parse-obj))
		(format T "~%    verb= ~a" (getp 'verb parse-obj))
		(format T "~%    directObject= ~a" (getp 'directObject parse-obj))
		(format T "~%    prep= ~a" (getp 'prep parse-obj))
		(format T "~%    indirectObject= ~a" (getp 'indirectObject parse-obj))
		(if (not (eql doing_extra 'EC3)) (return result))
		
		(format T "~%    subConjunction= ~a" (getp 'subConjunction parse-obj))
		(format T "~%    subSubject= ~a" (getp 'subSubject parse-obj))
		(format T "~%    subVerb= ~a" (getp 'subVerb parse-obj))
		(format T "~%    SubDirectObject= ~a" (getp 'SubDirectObject parse-obj))
		(format T "~%    SubPrep= ~a" (getp 'SubPrep parse-obj))
		(format T "~%    SubIndirectObject= ~a" (getp 'SubIndirectObject parse-obj))
		(return result) ) )
 
;;; putp 
;;;    Parameters:
;;;       symbol   - symbol to be given the property
;;;       ht       - hash table to store the symbol and its property value
;;;       value    - the property value
;;;    Purpose:
;;;       stores the property value for the symbol in the specified hash table
;;;    Notes:
;;;       If the symbol isn't an ATOM, putp breaks execution with an ERROR.
;;;    Example Usage:
;;;       (putp 'prep parse (list prep))
;;;       (putp 'mickey word-dict (list 'noun))

(defun putp (symbol ht value)
	(if (ATOM symbol)
		(setf (gethash symbol ht) value)
		(ERROR "~s is not a valid symbol for putp" symbol)
	)
)
	
;;; getp 
;;;    Parameters:
;;;       symbol   - symbol about which we want its property value
;;;       ht       - hash table which stores the symbol and its property value
;;;    Purpose:
;;;       returns the property value for the symbol in the specified hash table
(defun getp (symbol ht)
	 (gethash symbol ht) )
	 
;;; getCursor 
;;;    Parameters:
;;;       parse - the parse object containing a sentence, cursor position, and
;;;               value for each part of speech
;;;    Purpose:
;;;       returns the current cursor position (relative to zero)
(defun getCursor (parse)
	(getp 'cursor parse) )

;;; setCursor 
;;;    Parameters:
;;;       parse - the parse object containing a sentence, cursor position, and
;;;               value for each part of speech
;;;       cursorPosition - new cursor position
;;;    Purpose:
;;;       Sets the value of the cursor position (relative to zero) in the
;;;       parse object
;;;    Notes:
;;;       If the cursorPosition isn't a numeric, setCursor breaks execution 
;;;       with an ERROR.
(defun setCursor (parse cursorPosition)
	(if (numberp cursorPosition)
		(putp 'cursor parse cursorPosition)
		(ERROR "~s is not a numeric argument for setCursor" cursorPosition)
	)
)

;;; getToken
;;;    Parameters:
;;;       parse - the parse object containing a sentence, cursor position, and
;;;               value for each part of speech
;;;    Purpose:
;;;       returns the next token from the sentence.  If there are no more
;;;       tokens, it returns NIL.
;;;    Notes:
;;;       This modifies the cursor position after getting the current token
;;;       at the old position.
	
(defun getToken (parse)
	(prog (temp)
		(setf temp (nth (getp  'cursor parse) (getp  'sentence parse))) 
		(setCursor parse (1+ (getp 'cursor parse))) 
		(return temp) ) )
		
;;; set_isa
;;;    Parameters:
;;;       partOfSpeech - The value we will associate to each word
;;;       wordlist     - The words we will populate the tables keys with
;;;    Purpose:
;;;       Assign values to keys in hash table

(defun set_isa (partOfSpeech wordlist) 

	(cond ( (EQL (car wordlist) NIL) NIL )
		( T (putp (car wordlist) word-dict partOfSpeech) (set_isa partOfSpeech (cdr wordlist)) )
	)
	
)

(defun checkNP ()

)
;;; checkSentence
;;;    Parameters:
;;;       parse - The hash table 
;;;    Purpose:
;;;       Returns T if the sentence is valid, otherwise it returns NIL.
;;;       We also set values of partOfSpeech here.
;;;    Notes:
(defun checkSentence (parse)
	
	(setf sub (getToken parse))			;;get the first two tokens
	(setf ver (getToken parse)) 


	;; Lets do subjects first
	;; Here we check if there is an article before the subject
	;; If there is we get the next token and check for a noun.  
	(cond((isa sub 'article)									;;If it is an aritcle run the following 
		  (setf sub2 ver)										;; We have sub = to the article and sub 2 should be the noun
		  (setf temp (list sub sub2))							;;Put them in a list
	 	  (if(isa sub2 'noun) (putp 'subject parse temp)))		;;Check if sub to is even a noun, if T then capture the list
	 	 ((isa sub 'noun)(putp 'subject parse (list sub)))		;;If we dont have an article, check if its a noun and capture if true
	
	)
	
	;;Check for the verb
	(cond((isa ver 'verb) (putp 'verb parse (list ver)))		;;Check if the next token is a verb, if it is, them capture
		 ((setf ver (getToken parse))  							;;If it isnt, it may be the next one becaue articles...
		  (if(isa ver 'verb) 									;;If it is then capture it
		 (putp 'verb parse (list ver))))
	)
	
	;;Lets check for direct objects
	(setf obj (getToken parse))
	
	
	(cond((isa obj 'article) 													;;If its an article get the next token and check for noun
		  (setf obj2 (getToken parse)) 											;;If it isnt an article then check if its a noun
		  (if(isa obj2 'noun) (putp 'directObject parse (list obj obj2)))) 
		 ((if(isa obj 'noun) (putp 'directObject parse (list obj))NIL))
	)
	
	(cond((isa obj 'prep)  
			(putp 'prep parse (list obj)) 
			(setf indobj (getToken parse)) 
			(if(eql indobj 'the) (setf indobj (list indobj (getToken parse))) ) 
			(putp 'indirectObject parse indobj))
	)
	
	;;Now prepositions and indirect objects
	(setf prep (getToken parse))
	(cond((isa prep 'prep )  
		 (setf indobj (getToken parse)) (putp 'prep parse (list prep)) 
		  (cond((eql indobj 'the) 
				(setf indobj2 (getToken parse)) 
				(setf indobj (list indobj indobj2)) 
				(if(isa indobj2 'noun)(putp 'indirectObject parse indobj))) 
		  ))
	)
	;;Check if the sentence is valid with Subject, verb, and direct object
	(if(AND(getp 'subject parse-obj)(getp 'verb parse-obj)	(getp 'prep parse-obj)(getp 'indirectObject parse-obj)	) 
	(return-from checkSentence T)
	
	)
	
	;(if(AND(getp 'subject parse-obj)(getp 'verb parse-obj)	(getp 'indirectObject parse-obj)	) 
	;(return-from checkSentence T)
	;	
	;)
	;;Check if sentence is valid with Subject and verb only
	(if(AND(getp 'subject parse-obj)(getp 'verb parse-obj)	(not(getp 'indirectObject parse-obj))	(not(getp 'prep parse-obj))) 
	(return-from checkSentence T)
	
	)

)


;;; resetPartsOfSpeech
;;;    Parameters:
;;;       parse -  A has table who's values we will reset
;;;       arguments - The items we will reset
;;;    Purpose:
;;;       Sets the variables in the hash table to NIL 
;;;    Notes:
(defun resetPartsOfSpeech (parse &rest arguments)

	(dolist (pos arguments)
		(putp pos parse NIL)
	)

)

;;; isa
;;;    Parameters:
;;;       word         - The key we will be finding a value for
;;;       partOfSpeech - The value of the key we find
;;;    Purpose:
;;;       Tells us if the word we sent in is of the same part of speech that 
;;;       we are looking for. T if true, returns NIL otherwise
;;;    Notes:

(defun isa (word partOfSpeech)
	(setf temp2 (getp word word-dict))
	
	(cond ( (EQL temp2 partOfSpeech) T))
	
	
)
;;; Use set_isa to set the part of speech for each of the words.
(set_isa 'article '(a an the))
(set_isa 'noun '(mickey ball dog home))
(set_isa 'verb '(ran throw throws threw hit shot)) 
(set_isa 'prep '(at on under above to of from))

;;; running the check sentence
 (setf doing_extra NIL)
 (processSentence '(mickey throws a ball to the dog ))  

 (processSentence '(the dog ran home))  
 
 (processSentence '(mickey throws at the dog ))  
  
 (processSentence '(mickey throws a ball to the))
  
