; *********************************************
; *  341 Programming Languages                *
; *  Fall 2021                                *
; *  Author: Yakup Genc                       *
; *********************************************

; *********************************************
; *  Burak Cicek                              *
; *  		                                  *
; *  CSE 341 - ASSIGNMENT 1                   *
; *********************************************

;Keyword List and Operator List
(setq keywordList '("and" "or" "not" "equal" "less" "nil" "list" "append" "concat" "set" "deffun" "for" "if" "exit" "load" "disp" "true" "false")) 
(setq operatorList '("+" "-" "/" "*" "(" ")" "**" "‘" "’" "," ";" ";;"))

;After I read the file I separates the operators by spaces and i put the all chars in fileCode list. And my lexer is use the fileCode list.
(setq fileCode '())
;word is the readed word from file.
(setq word '())
;There are control flags and lists.
(setq opControl 0)
(setq starControl 0)
(setq starList '())
(setq comControl 0)
(setq comList '())
;I don't use this commentList list. If I write a comment, I put the all chars till the new line char after ;; operator.
(setq commentList '())

(defun gppinterpreter()
	;(setq termLine (read-line))
	;(print termLine)
	;(print *args*)
	(setq termLine (nth 0 *args*))
	;(print termLine)
	(setq line nil)
	(setq line (coerce line 'list))

(if(equal (length *args*) 0)
	(progn

		(loop while (not (string-equal line ""))
			do
			(setq line (read-line))
			(setq line (coerce line 'list))
			
			(loop for i from 0 to (length line)
	    		do
				(editOperators (nth i line))	
			)

			(nreverse fileCode)
			
			(loop for i from 0 to (length fileCode)
				do
				(if (not(eq (nth i fileCode) nil))
					(gpp_lexer(nth i fileCode))
					(setq fileCode nil)	
				)
			)

			(setq line (coerce line 'string))
		)

	)
)

	(if(not(equal termLine nil))
		(progn
			(setq filename termLine)
			(with-open-file (fp filename)
				(loop for ch = (read-char fp nil)
	    			while ch
	    			do   			
	    			(if (eq ch #\;)
						(progn
							(editOperators ch)
							(setf ch (read-char fp))
							(if (eq ch #\;)
								(progn
									(editOperators ch)
									(setf ch (read-char fp))

									(loop while (not(eq ch #\Newline))
									do
										(push ch commentList)
										(setf ch (read-char fp))
									)
								)
							)
						)
					)				
					(editOperators ch)	
				)
				(nreverse fileCode)
				(loop for i from 0 to (length fileCode)
	    			do
	    			(if (not(eq (nth i fileCode) nil))
						(gpp_lexer(nth i fileCode))	
					)
				)
			)
		)
	)
)

(defun writeFile(out)
  	(with-open-file (stream "parsed_lisp.txt"
						  	:direction :output 
						  	:if-exists :append 
						  	:if-does-not-exist :create)

	      	(format stream "~A~%" out)
	      	;(format stream #\Newline)
    )
)

;I write this helper function. It separates the operators by spaces.
;It works like this: If i read (x), it will be like this ( x ), or if i read x+y it will be like this x + y
(defun editOperators (ch)
	
	(if (or(string= ch "(") (string= ch ")") (string= ch "+") (string= ch "-")( string= ch "/") (string= ch ",") (string= ch "‘") (string= ch "’"))
		(progn
			(push #\Space fileCode)
			(push ch fileCode)
			(push #\Space fileCode)
		)
	)

	(if (not(or(string= ch "(") (string= ch ")") (string= ch "+") (string= ch "-")( string= ch "/") (string= ch ",") (string= ch "‘") (string= ch "’")))
		(progn			
			(push ch fileCode)
		)
	)
)

(defun gpp_lexer (ch)

	;Compare the char and operators.
	(cond
		((string= ch (nth 0 operatorList) )(writeFile "OP_PLUS"))
		((string= ch (nth 1 operatorList) )(writeFile "OP_MINUS"))
		((string= ch (nth 2 operatorList) )(writeFile "OP_DIV"))
		((string= ch (nth 3 operatorList) )(setq starControl 1) (push ch starList))
		((string= ch (nth 4 operatorList) )(writeFile "OP_OP"))
		((string= ch (nth 5 operatorList) )(writeFile "OP_CP")(setq opControl 1))
		;((string= ch (nth 6 operatorList) )(writeFile "OP_DBLMULT"))
		((string= ch (nth 7 operatorList) )(writeFile "OP_OC"))
		((string= ch (nth 8 operatorList) )(writeFile "OP_CC"))
		((string= ch (nth 9 operatorList) )(writeFile "OP_COMMA"))
		((string= ch (nth 10 operatorList) )(setq comControl 1)(push ch comList))
		;((string= ch (nth 11 operatorList) )(writeFile "COMMENT") )

	)


	(if(or 	(alpha-char-p ch)(digit-char-p ch)
			(eq ch #\?)
			(eq ch #\!)
			(eq ch #\=)
			(eq ch #\_)
			(eq ch #\.)
			(eq ch #\&)
			(eq ch #\%)
			(eq ch #\$)
			(eq ch #\#)
			(eq ch #\\)
			(eq ch #\£)
			(eq ch #\^)
			(eq ch #\:)
		)
	    (push ch word)

	)

	(if(or (eq ch #\Space) (eq ch #\Newline) (eq ch #\Tab) (eq opControl 1))
		(progn
			(setq word (nreverse word))
			(setq word (coerce word 'string))
			;(princ word)



			;There are * and ** operators in my operator list. I control here, the characther is * operator or ** operator.
			(if (and(eq (length starList) 1)(eq starControl 1))
				(progn
					(writeFile "OP_MULT") 
					
					(eq starControl 0)
					(setq starList nil)
				)
			)

			(if (and(eq (length starList) 2)(eq starControl 1))
				(progn
					(writeFile "OP_DBLMULT")
					
					(eq starControl 0)
					(setq starList nil)
				)
			)

			; ";;" opertor is in my operator list. But I read file char by char. So I add the ";"  operator in my list and
			; I control here, is the character ;; operator?
			(if (and(eq (length comList) 1)(eq comControl 1))
				(progn
					(writeFile "OP_SC") 
					
					(eq comControl 0)
					(setq comList nil)
				)
			)

			(if (and(eq (length comList) 2)(eq comControl 1))
				(progn
					(writeFile "COMMENT")
					
					(eq comControl 0)
					(setq comList nil)
				)
			)
			
			(if (> (length word) 0)
			(progn

				(setq counter 0)

				;Compare the keyword and word, if they are same, the word is an keyword. But they are not same, the word is an identifier.
				(if (not(eq word nil))
				(progn
				(cond
					((string= word (nth 0 keywordList) )(writeFile "KW_AND"))
					((string= word (nth 1 keywordList) )(writeFile "KW_OR"))
					((string= word (nth 2 keywordList) )(writeFile "KW_NOT"))
					((string= word (nth 3 keywordList) )(writeFile "KW_EQUAL"))
					((string= word (nth 4 keywordList) )(writeFile "KW_LESS"))
					((string= word (nth 5 keywordList) )(writeFile "KW_NIL"))
					((string= word (nth 6 keywordList) )(writeFile "KW_LIST"))
					((string= word (nth 7 keywordList) )(writeFile "KW_APPEND"))
					((string= word (nth 8 keywordList) )(writeFile "KW_CONCAT"))
					((string= word (nth 9 keywordList) )(writeFile "KW_SET"))
					((string= word (nth 10 keywordList) )(writeFile "KW_DEFFUN"))
					((string= word (nth 11 keywordList) )(writeFile "KW_FOR"))
					((string= word (nth 12 keywordList) )(writeFile "KW_IF"))
					((string= word (nth 13 keywordList) )(writeFile "KW_EXIT"))
					((string= word (nth 14 keywordList) )(writeFile "KW_LOAD"))
					((string= word (nth 15 keywordList) )(writeFile "KW_DISP"))
					((string= word (nth 16 keywordList) )(writeFile "KW_TRUE"))
					((string= word (nth 17 keywordList) )(writeFile "KW_FALSE"))
					(t

						(setq word (coerce word 'list))
						
						(loop for i from 0 to (- (length word) 1)
		    			do
							(if (digit-char-p (nth i word) )
								(progn
									(setq counter (+ counter 1))
								)							
							)
						)

						(if (eq counter (length word))
							(progn
								(if (not(eq (nth 0 word) #\0)) 
									(writeFile "VALUE")
								)
								(if (and (eq (nth 0 word) #\0) (not(eq 1 (length word))))
									(writeFile "INVALID VALUE")
								)
							)							
						)

						(if (not(eq counter (length word)))
							(progn

								(if (digit-char-p (nth 0 word) )
									(writeFile "ERROR")
								)

								(if (or (alpha-char-p (nth 0 word) )
									(eq (nth 0 word) #\?)
									(eq (nth 0 word) #\!)
									(eq (nth 0 word) #\=)
									(eq (nth 0 word) #\_)
									(eq (nth 0 word) #\.)
									(eq (nth 0 word) #\&)
									(eq (nth 0 word) #\%)
									(eq (nth 0 word) #\$)
									(eq (nth 0 word) #\#)
									(eq (nth 0 word) #\\)
									(eq (nth 0 word) #\£)
									(eq (nth 0 word) #\^)
									(eq (nth 0 word) #\:)
									)
									(progn
										(loop for i from 0 to (- (length word) 1)
											do
											(if (or (eq (nth i word) #\?)
													(eq (nth i word) #\!)
													(eq (nth i word) #\=)
													(eq (nth i word) #\_)
													(eq (nth i word) #\.)
													(eq (nth i word) #\&)
													(eq (nth i word) #\%)
													(eq (nth i word) #\$)
													(eq (nth i word) #\#)
													(eq (nth i word) #\\)
													(eq (nth i word) #\£)
													(eq (nth i word) #\^)
													(eq (nth i word) #\:)

												)
												(progn
													(writeFile "ERROR")
													(return)																										
												)										
											)
										)
										(writeFile "IDENTIFIER")	
									)																	
								)
							)
						)

						(setq word (coerce word 'string))
					)
				)))
			))

			(setq word nil)				
			(setq opControl 0)				
		)
	)
)

(gppinterpreter)