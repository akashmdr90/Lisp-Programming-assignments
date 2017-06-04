;;; 	reverseTop
;;;     Parameters:
;;;     L - a list of items from which need to be reversed


( defun reverseTop (L)
    ( COND
       ( (NULL L) NIL)
       ( T (APPEND (reverseTop (CDR L)) (list (CAR L))) ))
)



;;; 	reverseAll
;;;     Parameters:
;;;     L - a list of items from which need to be reversed at all levels.


(defun reverseAll  (L)
  (COND ( (NULL L) NIL)
        ( (ATOM (CAR L))(APPEND (reverseAll  (CDR L))(list (CAR L))) )
        ( T (APPEND (reverseAll  (CDR L)) (list (reverseAll  (CAR L)))) ))

)



;;; 	removeNILTop
;;;     Parameters:
;;;     L - a list of items from which need to removes NIL at the top level.


(defun removeNILTop (L)
(COND ( (NULL L) NIL)
      ( (EQ (CAR L) NIL) (removeNILTop (CDR L)) )
      ( T (CONS (CAR L) (removeNILTop (CDR L))) ))
)






;;; 	removeNILMost
;;;     Parameters:
;;;     L - a list of items from which need to removes NIL at any level.



(defun removeNILMost(L)
(COND ( (null L) nil)
      ( (ATOM (CAR L)) 
           (COND ( (NULL(CAR L)) (removeNILMost (CDR L)) )
                 ( T (CONS (CAR L) (removeNILMost (CDR L)))) ))
      ( T (CONS (removeNILMost (CAR L)) (removeNILMost (CDR L))) ))
)




;;; 	palindrome
;;;     Parameters:
;;;     L - a list of items from which need to be checked if its palindrome.



(defun palindrome (L)
  (EQUAL L (reverseTop L) )
)

;;; 	removeNILALL
;;;     Parameters:
;;;     L - a list of items from which need to remove
;;;		any resulting NIL 



(defun removeNILALL(L)
(COND ( (null L) nil )
      ( (ATOM (CAR L) ) 
          (COND ( (NULL(CAR L)) (removeNILALL(removeNILALL (CDR L))) )
                ( T (CONS (CAR L) (removeNILALL(removeNILALL (CDR L)))) )
          ) )
      ( T (CONS (removeNILALL (CAR L))(removeNILALL(removeNILALL (CDR L))) ) )
)
)








(reverseTop '(X Y Z))

(reverseTop '(X (Y Z (A)) (W)))

(reverseAll '(X Y Z))

(reverseAll '(X (Y Z (A)) (W)))

(removeNILTop '(NIL X NIL NIL Y  NIL Z))

(removeNILTop '(X NIL Y NIL Z NIL))

(removeNILTop '(NIL (X NIL Y) (NIL NIL)))

(removeNILMost '(NIL X NIL NIL Y  NIL Z))

(removeNILMost '(X NIL (Y NIL Z) NIL))

(removeNILMost '(NIL (NIL) (X NIL Y) (NIL NIL) Z))

(removeNILMost '(NIL ( (((((NIL) NIL)))))))

(palindrome '(R A C E C A R))

(palindrome '(W A S I T A C A R O R A C A T I S A W))

(palindrome '(N I X O N))

(removeNILAll '(NIL (NIL) (X NIL Y) (NIL NIL) Z))

(removeNILAll '(NIL ( (((((NIL) NIL)))))))

(removeNILAll '(NIL (X (NIL) Y) ((NIL)) ))

(removeNILAll '(NIL (((X ((((((((((NIL)))))))))) Y) Z) W) (((NIL))) ))
