;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname sillystring) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor mixed-fraction #f #t none #f () #t)))
;;
;; ***********************************************************
;; Mukund Rana (mk3rana)
;; CS 135 Fall 2020
;; Assignment 5, Q3 
;; ***********************************************************
;;

;; ***********************************************************
;; Q3 a)
;; ***********************************************************

(define-struct silly-string (first middle last))
;; A SillyStringStruct is a (make-silly-string Char SillyStr Char)

;; A SillyStr is one of:
;; * empty
;; * a Char
;; * a SillyStringStruct


;;(sillify s) produces the corresponding SillyStr of string s
;; Examples
(check-expect (sillify "Babbage")
              (make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e))

(check-expect (sillify "Lovelace")
              (make-silly-string
               #\L
               (make-silly-string
                #\o
                (make-silly-string
                 #\v
                 (make-silly-string
                  #\e
                  empty
                  #\l)
                 #\a)
                #\c)
               #\e))

;; sillify: Str -> SillyStr
(define (sillify s)
  (makeinto-silly-string (string->list s)))
  
;;tests
(check-expect (sillify "able")
              (make-silly-string #\a (make-silly-string #\b empty #\l) #\e))

(check-expect (sillify "") empty)

(check-expect (sillify "to" ) (make-silly-string #\t empty #\o))

(check-expect (sillify "a" ) #\a)

;;(middle-silly-string middlelst) finds the middle of the list 
;;middle-silly-string: (listof Char) -> (listof Char)
(define (middle-silly-string middlelst)
  (cond
    [(empty? (rest (rest middlelst))) empty ]
    [else (cons (second middlelst) (middle-silly-string (rest middlelst)))]))


;;(last-char lstchar) finds the last char in a list of char
;;last-char: (listof Char)-> (listof Char)
(define (last-char lstchar)
  (cond
    [(empty? (rest lstchar)) (first lstchar)]
    [else (last-char (rest lstchar))]))

;;(makeinto-silly-string lstchar) turns a list of chars into a silly string
;;makeinto-silly-string: (listof Char)-> SillyStr
(define (makeinto-silly-string lstchar)
  (cond
    [(empty? lstchar) empty]
    [(empty? (rest lstchar)) (first lstchar)]
    [else (make-silly-string (first lstchar) (makeinto-silly-string (middle-silly-string lstchar)) (last-char lstchar))]))



;; ***********************************************************
;; Q3 b)
;; ***********************************************************

;; Qb) and Qc

;; (unsillify ss) consumes a SillyStr and produces teh corressponding string
;; Examples



;; unsillify: SillyStr -> Str
;;(define (unsillify ss)) 

;;(make-into-string silly-strlst) turns a SillyStr into string.
;; make-into-string: SillyStr -> (listof Char)
(define (make-into-string silly-strlst)
  (cond
    [(char? silly-strlst) (cons silly-strlst empty)]
    [else (list (silly-string-first silly-strlst) (make-into-string (silly-string-middle silly-strlst)))]))

;;tests 
   


;;(make-into-string-frist silly-strlst) turns first part of a SillyStr into char.
;; make-into-string: SillyStr -> (listof Char)
(define (make-into-string-frist silly-strlst)
  (silly-string-first silly-strlst))

;;(make-into-string-frist silly-strlst) turns first part of a SillyStr into char.
;; make-into-string: SillyStr -> (listof Char)
(define (make-into-string-middle silly-strlst)
  (silly-string-middle silly-strlst))

;;(make-into-string-last silly-strlst) turns last part of a SillyStr into char.
;; make-into-string: SillyStr -> (listof Char)
(define (make-into-string-last silly-strlst)
  (silly-string-last silly-strlst))


;;tests

 
(make-into-string (silly-string-middle(make-silly-string
               #\B
               (make-silly-string
                #\a
                (make-silly-string
                 #\b
                 #\b
                 #\a)
                #\g)
               #\e)))










  


 



    
  
